
# MF: Deprecated, not general enough.  Just call the relevant function to make the dataset in the analysis script.

# prepareBenchmarkingDatasets <- function(benchmark.list, resolution = "HD"){
#   
#   benchmarking.datasets.list <- list()
#   
#   
#   # For each benchmark string 
#   for(benchmark.string in benchmark.list){
#     
#     # If it is a biome classification
#     if(benchmark.string %in% names(supported.biome.schemes)) {
#       
#       message(paste("Reading PNV biomes using" ,benchmark.string, "scheme", sep = " "))
#       benchmarking.datasets.list[[benchmark.string]] <- new("SpatialDataset",
#                                                             id = benchmark.string,
#                                                             name = paste("H&P PNV Biomes classified by scheme ", benchmark.string),
#                                                             temporal.extent = new("TemporalExtent", name = "PNV", start = 1961, end = 1990) ,
#                                                             data = readHandPBiomes(resolution = resolution, classification = benchmark.string),
#                                                             veg.quant = lookupVegQuantity("lai"),
#                                                             units = ""
#       )
#       
#     } # if it is a biome classification
#     
#     # If it is the Saatchi data
#     if(benchmark.string == "Saatchi2011") { benchmarking.datasets.list[[benchmark.string]] <- getSaatchi2011(resolution = resolution) }
#     
#     
#     # If it is the GFED data
#     
#   } # for each benchmark
#   
#   
#   
#   return(benchmarking.datasets.list)
#   
# }






#############################################################################################################
############################ COMPARE TWO RASTERS 
#' Statistically compare two rasters 
#' 
#' Returns the R^2, RMSE and Pearson's correlation coefficient between two rasters, as well as the rasters themselves 
#' (on a common extent and with the NA areas masked from both data datasets), 
#' a raster of the difference and their percentage difference (relative to the data).
#' 
#' @param data.raster The data as a raster
#' @param model.raster The model output as a raster
#' 
#' @return An object of class RasterComparison
#' 
#' The choice of statistical measures might not be the best but can easily be improved.  No spatial correlations are considered.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export

compareTwoRastersStats <- function(data.raster, model.raster){
  
  if(extent(data.raster) != extent(model.raster)){
    
    data.raster <- intersect(data.raster, model.raster)
    model.raster <- intersect(model.raster, data.raster)
    
  }
  
  
  model.raster <- mask(model.raster, data.raster)
  data.raster <- mask(data.raster, model.raster)
  
  # difference raster
  diff <- model.raster - data.raster
  names(diff) <- "Model.minus.Data"
  
  # calculated mean, SD, MSE, RMSE, R^2, Pearson correlation etc
  mean.diff <- cellStats(diff, stat= mean , na.rm=TRUE, asSample=FALSE)
  sd.diff <- cellStats(diff, stat= sd , na.rm=TRUE, asSample=FALSE)
  sd.observed <- cellStats(data.raster, stat= sd , na.rm=TRUE, asSample=FALSE)
  var.observed <- sd.observed^2
  MSE <- cellStats(diff^2, stat= sum, na.rm=TRUE, asSample=FALSE)/ cellStats(is.finite(diff), stat= sum, asSample=FALSE)
  RMSE <- (MSE)^(0.5)
  R.squ <- 1 - (MSE/var.observed)
  
  P.cor.full <- layerStats(stack(data.raster, model.raster), 'pearson', na.rm=TRUE, asSample=FALSE)
  P.cor <- P.cor.full$`pearson correlation coefficient`[1,2]
  
  perc.diff.raster <- (diff / data.raster) * 100
  comparison.results <- new("RasterComparison",
                            diff.raster = diff, 
                            perc.diff.raster = perc.diff.raster, 
                            data.raster = data.raster, 
                            model.raster = model.raster, 
                            R.squ = R.squ, 
                            P.cor = P.cor, 
                            RMSE = RMSE, 
                            mean.diff = mean.diff, 
                            sd.diff = sd.diff)
  
  
  return(comparison.results)
  
}


#' Compare a layer from a VegObject to a spatial dataset
#' 
#' Produces standard plot to compare model output to a spatial dataset.
#' 
#' @param dataset The data set as a SpatialDataset object
#' @param vegobject The VegObject which contains the layer to be compared from the model (and, helpfully, meta-data about the run)
#' @param layer The name of the layer to be compared to the spatial dataset
#' @param diff.cuts A sequence of bins for the histo plot (defaults to the cuts)
#' @param histo.plot.range A two-value numeric vector defining the range for the histo plots
#' @param doScatterPlots Logical, if TRUE make a scatter plot (default is TRUE)
#' @param doHistoPlots Logical, if TRUE make histograms plots (default is TRUE)
#' @param quant A VegQuant object to override the one from vegobject (if necessary)
#' @param ignore.raster A raster used to mask out gridcells wich are not to be compared (ie any gridcells set to NA in the ignore.raster
#' are set to NA in thr model and data rasters)
#' @param ... Parameters passed to plotVegMaps() for the spatial plots.
#'
#' @return An object of class RasterComparison
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export

compareRunToSpatialDataset <- function(dataset, 
                                       vegobject, 
                                       layer,
                                       diff.cuts = NULL, 
                                       histo.plot.range = NULL, 
                                       doScatterPlots=TRUE, 
                                       doHistoPlots=TRUE, 
                                       quant = NULL, 
                                       ignore.raster = NULL,
                                       ...){
  
  # PROMOTE MODEL TO RASTER FOR PROCESSING
  model.raster <- promoteToRaster(vegobject, layer, vegobject@run@tolerance)
  
  # PREPARE CUTS AND HISTO PLOT RANGE (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@veg.quant@cuts)[2] - range(dataset@veg.quant@cuts)[1] )/ (length(dataset@veg.quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@veg.quant@cuts)), max(dataset@veg.quant@cuts), interval)
  }
  
  if(is.null(histo.plot.range)) histo.plot.range <- c(diff.cuts[[1]], diff.cuts[[length(diff.cuts)]])
  
  # PREPARE QUANT
  if(is.null(quant)) quant <- dataset@veg.quant 
  
  # crop all rasters to the same size - not necessary now?
  #temp <- intersectionRVC(model.raster, dataset@data)
  #model.raster <- temp[[1]]
  #data.raster <- temp[[2]]
  #rm(temp)
  
  # EXTRA DATA FROM SpATIAL DATASET AND SET NAMES
  names(model.raster) <- layer
  data.raster <- dataset@data
  names(data.raster) <- dataset@id
  
  # make difference map and a stack to plot
  if(!is.null(ignore.raster)){ 
    model.raster <- mask(model.raster, ignore.raster)
    data.raster <- mask(data.raster, ignore.raster)
  }
  
  # calculate comparison
  comparison.results <- compareTwoRastersStats(data.raster, model.raster)
  data.raster <- comparison.results@data.raster
  model.raster <- comparison.results@model.raster
  
  ##### DIFFERENCE MAPS
  plotVegMaps(comparison.results@diff.raster,
              quant = quant, 
              period = dataset@temporal.extent, 
              doSummary = TRUE, 
              doIndividual = FALSE, 
              run = vegobject@run,
              summary.file.name = paste(quant@id, "Vs", dataset@id, sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              special = "diff",
              ...)
  
  ##### ABSOLUTE MAPS
  plotVegMaps(stack(comparison.results@model.raster, comparison.results@data.raster),
              quant = quant, 
              period = dataset@temporal.extent, 
              doSummary = TRUE, 
              doIndividual = TRUE, 
              run = vegobject@run,
              summary.file.name = paste(quant@id, "comp", dataset@id, "2-up", sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              plot.labels = c(vegobject@run@description, dataset@name),
              ...)
  
  
  ##### HISTOS
  if(doHistoPlots){
    
    plotHistoComparison(model = comparison.results@model.raster, 
                        data = comparison.results@data.raster, 
                        stat.results = comparison.results,
                        run = vegobject@run, 
                        data.name = dataset@id, 
                        quant = quant, 
                        plot.range = histo.plot.range)
    
  }
  
  ##### SCATTER PLOT
  if(doScatterPlots){
    
    plotScatterComparison(model = comparison.results@model.raster, 
                          data = comparison.results@data.raster,  
                          stat.results = comparison.results,
                          run = vegobject@run, 
                          data.name = dataset@id, 
                          quant = quant)
    
    
  }
  comparison.results@id <- dataset@id
  
  return(comparison.results)
  
}

#' Compare many VegRuns to a spatial dataset
#' 
#' To use this function it is *required* that \code{compareRunToSpatialDataset()} has been performed on each of the runs with 
#' the dataset to which we are comparing.
#' 
#' @param runs A list of VegRun objects (each with the RasterComparison object already created)
#' @param dataset The spatial dataset to which we are comparing (represented as a SpatialDataset object)
#' @param label A string with which to tag the resulting plots to specify the analysis (to differentiate them from other plots),
#' for example "ForcingDatasetComparison" or "SoilDepths.  Or whatever you want.
#' @param diff.cuts Numeric vector of cuts for plotting the absolute difference.  If not specified it is derived as the maximum
#' possible range based on the VegQuant object in the dataset.
#' @param perc.diff.cuts Numerice vector of cuts for plotting the percentage differences.  If not specified defaults to -100\% to +200\%
#' in 5\% intervals.
#' @param spatial.extent If specified (as a raster::Extent object), the spatial extent to plot.  Note that the statistics will *not* reflect
#'this extent
#' @param showR2 A logical, if TRUE, put the R squared values on the plots.
#' @param layout.objs A list of layout objects (see the \code{sp} package) for putting on the spatial plots.  For example,
#' continent outlines
#' @param ... Arguments to be passed to plotVegMaps when making the spatial plots.  
#' 
#' @return No return, just makes plots.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
summariseRasterComparisons <- function(runs,
                                       dataset, 
                                       label = NULL,
                                       diff.cuts = NULL,
                                       perc.diff.cuts = seq(-100,200,5),
                                       spatial.extent = NULL,
                                       showR2 = TRUE,
                                       layout.objs = NULL,
                                       ...) {
  
  message(paste("Comparing runs to ", dataset@name))
  
  # BUILD STACKS, NAMES AND PLOT TITLE LISTS FOR PLOTTING
  # the data 
  Absolute.stack <- brick(runs[[1]]@benchmarks[[dataset@id]]@data.raster)
  
  # text lists
  run.ids <- list()
  plot.titles <- list(dataset@name)
  
  # add each model run to stacks and text lists
  for(run in runs){
    
    # retrieve the comparison objec
    comparison.obj <- run@benchmarks[[dataset@id]]
    
    # add the text to the text lists
    run.ids <- append(run.ids, run@id)
    plot.titles <- append(plot.titles, run@description)
    
    # add the raster layers to the stack
    Absolute.stack <- addLayer(Absolute.stack,  comparison.obj@model.raster)
    if(exists("Difference.stack")) Difference.stack <- addLayer(Difference.stack,  comparison.obj@diff.raster)
    else Difference.stack <- brick(comparison.obj@diff.raster)
    if(exists("Percentage.Difference.stack")) Percentage.Difference.stack <- addLayer(Percentage.Difference.stack,  comparison.obj@perc.diff.raster)
    else Percentage.Difference.stack <- brick(comparison.obj@perc.diff.raster)
    
    rm(comparison.obj)
    
  }
  
  
  # CROP/EXTEND RASTERS (if extent specified)
  if(!is.null(spatial.extent)){
    Absolute.stack <- extend(crop(Absolute.stack, spatial.extent), spatial.extent)
    Difference.stack <- extend(crop(Difference.stack, spatial.extent), spatial.extent)
    Percentage.Difference.stack <- extend(crop(Percentage.Difference.stack, spatial.extent), spatial.extent)
  }
  
  
  # ASSIGN NAMES
  names(Absolute.stack) <- append(dataset@id, run.ids)
  names(Difference.stack) <- run.ids
  names(Percentage.Difference.stack) <- run.ids
  
  # PLOT ABSOLUTE VALUES
  plotVegMaps(Absolute.stack,
              quant = dataset@veg.quant, 
              period = dataset@temporal.extent, 
              summary.file.name = paste(paste("AbsoluteVs", dataset@id, sep = ""), label, sep = "."),
              special.string = "Corrected",               
              plot.labels = plot.titles,
              layout.objs = layout.objs,
              ...)
  
  # MAKE R^2 LABELS (if required)
  if(showR2){
    counter <- 0
    for(run in runs){
      counter <- counter + 1
      comparison.obj <- run@benchmarks[[dataset@id]]
      R2.val <- round(comparison.obj@R.squ, 3)
      layout.objs[[run@id]] <- list("sp.text", c(extent(Difference.stack)@xmin * 0.8, extent(Difference.stack)@ymin * 0.7), bquote(R^2 ~ "=" ~ .(R2.val)), which = counter, cex = 1.5)
      rm(comparison.obj)
    }
  }
  
  # PREPARE CUTS (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@veg.quant@cuts)[2] - range(dataset@veg.quant@cuts)[1] )/ (length(dataset@veg.quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@veg.quant@cuts)), max(dataset@veg.quant@cuts), interval)
  }
  
  
  # PLOT ABSOLUTE DIFFERENCE
  plot.titles <- plot.titles[-1]
  
  plotVegMaps(Difference.stack,
              quant = dataset@veg.quant, 
              period = dataset@temporal.extent, 
              summary.file.name = paste(paste("DiffVs", dataset@id, sep = ""), label, sep = "."),
              summary.title = paste("Simulated - ", dataset@id, " (", dataset@units, ")", sep = ""),
              special = "difference",               
              plot.labels = plot.titles,
              override.cuts = diff.cuts,
              text.multiplier = 1.0,
              layout.objs = layout.objs,
              ...)
  
  
  # PLOT PERCENTAGE DIFFERENCE
  plotVegMaps(Percentage.Difference.stack,
              quant = dataset@veg.quant, 
              period = dataset@temporal.extent, 
              summary.file.name = paste(paste("PercentageDiffVs", dataset@id, sep = ""), label, sep = "."),
              summary.title = paste("Simulated - ", dataset@id, "(percentage difference)"),
              special = "percentage.difference",               
              plot.labels = plot.titles,
              override.cuts = perc.diff.cuts,
              text.multiplier = 1.0,
              layout.objs = layout.objs,
              ...)
  
  
  # Clear up
  rm(run.ids, plot.titles, Absolute.stack, Difference.stack, Percentage.Difference.stack)
  gc()
  
  
  
  
}


###################################################################################################
####################  BIOME COMPARISIONS AND STATS ################################################
###################################################################################################

#' Kappa comparison between two biome rasters
#' 
#' Calculates a BiomeComparison object (which contains the Cohen's Kappa scores) given a stack containing two biome maps.
#' 
#' @param stack A two-layer stack containing the biomes maps (or other categorical data) represented as integer codes
#' @param id A character string to identify this comparison, typically the id of the biome scheme.
#' @param labels A vector of character strings to describe the categories over which Kappa is compared (typically a list of biomes)
#' following the order of the integer codes used in the data (see \code{stack} argument)
#' @param verbose A logical, if TRUE print out all the Kappa scores
#' 
#' @return A Biome comparison object
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
doKappa <- function(stack, 
                    id, 
                    labels = NULL, 
                    verbose = TRUE){
  
  # get the unique classes and put them in a vector with no missing values
  unique.classes <- union(unique(subset(stack,1)),unique(subset(stack,2)))
  unique.classes <- sort(unique.classes)
  unique.class.ids <- seq(unique.classes[1], unique.classes[length(unique.classes)])
  
  # assign each gridcell a code based on the classifications and get the frequency table          
  kappa.temp <- overlay(stack,  fun=function(x,y){return(1000*x + y)}, unstack=TRUE)
  freq.table <- as.data.frame(freq(kappa.temp))
  
  # make the empty kappa matrix
  kappa.matrix <- matrix(data = 0, nrow = length(unique.class.ids), ncol = length(unique.class.ids))
  
  # loop through all the entries of the matrix for the kappa calculation
  for(counter.Y in 1:length(unique.class.ids)){
    for(counter.X in 1:length(unique.class.ids)){
      
      # find the element in the freq table corresponding the position in the matrix
      position.code = 1000*counter.X + counter.Y
      position.row <- which(freq.table$value == position.code)
      
      # if it exists in the table stick it in the matric
      if(length(position.row > 0)){ kappa.matrix[counter.Y, counter.X] <-  freq.table$count[position.row]   }
      
    }
  }
  
  # now we have the kappa matrix, calculate the Cohen's Kappa
  # handy sums
  col.totals <- colSums(kappa.matrix)
  row.totals <- rowSums(kappa.matrix)
  ngridcells <- sum(colSums(kappa.matrix))
  
  # total agreement
  total.agreement <- sum(diag(kappa.matrix))
  
  # chance agreement
  chance.agreement <- 0
  for(counter in 1:length(unique.class.ids)){
    chance.agreement <- chance.agreement + (col.totals[counter] * row.totals[counter] / ngridcells)
  }
  
  #finally kappa
  kappa <- (total.agreement - chance.agreement) / (ngridcells - chance.agreement)
  
  if(is.null(labels)) labels <- paste("Class", 1:length(unique.class.ids), " ")
  
  # also calculate the per class agreement (This is ripped from TH Kappa script, I don't understand it but it gives identical results)
  per.class.kappa <- c()
  for(counter in 1:length(unique.class.ids)){
    a  <- kappa.matrix[counter, counter] / ngridcells
    p1 <- row.totals[counter] / ngridcells
    p2 <- col.totals[counter] / ngridcells
    q1 <- 1.0-p1
    q2 <- 1.0-p2
    b  <- p1-a
    c  <- p2-a
    d  <- q1-c
    per.class.kappa <- append(per.class.kappa, (a - p1*p2)/( (p1+p2)/2.0 -p1*p2))
    if(verbose) print(paste(labels[counter], round(per.class.kappa[counter], 3), sep = " "))
  }
  
  if(verbose) print(paste("Overall Kappa", round(kappa, 3), sep = " "))
  
  
  return(new("BiomeComparison",
             id = id,
             data.raster = subset(stack,2), 
             model.raster = subset(stack,1), 
             Kappa = kappa, 
             individual.Kappas = per.class.kappa)
  )
  
  
}

#' Compare a run to a biome map
#' 
#' Takes a run, a time period and a variable from which to calculate a modelled biome map, and biome scheme, 
#' and compares the model to the data
#' 
#' @param run The VegRun to be compared to the data
#' @param variable The VegQuant (or name) of the variable from which the biomes are to be derived. This is not flexible enough, 
#' and should be folded into the scheme.
#' @param period The temporal extent over which the model output should be averaged for calculating the biomes
#' @param scheme The biome scheme we are comparing. Also not flexible enough, here should provide a raster (or SpatialDataset?) 
#' containing the data
#' @param plot Logical, if true make a biome plot.
#' @param ... Additional parameters supplied to the plotting function (at time of writing the plotting function is plotBiomes(),
#'  but should be changed soon to plotVegMaps())
#'  
#' @return A Biome comparison object
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
compareBiomes <- function(run, 
                          variable, 
                          period, 
                          scheme, 
                          plot = TRUE, 
                          ...){
  
  # Prepare the veg. spatial object
  this.VegSpatial <- getVegSpatial(run, variable, period, reread.file = FALSE)
  
  #  calculate biomes from model output
  this.VegSpatial <- addBiomes(this.VegSpatial, scheme)
  
  # read expert-derived PNV biomes
  PNV.biomes <- readHandPBiomes(resolution = "HD", classification = scheme@id)
  
  # build the model biomes as a raster
  model.biomes <- promoteToRaster(this.VegSpatial, scheme@id, run@tolerance)
  
  # make the stack for the comparisons (note they needed to be cropped to the same size)
  comparison.stack <- stack(crop(model.biomes, PNV.biomes), crop(PNV.biomes, model.biomes))
  
  # calculate Kappa statistics (and possibily other similarity/dissimilarity metrics)
  Kappa.comparison <- doKappa(comparison.stack, id = scheme@id, labels = scheme@strings, verbose = TRUE)
  
  # plot biomes if requested
  if(plot){
    # plotBiomeMap(this.VegSpatial, 
    #              scheme = scheme,
    #              addData = PNV.biomes, 
    #              kappa.list = list(Kappa.comparison),
    #              ...
    # )
    
    plotVegMaps(this.VegSpatial, 
                biome.scheme = scheme, 
                special = "biomes", 
                biome.data = PNV.biomes, 
                kappa.list = list(Kappa.comparison),
                ...)
    
  }
  
  rm(PNV.biomes, model.biomes)
  return(Kappa.comparison)
  
}

#' Compare many runs to a biome map
#' 
#' Takes a list of VegRuns, looks up the relevant BiomeComparison object (which should have been pre-calculated)
#' and plots all the biome maps on one plot, including the data
#' 
#' @param runs List of VegRun objects, each run should have already had \code{compareBiomes()} for the relevant biome dataset 
#' run on it.
#' @param biome.dataset A SpatialDataset object holding the biomes to which we are comparing.
#' @param analysis.label A character string to differentiate the resulting plot from similar plots, e.g. "VariableSoilDepth"
#' @param plot.data A logical, if TRUE put the data on the plot as well as the model runs
#' @param ... Further arguments passed to the plotting function.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export

compareManyRunsToBiomes <- function(runs, 
                                    biome.dataset, 
                                    analysis.label = "", 
                                    plot.data = TRUE, 
                                    ...){
  
  # get biome scheme 
  scheme <-IDFromList(biome.dataset@id,supported.biome.schemes)
  
  # make a raster stack with all the runs
  labels <- list()
  for(run in runs){
    if(exists("biome.stack")) biome.stack <- addLayer(biome.stack, run@benchmarks[[biome.dataset@id]]@model.raster)
    else biome.stack <- brick(run@benchmarks[[biome.dataset@id]]@model.raster)
    labels <- append(labels, run@description)
  }
  
  # Sort out whether or not we want to plot that data on the graph as well
  if(plot.data){
    PNV.biomes <- biome.dataset
  }
  else{
    PNV.biomes <- NULL
  }
  
  # plotBiomeMap(biome.stack,
  #              addData = addData,
  #              targets = names(biome.stack),
  #              file.name = paste("Biomes", scheme@id, analysis.label, sep = "."),
  #              run.title = scheme@id,
  #              plot.labels = labels,
  #              ...)
 
  plotVegMaps(biome.stack, 
              targets = names(biome.stack),
              file.name = paste("Biomes", scheme@id, analysis.label, sep = "."),
              special = "biomes", 
              biome.scheme = scheme, 
              biome.data = PNV.biomes, 
              plot.labels = labels,
              summary.title = paste("Comparison of", scheme@name, "biomes", sep = " "),
              ...)
  
  
  
  
}

#' Compare runs to each other
#' 
#' Given a list of runs and an id of a spatial VegObjects, compare the specified layers between all runs, and, 
#' if a base line run is specified, also plot comaprisons relative to that
#' 
#' @param runs List of VegRun objects to be compared
#' @param veg.spatial.id Character string holding the id of the VegObject to be compared
#' @param layer The layer to be compared across all VegRuns
#' @param expand.layer Logical, if TRUE expand the layer arguments use \code{expandTargets}
#' @param base.run.id A character string to which (if specified) all the other runs are compared by plotting the difference 
#' and percentage difference relative to this base run
#' @param abs.value.cuts A numeric vector which (if specified), defines the cuts for the plot of the absolute values of each 
#' run on one figure 
#' @param plot.diff Logical, if TRUE and a base.run.id supplied, plot the absolute difference of each run relative to the base run.
#' @param diff.cuts A numeric vector which (if specified), defines the cuts for the difference plot of each run compared to the base run
#' @param plot.perc.diff Logical, if TRUE and a base.run.id supplied, plot the percentage difference of each run relative to the base run.
#' @param perc.diff.cuts A numeric vector which (if specified), defines the cuts for the percentage difference plot of each run compared to the base run
#' @param special A character string which, if specified, is used to give special plotting instructions to \code{plotVegMaps}, 
#' see the documentation of that function for details.
#' @param doIndividual Logical, if TRUE, make individual comparison plot for, say, all PFTs, as well as putting them all on one plot.
#' @param tag A character string to identify this plot/analysis in comparison to others, eg. "SoilDepthTests" or "NewSLAForumalation"  
#' @param ... Further arguments passed to the \code{plotVegMaps} function.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export


compareVegSpatialObject <- function(runs, 
                                    veg.spatial.id, 
                                    layer, 
                                    expand.layer = TRUE, 
                                    base.run.id = NULL,
                                    abs.value.cuts = NULL,
                                    plot.diff = TRUE,
                                    diff.cuts = NULL,
                                    plot.perc.diff = TRUE, 
                                    perc.diff.cuts = NULL,
                                    special = NULL,
                                    doIndividual = FALSE,
                                    tag = NULL,
                                    ...) {
  
  # To avoid NOTES
  Lon = Lat = NULL
  
  # initialise an empty data.table to hold the comparison
  comparison.dt <- data.table("Lon" = numeric(0), "Lat" = numeric(0))
  setkey(comparison.dt, Lon, Lat)
  
  # for each run
  for(run in runs){
    
    # grab the VegObject that we want from the vegRun
    temp.spatial <- run@objects[[veg.spatial.id]]
    
    # expand the layer if necessary
    if(expand.layer){ layer <- expandTargets(targets = layer, temp.spatial, run@pft.set) }
    
    # Extract the columns that we need and add them to the data.table and set the names appropriately
    comparison.dt <- comparison.dt[temp.spatial@data[,c("Lon","Lat",layer),with=FALSE]]
    setnames(comparison.dt, c(names(comparison.dt)[1:(length(names(comparison.dt))-length(layer))], paste(run@id, layer, sep = "_")))
    
    rm(temp.spatial)
    
  }
  
  
  # now plot the absolute values for each layer run on the same plot (one plot per layer)
  for(sub.layer in layer) {
    layers <- c()
    for(run in runs){
      layers <- append(layers, paste(run@id, sub.layer, sep = "_"))
    }
    plotVegMaps(comparison.dt,
                targets = layers,
                quant = run@objects[[veg.spatial.id]]@quant,
                summary.title = sub.layer,
                special.string = paste(tag, "RunComparison", sub.layer, sep = "."),
                special = special,
                override.cuts = abs.value.cuts,
                doIndividual = FALSE,
                ...
    )
  }
  
  # if a base.run is supplied, plot the the difference between each run and it
  if(!is.null(base.run.id)){
    
    # for each run that is not the base.run
    for(run in runs){
      if(run@id != base.run.id){
        
        # for each layer
        diff.titles = diff.names = c()
        perc.diff.titles = perc.diff.names = c()
        
        for(sub.layer in layer){
          
          # calculate the difference
          col.name <- paste(paste(run@id, sub.layer, sep = "_"), "minus", paste(base.run.id, sub.layer, sep = "_"), sep = ".")
          comparison.dt[, eval(col.name) := get(paste(run@id, sub.layer, sep = "_")) - get(paste(base.run.id, sub.layer, sep = "_"))]
          diff.names <- append(diff.names, col.name)
          diff.titles <- append(diff.titles, paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", IDFromList(base.run.id, runs)@description, sep = " "))
          
          # calculate the percentage difference
          col.name <- paste(paste(run@id, sub.layer, sep = "_"), "minus", paste(base.run.id, sub.layer, sep = "_"), "perc.diff", sep = ".")
          comparison.dt[, eval(col.name) := (get(paste(run@id, sub.layer, sep = "_")) - get(paste(base.run.id, sub.layer, sep = "_"))) %/0% get(paste(base.run.id, sub.layer, sep = "_")) * 100]
          perc.diff.names <- append(perc.diff.names, col.name)
          perc.diff.titles <- append(perc.diff.titles, paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", IDFromList(base.run.id, runs)@description, sep = " "))
          
        }
        
        # plot the difference
        if(plot.diff){
          
          plotVegMaps(comparison.dt,
                      targets = diff.names,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      special = "Diff",
                      summary.title = paste(paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", IDFromList(base.run.id, runs)@description, sep = " "),
                      plot.labels = diff.titles,
                      override.cuts = diff.cuts,
                      doIndividual = doIndividual,
                      special.string = tag,
                      ...)
        }
        
        # plot the percentage difference
        if(plot.perc.diff){
          
          plotVegMaps(comparison.dt,
                      targets = perc.diff.names,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      special = "Perc.Diff",
                      summary.title = paste(paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", IDFromList(base.run.id, runs)@description, "(% diff)", sep = " "),
                      limit = TRUE,
                      limits = c(-100,200),
                      plot.labels = perc.diff.titles,
                      override.cuts = perc.diff.cuts,
                      doIndividual = doIndividual,
                      special.string = tag,
                      ...)
          
        }
        
        
        
        
      }
    }
    
  } # if base run is specified  
  
}

