

#' Benchmark VegRuns against a spatial dataset
#'
#' To use this function it is *required* that \code{compareRunToSpatialDataset()} has been performed on each of the runs with 
#' the dataset to which we are comparing.
#' 
#' @param runs A list of VegRun objects (each with the RasterComparison object already created)
#' @param layer.name Layer in the vegetation model output to compare to the data
#' @param dataset The spatial dataset to which we are comparing (represented as a SpatialDataset object)
#' @param tag A string with which to tag the resulting plots to specify the analysis (to differentiate them from other plots),
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
#' @param histo.plot.range A two-value numeric vector defining the range for the histo plots
#' @param ignore.raster A raster used to mask out gridcells wich are not to be compared (ie any gridcells set to NA in the ignore.raster
#' are set to NA in thr model and data rasters)
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} dicumentation
#' @param ... Arguments to be passed to plotVegMaps when making the spatial plots.  
#' 
#' @return No return, just makes plots.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @import data.table
#' @export
benchmarkSpatial <- function(runs,
                             layer.name,
                             dataset, 
                             tag = NULL,
                             diff.cuts = NULL,
                             perc.diff.cuts = seq(-100,200,5),
                             spatial.extent = NULL,
                             showR2 = TRUE,
                             layout.objs = NULL,
                             histo.plot.range = NULL, 
                             ignore.raster = NULL,
                             summary.plot.dir = NULL,
                             canvas.options = list(dpi = 72,
                                                   type = "png", 
                                                   width = 1800, 
                                                   height = 1000,
                                                   bg = "transparent"),
                             ...) {
  
  message(paste("Comparing runs to ", dataset@name))
  
  
  # PREAMBLE - PREPARE CUTS AND HISTO PLOT RANGE (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@quant@cuts)[2] - range(dataset@quant@cuts)[1] )/ (length(dataset@quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@quant@cuts)), max(dataset@quant@cuts), interval)
  }
  
  if(is.null(histo.plot.range)) histo.plot.range <- c(diff.cuts[[1]], diff.cuts[[length(diff.cuts)]])
  

  # FIRST BENCHMARK EACH RUN SEPARATELY
  # but save the comparisons in a list
  comparison.objects.list <- list()
  
  # make a new data.table to store the data and the comparison layers 
  new.data.dt <- dataset@data
  
  for(run in runs){
    
    # get the VegObject for the period 
    model.vegobject <- getVegObject(run, 
                                    dataset@quant@id, 
                                    temporal.extent = dataset@temporal.extent,
                                    temporally.average = TRUE, 
                                    store.internally = TRUE, 
                                    write = TRUE, 
                                    reread.file = FALSE)
    
    # get the layer 
    if(!(layer.name %in% names(model.vegobject@data))) model.obj <- aggregateLayers(model.vegobject, layer.name, PFT.data = run@pft.set)
    
    # add the model output (keeping only points where there are data) and and rename the colum with the run id
    new.data.dt <- merge(x = new.data.dt, y = model.obj@data[, c("Lat", "Lon", layer.name), with=FALSE], all.x = TRUE, all.y = FALSE)
    setnames(new.data.dt, ncol(new.data.dt), run@id)
    
    # calculate the difference and percentage difference
    difference.string <- paste(run@id, "diff", sep = sep.char)
    new.data.dt[,eval(difference.string) := get(paste(run@id)) - get(paste(dataset@id))]
    perc.difference.string <- paste(run@id, "percdiff", sep = sep.char)
    new.data.dt[,eval(perc.difference.string) := get(difference.string) %/0%get(paste(dataset@id)) * 100]
    
    
    # make an additional data.table containing the comparable model and data quantities and remove the NAs
    comparison.dt <- new.data.dt[, .SD, .SDcols = c("Lon", "Lat", dataset@id, run@id,difference.string)]
    comparison.dt <- na.omit(comparison.dt)
    
    
    #  make vectors of the difference and the data for calculations
    difference.vector <- comparison.dt[[difference.string]]
    data.vector <- comparison.dt[[dataset@id]]
    model.vector <- comparison.dt[[run@id]]
    
    
    # calculated mean, SD, MSE, RMSE, R^2, Pearson correlation etc
    mean.diff <- mean(difference.vector, na.rm=TRUE)
    sd.diff <-sd(difference.vector, na.rm=TRUE)
    sd.observed <- sd(data.vector, na.rm=TRUE)
    var.observed <- sd.observed^2
    MSE <- mean(difference.vector^2, na.rm=TRUE)
    RMSE <- (MSE)^(0.5)
    R.squ <- 1 - (MSE/var.observed)
    P.cor <- cor(data.vector, model.vector, method = "pearson")
    
    comparison.result <- new("SpatialComparison",
                             id = paste(run@id, dataset@id, sep = "."), 
                             R.squ = R.squ, 
                             P.cor = P.cor, 
                             RMSE = RMSE, 
                             mean.diff = mean.diff, 
                             sd.diff = sd.diff)
    
    #runs[[run@id]] <- addToVegRun(comparison.result, vegrun.list[[run@id]])
    
    ##### DIFFERENCE MAPS
    
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "Diff", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    print(plotVegMaps(new.data.dt,
                layers = paste(run@id, "diff", sep = sep.char),
                quant = dataset@quant, 
                period = dataset@temporal.extent, 
                run = run,
                tag = "Corrected",
                maxpixels = 1000000,
                special = "diff",
                ...)
    )
    
    dev.off()
    
    ##### ABSOLUTE MAPS
    
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "Absolute", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(plotVegMaps(new.data.dt,
                layers = c(run@id, dataset@id),
                quant = dataset@quant, 
                period = dataset@temporal.extent, 
                doSummary = TRUE, 
                doIndividual = TRUE, 
                run = run,
                summary.file.name = paste(dataset@quant@id, "comp", dataset@id, "2-up", sep = "."),
                tag = "Corrected",
                maxpixels = 1000000,
                plot.labels = c(run@description, dataset@name),
                ...)
    )
    
    dev.off()
    
    ## RECODE LATER
    
    # ##### HISTOS
    # if(doHistoPlots){
    #   
    #   plotHistoComparison(model = comparison.results@model.raster, 
    #                       data = comparison.results@data.raster, 
    #                       stat.results = comparison.results,
    #                       run = vegobject@run, 
    #                       data.name = dataset@id, 
    #                       quant = dataset@quant, 
    #                       plot.range = histo.plot.range)
    #   
    # }
    # 
    # ##### SCATTER PLOT
    # if(doScatterPlots){
    #   
    #   plotScatterComparison(model = comparison.results@model.raster, 
    #                         data = comparison.results@data.raster,  
    #                         stat.results = comparison.results,
    #                         run = vegobject@run, 
    #                         data.name = dataset@id, 
    #                         quant = dataset@quant)
    #   
    #   
    # }
    
  }
  
  
  ### NOW COMPARE EACH RUN TO THE BENCHMARK SIMULTANEOUSLY
  
  if(!is.null(summary.plot.dir)) {
    
    message(paste("Making summary plots of all runs vs. ", dataset@name, sep = ""))
    
    # lists for the plots
    plot.titles <- list(dataset@name)
    abs.layers <- c(dataset@id)
    diff.layers <- c()
    perc.diff.layers <- c()
    
    # add each model run to stacks and text lists
    counter <- 0
    layout.obs.without.Rsquared <- layout.objs
    for(run in runs){
      
      abs.layers <- append(abs.layers, run@id)
      diff.layers <- append(diff.layers, paste(run@id, "diff", sep = sep.char))
      perc.diff.layers <- append(perc.diff.layers, paste(run@id, "percdiff", sep = sep.char))
      
      # retrieve the comparison object for the R^2 values
      counter <- counter + 1
      comparison.obj <- run@benchmarks[[paste(run@id, dataset@id, sep = ".")]]
      R2.val <- round(comparison.obj@R.squ, 3)
      layout.objs[[run@id]] <- list("sp.text", c(dataset@spatial.extent@extent@xmin * 0.8, dataset@spatial.extent@extent@ymin * 0.7), bquote(R^2 ~ "=" ~ .(R2.val)), which = counter, cex = 1.5)
      rm(comparison.obj)
      
      # add the text to the text lists
      plot.titles <- append(plot.titles, run@description)
      
      
    }
    
    # CROP/EXTEND RASTERS (if extent specified)
    # MF: reprogram for new implemetation, need to decide exactly how to handle  
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Absolute", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    # PLOT ABSOLUTE VALUES
    print(plotVegMaps(new.data.dt,
                layers = abs.layers,
                quant = dataset@quant, 
                period = dataset@temporal.extent, 
                tag = "Corrected",               
                plot.labels = plot.titles,
                layout.objs = layout.obs.without.Rsquared,
                ...)
    )
    
    dev.off()
    
    
    # PLOT ABSOLUTE DIFFERENCE
    plot.titles <- plot.titles[-1]
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Difference", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                canvas.options)) 
    
    print(plotVegMaps(new.data.dt,
                layers = diff.layers,
                quant = dataset@quant, 
                period = dataset@temporal.extent, 
                title = paste("Simulated - ", dataset@id, " (", dataset@quant@units, ")", sep = ""),
                special = "difference",               
                plot.labels = plot.titles,
                override.cuts = diff.cuts,
                text.multiplier = 1.0,
                layout.objs = layout.objs,
                ...)
    )
    
    dev.off()
    
    
    
    
    # PLOT PERCENTAGE DIFFERENCE
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("PercentageDifference", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(plotVegMaps(new.data.dt,
                layers = perc.diff.layers,
                quant = dataset@quant, 
                period = dataset@temporal.extent, 
                title = paste("Simulated - ", dataset@id, "(percentage difference)"),
                special = "percentage.difference",               
                plot.labels = plot.titles,
                override.cuts = perc.diff.cuts,
                text.multiplier = 1.0,
                layout.objs = layout.objs,
                ...)
    )
    
    dev.off()
    
    
    # Clear up
    rm(plot.titles, abs.layers, diff.layers, perc.diff.layers)
    gc()
    
    
  }
  
  
  dataset@data <- new.data.dt
  
  return(dataset)
  
}




###################################################################################################
####################  BIOME COMPARISIONS AND STATS ################################################
###################################################################################################

#' Kappa comparison between two biome rasters
#' 
#' Calculates a BiomeComparison object (which contains the Cohen's Kappa scores) given a stack containing two biome maps.
#' 
#' @param dt A two-columned data.table containing the biomes (or other categorical data) represented as integer codes
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
doKappa <- function(dt, 
                    id, 
                    labels = NULL, 
                    verbose = TRUE){
  
  x = y = code = NULL
  
  # get the unique classes and put them in a vector with no missing values
  unique.classes <- unique(c(as.matrix(dt)))
  unique.classes <- sort(unique.classes)
  unique.class.ids <- seq(unique.classes[1], unique.classes[length(unique.classes)])
  
  
  # assign each gridcell a code based on the classifications and get the frequency table       
  setnames(dt, c("x", "y"))
  dt[, code := 1000*x + y]
  freq.table <- as.data.frame(table(dt[,code]))
  
  # make the empty kappa matrix
  kappa.matrix <- matrix(data = 0, nrow = length(unique.class.ids), ncol = length(unique.class.ids))
  
  # loop through all the entries of the matrix for the kappa calculation
  for(counter.Y in 1:length(unique.class.ids)){
    for(counter.X in 1:length(unique.class.ids)){
      
      # find the element in the freq table corresponding the position in the matrix
      position.code = 1000*counter.X + counter.Y
      position.row <- which(freq.table$Var1 == position.code)
      
      # if it exists in the table stick it in the matric
      if(length(position.row > 0)){ kappa.matrix[counter.Y, counter.X] <-  freq.table$Freq[position.row]   }
      
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
             Kappa = kappa, 
             individual.Kappas = per.class.kappa)
  )
  
  
}

#' Compare a run to a biome map
#' 
#' Takes a run, a time period and a variable from which to calculate a modelled biome map, and biome scheme, 
#' and compares the model to the data
#' 
#' @param runs List of VegRuns to be compared to the biome data
#' @param variable The VegQuant (or name) of the variable from which the biomes are to be derived. This is not flexible enough, 
#' and should be folded into the scheme.
#' @param period The temporal extent over which the model output should be averaged for calculating the biomes
#' @param scheme The biome scheme we are comparing. Also not flexible enough, here should provide a raster (or SpatialDataset?) 
#' containing the data
#' @param biome.dataset The biomes to which you wish to comapre, as a SpatialDataset.
#' @param plot Logical, if true make a biome plot.
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param ... Additional parameters supplied to the plotting function (at time of writing the plotting function is plotBiomes(),
#'  but should be changed soon to plotVegMaps())
#'  
#' @param tag A character string (no spaces) used in labels and titles to differentiate these plots from similar ones.
#' For example "Corrected" or "EuropeOnly"
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} dicumentation
#' @return A Biome comparison object
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
compareBiomes <- function(runs, 
                          variable, 
                          period, 
                          scheme, 
                          biome.dataset, 
                          plot = TRUE, 
                          summary.plot.dir = NULL,
                          tag = "",
                          canvas.options = canvas.options,
                          ...){
  
  
  # First loop through runs calculate biomes and Kappas and plot individually
  
  
  list.of.comparisons <- list()
  
  # new data
  new.data.dt <- biome.dataset@data
  
  for(run in runs) {
    
    # Prepare the veg. spatial object
    this.VegSpatial <- getVegSpatial(run, variable, period, reread.file = FALSE)
    
    #  calculate biomes from model output
    this.VegSpatial <- addBiomes(this.VegSpatial, scheme)
    
    # add the modelled biomes to the dataset object
    new.data.dt <- merge(new.data.dt, this.VegSpatial@data[,c("Lon", "Lat", scheme@id), with=FALSE], all.x = TRUE, all.y = FALSE)
    setnames(new.data.dt, ncol(new.data.dt), run@id)
    
    # Make a comparison data.table for the statisticsd
    comparison.dt <- new.data.dt[,c(run@id, biome.dataset@id), with= FALSE]
    comparison.dt <- na.omit(comparison.dt)
    
    # calculate Kappa statistics (and possibily other similarity/dissimilarity metrics)
    list.of.comparisons[[run@id]] <- doKappa(comparison.dt, id = scheme@id, labels = scheme@strings, verbose = TRUE)
    
    # plot biomes if requested
    if(plot){
      do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste("Biomes", this.VegSpatial@id, scheme@id, tag, canvas.options[["type"]], sep = "."))), 
                                   canvas.options)) 
      print(plotVegMaps(this.VegSpatial, 
                        biome.scheme = scheme, 
                        special = "biomes", 
                        biome.data = biome.dataset, 
                        kappa.list = list(list.of.comparisons[[run@id]]),
                        ...)
      )
      
      dev.off()
    }
    
    rm(this.VegSpatial, comparison.dt)
    
  }
  
  # Second plot all biome maps in one figure
  
  # make a raster stack with all the runs
  labels <- c()
  layers <- c()
  for(run in runs){
    
    labels <- append(labels, run@description)
    layers <- append(layers, run@id)
    
    comparison.obj <- list.of.comparisons[[run@id]]
    # also compare kappas?
    
  }

  labels <- append(labels, biome.dataset@name)
  layers <- append(layers, biome.dataset@id)

  do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Biomes", scheme@id, tag, canvas.options[["type"]], sep = "."))), 
                               canvas.options)) 
  print(plotVegMaps(new.data.dt, 
                    layers = layers,
                    summary.file.name = paste("Biomes", scheme@id, tag, sep = "."),
                    special = "biomes", 
                    biome.scheme = scheme, 
                    plot.labels = labels,
                    summary.title = paste("Comparison of", scheme@name, "biomes", sep = " "),
                    ...)
  )
  
  dev.off()

  biome.dataset@data <- new.data.dt
  
  return(biome.dataset)
    
}


#' Compare runs to each other
#' 
#' Given a list of runs and an id of a spatial VegObjects, compare the specified layers between all runs, and, 
#' if a base line run is specified, also plot comaprisons relative to that
#' 
#' @param runs List of VegRun objects to be compared
#' @param veg.spatial.id Character string holding the id of the VegObject to be compared
#' @param layer The layer to be compared across all VegRuns
#' @param expand.layer Logical, if TRUE expand the layer arguments use \code{expandLayers}
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
                                    special = "none",
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
    temp.spatial <- byIDfromList(veg.spatial.id, run@objects)
    
    # expand the layer if necessary
    if(expand.layer){ layer <- expandLayers(layers = layer, input.data = temp.spatial, PFT.set = run@pft.set) }
    
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
    print(plotVegMaps(comparison.dt,
                layers = layers,
                quant = run@objects[[veg.spatial.id]]@quant,
                summary.title = sub.layer,
                tag = paste(tag, "RunComparison", sub.layer, sep = "."),
                special = special,
                override.cuts = abs.value.cuts,
                doIndividual = FALSE,
                ...)
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
          diff.titles <- append(diff.titles, paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " "))
          
          # calculate the percentage difference
          col.name <- paste(paste(run@id, sub.layer, sep = "_"), "minus", paste(base.run.id, sub.layer, sep = "_"), "perc.diff", sep = ".")
          comparison.dt[, eval(col.name) := (get(paste(run@id, sub.layer, sep = "_")) - get(paste(base.run.id, sub.layer, sep = "_"))) %/0% get(paste(base.run.id, sub.layer, sep = "_")) * 100]
          perc.diff.names <- append(perc.diff.names, col.name)
          perc.diff.titles <- append(perc.diff.titles, paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " "))
          
        }
        
        # plot the difference
        if(plot.diff){
          
          print(plotVegMaps(comparison.dt,
                      layers = diff.names,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      special = "Diff",
                      title = paste(paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " "),
                      plot.labels = diff.titles,
                      override.cuts = diff.cuts,
                      doIndividual = doIndividual,
                      tag = tag,
                      ...)
          )
        }
        
        # plot the percentage difference
        if(plot.perc.diff){
          
          print(plotVegMaps(comparison.dt,
                      layers = perc.diff.names,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      special = "Perc.Diff",
                      title = paste(paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, "(% diff)", sep = " "),
                      limit = TRUE,
                      limits = c(-100,200),
                      plot.labels = perc.diff.titles,
                      override.cuts = perc.diff.cuts,
                      doIndividual = doIndividual,
                      tag = tag,
                      ...)
          )
          
        }
        
        
        
        
      }
    }
    
  } # if base run is specified  
  
}

