

#' Benchmark ModelRuns against a spatial dataset
#'
#' Take a list of model runs and compares each run to a DataObject (which should obviously have spatial dimensions).  
#' Comparsion plots to each individual run are saved in the directory for that particular run, and overall plots (simultaneously showing all the runs 
#' next to the data) are saved in the \code{summary.plor.dir} argument.
#' 
#' @param runs A list of ModelRun objects (each with the RasterComparison object already created)
#' @param layer.name Layer in the vegetation model output to compare to the data
#' @param dataset The spatial dataset to which we are comparing (represented as a DataObject object)
#' @param tag A string with which to tag the resulting plots to specify the analysis (to differentiate them from other plots),
#' for example "ForcingDatasetComparison" or "SoilDepths.  Or whatever you want.
#' @param diff.cuts Numeric vector of cuts for plotting the absolute difference.  If not specified it is derived as the maximum
#' possible range based on the Quantity object in the dataset.
#' @param perc.diff.cuts Numerice vector of cuts for plotting the percentage differences.  If not specified defaults to -100\% to +200\%
#' in 5\% intervals.
#' @param spatial.extent If specified (as a raster::Extent object), the spatial extent to plot.  Note that the statistics will *not* reflect
#'this extent
#' @param showR2 A logical, if TRUE, put the R2.eff values on the plots.
#' @param showFitLine A logical, if TRUE, put the a linear fit line on the scatter plots
#' @param histo.plot.range A two-value numeric vector defining the range for the histo plots
#' @param correction.dt A data.table with columns "Lon", "Lat" and "Correction" to apply a multiplicative correction to the model data before performing the benchmark 
#' to account for (for example) land cover.  NA's can be used to completely mask out areas from the comparison. 
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} dicumentation
#' @param ... Arguments to be passed to plotSpatial when making the spatial plots.  
#' 
#' @return Returns a DataObject, and also makes plots (in the individual run@run.dir and in the summary.plot.dir argument).
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @import data.table
#' @importFrom stats lm coef
#' @export
benchmarkSpatial <- function(runs,
                             layer.name,
                             dataset, 
                             tag = NULL,
                             diff.cuts = NULL,
                             perc.diff.cuts = seq(-100,200,5),
                             spatial.extent = NULL,
                             showR2 = TRUE,
                             showFitLine = TRUE,
                             histo.plot.range = NULL, 
                             correction.dt = NULL,
                             summary.plot.dir = NULL,
                             canvas.options = list(dpi = 72,
                                                   type = "png", 
                                                   width = 1800, 
                                                   height = 1000,
                                                   bg = "transparent"),
                             ...) {
  
  message(paste("*** Comparing runs to ", dataset@name, " ***"))
  
  Correction = Run = Model = NULL
  
  # PREAMBLE - PREPARE CUTS AND HISTO PLOT RANGE (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@quant@cuts)[2] - range(dataset@quant@cuts)[1] )/ (length(dataset@quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@quant@cuts)), max(dataset@quant@cuts), interval)
  }
  if(is.null(histo.plot.range)) histo.plot.range <- c(diff.cuts[[1]], diff.cuts[[length(diff.cuts)]])
  
  
  ### HANDLE ARGUMENTS
  args1 <- list() # specify defaults here
  inargs <- list(...)
  args1[names(inargs)] <- inargs
  
  
  # SET UP A SQUARE CANVAS FOR SCATTER AND HISTO PLOTS
  # set up up a square canvas
  canvas.options.square <- canvas.options
  height.width = min(canvas.options$width, canvas.options$height)
  canvas.options.square$width <- height.width
  canvas.options.square$height <- height.width
  # wide
  canvas.options.wide <- canvas.options.square
  canvas.options.wide$width <-  canvas.options.wide$width * 1.5
  # set up up a square canvas, with 500 pixels per plot
  canvas.options.square.scaled <- canvas.options
  height.width = ceiling(length(runs)^0.5) * 500
  canvas.options.square.scaled$width <- height.width
  canvas.options.square.scaled$height <- height.width
  
  
  
  
  ######## FIRST BENCHMARK EACH RUN SEPARATELY
  
  # make a new data.table to store the data and the comparison layers 
  data.dt <- dataset@data
  
  # list of stuff for the multi-panel plots
  run.ids <- c()
  run.names <- c()
  run.fill.cols <- c()
  run.line.cols <- c()
  
  
  do.bootstrap <- TRUE
  for(run in runs){
    
    # make the id_to_description
    run.ids <- append(run.ids, run@id)
    run.names <- append(run.names, run@name)
    run.fill.cols <- append(run.fill.cols, run@fill.col)
    run.line.cols <- append(run.line.cols, run@line.col)
    
    # get the ModelObject for the period 
    model.object <- getModelObject(run, 
                                   dataset@quant, 
                                   temporal.extent = dataset@temporal.extent,
                                   temporally.average = TRUE, 
                                   store.internally = TRUE, 
                                   write = TRUE, 
                                   read.full = TRUE)
    
    # get the layer 
    if(!(layer.name %in% names(model.object@data))) model.object <- newLayer(model.object, layer.name, PFT.data = run@pft.set)
    model.dt <- model.object@data[, c("Lat", "Lon", layer.name), with=FALSE]
    rm(model.object)
    gc()
    
    # if required apply the correction
    if(!is.null(correction.dt)  && !run@landuseSimulated) {
      
      print(paste0("Applying land use correction for ", run@id))
      
      # first add the correction data and make the new column (name it usign the run id)
      model.dt <- merge(x = round(model.dt, 3), y = round(correction.dt, 3), all = FALSE)
      model.dt[,eval(quote(run@id)):=Correction*get(layer.name),]
      
      # remove the extra columns
      model.dt[,"Correction" := NULL,with=FALSE]
      model.dt[,layer.name := NULL,with=FALSE]
      
    }
    
    # else just change the name of the column to the run ID 
    else{
      setnames(model.dt, ncol(model.dt), run@id)
    }
    
    # add the model output (keeping only points where there are data) and and rename the column with the run id
    data.dt <- merge(x = round(data.dt, 3), y = round(model.dt, 3), all = FALSE)
    data.dt <- na.omit(data.dt)
    
    calcNME <- function(data.vector, model.vector) {
      return( sum(abs(data.vector - model.vector), na.rm=TRUE) / sum(abs(data.vector - mean(data.vector)), na.rm=TRUE)) 
    }
    
    calcNashSutcliffe <- function(data.vector, model.vector) {
      return( 1 -  (sum((data.vector - model.vector)^2, na.rm=TRUE) / length(data.vector)) / stats::var(data.vector) )
    }
    
    calcR2 <- function(data.vector, model.vector) {
      return( sum( (model.vector - mean(model.vector)) * (data.vector - mean(data.vector)) )^2 / (sum( (model.vector - mean(model.vector))^2 ) * sum( (data.vector - mean(data.vector)) ^2)) )
    }
    
    
    
    # if first time comparing to this dataset, calculate the bootstrap mean
    if(do.bootstrap){
      
      do.bootstrap <- FALSE
      
      # get the data as a dt
      data.vector <- data.dt[[dataset@id]]
      
      
      NME.boot.strap.scores <- c()
      R2.boot.strap.scores <- c()
      R2.eff.boot.strap.scores <- c()
      for(iter in 1:1000){
        
        fake.data <- sample(data.vector, length(data.vector), replace = TRUE)
        
        NME <- calcNME(data.vector, fake.data)
        NME.boot.strap.scores <- append(NME.boot.strap.scores, NME)
        
        R2 <- calcR2(data.vector, fake.data)
        R2.boot.strap.scores <- append(R2.boot.strap.scores, R2)
        
        R2.eff <- calcNashSutcliffe(data.vector, fake.data)
        R2.eff.boot.strap.scores <- append(R2.eff.boot.strap.scores, R2.eff)
        
      }
      
    }
    
    print(paste("Bootstrap R2 mean = ", mean(R2.boot.strap.scores)))
    print(paste("Bootstrap R2 sd = ", sd(R2.boot.strap.scores)))
    print(paste("Bootstrap R2.eff mean = ", mean(R2.eff.boot.strap.scores)))
    print(paste("Bootstrap R2.eff sd = ", sd(R2.eff.boot.strap.scores)))
    print(paste("Bootstrap NME mean = ", mean(NME.boot.strap.scores)))
    print(paste("Bootstrap NME sd = ", sd(NME.boot.strap.scores)))
    
    
    
    
    # calculate the difference and percentage difference
    error.string <- paste(run@id, "Error", sep = "_")
    data.dt[,eval(error.string) := get(paste(run@id)) - get(paste(dataset@id))]
    norm.error.string <- paste(run@id, "NormError", sep = "_")
    data.dt[,eval(norm.error.string) := get(error.string) %/0%get(paste(dataset@id))]
    
    
    # make an additional data.table containing the comparable model and data quantities and remove the NAs
    comparison.dt <- data.dt[, .SD, .SDcols = c("Lon", "Lat", dataset@id, run@id,error.string)]
    comparison.dt <- na.omit(comparison.dt)
    
    
    #  make vectors of the difference and the data for calculations
    difference.vector <- comparison.dt[[error.string]]
    data.vector <- comparison.dt[[dataset@id]]
    model.vector <- comparison.dt[[run@id]]
    
    
    # calculated mean, SD, MSE, RMSE, R^2, Pearson correlation etc
    
    # standard deviation and variance of the data
    sd.observed <- sd(data.vector, na.rm=TRUE)
    var.observed <- sd.observed^2
    sd.diff <-sd(difference.vector, na.rm=TRUE)
    
    
    # ME and NME 
    ME <- sum(abs(data.vector - model.vector))
    NME <- calcNME(data.vector, model.vector)
    
    # MSE, RMSE, NMSE
    MSE <- mean(difference.vector^2, na.rm=TRUE)
    NMSE <- MSE / mean((data.vector - mean(data.vector))^2)
    RMSE <- MSE^0.5
    
    
    # R2 - coefficient of determination
    R2 <- calcR2(data.vector, model.vector)
    
    # R2eff - model efficiency
    R2.eff <- calcNashSutcliffe(data.vector, model.vector)
    
    
    print(paste0("R2 = ", R2))
    print(paste0("R2.eff = ", R2.eff))
    print(paste0("NME = ", NME))
    
    # Pearson product moment correlation coefficient
    P.cor <- cor(data.vector, model.vector, method = "pearson")
    
    comparison.result <- new("SpatialComparison",
                             id = paste(run@id, dataset@id, sep = "."), 
                             ME = ME,
                             NME = NME,
                             RMSE = RMSE, 
                             NMSE = NMSE,
                             R2 = R2,
                             R2.eff = R2.eff, 
                             P.cor = P.cor, 
                             sd.diff = sd.diff)
    
    # save this comparison for later
    comparisons <- dataset@comparisons
    comparisons[[run@id]] <- comparison.result
    dataset@comparisons <- comparisons
    rm(comparisons)
    
    
    
    # set up local arguments for plotting
    local.args <- args1
    
    
    # get extent for displaying R^2
    this.extent <- extent(data.dt)
    if(!is.null(local.args[["plot.extent"]])) {
      this.extent <- intersect(local.args[["plot.extent"]], this.extent)
      warning("Reported R^2 value in benchmarkSpatial applies to the whole spatial extent, not the smaller extent you have plotted.  If you want the R^2 for this sub-region, crop the data *before* called the benchmarking routine.")
    }
    
    # calculate R^2 position
    stats.pos.x <- (this.extent@xmax-this.extent@xmin) * 0.2+ this.extent@xmin
    stats.pos.y <- (this.extent@ymax-this.extent@ymin) * 0.1 + this.extent@ymin
    
    # make a layout.objects for the R^2
    R2.eff.val <- round(comparison.result@R2.eff, 3)
    R2.val <- round(comparison.result@R2, 3)
    NME.val <- round(comparison.result@NME, 3)
    if(is.null(local.args[["layout.objs"]])) {  
      local.args[["layout.objs"]] <- list()
    }
    
    
    
    ##### DIFFERENCE MAPS
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "Diff", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    local.args[["layout.objs"]][[run@id]] <- list("sp.text", txt = bquote(R^2 ~ "=" ~ .(R2.val) *  "," ~ R[eff]^2 ~ "=" ~ .(R2.eff.val) * "," ~ NME ~ "=" ~ .(NME.val)), loc = c(stats.pos.x,stats.pos.y), which = 1, cex = 2)
    
    
    
    print(
      do.call(plotSpatial, c(list(x =  data.dt,
                                  layers = paste(run@id, "Error", sep = "_"),
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  run = run,
                                  tag = "Corrected",
                                  maxpixels = 1000000,
                                  special = "diff"),
                             local.args)
      )
    )
    
    dev.off()
    
    ##### ABSOLUTE MAPS
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "Absolute", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(
      do.call(plotSpatial, c(list(x =  data.dt,
                                  layers = c(run@id, dataset@id),
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  run = run,
                                  summary.file.name = paste(dataset@quant@id, "comp", dataset@id, "2-up", sep = "."),
                                  tag = "Corrected",
                                  maxpixels = 1000000,
                                  plot.labels = c(run@name, dataset@name)),
                             local.args)
      )
    )
    
    dev.off()
    
    
    
    ###### MAKE TEMPORARY DATAOBJECT (WITH DATA FOR ONLY THIS RUN) FOR SCATTERS AND RESIDUAL PLOTS
    temp.data.obj <- dataset
    temp.data.obj@data <- data.dt[, c(dataset@id, run@id, paste(run@id, "Error", sep = "_"), paste(run@id, "NormError", sep = "_")),with=FALSE]
    
 
  
    
  } # for each run
  
  
  ### NOW THAT ALL RUNS HAVE BEEN PROCESSED UPDATE THE DATAOBJECT
  dataset@data <- data.dt
  
  
  
  ### NOW COMPARE EACH RUN TO THE BENCHMARK SIMULTANEOUSLY (USING THE COMPLETED DATAOBJECT)
  
  if(!is.null(summary.plot.dir)) {
    
    message(paste("Making summary plots of all runs vs. ", dataset@name, sep = ""))
    
    # lists for the plots
    plot.titles <- list(dataset@name)
    abs.layers <- c(dataset@id)
    diff.layers <- c()
    perc.diff.layers <- c()
    
    # add each model run to stacks and text lists
    counter <- 1
    this.extent <- extent(data.dt)
    if(!is.null(args1[["plot.extent"]])) {
      this.extent <- intersect(args1[["plot.extent"]], this.extent)
      warning("Reported R^2 value in benchmarkSpatial applies to the whole spatial extent, not the smaller extent you have plotted.  If you want the R^2 for this sub-region, crop the data *before* called the benchmarking routine.")
    }
    
    stats.pos.x <- (this.extent@xmax-this.extent@xmin) * 0.1 + this.extent@xmin
    stats.pos.y <- (this.extent@ymax-this.extent@ymin) * 0.1 + this.extent@ymin
    for(run in runs){
      
      abs.layers <- append(abs.layers, run@id)
      diff.layers <- append(diff.layers, paste(run@id, "Error", sep = "_"))
      perc.diff.layers <- append(perc.diff.layers, paste(run@id, "NormError", sep = "_"))
      
      # retrieve the comparison object for the R^2 values
      counter <- counter + 1
      comparison.obj <- byIDfromList(paste(run@id, dataset@id, sep = "."), dataset@comparisons)
      NME.val <- round(comparison.obj@NME, 3)
      args1[["layout.objs"]][[run@id]] <- list("sp.text", txt = bquote(NME ~ "=" ~ .(NME.val)), loc = c(stats.pos.x,stats.pos.y), which = counter, cex = 2)
      
      rm(comparison.obj)
      
      # add the text to the text lists
      plot.titles <- append(plot.titles, run@name)
      
    }
    
    
    
    # CROP/EXTEND RASTERS (if extent specified)
    # MF: reprogram for new implemetation, need to decide exactly how to handle  
    
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Absolute", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    # PLOT ABSOLUTE VALUES
    print(
      do.call(plotSpatial, c(list(x =  data.dt,
                                  layers = abs.layers,
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  tag = "Corrected",               
                                  plot.labels = plot.titles),
                             args1)
      )
    )
    
    dev.off()
    
    
    ##### PLOT ABSOLUTE DIFFERENCE
    
    # first remove the data from the plot title list and also decrease the plot number for the R-sqauareds 
    plot.titles <- plot.titles[-1]
    if(!is.null(args1[["layout.objs"]]))
      for(temp.layout.obj.name in names(args1[["layout.objs"]])){
        if("which" %in% names(args1[["layout.objs"]][[temp.layout.obj.name]]) ) {
          args1[["layout.objs"]][[temp.layout.obj.name]]$which <-  args1[["layout.objs"]][[temp.layout.obj.name]]$which -1
        }
      }
    
    
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Difference", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(
      do.call(plotSpatial, c(list(x =  data.dt,
                                  layers = diff.layers,
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  title = paste("Simulated - ", dataset@id, " (", dataset@quant@units, ")", sep = ""),
                                  special = "difference",               
                                  plot.labels = plot.titles,
                                  override.cuts = diff.cuts,
                                  text.multiplier = 1.0),
                             args1)
      )
    )
    dev.off()
    
    
    
    
    ##### PLOT PERCENTAGE DIFFERENCE
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("PercentageDifference", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    # multiply the NormError_* layers by 100 to get percentage difference
    temp.dt <- data.dt[,append(c("Lon", "Lat"), perc.diff.layers), with= FALSE]
    temp.dt <- temp.dt[,(perc.diff.layers) := lapply(.SD, function(x) x * 100 ), .SDcols = perc.diff.layers]
    
    print(
      do.call(plotSpatial, c(list(x =  temp.dt,
                                  layers = perc.diff.layers,
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  title = paste("Simulated - ", dataset@id, "(percentage difference)"),
                                  special = "percentage.difference",               
                                  plot.labels = plot.titles,
                                  override.cuts = perc.diff.cuts,
                                  text.multiplier = 1.0),
                             args1)
      )
    )
    
    rm(temp.dt)
    
    dev.off()
    
    
    
  
    #####  CLEAR UP
    rm(plot.titles, abs.layers, diff.layers, perc.diff.layers)
    gc()
    
    
  }
  
  
  return(dataset)
  
}




###################################################################################################
####################  BIOME COMPARISIONS AND STATS ################################################
###################################################################################################

#' Kappa comparison between two biome rasters
#' 
#' Calculates a SpatialComparison object (which contains the Cohen's Kappa scores) given a stack containing two maps of categorical data (eg. biomes, land cover classes).
#' 
#' @param dt A two-columned data.table containing the biomes (or other categorical data) represented as integer codes
#' @param x Character string giving the column name of the first of the datasets to be compared
#' @param y Character string giving the column name of the second of the datasets to be compared
#' @param id A character string to identify this comparison, typically a combination the id of the biome scheme and the id of the vegetation model run.
#' @param labels A vector of character strings to describe the categories over which Kappa is compared (typically a list of biomes)
#' following the order of the integer codes used in the data (see \code{stack} argument)
#' @param verbose A logical, if TRUE print out all the Kappa scores
#' 
#' Note that there are many other slots in a SpatialComparison object which will not be filled in the resulting object because they are for continuous as opposed to categorical data
#' 
#' @return A spatial comparison object
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
doKappa <- function(dt,
                    x = 1,
                    y = 2,
                    id, 
                    labels = NULL, 
                    verbose = FALSE){
  
  dataset1 = dataset2 = code = NULL
  
  # subset the data.table
  dt.local <- dt[,c(x,y), with = FALSE]
  
  # get the unique classes and put them in a vector with no missing values
  unique.classes <- unique(c(as.matrix(dt.local)))
  unique.classes <- sort(unique.classes)
  unique.class.ids <- seq(unique.classes[1], unique.classes[length(unique.classes)])
  
  
  # assign each gridcell a code based on the classifications and get the frequency table       
  setnames(dt.local, c("dataset1", "dataset2"))
  dt.local[, code := 1000*dataset1 + dataset2]
  freq.table <- as.data.frame(table(dt.local[,code]))
  
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
  
  
  return(new("SpatialComparison",
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
#' @param runs List of ModelRuns to be compared to the biome data
#' @param variable The variable (as a Quantity object or a name string) from which the biomes are to be derived. This is not flexible enough, 
#' and should be folded into the scheme.
#' @param period The temporal extent (as a TemporalExtent object) over which the model output should be averaged for calculating the biomes
#' @param scheme The biome scheme (as a BiomeScheme object) we are comparing. Also not flexible enough, here should provide a raster (or DataObject?) 
#' containing the data
#' @param biome.dataset The biomes to which you wish to comapre, as a DataObject.
#' @param plot Logical, if true make a biome plot.
#' @param show.stats A logical, if TRUE, put the Kappa values on the plots.
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param ... Additional parameters supplied to the plotSpatial() plotting function.
#' 
#' @param tag A character string (no spaces) used in labels and titles to differentiate these plots from similar ones.
#' For example "Corrected" or "EuropeOnly"
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} documentation
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
                          show.stats = TRUE,
                          summary.plot.dir = NULL,
                          tag = "",
                          canvas.options = list(dpi = 72,
                                                type = "png", 
                                                width = 1800, 
                                                height = 1000,
                                                bg = "transparent"),
                          ...){
  
  Lon = Lat = NULL
  
  ### HANDLE ARGUMENTS
  args1 <- list() # specify defaults here
  inargs <- list(...)
  args1[names(inargs)] <- inargs
  
  
  # align the data to be on the same grid as the model using the nearest neqbour algorthim
  
  # first rasterise the data and model
  biome.data.raster <- promoteToRaster(biome.dataset@data, tolerance = 0.01)
  model.data.raster <- promoteToRaster(getVegSpatial(runs[[1]], variable, period, read.full = FALSE, write = TRUE), tolerance = 0.01)
  
  # 
  # first check if they are on identical grids
  if(!compareRaster(biome.data.raster, model.data.raster, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=FALSE)){
    biome.data.raster <- resample(biome.data.raster, model.data.raster, method = "ngb")
  }
  
  # make the new data.table on the aligned grid
  data.dt <- data.table(as.data.frame(biome.data.raster,xy = TRUE))
  data.dt <- na.omit(data.dt)
  setnames(data.dt, append(c("Lon", "Lat"), names(data.dt)[length(names(data.dt))]))
  setkey(data.dt, Lon, Lat)
  
  
  
  # First loop through runs calculate biomes and Kappas and plot individually
  list.of.comparisons <- list()
  for(run in runs) {
    
    # Prepare the veg. spatial object
    this.VegSpatial <- getVegSpatial(run, variable, period, read.full = FALSE, write = TRUE)
    
    #  calculate biomes from model output
    this.VegSpatial <- addBiomes(this.VegSpatial, scheme)
    
    # add the modelled biomes to the dataset object
    data.dt <- merge(round(data.dt, 3), round(this.VegSpatial@data[,c("Lon", "Lat", scheme@id), with=FALSE],3), all = FALSE)
    setnames(data.dt, ncol(data.dt), run@id)
    
    # Make a comparison data.table for the statisticsd
    comparison.dt <- data.dt[,c(run@id, biome.dataset@id), with= FALSE]
    comparison.dt <- na.omit(comparison.dt)
    
    # calculate Kappa statistics (and possibily other similarity/dissimilarity metrics
    list.of.comparisons[[run@id]] <- doKappa(comparison.dt, id = scheme@id, labels = scheme@strings, verbose = TRUE)
    
    # plot biomes if requested
    if(plot){
      do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste("Biomes", this.VegSpatial@id, scheme@id, tag, canvas.options[["type"]], sep = "."))), 
                                   canvas.options)) 
      
      
      print(
        do.call(plotSpatial, c(
          list(x =  this.VegSpatial,
               biome.scheme = scheme, 
               special = "biomes", 
               biome.data = biome.dataset, 
               kappa.list = ifelse(show.stats,list(list.of.comparisons[[run@id]]), NA)
          ),
          args1)
        )
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
    
    labels <- append(labels, run@name)
    layers <- append(layers, run@id)
    
    # also compare kappas?
    
  }
  
  if(FALSE) {
    labels <- append(labels, biome.dataset@name)
    layers <- append(layers, biome.dataset@id)
  }
  
  do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Biomes", scheme@id, tag, canvas.options[["type"]], sep = "."))), 
                               canvas.options)) 
  
  
  comparisons <- NA
  if(show.stats){
    comparisons <- list.of.comparisons
  }
  
  print(
    do.call(plotSpatial, c(list(x =  data.dt, 
                                layers = layers,
                                summary.file.name = paste("Biomes", scheme@id, tag, sep = "."),
                                special = "biomes", 
                                biome.scheme = scheme, 
                                biome.data = biome.dataset, 
                                plot.labels = labels,
                                summary.title = paste("Comparison of", scheme@name, "biomes", sep = " "),
                                kappa.list = comparisons),
                           args1)
    )
  )
  
  dev.off()
  
  biome.dataset@data <- data.dt
  
  rm(comparisons, list.of.comparisons)
  
  return(biome.dataset)
  
}


#' Compare runs to each other
#' 
#' Given a list of runs and an id of a spatial ModelObjects, compare the specified layers between all runs, and, 
#' if a base line run is specified, also plot comparisons relative to that
#' 
#' @param runs List of ModelRun objects to be compared
#' @param veg.spatial.id Character string holding the id of the ModelObject to be compared
#' @param layer The (single) layer to be compared across all ModelRuns
#' @param expand.layer Logical, if TRUE expand the layer arguments use \code{expandLayers} and so actually the function will compare many layers.  
#' For example, if the layer is "seasons" and expand.layer is TRUE, the DJF, MAM, JJA and SON will all be analysed.
#' @param base.run.id A character string to which (if specified) all the other runs are compared by plotting the difference 
#' and percentage difference relative to this base run
#' @param abs.value.cuts A numeric vector which (if specified), defines the cuts for the plot of the absolute values of each 
#' run on one figure 
#' @param plot.diff Logical, if TRUE and a base.run.id supplied, plot the absolute difference of each run relative to the base run.
#' @param diff.cuts A numeric vector which (if specified), defines the cuts for the difference plot of each run compared to the base run
#' @param plot.perc.diff Logical, if TRUE and a base.run.id supplied, plot the percentage difference of each run relative to the base run.
#' @param perc.diff.cuts A numeric vector which (if specified), defines the cuts for the percentage difference plot of each run compared to the base run
#' @param special A character string which, if specified, is used to give special plotting instructions to \code{plotSpatial}, 
#' see the documentation of that function for details.
#' @param single.page Logical, if TRUE, put all "sub-layers" on one plot instead of separate individual plots.
#' @param tag A character string to identify this plot/analysis in comparison to others, eg. "SoilDepthTests" or "NewSLAForumalation"  
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} documentation
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param ... Further arguments passed to the \code{plotSpatial} function.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export


compareRuns <- function(runs, 
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
                        single.page = FALSE,
                        tag = NULL,
                        canvas.options = canvas.options,
                        summary.plot.dir = NULL,
                        ...) {
  
  # To avoid NOTES
  Lon = Lat = NULL
  
  # initialise an empty data.table to hold the comparison
  comparison.dt <- data.table("Lon" = numeric(0), "Lat" = numeric(0))
  setkey(comparison.dt, Lon, Lat)
  
  ###########################################################################
  ###### PLOT RUNS BESIDE EACH OTHER ########################################
  ###########################################################################
  
  # for each run get the layer that we wanr
  original.layer <- layer
  for(run in runs){
    
    # grab the ModelObject that we want from the ModelRun
    temp.spatial <- byIDfromList(veg.spatial.id, run@objects)
    
    # expand the layer if necessary
    if(expand.layer){layer <- expandLayers(layers = layer, input.data = temp.spatial, PFT.set = run@pft.set) }
    
    # Extract the columns that we need and add them to the data.table and set the names appropriately
    comparison.dt <- merge(comparison.dt, temp.spatial@data[,c("Lon","Lat",layer),with=FALSE], by = c("Lon", "Lat"), all = TRUE)
    
    setnames(comparison.dt, c(names(comparison.dt)[1:(length(names(comparison.dt))-length(layer))], paste(run@id, layer, sep = "_")))
    
    rm(temp.spatial)
    
  }
  
  
  # ABSOLUTE VALUES FOR EACH LAYER, SIDE-BY-SIDE (one plot per layer)
  all.layers <- c()
  for(sub.layer in layer) {
    layers <- c()
    labels <- c()
    for(run in runs){
      layers <- append(layers, paste(run@id, sub.layer, sep = "_"))
      all.layers  <- append(all.layers, paste(run@id, sub.layer, sep = "_"))
      labels  <- append(labels, run@name)
    }
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunComparison", sub.layer, veg.spatial.id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(plotSpatial(comparison.dt,
                      layers = layers,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      title = sub.layer,
                      tag = paste(tag, "RunComparison", sub.layer, sep = "."),
                      special = special,
                      override.cuts = abs.value.cuts,
                      plot.labels = labels,
                      ...)
    )
    
    dev.off()
  }
  
  # IF SELECTED PLOT ALL LAYERS FOR ALL RUNS ON A SINGLE PAGE
  if(single.page){
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunComparison", paste(original.layer, collapse= ".", sep = ""), veg.spatial.id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(plotSpatial(comparison.dt,
                      layers = all.layers,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      title = paste(paste(original.layer, collapse= " ", sep = ""), run@objects[[veg.spatial.id]]@quant@name, sep = " "),
                      tag = paste(tag, "RunComparison", sub.layer, sep = "."),
                      special = special,
                      override.cuts = abs.value.cuts,
                      ...)
    )
    
    dev.off()
    
  }
  
  ####################################################################################
  ########### PLOT DIFFERENCES TO A BASE RUN ########################################
  ####################################################################################
  
  # if a base.run is supplied, plot the the difference between each run and it
  if(!is.null(base.run.id)){
    
    # save for making an overall plot later
    all.diff.titles = all.diff.names = c()
    all.perc.diff.titles = all.perc.diff.names = c()
    
    # for each run that is not the base.run
    for(run in runs){
      
      if(run@id != base.run.id){
        
        for(sub.layer in layer){
          
          ### ABSOLUTE DIFFERENCE
          col.name <- paste(paste(run@id, sub.layer, sep = "_"), "minus", paste(base.run.id, sub.layer, sep = "_"), sep = ".")
          comparison.dt[, eval(col.name) := get(paste(run@id, sub.layer, sep = "_")) - get(paste(base.run.id, sub.layer, sep = "_"))]
          this.diff.names <- col.name
          this.diff.titles <- paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@name, "-", byIDfromList(base.run.id, runs)@name, sep = " ")
          all.diff.names <- append(all.diff.names, this.diff.names)
          all.diff.titles <- append(all.diff.titles, this.diff.titles)
          
          
          # plot the difference
          if(plot.diff){
            
            do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunDifference", sub.layer, veg.spatial.id, tag, paste(run@id, base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                         canvas.options)) 
            
            print(plotSpatial(comparison.dt,
                              layers = this.diff.names,
                              quant = run@objects[[veg.spatial.id]]@quant,
                              special = "Diff",
                              title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@name, "-", byIDfromList(base.run.id, runs)@name, sep = " "),
                              plot.labels = this.diff.titles,
                              override.cuts = diff.cuts,
                              tag = tag,
                              ...)
            )
            
            dev.off()
            
          } # if plot difference
          
          
          # PERCENTAGE DIFFERENCE
          col.name <- paste(paste(run@id, sub.layer, sep = "_"), "minus", paste(base.run.id, sub.layer, sep = "_"), "perc.diff", sep = ".")
          comparison.dt[, eval(col.name) := (get(paste(run@id, sub.layer, sep = "_")) - get(paste(base.run.id, sub.layer, sep = "_"))) %/0% get(paste(base.run.id, sub.layer, sep = "_")) * 100]
          this.perc.diff.names <- col.name
          this.perc.diff.titles <- paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@name, "-", byIDfromList(base.run.id, runs)@name, sep = " ")
          all.perc.diff.names <- append(all.perc.diff.names, this.perc.diff.names)
          all.perc.diff.titles <- append(all.perc.diff.titles, this.perc.diff.titles)
          
          
          # plot the percentage difference
          if(plot.perc.diff){
            
            do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunPercentageDifference", sub.layer, veg.spatial.id, tag, paste(run@id, base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                         canvas.options)) 
            
            print(plotSpatial(comparison.dt,
                              layers = this.perc.diff.names,
                              quant = run@objects[[veg.spatial.id]]@quant,
                              special = "Perc.Diff",
                              title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@name, "-", byIDfromList(base.run.id, runs)@name, "(% diff)", sep = " "),
                              limit = TRUE,
                              limits = c(-100,200),
                              plot.labels = this.perc.diff.titles,
                              override.cuts = perc.diff.cuts,
                              tag = tag,
                              ...)
            )
            
            dev.off()
            
          } # if plot percentage difference
          
        } # for each sublayer
        
      } # if run is not baseline run
      
    } # for each run
    
    
    # Now plot all layers together if requested
    if(single.page) {
      
      # plot the difference
      if(plot.diff){
        
        do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunDifference", paste(original.layer, collapse= ".", sep = ""), veg.spatial.id, tag, paste("All", base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                     canvas.options)) 
        
        print(plotSpatial(comparison.dt,
                          layers = all.diff.names,
                          quant = run@objects[[veg.spatial.id]]@quant,
                          special = "Diff",
                          title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), "All", "-", byIDfromList(base.run.id, runs)@name, sep = " "),
                          plot.labels = all.diff.titles,
                          override.cuts = diff.cuts,
                          tag = tag,
                          ...)
        )
        
        dev.off()
        
      } # if plot.diff
      
      if(plot.perc.diff){
        
        do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunPercentageDifference", paste(original.layer, collapse= ".", sep = ""), veg.spatial.id, tag, paste("All", base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                     canvas.options)) 
        
        print(plotSpatial(comparison.dt,
                          layers = all.perc.diff.names,
                          quant = run@objects[[veg.spatial.id]]@quant,
                          special = "Perc.Diff",
                          title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), "All", "-", byIDfromList(base.run.id, runs)@name, "(% diff)", sep = " "),
                          limit = TRUE,
                          limits = c(-100,200),
                          plot.labels = all.perc.diff.titles,
                          override.cuts = perc.diff.cuts,
                          tag = tag,
                          ...)
        )
        
        dev.off()
        
      } # if plot.perc.diff
      
    } # if single.page 
    
  } # if base run is specified  
  
} # end function





#' Make linear fit equation string
#' 
#' Makes a string (form: y = ax + b, r^2 = r^2) for putting on plots from a linear model (lm)
#' 
#' @param linear.model An object of class lm, should have been made with a simple \code{y ~ x} formula
#' 
#' @details
#' Make sure the model is \code{y ~ x} or this function doesn't really make sense 
#'
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A character string

lm_eqn <- function(linear.model) {
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(linear.model)[1], digits = 2), 
                        b = format(coef(linear.model)[2], digits = 2), 
                        r2 = format(summary(linear.model)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


calcNME <- function(vector2, vector1) {
  return( sum(abs(vector2 - vector1), na.rm=TRUE) / sum(abs(vector2 - mean(vector2)), na.rm=TRUE)) 
}

calcNashSutcliffe <- function(vector2, vector1) {
  return( 1 -  (sum((vector2 - vector1)^2, na.rm=TRUE) / length(vector2)) / stats::var(vector2) )
}

calcR2 <- function(vector2, vector1) {
  return( sum( (vector1 - mean(vector1)) * (vector2 - mean(vector2)) )^2 / (sum( (vector1 - mean(vector1))^2 ) * sum( (vector2 - mean(vector2)) ^2)) )
}


continuousComparison <- function(vector1, vector2, name1, name2, verbose = TRUE){
  
  # Preamble - remove NAs from both vectors 
  
  # first remove where there are NAs in vector1
  vector2 <- vector2[!is.na(vector1)]
  vector1 <- vector1[!is.na(vector1)]
  # now for vector2
  vector1 <- vector1[!is.na(vector2)]
  vector2 <- vector2[!is.na(vector2)]

  difference.vector <- vector1 - vector2
  
  # ME and NME 
  ME <- mean(abs(vector2 - vector1))
  NME <- calcNME(vector2, vector1)
  
  # MSE, RMSE, NMSE
  MSE <- mean(difference.vector^2, na.rm=TRUE)
  NMSE <- MSE / mean((vector2 - mean(vector2))^2)
  RMSE <- MSE^0.5

  # R2 - coefficient of determination
  R2 <- calcR2(vector2, vector1)
  
  # R2eff - model efficiency
  R2.eff <- calcNashSutcliffe(vector2, vector1)
  
  # Pearson product moment correlation coefficient
  P.cor <- cor(vector1, vector2, method = "pearson")
  
  if(verbose) {
    print(paste("+++ Stats for", name1, "vs",  name2,  "+++", sep = " "))
    print(paste("Mean Error (ME) = ", round(ME, 4)))
    print(paste("Normalised Mean Error (NME) = ", round(NME, 4)))
    print(paste("Mean Squared Error (MSE) = ", round(MSE, 4)))
    print(paste("Normalised Mean Squared Error (NMSE) = ", round(NMSE, 4)))
    print(paste("Root Mean Squared Error (RMSE) = ", round(RMSE, 4)))
    print(paste("Coefficient of Determiantion (R^2) = ", round(R2, 4)))
    print(paste("Nash-Sutcliffe Model Efficiency (R^2_eff) = ", round(R2.eff, 4)))
    print(paste("Pearson's PMCC (r) = ", round(P.cor, 4)))
  }
  
  stats <- new("SpatialComparison",
               id = paste(name1, "vs",  name2,  sep = "."),
               R2 = R2, 
               R2.eff = R2.eff,
               P.cor = P.cor,
               ME = ME, 
               NME = NME,
               NMSE = NMSE,
               RMSE = RMSE
  )
  
  
  return(stats)
  
}


proportionsComparison <- function(dt1, dt2, name1, name2, verbose = TRUE){
  
  # check the incoming data.tables are the same size
  if(ncol(dt1) != ncol(dt1)) stop("Trying to compare proportions (Manhattan Metric and Square Chord Distance) with different number of components")
  
  # calculate Manhattan Metric and Squared Chord Distance
  MM <- 0
  SCD <- 0
  for(layer.index in 1:ncol(dt1)){
    
    # for Manhattan Metric
    difference.vector <- abs(dt1[[layer.index]] - dt2[[layer.index]])
    MM <- MM + sum(difference.vector)
    
    # for Square Chord Distance
    difference.vector <- ((dt1[[layer.index]])^0.5 - (dt2[[layer.index]])^0.5)^2
    SCD <- SCD + sum(difference.vector)
    
  }
  
  MM <- MM/nrow(dt1)
  SCD <- SCD/nrow(dt1)
  
  if(verbose) {
    print(paste("+++ Stats for", name1, "vs",  name2,  "+++", sep = " "))
    print(paste("Manhattan Metric (MM) = ", round(MM, 4)))
    print(paste("Squared Chord Distance (NME) = ", round(SCD, 4)))
  }
  
  stats <- new("SpatialComparison",
               id = paste(name1, "vs",  name2,  sep = "."),
               MM = MM, 
               SCD = SCD
  )
  
  
  return(stats)
  
}

#' Kappa comparison between two biome rasters
#' 
#' Calculates a SpatialComparison object (which contains the Cohen's Kappa scores) given a stack containing two maps of categorical data (eg. biomes, land cover classes).
#' 
#' @param dt A two-columned data.table containing the biomes (or other categorical data) represented as integer codes
#' @param x Character string giving the column name of the first of the datasets to be compared
#' @param y Character string giving the column name of the second of the datasets to be compared
#' @param id A character string to identify this comparison, typically a combination the id of the biome scheme and the id of the vegetation model run.
#' @param labels A vector of character strings to describe the categories over which Kappa is compared (typically a list of biomes)
#' following the order of the integer codes used in the data (see \code{stack} argument)
#' @param verbose A logical, if TRUE print out all the Kappa scores
#' 
#' Note that there are many other slots in a SpatialComparison object which will not be filled in the resulting object because they are for continuous as opposed to categorical data
#' 
#' @return A spatial comparison object
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export    
#' stats <- categoricalComparison(vector1 = vector1, vector2 = vector2, name1 = info1@name, name2 = info2@name, verbose = verbose)

categoricalComparison<- function(vector1,
                                 vector2, 
                                 name1, 
                                 name2, 
                                 verbose = TRUE){
  
  
  dataset1 = dataset2 = code = NULL
  
  # Old south order new nothern horizon
  # make a factor with all possibilities (ie from both vector)
  all.factors <- factor(append(as.character(vector1), as.character(vector2)))
  
  # as numerics
  all.factors.numerics <- as.numeric(all.factors)
  
  # mangle that into a data.table
  dt.local <- data.table(x = all.factors.numerics[1:(length(all.factors.numerics)/2)], y = all.factors.numerics[((length(all.factors.numerics)/2)+1):length(all.factors.numerics)])
  
  # labels 
  labels <- as.numeric(unique(all.factors))
  names(labels) <- as.character(unique(all.factors))
  
  # list of unique classes
  both.vector <- as.numeric(as.character(append(vector1, vector2)))
  unique.classes <- unique(all.factors.numerics)
  unique.classes <- sort(unique.classes)
  unique.class.ids <- seq(unique.classes[1], unique.classes[length(unique.classes)])
  
  
  
  # assign each gridcell a code based on the classifications and get the frequency table       
  setnames(dt.local, c("dataset1", "dataset2"))
  dt.local[, code := 1000*dataset1 + dataset2]
  freq.table <- as.data.frame(table(dt.local[,code]))
  
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
    #per.class.kappa <- append(per.class.kappa, (a - p1*p2)/( (p1+p2)/2.0 -p1*p2))
    per.class.kappa[[names(labels[which(labels == counter)])]] <- (a - p1*p2)/( (p1+p2)/2.0 -p1*p2)
    
    if(verbose) print(paste(names(labels[which(labels == counter)]), round(per.class.kappa[counter], 3), sep = " "))
  }
  
  if(verbose) print(paste("Overall Kappa", round(kappa, 3), sep = " "))
  
  
  return(new("SpatialComparison",
             id = paste(name1, "vs",  name2,  sep = "."),
             Kappa = kappa, 
             individual.Kappas = per.class.kappa)
  )
  
  
}
