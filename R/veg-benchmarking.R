

#' Benchmark ModelRuns against a spatial dataset
#'
#' To use this function it is *required* that \code{compareRunToSpatialDataset()} has been performed on each of the runs with 
#' the dataset to which we are comparing.
#' 
#' @param runs A list of ModelRun objects (each with the RasterComparison object already created)
#' @param layer.name Layer in the vegetation model output to compare to the data
#' @param dataset The spatial dataset to which we are comparing (represented as a SpatialDataset object)
#' @param tag A string with which to tag the resulting plots to specify the analysis (to differentiate them from other plots),
#' for example "ForcingDatasetComparison" or "SoilDepths.  Or whatever you want.
#' @param diff.cuts Numeric vector of cuts for plotting the absolute difference.  If not specified it is derived as the maximum
#' possible range based on the Quantity object in the dataset.
#' @param perc.diff.cuts Numerice vector of cuts for plotting the percentage differences.  If not specified defaults to -100\% to +200\%
#' in 5\% intervals.
#' @param spatial.extent If specified (as a raster::Extent object), the spatial extent to plot.  Note that the statistics will *not* reflect
#'this extent
#' @param showR2 A logical, if TRUE, put the R squared values on the plots.
#' @param layout.objs A list of layout objects (see the \code{sp} package) for putting on the spatial plots.  For example,
#' continent outlines
#' @param histo.plot.range A two-value numeric vector defining the range for the histo plots
#' @param correction.dt A data.table with columns "Lon", "Lat" and "Correction" to apply a multiplicative correction to the model data before performing the benchmark 
#' to account for (for example) land cover.  NA's can be used to completely mask out areas from the comparison. 
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} dicumentation
#' @param ... Arguments to be passed to plotLayer when making the spatial plots.  
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
  

  # COMBINE LAYOUT OBJECTS FROM DIFFERENT SOURCES - MF not necessary any more
  # if(!is.null(layout.objs)){
  #   
  #   # check if there is a layout.objs already provided to which any new ones must be added
  #   if("layout.objs" %in% names(args1)){
  #     
  #     layout.objs <- append(layout.objs, args1["layout.objs"])
  #     args1[["layout.objs"]] <- NULL
  #     
  #   }
  #   
  # }
  
  
  # SET UP A SQUARE CANVAS FOR SCATTER AND HISTO PLOTS
  # set up up a square canvas
  canvas.options.square <- canvas.options
  height.width = min(canvas.options$width, canvas.options$height)
  canvas.options.square$width <- height.width
  canvas.options.square$height <- height.width
  # set up up a square canvas, with 500 pixels per plot
  canvas.options.square.scaled <- canvas.options
  height.width = ceiling(length(runs)^0.5) * 500
  canvas.options.square.scaled$width <- height.width
  canvas.options.square.scaled$height <- height.width
  
  
  
  # FIRST BENCHMARK EACH RUN SEPARATELY
  # but save the comparisons in a list
  comparison.objects.list <- list()
  
  # make a new data.table to store the data and the comparison layers 
  data.dt <- dataset@data
  
  # list of stuff for the multi-panel plots
  scatter.label.vector <- c()
  equation.label.vector <- c()
  run.ids <- c()
  run.descriptions <- c()
  run.fill.cols <- c()
  run.line.cols <- c()
  
  for(run in runs){
    
    # make the id_to_description
    run.ids <- append(run.ids, run@id)
    run.descriptions <- append(run.descriptions, run@description)
    run.fill.cols <- append(run.fill.cols, run@fill.col)
    run.line.cols <- append(run.line.cols, run@line.col)
    
    # get the ModelObject for the period 
    model.model.object <- getModelObject(run, 
                                    dataset@quant, 
                                    temporal.extent = dataset@temporal.extent,
                                    temporally.average = TRUE, 
                                    store.internally = TRUE, 
                                    write = TRUE, 
                                    read.full = FALSE)
    
    # get the layer 
    if(!(layer.name %in% names(model.model.object@data))) model.model.object <- newLayer(model.model.object, layer.name, PFT.data = run@pft.set)
    model.dt <- model.model.object@data[, c("Lat", "Lon", layer.name), with=FALSE]
    
    # if required apply the correction
    if(!is.null(correction.dt)) {
      
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
    
    # add the model output (keeping only points where there are data) and and rename the colum with the run id
    data.dt <- merge(x = round(data.dt, 3), y = round(model.dt, 3), all = FALSE)
    data.dt <- na.omit(data.dt)
    
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
    ME <- mean(abs(difference.vector), na.rm=TRUE)
    NME <- ME / mean(abs(data.vector - mean(data.vector)))
    
    # MSE, RMSE, NMSE
    MSE <- mean(difference.vector^2, na.rm=TRUE)
    NMSE <- MSE/var.observed
    RMSE <- MSE^0.5
    
    
    
    R.squ <- 1 - (MSE/var.observed)
    P.cor <- cor(data.vector, model.vector, method = "pearson")
    
    comparison.result <- new("SpatialComparison",
                             id = paste(run@id, dataset@id, sep = "."), 
                             ME = ME,
                             NME = NME,
                             RMSE = RMSE, 
                             NMSE = NMSE,
                             R.squ = R.squ, 
                             P.cor = P.cor, 
                             sd.diff = sd.diff)
    
    runs[[run@id]] <- addToModelRun(comparison.result, runs[[run@id]])
    
   
    
    # set up local arguments for plotting
    local.args <- args1
    
    # get extent for displaying R^2
    this.extent <- getExtentFromDT(data.dt)
    if(!is.null(local.args[["plot.extent"]])) {
      this.extent <- intersect(local.args[["plot.extent"]], this.extent)
      warning("Reported R^2 value in benchmarkSpatial applies to the whole spatial extent, not the smaller extent you have plotted.  If you want the R^2 for this sub-region, crop the data *before* called the benchmarking routine.")
    }
    
      # calculate R^2 position
    stats.pos.x <- (this.extent@xmax-this.extent@xmin) * 0.1 + this.extent@xmin
    stats.pos.y <- (this.extent@ymax-this.extent@ymin) * 0.1 + this.extent@ymin
    
    # make a layout.objects for the R^2
    R2.val <- round(comparison.result@R.squ, 3)
    if(is.null(local.args[["layout.objs"]])) {  
      local.args[["layout.objs"]] <- list()
    }
    local.args[["layout.objs"]][[run@id]] <- list("sp.text", txt = bquote(R^2 ~ "=" ~ .(R2.val)), loc = c(stats.pos.x,stats.pos.y), which = 1, cex = 2)
  
    
    
    ##### DIFFERENCE MAPS
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "Diff", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    print(
      do.call(plotLayer, c(list(data = data.dt,
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
      do.call(plotLayer, c(list(data = data.dt,
                                  layers = c(run@id, dataset@id),
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  run = run,
                                  summary.file.name = paste(dataset@quant@id, "comp", dataset@id, "2-up", sep = "."),
                                  tag = "Corrected",
                                  maxpixels = 1000000,
                                  plot.labels = c(run@description, dataset@name)),
                             local.args)
      )
    )
    
    dev.off()
    
    ##### HISTOS
    
    # absolute difference
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "ResidualsHisto", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options.square)) 
    
    histo.plot <- ggplot(as.data.frame(na.omit(data.dt)), aes_string(x=paste(run@id, "Error", sep = "_"))) + geom_histogram(colour = "darkgreen", fill = "white", binwidth = abs(diff.cuts[2] - diff.cuts[1]))
    histo.plot <- histo.plot + ggtitle(paste("Residuals: ",  paste(run@model,run@description,sep = " "), "-", paste(dataset@name, sep = " "))) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    histo.plot <- histo.plot + theme(text = element_text(size=30))
    histo.plot <- histo.plot +  xlab(paste("Residuals: ", paste(run@model,run@description,sep = " "), "-", dataset@name, sep = ""))   +   ylab("# gridcells")     
    histo.plot <- histo.plot + geom_vline(xintercept = 0, size = 2, colour = "red3")
    
    print(histo.plot)
    
    dev.off()
    
    
    # normalised
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "NormalisedResidualsHisto", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options.square)) 
    
    histo.plot <- ggplot(as.data.frame(na.omit(data.dt)), aes_string(x=paste(run@id, "NormError", sep = "_"))) + geom_histogram(colour = "darkgreen", fill = "white", binwidth = 0.05)
    histo.plot <- histo.plot + ggtitle(paste("Normalised Residuals: ",  paste(run@model,run@description,sep = " "), "-", paste(dataset@name, sep = " "))) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    histo.plot <- histo.plot + theme(text = element_text(size=30))
    histo.plot <- histo.plot +  xlab(paste("Normalised Residuals: ", paste(run@model,run@description,sep = " "), "-", dataset@name, sep = ""))   +   ylab("# gridcells")     
    histo.plot <- histo.plot + geom_vline(xintercept = 0, size = 1, colour = "red3")
    histo.plot <- histo.plot + xlim(-10, 10)
    
    print(histo.plot)
    
    dev.off()
    
    
    
    
    ##### SCATTER PLOT
    
    do.call(Cairo, args = append(list(file = file.path(run@run.dir, paste(dataset@quant@id, "Scatter", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options.square)) 
    
    
    scatter.plot <- ggplot(as.data.frame(na.omit(data.dt)), aes_string(x=dataset@id, y=run@id)) +  geom_point(size=3, alpha = 0.05)
    scatter.plot <- scatter.plot + theme(text = element_text(size=25))
    scatter.plot <- scatter.plot + ggtitle(paste(paste(run@description,sep = " "), "vs.", paste(dataset@name, sep = " "))) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    scatter.plot <- scatter.plot +  xlab(paste(dataset@name, dataset@quant@name, paste0("(", dataset@quant@units, ")"), sep = " "))   +   ylab(paste(run@description, dataset@quant@name, paste0("(", dataset@quant@units, ")"), sep = " "))     
    scatter.plot <- scatter.plot +  coord_cartesian(xlim = c(dataset@quant@cuts[1], dataset@quant@cuts[length(dataset@quant@cuts)]), ylim = c(dataset@quant@cuts[1], dataset@quant@cuts[length(dataset@quant@cuts)])) 
    scatter.plot <- scatter.plot + geom_abline(intercept = 0, slope = 1, size = 1.5, colour = "red3")
    scatter.plot <- scatter.plot + geom_smooth(method = "lm", se = FALSE, size = 1.5, colour = "blue3")
    
    lm.model <- stats::lm(get(run@id) ~ get(dataset@id), as.data.frame(na.omit(data.dt)))
    lm.eq <- paste("y = ", signif(stats::coef(lm.model)[2], digits = 3), "*x + ",  signif(stats::coef(lm.model)[1], digits = 3), ", R^2 =", signif(summary(lm.model)$r.squared, digits = 3), sep = "")
    scatter.plot <- scatter.plot + annotate("text", x = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.50, y = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.975, label = lm.eq, colour = "blue3", size = 10, hjust = 0, parse = FALSE)
    
    
    # add text
    label.text <- paste("NME = ", signif(NME, 3), "\nNMSE = ", signif(NMSE, 3), "\nRMSE = ", signif(RMSE, 3), "\nR^2 = ", signif(R.squ, 3), "\nPearman Corr. = ", signif(P.cor, 3), sep = "")
    scatter.plot <- scatter.plot + annotate("text", x = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.05, y = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.9, label = label.text, colour = "red3", size = 10, hjust = 0)
    
    print(scatter.plot)
    
    dev.off()
    
    # store text labels for multipanel plot
    scatter.label.vector <- append(scatter.label.vector, label.text)
    equation.label.vector <- append(equation.label.vector, lm.eq)
    
    
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
    counter <- 1
    this.extent <- getExtentFromDT(data.dt)
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
      comparison.obj <- run@benchmarks[[paste(run@id, dataset@id, sep = ".")]]
      R2.val <- round(comparison.obj@R.squ, 3)
      args1[["layout.objs"]][[run@id]] <- list("sp.text", txt = bquote(R^2 ~ "=" ~ .(R2.val)), loc = c(stats.pos.x,stats.pos.y), which = counter, cex = 2)
      
      rm(comparison.obj)
      
      # add the text to the text lists
      plot.titles <- append(plot.titles, run@description)
      
      
    }
    
    
    
    # CROP/EXTEND RASTERS (if extent specified)
    # MF: reprogram for new implemetation, need to decide exactly how to handle  
    
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Absolute", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    # PLOT ABSOLUTE VALUES
    print(
      do.call(plotLayer, c(list(data = data.dt,
                                  layers = abs.layers,
                                  quant = dataset@quant, 
                                  period = dataset@temporal.extent, 
                                  tag = "Corrected",               
                                  plot.labels = plot.titles),
                             args1)
      )
    )
    
    dev.off()
    
    
    # PLOT ABSOLUTE DIFFERENCE
    
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
      do.call(plotLayer, c(list(data = data.dt,
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
    
    
    
    
    # PLOT PERCENTAGE DIFFERENCE
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("PercentageDifference", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    # multiply the NormError_* layers by 100 to get percentage difference
    temp.dt <- data.dt[,append(c("Lon", "Lat"), perc.diff.layers), with= FALSE]
    temp.dt <- temp.dt[,(perc.diff.layers) := lapply(.SD, function(x) x * 100 ), .SDcols = perc.diff.layers]
    
    print(
      do.call(plotLayer, c(list(data = temp.dt,
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
    
    ##################################################################################################
    ##### PLOT ALL SCATTERS ON ONE CANVAS AND ALL HISTOS ON TOP OF EACH OTHER
    
    
    # Make labeller for facet_wrap
    run.labeller <- run.descriptions
    names(run.labeller) <- run.ids
    
    # MULTI-PANEL SCATTER
    
    
    # select all the layers that we want
    temp.dt <- na.omit(data.dt[, abs.layers, with = FALSE])
   
    
    temp.dt <- melt.data.table(temp.dt, id.vars = dataset@id)
    setnames(temp.dt, c(dataset@id, "Run", "Model"))
    
    scatter.label.df <- data.frame(label = scatter.label.vector, Run = abs.layers[2:length(abs.layers)] )
    equation.label.df <- data.frame(label = equation.label.vector, Run = abs.layers[2:length(abs.layers)] )
    
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Scatters", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options.square.scaled)) 
    
    scatter.plot <- ggplot(as.data.frame(na.omit(temp.dt)), aes_string(x=dataset@id, y="Model")) +  geom_point(size=3, alpha = 0.05)
    scatter.plot <- scatter.plot + facet_wrap(~ Run, labeller = as_labeller(run.labeller))
    scatter.plot <- scatter.plot + theme(text = element_text(size=25))
    scatter.plot <- scatter.plot + ggtitle(paste("All Simulations vs.", paste(dataset@name, sep = " "))) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    scatter.plot <- scatter.plot +  xlab(paste(dataset@name, layer.name, dataset@quant@name ,sep = " "))   +   ylab(paste("Simulated", dataset@quant@name, sep = " "))     
    scatter.plot <- scatter.plot +  coord_cartesian(xlim = c(dataset@quant@cuts[1], dataset@quant@cuts[length(dataset@quant@cuts)]), ylim = c(dataset@quant@cuts[1], dataset@quant@cuts[length(dataset@quant@cuts)])) 
    scatter.plot <- scatter.plot + geom_abline(intercept = 0, slope = 1, size= 1, colour = "red3")
    scatter.plot <- scatter.plot + geom_smooth(method = "lm", se = FALSE, size = 1, colour = "blue3")
    scatter.plot <- scatter.plot + geom_text(x = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.25, y = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.85, aes(label = label), data = scatter.label.df, size = 6, colour = "red3")
    scatter.plot <- scatter.plot + geom_text(x = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.75, y = (dataset@quant@cuts[length(dataset@quant@cuts)] - dataset@quant@cuts[1]) * 0.95, aes(label = label), data = equation.label.df, size = 6, colour = "blue3")
    print(scatter.plot)
    
    
    dev.off()
    
    rm(temp.dt)
    
    
    # OVERLAY HISTOS
    
    # select all the layers that we want
    temp.dt <- na.omit(data.dt[, append(diff.layers,dataset@id) , with = FALSE])
    temp.dt <- melt.data.table(temp.dt, id.vars = dataset@id)
    setnames(temp.dt, c(dataset@id, "Run", "Model"))
    for(run in runs) {
      
      
      
    }
    
    
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("ResidualHistos", "vs", dataset@id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options.square)) 
    
    
    
    histo.plot <- ggplot(as.data.frame(temp.dt), aes(x = Model, colour = Run)) 
    
   
    histo.plot <- histo.plot + geom_histogram(aes(colour = Run, fill = Run), binwidth = abs(diff.cuts[2] - diff.cuts[1]), position="identity")
    histo.plot <- histo.plot + scale_colour_manual(labels = run.descriptions, values = run.line.cols) + scale_fill_manual(labels = run.descriptions, values = run.fill.cols)
    histo.plot <- histo.plot + guides(colour = guide_legend(keywidth = 5, keyheight = 3), fill = guide_legend())

    histo.plot <- histo.plot + ggtitle(paste("Residuals: Simulations - ", dataset@name, dataset@quant@name, sep = " ")) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    histo.plot <- histo.plot + theme(text = element_text(size=30))
    histo.plot <- histo.plot + xlab(paste("Residuals: Simulations -", dataset@name, dataset@quant@name, paste0("(",dataset@quant@units,")"), sep = " "))   
    histo.plot <- histo.plot + ylab("# gridcells")     
    histo.plot <- histo.plot + geom_vline(xintercept = 0, size = 1, colour = "black")
    histo.plot <- histo.plot + xlim(diff.cuts[1], diff.cuts[length(diff.cuts)])
    #histo.plot <- histo.plot + facet_grid( Run ~ ., labeller = as_labeller(run.labeller))
    
    
    print(histo.plot)
    
    
    dev.off()
    
    rm(temp.dt)
    
    
    # Clear up
    rm(plot.titles, abs.layers, diff.layers, perc.diff.layers)
    gc()
    
    
  }
  
  
  dataset@data <- data.dt
  
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
                    id, 
                    labels = NULL, 
                    verbose = FALSE){
  
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
#' @param variable The Quantity (or name) of the variable from which the biomes are to be derived. This is not flexible enough, 
#' and should be folded into the scheme.
#' @param period The temporal extent over which the model output should be averaged for calculating the biomes
#' @param scheme The biome scheme we are comparing. Also not flexible enough, here should provide a raster (or SpatialDataset?) 
#' containing the data
#' @param biome.dataset The biomes to which you wish to comapre, as a SpatialDataset.
#' @param plot Logical, if true make a biome plot.
#' @param show.stats A logical, if TRUE, put the Kappa values on the plots.
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param ... Additional parameters supplied to the plotLayer() plotting function.
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
                          canvas.options = canvas.options,
                          ...){
  
  Lon = Lat = NULL
  
  ### HANDLE ARGUMENTS
  args1 <- list() # specify defaults here
  inargs <- list(...)
  args1[names(inargs)] <- inargs
 
  
  # align the data to be on the same grid as the model using the nearest neqbour algorthim
  
  # first rasterise the data and model
  biome.data.raster <- promoteToRaster(biome.dataset@data)
  model.data.raster <- promoteToRaster(getVegSpatial(runs[[1]], variable, period, read.full = FALSE))
  
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
    this.VegSpatial <- getVegSpatial(run, variable, period, read.full = FALSE)
    
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
        do.call(plotLayer, c(
          list(data = this.VegSpatial,
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
    
    labels <- append(labels, run@description)
    layers <- append(layers, run@id)
    
    # also compare kappas?
    
  }
  
  labels <- append(labels, biome.dataset@name)
  layers <- append(layers, biome.dataset@id)
  
  do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("Biomes", scheme@id, tag, canvas.options[["type"]], sep = "."))), 
                               canvas.options)) 
 
  
  comparisons <- NA
  if(show.stats){
    comparisons <- list.of.comparisons
  }
  
  print(
    do.call(plotLayer, c(list(data = data.dt, 
                                layers = layers,
                                summary.file.name = paste("Biomes", scheme@id, tag, sep = "."),
                                special = "biomes", 
                                biome.scheme = scheme, 
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
#' if a base line run is specified, also plot comprisons relative to that
#' 
#' @param runs List of ModelRun objects to be compared
#' @param veg.spatial.id Character string holding the id of the ModelObject to be compared
#' @param layer The layer to be compared across all ModelRuns
#' @param expand.layer Logical, if TRUE expand the layer arguments use \code{expandLayers}
#' @param base.run.id A character string to which (if specified) all the other runs are compared by plotting the difference 
#' and percentage difference relative to this base run
#' @param abs.value.cuts A numeric vector which (if specified), defines the cuts for the plot of the absolute values of each 
#' run on one figure 
#' @param plot.diff Logical, if TRUE and a base.run.id supplied, plot the absolute difference of each run relative to the base run.
#' @param diff.cuts A numeric vector which (if specified), defines the cuts for the difference plot of each run compared to the base run
#' @param plot.perc.diff Logical, if TRUE and a base.run.id supplied, plot the percentage difference of each run relative to the base run.
#' @param perc.diff.cuts A numeric vector which (if specified), defines the cuts for the percentage difference plot of each run compared to the base run
#' @param special A character string which, if specified, is used to give special plotting instructions to \code{plotLayer}, 
#' see the documentation of that function for details.
#' @param single.page Logical, if TRUE, put all "sub-layers" on one plot instead of separate individual plots.
#' @param tag A character string to identify this plot/analysis in comparison to others, eg. "SoilDepthTests" or "NewSLAForumalation"  
#' @param canvas.options A list of options (to be given to the \code{Cairo}) function to define the canvas. See the \code{Cairo} documentation
#' @param summary.plot.dir A directory (full path as a character string) so save the plots which compare many runs
#' @param ... Further arguments passed to the \code{plotLayer} function.
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
  
  # for each run
  original.layers <- layer
  for(run in runs){
    
    # grab the ModelObject that we want from the vegRun
    temp.spatial <- byIDfromList(veg.spatial.id, run@objects)
    
    # expand the layer if necessary
    if(expand.layer){layer <- expandLayers(layers = layer, input.data = temp.spatial, PFT.set = run@pft.set) }
    
    # Extract the columns that we need and add them to the data.table and set the names appropriately
    comparison.dt <- comparison.dt[temp.spatial@data[,c("Lon","Lat",layer),with=FALSE]]
    setnames(comparison.dt, c(names(comparison.dt)[1:(length(names(comparison.dt))-length(layer))], paste(run@id, layer, sep = "_")))
    
    rm(temp.spatial)
    
  }
  
  
  # now plot the absolute values for each layer run on the same plot (one plot per layer)
  all.layers <- c()
  for(sub.layer in layer) {
    layers <- c()
    for(run in runs){
      layers <- append(layers, paste(run@id, sub.layer, sep = "_"))
      all.layers  <- append(all.layers, paste(run@id, sub.layer, sep = "_"))
    }
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunComparison", sub.layer, veg.spatial.id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(plotLayer(comparison.dt,
                      layers = layers,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      title = sub.layer,
                      tag = paste(tag, "RunComparison", sub.layer, sep = "."),
                      special = special,
                      override.cuts = abs.value.cuts,
                      ...)
    )
    
    dev.off()
  }
  
  if(single.page){
    
    do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunComparison", paste(original.layers, collapse= ".", sep = ""), veg.spatial.id, tag, canvas.options[["type"]], sep = "."))), 
                                 canvas.options)) 
    
    print(plotLayer(comparison.dt,
                      layers = all.layers,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      title = paste(paste(original.layers, collapse= " ", sep = ""), run@objects[[veg.spatial.id]]@quant@name, sep = " "),
                      tag = paste(tag, "RunComparison", sub.layer, sep = "."),
                      special = special,
                      override.cuts = abs.value.cuts,
                      ...)
    )
    
    dev.off()
    
  }
  
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
          this.diff.titles <- paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " ")
          all.diff.names <- append(all.diff.names, this.diff.names)
          all.diff.titles <- append(all.diff.titles, this.diff.titles)
          
          
          # plot the difference
          if(plot.diff){
            
            do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunDifference", sub.layer, veg.spatial.id, tag, paste(run@id, base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                         canvas.options)) 
            
            print(plotLayer(comparison.dt,
                              layers = this.diff.names,
                              quant = run@objects[[veg.spatial.id]]@quant,
                              special = "Diff",
                              title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " "),
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
          this.perc.diff.titles <- paste(sub.layer, paste(run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " ")
          all.perc.diff.names <- append(all.perc.diff.names, this.perc.diff.names)
          all.perc.diff.titles <- append(all.perc.diff.titles, this.perc.diff.titles)
          
          
          # plot the percentage difference
          if(plot.perc.diff){
            
            do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunPercentageDifference", sub.layer, veg.spatial.id, tag, paste(run@id, base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                         canvas.options)) 
            
            print(plotLayer(comparison.dt,
                              layers = this.perc.diff.names,
                              quant = run@objects[[veg.spatial.id]]@quant,
                              special = "Perc.Diff",
                              title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, "(% diff)", sep = " "),
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
        
        do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunDifference", paste(original.layers, collapse= ".", sep = ""), veg.spatial.id, tag, paste(run@id, base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                     canvas.options)) 
        
        print(plotLayer(comparison.dt,
                          layers = all.diff.names,
                          quant = run@objects[[veg.spatial.id]]@quant,
                          special = "Diff",
                          title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, sep = " "),
                          plot.labels = all.diff.titles,
                          override.cuts = diff.cuts,
                          tag = tag,
                          ...)
        )
        
        dev.off()
        
      } # if plot.diff
      
      if(plot.perc.diff){
        
        do.call(Cairo, args = append(list(file = file.path(summary.plot.dir, paste("RunPercentageDifference", paste(original.layers, collapse= ".", sep = ""), veg.spatial.id, tag, paste(run@id, base.run.id, sep = "-"), canvas.options[["type"]], sep = "."))), 
                                     canvas.options)) 
        
        print(plotLayer(comparison.dt,
                          layers = all.perc.diff.names,
                          quant = run@objects[[veg.spatial.id]]@quant,
                          special = "Perc.Diff",
                          title = paste(paste(sub.layer, " ", run@objects[[veg.spatial.id]]@quant@name, ":", sep = ""), run@description, "-", byIDfromList(base.run.id, runs)@description, "(% diff)", sep = " "),
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


# plotResidualsHisto <- function(data.obj) {
#   
# }
