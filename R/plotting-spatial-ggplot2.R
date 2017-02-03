#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################


#' Plots a map from a temporally-averaged \code{ModelObject}, a data.table, a Raster* or a SpatialPixelsDataFrame.
#' 
#' This is a heavy lifting function for plotting models variables with flexibly, but with a high degree of automation. It's main use is to plot a map from a \code{ModelObject} or a \code{DataObject}, although plotting a Raster* is also useful.
#' It has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults,
#' so that in the case of plotting a \code{ModelObject} all you *need* to supply is the data itself, everything else is either set to a sensible default,
#' or provided by the \code{ModelObject} itself.  It is basically a very complex wrapper for spplot, and can automatically plot things like biomes, dominant PFTs, months of maximum values, 
#' burnt fraction on an approximately logarithic scale etc.  It returns a plot, which will need to be displayed using a \code{print()} command. 
#'
#' @param data The data to plot. Can be a ModelObject, data.table, a SpatialPixelsDataFrame or a Raster* object.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param expand.layers A boolean, determines wether to expand the layers arguement.  See documentation for \code{expandLayers} for details.
#' @param period The time period (represented by a \code{TemporalExtent} object), used only for plot labels and filenames.   
#' In the case of plotting a \code{ModelRun} object this is taken automatically from the object, but this provides an override.
#' @param quant A \code{Quantity} object describy the quantity to be plotted.  This provides an override \code{Quantity} when plotting a \code{ModelObject}
#' and is useful to specify metadata (colours, plot ranges, names, etc.) when plotting other objects.
#' @param run A \code{ModelRun} object from which to pull metadata.  Note that normally this information is stored in the \code{ModelObject}. 
#' @param PFT.set A PFT set, necessary for expanding layers and plotting long names.  Normally taken from the \code{ModelObject}.
#' @param title A character string to override the default title.
#' @param layout.objs List of overlays (for example coastlines or rivers or statistical values) or other objects to be plotted by \code{spplot} 
#' so see there for how to build them.
#' @param plot.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param plot.bg.col Colour string for the plot background.
#' @param useLongnames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. 
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa
#' @param plot.extent An extent object to limit the plot area.
#' @param limit Boolean, whether or not to limit the plotted values between a range, either the limits argument below,
#' or the range of the plot.
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values
#' @param override.cols A colour palette function to override the defaults
#' @param override.cuts Cut ranges (a numeric vector) to override the defaults.
#' @param special A character string to indicate certain "special modes" which modifies the behaviour of the function a bit.
#' Special modes are currectly "fraction", "difference", "percentage.difference", "firert" (fire return time) and "dominant.pft".
#' @param maxpixels Maximum number of pixels to plot (see \code{raster} version of \code{spplot})
#' @param ... Extra arguments to be be passed to \code{spplot} and therefore also to \code{lattice::levelplot}.
#' 
#' @details  This function is heavily used by the benchmarking functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' However, the \code{plotGGSpatial} function makes pretty plots with a simpler syntax, but with less flexibility.
#' 
#' The function works best for \code{ModelObjects} (which contain a lot of useful metadata).   
#' 
#' @return Returns a plot object (from spplot)
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @importFrom Cairo Cairo
#' @import raster data.table
#' @export 
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotSpatial2 <- function(data, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a ModelObject
                        layers = NULL,
                        expand.layers = TRUE,
                        quant = NULL, 
                        period = NULL, 
                        run = NULL, 
                        PFT.set = lpj.global.PFTs,
                        title = NULL,
                        layout.objs = NULL, 
                        plot.labels =  NULL,
                        plot.bg.col =  "transparent",
                        useLongnames = FALSE,
                        text.multiplier = 1,
                        plot.extent = NULL,
                        limit = FALSE,
                        limits = NULL,
                        override.cols = NULL,
                        override.cuts = NULL,
                        special = "none",
                        maxpixels = 1E6,
                        ...){
  
  ###################################################################################################
  ### PRE-AMBLE:                                                             ########################
  ### 1. INITIAL ARGUMENT CHECKS AND WARNINGS ABOUT ARGUMENT  COMBINATIONS   ########################
  ### 2. WHEN THINGS ARE NOT SPECIFIED, PULL SOME DEFAULTS                   ########################
  ###################################################################################################
  
  ##### 1. ARGUMENTS, WARNINGS
  
  ### HANDLE SPECIAL
  
  if(!is.null(special)) special <- tolower(special)
  
  special <- match.arg(special, 
                       c("none",
                         "difference", 
                         "percentage.difference",
                         "perc.diff",
                         "fraction",
                         "burnt.area",
                         "burnt.fraction",
                         "firert",
                         "fire.return.time",
                         "dominant.pft"))
  
  special <- switch(special,
                    none = "none", 
                    difference = "difference", 
                    percentage.difference  = "percentage.difference",
                    perc.diff= "percentage.difference",
                    fraction = "fraction",
                    burnt.area = "burnt.area",
                    burnt.fraction = "burnt.fraction",
                    firert = "fire.return.time",
                    fire.return.time = "fire.return.time",
                    dominant.pft = "dominant" )
  
  # Warn ig quant is being ignored in special
  if(!is.null(quant) & special == "dominant")
    warning(paste("When using a \"special\" = ", special, ", argument \"quant\" is ignored"))
  
  
  
  
  ##### 2. DEFAULTS
  
  ### IF IS VEGOBJECT MANY THINGS ARE AVAILABLE FROM IT
  if(is.ModelObject(data)){
    
    if(data@is.temporally.averaged){
      run <- data@run
      run.id <- run@id
      period <- data@temporal.extent
      PFT.set <- run@pft.set
      if(is.null(quant)) quant <- data@quant
    } 
    else {
      stop("plotSpatial:: trying to spatially plot a ModelObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
    }
    
  }
  else if(is.DataObject(data)){
    
    #if(data@is.temporally.averaged){
          if(is.null(quant)) quant <- data@quant
    #} 
    #else {
    #  stop("plotSpatial:: trying to spatially plot a ModelObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
    #}
    
    if(!is.null(run)) run.id <- run@id
    else run.id <- NULL
    
  }
  else{
    
    print(paste("ATTENTION:  plotSpatial() NOT running on a ModelObject or DataObject, plotting an object of class ", class(data)[1]))
    warning(paste("ATTENTION:  plotSpatial() NOT running on a ModelObject or DataObject, plotting an object of class ", class(data)[1]))
    
    if(is.null(quant)) {
      
      quant <- new("Quantity",
                   id = "generic",
                   type = "unknown",
                   name = "Generic",
                   units = "unknown",
                   colours = fields::tim.colors, 
                   cuts = seq(0,1,0.05),
                   model = "none")
    } 
    
    if(!is.null(run)) run.id <- run@id
    else run.id <- NULL
  }
  
  ### CHECK FOR SPECIAL VARIABLES FOR NICER PLOTTING
  if(special == "none" | is.null(special)){
 
    if(quant@id == "burntfraction") special <- "burnt.fraction"
 
  }
  
  
  ### COLORKEY - standard, updated by specials below
  if(!is.null(quant)) colours <- quant@colours
  else colours <- fields::tim.colors
  colorkey.list <- list(space = "right", 
                        col = colours, 
                        labels = list(cex = 3 * text.multiplier)
  )
  if(!is.null(quant)) {
    if(tolower(quant@type) == "categorical") {
      colorkey.list[["labels"]] <- list(cex = 2 * text.multiplier,
                                        labels = rev(quant@units),
                                        at = (0:(length(quant@units)-1)) + 0.5)
   
     colorkey.list[["col"]] <- colorRampPalette(rev(colorkey.list$col(length(quant@units))))
    }
  }
  
  
  #####################################################################################
  ############# PREPARE DATA AND layer LIST FOR PLOTTING        ######################
  #####################################################################################
  
  ### TOLERANCE - for when making grid to rasterise
  if(is.null(run)) { tolerance <- 0.01 }
  else {tolerance <- run@tolerance}  
  
  ### SPECIAL CASE OF DOMINANT WITH NULL layer
  if(is.null(layers) & !is.null(special)){
    if(special == "dominant") layers = "Dominant"
  }
  
  ### EXPAND layerS
  if(is.null(layers)) {
    if(is.ModelObject(data)) layers <- names(data@data)
    else layers <- names(data)
  }
  if(expand.layers) {
    layers <- expandLayers(layers, data, PFT.set)
    if(!is.null(special)){
      if(special == "fraction") layers <- paste(layers, "Fraction", sep = sep.char)
    }
  }
  
  ### PROMOTE TO RASTER AND SANITISE NAMES - also make plot labels (if necessary) before the sanitatisation 
  original.layers <- layers # save for building plot label later
  data.toplot <- promoteToRaster(data, layers, tolerance)
  layers <- names(data.toplot) # update layers to be the names of the raster layers  (which might have changed)
  
  ### CROP THE DATA IF PLOT EXTENT HAS BEEN SPECIFIED
  if(!is.null(plot.extent)){ data.toplot <- crop(data.toplot, plot.extent)}
  
  
  #####################################################################################
  ############# DEAL WITH SPECIAL CASES    ############################################
  #####################################################################################
  
  ### IF SPECIALS 
  
  
  ### PLOT DIFFERENCE MAPS
  if(special == "difference"){
    
    minmax <- max(quant@cuts) - min(quant@cuts)
    step <- (max(quant@cuts) - min(quant@cuts)) / (length(quant@cuts) - 1)
    quant@cuts <- seq(from = -minmax, to = minmax, by = step)
    quant@colours <- colorRampPalette(c("green","blue","white","red", "yellow"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    quant@id <-  paste(quant@id, "diff", sep =".")
    quant@name = paste("Difference: ", quant@name, sep = "")
    plot.bg.col <- "grey"
    
  }
  
  ### PLOT PERCENTAGE DIFFERENCE MAPS
  else if(special == "percentage.difference"){
    
    quant@cuts <- seq(from = -100, to = 200, by = 10)
    quant@colours <- colorRampPalette(c("blue","white","red", "yellow"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    quant@id <-  paste(quant@id, "percdiff", sep =".")
    quant@name = paste("Percentage Difference: ", quant@name, sep = "")
    plot.bg.col <- "grey"
    
    # SET THE INTERVALS (using either these sensible options or the overrides)
    # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
    if(!is.null(override.cols)) {quant@colours <- override.cols}
    if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
    override.cols = override.cuts = NULL
    
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    # get lowest and highest '50's
    smallest.limit = min(abs(quant@cuts[1]), abs(quant@cuts[length(quant@cuts)]))
    interval <- 50
    if(smallest.limit < 100) interval <- 25
    if(smallest.limit < 50) interval <- 10
    if(smallest.limit < 20) interval <- 5
    if(smallest.limit < 10) interval <- 1
    lower <- ceiling(quant@cuts[1]/interval) * interval
    upper <- floor(quant@cuts[length(quant@cuts)]/interval) * interval
    colourkey.at <- seq(lower, upper, by = interval)
    colorkey.labels <- paste(seq(lower, upper, by = interval))
    for(label.index in 1:length(colorkey.labels)){
      if(!(substr(colorkey.labels[label.index], 1, 1) == "-" | substr(colorkey.labels[label.index], 1, 1) == "0")){
        colorkey.labels[label.index] <- paste0("+", colorkey.labels[label.index])
      }
    }
    if(limit) colorkey.labels[length(colorkey.labels)] <- paste0("\u2265", colorkey.labels[length(colorkey.labels)])
    colorkey.labels <- paste(colorkey.labels, "%", sep = "")
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = colourkey.at)
    colorkey.list[["at"]] <- quant@cuts
    
    
    
  }
  
  #### PLOT FRACTION MAPS
  else if(special == "fraction"){
    
    quant@cuts <- seq(from = 0, to = 1, by = 0.05)
    quant@id <- paste(quant@id, "fraction", sep = ".")
    quant@name <- paste(quant@name, "Fraction", sep = " ")
    quant@colours <- colorRampPalette(c("grey85", "black"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    
  }
  
  #### PLOT BURNT FRACTION
  else if(special == "burnt.fraction"){
    
    # SET THE INTERVALS (using either these sensible options or the overrides)
    quant@cuts <- c(0, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
    quant@colours <-  colorRampPalette(rev(c("red4", "red","orange","yellow", "palegreen2", "cyan", "dodgerblue2", "blue", "midnightblue")))
    
    # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
    if(!is.null(override.cols)) {quant@colours <- override.cols}
    if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
    override.cols = override.cuts = NULL
    
    # RECLASSIFY THE DATA ACCORDING TO THE CUTS 
    temp.names <- names(data.toplot)
    data.toplot <- cut(data.toplot, quant@cuts, include.lower = TRUE, right = FALSE) 
    names(data.toplot) <- temp.names
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.labels <- paste(quant@cuts)
    colorkey.labels[length(colorkey.labels)] <- paste0(colorkey.labels[length(colorkey.labels)], "+")
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = 0:(length(quant@cuts)-1))
    colorkey.list[["at"]] <- 0:(length(quant@cuts)-1)
    colorkey.list[["col"]] <- quant@colours
    quant@cuts = 0:(length(quant@cuts)-1)
    
  }
  
  #### PLOT BURNT FRACTION
  else if(special == "burnt.area"){
    
    
    stop("plotSpatial: special burnt.fraction or ba not impletemted yet")
    
  }
  
  #### PLOT FIRE RETURN TIME
  else if(special == "fire.return.time" | quant@id == "firert"){
    
    # SET THE INTERVALS (using either these sensible options or the overrides)
    quant@cuts <- c(0, 1, 3, 5, 10, 25, 50, 100, 200, 400, 800, 1000)
    quant@colours <-  colorRampPalette(c("black", "red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3"))
    # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
    if(!is.null(override.cols)) {quant@colours <- override.cols}
    if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
    override.cols = override.cuts = NULL
    
    # RECLASSIFY THE DATA ACCORDING TO THE CUTS 
    temp.names <- names(data.toplot)
    data.toplot <- cut(data.toplot, quant@cuts) 
    names(data.toplot) <- temp.names
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.labels <- paste(quant@cuts)
    colorkey.labels[length(colorkey.labels)] <- paste0(colorkey.labels[length(colorkey.labels)], "+")
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = 0:(length(quant@cuts)-1))
    colorkey.list[["at"]] <- 0:(length(quant@cuts)-1)
    quant@cuts = 0:(length(quant@cuts)-1)
    
  }
 
  
  #### PLOT DOMINANT
  
  else if(special == "dominant"){
    
    
    quant.id <- quant@id
    
    # Get Raster Attribute Table and then convert raster back to simple integers instead of factors 
    # because currently (May 2016) raster package doesn't handle factor rasters very well
    RAT <- data.toplot@data@attributes[[1]]
    data.toplot <- deratify(data.toplot, complete = TRUE)
    
    # Set colours to be the list of PFT colours
    col.list <- c()
    label.list <- c()
    
    if(tolower(quant@type) == "pft") {
      
      # for each row of the RAT
      for(row.index in 1:NROW(RAT)){
        
        # get the row
        row <- RAT[row.index,]
        
        # special case if barren
        if(as.character(row$levels == "Barren")) {
          col.list <- append(col.list, "gray75")
          label.list <- append(label.list, "Barren")
        }
        
        # else (assuming others rows are PFTs)
        else{
          
          # get the PFT data, append the colour and the id/long name (as appropriate)
          PFT <- byIDfromList(as.character(row$levels), PFT.set)
          col.list <- append(col.list, PFT@colour)
          if(useLongnames) label.list <- append(label.list, PFT@name)
          else  label.list <- append(label.list, PFT@id)
          
        }
        
      } # for each row in RAT
      
      quant <- new("Quantity",
                   id = "Dominant",
                   name = "Dominant PFT",
                   type = "DominantPFTs",
                   units = "categorical",
                   colours = colorRampPalette(col.list),
                   cuts = 0:length(col.list),
                   aggregate.method = "categorical"
      )
      
    } # if per PFT
    
    else if(tolower(quant@type) == "monthly") {
      
      # for each row of the RAT
      for(row.index in 1:NROW(RAT)){
        
        # get the row
        row <- RAT[row.index,]
        
        # special case if none
        if(as.character(row$levels == "None")) {
          col.list <- append(col.list, "gray75")
          label.list <- append(label.list, "None")
        }
        
        # else (assuming others rows are months)
        else{
          
          # get the PFT data, append the colour and the id/long name (as appropriate)
          month <- byIDfromList(as.character(row$levels), months)
          col.list <- append(col.list, month@col)
          if(useLongnames) label.list <- append(label.list, month@name)
          else  label.list <- append(label.list, month@id)
          
        }
        
        quant <- new("Quantity",
                     id = "Dominant",
                     name = paste("Dominant Month by", quant.id, sep = " "),
                     type = "DominantMonth",
                     units = "categorical",
                     colours = colorRampPalette(col.list),
                     cuts = 0:length(col.list),
                     aggregate.method = "categorical"
        )
        
        
        
      } # for each row in RAT
      
    } # if monthly
    
    
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, 
                                      "labels" = rev(label.list), 
                                      "at" = ((0:length(col.list))-1) + 0.5)
    colorkey.list[["at"]] <- 0:length(col.list)
    colorkey.list[["col"]] <- rev(col.list)
    
  }
  
  #### CATCH UNIMPLEMENTED SPECIAL CASES
  else if(!(special != "" | special != "none")){
    stop(paste("Special case", tolower(special), "not implemented yet", sep = " "))
  }
  
  
  ### OVERRIDE (CUTS AND COLOUR SCHEME) 
  if(!is.null(override.cols)) {quant@colours <- override.cols}
  if(!is.null(override.cuts)) {quant@cuts <- override.cuts}
  
  
  
  
  #####################################################################################
  ############# LIMIT THE PLOTTED VARIABLE IF REQUESTED################################
  ## Not really recommended as it hides scientific content, but can make plots nicer ##
  #####################################################################################
  
  if(limit){
    
    if(is.null(limits)){
      if(!is.null(quant)) {
        limits <- c(quant@cuts[0], quant@cuts[length(quant@cuts)]) 
      }
      else{
        warning("plotLPJMaps:: limit set to TRUE but no limits provided and no Quantity object provided either")
      }
      
    }
    
    quant@cuts <- seq(from = limits[1], to = limits[2], length.out = length(quant@cuts))
    
    for(layer in 1:nlayers(data.toplot)) {
      this.layer <- subset(data.toplot, layer)
      this.layer[this.layer < limits[1]] <- limits[1]
      this.layer[this.layer > limits[2]] <- limits[2]
      if(exists("data.limited")) data.limited <- addLayer(data.limited, this.layer)
      else data.limited <- brick(this.layer)
      rm(this.layer)
    }
    rm(data.toplot)
    data.toplot <- data.limited
    rm(data.limited)
    
  }
  
  
  
  #####################################################################################
  ############# MAKE THE PLOTS ########################################################
  #####################################################################################
  
  # PLOT MAIN TITLE
  if(is.null(title)) {
    if(length(layers) == 1)  title <- makePlotTitle(paste(quant@name, sep = " "), layer = layers, run = run, period = period)
    else  title <- makePlotTitle(paste(quant@name, sep = " "), run = run, period = period)
  }
  
  # PANEL LABELS - note expand longnames here if requested 
  if(is.null(plot.labels)) {
    plot.labels.here <- original.layers
    ### USE LONGNAMES - for PFTs
    if(useLongnames) {
      for(plot.label.index in 1:length(plot.labels.here)){
        # look up PFT
        for(PFT in PFT.set){
          if(plot.labels.here[plot.label.index] == PFT@id) plot.labels.here[[plot.label.index]] <- unlist(PFT@name)
        }
      }
      plot.labels.here<- unlist(plot.labels.here)
    }
  }
  else plot.labels.here <- plot.labels

  # return(spplot(data.toplot,
  #               layers,
  #               par.settings = list(panel.background=list(col=plot.bg.col)),
  #               xlab = list(label = "Longitude", cex = 3 * text.multiplier),
  #               ylab = list(label = "Latitude", cex = 3 * text.multiplier),
  #               col.regions= quant@colours,
  #               colorkey = colorkey.list,                                                                                            
  #               at = quant@cuts,
  #               scales = list(draw = TRUE, cex = 3 * text.multiplier),
  #               as.table = TRUE,
  #               main=list(label=title, 
  #                         cex = 4 * text.multiplier),
  #               par.strip.text = list(#lines = 1.0, 
  #                 cex = 2 * text.multiplier),
  #               sp.layout = layout.objs,
  #               maxpixels = maxpixels,
  #               names.attr = plot.labels.here,
  #               ...)
  #  )
  
  
  # PREPARE DATA FOR PLOTTING
  
  # select layers and convert to a data,table
  data.toplot <- selectLayers(data, layers)
  data.toplot <- as.data.table(data.toplot)
  
  # now melt the layers
  data.toplot <- melt(data.toplot, measure.vars = layers)
  print(data.toplot)
  
  
  print(layers)
  
  
  
  
  mp <- ggplot(data = as.data.frame(data.toplot))
  mp <- mp + geom_raster(aes_string(x = "Lon", y = "Lat", fill = "value"))
  mp <- mp + facet_wrap(as.formula(paste("~", "variable")))
  mp <- mp + scale_fill_distiller(lai.palette)
  mp <- mp + coord_fixed()
  
  
  return(mp)
  
  
  # clean up
  rm(data.toplot)
  gc()
  
}
