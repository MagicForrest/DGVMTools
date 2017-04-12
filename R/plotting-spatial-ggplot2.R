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
#' @param x The data to plot. Can be a ModelObject, data.table, a SpatialPixelsDataFrame or a Raster* object.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' @param layout.objs List of overlays (for example coastlines or rivers or statistical values) or other objects to be plotted by \code{spplot} 
#' so see there for how to build them.
#' @param plot.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param plot.bg.col Colour string for the plot background.
#' @param useLongnames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. 
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa
#' @param limit Boolean, whether or not to limit the plotted values between a range, either the limits argument below,
#' or the range of the plot.
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values
#' @param override.cols A colour palette function to override the defaults
#' @param override.cuts Cut ranges (a numeric vector) to override the defaults.
#' @param special A character string to indicate certain "special modes" which modifies the behaviour of the function a bit.
#' Special modes are currectly "fraction", "difference", "percentage.difference", "firert" (fire return time) and "dominant.pft".
#' @param map.overlay A character string specifying which map overlay (from the maps and mapdata packages) should be overlain.  
#' Other things can be overlain on the resulting plot with further ggplot2 commands.
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
#' @import ggplot2 data.table
#' @importFrom maptools map2SpatialLines
#' @importFrom rgeos gLength
#' @importFrom sp SpatialLinesDataFrame
#' @importFrom raster crs
#' 
#' @export 
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotSpatial2 <- function(data, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a ModelObject
                         layers = NULL,
                         title = NULL,
                         layout.objs = NULL, 
                         facet.labels =  NULL,
                         facet.order = NULL,
                         plot.bg.col =  "white",
                         useLongnames = FALSE,
                         text.multiplier = 1,
                         xlim = NULL,
                         ylim = NULL,
                         limit = FALSE,
                         limits = NULL,
                         override.cols = NULL,
                         override.cuts = NULL,
                         map.overlay = NULL,
                         dont.grid = FALSE,
                         return.data = FALSE,
                         tile = TRUE,
                         interpolate = FALSE,
                         ...){
 
  ### PREPARE DATA FOR PLOTTING
  
  # some flags tonote what type of data we have been handed
  discrete <- FALSE
  continuous <- FALSE
  single.object <- FALSE
  
  ### CASE 1 - A single ModelObject or DataObject
  if(is.ModelObject(data) || is.DataObject(data)) {
    
    single.object <- TRUE
    grid <- FALSE
    
    # if layers not specified, assume all
    if(is.null(layers)) layers <- names(data)
    
    # check if layers are all continuous or discrete
    for(layer in layers) {
      if(class(data@data[[layer]]) == "factor") discrete <- TRUE
      if(class(data@data[[layer]]) == "numeric") continuous <- TRUE
    }
    if(discrete & continuous) stop("plotSpatial cannot simultaneously plot discrete and continuous layers, check your layers")    
    
    # select layers and convert to a data,table
    data.toplot <- selectLayers(data, layers)
    data.toplot <- as.data.table(data.toplot)
    
    # now melt the layers
    data.toplot <- melt(data.toplot, measure.vars = layers)
    
    if(is.null(override.cols) & continuous) override.cols <- data@quant@colours(20)
    legend.title <- data@quant@units
    quant <- data@quant
    temporal.extent <- data@temporal.extent
    
  }
  
  ### CASE 2 - A single ComparisionLayer
  else if(is.ComparisonLayer(data)) {
    
    single.object <- TRUE
    grid <- FALSE
    original.layers <- layers # this is needed to keep track of the plotting mode since 'layers' is changed
    
    # first check if discrete or continuous
    for(layer in names(data)[1:2]) {
      if(class(data@data[[layer]]) == "factor") discrete <- TRUE
      if(class(data@data[[layer]]) == "numeric") continuous <- TRUE
    }
    if(discrete & continuous) stop("plotSpatial ComparisonObject does not seem to be consistenly defined")    
    
    
    # if layers not specified, assume "Difference"
    if(is.null(layers)  || tolower(layers) == "difference") {
      layers <- "Difference" 
      original.layers <- "Difference"
      if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
    }
    else if(tolower(layers) == "absolute") {
      # put side by side
      layers <- names(data)[1:2]
    }
    else if(tolower(layers) == "percentage.difference" || tolower(layers) == "percentagedifference"){
      
      layers <- "Percentage Difference"
      data.temp <- data@data[, "Percentage Difference" := (get(paste("Difference")) %/0% get(names(data)[2])) * 100]
      data@data <- data.temp
      if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
      
    }
    
    # select layers and convert to a data,table
    data.toplot <- selectLayers(data, layers)
    data.toplot <- as.data.table(data.toplot)
    
    
    # in the special case of absolute, change the layer names at this point from the ugly ids to the nice names
    if(tolower(original.layers) == "absolute") {
      new.layer.names <- c(data@info1@name, data@info2@name)
      setnames(data.toplot, layers, new.layer.names)
      layers <- new.layer.names
    }
    
    # now melt the layers
    data.toplot <- melt(data.toplot, measure.vars = layers)
    
    if(is.null(override.cols) & continuous) override.cols <- data@quant@colours(20)
    legend.title <- data@quant@units
    quant <- data@quant
    temporal.extent <- data@temporal.extent
    
    # special case for difference plots, make limits symmetric around 0
    if(is.null(limits) & (layers == "Difference" || layers == "Percentage Difference")){
      
      min.value <- min(data.toplot[["value"]], na.rm = TRUE)
      max.value <- max(data.toplot[["value"]], na.rm = TRUE)
      abs.max <- max(abs(min.value),abs(max.value))
      if(layers == "Percentage Difference") {
        abs.max <- min(abs.max, 300)
        legend.title <- "%"
      }
      limits <- c(-abs.max, abs.max)
      
    }
    
  }
  
  
  ### CASE 3- A list, hopefully made exclusively of ModelObjects/DataObjects xor ComparisonLayers
  else if(class(data)[1] == "list") {
    
    # PREAMBLE - first determine if the list contains consistent types and then fail if it does not
    only.data.or.model.objects <- TRUE
    only.comparison.layers <- TRUE
    for(object in data){ 
      if(!(is.ModelObject(object) || is.DataObject(object))) only.data.or.model.objects <- FALSE
      if(!is.ComparisonLayer(object)) only.comparison.layers <- FALSE
    }
    
    if(!xor(only.data.or.model.objects, only.comparison.layers)) {
      stop("You have passed me a list of items to plot but the items are exclusively of ModelObjects/DataObjects or ComparisonLayers (note you cannot mix those two types)")
    }
    
    # list of all data.tables to be rbinded at the end
    data.toplot.list <- list()
    
    ### CASE 3A - Plotting a bunch of ModelObjects/DataObjects
    if(only.data.or.model.objects) {
      
      # Loop through the objects and pull layers from each one into a large data.table for plotting
      
      temporal.extent <- NULL
      first <- TRUE
      for(object in data){
        
        # select the layers and mash the data into shape
        these.layers <- selectLayers(object, layers)
        these.layers.melted <- melt(these.layers@data, measure.vars = layers)
        if(is.DataObject(object)) these.layers.melted[, Source := object@name]
        else  these.layers.melted[, Source := object@run@name]
        data.toplot.list[[length(data.toplot.list)+1]] <- these.layers.melted
        
        # check if layers are all continuous or discrete
        for(layer in layers) {
          if(class(object@data[[layer]]) == "factor") discrete <- TRUE
          if(class(object@data[[layer]]) == "numeric") continuous <- TRUE
        }
        if(discrete & continuous) stop("plotSpatial canot simultaneously plot discrete and continuous layers, check your layers")   
        
        # check for meta-data to automagic the plots a little bit if possble
        if(first) {
          if(is.null(override.cols) & continuous) override.cols <- object@quant@colours(20)
          legend.title <- object@quant@units
          quant <- object@quant
          temporal.extent <- object@temporal.extent
        }
        else {
          # check for consistent temporal extent
          if(temporal.extent@start != object@temporal.extent@start || temporal.extent@end != object@temporal.extent@end) temporal.extent <- NULL
          # check for consistent Quantity
          if(!identical(quant, object@quant, ignore.environment = TRUE)) warning("Not all of the Data/ModeObjects supplied in the list have the same Quantity, I am using the Quantity from the first one")
        }
        
        first <- FALSE
        
      }
    }
    
    
    ### CASE 3B - Plotting a bunch of ComparisonLayers
    else if(only.comparison.layers) {
      
      # This follows Case 2 above
      original.layers <- layers # this is needed to keep track of the plotting mode since 'layers' is changed
      for(comp.layer in data){  
        
        # first check if discrete or continuous
        for(layer in names(comp.layer)[1:2]) {
          if(class(comp.layer@data[[layer]]) == "factor") discrete <- TRUE
          if(class(comp.layer@data[[layer]]) == "numeric") continuous <- TRUE
        }
        if(discrete & continuous) stop("plotSpatial ComparisonObject does not seem to be consistenly defined")    
        
        
        # if layers not specified, assume "Difference"
        if(is.null(original.layers)  || tolower(original.layers) == "difference") {
          layers <- "Difference" 
          original.layers <- layers
          if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
        }
        else if(tolower(original.layers) == "absolute") {
          # changes the names of the ComparisonLayer 
          
          layers <- names(comp.layer)[1:2]
        }
        else if(tolower(original.layers) == "percentage.difference" || tolower(original.layers) == "percentagedifference"){
          
          layers <- "Percentage Difference"
          comp.layer.temp <- comp.layer@data[, "Percentage Difference" := (get(paste("Difference")) %/0% get(names(comp.layer)[2])) * 100]
          comp.layer@data <- comp.layer.temp
          if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
          
        }
        
        # select layers and convert to a data,table
        data.toplot <- selectLayers(comp.layer, layers)
        data.toplot <- as.data.table(data.toplot)
        
        # in the special case of absolute, change the layer names at this point from the ugly ids to the nice names
        if(tolower(original.layers) == "absolute") {
          new.layer.names <- c(comp.layer@info1@name, comp.layer@info2@name)
          setnames(data.toplot, layers, new.layer.names)
          layers <- new.layer.names
        }
        
        # now melt the layers and add the Source
        data.toplot <- melt(data.toplot, measure.vars = layers)
        if(!tolower(original.layers) == "absolute") { data.toplot[, Source := comp.layer@name] }
        else {
          setnames(data.toplot, "variable", "Source")
          data.toplot[, variable := "Absolute"]
        }
        
        # save 
        data.toplot.list[[length(data.toplot.list)+1]] <- data.toplot
        
        
        if(is.null(override.cols) & continuous) override.cols <- comp.layer@quant@colours(20)
        legend.title <- comp.layer@quant@units
        quant <- comp.layer@quant
        temporal.extent <- comp.layer@temporal.extent
        
      } # for each ComparisonLayer
      
      # special case for difference plots, make limits symmetric around 0
      if(is.null(limits) & (layers == "Difference" || layers == "Percentage Difference")){
        
        min.value <- min(data.toplot[["value"]], na.rm = TRUE)
        max.value <- max(data.toplot[["value"]], na.rm = TRUE)
        abs.max <- max(abs(min.value),abs(max.value))
        if(layers == "Percentage Difference") {
          abs.max <- min(abs.max, 300)
          legend.title <- "%"
        }
        limits <- c(-abs.max, abs.max)
        
      } # if special case
      
    } # for each ComparisonLayer in the list
    
    
    # finally mash them all togther to make the final data.table to plot
    data.toplot <- rbindlist(data.toplot.list)
    rm(data.toplot.list)
    
  } 
  
  else {
    
    stop("plotSpatial can only handle single a DataObject or ModelObject, or a list of Data/ModelObjects")
    
  }
  
  ### Rename "variable" to "Layer" which makes more conceptual sense
  setnames(data.toplot, "variable", "Layer")
  setnames(data.toplot, "value", "Value")
  
  
  ### VERBOSE
  #if(discrete) print("Printing in mode for discrete variables")
  #if(continuous) print("Printing in mode for continuous variables")
  #if(!continuous & !discrete) stop("Neither discrete nor continuous")
  
  
  
  ### APPLY CUSTOM CUTS TO DISCRETISE IF NECESSARY
  if(continuous & !is.null(override.cuts)) {
    data.toplot[,Value:= cut(Value, override.cuts, right = FALSE, include.lowest = TRUE)]
    discrete <- TRUE
    continuous <- FALSE
    breaks <- waiver()
  }
 
  
  ### RETURN DATA ONLY IF REQUESTED
  if(return.data) return(data.toplot)
  
  
  ### CALCULATE THE RANGE OF LONGITUDE AND LATITUDE TO BE PLOTTED
  all.lons <- sort(unique(data.toplot[["Lon"]]))
  all.lats <- sort(unique(data.toplot[["Lat"]]))
  
  if(is.null(ylim)) ylim <- c(min(all.lats), max(all.lats)) 
  if(is.null(xlim)) xlim <- c(min(all.lons), max(all.lons)) 
  
  
  
  ### PREPARE THE MAP OVERLAY
  if(class(map.overlay)[1] == "character"){
    
    # determine if london centered (if not call "maps2" for Pacific centered versions)
    gt.180 <- FALSE
    for(lon in all.lons) {
      if(lon > 180) gt.180 <- TRUE
    }
    if(tolower(map.overlay)=="world" && gt.180) map.overlay <- "world2"
    else if(tolower(map.overlay)=="worldHires" && gt.180) map.overlay <- "worldHires2"
    else if(tolower(map.overlay)=="world2" && !gt.180) map.overlay <- "world"
    else if(tolower(map.overlay)=="world2ires" && !gt.180) map.overlay <- "worldHires"
    
    # Convert map to SpatialLinesDataFrame, perform the 'Russian Correction' and then fortify() for ggplot2
    proj4str <- "+proj=longlat +datum=WGS84"
    map.sp.lines <- map2SpatialLines(map(map.overlay, plot = FALSE, interior = TRUE, xlim=xlim, ylim=ylim, fill=TRUE), proj4string = CRS(proj4str))
    df <- data.frame(len = sapply(1:length(map.sp.lines), function(i) rgeos::gLength(map.sp.lines[i, ])))
    rownames(df) <- sapply(1:length(map.sp.lines), function(i) map.sp.lines@lines[[i]]@ID)
    map.sp.lines.df <- SpatialLinesDataFrame(map.sp.lines, data = df)
    map.sp.lines.df <- correct.map.offset(map.sp.lines.df)
    map.overlay <- fortify(map.sp.lines.df)
    
    rm(df, map.sp.lines, map.sp.lines.df)   
    
  }
  else if(!is.null(map.overlay)) {
    stop("Some other overlay type...")
  }
  
  ### IF PLOT IS DISCRETE, BUILD THE COLOURS 
  if(discrete & is.null(override.cols)){

    # make a list of all the unique values (factors), each of these will need a colour
    unique.vals <- unique(data.toplot[["Value"]])
    
    # Final results of stuff below
    cols <- c()
    is.PFTs <- FALSE
    is.categorical <- FALSE
    
    ###  If the Quantity if specifically defined as categorical then use the colours defined in the Quantity's units slot
    if(tolower(quant@type) == "categorical") {
      
      legend.title = NULL
      
      # reverse engineer colours from palette
      quant.cols <- quant@colours(length(quant@units))
      names(quant.cols) <- quant@units
      
      for(val in unique.vals) {
        if(!is.na(val)){
          for(factor.value in quant@units) {
            if(val == factor.value) cols[[val]] <- quant.cols[[val]]
          }  
        }
      }
      
      if(length(cols) == length(unique.vals)){
        is.categorical <- TRUE
      }
      
      
    }
    
    ### Else check if the factors are PFTs 
    # TODO - implement months below!!
    else {
      
      # check if the factors are PFTs, and if so assign them their meta-data colour
      pft.superset <- NULL
      if(is.ModelObject(data)) {
        pft.superset <- data@run@pft.set 
      }
      else {
        
        for(object in data) {
          if(is.ModelObject(object)) {
            pft.superset <- append(pft.superset, object@run@pft.set)
          }
          
        }
      }
      
      for(val in unique.vals) {
        for(PFT in pft.superset) {
          if(val == PFT@id) cols[[val]] <- PFT@colour
        }    
      }
      
      if(length(cols) == length(unique.vals)) is.PFTs <- TRUE
      
      # if not PFTs, look for months
      if(!is.PFTs){
        
        print("Here look for months")
        
      }
      
    }
    
    
    # If found colours for all the factors, set the values for plotting
    if(is.PFTs) {
      if(is.null(override.cols)) override.cols <- cols
      legend.title <- "PFT"
      breaks <- sort(names(override.cols))
    }
    else if(is.categorical) {
      if(is.null(override.cols)) override.cols <- cols
      legend.title <- quant@name
      breaks <- quant@units
    }
    
  }
  
  ### HANDLE THE FACET GRID/WRAP ISSUE
  multiple.sources <- FALSE
  multiple.layers <- FALSE
  
  # Do we have multiple sources?
  if("Source" %in% names(data.toplot)) {
    multiple.sources <- TRUE
  }
  
  # Do we have multiple layers?
  if(length(unique(data.toplot[["Layer"]])) > 1) {
    multiple.layers <- TRUE
  }
  
  #  CASE 1 - single source and single layer 
  #           ie. one map so don't wrap or grid
  if(!multiple.layers & !multiple.sources) {
    grid <- FALSE
    wrap <- FALSE
  }
  
  # CASE 2 - single source and multiple layers
  #          then wrap, gridding doesn't make sense
  else if(multiple.layers & !multiple.sources) {
    grid <- FALSE
    wrap <- TRUE
    facet.string <- "~Layer"
  }
  
  # CASE 3 - multiple sources and single layer (opposite of case 2)
  #          then wrap, gridding doesn't make sense
  else if(!multiple.layers & multiple.sources) {
    grid <- FALSE
    wrap <- TRUE
    facet.string <- "~Source"
  }
  
  # CASE 4 - multiple sources and multiple layers
  #          the grid (unless special instructions not to)
  else if(multiple.layers & multiple.sources) {
    if(!dont.grid) {
      grid <- TRUE
      wrap <- FALSE
      facet.string <- "Layer~Source"
    }
    else {
      stop("If you want some 'dont.grid' option action then code it up!")
    }
  }
  
  # if wrapping and facet order has been provided in facet.order, re-order the factor to re-order the facets
  if(wrap & !is.null(facet.order)) {
    if(multiple.layers & !multiple.sources) data.toplot[, Layer := factor(Layer, facet.order)]
    else  if(!multiple.layers & multiple.sources) data.toplot[, Source := factor(Source, levels = facet.order)]
  }
  
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(is.null(title)) {
    if(single.object) {
      if(length(unique(data.toplot[["Layer"]])) > 1) {
        title <- makePlotTitle(quant@name, layer = NULL, source = data, period = data@temporal.extent) 
      }
      else {
        title <- makePlotTitle(quant@name, layer = layers, source = data, period = data@temporal.extent) 
      }
    }
    else {
      if(length(unique(data.toplot[["Layer"]])) > 1) title <- makePlotTitle(quant@name, layer = NULL, source = NULL, period = temporal.extent) 
      else title <- makePlotTitle(quant@name, layer = layers, source = NULL, period = temporal.extent) 
    }
  }
  
  
  ### BUILD THE PLOT

  # basic plot building
  mp <- ggplot(data = as.data.frame(data.toplot))
  if(tile) mp <- mp + geom_tile(aes_string(x = "Lon", y = "Lat", fill = "Value"))
  else mp <- mp + geom_raster(aes_string(x = "Lon", y = "Lat", fill = "Value"), interpolate = interpolate)
  
  # facet with grid or wrap 
  if(grid) mp <- mp + facet_grid(as.formula(paste(facet.string)), switch = "y", labeller = as_labeller(facet.labels))
  else if(wrap) mp <- mp + facet_wrap(as.formula(facet.string), labeller = as_labeller(facet.labels))
  
  # colour bar
  if(continuous)  {
    mp <- mp + scale_fill_gradientn(name = legend.title, limits = limits, colors = override.cols, na.value="grey75")
    mp <- mp + guides(fill = guide_colorbar(barwidth = 2, barheight = 20))
  }
  if(discrete) {
    mp <- mp + scale_fill_manual(values = override.cols, breaks = breaks)
    mp <- mp + guides(fill = guide_legend(keyheight = 2))
  }
  
  # crop to xlim and ylim as appropriate and fix the aspect ratio 
  mp <- mp + scale_x_continuous(limits = xlim, expand = c(0, 0))
  mp <- mp + scale_y_continuous(limits = ylim, expand = c(0, 0))
  mp <- mp + coord_fixed()
  
  # labels and positioning
  mp <- mp + labs(title = title, y = "Latitude", x = "Longitude")
  if(!is.null(legend.title)) {mp <- mp + labs(fill=legend.title) }
  else { mp <- mp + theme(legend.title = element_blank()) }
  mp <- mp + theme(plot.title = element_text(hjust = 0.5))
  mp <- mp + theme(text = element_text(size=30))
  
  # set background colour of panel
  mp <- mp + theme(
    panel.background = element_rect(fill = plot.bg.col), # bg of the panel
    plot.background = element_rect(fill = plot.bg.col), # bg of the plot
    #, panel.grid.major = element_blank() # get rid of major grid
    #, panel.grid.minor = element_blank() # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), #, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    panel.border = element_rect(colour = "black", fill=NA),
    strip.background  = element_rect(colour = "black")
  )
  
  
  # list(theme(panel.grid.minor = element_line(size=0.1, colour = "black", linetype = "dotted"),
  #            panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
  #            panel.background  = element_rect(fill="#cae1ff"),
  #            panel.border      = element_rect(fill=NA, linetype = "solid", colour = "black"),
  #            axis.line         = element_blank(),
  #            axis.text         = element_text(size=10, colour = "black"),
  #            axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
  #            axis.ticks.length = unit(1.5, "points"),
  #            axis.title        = element_blank(),
  #            legend.text       = element_text(size=10),
  #            legend.title      = element_blank(),
  #            legend.position   = "bottom",
  #            legend.key        = element_rect(colour = "black"),
  #            legend.key.width  = unit(0.08, "npc"),
  #            plot.background   = element_blank(),
  #            plot.title        = element_text(size=22),
  #            strip.background  = element_rect(fill=NA)))
  
  
  
  # map overlay
  if(!is.null(map.overlay)) mp <- mp + geom_path(data=map.overlay, size=0.1, color = "black", aes(x=long, y=lat, group = group))
  
  return(mp)
  
  
}

######################### CORRECTS AN ARTEFACT FROM MAPS PACKAGE WHERE EASTERN ASIA IS WRONGLY PLACED #####################################################################
#' 
#' Fixes a spatial lines object where some of eastern Russia transposed to the other side of the world
#' 
#' 
#' @param spl SpatialLines object to fix
#' @return a the SpatialLines object 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import raster
correct.map.offset <- function(spl) {
  we <- crop(spl, extent(-180, 180, -90, 90))
  ww <- crop(spl, extent(179.999, 200, -90, 90))
  if(!is.null(ww) & !is.null(we)) {
    ww <- raster::shift(ww, -360)
    spl <- raster::bind(we, ww)  
  }
  return(spl)
}


#######################################################################################################################################
################### HELPER FUNCTIONS FOR MAKING PLOT TITLES, FILENAMES ETC... #########################################################
#######################################################################################################################################

#' Make a plot title
#' 
#' Build an appropriate plot title from some possibly relevant variables.  
#' It will use a string to represent the quantity (obligatory), and optionally a period and an ID.
#' 
#' @param quantity.str Character string for the quantity plotted
#' @param layer The names of the layer (or another identifier)
#' @param run The \code{ModelRun} object for the run plotted (optional)
#' @param period The time period plotted as \code{TemporalExtent} (optional)
#' @return A character string for use as a plot title
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(quantity.str, layer = NULL, source = NULL, period = NULL){
  
  # Quantity.str must be supplied
  string <- quantity.str
  
  # A layer name may be supplied
  if(!is.null(layer)) string <- paste(string, layer, sep = " ")
  
  # A source may be supplied (either a DataObject, ModelObject or ComparisonLayer)
  if(!is.null(source)) {
    if(is.ModelObject(source)) string <- paste(string, source@run@name, sep = " ")
    if(is.DataObject(source)) string <- paste(string, source@name, sep = " ")
    if(is.ComparisonLayer(source)) string <- paste(string, source@name, sep = " ")
  }
  
  # Ands a period may be suppled  
  if(!is.null(period)) string <- paste(string, paste("(", period@start, "-", period@end, ")", sep = ""), sep = " ")
  return(string)
  
}
