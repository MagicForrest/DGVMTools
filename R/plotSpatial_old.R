#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################


#' Plot maps from a temporally-averaged \code{Field}, \code{DataObject} or \code{ComaprisonLayer} (and lists thereof)
#' 
#' This is a heavy lifting function for plotting maps from Fields, DataObjects, and ComparisonLayers (and lists of those things) with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  In principle you can supply only the objext and it will plot.
#' It is basically a complex wrapper for the ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param data The data to plot. Can be a Field, data.table, a SpatialPixelsDataFrame or a Raster* object.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' @param layout.objs A character string name of a map overlay from the \code{maps} or \code{mapdata} packages. For example, "world" or "worldHires".  
#' Note that using these, especially "worldHires", can add quite a bit off time. 
#' @param facet.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param facet.order A vector of the characters that, if supplied, control the order of the facets.  To see what these values are you can call this funtion with "plot=FALSE"
#' and check the values of the XXXX column.  But generally they will be the values of the @names slots of the Data/Fields and/or the layers (as layers plotted as defined by the layers arguments 
#' in this function). 
#' @param plot.bg.col Colour string for the plot background.  "white"
#' @param useLongNames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. NOT CURRENTLY IMPLEMENTED!!
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param ylim An optional vector of two numerics to specify the y/latitude range of the plot.
#' @param xlim An optional vector of two numerics to specify the x/longitude range of the plot.
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param override.cols A colour palette function to override the defaults.
#' @param override.cuts Cut ranges (a numeric vector) to override the default colour delimitation
#' @param discretise Boolen, if true, but the discretise the data according the override.cuts argument above
#' @param dont.grid Boolean, if TRUE then don't use facet_grid() to order the panels in a grid.  Instead use facet_wrap().  
#' Useful when not all combinations of Sources x Layers exist which would leave blank panels.
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param tile Boolean If true use the ggplot function geom_tile() instead of the function geom_raster().
#' @param interpolate Boolean, whether or not to use interpolation in the case of a call to geom_raster().  Probably not a good idea, results tend to hurt my eyes. 
#' @param map.overlay A character string specifying which map overlay (from the maps and mapdata packages) should be overlain.  
#' @param interior.lines Boolean, if TRUE plot country lines with the continent outlines of the the requested map.overlay
#' Other things can be overlain on the resulting plot with further ggplot2 commands.
#' 
#' @details  This function is heavily used by the benchmarking functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' However, the \code{plotGGSpatial} function makes pretty plots with a simpler syntax, but with less flexibility.
#' 
#' The function works best for \code{Fields} (which contain a lot of useful metadata).   
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
#' @keywords internal
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotSpatial_old <- function(data, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a Field
                        layers = NULL,
                        title = NULL,
                        layout.objs = NULL, 
                        facet.labels =  NULL,
                        facet.order = NULL,
                        plot.bg.col =  "white",
                        useLongNames = FALSE,
                        text.multiplier = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        limits = NULL,
                        override.cols = NULL,
                        override.cuts = NULL,
                        discretise = FALSE,
                        map.overlay = NULL,
                        dont.grid = FALSE,
                        plot = TRUE,
                        tile = TRUE,
                        interpolate = FALSE,
                        interior.lines = TRUE){
  
  
  Source = variable = Value = Lat = Lon = Layer = long = lat = group = NULL
  
  ### CHECK FOR MISSING ARGUMENTS AND INITILIASE WHERE APPROPRIATE
  categorical.legend.labels <- waiver()
  
  ### PREPARE DATA FOR PLOTTING
  
  # some flags tonote what type of data we have been handed
  discrete <- FALSE
  continuous <- FALSE
  single.object <- FALSE
  
  # very special case 
  dont.wrap.comparison.layer.only <- FALSE
  
  # this is needed to keep track of the plotting mode since 'layers' may be changed below
  original.layers <- layers 
  
  # special flag to make symmetric difference limits about zero 
  # (only in the case that no limits are defined and the layers to be plotted are "Difference" or "Percentage Difference")
  symmetric.diff.limits <- FALSE
  
  ### CASE 1 - A single Field or DataObject
  if(is.Field(data)) {
    
    single.object <- TRUE
    grid <- FALSE
    
    # if layers not specified, assume all
    if(is.null(layers)) {
      layers <- names(data)
      original.layers <- layers 
    }
    
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
    #temporal.extent <- data@temporal.extent
    
    # for later (handling wrap/grid and plot titles)
    multiple.sources <- FALSE
    multiple.layers <- length(layers) > 1
    
  }
  
  ### CASE 2 - A single ComparisonLayer
  else if(is.ComparisonLayer(data)) {
    
    single.object <- TRUE
    grid <- FALSE
    
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
      if(is.null(override.cols) & continuous) override.cols <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
    }
    else if(tolower(layers) == "absolute") {
      # put side by side
      layers <- names(data)[1:2]
    }
    else if(tolower(layers) == "percentage.difference" || tolower(layers) == "percentagedifference"){
      
      layers <- "Percentage Difference"
      data.temp <- data@data[, "Percentage Difference" := (get(paste("Difference")) %/0% get(names(data)[2])) * 100]
      data@data <- data.temp
      if(is.null(override.cols) & continuous) override.cols <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
      
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
    
    if(tolower(original.layers) == "absolute") {
      setnames(data.toplot, "variable", "Source")
      data.toplot[, variable := "Absolute"]
    }
    
    
    if(is.null(override.cols) & continuous) override.cols <- data@quant@colours(20)
    legend.title <- data@quant@units
    quant <- data@quant
    #temporal.extent <- data@temporal.extent
    
    # special case for difference plots, make limits symmetric around 0
    if(missing(limits) & (layers == "Difference" || layers == "Percentage Difference")){
      symmetric.diff.limits <- TRUE
    }
    
    # for later (handling wrap/grid and plot titles)
    multiple.sources <- FALSE
    multiple.layers <- FALSE
    if(tolower(original.layers) == "absolute") multiple.sources <- TRUE
    
  }
  
  
  ### CASE 3- A list, hopefully made exclusively of Fields/DataObjects xor ComparisonLayers
  else if(class(data)[1] == "list") {
    
    # PREAMBLE - first determine if the list contains consistent types and then fail if it does not
    only.data.or.model.objects <- TRUE
    only.comparison.layers <- TRUE
    for(object in data){ 
      if(!is.Field(object)) only.data.or.model.objects <- FALSE
      if(!is.ComparisonLayer(object)) only.comparison.layers <- FALSE
    }
    
    if(!xor(only.data.or.model.objects, only.comparison.layers)) {
      stop("You have passed me a list of items to plot but the items are exclusively of Fields or ComparisonLayers (note you cannot mix those two types)")
    }
    
    # list of all data.tables to be rbinded at the end
    data.toplot.list <- list()
    
    ### CASE 3A - Plotting a bunch of Fields/DataObjects
    if(only.data.or.model.objects) {
      
      # Loop through the objects and pull layers from each one into a large data.table for plotting
      
      temporal.extent <- NULL
      first <- TRUE
      for(object in data){
        
        # select the layers and mash the data into shape
        these.layers <- selectLayers(object, layers)
        these.layers.melted <- melt(these.layers@data, measure.vars = layers)
        these.layers.melted[, Source := object@source@name]
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
          #temporal.extent <- object@temporal.extent
        }
        else {
          # # check for consistent temporal extent
          # if(!is.null(temporal.extent)){
          #   if(temporal.extent@start != object@temporal.extent@start || temporal.extent@end != object@temporal.extent@end) temporal.extent <- NULL
          # }
          # # check for consistent Quantity
          # if(!identical(quant, object@quant, ignore.environment = TRUE)) warning("Not all of the Data/ModeObjects supplied in the list have the same Quantity, I am using the Quantity from the first one")
        }
        
        first <- FALSE
        
      }
      
      # for later (handling wrap/grid and plot titles)
      multiple.sources <- TRUE
      multiple.layers <- length(layers) > 1
      
    }
    
    
    ### CASE 3B - Plotting a bunch of ComparisonLayers
    else if(only.comparison.layers) {
      
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
          if(is.null(override.cols) & continuous) override.cols <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
        }
        else if(tolower(original.layers) == "absolute") {
          # changes the names of the ComparisonLayer 
          
          layers <- names(comp.layer)[2:1]
        }
        else if(tolower(original.layers) == "percentage.difference" || tolower(original.layers) == "percentagedifference"){
          
          layers <- "Percentage Difference"
          comp.layer.temp <- comp.layer@data[, "Percentage Difference" := (get(paste("Difference")) %/0% get(names(comp.layer)[2])) * 100]
          comp.layer@data <- comp.layer.temp
          if(is.null(override.cols) & continuous) override.cols <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
          
        }
        
        # select layers and convert to a data,table
        data.toplot <- selectLayers(comp.layer, layers)
        data.toplot <- as.data.table(data.toplot)
        
        # in the special case of absolute, change the layer names at this point from the ugly ids to the nice names
        if(tolower(original.layers) == "absolute") {
          new.layer.names <- c(comp.layer@info2@name, comp.layer@info1@name)
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
        #temporal.extent <- comp.layer@temporal.extent
        
      } # for each ComparisonLayer
      
      # special case for difference plots, make limits symmetric around 0
      if(missing(limits) & (layers == "Difference" || layers == "Percentage Difference")){
        symmetric.diff.limits <- TRUE
      }
      
      # for later (handling wrap/grid and plot titles)
      multiple.sources <- TRUE
      multiple.layers <- FALSE
      if(tolower(original.layers) == "absolute") {
        multiple.layers <- TRUE
        dont.wrap.comparison.layer.only <- TRUE
      }
      
    } # for each ComparisonLayer in the list
    
    
    # finally mash them all togther to make the final data.table to plot
    data.toplot <- rbindlist(data.toplot.list)
    rm(data.toplot.list)
    
    
    
  } 
  
  else {
    
    stop("plotSpatial can only handle single a DataObject or Field, or a list of Data/Fields")
    
  }
  
  # Once got all data to plot,
  # check special case for difference plots, make limits symmetric around 0
  if(symmetric.diff.limits){
    min.value <- min(data.toplot[["value"]], na.rm = TRUE)
    max.value <- max(data.toplot[["value"]], na.rm = TRUE)
    abs.max <- max(abs(min.value),abs(max.value))
    if(layers == "Percentage Difference") {
      abs.max <- min(abs.max, 300)
      legend.title <- "%"
    }
    limits <- c(-abs.max, abs.max)
  } # if special case
  
  
  ### Rename "variable" to "Layer" which makes more conceptual sense
  setnames(data.toplot, "variable", "Layer")
  setnames(data.toplot, "value", "Value")
  
  
  ### VERBOSE
  #if(discrete) print("Printing in mode for discrete variables")
  #if(continuous) print("Printing in mode for continuous variables")
  #if(!continuous & !discrete) stop("Neither discrete nor continuous")
  
  
  
  ### APPLY CUSTOM CUTS TO DISCRETISE IF NECESSARY
  if(continuous & !is.null(override.cuts)) {
    if(discretise) {
      data.toplot[,Value:= cut(Value, override.cuts, right = FALSE, include.lowest = TRUE)]
      discrete <- TRUE
      continuous <- FALSE
      breaks <- waiver()
      if(length(override.cols) != length(override.cuts)){
        override.cols <- grDevices::colorRampPalette(override.cols)(length(override.cuts))
      }
    }
    else{
      
    }
  }
  else{
    override.cuts <- waiver()
  }
  
  
  ### RETURN DATA ONLY IF REQUESTED
  if(!plot) return(data.toplot)
  
  
  ### CALCULATE THE RANGE OF LONGITUDE AND LATITUDE TO BE PLOTTED AND CROP
  all.lons <- sort(unique(data.toplot[["Lon"]]))
  all.lats <- sort(unique(data.toplot[["Lat"]]))
  
  # y cropping
  if(is.null(ylim)) {
    
    # MF this code is not bullet proof, could possible be replaced with a dedicated getSpacing() function to handle this better
    
    # expand to the limit to the edge of the gridcell rather than the gridcell centres
    ylim <- c(min(all.lats) - abs(all.lats[1] - all.lats[2])/2, max(all.lats) + abs(all.lats[length(all.lats)] - all.lats[length(all.lats)-1])/2) 
  }
  else {
    data.toplot <- data.toplot[Lat >= ylim[1] & Lat <= ylim[2],]
  }
  
  # x cropping
  if(is.null(xlim)) {
    
    # MF as above
    
    # expand to the limit to the edge of the gridcell rather than the gridcell centres
    xlim <- c(min(all.lons) - abs(all.lons[1] - all.lons[2])/2, max(all.lons) + abs(all.lons[length(all.lons)] - all.lons[length(all.lons)-1])/2) 
  }
  else {
    data.toplot <- data.toplot[Lon >= xlim[1] & Lon <= xlim[2],]
  }
  
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
    else if(tolower(map.overlay)=="world2Hires" && !gt.180) map.overlay <- "worldHires"
    
    # Convert map to SpatialLinesDataFrame, perform the 'Russian Correction' and then fortify() for ggplot2
    
    proj4str <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"
    map.sp.lines <- map2SpatialLines(map(map.overlay, plot = FALSE, interior = interior.lines, xlim=xlim, ylim=ylim, fill=TRUE), proj4string = CRS(proj4str))
    suppressWarnings(df <- data.frame(len = sapply(1:length(map.sp.lines), function(i) rgeos::gLength(map.sp.lines[i, ]))))
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
    
    ### Else data is categorical, but not explicitly defined as so in the Quantity, so we need to scan the names to identify the PFTs or months
    # check if the factors are PFTs 
    # TODO - implement months below!!
    else {
      
      # check if the factors are PFTs, and if so assign them their meta-data colour
      pft.superset <- NULL
      if(is.Field(data)) {
        pft.superset <- data@source@pft.set 
      }
      else {
        
        for(object in data) {
          if(is.Field(object)) {
            pft.superset <- append(pft.superset, object@source@pft.set)
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
      if(useLongNames) {
        categorical.legend.labels <- c()
        for(this.break in breaks){
          
          for(PFT in pft.superset) {
            if(this.break == PFT@id) categorical.legend.labels <- append(categorical.legend.labels, PFT@name)
          }   
          
          
        }
      }
    }
    else if(is.categorical) {
      if(is.null(override.cols)) override.cols <- cols
      legend.title <- quant@name
      breaks <- quant@units
    }
    
  }
  
  ### HANDLE THE FACET GRID/WRAP ISSUE
  # note that multiple.layers and multiple.sources should have been defined about
  
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
    
    # also set the facet order if not defined
    if(missing(facet.order) && !only.comparison.layers) {
      facet.order <- character(0)
      for(this.object in data) {
          facet.order <- append(facet.order, this.object@source@name)
      }
    }
    
  }
  
  # CASE 4 - multiple sources and multiple layers
  #          the grid (unless special instructions not to)
  else if(multiple.layers & multiple.sources) {
    if(!dont.grid) {
      grid <- TRUE
      wrap <- FALSE
      facet.string <- "Layer~Source"
      
      # very special case form comparison layers side-by-side (this is just to avoid one stupid extra label)
      if(dont.wrap.comparison.layer.only){
        grid <- FALSE
        wrap <- TRUE
        facet.string <- "~Source"
      }
      
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
    # 
    # layer.string <- NULL
    # # only use the layer string for the title if we are only plotting one layer
    # if(!multiple.layers & tolower(original.layers) != "absolute") layer.string <- layers
    # 
    # # also only use the 'source' argument in the title if we are plotting only data from only one source 
    # if(multiple.sources)  title <- makePlotTitle(quant@name, layer = layer.string, source = NULL, period = temporal.extent) 
    # else title <- makePlotTitle(quant@name, layer = layer.string, source = data, period = data@temporal.extent)
    # 
  }
  
  ### BUILD THE PLOT
  
  # basic plot building
  mp <- ggplot(data = as.data.frame(data.toplot))
  if(tile) mp <- mp + geom_tile(aes_string(x = "Lon", y = "Lat", fill = "Value"))
  else mp <- mp + geom_raster(aes_string(x = "Lon", y = "Lat", fill = "Value"), interpolate = interpolate)
  
  # facet with grid or wrap 
  if(grid) mp <- mp + facet_grid(stats::as.formula(paste(facet.string)), switch = "y", labeller = as_labeller(facet.labels))
  else if(wrap) mp <- mp + facet_wrap(stats::as.formula(facet.string), labeller = as_labeller(facet.labels))
  
  # colour bar
  if(continuous)  {
    mp <- mp + scale_fill_gradientn(name = legend.title, 
                                    limits = limits, 
                                    colors = override.cols, 
                                    breaks = override.cuts,
                                    na.value="grey75")
    mp <- mp + guides(fill = guide_colorbar(barwidth = 2, barheight = 20))
  }
  if(discrete) {
    mp <- mp + scale_fill_manual(values = override.cols, 
                                 breaks = breaks,
                                 labels = categorical.legend.labels)
    # mp <- mp + guides(fill = guide_legend(keyheight = 1))
  }
  
  # crop to xlim and ylim as appropriate and fix the aspect ratio 
  if(!is.null(xlim)) mp <- mp + scale_x_continuous(limits = xlim, expand = c(0, 0))
  else   mp <- mp + scale_x_continuous(expand = c(0, 0))  
  
  if(!is.null(ylim)) mp <- mp + scale_y_continuous(limits = ylim, expand = c(0, 0))
  else   mp <- mp + scale_y_continuous(expand = c(0, 0))
  
  mp <- mp + coord_fixed()
  
  # labels and positioning
  mp <- mp + labs(title = title)
  mp <- mp + labs(y = "Latitude", x = "Longitude")
  if(!is.null(legend.title)) {mp <- mp + labs(fill=legend.title) }
  else { mp <- mp + theme(legend.title = element_blank()) }
  mp <- mp + theme(plot.title = element_text(hjust = 0.5))
  
  # overall text multiplier
  if(!missing(text.multiplier)) mp <- mp + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  # set background colour of panel
  mp <- mp + theme(
    plot.background = element_rect(fill = plot.bg.col), # bg of the plot
    panel.background = element_rect(fill = "#cae1ff"), # bg of the panel
    #panel.grid.major = element_blank(), # get rid of major grid
    #panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), #, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    panel.border = element_rect(colour = "black", fill=NA),
    strip.background  = element_rect(fill=NA)
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
  
  
  
  # map overlay - suppress warning about missing values
  
  if(!is.null(map.overlay)) {
    suppressWarnings( mp <- mp + geom_path(data=map.overlay, size=0.1, color = "black", aes(x=long, y=lat, group = group)))
  }
  
  return(mp)
  
  
}