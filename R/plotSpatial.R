#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################


#' Plot maps from a temporally-averaged \code{ModelObject}, \code{DataObject} or \code{ComaprisonLayer} (and lists thereof)
#' 
#' This is a heavy lifting function for plotting maps from ModelObjects, DataObjects, and ComparisonLayers (and lists of those things) with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  In principle you can supply only the objext and it will plot.
#' It is basically a complex wrapper for the ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param sources The data to plot. Can be a ModelObject, a DataObject, or a list of including both
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' Note that using these, especially "worldHires", can add quite a bit off time. 
#' @param facet.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param facet.order A vector of the characters that, if supplied, control the order of the facets.  To see what these values are you can call this funtion with "plot=FALSE"
#' and check the values of the XXXX column.  But generally they will be the values of the @names slots of the Data/ModelObjects and/or the layers (as layers plotted as defined by the layers arguments 
#' in this function). 
#' @param plot.bg.col Colour string for the plot background.  "white"
#' @param useLongNames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. NOT CURRENTLY IMPLEMENTED!!
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param ylim An optional vector of two numerics to specify the y/latitude range of the plot.
#' @param xlim An optional vector of two numerics to specify the x/longitude range of the plot.
#' @param years An optional numeric vector specifying which years to plot  
#' @param days An optional numeric vector specifying which days to plot
#' @param months An optional numeric vector specifying which months to plot
#' @param seasons An optional character vector specifying which seasons to plot (any or all of "DJF", "MAM, "JJA", "SON")  
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param override.cols A colour palette function to override the defaults.
#' @param override.cuts Cut ranges (a numeric vector) to override the default colour delimitation
#' @param discretise Boolen, if true, but the discretise the data according the override.cuts argument above
#' @param grid Boolean, if TRUE then don't use facet_grid() to order the panels in a grid.  Instead use facet_wrap().  
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
#' The function works best for \code{ModelObjects} (which contain a lot of useful metadata).   
#' 
#' @return Returns a ggplot object
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

plotSpatial2 <- function(sources, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a ModelObject
                         layers = NULL,
                         title = NULL,
                         facet.labels =  NULL,
                         facet.order = NULL,
                         plot.bg.col =  "white",
                         useLongNames = FALSE,
                         text.multiplier = NULL,
                         xlim = NULL,
                         ylim = NULL,
                         years = NULL,
                         days = NULL,
                         months = NULL,
                         seasons = NULL,
                         limits = NULL,
                         override.cols = NULL,
                         override.cuts = NULL,
                         discretise = FALSE,
                         map.overlay = NULL,
                         grid = FALSE,
                         plot = TRUE,
                         tile = TRUE,
                         interpolate = FALSE,
                         interior.lines = TRUE){
  
  Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = NULL
  
  ### CHECK FOR MISSING OR INCONSISTENT ARGUMENTS AND INITIALISE STUFF WHERE APPROPRIATE
  categorical.legend.labels <- waiver()
  
  plot.seasons = !(missing(seasons) || is.null(seasons))
  plot.days = !(missing(days) || is.null(days))
  plot.months =!(missing(months) || is.null(months))
  plot.years = !(missing(years) || is.null(years))
  
  # require that at most only one of seasons/months/days be specified
  if(sum(plot.months, plot.seasons, plot.days ) > 1){
    warning("More than one of days/months/seasons specified for plotting. I can't handle this, returning NULL")
    return(NULL)
  }
  
  
  
  
  ### CHECK TO SEE EXACTLY WHAT WE SHOULD PLOT
  
  ### 1. SOURCES - check the sources
  if(is.ModelObject(sources) || is.DataObject(sources)) {
    sources<- list(sources)
  }
  else if(class(sources)[1] == "list") {
    for(object in sources){ 
      if(!(is.ModelObject(object) || is.DataObject(object))) {
        warning("You have passed me a list of items to plot but the items are not exclusively of ModelObjects/DataObjects.  Returning NULL")
        return(NULL)
      }
    }
  }
  else{
    stop(paste("plotSpatial can only handle single a DataObject or ModelObject, or a list of Data/ModelObjects can't plot an object of type", class(sources)[1], sep = " "))
  }
  
  
  ### 2. LAYERS - check the number of layers
  
  layers.superset <- c()
  num.layers.x.sources <- 0
  
  # if no layers argument supplied make a list of all layers present (in any object)
  if(is.null(layers) || missing(layers)){
    
    for(object in sources){
      temp.layers <- names(object)
      num.layers.x.sources <- num.layers.x.sources + length(temp.layers)
      layers.superset <- append(layers.superset, temp.layers)
    } 
    
    layers <- unique(layers.superset)
    
  }
  
  # else if layers have been specified check that we have some of the requested layers present
  else{
    
    for(object in sources){
      
      layers.present <- intersect(names(object), layers)
      num.layers.x.sources <- num.layers.x.sources + length(layers.present)
      
      if(length(layers.present) == 0) {warning("Some Data/ModelObjects to plot don't have all the layers that were requested to plot")}
      layers.superset <- append(layers.superset, layers.present)
      
    } 
    
    # Return empty plot if not layers found
    if(num.layers.x.sources == 0){
      warning("None of the specified layers found in the objects provided to plot.  Returning NULL.")
      return(NULL)
    }
    
    # Also check for missing layers and given a warning
    missing.layers <- layers[!(layers %in% unique(layers.superset))]
    if(length(missing.layers) != 0) { warning(paste("The following layers were requested to plot but not present in any of the supplied objects:", paste(missing.layers, collapse = " "), sep = " ")) }
    
    # finally make a unique list of layers to be carried in to the actual plotting
    layers <- unique(layers.superset)
    
  }
  
  
  ### 3. SPATIOTEMPORAL - check the dimensions etc.
  
  for(object in sources){
    
    this.stinfo.names <- getSTInfo(object, info = "names")
    years.all <- c()
    days.all <- c()
    
    # return NULL if lon and lat are not available for every object to be plotted
    if(!"Lon" %in% this.stinfo.names || !"Lat" %in% this.stinfo.names) {
      warning("Lon (longitude) and/or Lat (latitude) missing from a Model/DataObject for which a map is tried to be plotted.  Obviously this won't work, returning NULL.")
      return(NULL)
    }
    
    
    # check if individual years are present
    if(plot.years) {
      
      if("Year" %in% this.stinfo.names) {
        years.all <- append(years.all, getSTInfo(object, info = "values")$Year)
      }
      else{
        warning("Plotting of individual years requested, but they are not avialable in at least one of the object to be plotted, returning NULL plot.")
        return(NULL)
      }
      
    }
    else{
      
      if("Year" %in% this.stinfo.names){
        warning("Plotting of individual years not requested, but individual years are present in the data to be plotted!  In the future average the Object first or select the years, for now returning NULL plot.")
        return(NULL)
      }
      
    }
    
    # now check the subannual dimensions
    
    # first check if the data is somehow corrupted with (ie has more than one of Season/Day/Month)
    if(sum("Month" %in% this.stinfo.names, "Season" %in% this.stinfo.names, "Day" %in% this.stinfo.names) > 1) {
      warning("At least one object to plot has more than one of Day/Month/Season present so would seem to be corrupted or strangely constructed.  Don't know how to interpret this, so returning NULL plot.")
      return(NULL)
    }
    
    
    if(plot.days){
      
      if(!"Day" %in% this.stinfo.names){
        warning("Plotting of days requested but not present in at least one input object, so returning NULL.")
        return(NULL)
      }
      else{
        
      }
      
    }
    
    if(plot.months){
      
      if(!"Month" %in% this.stinfo.names){
        warning("Plotting of days requested but not present in at least one input object, so returning NULL.")
        return(NULL)
      }
      else{
        
      }
      
    }
    
    if(plot.seasons){
      
      if(!"Season" %in% this.stinfo.names){
        warning("Plotting of days requested but not present in at least one input object, so returning NULL.")
        return(NULL)
      }
      else{
        
      }
      
    }
    
  }
  
  
  
  
  ### PREPARE DATA FOR PLOTTING
  # Here we extract the data.table from the object to be plotted and melt it as appropriate
  
  data.toplot.list <- list()
  
  # Loop through the objects and pull layers from each one into a large data.table for plotting
  
  temporal.extent <- NULL
  discrete <- FALSE
  continuous <- FALSE
  first <- TRUE
  for(object in sources){
    
    # select the layers and time periods required and mash the data into shape
    these.layers <- selectLayers(object, layers)
    if(!is.null(years)) these.layers <- selectYears(these.layers, years)
    
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
      if(!is.null(temporal.extent)){
        if(temporal.extent@start != object@temporal.extent@start || temporal.extent@end != object@temporal.extent@end) temporal.extent <- NULL
      }
      # check for consistent Quantity
      if(!identical(quant, object@quant, ignore.environment = TRUE)) warning("Not all of the Data/ModeObjects supplied in the list have the same Quantity, I am using the Quantity from the first one")
    }
    
    first <- FALSE
    
  }
  
  
  # finally mash them all togther to make the final data.table to plot
  data.toplot <- rbindlist(data.toplot.list)
  
  
  
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
    # MF  not bullet proof as above
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
    map.sp.lines <- maptools::map2SpatialLines(map(map.overlay, plot = FALSE, interior = interior.lines, xlim=xlim, ylim=ylim, fill=TRUE), proj4string = sp::CRS(proj4str))
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
      if(is.ModelObject(sources)) {
        pft.superset <- source@run@pft.set 
      }
      else {
        
        for(object in sources) {
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
  
  ### Swap 1,2,3... for Jan,Feb,Mar... if plotting months
  if(plot.months) {
    
    # make a list of months from the meta data
    month.list <- c()
    for(this.month in all.months) { month.list <- append(month.list, paste0("%^&", this.month@padded.index, "%^&", this.month@id)) }
    
    # simple replacement function
    get.pos <- function(x, v){ return(v[x]) }
    
    # apply replacement function
    data.toplot[, Month := unlist(lapply(data.toplot[["Month"]], FUN = get.pos, month.list))]
    setKeyDGVM(data.toplot)
    
  }
  
  ### HANDLE THE FACET GRID/WRAP ISSUE
  
  # first determine how many facet "dimensions" we have
  multiple.years = multiple.days =  multiple.months = multiple.seasons = FALSE
  if(!is.null(years)) multiple.years <- length(years) > 1
  if(!is.null(days)) multiple.days <- length(days) > 1
  if(!is.null(months)) multiple.months <- length(months) > 1
  if(!is.null(seasons)) multiple.seasons <- length(seasons) > 1
  
  multiple.sources <- length(sources) > 1
  multiple.layers <- length(layers) > 1
  
  
  
  num.panel.dimensions <- sum(multiple.sources, multiple.layers, multiple.years, multiple.days, multiple.months, multiple.seasons)
  
  
  # if got a single source, layer and year facetting is impossible/unnecessary 
  if(num.panel.dimensions == 0) {
    facet <- FALSE
    wrap <- FALSE
    if(grid) warning("Option grid is TRUE, but there is only one panel to plot so using facet_grid seems silly.  I am ignoring the grid option.")
    grid <- FALSE
  }
  # if got exactly one or two multiples of source, layer or year facetting is necessary and gridding is possible
  else if(num.panel.dimensions == 1 || num.panel.dimensions == 2) {
    facet <- TRUE
    if(!grid) wrap <- TRUE
    else wrap <- FALSE
  }
  # else if got more that two of multiple sources, layers or years then gridding is impossible, must use facet_wrap()
  else if(num.panel.dimensions > 2){
    facet <- TRUE
    wrap <- TRUE
    if(grid) warning("Option grid is TRUE, but there are too many plot 'dimensions' to make a 2D grid of panels so I am ignoring the grid option.")
    grid <- FALSE
  }
  
  
  
  # if wrapping, add a new column to uniquely describe each facet
  if(wrap){
    data.toplot[, Facet := ""]
    if(multiple.layers) { data.toplot[, Facet := paste(Facet, Layer)] }
    if(multiple.sources) { data.toplot[, Facet := paste(Facet, Source)] }
    if(multiple.years) { data.toplot[, Facet := paste(Facet, Year)] }
    if(multiple.days) { data.toplot[, Facet := paste(Facet, Day)] }
    if(multiple.months) { data.toplot[, Facet := paste(Facet, Month)] }
    if(multiple.seasons) { data.toplot[, Facet := paste(Facet, Season)] }
    data.toplot[, Facet := trimws(Facet)]
  }
  
  # if gridding get the columns to grid by
  if(grid){
    grid.columns <- c()
    if(multiple.layers) grid.columns <- append(grid.columns, "Layer")
    if(multiple.sources) grid.columns <- append(grid.columns, "Source")
    if(multiple.years) grid.columns <- append(grid.columns, "Year")
    if(multiple.days) grid.columns <- append(grid.columns, "Day")
    if(multiple.months) grid.columns <- append(grid.columns, "Month")
    if(multiple.seasons) grid.columns <- append(grid.columns, "Season")
    grid.string <- paste(grid.columns, collapse = "~")
  }
  
  
  
  
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  # if(is.null(title)) {
  #   
  #   layer.string <- NULL
  #   # only use the layer string for the title if we are only plotting one layer
  #   if(!multiple.layers & tolower(layers) != "absolute") layer.string <- layers
  #   
  #   # also only use the 'source' argument in the title if we are plotting only data from only one source 
  #   if(multiple.sources)  title <- makePlotTitle(quant@name, layer = layer.string, source = NULL, period = temporal.extent) 
  #   else title <- makePlotTitle(quant@name, layer = layer.string, source = sources, period = source@temporal.extent)
  #   
  # }
  
  ### BUILD THE PLOT
  
  # basic plot building
  mp <- ggplot(data = as.data.frame(data.toplot))
  #if(tile) mp <- mp + geom_tile(aes_string(x = "Lon", y = "Lat", fill = "Value"))
  #else mp <- mp + geom_raster(aes_string(x = "Lon", y = "Lat", fill = "Value"), interpolate = interpolate)
  
  # facet with grid or wrap 
  if(facet){
    
    # note that we add each facet as an individual layer to support multiple resolutions
    if(wrap){
      for(facet in unique(data.toplot[["Facet"]])){ mp <- mp + geom_raster(data = data.toplot[Facet == facet,], aes_string(x = "Lon", y = "Lat", fill = "Value")) }
      mp <- mp + facet_wrap(~ Facet)      
    }
    if(grid){
      for(col1 in unique(data.toplot[[grid.columns[1]]])){ 
        for(col2 in unique(data.toplot[[grid.columns[2]]])){ 
          mp <- mp + geom_raster(data = data.toplot[get(grid.columns[1]) == col1 && get(grid.columns[2]) == col2,], aes_string(x = "Lon", y = "Lat", fill = "Value")) 
        }
      }
      mp <- mp + facet_grid(stats::as.formula(paste(grid.string)), switch = "y")    
    }
    
  }
  # else simple case with no facetting
  else {
    mp <- mp + geom_raster(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value")) 
  }
  
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

