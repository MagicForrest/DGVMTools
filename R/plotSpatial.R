#!/usr/bin/Rscript


#########################################################################################################################################
################################################## PLOT SPATIAL #########################################################################
#########################################################################################################################################


#' Plot maps from a \code{Field} and a lists of \code{Field}.
#' 
#' This is a heavy lifting function for plotting maps from Fields, DataObjects, and Comparisons (and lists of those things) with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  In principle you can supply only the objext and it will plot.
#' It is basically a complex wrapper for the ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param fields The data to plot. Can be a Field, a DataObject, or a list of including both
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.  Set to NULL for no title.
#' @param subtitle A character string to override the default subtitle. Set to NULL for no subtitle.
#' Note that using these, especially "worldHires", can add quite a bit of time. 
#' @param facet.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param facet.order A vector of the characters that, if supplied, control the order of the facets.  To see what these values are you can call this funtion with "plot=FALSE"
#' and check the values of the Facet column.  But generally they will be the values of the @names slots of the Data/Fields and/or the layers (as layers plotted as defined by the layers arguments 
#' in this function). 
#' @param plot.bg.col Colour string for the plot background, default "white".
#' @param panel.bg.col Colour string for the panel background, default "white".
#' @param useLongNames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots.
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param ylim An optional vector of two numerics to specify the y/latitude range of the plot.
#' @param xlim An optional vector of two numerics to specify the x/longitude range of the plot.
#' @param years An optional numeric vector specifying which years to plot (take care, this defaults to all the years in the input Fields which can be a lot!)
#' @param days An optional numeric vector specifying which days to plot (take care, this defaults to all the days in the input Fields which can be a lot!)
#' @param months An optional numeric vector specifying which months to plot(defaults to all the days in the input Fields)
#' @param seasons An optional character vector specifying which seasons to plot (any or all of "DJF", "MAM, "JJA", "SON", defaults to all the seasons in the input Fields)  
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param cols A colour palette function to override the defaults.
#' @param cuts Cut ranges (a numeric vector) to override the default colour delimitation,  discretise the data into discrete colour bands
#' @param drop.cuts Logical, if TRUE then drop cut at each end which do not have any data in them in order more to fully use the colour scale.  
#' Default is TRUE.  Ignored if 'cuts' argument is not used.
#' @param grid Boolean, if TRUE then don't use facet_grid() to order the panels in a grid.  Instead use facet_wrap().  
#' Useful when not all combinations of Sources x Layers exist which would leave blank panels.
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param map.overlay A character string specifying which map overlay (from the maps and mapdata packages) should be overlain.  
#' @param interior.lines Boolean, if TRUE plot country lines with the continent outlines of the the requested map.overlay
#' Other things can be overlain on the resulting plot with further ggplot2 commands.
#' @param time Logical, if TRUE use \code{geom_tile} instead of \code{geom_raster}.  The advantage is that plots made with \code{geom_tile} are more malleable and can, 
#' for example, be plotted on ploar coordinates.  However \code{geom_tile} is much slower than \code{geom_raster}.
#' 
#' @details  This function is heavily used by the benchmarking functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' However, the \code{plotGGSpatial} function makes pretty plots with a simpler syntax, but with less flexibility.
#' 
#' The function works best for \code{Fields} (which contain a lot of useful metadata).   
#' 
#' @return Returns a ggplot object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' 
#' @export 
#' @seealso \code{plotTemporal}
#' 
plotSpatial <- function(fields, # can be a Field or a list of Fields
                        layers = NULL,
                        title = character(0),
                        subtitle = character(0),
                        facet.labels =  NULL,
                        facet.order = NULL,
                        plot.bg.col =  "white",
                        panel.bg.col = "white", #"809DB8", #"cae1ff",
                        useLongNames = FALSE,
                        text.multiplier = NULL,
                        xlim = NULL,
                        ylim = NULL,
                        years = NULL,
                        days = NULL,
                        months = NULL,
                        seasons = NULL,
                        limits = NULL,
                        cols = NULL,
                        cuts = NULL,
                        drop.cuts = TRUE,
                        map.overlay = NULL,
                        grid = FALSE,
                        plot = TRUE,
                        interior.lines = TRUE,
                        tile = FALSE){
  
  J = Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = NULL
  
  ### CHECK FOR MISSING OR INCONSISTENT ARGUMENTS AND INITIALISE STUFF WHERE APPROPRIATE
  categorical.legend.labels <- waiver()
  legend.title <- NULL
  drop.from.scale <- waiver() # only set to FALSE when discretising a scale 
  
  
  ### SANITISE FIELDS, LAYERS AND STINFO
  
  ### 1. FIELDS - check the input Field objects (and if it is a single Field put it into a one-item list)
  
  fields <- santiseFieldsForPlotting(fields)
  if(is.null(fields)) return(NULL)
  
  ### 2. LAYERS - check the number of layers
  
  layers <- santiseLayersForPlotting(fields, layers)
  if(is.null(layers)) return(NULL)
  
  
  ### 3. DIMENSIONS - check the dimensions (require that all fields the same dimensions and that they include 'Lon' and 'Lat' )
  
  dim.names <- santiseDimensionsForPlotting(fields, require = c("Lon", "Lat"))
  if(is.null(dim.names)) return(NULL)
  
  
  
  
  
  ##### CHECK POTENTIAL TEMPORAL FACET AXES AND GET A LIST OF VALUES PRESENT FOR PLOTTING
  
  if("Day" %in% dim.names) days <- checkDimensionValues(fields, days, "Day")
  if("Month" %in% dim.names) months <- checkDimensionValues(fields, months, "Month") 
  if("Season" %in% dim.names) seasons <- checkDimensionValues(fields, seasons, "Season") 
  if("Year" %in% dim.names) years <- checkDimensionValues(fields, years, "Year")
  
  
  
  ### PREPARE DATA FOR PLOTTING
  # Here we extract the data.table from the object to be plotted and melt it as appropriate
  # we also make a list of PFTs in case we need them for colour matching later
  data.toplot.list <- list()
  
  # Loop through the objects and pull layers from each one into a large data.table for plotting
  
  discrete <- FALSE
  continuous <- FALSE
  first <- TRUE
  final.fields <- list()
  PFTs <- list()
  for(object in fields){
    
    # check that at least one layer is present in this object and make a list of those which are
    all.layers <- names(object)
    something.present <- FALSE
    layers.present <- c()
    for(this.layer in layers) {
      if(this.layer %in% all.layers)  layers.present <- append(layers.present, this.layer)
    }
    if(length(layers.present) > 0) something.present <- TRUE
    
    # if at least one layer present subset it
    if(something.present) {
      
      
      # select the layers and time periods required and mash the data into shape
      these.layers <- selectLayers(object, layers.present)
      if(!is.null(years)) {
        # set key to Year, subset by years, the set keys back
        # not were are not doing selectYears() because we may want to select non-contiguous years
        setkey(these.layers@data, Year)
        these.layers@data <- these.layers@data[J(years)]
        setKeyDGVM(these.layers@data)
      }
      if(!is.null(days)) these.layers <- selectDays(these.layers, days)
      if(!is.null(months)) these.layers <- selectMonths(these.layers, months)
      if(!is.null(seasons)) these.layers <- selectSeasons(these.layers, seasons)
      
      final.fields <- append(final.fields, these.layers)
      
      these.layers.melted <- melt(these.layers@data, measure.vars = layers.present)
      these.layers.melted[, Source := object@source@name]
      data.toplot.list[[length(data.toplot.list)+1]] <- these.layers.melted
      
      # check if layers are all continuous or discrete
      for(layer in layers.present) {
        if(class(object@data[[layer]]) == "factor" || class(object@data[[layer]]) == "logical" || class(object@data[[layer]]) == "ordered") discrete <- TRUE
        if(class(object@data[[layer]]) == "numeric" || class(object@data[[layer]]) == "integer" ) continuous <- TRUE
      }
      if(discrete & continuous) stop("plotSpatial cannot simultaneously plot discrete and continuous layers, check your layers") 
      if(!discrete & !continuous) stop("plotSpatial can only plot 'numeric', 'integer', 'factor' or 'logical' layers, check your layers")   
      
      # check for meta-data to automagic the plots a little bit if possble
      if(first) {
        
        quant <- object@quant
        if(length(quant@units) > 1) quant.is.categorical <-  TRUE
        else quant.is.categorical <-  FALSE
        if(continuous & !quant.is.categorical) {
          if(is.null(cols)) cols <- object@quant@colours(20)
          legend.title <- object@quant@units
        }
        
      }
      else {
        
        # check for consistent Quantity
        if(!identical(quant, object@quant, ignore.environment = TRUE)) warning("Not all of the Fields supplied in the list have the same Quantity, I am using the Quantity from the first one")
      }
      
      first <- FALSE
      PFTs <- append(PFTs, object@source@pft.set)
      
    } # end if something.present
    
  }
  
  # finally mash them all togther to make the final data.table to plot
  data.toplot <- rbindlist(data.toplot.list)
  
  # check the PFTs present and make a unique list
  PFTs <- unique(PFTs)
  
  ### Rename "variable" to "Layer" which makes more conceptual sense
  setnames(data.toplot, "variable", "Layer")
  setnames(data.toplot, "value", "Value")
  
  
  
  
  ### DETERMINE X- AND Y-LIMITS, CROP AND MAKE OVERLAY
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
  
  # prepare overlay
  all.lons <- sort(unique(data.toplot[["Lon"]]))
  if(!is.null(map.overlay)) map.overlay.df <- makeMapOverlay(map.overlay, all.lons, interior.lines, xlim, ylim) 
  
  
  ### DO CUTS IF DEFINED
  #  variable  is continuous but should be discretised
  has.been.discretized <- FALSE
  if(continuous & !is.null(cuts)) {
    
    # drop cut intervals at the beginning and the end to which don't have data in them to better use the colour scale.
    if(drop.cuts) {
      
      # find minimum cut
      this.min <- min(data.toplot[["Value"]], na.rm = TRUE)
      min.cut.index <- 1
      for(cut.index in 1:length(cuts)) {
        if(this.min > cuts[cut.index]) min.cut.index <- cut.index
      }
      
      # find maximum cut
      this.max <- max(data.toplot[["Value"]], na.rm = TRUE)
      max.cut.index <- length(cuts)
      for(cut.index in length(cuts):1) {
        if(this.max < cuts[cut.index]) max.cut.index <- cut.index
      }
      
      # subset the cuts
      cuts <- cuts[min.cut.index:max.cut.index]
      
    }
    
    # apply the cuts
    data.toplot[,Value:= cut(Value, cuts, right = FALSE, include.lowest = TRUE, ordered_result = FALSE)]
    
    # set flags
    discrete <- TRUE
    continuous <- FALSE
    has.been.discretized <- TRUE
    
    # set colours, labels and breaks
    breaks <- levels(data.toplot[["Value"]])
    categorical.legend.labels <- breaks
    cols <- grDevices::colorRampPalette(cols)(length(cuts))
    names(cols) <- breaks
    drop.from.scale <- FALSE
    
  }
  
  
  ### FOR CONTININOUS VARIABLE, CATCH THE CASE WHERE NO COLOURS HAVE BEEN PROVIDED
  if(continuous) {
    
    # this solves the special case that a continuous layer has been derived from a categorical quant 
    # and no colours have been supplied
    if(quant.is.categorical && is.null(cols)) {
      cols <- viridis::viridis(11)
    }
    
  }
  
  ### IF PLOT IS DISCRETE, BUILD THE COLOURS 
  else if(discrete) {
    
    # make a list of all the unique values (factors), each of these will need a colour
    unique.vals <- unique(data.toplot[["Value"]])
    values.are.logical <- is.logical(data.toplot[["Value"]])
    
    # Final results of stuff below
    here.cols <- c()
    all.Matched <- FALSE
    
    ###  If the Quantity if specifically defined as categorical then use the colours defined in the Quantity's units slot
    ###  or use the provided col arguments to over-ride.
    ###  Catch the logical case first
    if(values.are.logical) {
      
      # and set the break and cols to what we just figured out
      breaks <- c(TRUE,FALSE)
      cols <- c("red", "blue")
      
    }
    else if(quant.is.categorical) {
      
      legend.title = NULL
      
      # if colours not supplied,  reverse engineer colours from palette
      if(missing(cols) || is.null(cols)) { 
        quant.cols <- quant@colours(length(quant@units)) 
        names(quant.cols) <- quant@units
      }
      # else use supplied colours
      else{
        quant.cols <- cols
      }
      
      # and set the break and cols to what we just figured out
      breaks <- quant@units
      cols <- quant.cols
      
    }
    
    ### Else data is categorical, but not explicitly defined as so in the Quantity, so we need to lookup colours for each factor
    else {
      
      # Attempt to match the colours
      here.cols <- matchPFTCols(unique.vals, PFTs)
      if(length(here.cols) == length(unique.vals)) all.Matched <- TRUE
      
      
      # If found colours for all the factors, set the values for plotting
      if(all.Matched) {
        if(is.null(cols)) cols <- here.cols
        legend.title <- element_blank()
        breaks <- sort(names(cols))
        if(useLongNames) {
          categorical.legend.labels <- c()
          for(this.break in breaks){
            
            if(this.break == "None") {
              categorical.legend.labels <- append(categorical.legend.labels, "None")
            }
            else {
              for(PFT in PFTs) {
                if(this.break == PFT@id) categorical.legend.labels <- append(categorical.legend.labels, PFT@name)
              } # for each PFT   
            } # if note "None"
            
          } # for each breal
        } # if useLongNames
      } # if all.Matched 
      
      else if(!has.been.discretized){
        breaks <- waiver()
        cols <- viridis::viridis(length(unique.vals))
      } # else if not been discretised
      
    } # if not categorical
    
  }  # if discrete
  
  else {
    stop("Not discrete or continuous??")
  }
  
  
  
  
  
  
  ### DEAL WITH MONTHS - swap 1,2,3... for Jan,Feb,Mar... if plotting months
  if("Month" %in% names(data.toplot)) {
    
    # make a list of months from the meta data
    month.list <- c()
    for(this.month in all.months) { 
      if(this.month@index %in% months) {
        month.list <- append(month.list, this.month@id) 
      }
    }
    
    # apply replacement function
    data.toplot[, Month := factor(Month, labels = month.list)]
    setKeyDGVM(data.toplot)
    
  }
  
  ### HANDLE THE FACET GRID/WRAP ISSUE
  
  # first determine how many facet "dimensions" we have
  multiple.years = multiple.days =  multiple.months = multiple.seasons = FALSE
  if(!is.null(years)) multiple.years <- length(years) > 1
  if(!is.null(days)) multiple.days <- length(days) > 1
  if(!is.null(months)) multiple.months <- length(months) > 1
  if(!is.null(seasons)) multiple.seasons <- length(seasons) > 1
  
  multiple.fields <- length(fields) > 1
  multiple.layers <- length(layers) > 1
  
  
  
  num.panel.dimensions <- sum(multiple.fields, multiple.layers, multiple.years, multiple.days, multiple.months, multiple.seasons)
  
  
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
  # else if got more that two of multiple fields, layers or years then gridding is impossible, must use facet_wrap()
  else if(num.panel.dimensions > 2){
    facet <- TRUE
    wrap <- TRUE
    if(grid) warning("Option grid is TRUE, but there are too many plot 'dimensions' to make a 2D grid of panels so I am ignoring the grid option.")
    grid <- FALSE
  }
  
  
  # if wrapping, add a new column to uniquely describe each facet
  if(wrap){
    factor.levels <- ""
    data.toplot[, Facet := ""]
    if(multiple.layers) { 
      data.toplot[, Facet := paste(Facet, Layer)] 
      factor.levels <- as.vector(outer(factor.levels, unique(data.toplot[["Layer"]]), paste))
    }
    if(multiple.fields) {
      data.toplot[, Facet := paste(Facet, Source)] 
      factor.levels <- as.vector(outer(factor.levels, unique(data.toplot[["Source"]]), paste))
    }
    if(multiple.years) { 
      data.toplot[, Facet := paste(Facet, Year)] 
      factor.levels <- as.vector(outer(factor.levels, unique(data.toplot[["Year"]]), paste))
    }
    if(multiple.days) { 
      data.toplot[, Facet := paste(Facet, Day)]
      factor.levels <- as.vector(outer(factor.levels, unique(data.toplot[["Day"]]), paste))
    }
    if(multiple.months) {
      data.toplot[, Facet := paste(Facet, Month)]
      factor.levels <- as.vector(outer(factor.levels, unique(data.toplot[["Month"]]), paste))
    }
    if(multiple.seasons) { 
      
      data.toplot[, Facet := paste(Facet, Season)] 
      #  special case for the seasons (since they have an 'inherent' ordering not reflected in their alphabetical ordering)
      #  -> re-order the "unique(data.toplot[["Season"]])" part to orders the panels correctly
      final.ordered <- c()
      for(season in all.seasons){
        if(season@id %in% unique(data.toplot[["Season"]])) final.ordered <- append(final.ordered, season@id)
      }
      factor.levels <- as.vector(outer(factor.levels, final.ordered, paste))
      
    }
    data.toplot[, Facet := factor(trimws(Facet), levels = trimws(factor.levels))]
    
    # if facet order specified, re-order the facets
    if(!is.null(facet.order)) {
      if(length(facet.order) != length(levels(data.toplot[["Facet"]]))) {
        warning(paste("You have not supplied the correct number of facets in the \'facet.order\' argument, (you supplied ", length(facet.order), "but I need", length(levels(data.toplot[["Facet"]])), ")", "so I am ignoring your facte re-ordering command", sep = " "))
      }
      else {
        data.toplot[, Facet := factor(trimws(Facet), levels = trimws(facet.order))]
      }
      
    }
    
  }
  
  
  # if gridding get the columns to grid by
  if(grid){
    grid.columns <- c()
    if(multiple.layers) {
      grid.columns <- append(grid.columns, "Layer")
      data.toplot[, Layer := factor(Layer, levels = unique(data.toplot[["Layer"]]))]
    }
    if(multiple.fields) {
      grid.columns <- append(grid.columns, "Source")
      data.toplot[, Source := factor(Source, levels = unique(data.toplot[["Source"]]))]
    }
    if(multiple.years) {
      grid.columns <- append(grid.columns, "Year")
      data.toplot[, Year := factor(Year, levels = unique(data.toplot[["Year"]]))]
    }
    if(multiple.days) {
      grid.columns <- append(grid.columns, "Day")
      data.toplot[, Day := factor(Day, levels = unique(data.toplot[["Day"]]))]
    }
    if(multiple.months) {
      grid.columns <- append(grid.columns, "Month")
      data.toplot[, Month := factor(Month, levels = unique(data.toplot[["Month"]]))]
    }
    if(multiple.seasons) {
      grid.columns <- append(grid.columns, "Season")
      final.ordered <- c()
      for(season in all.seasons){
        if(season@id %in% unique(data.toplot[["Season"]])) final.ordered <- append(final.ordered, season@id)
      }
      data.toplot[, Season := factor(Season, levels = final.ordered)]
    }
    grid.string <- paste(grid.columns, collapse = "~")
  }
  
  
  ### RETURN DATA.TABLE ONLY NOT PLOT REQUESTED
  if(!plot) return(data.toplot)
  
  
  
  ### MAKE A DESCRIPTIVE TITLE AND SUBTITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(final.fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  ### BUILD THE PLOT
  
  # basic plot building
  mp <- ggplot(data = as.data.frame(data.toplot))
  
  # facet with grid or wrap 
  if(facet){
    
    # note that we add each facet as an individual layer to support multiple resolutions
    if(wrap){
      for(facet in levels(data.toplot[["Facet"]])){
        if(!tile) mp <- mp + geom_raster(data = data.toplot[Facet == facet,], aes_string(x = "Lon", y = "Lat", fill = "Value")) 
        else mp <- mp + geom_tile(data = data.toplot[Facet == facet,], aes_string(x = "Lon", y = "Lat", fill = "Value")) 
      }
      mp <- mp + facet_wrap(~Facet)      
    }
    if(grid){
      for(col1 in unique(data.toplot[[grid.columns[1]]])){ 
        for(col2 in unique(data.toplot[[grid.columns[2]]])){ 
          if(!tile) mp <- mp + geom_raster(data = data.toplot[get(grid.columns[1]) == col1 && get(grid.columns[2]) == col2,], aes_string(x = "Lon", y = "Lat", fill = "Value")) 
          else mp <- mp + geom_tile(data = data.toplot[get(grid.columns[1]) == col1 && get(grid.columns[2]) == col2,], aes_string(x = "Lon", y = "Lat", fill = "Value")) 
        }
      }
      mp <- mp + facet_grid(stats::as.formula(paste(grid.string)), switch = "y")    
    }
    
  }
  # else simple case with no facetting
  else {
    if(!tile) mp <- mp + geom_raster(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value")) 
    else mp <- mp + geom_tile(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value")) 
  }
  
  # colour bar
  if(continuous)  {
    
    mp <- mp + scale_fill_gradientn(name = legend.title,
                                    limits = limits,
                                    colors = cols,
                                    na.value="grey75",
                                    guide = "legend")
    
    
    # MF TODO this code is problematic if the legend is subsequently moved, should maybe change this.  Maybe scale barheight and barwidth by text.multiplier?
    # In the meantine, workaround is finalplot <- finalplot +  guides(fill = guide_colorbar(barwidth = NULL, barheight = NULL))
    bar.height.unit <- unit(0.7, units = "npc")
    mp <- mp + guides(fill = guide_colorbar(barwidth = 2, barheight = bar.height.unit))
    
    
  }
  if(discrete) {
    mp <- mp + scale_fill_manual(values = cols, 
                                 breaks = breaks,
                                 labels = categorical.legend.labels, 
                                 drop = drop.from.scale)
  }
  
  # crop to xlim and ylim as appropriate and fix the aspect ratio 
  if(!is.null(xlim)) mp <- mp + scale_x_continuous(limits = xlim, expand = c(0, 0))
  else   mp <- mp + scale_x_continuous(expand = c(0, 0))  
  
  if(!is.null(ylim)) mp <- mp + scale_y_continuous(limits = ylim, expand = c(0, 0))
  else   mp <- mp + scale_y_continuous(expand = c(0, 0))
  
  mp <- mp + coord_fixed()
  
  # labels and positioning
  mp <- mp + labs(title = title, subtitle = subtitle)
  mp <- mp + labs(y = "Latitude", x = "Longitude")
  if(!is.null(legend.title)) {mp <- mp + labs(fill=legend.title) }
  else { mp <- mp + theme(legend.title = element_blank()) }
  mp <- mp + theme(plot.title = element_text(hjust = 0.5),
                   plot.subtitle = element_text(hjust = 0.5))
  
  # overall text multiplier
  if(!missing(text.multiplier)) mp <- mp + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  # set background colour of panel
  mp <- mp + theme(
    plot.background = element_rect(fill = plot.bg.col), # bg of the plot
    panel.background = element_rect(fill = panel.bg.col), # bg of the panel
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
    suppressWarnings(mp <- mp + geom_path(data=map.overlay.df, size=0.1, color = "black", aes(x=long, y=lat, group = group)))
  }
  
  return(mp)
  
  
}

