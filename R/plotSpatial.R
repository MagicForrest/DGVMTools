#!/usr/bin/Rscript


#########################################################################################################################################
################################################## PLOT SPATIAL #########################################################################
#########################################################################################################################################



#' Plot maps from a \code{\linkS4class{Field}} or a lists of \code{\linkS4class{Field}}
#' 
#' This is a heavy lifting function for plotting maps from Fields with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  
#' In principle you can supply only the Fields and it will plot something sensible. It extracts the relevant data from the Fields, bashes it into
#' a data.table then calls ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param fields The data to plot. Can be a Field or a list of Fields.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.  Set to NULL for no title.
#' @param subtitle A character string to override the default subtitle. Set to NULL for no subtitle.
#' @param legend.title A character string or expression to override the default legend title. Set to NULL for no legend title.  The default legend title is the \code{units}
#' of the \code{\linkS4class{Quantity}} of the first \code{\linkS4class{Field}} provided in the \code{field} argument.  This argument allows general flexibility, but it is particularly handy
#' to facilitate expressions for nicely marked up subscript and superscript. 
#' @param facet.order A vector of the characters that, if supplied, control the order of the facets.  To see what these values are you can call this funtion with "plot=FALSE"
#' and check the values of the Facet column.  But generally they will be the values of the @@names slots of the Data/Fields and/or the layers (as layers plotted as defined by the layers arguments 
#' in this function). 
#' @param plot.bg.col Colour string for the plot background, default "white".
#' @param panel.bg.col Colour string for the panel background, default "white".
#' @param useLongNames Boolean, if TRUE replace Layer IDs with the Layer's full names on the plots.
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
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param map.overlay An optional character string specifying either "coastlines" (alternatively "ne_coastlines") or "countries" (alternatively "ne_countries" or,
#' for backwards compatibility with previous versions "world") to plot either the coastlines or coastlines and country borders from the ranturalearth(data) packages.
#' Other things can be overlain on the resulting plot with further ggplot2 commands, and check the rnaturalearth package family for higher res versions, lakes, etc.
#' @param tile Logical, if TRUE use \code{geom_tile} instead of \code{geom_raster}.  The advantage is that plots made with \code{geom_tile} are more malleable and can, 
#' for example, be plotted on polar coordinates.  However \code{geom_tile} is much slower than \code{geom_raster}.
#' @param pixel.size Numeric, allows you to alter the plotted pixel size (height and width simultaneously using the same value).  This is useful 
#' if you are plotting a collection of individual sites which do not have regular spacing.  Note the "tile = TRUE" (see above) will automatically set if you haven't done it manually.
#' @param ... Arguments passed to \code{ggplot2::facet_wrap()}.  See the ggplot2 documentation for full details but the following are particularly useful.
#' \itemize{
#'  \item{"nrow"}{The number of rows of facets}
#'  \item{"ncol"}{The number of columns of facets}
#'  \item{"scales"}{Whether the scales (ie. x and y ranges) should be fixed for all facets.  Options are "fixed" (same scales on all facets, default)
#'  "free" (all facets can their x and y ranges), "free_x" and "free_y"  (only x and y ranges can vary, respectively).}
#'  \item{"labeller"}{A function to define the labels for the facets.  This is a little tricky, please look to the ggplot2 documentation.
#'  But basically what you want is to define a named character vector, the names are the previous facet names and the values are the new names.  
#'  Then make this into a function by passing it to the ggplot function "as.labeller", and then that becomes your 'labeller' argument.} 
#' }
#' 
#' @details  This function is the main spatial plotting functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' 
#' 
#' @return Returns a ggplot object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' 
#' @export 
#' @seealso \link{plotTemporal}, \link{plotSpatialComparison}
#' 
plotSpatial <- function(fields, # can be a Field or a list of Fields
                        layers = NULL,
                        title = character(0),
                        subtitle = character(0),
                        legend.title = character(0),
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
                        plot = TRUE,
                        tile = FALSE,
                        pixel.size = NULL,
                        ...){
  
  J = Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = Years = NULL
  
  ### CHECK FOR MISSING OR INCONSISTENT ARGUMENTS AND INITIALISE STUFF WHERE APPROPRIATE
  categorical.legend.labels <- waiver()
  if(missing(legend.title)) legend.title <- NULL
  drop.from.scale <- waiver() # only set to FALSE when discretising a scale 
  if(!missing(pixel.size) & !is.null(pixel.size) & !tile){
    warning("In plotSpatial(), if you specifiy the 'pixel.size' argument then the 'tile' argument should also be set to TRUE.  I have done this for you, but plotting might take longer as a result.")
    tile <- TRUE
  }
  
  
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
  
  
  ### 4. CHECK POTENTIAL TEMPORAL FACET AXES AND GET A LIST OF VALUES PRESENT FOR PLOTTING
  
  if("Day" %in% dim.names) days <- checkDimensionValues(fields, days, "Day")
  if("Month" %in% dim.names) months <- checkDimensionValues(fields, months, "Month") 
  if("Season" %in% dim.names) seasons <- checkDimensionValues(fields, seasons, "Season") 
  if("Year" %in% dim.names) years <- checkDimensionValues(fields, years, "Year")
  
  
  
  ### 5. PREPARE AND CHECK DATA FOR PLOTTING
  
  # first select the layers and points in space-time that we want to plot
  final.fields <- trimFieldsForPlotting(fields, layers, years = years, days = days, months = months, seasons = seasons)
  
  
  ### 6. LAYER TYPE CHECKS
  # check if layers are all continuous or discrete
  discrete <- FALSE
  continuous <- FALSE
  for(this.field in final.fields) {
    for(layer in layers(this.field)) {
      if(is(this.field@data[[layer]], "factor") || is(this.field@data[[layer]], "logical") || is(this.field@data[[layer]], "ordered")) discrete <- TRUE
      if(is(this.field@data[[layer]], "numeric") || is(this.field@data[[layer]], "integer" )) continuous <- TRUE
    }
    if(discrete & continuous) stop("plotSpatial cannot simultaneously plot discrete and continuous layers, check your layers") 
    if(!discrete & !continuous) stop("plotSpatial can only plot 'numeric', 'integer', 'factor' or 'logical' layers, check your layers")  
  }
  
  ### 7. MELT AND COMBINE THE FINAL FIELDS 
  # MF TODO: Consider adding add.Region like for plotTemporal?
  
  # first determine if there are different time periods (ie years) in the Fields to be plotted
  add.Years= FALSE
  if(length(final.fields) > 1){
    for(field.index.1 in 1:(length(final.fields)-1)){
      for(field.index.2 in (field.index.1+1):length(final.fields)){
        if(identical(final.fields[[field.index.1]]@source@name, final.fields[[field.index.2]]@source@name)
           && (!identical(final.fields[[field.index.1]]@first.year, final.fields[[field.index.2]]@first.year)
               || !identical(final.fields[[field.index.1]]@last.year, final.fields[[field.index.2]]@last.year))){
          add.Years= TRUE
        }
      }
    }
  }
  
  data.toplot <- mergeFieldsForPlotting(final.fields, add.Quantity = FALSE, add.Site = FALSE, add.Region = FALSE, add.Years = add.Years)
  
  
  ### Check for meta-data to automagic the plots a little bit if possble
  first <- TRUE
  for(this.field in final.fields) {
    if(first) {
      
      quant <- this.field@quant
      if(length(quant@units) > 1) quant.is.categorical <-  TRUE
      else quant.is.categorical <-  FALSE
      if(continuous & !quant.is.categorical) {
        if(is.null(cols)) cols <- this.field@quant@colours(20)
        if(is.null(legend.title)) legend.title <- this.field@quant@units
      }
      
    }
    else {
      
      # check for consistent Quantity
      if(!identical(quant, this.field@quant, ignore.environment = TRUE)) warning("Not all of the Fields supplied in the list have the same Quantity, I am using the Quantity from the first one")
    }
    
    first <- FALSE
    
  }
  
  ### LEGEND TITLE
  
  # attempt to convert the legend title to a string which can be evaluated as an expression using he units  package
  # (function includes exception handling)
  legend.title <- standardiseUnitString(legend.title)
  
  # convert said legend title to an expression (again includes exception handling)
  legend.title <- stringToExpression(legend.title)
  
  
  # check the defined Layers present in the Fields and make a unique list
  # maybe also here check which one are actually in the layers to plot, since we have that information
  all.layers.defined <- list()
  for(object in fields){
    all.layers.defined <- append(all.layers.defined, object@source@defined.layers)
  }
  all.layers.defined <- unique(all.layers.defined)
  
  
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
  

  ### DO CUTS IF DEFINED
  #  variable is continuous but should be discretised
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
      breaks <- c("Matches", "Different")
      cols <- c("red", "blue")
      
      # also set the values to be factors
      data.toplot[, Value := sapply(X = Value, FUN = function(x){ if(x) return("Matches"); return("Different")})]
      
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
      here.cols <- matchLayerCols(unique.vals, all.layers.defined)
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
              for(this.Layer in all.layers.defined) {
                if(this.break == this.Layer@id) categorical.legend.labels <- append(categorical.legend.labels, this.Layer@name)
              } # for each Layer   
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
  
  ### XX FACETTING
  
  # all column names, used a lot below 
  all.columns <- names(data.toplot)
  
  # at first, assume facetting by everything except for dontFacet values below
  # it is essential to facet be everything else otherwise data can be plotted over other data and you don't know
  dontFacet <- c("Value", "Lon", "Lat")
  facet.vars <- all.columns[!all.columns %in% dontFacet]
  
  # then remove facets with only one unique value
  for(this.facet in facet.vars) {
    if(length(unique(data.toplot[[this.facet]])) == 1) facet.vars <- facet.vars[!facet.vars == this.facet]
  }
  
  # if at least one facet variable make a "Facet" column of a specially ordered factor to control the facettigng  
  if(length(facet.vars) > 0) {
    
    
    # First, re-order so that Layer comes first and Day, Month, Season, Year, Years come at the end (in that order)
    # this just an aesthetic choice in the labelling
    if("Layer" %in% facet.vars) {
      facet.vars <- facet.vars[!facet.vars %in% "Layer"] 
      facet.vars <- append("Layer", facet.vars)
    } 
    for(this.ender in c("Day", "Month", "Season", "Year", "Years")){
      if(this.ender %in% facet.vars) {
        facet.vars <- facet.vars[!facet.vars %in% this.ender] 
        facet.vars <- append(facet.vars, this.ender)
      }
    }
    
    factor.levels <- ""
    data.toplot[, Facet := ""]
    
    # for each variable we are facetting over
    for(this.facet.var in facet.vars) {
      
      # add the facet variable name to the Facet column
      data.toplot[, Facet := paste(Facet, get(this.facet.var))] 
      
      # add to the list of factor levels in a way that produces a sensible ordering
      if(!this.facet.var == "Season") {
        factor.levels <- as.vector(outer(factor.levels, unique(data.toplot[[this.facet.var]]), paste))
      } 
      # SPECIAL CASE for the seasons (since they have an 'inherent' ordering not reflected in their alphabetical ordering)
      # -> re-order the "unique(data.toplot[["Season"]])" part to order the panels correctly
      else {
        final.ordered <- c()
        for(season in all.seasons){
          if(season@id %in% unique(data.toplot[["Season"]])) final.ordered <- append(final.ordered, season@id)
        }
        factor.levels <- as.vector(outer(factor.levels, final.ordered, paste))
      }
      
    }
    
    
    # trim white space, drop factors from the the list of levels which are not actually present in the Facet column 
    # and set the Facet column as an apropriately ordered fac
    data.toplot[, Facet := trimws(Facet)]
    factor.levels <- trimws(factor.levels)
    factor.levels <- factor.levels[which(factor.levels %in% unique(data.toplot[["Facet"]]))]
    data.toplot[, Facet := factor(Facet, levels = factor.levels)]
    
    # if facet order specified, use that instead
    if(!is.null(facet.order)) {
      
      if(length(facet.order) != length(levels(data.toplot[["Facet"]]))) {
        warning(paste("You have not supplied the correct number of facets in the \'facet.order\' argument, (you supplied ", length(facet.order), "but I need", length(levels(data.toplot[["Facet"]])), ")", "so I am ignoring your facte re-ordering command", sep = " "))
      }
      else {
        data.toplot[, Facet := factor(Facet, levels = facet.order)]
      }
      
    }
    
  } # end if facetting
  
  
  
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
  
  
  ### IF A 'limits' ARGUMENT HAS BEEN SET THEN LIMITS THE PLOTTED VALUES ACCORDINGLY
  if(!missing(limits) && !is.null(limits) && !is.character(data.toplot[["Value"]])){
    data.toplot[, Value := pmax(Value, limits[1])]
    data.toplot[, Value := pmin(Value, limits[2])]
  }
  
  ### BUILD THE PLOT
  
  # basic plot building
  mp <- ggplot(data = as.data.frame(data.toplot))
  
  ### MAKE THE PLOT - PANEL-BY-PANEL
  
  # if facetting required (column "Facet" will have been defined above)
  if(length(facet.vars > 0)){
    
    # note that we add each facet as an individual layer to support multiple resolutions
    for(facet in levels(data.toplot[["Facet"]])){
      if(!tile) mp <- mp + geom_raster(data = data.toplot[Facet == facet,], aes_string(x = "Lon", y = "Lat", fill = "Value"))
      else {
        if(!missing(pixel.size) & !is.null(pixel.size)) mp <- mp + geom_tile(data = data.toplot[Facet == facet,], aes_string(x = "Lon", y = "Lat", fill = "Value"), width = pixel.size, height = pixel.size)
        else mp <- mp + geom_tile(data = data.toplot[Facet == facet,], aes_string(x = "Lon", y = "Lat", fill = "Value"))
      }
    }
    mp <- mp + facet_wrap(~Facet, ...)
    
  }
  # else simple case with no facetting
  else {
    if(!tile) mp <- mp + geom_raster(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value"))
    else {
      if(!missing(pixel.size) & !is.null(pixel.size)) mp <- mp + geom_tile(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value"), width = pixel.size, height = pixel.size)
      else mp <- mp + geom_tile(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value"))
    }
  }
  
  
  # # Alternative facetting without using a "Facet" column, not used
  # # Note that this is in many ways much simpler and more elegant in the ggplot style *but* I can't figure out how to manipulate
  # # the facet labelling with the 'labeller' argument and because setting the correct order with multiple facetting variables 
  # # in a programatic way within this function will be complicated.  The better option is therefore to make a "Facet" column
  # # so that the facet ordering and labelling van be more easily manipulated
  # if(length(facet.vars > 0)){
  #   
  #   # get all the facet combos and set keys
  #   all.facet.combos <- unique(data.toplot[,facet.vars, with = FALSE])
  #   setkeyv(data.toplot, facet.vars)
  #   
  #   # plot each layer inviidiually (to presevre different resolution on each plo)
  #   for(row.index in 1:NROW(all.facet.combos)) { 
  #     if(!tile) mp <- mp + geom_raster(data = data.toplot[as.list(all.facet.combos[row.index])], aes_string(x = "Lon", y = "Lat", fill = "Value"))
  #     else mp <- mp + geom_tile(data = data.toplot[as.list(all.facet.combos[row.index])], aes_string(x = "Lon", y = "Lat", fill = "Value"))
  #   }
  #   
  #   # This is the trouble
  #   # mp <- mp + facet_wrap(facet.vars, labeller = label_context(multi_line = FALSE, sep = ", "), ...)
  #   # mp <- mp + facet_wrap(facet.vars, labeller = label_context(labels = all.facet.combos), ...)
  #   mp <- mp + facet_wrap(facet.vars2, ...)
  # }
  # else {
  #   if(!tile) mp <- mp + geom_raster(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value"))
  #   else mp <- mp + geom_tile(data = data.toplot, aes_string(x = "Lon", y = "Lat", fill = "Value"))
  # }
  
  
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
  
  # set these stuff, background colour of panel etc
  # these can all be altered after the fact with further ggplot commands
  mp <- mp + theme(
    plot.background = element_rect(fill = plot.bg.col), # bg of the plot
    panel.background = element_rect(fill = panel.bg.col), # bg of the panel
    legend.background = element_rect(fill = "transparent"), #, # get rid of legend bg
    panel.border = element_rect(colour = "black", fill=NA),
    strip.background  = element_rect(fill=NA)
  )
  
  
  
  # map overlay - suppress warning about additional coordinate system
  if(!is.null(map.overlay)) {  suppressWarnings(mp <- addMapOverlay(mp, map.overlay)) }
  
  
  return(mp)
  
  
}

