#' Plot temporal data
#' 
#' Makes a line plot graphing the temporal evolution of data (using ggplot2).  Allows control of panel layout and line aesthetics whilst simultaneously plotting
#' multiple Sources, Layers, Gridcells (Sites) and Quantities.
#'
#' @param fields The data to be plotted, either as a Field or list of Fields.  
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param gridcells A list of gridcells to be plotted (either in different panels or the same panel). For formatting of this argument see \code{selectGridcells}.  
#' Leave empty or NULL to plot all gridcells (but note that if this involves too many gridcells the code will stop) 
#' @param title A character string to override the default title.  Set to NULL for no title.
#' @param subtitle A character string to override the default subtitle. Set to NULL for no subtitle.
#' @param col.by,linetype.by,linewidth.by,size.by,shape.by,alpha.by Character strings defining the aspects of the data which which should be used to set the colour, line type, line width, point size and point shape and alpha (transparency).
#' Can meaningfully take the values "Layer", "Source", "Site" or "Quantity". By default \code{col.by} is set to "Layer" and all others set to NULL, which means the different aspects are 
#' distinguished by different facet panels.  Thus the standard behaviour is that different Layers are distinguished by different colours, but everything is separated into different panels.
#' @param cols,linetypes,linewidths,sizes,shapes,alphas Values of  colours, line types, line width, point sizes, point shapes or alpha values (respectively).
#' These can either be a single values if the aesthetic has not specified by xxx.by argument above of a vector of values, or a vector of values which
#' can/should be named to match particular col/size/linetype/shape/alpha values to particular Layers/Sources/Sites/Quantities.     
#' @param col.labels,linetype.labels,linewidth.labels,size.labels,shape.labels,alpha.labels A vector of character strings which are used as the labels for the lines/points. Must have the same length as the
#' number of Sources/Layers/Site/Quantities in the plot.  The vectors can/should be named to match particular col/size/linetype/linewidth/shape/alpha values to particular Layers/Sources/Sites/Quantities.    
#' @param x.label,y.label Character strings (or expressions) for the x and y axes (optional)
#' @param x.lim,y.lim Limits for the x and y axes (each a two-element numeric, optional)
#' @param points Logical, if TRUE plot data as points (with geom_points) instead of lines (with geom_lines).  
#' Good for plotting time series with missing data where geom_lines joins lines over the gaps which is not helpful
#' @param legend.position Position of the legend, in the ggplot2 style.  Passed to the ggplot function \code{theme()}. Can be "none", "top", "bottom", "left" or "right" or two-element numeric vector
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot. 
#' @param plotTrend Logical, if TRUE plot the linear trend (default is FALSE)
#' @param dropEmpty Logical, if TRUE don't plot time series lines consisting only of zeros (default is FALSE).
#' @param plot Logical, if FALSE return the data.table of data instead of the plot
#' @param ... Arguments passed to \code{ggplot2::facet_wrap()} and \code{ggplot2::stat_smooth()}.  See the ggplot2 documentation for full details but the following are particularly useful.
#' \itemize{
#'  \item{"nrow"}{The number of rows of facets. (facet_wrap)}
#'  \item{"ncol"}{The number of columns of facets. (facet_wrap)}
#'  \item{"scales"}{Whether the scales (ie. x and y ranges) should be fixed for all facets.  Options are "fixed" (same scales on all facets, default)
#'  "free" (all facets can their x and y ranges), "free_x" and "free_y"  (only x and y ranges can vary, respectively). (facet_wrap)}
#'  \item{"labeller"}{A function to define the labels for the facets.  This is a little tricky, please look to the ggplot2 documentation. (facet_wrap)} 
#'  \item{"se"}{Boolean to determine whether or not to show the confidence intervals for the trend line (stat_smooth)} 
#' }
#'   
#' @details
#' 
#' It allows fairly fine-grained control with respect to labelling lines  corresponding to different Sources, Layers, Sites and Quantities with different colours, sizes, linetypes, alpha (transparency) values, and text labels.  It also
#' allows one to decide if you want different Sources/Layers/Quantities on the same panel or on different panels.  The default is to put different Sources
#' (ie. runs and datasets) and Quantities (ie different output variables) on different panels, and Layers on the same panel distinguished by colour.  
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2
#' @export
#' @return A ggplot
#'
plotTemporal <- function(fields, 
                         layers = NULL,
                         gridcells = NULL,
                         title = character(0),
                         subtitle = character(0),
                         cols = NULL,
                         col.by = "Layer",
                         col.labels = waiver(),
                         linetypes = NULL,
                         linetype.by = NULL,
                         linetype.labels = waiver(),
                         linewidths = NULL,
                         linewidth.by = NULL,
                         linewidth.labels = waiver(),
                         sizes = NULL,
                         size.by = NULL,
                         size.labels = waiver(),
                         shapes = NULL,
                         shape.by = NULL,
                         shape.labels = waiver(),
                         alphas = NULL,
                         alpha.by = NULL,
                         alpha.labels = waiver(),
                         y.label = NULL,
                         y.lim = NULL,
                         x.label = NULL,
                         x.lim = NULL,
                         points = FALSE,
                         legend.position = "bottom",
                         text.multiplier = NULL,
                         dropEmpty = FALSE,
                         plotTrend = FALSE,
                         plot = TRUE,
                         ...
){
  
  
  # Just to avoid WARNINGS when checking
  Time = Year = Season = Month = Day = Source = Value = value = variable = Lat = Lon = NULL
  
  
  ### 0. Check consistency of aesthetics
  
  if(points) {
    if(!is.null(linetype.by)) {warning("With plotTemporal, specifying  'linetype.by' argument is not consistent with 'points = TRUE'.  ggplot2 may give a warning or possibly fail.")}
    if(!is.null(linewidth.by)) {warning("With plotTemporal, specifying  'linewidth.by' argument is not consistent with 'points = TRUE'.  ggplot2 may give a warning or possibly fail.")}
  }
  else{
    if(!is.null(shape.by)) {warning("With plotTemporal, specifying  'shape.by' argument is not consistent with 'points = FALSE'.  ggplot2 may give a warning or possibly fail.")}
    if(!is.null(size.by)) {warning("With plotTemporal, specifying  'size.by' argument is not consistent with 'points = TRUE'.  ggplot2 may give a warning or possibly fail.")}
  }
  
  
  
  
  ### 1. FIELDS - check the input Field objects (and if it is a single Field put it into a one-item list)
  
  fields <- santiseFieldsForPlotting(fields)
  if(is.null(fields)) return(NULL)
  
  
  ### 2. LAYERS - check the layers
  
  layers <- santiseLayersForPlotting(fields, layers)
  if(is.null(layers)) return(NULL)
  
  
  ### 3. DIMENSIONS - check the dimensions (require that all fields have the same dimensions and that they include 'Year' )
  
  dim.names <- santiseDimensionsForPlotting(fields, require = c("Year"))
  if(is.null(dim.names)) return(NULL)
  
  
  ### 4. PREPARE AND CHECK DATA FOR PLOTTING
  
  # first select the layers and points in space-time that we want to plot
  final.fields <- trimFieldsForPlotting(fields, layers, gridcells = gridcells)
  
  
  ### 5. CHECK IF ALL LAYERS ARE CONTINOUS - if not fail
  for(this.field in final.fields) {
    for(layer in layers(this.field)) {
      if(!(is(this.field@data[[layer]], "numeric") || is(this.field@data[[layer]], "integer" ))) {
        stop("plotTemoral can only plot continuous layers ie. 'integer' or 'numeric' types, not 'logical' or 'factor' data.")
      }
    }
  }
  
  
  ###  6. MERGE THE FINAL FIELDS FOR PLOTTING - INCLUDING METADATA COLUMNS FOR FACETTING AND AESTHEICS
  
  # MF TODO maybe make some clever checks on these switches
  add.Quantity <- TRUE
  if("Lon" %in% dim.names & "Lat" %in% dim.names) add.Site <- TRUE
  else add.Site <- FALSE
  add.Region <- TRUE
  
  # Final data.table for plotting.  Actual values are in a column called "Value"
  data.toplot <- mergeFieldsForPlotting(final.fields, add.Quantity = add.Quantity, add.Site = add.Site, add.Region = add.Region)
  
  ### 7. MAKE THE Y-AXIS LABEL
  if(is.null(y.label)) {
    y.label <- stringToExpression(makeYAxis(final.fields))
  }
  
  # check the defined Layers present in the Fields and make a unique list
  # maybe also here check which one are actually in the layers to plot, since we have that information
  all.layers.defined <- list()
  for(object in fields){
    all.layers.defined <- append(all.layers.defined, object@source@defined.layers)
  }
  all.layers.defined <- unique(all.layers.defined)
  
  
  ### 8. MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  
  # helpful check here
  if(nrow(data.toplot) == 0) stop("Trying to plot an empty data.table in plotTemporal, something has gone wrong.  Perhaps you are selecting a site that isn't there?")
  
  
  
  ### 9. MAKE A 'Time' COLUMN FOR THE X-AXIS
  earliest.year <- min(data.toplot[["Year"]])
  if(earliest.year >= 0) {
    # convert years and months to dates 
    if("Year" %in% names(data.toplot) && "Month" %in% names(data.toplot)) {
      pad <- function(x) { ifelse(x < 10, paste0(0,x), paste0(x)) }
      data.toplot[, Time := as.Date(paste0(Year, "-", pad(Month), "-01"), format = "%Y-%m-%d")]
      data.toplot[, Year := NULL]
      data.toplot[, Month := NULL]
    }
    # convert years and days to dates 
    else if("Year" %in% names(data.toplot) && "Day" %in% names(data.toplot)) {
      pad <- function(x) { ifelse(x < 10, paste0(0,x), paste0(x)) }
      data.toplot[, Time := as.Date(paste0(Year, "-", Day), format = "%Y-%j")]
      data.toplot[, Year := NULL]
      data.toplot[, Day := NULL]
    }
    # convert years and seasons to dates 
    else if("Year" %in% names(data.toplot) && "Season" %in% names(data.toplot)) {
      # make a Day colum based on the centre point of a Season
      day.lookup <- c("DJF" = 14, "MAM" = 105, "JJA" = 196, "SON" = 287)
      data.toplot[, Day := day.lookup[Season]]
      data.toplot[, Time := as.Date(paste0(Year, "-", Day), format = "%Y-%j")]
      data.toplot[, Year := NULL]
      data.toplot[, Season := NULL]
      data.toplot[, Day := NULL]
      
    }
    # convert years to dates 
    else if("Year" %in% names(data.toplot)) {
      data.toplot[, Time := as.Date(paste0(Year, "-01-01"), format = "%Y-%m-%d")]
      data.toplot[, Year := NULL]
    }
  }
  else {
    if("Year" %in% names(data.toplot) && "Month" %in% names(data.toplot)) {
      latest.year <- max(data.toplot[["Year"]])
      print(latest.year)
      print(earliest.year)
      earliest.year.days <- as.numeric(earliest.year, as.Date(("0001-01-01")))
      latest.year.days <- as.numeric(latest.year, as.Date(("0001-01-01")))
      print(earliest.year.days)
      print(latest.year.days)
      stop("Hmm... not yet sure how to plot months with negative years")
    }
    else if("Year" %in% names(data.toplot)) {
      data.toplot[, Time := Year]
      data.toplot[, Year := NULL]
    }
    #
  }
  
  ### 10. FACETTING
  
  # all column names, used a lot below 
  all.columns <- names(data.toplot)
  
  # check the "xxx.by" arguments 
  if(!missing(col.by) && !is.null(col.by) && !col.by %in% all.columns) stop(paste("Colouring by", col.by, "requested, but that is not available, so failing."))
  if(!missing(linetype.by) && !is.null(linetype.by) && !linetype.by %in% all.columns) stop(paste("Setting linetypes by", linetype.by, "requested, but that is not available, so failing."))
  if(!missing(linewidth.by) && !is.null(linewidth.by) && !linewidth.by %in% all.columns) stop(paste("Setting linewidth by", linewidth.by, "requested, but that is not available, so failing."))
  if(!missing(size.by) && !is.null(size.by) && !size.by %in% all.columns) stop(paste("Setting sizes by", size.by, "requested, but that is not available, so failing."))
  if(!missing(shape.by) && !is.null(shape.by) && !shape.by %in% all.columns) stop(paste("Setting shapes by", shape.by, "requested, but that is not available, so failing."))
  if(!missing(alpha.by) && !is.null(alpha.by) && !alpha.by %in% all.columns) stop(paste("Setting alphas by", alpha.by, "requested, but that is not available, so failing."))
  
  # ar first assume facetting by everything except for...
  dontFacet <- c("Value", "Time", "Year", "Month", "Season", "Day", "Lon", "Lat", col.by, linetype.by, linewidth.by,  size.by, shape.by, alpha.by)
  vars.facet <- all.columns[!all.columns %in% dontFacet]
  
  # then remove facets with only one unique value
  for(this.facet in vars.facet) {
    if(length(unique(data.toplot[[this.facet]])) == 1) vars.facet <- vars.facet[!vars.facet == this.facet]
  }
  
  
  
  
  ### LINE COLOURS
  
  # if cols is not specified and plots are to be coloured by Layers, look up line colours from Layer meta-data
  if(missing(cols) && !is.null(col.by) && col.by == "Layer"){
    all.layers <- unique(as.character(data.toplot[["Layer"]]))
    cols <- matchLayerCols(all.layers, all.layers.defined)
  }
  # else colours will be determined by ggplot (or cols argument)
  
  ### LINETYPES, SIZES & ALPHAS
  # Thus far either ignored or specified by the user
  
  ### LABELS
  # Can be specified by the user, otherwise sensible defaults
  
  
  ### LEGEND ENTRY ORDERING
  ## Fix order of items in legend(s) by making them factors with levels corresponding to the order of the input fields
  all.sources <- list()
  # first loop across the fields
  for(this.field in fields) {
    all.sources <- append(all.sources, this.field@source@name)
  }
  if("Source" %in% names(data.toplot)) data.toplot[, Source := factor(Source, levels = unique(all.sources))]
  
  
  ### If requested, just return the data
  if(!plot) return(data.toplot)
  
  ### PLOT! - now make the plot
  
  # first make the "symbols" for the ggplot2 call.  A bit of a pain -since they ggplot2 folks took away aes_string()- but what can you do...
  col.sym <- if(is.character(col.by)) ensym(col.by) else NULL  
  alpha.sym <- if(is.character(alpha.by))  ensym(alpha.by) else NULL 
  size.sym <- if(is.character(size.by)) ensym(size.by) else  NULL  
  shape.sym <- if(is.character(shape.by)) ensym(shape.by) else NULL  
  linewidth.sym <- if(is.character(linewidth.by)) ensym(linewidth.by) else NULL
  linetype.sym <- if(is.character(linetype.by)) ensym(linetype.by) else  NULL
 
  p <- ggplot(as.data.frame(data.toplot), aes(x = Time, y = Value, 
                                              col = !! col.sym, 
                                              alpha = !! alpha.sym,
                                              size = !! size.sym,
                                              shape = !! shape.sym,
                                              linetype = !! linetype.sym,
                                              linewidth = !! linewidth.sym))
  
  # add trend lines
  if(plotTrend) suppressWarnings( p <- p + stat_smooth(method = "lm", formula = y ~ x, ...) )
  
  # build arguments for aesthetics to geom_line/geom_line and/or fixed arguments outside
  geom_args <- list()
  # col and alpha (for both geom_points and geom_line)
  if(!is.null(cols) && is.null(col.by)) geom_args[["col"]] <- cols
  if(!is.null(alphas) && is.null(alpha.by)) geom_args[["alpha"]] <- alphas
  # for points only  
  if(points){
    if(!is.null(shapes) && is.null(shape.by)) geom_args[["shape"]] <- shapes
    if(!is.null(sizes) && is.null(size.by)) geom_args[["size"]] <- sizes
  }
  # for lines only
  else{
    if(!is.null(linetypes) && is.null(linetype.by)) geom_args[["linetype"]] <- linetypes
    if(!is.null(linewidths) && is.null(linewidth.by)) geom_args[["linewidth"]] <- linewidths
  }
  
  # call geom_line or geom_point (with fixed aesthetics defined above)
  if(points) p <- p + do.call(geom_point, geom_args)
  else p <- p + do.call(geom_line, geom_args)

  
  # apply labels
  if(!is.null(col.by) & !is.null(cols)) p <- p + scale_color_manual(values=cols, labels=col.labels)
  if(!is.null(alpha.by) & !is.null(alphas)) p <- p + scale_alpha_manual(values=alphas, labels=alpha.labels)
  if(points){
    if(!is.null(size.by) & !is.null(sizes)) p <- p + scale_size_manual(values=sizes, labels=size.labels)
    if(!is.null(shape.by) & !is.null(shapes)) p <- p + scale_shape_manual(values=shapes, labels=shape.labels)
  }
  else{
    if(!is.null(linewidth.by) & !is.null(linewidths)) p <- p + scale_linewidth_manual(values=linewidths, labels=linewidth.labels)
    if(!is.null(linetype.by) & !is.null(linetypes)) p <- p + scale_linetype_manual(values=linetypes, labels=linetype.labels)
  }
  
  # set the theme to theme_bw, simplest way to set the background to white
  p <- p + theme_bw()
  
  # labels and positioning
  p <- p + labs(title = title, subtitle = subtitle)
  
  p <- p + theme(legend.title=element_blank())
  p <- p + theme(legend.position = legend.position, legend.key.size = unit(2, 'lines'))
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5))
  
  # overall text multiplier
  if(!is.null(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))

  # set limits
  if(!is.null(x.lim)) p <- p + xlim(x.lim)
  if(!is.null(y.lim)) p <- p + scale_y_continuous(limits = y.lim, name = y.label)
  else p <- p + labs(y = y.label)
  
  # facetting
  if(length(vars.facet > 0)){
    suppressWarnings( p <- p + facet_wrap(vars.facet, ...))
  }
  
  return(p)
  
  
}