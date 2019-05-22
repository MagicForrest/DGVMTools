#' Plot temporal data
#' 
#' Makes a line plot graphing the temporal evolution of data (using ggplot2).  Full functionality not implemented, or even defined...  
#'
#' @param fields The data to be plotted, either as a Field, DataObject or a list of Model/DataObjects.  
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param gridcells A list of gridcells to be plotted in different panels, for formatting of this argument see \code{selectGridcells}.  
#' Leave empty or NULL to plot all gridcells (but note that if this involves too many gridcells the code will stop) 
#' @param title A character string to override the default title.  Set to NULL for no title.
#' @param subtitle A character string to override the default subtitle. Set to NULL for no subtitle.
#' @param cols,types Colour and types for the lines.  They do not each necessarily need to be specified, but if they are then the they need to be 
#' the same length as the labels arguments
#' @param labels A list of character strings which are used as the labels for the lines.  Must have the same length as the layers argument (after expansion if necessary)
#' @param x.label,y.label Character strings for the x and y axes (optional)
#' @param x.lim,y.lim Limits for the x and y axes (each a two-element numeric, optional)
#' @param facet Logical, if TRUE split the plot into panels by source.  If false, plots all data in a single panel. 
#' @param facet.scales Character string.  If faceting (see above) use "fixed" to specify same scales on each ribbon (default), or "free"/"free_x"/"free_y" for tailored scales
#' @param legend.position Position of the legend, in the ggplot2 style.  Passed to the ggplot function \code{theme()}. Can be "none", "top", "bottom", "left" or "right" or two-element numeric vector
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' @param plot Logical, if FALSE return the data.table of data instead of the plot
#' Make it bigger if the text is too small on large plots and vice-versa.
#'  
#' @details
#' This function is WORK IN PROGRESS!!  For questions about functionality or feature requests contact the author
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
                         types = NULL,
                         type.by = NULL,
                         type.labels = waiver(),
                         sizes = NULL,
                         size.by = NULL,
                         size.labels = waiver(),
                         alphas = NULL,
                         alpha.by = NULL,
                         alpha.labels = waiver(),
                         y.label = NULL,
                         y.lim = NULL,
                         x.label = NULL,
                         x.lim = NULL,
                         facet = TRUE,
                         facet.scales = "fixed",
                         legend.position = "bottom",
                         text.multiplier = NULL,
                         plot = TRUE
){
  
  
  # Just to avoid WARNINGS when checking
  Time = Year = Month = Day = Source = value = variable = Lat = Lon = NULL
  
  
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
  
  
  ### 5. CHECK IF ALL LAYES ARE CONTINOUS - if not fail
  for(this.field in final.fields) {
    for(layer in layers(this.field)) {
      if(!(class(this.field@data[[layer]]) == "numeric" || class(this.field@data[[layer]]) == "integer" )) {
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
  
  
  ### 6. MAKE THE Y-AXIS LABEL
  
  if(is.null(y.label)) {
    
    # first extract the names and units and store them in a tuples (two element vector) for the Quantity from each Field
    all.quant.tuples <- list()
    for(field in final.fields) {
      all.quant.tuples[[length(all.quant.tuples)+1]] <- c(field@quant@name, field@quant@units)
    } 
    
    # select the unique ones
    all.quant.tuples <- unique(all.quant.tuples)

    # form the label string
    y.axis.label <- character(0)
    for(this.tuple in all.quant.tuples) {
      y.axis.label <- paste0(y.axis.label, paste0(this.tuple[1], " (", this.tuple[2], "),\n") )
    }
    print(y.axis.label)
    y.axis.label <- substr(y.axis.label,  1, nchar(y.axis.label) - 2)
    print(y.axis.label)
  }
 
  # TODO quick n dirty
  PFTs <- fields[[1]]@source@pft.set
  
  
  
  
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  
  # helpful check here
  if(nrow(data.toplot) == 0) stop("Trying to plot an empty data.table in plotTemporal, something has gone wrong.  Perhaps you are selecting a site that isn't there?")
  
  
  
  ### Make a 'Time' column of data objects for the x-axis 
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

  ### FACETTING
 
  # all column names, used a lot below 
  all.columns <- names(data.toplot)
  
  # First check arguments
  if(!missing(col.by) && !is.null(col.by) && !col.by %in% all.columns) stop(paste("Colouring lines by", col.by, "requested, but that is not available, so failing."))
  if(!missing(type.by) && !is.null(type.by) && !type.by %in% all.columns) stop(paste("Setting line types by", type.by, "requested, but that is not available, so failing."))
  if(!missing(size.by) && !is.null(size.by) && !size.by %in% all.columns) stop(paste("Setting line sizes by", size.by, "requested, but that is not available, so failing."))
  
  # a first assume facetting by everything except for...
  dontFacet <-c ("Value", "Time", "Year", "Month", "Season", "Day", "Lon", "Lat", col.by, type.by, size.by)
  vars.facet <- all.columns[!all.columns %in% dontFacet]
  
  # then remove facets with only one unique value
  for(this.facet in vars.facet) {
    if(length(unique(data.toplot[[this.facet]])) == 1) vars.facet <- vars.facet[!vars.facet == this.facet]
  }
  
  ### LINE COLOURS
  
  # if cols is not specified and plots are to be coloured by Layers, look up line colours from Layer (currently still 'PFT') meta-data
  if(missing(cols) & col.by == "Layer"){
    all.layers <- unique(as.character(data.toplot[["Layer"]]))
    cols <- matchPFTCols(all.layers, PFTs)
  }
  # else colours will be determined by ggplot (or cols argument)
  
  ### LINE TYPES, SIZES & ALPHAS
  # Thus far either ignored or specified by the user
  
  ### LABELS
  # Can be specified by the user, otherwise sensibe defaults
  
  
  
  ### If requested, just return the data
  if(!plot) return(data.toplot)
  
  ### PLOT! - now make the plot
  p <- ggplot(as.data.frame(data.toplot), aes_string(x = "Time", y = "Value", colour = col.by, linetype = type.by, size = size.by, alpha = alpha.by))
  p <- p + geom_line(data = data.toplot)
  
  
  # line formatting
  if(!is.null(col.by) & !is.null(cols)) p <- p + scale_color_manual(values=cols, labels=col.labels) 
  if(!is.null(type.by) & !is.null(types)) p <- p + scale_linetype_manual(values=types, labels=type.labels)
  if(!is.null(size.by) & !is.null(sizes)) p <- p + scale_size_manual(values=sizes, labels=size.labels)
  if(!is.null(alpha.by) & !is.null(alphas)) p <- p + scale_alpha_manual(values=alphas, labels=alpha.labels)

  # labels and positioning
  p <- p + labs(title = title, subtitle = subtitle, y = y.label)
  p <- p + theme(legend.title=element_blank())
  p <- p + theme(legend.position = legend.position, legend.key.size = unit(2, 'lines'))
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5))
  
  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  # set limits
  if(!is.null(x.lim)) p <- p + xlim(x.lim)
  if(!is.null(y.lim)) p <- p + scale_y_continuous(limits = y.lim, name = y.label)
  p <- p + labs(y = y.axis.label)
  
  # facetting
  if(length(vars.facet > 0)){
    p <- p + facet_wrap(vars.facet, scales = facet.scales)
  }
  
  
  
  return(p)
  
  
}