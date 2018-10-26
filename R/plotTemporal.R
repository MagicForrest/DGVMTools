#' Plot temporal data
#' 
#' Makes a line plot graphing the temporal evolution of data (using ggplot2).  Full functionality not implemented, or even defined...  
#'
#' @param input.data The data to be plotted, either as a Field, DataObject or a list of Model/DataObjects.  
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param expand.layers A boolean, determines wether to expand the layers arguement.  See documentation for \code{expandLayers} for details.
#' @param title A character string to override the default title.  Set to NULL for no title.
#' @param subtitle A character string to override the default subtitle. Set to NULL for no subtitle.
#' 
#' @param quant A Quantity object to provide meta-data about how to make this plot
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
plotTemporal <- function(input.data, 
                         layers = NULL,
                         expand.layers = TRUE,
                         title = character(0),
                         subtitle = character(0),
                         quant = NULL,
                         cols = NULL,
                         types = NULL,
                         labels = NULL,
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
  
  
  Time = Year = Month = Day = Source = value = variable = Lat = Lon = NULL
  
  # whether to to use the a grouping (ie plot many objects on one plot)
  single.object <- FALSE
  
  # Deal with class action and organise into a data.table for further manipulations and plotting
  # if it is a single Field, pull out the data (and PFTs if present)
  if(is.Field(input.data)){
    
    if(!is.null(layers)) input.data <- selectLayers(input.data, layers)
    plotting.data.dt <- input.data@data
    plotting.data.dt[,"Source" := input.data@source@name]
    if(is.Field(input.data)) PFTs <- input.data@source@pft.set
    if(is.null(quant)) quant <- input.data@quant
    single.object <- TRUE
    if(!missing(facet) & facet) {
      warning("Only got one Source object so ignoring your facet = TRUE argument")
    }
    facet <- FALSE
    
    # check temporal dimensions
    stinfo.names <- getDimInfo(input.data)
    if(!"Year" %in% stinfo.names && !"Month" %in% stinfo.names && !"Day" %in% stinfo.names) {
      warning("Neither Year nor Month nor Day found in input Field for which a time serie is to be plotted.  Obviously this won't work, returning NULL.")
      return(NULL)
    }
    if("Lon" %in% stinfo.names || "Lat" %in% stinfo.names) {
      warning("Either Lon or Lat found in input Field for which a time series is to be plotted, indicating that this is not only time series data. Therefore returning NULL.")
      return(NULL)
    }
    
    
  }
  
  
  # If we get a list it should be a list of Fields.  Here we check that, and mangle it in to a data.table
  else if(class(input.data) == "list") {
    
    plotting.data.dt <- data.table()
    PFTs <- list()
    for(x.object in input.data){
      
      if(!(is.Field(x.object))) { stop("One of the elements in the list for the input.data arguments is not a Field") }
      temp.dt <- copy(x.object@data)
      temp.dt[,"Source" := x.object@source@name]
      plotting.data.dt <- rbind(plotting.data.dt, copy(temp.dt), fill = TRUE)
      rm(temp.dt)
      
      # also make a superset of all the PFTs
      if(is.Field(x.object)) PFTs <- append(PFTs, x.object@source@pft.set)
      
      # check temporal dimensions
      stinfo.names <- getDimInfo(x.object)
      if(!"Year" %in% stinfo.names && !"Month" %in% stinfo.names && !"Day" %in% stinfo.names) {
        warning("Neither Year nor Month nor Day found in input Field for which a time serie is to be plotted.  Obviously this won't work, returning NULL.")
        return(NULL)
      }
      if("Lon" %in% stinfo.names || "Lat" %in% stinfo.names) {
        warning("Either Lon or Lat found in input Field for which a time series is to be plotted, indicating that this is not only time series data. Therefore returning NULL.")
        return(NULL)
      }
      
    }
    
    # assume Quantity is the same for each facet
    if(is.null(quant)) quant <- input.data[[1]]@quant
    
  }
  
  # else fail
  else{
    stop(paste("Don't know how to make temporal plot for object of class", class(input.data), sep = " "))
  }
  
  
  # Check for Lon and Lat (and remove 'em)
  if("Lon" %in% names(plotting.data.dt)) { plotting.data.dt[, Lon := NULL] }
  if("Lat" %in% names(plotting.data.dt)) { plotting.data.dt[, Lat := NULL] }
  
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(input.data)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  
  
  # make y label
  if(is.null(y.label)) {
    y.label <- element_blank()
    if(!is.null(quant)) y.label  <- paste0(quant@name, " (", quant@units, ")")
  }
  
  # melt the data table so that all remaining layers become entries in a column (instead of column names)
  id.vars <- c()
  for(dim in c("Year", "Season", "Month", "Day")) {
    if(dim %in% names(plotting.data.dt)) id.vars <- append(id.vars, dim)
  }
  if(length(dim) == 0) warning("plotTemporal: No suitable time axis found, aborting")  
  if("Source" %in% names(plotting.data.dt)) id.vars <- append(id.vars, "Source")
  plotting.data.dt.melted <- melt(plotting.data.dt, id.vars = id.vars)
  setnames(plotting.data.dt.melted, "variable", "Layer")
  setnames(plotting.data.dt.melted, "value", "Value")
  
  # helpful check here
  if(nrow(plotting.data.dt.melted) == 0) stop("Trying to plot an empty data.table in plotTemporal, something has gone wrong.  Perhaps you are selecting a site that isn't there?")
  
  
  # Now that the data is melted into the final form, set the colours if not already specified and if enough meta-data is available
  all.layers <- unique(as.character(plotting.data.dt.melted[["Layer"]]))
  labels <- all.layers
  names(labels) <- all.layers
  
  if(is.null(cols)){
    new.cols <- matchPFTCols(all.layers, PFTs)
    if(length(new.cols) == length(all.layers))  cols <- new.cols 
  }
  
  if(is.null(types)) {
    new.types <- list()
    for(layer in all.layers) {
      for(PFT in PFTs){
        if(layer == PFT@id) { 
          if(PFT@shade.tolerance != "no" &&  tolower(PFT@shade.tolerance) != "none") new.types[[layer]] <- 2
          else new.types[[layer]] <- 1
        }
      }
    }
    if(length(new.types) == length(all.layers)) {
      types <- unlist(new.types)
    }
  }
  
  
  
  ### Make a 'Time' column of data objects for the x-axis 
  earliest.year <- min(plotting.data.dt.melted[["Year"]])
  if(earliest.year >= 0) {
    # convert years and months to dates 
    if("Year" %in% names(plotting.data.dt.melted) && "Month" %in% names(plotting.data.dt.melted)) {
      pad <- function(x) { ifelse(x < 10, paste0(0,x), paste0(x)) }
      plotting.data.dt.melted[, Time := as.Date(paste0(Year, "-", pad(Month), "-01"), format = "%Y-%m-%d")]
      plotting.data.dt.melted[, Year := NULL]
      plotting.data.dt.melted[, Month := NULL]
    }
    # convert years and days to dates 
    else if("Year" %in% names(plotting.data.dt.melted) && "Day" %in% names(plotting.data.dt.melted)) {
      pad <- function(x) { ifelse(x < 10, paste0(0,x), paste0(x)) }
      plotting.data.dt.melted[, Time := as.Date(paste0(Year, "-", Day), format = "%Y-%j")]
      plotting.data.dt.melted[, Year := NULL]
      plotting.data.dt.melted[, Day := NULL]
    }
    # convert years to dates 
    else if("Year" %in% names(plotting.data.dt.melted)) {
      plotting.data.dt.melted[, Time := as.Date(paste0(Year, "-01-01"), format = "%Y-%m-%d")]
      plotting.data.dt.melted[, Year := NULL]
    }
  }
  else {
    if("Year" %in% names(plotting.data.dt.melted) && "Month" %in% names(plotting.data.dt.melted)) {
      latest.year <- max(plotting.data.dt.melted[["Year"]])
      print(latest.year)
      print(earliest.year)
      earliest.year.days <- as.numeric(earliest.year, as.Date(("0001-01-01")))
      latest.year.days <- as.numeric(latest.year, as.Date(("0001-01-01")))
      print(earliest.year.days)
      print(latest.year.days)
      stop("Hmm... not yet sure how to plot months with negative years")
    }
    else if("Year" %in% names(plotting.data.dt.melted)) {
      plotting.data.dt.melted[, Time := Year]
      plotting.data.dt.melted[, Year := NULL]
    }
    #
  }
  
  
  ### If requested, just return the data
  if(!plot) return(plotting.data.dt.melted)
  
  # now make the plot
  p <- ggplot(as.data.frame(plotting.data.dt.melted), aes_string(x = "Time", y = "Value", colour = "Layer"))
  for(this.source in unique(plotting.data.dt.melted[["Source"]])) {
    p <- p + geom_line(data = plotting.data.dt.melted[Source == this.source,], size = 1)
    #p <- p + geom_line(data = plotting.data.dt.melted[Source == this.source,], size = 1)
  }
  
  # line formatting
  if(!is.null(cols)) p <- p + scale_color_manual(values=cols, labels=labels) 
  if(!is.null(types)) p <- p + scale_linetype_manual(values=types, labels=labels)
  
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
  p <- p + labs(y = y.label)
  
  # if facet
  if(facet){
    p <- p + facet_wrap(stats::as.formula(paste("~Source")), ncol = 1, scales = facet.scales)
  }
  
  
  
  return(p)
  
  
}