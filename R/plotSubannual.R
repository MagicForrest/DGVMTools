
#' Plots sub-annual cycles
#' 
#' For a list of Fields, plots the sub-annual cycles of the selected layers on on top of each other.
#' 
#' @param fields The data to plot. Can be a Field or a list of Fields.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' @param subtitle A character string to override the default subtitle.
#' @param plotAverage Boolean, if TRUE plot the mean of all years
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param alpha A numeric (range 0-1), to give the transparency (alpha) of the annual lines
#' @param year.col.gradient A colour palette as a function to use to colour the annual lines according to their Year.  Only works for a single Quantity and a single Source.
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param ... Arguments passed to \code{ggplot2::facet_wrap()}.  See the ggplot2 documentation for full details but the following are particularly useful.
#' \itemize{
#'  \item{"nrow"}{The number of rows of facets}
#'  \item{"ncol"}{The number of columns of facets}
#'  \item{"scales"}{Whether the scales (ie. x and y ranges) should be fixed for all facets.  Options are "fixed" (same scales on all facets, default)
#'  "free" (all facets can their x and y ranges), "free_x" and "free_y"  (only x and y ranges can vary, respectively).}
#'  \item{"labeller"}{A function to define the labels for the facets.  This is a little tricky, please look to the ggplot2 documentation} 
#' }
#' 
#' @details 
#' 
#' Note that like all \code{DGVMTools} plotting functions, \code{plotSubannual} splits the data into separate panels using the \code{ggplot2::facet_wrap()}.  If you want to 'grid' the facets
#' using \code{ggplot2::facet_grid()} you can do so afterwards. 'gridding the facets' implies the each column and row of facets vary by one specific aspect.
#' For example you might have one column for each Source, and one row for each "Quantity".
#' 
#' 
#' 
#' @return Returns either a ggplot2 object or a data.table (depending on the 'plot' argument)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export



plotSubannual <- function(fields, # can be a Field or a list of Fields
                          layers = NULL,
                          title = NULL,
                          subtitle = NULL,
                          plotAverage = TRUE,
                          text.multiplier = NULL,
                          plot = TRUE,
                          year.col.gradient = NULL,
                          alpha = 0.2,
                          ...) {
  
  
  Quantity = Month = Source = Value = Year = NULL
  
  
  
  ### SANITISE FIELDS, LAYERS AND DIMENSIONS
  
  ## 1. FIELDS - check the input Field objects (and if it is a single Field put it into a one-item list)
  
  fields <- santiseFieldsForPlotting(fields)
  if(is.null(fields)) return(NULL)
  
  ## 2. LAYERS - check the number of layers
  
  layers <- santiseLayersForPlotting(fields, layers)
  if(is.null(layers)) return(NULL)
  
  
  ## 3. DIMENSIONS - check the dimensions (require that all fields the same dimensions) and then that one and only subannual dimension is present )
  
  # check the Fields for consistent dimensions
  dim.names <- santiseDimensionsForPlotting(fields)
  if(is.null(dim.names)) return(NULL)
  
  # get the subannual dimensions and check one and only one
  subannual.dimension <- dim.names[which(dim.names %in% c("Day", "Month", "Season")) ]
  if(length(subannual.dimension) == 0) stop("No subannual dims found for plotSubannual ")
  else if((length(subannual.dimension) > 1) ) stop(paste0("Multiple subannual dimensions found in plotSubannual: ", paste(subannual.dimension)))
  
  
  
  ### PREPARE AND CHECK DATA FOR PLOTTING
  final.fields <- trimFieldsForPlotting(fields, layers)
  
  ### MERGE DATA FOR PLOTTING INTO ONE BIG DATA.TABLE
  # MF TODO: Consider adding add.TimePeriod?
  data.toplot <- mergeFieldsForPlotting(final.fields, add.Quantity = TRUE, add.Site = FALSE, add.Region = FALSE)
  
  
  
  #### MAKE LIST OF QUANTITIES AND UNITS FOR Y LABEL
  
  unit.str <- list()
  quant.str <- list()
  id.str <- list()
  for(this.field in final.fields) {
    
    # pull out the unit string, id and full name string
    quant <- this.field@quant
    unit.str <- append(unit.str, quant@units)
    quant.str <- append(quant.str, quant@name)
    id.str <- append(id.str, quant@id)
    
  }
  
  # check the units
  if(length(unique(unit.str)) == 1){
    unit.str <- unique(unit.str)
  }
  else {
    unit.str <- paste(unique(unit.str), collapse = ", ")
    warning("Quants to be plotted have non-identical units in plotSeasonal().")
  }
  
  # check the strings
  if(length(unique(quant.str)) == 1){ quant.str <- unique(quant.str) }
  else{ quant.str <- paste(id.str, sep = ", ", collapse = ", ") }
  
  
  ##### YEAR COLOUR GRADIENT
  if(!missing(year.col.gradient) && !is.null(year.col.gradient)) {
    
    if(length(unique(data.toplot[["Quantity"]])) > 1) {
      warning("Since plotSeasonal is already trying to plot multiple quantities, and therefore multiple line colours, the year.col.gradient argument is being ignored ")
      year.col.gradient <- NULL
    }
    
    else if(is.logical(year.col.gradient)){
      
      if(year.col.gradient) year.col.gradient <- viridis::viridis
      else  year.col.gradient <- NULL
      
    }
    
  }
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(final.fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  # return the data if plot = FALSE
  if(!plot) return(data.toplot)
  
  
  ###### MAKE THE PLOT ######
  
  # basic plot
  if(is.null(year.col.gradient)) {
    p <- ggplot(as.data.frame(data.toplot), aes(get(subannual.dimension), Value, colour = Quantity, group = interaction(Year, Quantity)), alpha = alpha) + geom_line(alpha = alpha)
    # add average line if chosen
    if(plotAverage) {
      p <- p + stat_summary(aes(group=Quantity, color=paste("mean", Quantity)), fun.y=mean, geom="line", size = 1, linetype = "longdash")
    }
  }
  else {
    p <- ggplot(as.data.frame(data.toplot), aes(get(subannual.dimension), Value, colour = Year, group = interaction(Year, Quantity)), alpha = alpha) + geom_line(alpha = alpha)
    p <- p + scale_color_gradientn(colours = year.col.gradient(100))
    # add average line if chosen
    if(plotAverage) {
      p <- p + stat_summary(aes(group=Quantity, linetype = "mean year"), fun.y=mean, geom="line", size = 1)
      p <- p + scale_linetype_manual(values=c("mean year"="longdash"), name = element_blank())
    }
  }
  
  
  # set the x-axis
  if(subannual.dimension == "Month") p <- p + scale_x_continuous(breaks = 1:12,labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  
  # set the title
  p <- p + labs(title = title, subtitle = subtitle,  y = paste(quant.str, " (", unit.str, ")", sep = ""), x = subannual.dimension)
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5))
  
  # set legend 
  if(is.null(year.col.gradient)) p <- p + theme(legend.title=element_blank())
  p <- p + theme(legend.position = "right", legend.key.size = unit(2, 'lines'))
  
  # wrap to split by source
  p <- p + facet_wrap(~Source, ...)

  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  return(p)
  
}
