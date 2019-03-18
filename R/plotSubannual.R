
#' Plots sub-annual cycles
#' 
#' For a list of Runs and a list of Quantities, plots the sub-annual cycles.  Note this function actually reads the data, 
#' so might not be too efficient unless the arguments which are passes through to getField() via the "..." argument are optimised.  
#' For example, by using "store.full = TRUE" and the combination "write = TRUE" and "read.full = FALSE".
#' 
#' @param fields The data to plot. Can be a Field or a list of Fields.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' @param subtitle A character string to override the default subtitle.
#' @param plotAverage Boolean, if TRUE plot the mean of all years
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param facet.scales Character string.  If faceting (see above) use "fixed" to specify same scales on each ribbon (default), or "free"/"free_x"/"free_y" for tailored scales
#' @param alpha A numeric (range 0-1), to give the transparency (alpha) of the annual lines
#' @param year.col.gradient A colour palette as a function to use to colour the annual lines according to their Year.  Only works for a single Quantity and a single Source.
#' @param ... Arguments passed to getField().  Of particular relevance are \code{spatial.extent} and \code{spatial.aggregate.method} (to determine over 
#' which spatial extent to plot the seasonal cycle and, if that extent includes more that one gridcell, how to aggregate across that extent)
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
                         facet.scales = "fixed",
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
  
  # melt and combine the final.fields, adding Source and Quantity columns for facetting 
  data.toplot.list <- list()
  for(this.field in final.fields) {
 
    this.field.melted <- melt(this.field@data, id.vars = getDimInfo(this.field), variable.factor = FALSE)
    this.field.melted[, Source := this.field@source@name]
    this.field.melted[, Quantity := this.field@quant@name]

    data.toplot.list[[length(data.toplot.list)+1]] <- this.field.melted
    rm(this.field.melted)
  }
  data.toplot <- rbindlist(data.toplot.list)
  rm(data.toplot.list)
  
  setnames(data.toplot, "variable", "Layer")
  setnames(data.toplot, "value", "Value")

  #  
  
  
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
  p <- p + facet_wrap(~Source, ncol = 1, scales = facet.scales)
  
  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  return(p)
  
}
