#' Plot temporal data
#' 
#' Makes a line plot graphing the temporal evolution of data (using ggplot2).  Full functionality not implemented, or even defined...  
#'
#' @param input.data The data to be plotted, either as a ModelObject, DataObject or a data.table.  
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param expand.layers A boolean, determines wether to expand the layers arguement.  See documentation for \code{expandLayers} for details.
#' @param title Main plot title (character string)
#' @param quant A Quantity object to provide meta-data about how to make this plot
#' @param cols,types Colour and types for the lines.  They do not each necessarily need to be specified, but if they are then the they need to be 
#' the same length as the labels arguments
#' @param labels A list of character strings which are used as the labels fro the lines.  Must have the same length as the layers argument (after expansion if necessary)
#' @param x.label,y.label Character strings for the x and y axes (optional)
#' @param x.lim,y.lim Limits for the x and y axes (each a two-element numeric, optional)
#' @param wrap Character string. If specified, split the data (ie melt) the data by the column specified in the argument, and then split the plot into ribbons accordingly.
#' @param wrap.scales Character string.  If wrapping (see above) use "fixed" to specify same scales on each ribbon (default), or "free"/"free_x"/"free_y" for tailored scales
#' on both scales/x only/y only on each ribbon 
#' 
#' @details
#' This function is WORK IN PROGRESS!!  For questions about functionality or feature requests contact the author
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @internal
#' @import ggplot2
#' @return A ggplot
#'
plotTemporal <- function(input.data, 
                         layers = NULL,
                         expand.layers = TRUE,
                         title = NULL,
                         quant = NULL,
                         cols = NULL,
                         types = NULL,
                         labels = NULL,
                         y.label = NULL,
                         y.lim = NULL,
                         x.label = NULL,
                         x.lim = NULL,
                         wrap = NULL,
                         wrap.scales = "fixed"
                         ){
  
  
  Year = value = variable = NULL
  
  # Deal with class action
  
  
  # Check for Lon and Lat
  
  
  # Select the layers 
  if(!is.null(layers)) {
    
    # select the layers you want
    
  }
  
  # make title
  if(is.null(title)) {
    title <- ""
  }
  
  # make y label
  if(is.null(y.label)) {
    title <- ""
  }
  
  
  
  # melt the data table so that all remaining layers become entries in a column (instead of column names)
  id.vars <- c("Year")
  if(!is.null(wrap)) id.vars <- append(id.vars, wrap)
  input.data.melted <- melt(input.data, id.vars = id.vars)
  
  
  
  # now make the plot
  p <- ggplot(as.data.frame(input.data.melted), aes(Year, value, colour = variable)) + geom_line(aes(linetype=variable), size = 1)
  #p <- p + scale_x_continuous(breaks = unique(input.data.melted[["Year"]])
  
  # line formatting
  if(!is.null(cols)) p <- p + scale_color_manual(values=cols, labels=labels) 
  if(!is.null(types)) p <- p + scale_linetype_manual(values=types, labels=labels)
  
  # labels and positioning
  p <- p + labs(title = title,  y = y.label)
  p <- p + theme(legend.title=element_blank())
  p <- p + theme(text = element_text(size=30))
  p <- p +  theme(legend.position = "bottom", legend.key.size = unit(2, 'lines'))
  
  # set limits
  if(!is.null(x.lim)) p <- p + scale_x_continuous(limits = x.lim)
  if(!is.null(y.lim)) p <- p + scale_y_continuous(limits = y.lim)
  
  # if wrap
  if(!is.null(wrap)){
    p <- p + facet_wrap(as.formula(paste("~", wrap)), ncol = 1, scales = wrap.scales)
  }

  
  
    return(p)
    
  
}