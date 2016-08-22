#' cheese
#' 
#' biscuits
#'
#'
#' @param input.data The data to be plotted, either as a ModelObject, DataObject or a data.table.  
#' If it is a data.table (this is the way to compare runs or runs and data) then it must have been "pre-melted" 
#' so that the run/data identified are values in a column (name can specified using the "by" argument). 
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