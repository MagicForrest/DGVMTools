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
#' @param labels A list of character strings which are used as the labels for the lines.  Must have the same length as the layers argument (after expansion if necessary)
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
  
  
  Year = value = variable = Lat = Lon = NULL
  
  # Deal with class action and organise into a data.table for further manipulations and plotting

  # If we get a list it should be a list of ModelObjects and/or DataObjects.  Here we check that, and mangle it in to a data.table
  if(class(input.data) == "list") {
    
    wrap <- "Run"

    plotting.data.dt <- data.table()
    PFTs <- list()
    for(x.object in input.data){
  
      if(!(is.DataObject(x.object) || is.ModelObject(x.object))) { stop("One of the elements in the list for the input.data arguments is not a DataObject or a  ModelObject") }
      temp.dt <- copy(x.object@data)
      temp.dt[,"Run" := x.object@run@description]
      plotting.data.dt <- rbind(plotting.data.dt, copy(temp.dt))
      rm(temp.dt)

      # also make a superset of all the PFTs
      if(is.ModelObject(x.object)) PFTs <- append(PFTs, x.object@run@pft.set)
      
    }
 
  }
  # if it is a single DataObject or ModelObject, pull out the data (and PFTs if present)
  else if(is.DataObject(input.data) || is.ModelObject(input.data)){
    plotting.data.dt <- input.data@data
    if(is.ModelObject(x.object)) PFTs <- x.object@run@pft.set
  }
  # if it is a data.table then we just use that straight
  else if(is.data.table(input.data)) {
    plotting.data.dt <- copy(input.data)
  }
  # if it is a data.frame convert it to a data.table
  else if(is.data.frame(input.data)) {
    plotting.data.dt <- as.data.frame(input.data)
  }
  # else fail
  else{
    stop(paste("Don't know how to make temporal plot for object of class", class(input.data), sep = " "))
  }
  
   
  
  # Check for Lon and Lat (and remove 'em)
  if("Lon" %in% names(plotting.data.dt)) plotting.data.dt[, Lon := NULL]
  if("Lat" %in% names(plotting.data.dt)) plotting.data.dt[, Lat := NULL]
  
  
 
  
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
    y.label <- ""
  }
  
  
  
  # melt the data table so that all remaining layers become entries in a column (instead of column names)
  id.vars <- c("Year")
  if(!is.null(wrap)) id.vars <- append(id.vars, wrap)
  plotting.data.dt.melted <- melt(plotting.data.dt, id.vars = id.vars)
  
  # Now that the data is melted into the final form, set the colours if not already specified and if enough meta-data is available
  if(is.null(cols) && is.null(types)){
    
    got.all <- TRUE
    new.cols <- c()
    new.types <- c()
    new.labels <- c()
    
    all.layers <- as.character(unique(plotting.data.dt.melted[["variable"]]))
    
    for(layer in all.layers) {
      
      colour <- NULL
      type <- NULL

      # check if it is a PFT and use that colour      
      for(PFT in PFTs){
          if(layer == PFT@id) { 
            colour <- PFT@colour
            if(PFT@combine != "no") type <- 2
            else type <- 1
            label <- PFT@id
          }
      }
      
      # now check for specific aggregated layers
      if(layer == "Woody") {
        colour <- "brown"
        label <- "Woody"
        type <- 1
      }
      
      
      if(!is.null(colour)) {
        new.cols <- append(new.cols, colour)
        new.types <- append(new.types, type)
        new.labels <- append(new.labels, labels)
      }
      else got.all <- FALSE
      
    }
    
   if(got.all) {
     cols <- new.cols
     types <- new.types
     if(is.null(labels)) labels <- all.layers
     names(cols) <- all.layers
     names(types) <- all.layers
     names(labels) <- all.layers
   }
    
  
    
  }
  
 
  # now make the plot
  p <- ggplot(as.data.frame(plotting.data.dt.melted), aes(Year, value, colour = variable)) + geom_line(aes(linetype=variable), size = 1)
  #p <- p + scale_x_continuous(breaks = unique(plotting.data.dt.melted[["Year"]])
  
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