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
#' @param facet Character string. If specified, split the data (ie melt) the data by the column specified in the argument, and then split the plot into ribbons accordingly.  
#' @param facet.scales Character string.  If faceting (see above) use "fixed" to specify same scales on each ribbon (default), or "free"/"free_x"/"free_y" for tailored scales
#' @param legend.position Position of the legend, in the ggplot2 style.  Passed to the ggplot function \code{theme()}. Can be "none", "top", "bottom", "left" or "right" or two-element numeric vector
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
                         facet = NULL,
                         facet.scales = "fixed",
                         legend.position = "bottom"
){
  
  
  Year = value = variable = Lat = Lon = NULL
  
  # whether to to use the a grouping (ie plot many objects on one plot)
  group <- NULL  
  
  
  # Deal with class action and organise into a data.table for further manipulations and plotting
  
  # If we get a list it should be a list of ModelObjects and/or DataObjects.  Here we check that, and mangle it in to a data.table
  if(class(input.data) == "list") {
    
    # if a list of objects are supplied there are two possibilites:  facet the plot, one facet (ribbon) for each object or plot the same layers in one plot or   
 
    # POSSIBILITY 1 - put all on one plot
    if(is.null(facet)) {
      
      group <- "Source"
      
      plotting.data.dt <- data.table()
      PFTs <- list()
      for(x.object in input.data){
        
        if(!(is.DataObject(x.object) || is.ModelObject(x.object))) { stop("One of the elements in the list for the input.data arguments is not a DataObject or a  ModelObject") }
        temp.dt <- copy(x.object@data)
        if(is.ModelObject(x.object)) temp.dt[,"Source" := x.object@run@name]
        if(is.DataObject(x.object)) temp.dt[,"Source" := x.object@name]
        plotting.data.dt <- rbind(plotting.data.dt, copy(temp.dt), fill = TRUE)
        rm(temp.dt)
        
        # also make a superset of all the PFTs
        if(is.ModelObject(x.object)) PFTs <- append(PFTs, x.object@run@pft.set)
        
      }
      
    }
    
    # POSSIBILITY 2- facet per object
    else {
      
      plotting.data.dt <- data.table()
      PFTs <- list()
      for(x.object in input.data){
        
        if(!(is.DataObject(x.object) || is.ModelObject(x.object))) { stop("One of the elements in the list for the input.data arguments is not a DataObject or a  ModelObject") }
        temp.dt <- copy(x.object@data)
        if(is.ModelObject(x.object)) temp.dt[,"Source" := x.object@run@name]
        if(is.DataObject(x.object)) temp.dt[,"Source" := x.object@name]
        plotting.data.dt <- rbind(plotting.data.dt, copy(temp.dt), fill = TRUE)
        rm(temp.dt)
        
        # also make a superset of all the PFTs
        if(is.ModelObject(x.object)) PFTs <- append(PFTs, x.object@run@pft.set)
        
      }
      
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
  if("Year" %in% names(plotting.data.dt)) id.vars <- c("Year")
  else if("Month" %in% names(plotting.data.dt)) id.vars <- c("Month")
  else warning("plotTemporal: No suitable time axis found, aborting")  
  
  if(!is.null(facet)) id.vars <- append(id.vars, facet)
  if(!is.null(group)) id.vars <- append(id.vars, group)
  plotting.data.dt.melted <- melt(plotting.data.dt, id.vars = id.vars)
  
  # helpful check here
  if(nrow(plotting.data.dt.melted) == 0) stop("Trying to plot an empty data.table in plotTemporal, something has gone wrong.  Perhaps you are slecting a site that isn't there?")
  
  # Now that the data is melted into the final form, set the colours if not already specified and if enough meta-data is available
  if(is.null(cols) && is.null(types)){
    
    got.all <- TRUE
    new.cols <- c()
    new.types <- c()
    new.labels <- c()
    
    all.layers <- as.character(unique(plotting.data.dt.melted[["variable"]]))
    
    #### IF PFTs are present try to match layers to PFTs
    if(exists("PFTs")){
      
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
          type <- 3
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
    
    
    
  }
  
  
  # now make the plot
  if(is.null(group)) p <- ggplot(as.data.frame(plotting.data.dt.melted), aes_string(id.vars, "value", colour = "variable")) + geom_line(aes_string(linetype="variable"), size = 1)
  else p <- ggplot(as.data.frame(plotting.data.dt.melted), aes_string(id.vars, "value", group = "Source", colour = "Source")) + geom_line(aes_string(linetype="Source"), size = 1)
 
  
  #else p <- ggplot(as.data.frame(plotting.data.dt.melted), aes_string(id.vars, "value", group = "Source", colour = "Source")) + geom_line(size = 1)

  #p <- p + scale_x_continuous(breaks = unique(plotting.data.dt.melted[["Year"]])
  
  # line formatting
  if(!is.null(cols)) p <- p + scale_color_manual(values=cols, labels=labels) 
  if(!is.null(types)) p <- p + scale_linetype_manual(values=types, labels=labels)

  
  # labels and positioning
  p <- p + labs(title = title,  y = y.label)
  p <- p + theme(legend.title=element_blank())
  p <- p + theme(text = element_text(size=30))
  p <- p + theme(legend.position = legend.position, legend.key.size = unit(2, 'lines'))
  
  # set limits
  if(!is.null(x.lim)) p <- p + scale_x_continuous(limits = x.lim)
  if(!is.null(y.lim)) p <- p + scale_y_continuous(limits = y.lim)
  
  # if facet
  if(!is.null(facet)){
    p <- p + facet_wrap(stats::as.formula(paste("~", facet)), ncol = 1, scales = facet.scales)
  }
  
  
  
  return(p)
  
  
}