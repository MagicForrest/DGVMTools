#!/usr/bin/Rscript


#################################################################################################################################################
################################################## PLOT COMPARISON MAPS #########################################################################
#################################################################################################################################################


#' Plot a comparion between two layers
#' 
#' This is a heavy lifting function for plotting maps from Fields, DataObjects, and Comparisons (and lists of those things) with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  In principle you can supply only the objext and it will plot.
#' It is basically a complex wrapper for the ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param sources The data to plot, must be a Comparison or a list of Comparisons
#' @param type A character specifying what type of plot to make. Can be "difference" (default, for a difference plot), "percentage.difference", "values" 
#' (actual values, side-by-side) or "nme" (for the Normalised Mean Error, not yet implemented)
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param panel.bg.col Colour string for the panel background, default to "white" for absolute values plots, and a sort of blue grey for difference plots.
#' @param override.cols A colour palette function to override the defaults.
#' @param symmetric.scale If plotting a differences, make the scale symmetric around zero (default is TRUE)
#' @param percentage.difference.limit If precentage difference to be plotted, what to limit the scale to.
#' @param ... Parameters passed to \code{plotSpatial()}
#' 
#' @details  A wrapper for around \code{plotSpatial()} to plot the spatial Comparisons as maps.  
#' 
#' @return Returns a ggplot object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' 
#' @export 
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotSpatialComparison <- function(sources, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a Field
                                  type = c("difference", "percentage.difference", "values", "nme"),
                                  limits = NULL,
                                  panel.bg.col = "white",
                                  override.cols = NULL,
                                  symmetric.scale = TRUE,
                                  percentage.difference.limit = 300,
                                  ...){
  
  Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = NULL
  Difference = Percentage.Difference = NULL
  
  # sort type argument
  type <- match.arg(type)
  
  
  if(!missing(limits)) symmetric.scale <- FALSE

  ### CHECK TO SEE EXACTLY WHAT WE SHOULD PLOT
  
  ### 1. SOURCES - check the sources
  if(is.Comparison(sources)) {
    sources<- list(sources)
  }
  else if(class(sources)[1] == "list") {
    for(object in sources){ 
      if(!is.Comparison(object)) {
        warning("You have passed me a list of items to plot but the items are not exclusively of Comparisons.  Returning NULL")
        return(NULL)
      }
    }
  }
  else{
    stop(paste("plotStatistics can only handle single a Comparison, or a list of Comparisons can't plot an object of type", class(sources)[1], sep = " "))
  }
  
  
  ### 2. LAYERS - the layers to plot are defined by the plot type
  
  
  #### DIFFERENCE OR PERCENTAGE DIFFERENCE
  if(type == "difference" || type == "percentage.difference") {
    
    # convert the Comparisons into a Fields  for plotting 
    objects.to.plot <- list()
    max.for.scale <- 0
    final.layers.to.plot <- c()
    for(object in sources){ 
      
      if(type == "difference") {
        layer.to.plot <- "Difference"
      }
      else {
        layer.to.plot <- "Percentage.Difference"
        temp.dt <- object@data
        layers.names <- names(object)
        temp.dt[ , Percentage.Difference := Difference %/0% get(layers.names[2]) * 100 ]
        object@data <- temp.dt
      }
      new.object <- selectLayers(object, layer.to.plot)
      
      new.field <- new("Field",
                       id = object@id,
                       data = new.object@data,
                       quant = object@quant1,
                       source = object@source1,
                       commonSTAInfo(list(object@sta.info1, object@sta.info2)))
      
      #new.field@source@name <- paste0("\u0394(", object@source1@name, " - ", object@source2@name, ")")
      new.field@source@name <- object@name
      new.field@source@id <- paste(object@source1@id, object@source2@id, sep ="-")
      
      # if comparing only one layer with the same name in both sources then append the layer to the 'difference' layer name to make a more descriptive layer name and hence plot title
      # eg if comparing two different between two layers original called totally, then call the new layer "Difference Total" 
      if(length(object@layers1) == 1 && length(object@layers2) == 1 && object@layers1 == object@layers2) {
        new.name <- paste(layer.to.plot, object@layers1, sep = " ")
        setnames(new.field@data, layer.to.plot, new.name)
        final.layers.to.plot <- append(final.layers.to.plot, new.name)
      }
      else{
        final.layers.to.plot <- append(final.layers.to.plot, layer.to.plot)
      }
      
      objects.to.plot[[length(objects.to.plot)+1]] <- new.field
      
      # get max value for making the scale symmetric
      if(symmetric.scale && missing(limits)) {
        max.for.scale <- max(max.for.scale, max(abs(object@data[[layer.to.plot]])))
      }
      
    }
    
    # get the unique layer names (since their might be duplicates)
    final.layers.to.plot <- unique(final.layers.to.plot)
    
    # set the colours
    if(missing(override.cols)) override.cols <-  rev(RColorBrewer::brewer.pal(11, "RdBu"))
    
    # set a symmetric scale (so zero always white/centre colour)
    if(symmetric.scale) limits <- c(-max.for.scale, max.for.scale)
    
    # if no panel background panel colour specified, use a non-white one
    if(missing(panel.bg.col)) panel.bg.col = "#809DB8"
     
    the.plot <- plotSpatial(objects.to.plot,
                            layers = final.layers.to.plot,
                            cols = override.cols,
                            limits = limits,
                            panel.bg.col = panel.bg.col,
                            ...)
    
    
    
    if(is.logical(objects.to.plot[[1]]@data[[layer.to.plot]])) the.plot <- the.plot + scale_fill_discrete(name = "Agreement")
    return(the.plot)
    
  }
  
  ### VALUES 
  else if(type == "values") {
    
    # convert the Comparisons into a Fields  for plotting 
    objects.to.plot <- list()
    layers.to.plot <- c()
    for(object in sources){ 
      
      # SECOND INFO - putting this first because this is the 'base' dataset ("one minus two" convention)
      new.dt <- object@data[, append(getDimInfo(object), names(object)[2]), with=FALSE]
      #setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant2@id )
      setnames(new.dt, names(new.dt)[length(names(new.dt))], object@layers2)
      layers.to.plot <- append(layers.to.plot, object@layers2)
      objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
                                                          id = object@id,
                                                          data = new.dt,
                                                          quant = object@quant2,
                                                          source = object@source2,
                                                          object@sta.info2)
      
      # FIRST INFO
      new.dt <- object@data[, append(getDimInfo(object), names(object)[1]), with=FALSE]
      #setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant1@id )
      setnames(new.dt, names(new.dt)[length(names(new.dt))], object@layers2 )
      layers.to.plot <- append(layers.to.plot, object@layers2)
      objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
                                                          id = object@id,
                                                          data = new.dt,
                                                          quant = object@quant1,
                                                          source = object@source1,
                                                          object@sta.info1)
      
    }
    
    return(plotSpatial(objects.to.plot,
                       layers =  unique(layers.to.plot),
                       cols = override.cols,
                       limits = limits,
                       ...))
    
    
    
  }
  
  
  
}