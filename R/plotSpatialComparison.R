#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################


#' Plot maps from a temporally-averaged \code{Field}, \code{DataObject} or \code{ComaprisonLayer} (and lists thereof)
#' 
#' This is a heavy lifting function for plotting maps from Fields, DataObjects, and ComparisonLayers (and lists of those things) with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  In principle you can supply only the objext and it will plot.
#' It is basically a complex wrapper for the ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param sources The data to plot, must be a ComparisonLayer or a list of ComparisonLayers
#' @param type A character specifying what type of plot to make. Can be "difference" (default, for a difference plot), "percentage.difference", "values" 
#' (actual values, side-by-side) or "nme" (for the Normalised Mean Error, not yet implemented)
#' @param title A character string to override the default title.
#' Note that using these, especially "worldHires", can add quite a bit off time. 
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param override.cols A colour palette function to override the defaults.
#' @param symmetric.scale If plotting a differences, make the scale symmetric around zero (default is TRUE)
#' @param percentage.difference.limit If precentage difference to be plotted, what to limit the scale to.
#' @param ... Parameters passed to \code{plotSpatial()}
#' 
#' @details  A wrapper for around \code{plotSpatial()} to plot the spatial ComparisonLayers as maps.  
#' 
#' @return Returns a ggplot object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' @importFrom maptools map2SpatialLines
#' @importFrom rgeos gLength
#' @importFrom sp SpatialLinesDataFrame
#' @importFrom raster crs
#' 
#' @export 
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotSpatialComparison <- function(sources, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a Field
                                  type = c("difference", "percentage.difference", "values", "nme"),
                                  title = NULL,
                                  limits = NULL,
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
  if(is.ComparisonLayer(sources)) {
    sources<- list(sources)
  }
  else if(class(sources)[1] == "list") {
    for(object in sources){ 
      if(!is.ComparisonLayer(object)) {
        warning("You have passed me a list of items to plot but the items are not exclusively of ComparisonLayers.  Returning NULL")
        return(NULL)
      }
    }
  }
  else{
    stop(paste("plotSpatialComparison can only handle single a ComparisonLayer, or a list of ComparisonLayers can't plot an object of type", class(sources)[1], sep = " "))
  }
  
  
  ### 2. LAYERS - the layers to plot are defined by the plot type
  
  
  #### DIFFERENCE OR PERCENTAGE DIFFERENCE
  if(type == "difference" || type == "percentage.difference") {
    
    # convert the ComparisonLayers into a Fields  for plotting 
    objects.to.plot <- list()
    max.for.scale <- 0
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
                       quant = object@quant,
                       first.year = 0,
                       last.year = 0,
                       year.aggregate.method = "NULL",
                       spatial.extent = object@spatial.extent,
                       spatial.extent.id = object@spatial.extent.id,
                       spatial.aggregate.method = object@spatial.aggregate.method,
                       subannual.aggregate.method = object@ subannual.aggregate.method,
                       subannual.original = object@subannual.original,
                       source = object@info1)
      
      objects.to.plot[[length(objects.to.plot)+1]] <- new.field
      
      # get max value for making the scale symmetric
      if(symmetric.scale && missing(limits)) {
        max.for.scale <- max(max.for.scale, max(abs(object@data[[layer.to.plot]])))
      }
      
    }
    
    # set the colours
    if(missing(override.cols)) override.cols <-  rev(RColorBrewer::brewer.pal(11, "RdBu"))
    
    # set a symmetric scale (so zero always white/centre colour)
    if(symmetric.scale) limits <- c(-max.for.scale, max.for.scale)
    
    # make an appropriate title if not provided
    #if(missing(title)) title <- paste()
    
    return(plotSpatial(objects.to.plot,
                        layers = layer.to.plot,
                        override.cols = override.cols,
                        limits = limits,
                        title = title,
                        ...))
    
    
  }
  
  ### VALUES 
  else if(type == "values") {
    
    # convert the ComparisonLayers into a Fields  for plotting 
    objects.to.plot <- list()
    
    for(object in sources){ 
   
      # FIRST INFO
      new.dt <- object@data[, append(getSTInfo(object), names(object)[1]), with=FALSE]
      setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant@id )
      objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
                                                          id = object@id,
                                                          data = new.dt,
                                                          quant = object@quant,
                                                          first.year = 0,
                                                          last.year = 0,
                                                          year.aggregate.method = "NULL",
                                                          spatial.extent = object@spatial.extent,
                                                          spatial.extent.id = object@spatial.extent.id,
                                                          spatial.aggregate.method = object@spatial.aggregate.method,
                                                          subannual.aggregate.method = object@ subannual.aggregate.method,
                                                          subannual.original = object@subannual.original,
                                                          source = object@info1)
      
      # SECOND INFO
      new.dt <- object@data[, append(getSTInfo(object), names(object)[2]), with=FALSE]
      setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant@id )
      objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
                                                          id = object@id,
                                                          data = new.dt,
                                                          quant = object@quant,
                                                          first.year = 0,
                                                          last.year = 0,
                                                          year.aggregate.method = "NULL",
                                                          spatial.extent = object@spatial.extent,
                                                          spatial.extent.id = object@spatial.extent.id,
                                                          spatial.aggregate.method = object@spatial.aggregate.method,
                                                          subannual.aggregate.method = object@ subannual.aggregate.method,
                                                          subannual.original = object@subannual.original,
                                                          source = object@info2)
      
    }
    
    
    # set the colours
    #if(missing(override.cols)) override.cols <-  rev(RColorBrewer::brewer.pal(11, "RdBu"))

    # make an appropriate title if not provided
    #if(missing(title)) title <- paste()

   
    return(plotSpatial(objects.to.plot,
                        layers =  object@quant@id,
                        override.cols = override.cols,
                        limits = limits,
                        title = title,
                        ...))

    
    
  }
  
  
  
}