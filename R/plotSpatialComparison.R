#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################


#' Plot maps from a temporally-averaged \code{ModelObject}, \code{DataObject} or \code{ComaprisonLayer} (and lists thereof)
#' 
#' This is a heavy lifting function for plotting maps from ModelObjects, DataObjects, and ComparisonLayers (and lists of those things) with flexibility, but also with a high degree of automation. 
#' As a consequence, it has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults.  In principle you can supply only the objext and it will plot.
#' It is basically a complex wrapper for the ggplot2 function geom_raster() and it returns a ggplot object, which will need to be displayed using a \code{print()} command.  Note that this object can be firther modified 
#' using further ggplot2 commands. 
#'
#' @param sources The data to plot. Can be a ModelObject, a DataObject, or a list of including both
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' Note that using these, especially "worldHires", can add quite a bit off time. 
#' @param facet.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param facet.order A vector of the characters that, if supplied, control the order of the facets.  To see what these values are you can call this funtion with "plot=FALSE"
#' and check the values of the XXXX column.  But generally they will be the values of the @names slots of the Data/ModelObjects and/or the layers (as layers plotted as defined by the layers arguments 
#' in this function). 
#' @param plot.bg.col Colour string for the plot background.  "white"
#' @param useLongNames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. NOT CURRENTLY IMPLEMENTED!!
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param ylim An optional vector of two numerics to specify the y/latitude range of the plot.
#' @param xlim An optional vector of two numerics to specify the x/longitude range of the plot.
#' @param years An optional numeric vector specifying which years to plot  
#' @param days An optional numeric vector specifying which days to plot
#' @param months An optional numeric vector specifying which months to plot
#' @param seasons An optional character vector specifying which seasons to plot (any or all of "DJF", "MAM, "JJA", "SON")  
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param override.cols A colour palette function to override the defaults.
#' @param override.cuts Cut ranges (a numeric vector) to override the default colour delimitation
#' @param discretise Boolen, if true, but the discretise the data according the override.cuts argument above
#' @param grid Boolean, if TRUE then don't use facet_grid() to order the panels in a grid.  Instead use facet_wrap().  
#' Useful when not all combinations of Sources x Layers exist which would leave blank panels.
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param map.overlay A character string specifying which map overlay (from the maps and mapdata packages) should be overlain.  
#' @param interior.lines Boolean, if TRUE plot country lines with the continent outlines of the the requested map.overlay
#' Other things can be overlain on the resulting plot with further ggplot2 commands.
#' @param symmetric.scale If plotting a differences, make the scale symmetric around zero (default is TRUE)
#' 
#' @details  This function is heavily used by the benchmarking functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' However, the \code{plotGGSpatial} function makes pretty plots with a simpler syntax, but with less flexibility.
#' 
#' The function works best for \code{ModelObjects} (which contain a lot of useful metadata).   
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

plotSpatialComparison <- function(sources, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a ModelObject
                                  type = c("difference", "percentage.difference", "values", "nme"),
                                  title = NULL,
                                  limits = NULL,
                                  override.cols = NULL,
                                  symmetric.scale = TRUE,
                                  percentage.difference.limit = 300,
                                  ...){
  
  Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = NULL
  
  
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
    
    # convert the ComparisonLayers into a ModelObjects  for plotting 
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
      
      new.field <- new("ModelObject",
                       id = object@id,
                       data = new.object@data,
                       quant = object@quant,
                       spatial.extent = object@spatial.extent,
                       spatial.extent.id = object@spatial.extent.id,
                       temporal.extent = object@temporal.extent,
                       temporal.extent.id = object@temporal.extent.id,
                       spatial.aggregate.method = object@spatial.aggregate.method,
                       temporal.aggregate.method = object@temporal.aggregate.method,
                       subannual.aggregate.method = object@ subannual.aggregate.method,
                       subannual.original = object@subannual.original,
                       run = object@info1)
      
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
    
    return(plotSpatial2(objects.to.plot,
                        layers = layer.to.plot,
                        override.cols = override.cols,
                        limits = limits,
                        title = title,
                        ...))
    
    
  }
  
  ### VALUES 
  else if(type == "values") {
    
    # convert the ComparisonLayers into a ModelObjects  for plotting 
    objects.to.plot <- list()
    
    
    #### max.for.scale <- 0
    data.layers <- list()
    model.layers <- list()
    
    for(object in sources){ 
      
      
      # first check the first contributing object/field
      if(is.ModelRunInfo(object@info1)){
        
        if(!object@info1@id %in%  model.layers){
          
          model.layers[[length(model.layers) + 1]] <- object@info1@id
          new.dt <- object@data[, append(getSTInfo(object), names(object)[1]), with=FALSE]
          setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant@id )
          
          objects.to.plot[[length(objects.to.plot)+1]] <- new("ModelObject",
                                                             id = object@id,
                                                             data = new.dt,
                                                             quant = object@quant,
                                                             spatial.extent = object@spatial.extent,
                                                             spatial.extent.id = object@spatial.extent.id,
                                                             temporal.extent = object@temporal.extent,
                                                             temporal.extent.id = object@temporal.extent.id,
                                                             spatial.aggregate.method = object@spatial.aggregate.method,
                                                             temporal.aggregate.method = object@temporal.aggregate.method,
                                                             subannual.aggregate.method = object@ subannual.aggregate.method,
                                                             subannual.original = object@subannual.original,
                                                             run = object@info1)
        }
      }
      else if(is.DatasetInfo(object@info1)){
        
        if(!object@info1@id %in%  data.layers){
          
          data.layers[[length(data.layers) + 1]] <- object@info1@id
          new.dt <- object@data[, append(getSTInfo(object), names(object)[1]), with=FALSE]
          setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant@id )
          
          objects.to.plot[[length(objects.to.plot)+1]] <- new("DataObject",
                                                              id = object@id,
                                                              data = new.dt,
                                                              quant = object@quant,
                                                              spatial.extent = object@spatial.extent,
                                                              spatial.extent.id = object@spatial.extent.id,
                                                              temporal.extent = object@temporal.extent,
                                                              temporal.extent.id = object@temporal.extent.id,
                                                              spatial.aggregate.method = object@spatial.aggregate.method,
                                                              temporal.aggregate.method = object@temporal.aggregate.method,
                                                              subannual.aggregate.method = object@ subannual.aggregate.method,
                                                              subannual.original = object@subannual.original,
                                                              run = object@info1)
          }
      }
      
      # then check the second contributing object/field
      if(is.ModelRunInfo(object@info2)){
        
        if(!object@info2@id %in%  model.layers){
          
          model.layers[[length(model.layers) + 1]] <- object@info2@id
          new.dt <- object@data[, append(getSTInfo(object), names(object)[1]), with=FALSE]
          setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant@id )
          
          objects.to.plot[[length(objects.to.plot)+1]] <- new("ModelObject",
                                                              id = object@id,
                                                              data = new.dt,
                                                              quant = object@quant,
                                                              spatial.extent = object@spatial.extent,
                                                              spatial.extent.id = object@spatial.extent.id,
                                                              temporal.extent = object@temporal.extent,
                                                              temporal.extent.id = object@temporal.extent.id,
                                                              spatial.aggregate.method = object@spatial.aggregate.method,
                                                              temporal.aggregate.method = object@temporal.aggregate.method,
                                                              subannual.aggregate.method = object@ subannual.aggregate.method,
                                                              subannual.original = object@subannual.original,
                                                              run = object@info2)
        }
      }
     else if(is.DatasetInfo(object@info2)){
        
        if(!object@info2@id %in%  data.layers){
          
          data.layers[[length(data.layers) + 1]] <- object@info2@id
          new.dt <- object@data[, append(getSTInfo(object), names(object)[1]), with=FALSE]
          setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant@id )
          
          objects.to.plot[[length(objects.to.plot)+1]] <- new("DataObject",
                                                              id = object@id,
                                                              data = new.dt,
                                                              quant = object@quant,
                                                              spatial.extent = object@spatial.extent,
                                                              spatial.extent.id = object@spatial.extent.id,
                                                              temporal.extent = object@temporal.extent,
                                                              temporal.extent.id = object@temporal.extent.id,
                                                              spatial.aggregate.method = object@spatial.aggregate.method,
                                                              temporal.aggregate.method = object@temporal.aggregate.method,
                                                              subannual.aggregate.method = object@ subannual.aggregate.method,
                                                              subannual.original = object@subannual.original,
                                                              run = object@info2)
        }
      }
      
      
      
    }
    
    #   
    #   if(type == "difference") {
    #     layer.to.plot <- "Difference"
    #   }
    #   else {
    #     layer.to.plot <- "Percentage.Difference"
    #     temp.dt <- object@data
    #     layers.names <- names(object)
    #     temp.dt[ , Percentage.Difference := Difference %/0% get(layers.names[2]) * 100 ]
    #     object@data <- temp.dt
    #   }
    #   new.object <- selectLayers(object, layer.to.plot)
    #   
    #   new.field <- new("ModelObject",
    #                    id = object@id,
    #                    data = new.object@data,
    #                    quant = object@quant,
    #                    spatial.extent = object@spatial.extent,
    #                    spatial.extent.id = object@spatial.extent.id,
    #                    temporal.extent = object@temporal.extent,
    #                    temporal.extent.id = object@temporal.extent.id,
    #                    spatial.aggregate.method = object@spatial.aggregate.method,
    #                    temporal.aggregate.method = object@temporal.aggregate.method,
    #                    subannual.aggregate.method = object@ subannual.aggregate.method,
    #                    subannual.original = object@subannual.original,
    #                    run = object@info1)
    #   
    #   objects.to.plot[[length(objects.to.plot)+1]] <- new.field
    #   
    #   # get max value for making the scale symmetric
    #   if(symmetric.scale && missing(limits)) {
    #     max.for.scale <- max(max.for.scale, max(abs(object@data[[layer.to.plot]])))
    #   }
    #   
    # }
    # 
    # # set the colours
    # if(missing(override.cols)) override.cols <-  rev(RColorBrewer::brewer.pal(11, "RdBu"))
    # 
    # # set a symmetric scale (so zero always white/centre colour)
    # if(symmetric.scale) limits <- c(-max.for.scale, max.for.scale)
    # 
    # # make an appropriate title if not provided
    # #if(missing(title)) title <- paste()
    # 
    # return(plotSpatial2(objects.to.plot,
    #                     layers = layer.to.plot,
    #                     override.cols = override.cols,
    #                     limits = limits,
    #                     title = title,
    #                     ...))
    
    
    
  }
  
  
  
}