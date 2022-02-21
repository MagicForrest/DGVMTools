#!/usr/bin/Rscript


#################################################################################################################################################
################################################## PLOT COMPARISON MAPS #########################################################################
#################################################################################################################################################


#' Plot a comparison between two spatial layers
#' 
#' This function is for plotting maps from Comparison objects (or a list of those Comparisons).  Three types of comparisons plots are supported: 'difference' - 
#' a difference map; "values" - the absolute values plotted in panels and "percentage.difference" - the percentage differences.  
#' 
#' @param comparisons The data to plot, must be a Comparison or a list of Comparisons
#' @param type A character specifying what type of plot to make. Can be "difference" (default, for a difference plot), "percentage.difference", "values" 
#' (actual values, side-by-side) or "nme" (for the Normalised Mean Error, not yet implemented)
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values.
#' @param legend.title A character string or expression to override the default legend title. Set to NULL for no legend title.  The default legend title is the \code{units}
#' of the \linkS4class{Quantity} of the first \linkS4class{Comparison} provided in the \code{comparisions} argument.  This argument allows general flexibility, but it is particularly handy
#' to facilitate expressions for nicely marked up subscript and superscript. 
#' @param panel.bg.col Colour string for the panel background, default to "white" for absolute values plots, and a grey for difference plots.
#' @param override.cols A colour palette function to override the defaults.
#' @param symmetric.scale If plotting a differences, make the scale symmetric around zero (default is TRUE)
#' @param do.phase Logical, only applies to plotting Comparison objects of type "seasonal".
#' If TRUE plot the the seasonal phase, if FALSE (the default), plot the seasonal concentration.
#' @param ... Parameters passed to \code{plotSpatial()}
#' 
#' @details  A wrapper for around \code{plotSpatial()} to plot the spatial Comparisons as maps.  Extra arguments to \code{plotSpatial} can also be specified. 
#' 
#' @return Returns a ggplot object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' 
#' @export 
#' @seealso \code{plotSpatial},  \code{compareLayers}

plotSpatialComparison <- function(comparisons,
                                  type = c("difference", "percentage.difference", "values", "nme"),
                                  limits = NULL,
                                  legend.title,
                                  panel.bg.col = "white",
                                  override.cols = NULL,
                                  symmetric.scale = TRUE,
                                  do.phase = FALSE,
                                  ...){
  
  Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = NULL
  Difference = Percentage.Difference = NULL
  
  # sort type argument
  type <- match.arg(type)
  
  if(!missing(limits)) symmetric.scale <- FALSE

  ### CHECK TO SEE EXACTLY WHAT WE SHOULD PLOT
  
  ### 1. COMPARISONS - check the input Comparison objects (and if it is a single Comparison put it into a one-item list)
  
  comparisons <- santiseComparisonsForPlotting(comparisons)
  if(is.null(comparisons)) return(NULL)
  
  
  ### 2. DIMENSIONS - check the dimensions (require that all fields the same dimensions and that they include 'Lon' and 'Lat' )
  
  dim.names <- santiseDimensionsForPlotting(comparisons, require = c("Lon", "Lat"))
  if(is.null(dim.names)) return(NULL)
  # dim names not used later
  
  
  ### 3. LAYERS AND FIELDS - the layers to plot are defined by the plot type, here build appropriate Field objects
  
  #### DIFFERENCE OR PERCENTAGE DIFFERENCE
  if(type == "difference" || type == "percentage.difference") {
    
    # convert the Comparisons into a Fields  for plotting 
    objects.to.plot <- list()
    max.for.scale <- 0
    final.layers.to.plot <- c()
    for(object in comparisons){ 
      
      # special case for seasonal comparisons, the layers are actually called "SeasonalConcentration" and "SeasonalPhase"
      if(object@type == "seasonal") {
        if(do.phase){
          object@layers1 <- c("Seasonal Phase")
          object@layers2 <- c("Seasonal Phase")
          object@quant1@units <- "Month"
          object@quant2@units <- "Month"
        }
        else {
          object@layers1 <- c("Seasonal Concentration")
          object@layers2 <- c("Seasonal Concentration")
          object@quant1@units <- "Concentration"
          object@quant2@units <- "Concentration"
        }
      }
      
      # first make a list of the layers that we expect to be present in the data.table, based on the meta-data in the Comparison object
      layers.names <- names(object)
      expected.layers.1 <- paste(object@layers1, makeFieldID(source = object@source1, quant.string = object@quant1@id, sta.info = object@sta.info1), sep = ".")
      expected.layers.2 <- paste(object@layers2, makeFieldID(source = object@source2, quant.string = object@quant2@id, sta.info = object@sta.info2), sep = ".")
    
      # check the layers
      for(this.layer in expected.layers.1) if(!this.layer %in% layers.names) stop(paste("Layer", this.layer, "expected in Comparison object but not found"))
      for(this.layer in expected.layers.2) if(!this.layer %in% layers.names) stop(paste("Layer", this.layer, "expected in Comparison object but not found"))
      
      
      # adjust the source ids if they are identical 
      if(object@source1@id == object@source2@id) {
        
        # include the first and last years if they are not the same
        if((object@sta.info1@first.year != object@sta.info2@first.year) && (object@sta.info1@last.year != object@sta.info2@last.year)){
          object@source1@name <- paste0(object@source1@name, " (", object@sta.info1@first.year, "-", object@sta.info1@last.year, ")")
          object@source2@name <- paste0(object@source2@name, " (", object@sta.info2@first.year, "-", object@sta.info2@last.year, ")")
        }
        
      }
      
      # calculate the difference layers
      temp.dt <- object@data
      layers.to.plot <- c()
      for(layer.counter in 1:length(expected.layers.1)) {
        
        difference.column.name <- paste()
        
        if(object@type == "categorical")  {
          difference.column.name <- paste("Difference", object@layers1[layer.counter])
          layers.to.plot <- append(layers.to.plot, difference.column.name)
          temp.dt[, c(difference.column.name) := as.character(get(expected.layers.1[layer.counter])) == as.character(get(expected.layers.2[layer.counter]))]
        }
        else {
          difference.column.name <- paste("Difference", object@layers1[layer.counter])
          layers.to.plot <- append(layers.to.plot, difference.column.name)
          if(object@type == "seasonal" & do.phase) {
            temp.dt[,c(difference.column.name) := get(expected.layers.1[layer.counter]) - get(expected.layers.2[layer.counter])]
            temp.dt[,c(difference.column.name) := ifelse(get(difference.column.name) < 6, get(difference.column.name) + 12, get(difference.column.name))]
            temp.dt[,c(difference.column.name) := ifelse(get(difference.column.name) > 6, get(difference.column.name) - 12, get(difference.column.name))]
            
          }
          else {
            temp.dt[,c(difference.column.name) := get(expected.layers.1[layer.counter]) - get(expected.layers.2[layer.counter])]
            if(type == "percentage.difference") temp.dt[,c(difference.column.name) := 100 * get(difference.column.name) / get(expected.layers.2[layer.counter])]

              
          }
        }
        
      }  
    
      object@data <- temp.dt
      
      new.object <- selectLayers(object, layers.to.plot)
      
      new.field <- new("Field",
                       id = object@id,
                       data = new.object@data,
                       quant = object@quant1,
                       source = object@source1,
                       commonSTAInfo(list(object@sta.info1, object@sta.info2)))
      
      new.field@source@name <- object@name
      if(object@type == "continuous") new.field@source@id <- paste(object@source1@id, object@source2@id, sep ="-")
      
      
      objects.to.plot[[length(objects.to.plot)+1]] <- new.field
      
      # get max value for making the scale symmetric
      if(symmetric.scale && missing(limits)) {
        for(this.layer in layers.to.plot)  max.for.scale <- max(max.for.scale, max(abs(object@data[[this.layer]])))
      }
      
    }
    
    # get the unique layer names (since their might be duplicates)
    layers.to.plot <- unique(final.layers.to.plot)
    
    # set the colours
    if(missing(override.cols)) override.cols <-  rev(RColorBrewer::brewer.pal(11, "RdBu"))
    
    # set a symmetric scale (so zero always white/centre colour)
    if(symmetric.scale) limits <- c(-max.for.scale, max.for.scale)

    
    # if no panel background panel colour specified, use a non-white one
    if(missing(panel.bg.col)) panel.bg.col = "#999999"
    
    # make a legend title if one has not been supplied
    if(missing(legend.title)) {
      if(type == "percentage.difference") legend.title <- expression(Delta * "%")
      else legend.title <- stringToExpression(paste0("Delta~", standardiseUnitString(object@quant1@units)))
    }

    the.plot <- plotSpatial(objects.to.plot,
                            layers = layers.to.plot,
                            cols = override.cols,
                            legend.title = legend.title,
                            limits = limits,
                            panel.bg.col = panel.bg.col,
                            ...)
    
    
    
    if(object@type[[1]] == "categorical") the.plot <- the.plot + scale_fill_discrete(name = "Agreement")
    return(the.plot)
    
  }
  
  ### VALUES 
  else if(type == "values") {
    
    
    # convert the Comparisons into Fields for plotting 
    objects.to.plot <- list()
    layers.to.plot <- c()
    for(object in comparisons){ 
      
      # special case for seasonal comparisons, the layers are actually called "SeasonalConcentration" and "SeasonalPhase"
      if(object@type == "seasonal") {
        if(do.phase){
          object@layers1 <- c("Seasonal Phase")
          object@layers2 <- c("Seasonal Phase")
          object@quant1@units <- "month"
          object@quant2@units <- "month"
          if(missing(limits)) limits <- c(0,12)
        }
        else {
          object@layers1 <- c("Seasonal Concentration")
          object@layers2 <- c("Seasonal Concentration")
          object@quant1@units <- "concentration"
          object@quant2@units <- "concentration"
          if(missing(limits)) limits <- c(0,1)
        }
      }
      
      # first make a list of the layers that we expect to be present in the data.table, based on the meta-data in the Comparison object
      layers.names <- names(object)
      expected.layers.1 <- paste(object@layers1, makeFieldID(source = object@source1, quant.string = object@quant1@id, sta.info = object@sta.info1), sep = ".")
      expected.layers.2 <- paste(object@layers2, makeFieldID(source = object@source2, quant.string = object@quant2@id, sta.info = object@sta.info2), sep = ".")
      
      # check the layers
      for(this.layer in expected.layers.1) if(!this.layer %in% layers.names) stop(paste("Layer", this.layer, "expected in Comparison object but not found"))
      for(this.layer in expected.layers.2) if(!this.layer %in% layers.names) stop(paste("Layer", this.layer, "expected in Comparison object but not found"))
      
      # adjust the source ids if they are identical 
      if(object@source1@id == object@source2@id) {
        
        # include the first and last years if they are not the same
        if((object@sta.info1@first.year != object@sta.info2@first.year) && (object@sta.info1@last.year != object@sta.info2@last.year)){
          object@source1@name <- paste0(object@source1@name, " (", object@sta.info1@first.year, "-", object@sta.info1@last.year, ")")
          object@source2@name <- paste0(object@source2@name, " (", object@sta.info2@first.year, "-", object@sta.info2@last.year, ")")
        }
        
      }
      
      
      
      # SECOND INFO - putting this first because this is the 'base' dataset ("one minus two" convention)
      new.dt <- object@data[, append(getDimInfo(object), expected.layers.2), with=FALSE]
      #setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant2@id )
      setnames(new.dt, expected.layers.2, object@layers2)
      layers.to.plot <- append(layers.to.plot, object@layers2)
      objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
                                                          id = object@id,
                                                          data = new.dt,
                                                          quant = object@quant2,
                                                          source = object@source2,
                                                          object@sta.info2)
      
      # FIRST INFO
      new.dt <- object@data[, append(getDimInfo(object), expected.layers.1), with=FALSE]
      #setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant1@id )
      setnames(new.dt, expected.layers.1, object@layers1 )
      layers.to.plot <- append(layers.to.plot, object@layers1)
      objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
                                                          id = object@id,
                                                          data = new.dt,
                                                          quant = object@quant1,
                                                          source = object@source1,
                                                          object@sta.info1)
      
    }
    
    # make a legend title if one has not been supplied
    if(missing(legend.title)) legend.title <- stringToExpression(standardiseUnitString(object@quant1@units))
    
    return(plotSpatial(objects.to.plot,
                       layers =  unique(layers.to.plot),
                       cols = override.cols,
                       limits = limits,
                       legend.title = legend.title,
                       ...))
    
    
    
  }
  
  
  
}