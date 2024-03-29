#' 
#' This is an internal helper function which checks the inputs to a plotXXXX function (which should be a single Field or a list of Fields) and returns a list of Fields
#' 
#' @param fields The input to a plotXXXX() functions to be checked
#' @return Returns a list of DGVMTools::Field objects or NULL if a problem was found
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
santiseFieldsForPlotting <- function(fields) {
  
  if(is.Field(fields)) {
    fields <- list(fields)
  }
  else if(is(fields, "list")) {
    for(object in fields){ 
      if(!is.Field(object)) {
        warning("You have passed me a list of items to plot but the items are not exclusively Fields.  Returning NULL.\n")
        return(NULL)
      }
    }
  }
  else{
    warning(paste("This plot function can only handle single a Field, or a list of Fields, it can't plot an object of type", class(fields)[1], ".\n", sep = " "))
    return(NULL)
  }
  
  return(fields)
  
}


#' Sanitise input Comparisons for plotting
#' 
#' This is an internal helper function which checks the inputs to a plotXXXXComparison() function (which should be a single Comparison or a list of Comparisons) 
#' and returns a list of Comparisons
#' 
#' @param comparisons The input to a plotXXXXComparison() functions to be checked
#' @return Returns a list of DGVMTools::Comparison objects or NULL if a problem was found
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
santiseComparisonsForPlotting <- function(comparisons) {
  
  if(is.Comparison(comparisons)) {
    comparisons <- list(comparisons)
  }
  else if(is(comparisons[1], "list")) {
    for(object in comparisons){ 
      if(!is.Comparison(object)) {
        warning("You have passed me a list of items to plot but the items are not exclusively Comparisons.  Returning NULL.\n")
        return(NULL)
      }
    }
  }
  else{
    warning(paste("This plot function can only handle single a Comparison, or a list of Comparison, it can't plot an object of type", class(comparisons)[1], "\n", sep = " "))
    return(NULL)
  }
  
  return(comparisons)
  
}

#' Sanitise input layers for plotting
#' 
#' This is an internal helper function which checks the layers requested to be plotted against the layers in the the fields to be plotted.  If layers is NULL, then 
#' it returns all layers present in any fields
#' 
#' @param fields The list of Fields to be plotted (should have been check by santiseFieldsForPlotting first)
#' @param layers The layers requested to be plotted
#' @return Returns character vector of the layers to be plotted
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
santiseLayersForPlotting <- function(fields, layers) {
  
  
  layers.superset <- c()
  num.layers.x.fields <- 0
  
  # if no layers argument supplied make a list of all layers present (in any object)
  if(is.null(layers) || missing(layers)){
    
    for(object in fields){
      temp.layers <- names(object)
      num.layers.x.fields <- num.layers.x.fields + length(temp.layers)
      layers.superset <- append(layers.superset, temp.layers)
    } 
    layers <- unique(layers.superset)
    
  }
  
  # else if layers have been specified check that we have some of the requested layers present
  else{
    
    for(object in fields){
      
      layers.present <- intersect(names(object), layers)
      num.layers.x.fields <- num.layers.x.fields + length(layers.present)
      
      if(length(layers.present) == 0) {warning("Some Fields to plot don't have all the layers that were requested to plot.\n")}
      layers.superset <- append(layers.superset, layers.present)
      
    } 
    
    # Return empty plot if not layers found
    if(num.layers.x.fields == 0){
      warning("None of the specified layers found in the objects provided to plot.  Returning NULL.\n")
      return(NULL)
    }
    
    # Also check for missing layers and given a warning
    missing.layers <- layers[!(layers %in% unique(layers.superset))]
    if(length(missing.layers) != 0) { warning(paste("The following layers were requested to plot but not present in any of the supplied objects:", paste(missing.layers, collapse = " "), ".\n", sep = " ")) }
    
    # finally make a unique list of layers to be carried in to the actual plotting
    layers <- unique(layers.superset)
    
  }
  
  return(layers)
  
}



#' Sanitise STAInfo for plotting
#' 
#' This is an internal helper function which checks the dimensions of the Fields to be plotted 
#' 
#' @param fields The list of Fields to be plotted (should have been check by santiseFieldsForPlotting first)
#' @return Returns character vector of the layers to be plotted
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
santiseDimensionsForPlotting <- function(fields, require = NULL) {
  
  ### Check if all the fields have the same ST dimensions and that Lon and Lat are present.  If not, warn and return NULL
  sta.info <- getDimInfo(fields[[1]], info = "names")
  
  # check Lon and Lat present
  for(required.sta in require)
    if(!required.sta %in% sta.info) {
      warning(paste0("Dimension ", required.sta, " is missing from an input Field but is required for this plot type.  Obviously this won't work, returning NULL.\n"))
      return(NULL)
    }
  
  # check all the same dimensions
  if(length(fields) > 1) {
    for(counter in 2:length(fields)){
      if(!identical(sta.info, getDimInfo(fields[[counter]], info = "names"))) {
        warning(paste0("Trying to plot two Fields with different Spatial-Temporal dimensions.  One has \"", paste(sta.info, collapse = ","), "\" and the other has \"",  paste(getDimInfo(fields[[counter]], info = "names"), collapse = ","), "\".  So not plotting and returning NULL.\n"))
        return(NULL)
      }
    }
  }
  
  return(sta.info)
  
}



#' Check values of a particular dimension 
#' 
#' This is an internal helper function which either stops if a dimension is requested to be plotted but is not present
#  or, if they have not explicitly been requested, it makes a list of possible values of that dimension.  If a value has been requested but is not present for the 
#' dimension, it gives a warning
#' 
#' @param fields The list of Fields to be plotted (should have been check by santiseFieldsForPlotting first)
#' @param input.values A list of the values that have been requested to be plotted
#' @param dimension A character string specifying which dimension is to be checked (one of "Lon", "Lat", "Year", "Month", "Day" or "Season") 
#' @return Returns character vector of the layers to be plotted
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
checkDimensionValues <- function(fields, input.values = NULL,  dimension) {
  
  all.values <- c()
  for(object in fields){
    
    # check if dimension is actually present in the source
    if(!dimension %in% getDimInfo(object)) {
      stop(paste("In plotSpatial you requested plotting of maps per", dimension, "but not all the fields have", dimension, "data.\n I am therefore returning NULL for this plot, but your script should continue.  Check that your input Fields have the time dimensions that you think they have.", sep = " "))
    }
    
    # get a list of all unique days present
    values.present <- unique(object@data[[dimension]])
    
    # input.list specified so check that they are present
    if(!is.null(input.values)) {
      for(counter in input.values) { if(!counter %in% values.present) warning(paste0(dimension, " ", counter, " not present in Field ", object@id), ".\n") }
    }
    # else input.list not specified so make a list of unique days across all Fields
    else { all.values <- append(all.values, values.present) }
    
  }
  
  # make a unique and sorted list of values
  if(is.null(input.values)) input.values <- sort(unique(all.values))
  
  # return
  return(input.values)
  
}

#' Subsets data from Field for plotting 
#' 
#' This is an internal helper function which pulls out the data needed to make a plot from a bunch of Fields, and returns 
#' a list of the Field with only the required layers and points in space and time included  
#' 
#' @param fields The list of Fields to be plotted (should have been check by santiseFieldsForPlotting first)
#' @param layers A character vector of the layers to be plotted
#' @param years The years to be extracted (as a numeric vector), if NULL all years are used
#' @param days The days to be extracted (as a numeric vector), if NULL all days are used
#' @param months The months to be extracted (as a numeric vector), if NULL all months are used
#' @param seasons The months to be extracted (as a character vector), if NULL all seasons are used
#' @param gridcells The months to be extracted (as a character vector), if NULL all seasons are used
#' @param dropEmpty Logical, if TRUE drop layers consisting only of zeros
#' 
#' @return Returns a list of Fields
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
trimFieldsForPlotting <- function(fields, layers, years = NULL, days = NULL, months = NULL, seasons = NULL, gridcells = NULL, dropEmpty = FALSE) {
  
  J = Year = NULL
  
  discrete <- FALSE
  continuous <- FALSE
  
  # Loop through the objects and select the layers and dimensions that we want for plotting
  
  final.fields <- list()
  
  for(object in fields){
    
    # check that at least one layer is present in this object and make a list of those which are
    all.layers <- names(object)
    layers.present <- c()
    for(this.layer in layers) {
      # if layer is present
      if(this.layer %in% all.layers)  {
        
        # if dropEmpty is true check if it it non-zero before appending it
        if(dropEmpty) {
          if(!(object@data[[this.layer]][1] == 0 && all(duplicated(object@data[[this.layer]])[-1L]))){
            layers.present <- append(layers.present, this.layer)
          }
        }
        # else just append it
        else {
          layers.present <- append(layers.present, this.layer)
        }
        
      } #  if layer present
    }  # for all requested layers loop 
    
    # if at least one layer present subset it
    if(length(layers.present) > 0) {
      
      # select the layers and time periods required and mash the data into shape
      these.layers <- selectLayers(object, layers.present)
      if(!is.null(years)) {
        # set key to Year, subset by years, the set keys back
        # note we are not doing selectYears() because we may want to select non-contiguous years
        setkey(these.layers@data, Year)
        these.layers@data <- these.layers@data[J(years)]
        setKeyDGVM(these.layers@data)
      }
      if(!is.null(days)) these.layers <- selectDays(these.layers, days)
      if(!is.null(months)) these.layers <- selectMonths(these.layers, months)
      if(!is.null(seasons)) these.layers <- selectSeasons(these.layers, seasons)
      if(!is.null(gridcells)) these.layers <- selectGridcells(these.layers, gridcells, spatial.extent.id = "Subset_For_Plotting")
      
      # check if layers are all continuous or discrete
      for(layer in layers.present) {
        if(is(object@data[[layer]], "factor") || is(object@data[[layer]], "logical") || is(object@data[[layer]], "ordered")) discrete <- TRUE
        if(is(object@data[[layer]], "numeric") || is(object@data[[layer]],"integer" )) continuous <- TRUE
      }
      if(discrete & continuous) stop("Cannot simultaneously plot discrete and continuous layers, check your layers") 
      if(!discrete & !continuous) stop("Can only plot 'numeric', 'integer', 'factor', 'ordered' or 'logical' layers, check your layers")   
      
      
      final.fields <- append(final.fields, these.layers)
      
    } # end if length(layers.present) > 0
    
  }
  
  return(final.fields)
  
}


#' Merge data from Field for plotting 
#' 
#' This is an internal helper function which pulls out the data from a bunch of Fields to be plotted, adds columns to describe the characteristics 
#' of each Field (ie Quantity, Site code etc) and returns it all in one big melted data.table.
#' 
#' @param fields The list of Fields to be plotted (should have been check by santiseFieldsForPlotting first)
#' @param add.Quantity Logical, if TRUE add a column with the Quantity name of each Field
#' @param add.Site Logical, if TRUE add a column with the with a string containing the Lon and Lat of each site (gridcell).
#' @param add.Region Logical, if TRUE add a column with the with a string containing the Region (as defined by the spatial.extent.id) of each Field.
#' @param add.Years Logical, if TRUE add a column (with name "Years") with a string containing the first and last years of each Field
#'  (as defined by first.year and last.year).
#' 
#' @return Returns a data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 
mergeFieldsForPlotting <- function(fields,  add.Quantity = FALSE,  add.Site = FALSE, add.Region = FALSE, add.Years = FALSE) {
  
  Source = Quantity = Site = Lon = Lat = Region = Years = NULL
  
  data.toplot.list <- list()
  for(this.field in fields) {
    this.dim.info <- getDimInfo(this.field)
    this.field.melted <- melt(this.field@data, id.vars = this.dim.info)
    
    # Source column add in evergy case
    this.field.melted[, Source := this.field@source@name]
    # Quantity column - only if required
    if(add.Quantity) this.field.melted[, Quantity := this.field@quant@name]
    # Site column - only if required
    if(add.Site) {
      if("Lon" %in% this.dim.info  && "Lat" %in% this.dim.info){
        this.field.melted[, Site := paste0("(", Lon, ",",  Lat, ")")]
      }
      else {
        stop("Don't have Lons and Lats so can't make site strings")
      }
    }
    # Region column - only if required
    if(add.Region) {
      this.field.melted[, Region:= this.field@spatial.extent.id]
    }
    # Years column - only if required
    if(add.Years) {
      this.field.melted[, Years := paste(this.field@first.year, this.field@last.year, sep = "-")]
    }
    # add to the list of data.tables
    data.toplot.list[[length(data.toplot.list)+1]] <- this.field.melted
  }
  
  data.toplot <- rbindlist(data.toplot.list)
  rm(data.toplot.list)
  
  # Rename
  setnames(data.toplot, "variable", "Layer")
  setnames(data.toplot, "value", "Value")
  
  
  return(data.toplot)
  
}


#' Make y-axis
#' 
#' This is an internal helper function to build a y-axis for Temporal and Subannual plots, possibly with multiple Quantities
#' 
#' @param final.fields The list of Fields to be plotted (should have been check by santiseFieldsForPlotting first)
#' 
#' @return Returns the y-axis as a chatacter string,
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 

makeYAxis <- function(final.fields) {
  
  # first extract the names and units and store them in a tuples (two element vector) for the Quantity from each Field
  all.quant.tuples <- list()
  for(field in final.fields) {
    all.quant.tuples[[length(all.quant.tuples)+1]] <- c(field@quant@name, field@quant@units)
  } 
  
  # select the unique ones
  all.quant.tuples <- unique(all.quant.tuples)
  
  # form the label string
  y.axis.label <- character(0)
  for(this.tuple in all.quant.tuples) {
    y.axis.label <- paste0(y.axis.label, paste0(this.tuple[1], " (", standardiseUnitString(this.tuple[2]), "),\n") )
  }
  y.axis.label <- substr(y.axis.label,  1, nchar(y.axis.label) - 2)
  
  return(stringToExpression(y.axis.label))
  
}



#' Make a LAyer colour list
#' 
#' This is a helper function for when plotting Layers by colour.  It takes a list of Layer ids (other things like "Total" or "Tree" can also be specified) and returns a list 
#' of colours with the names of the Layer (which is how ggplot likes colours to be specified).
#' 
#' @param values List of values (as chararacters) for which you want standard colours.
#' @param layers A list of Layer objects (which should contain Layers with ids provided in 'values)
#' @param others A list of other name-colour combinations, for example to plot 'Total' as black, "None" as grey, or whatever.  Some defaults are defined.
#' @return Returns a named list of colours, where the names are the values that the colours will represent
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
#' 
#' 
matchLayerCols <- function(values, layers, others = list(Total = "black", None = "grey75", Tree = "brown", Grass = "green", Shrub = "red")) {
  
  these.cols <- list()
  at.least.one.match <- FALSE
  for(val in values) {
    
    # ignore NAs
    if(!is.na(val)) {
      
      # check if it is a layer
      done <- FALSE
      for(this.layer in layers){
        if(val == this.layer@id) {
          these.cols[[val]] <- this.layer@colour
          done <- TRUE
          at.least.one.match <- TRUE
        }
      } 
      
      # if not a Layer, check if it is as 'other' 
      if(!done) {
        for(other in names(others)) {
          if(tolower(val) == tolower(other)) {
            these.cols[[val]] <- others[[other]]
            done <- TRUE
            at.least.one.match <- TRUE
          }
        }
      }
      
      # if no colour can be found to match the value, fail gently
      if(!done && at.least.one.match) {
        warning(paste0("Some value (", val, ") doesn't have a specified colour, so matchLayerCols is returning NULL. Check your inputs and note the you can provide a colour for (", val, ") using the 'others' argument.\n"))
        return(NULL)
      }  
      
    }  # if not NA
    
  } # for each value
  
  if(length(these.cols) == length(values))  return(unlist(these.cols))
  else return(NULL)
  
}


#' Make a map overlay for ggplot2
#' 
#' Reads coastlines or country boundaries from the rnaturalearth package (rnaturalearthdata for the coastlines) and adds it to a ggplot.
#' 
#' @param map_plot  The ggplot to which the map overlay should be addeded
#' @param map_overlay A character string specifying the overlay to be used a string matching maps package dataset
#' @import rnaturalearth rnaturalearthdata
#' @return Returns data.frame suitable for plotting with ggplot::geom_path
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
#' 
addMapOverlay <- function(map_plot, map_overlay) {
  
  # check and autocomplete argument
  country_strings <- c("world", "countries", "ne_countries")
  coastline_strings <- c("coastlines", "ne_coastlines")
  completed_arg <- match.arg(tolower(map_overlay), c(country_strings, coastline_strings))
  
  # add the country lines
  if(completed_arg %in% country_strings) {
    map_plot <- map_plot + geom_sf(data=rnaturalearth::ne_countries(returnclass = "sf"), 
                                   fill = "transparent", 
                                   linewidth = 0.1,
                                   colour= "black")
  }
  # add the the coastlines
  # NOTE: we are adding deliberatealy adding these on top of the country lines because ggplat draw the coastlines fainter than the country lines in the 
  # command above.  Don't why this is, but will maybe report it.
  map_plot <- map_plot + geom_sf(data=rnaturalearth::ne_coastline(returnclass = "sf"), 
                                 fill = "transparent", 
                                 linewidth = 0.2,
                                 colour= "black")
  
  return(map_plot)
  
}


#' @keywords internal
#' @importFrom units as_units

standardiseUnitString <- function(unit.str) {
  
  unit.str <- tryCatch(
    {
      as.character(units(as_units(unit.str)))
    },
    error= function(cond){
      unit.str
    },
    warning=function(cond) {
      unit.str
    },
    finally={}
  )    
  return(unit.str)
  
}

#' @keywords internal
stringToExpression <- function(x){
  
  if(is.character(x)) {
    x <- tryCatch(
      {
        x <- parse(text = gsub(" ", "~", x))
      },
      error= function(cond){
        x
      },
      warning=function(cond) {
        x
      },
      finally={}
    )
  }
  return(x)
  
}