#' Extent methods
#' 
#' Methods for returning a raster::extent object from a ModelObject, DataObject or a data.table.  The data.table, or the data.table in the "data" slot of the ModelObject and DataObject, 
#' must have columns named "Lon" and "Lat" otherwise these methods fail.  
#' 
#' @param x A ModelObject, DataObject or a data.table.
#' @param ... Other arguments, not currently used
#' @return A raster::extent
#' @name extent-methods
#' @rdname extent-methods
#' @aliases extent
#' @importMethodsFrom raster extent
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
setGeneric("extent", function(x) standardGeneric("extent"))


#' @rdname extent-methods
setMethod("extent", signature(x="data.table"), function(x) {
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(x)) { ordered.lats <- sort(unique(x[,Lat]))}
  else {stop("No column called \"Lat\" in the data.table")}
  if("Lon" %in% names(x)) { ordered.lons <- sort(unique(x[,Lon]))}
  else {stop("No column called \"Lon\" in the data.table")}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site or a transect
  if(length(ordered.lons) == 1 && length(ordered.lats) == 1){
    extent.temp <- extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  extent(ordered.lons[1], ordered.lons[1],
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  return(TRUE)
  
  
})

#' @rdname extent-methods
setMethod("extent", signature(x="ModelObject"), function(x) {
  
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(x@data)) { ordered.lats <- sort(unique(x@data[,Lat]))}
  else {stop("No column called \"Lat\" in the data.table in the data slot of the ModelObject")}
  if("Lon" %in% names(x@data)) { ordered.lons <- sort(unique(x@data[,Lon]))}
  else {stop("No column called \"Lon\" in the data.table in the data slot of the ModelObject")}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site
  if(length(ordered.lons) == 1 && length(ordered.lats) == 1){
    extent.temp <- extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  extent(ordered.lons[1], ordered.lons[1],
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  
})

#' @rdname extent-methods 
setMethod("extent", signature(x="DataObject"), function(x) {
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(x@data)) { ordered.lats <- sort(unique(x@data[,Lat]))}
  else {stop("No column called \"Lat\" in the data.table in the data slot of the DataObject")}
    if("Lon" %in% names(x@data)) { ordered.lons <- sort(unique(x@data[,Lon]))}
  else {stop("No column called \"Lon\" in the data.table in the data slot of the DataObject")}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site
  if(length(ordered.lons) == 1 && length(ordered.lats) == 1){
    extent.temp <- extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  extent(ordered.lons[1], ordered.lons[1],
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  
})

#' @rdname extent-methods 
setMethod("extent", signature(x="ComparisonLayer"), function(x) {
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(x@data)) { ordered.lats <- sort(unique(x@data[,Lat]))}
  else {stop("No column called \"Lat\" in the data.table in the data slot of the ComparisonLayer")}
  if("Lon" %in% names(x@data)) { ordered.lons <- sort(unique(x@data[,Lon]))}
  else {stop("No column called \"Lon\" in the data.table in the data slot of the ComparisonLayer")}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site
  if(length(ordered.lons) == 1 && length(ordered.lats) == 1){
    extent.temp <- extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  extent(ordered.lons[1], ordered.lons[1],
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  
})


#' @rdname extent-methods 
setMethod("extent", signature(x="SpatialExtent"), function(x) {
  
  return(x@extent)
  
})
