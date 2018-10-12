#' Extent methods
#' 
#' Methods for returning a raster::extent object from a Field, DataObject or a data.table.  The data.table, or the data.table in the "data" slot of the Field and DataObject, 
#' must have columns named "Lon" and "Lat" otherwise these methods fail.  
#' 
#' @param x A Field, DataObject or a data.table.
#' @param ... Other arguments, not currently used
#' @return A raster::extent
#' @name extent-methods
#' @rdname extent-methods
#' @aliases extent
#' @importMethodsFrom raster extent
#' @exportMethod extent
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
if (!isGeneric("extent")) {
  setGeneric("extent", function(x) standardGeneric("extent"))
}


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
    extent.temp <- raster::extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  raster::extent(ordered.lons[1], ordered.lons[1],
                                  ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                                  ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp =  raster::extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                                  ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                                  ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  raster::extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                                  ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                                  ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                                  ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  return(TRUE)
  
  
})

#' @rdname extent-methods
setMethod("extent", signature(x="Field"), function(x) {
  
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(x@data)) { ordered.lats <- sort(unique(x@data[,Lat]))}
  else {stop("No column called \"Lat\" in the data.table in the data slot of the Field")}
  if("Lon" %in% names(x@data)) { ordered.lons <- sort(unique(x@data[,Lon]))}
  else {stop("No column called \"Lon\" in the data.table in the data slot of the Field")}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site
  if(length(ordered.lons) == 1 && length(ordered.lats) == 1){
    extent.temp <- raster::extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  raster::extent(ordered.lons[1], ordered.lons[1],
                                  ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                                  ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp = raster::extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                                 ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                                 ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  raster::extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                                  ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                                  ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                                  ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  
})


#' @rdname extent-methods 
setMethod("extent", signature(x="Comparison"), function(x) {
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(x@data)) { ordered.lats <- sort(unique(x@data[,Lat]))}
  else {stop("No column called \"Lat\" in the data.table in the data slot of the Comparison")}
  if("Lon" %in% names(x@data)) { ordered.lons <- sort(unique(x@data[,Lon]))}
  else {stop("No column called \"Lon\" in the data.table in the data slot of the Comparison")}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site
  if(length(ordered.lons) == 1 && length(ordered.lats) == 1){
    extent.temp <- raster::extent(ordered.lons[1], ordered.lons[1], ordered.lats[1], ordered.lats[1])
  }
  # transect along lon
  else if (length(ordered.lons) == 1) {
    extent.temp =  raster::extent(ordered.lons[1], ordered.lons[1],
                                  ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                                  ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  # transect along lat
  else if (length(ordered.lats) == 1) {
    extent.temp =  raster::extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                                  ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                                  ordered.lats[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    extent.temp =  raster::extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2),
                                  ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                                  ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2),
                                  ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  
})


