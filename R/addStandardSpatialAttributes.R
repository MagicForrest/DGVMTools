
#' Adds standard spatial attributes to a netCDF file
#' 
#' A very simple convenience function for filling the the standard Lon/Lat info when preparing a netCDF file 
#' 
#' @param nc.file The netcdf object to which the attributes should be added
#' @return A netcdf object
#' @import ncdf4
#' @export

addStandardSpatialAttributes <- function(nc.file) { 
  
  
  # FIND CORRECT DIMENSION NAMES
  all.dim.names <- names(nc.file$dim)
  
  lat.name <- NULL
  possible.lat.names <- c("lat", "Lat", "latitude", "Latitude")
  for(possible in possible.lat.names) {
    if(possible %in% all.dim.names) lat.name <- possible
  }
  if(is.null(lat.name)) stop("Can't find a latitude dimension.  Should be one of lat/Lat/latitude/Latitude")
  
  lon.name <- NULL
  possible.lon.names <- c("lon", "Lon", "longitude", "Longitude")
  for(possible in possible.lon.names) {
    if(possible %in% all.dim.names) lon.name <- possible
  }
  if(is.null(lon.name)) stop("Can't find a longitude dimension.  Should be one of lon/Lon/longitude/Longitude")
  
  
  # ADD ATTRIBUTES
  ncatt_put(nc.file, lon.name , "units", "degrees_east")
  ncatt_put(nc.file, lon.name , "axis", "X")
  ncatt_put(nc.file, lon.name , "standard_name", "longitude")
  ncatt_put(nc.file, lon.name , "long_name", "longitude")
  
  ncatt_put(nc.file, lat.name , "units", "degrees_north")
  ncatt_put(nc.file, lat.name , "axis", "Y")
  ncatt_put(nc.file, lat.name , "standard_name", "latitude")
  ncatt_put(nc.file, lat.name , "long_name", "latitude")
  
  return(nc.file)
  
}
