
#' Adds standard spatial attributes to a netCDF file
#' 
#' A very simple convenience function for filling the the standard Lon/Lat info when preparing a netCDF file 
#' 
#' @param nc.file The netcdf object to which the attributes should be added
#' @return A netcdf object
#' @export

addStandardSpatialAttributes <- function(nc.file) { 
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to manipulate netCDF files.")
  
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
  ncdf4::ncatt_put(nc.file, lon.name , "units", "degrees_east")
  ncdf4::ncatt_put(nc.file, lon.name , "axis", "X")
  ncdf4::ncatt_put(nc.file, lon.name , "standard_name", "longitude")
  ncdf4::ncatt_put(nc.file, lon.name , "long_name", "longitude")
  
  ncdf4::ncatt_put(nc.file, lat.name , "units", "degrees_north")
  ncdf4::ncatt_put(nc.file, lat.name , "axis", "Y")
  ncdf4::ncatt_put(nc.file, lat.name , "standard_name", "latitude")
  ncdf4::ncatt_put(nc.file, lat.name , "long_name", "latitude")
  
  return(nc.file)
  
}
