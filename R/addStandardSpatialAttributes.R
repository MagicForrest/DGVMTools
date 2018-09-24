
#' Adds standard spatial attributes to a netCDF file
#' 
#' A very simple convenience function for filling the the standard Lon/Lat info when preparing a netCDF file 
#' 
#' @param nc.file The netcdf object to which the attributes should be added
#' @return A netcdf object
#' @keywords internal

addStandardSpatialAttributes <- function(nc.file, lat.name = "Lat", lon.name = "Lon") { 
  
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
