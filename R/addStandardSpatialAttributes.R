
#' Adds standard spatial attributes to a netCDF file
#' 
#' 
#' lala
#' 
#' @param nc.file The netcdf object to which the attributes should be added
#' 
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