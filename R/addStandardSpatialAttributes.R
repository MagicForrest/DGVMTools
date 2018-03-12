
#' Adds standard spatial attributes to a netCDF file
#' 
#' 
#' lala
#' 
#' @param nc.file The netcdf object to which the attributes should be added
#' 
#' @keywords internal

addStandardSpatialAttributes <- function(nc.file) { 
  
  # ADD ATTRIBUTES
  ncatt_put(nc.file, "Lon" , "units", "degrees_east")
  ncatt_put(nc.file, "Lon" , "axis", "X")
  ncatt_put(nc.file, "Lon" , "standard_name", "longitude")
  ncatt_put(nc.file, "Lon" , "long_name", "longitude")
  
  ncatt_put(nc.file, "Lat" , "units", "degrees_north")
  ncatt_put(nc.file, "Lat" , "axis", "Y")
  ncatt_put(nc.file, "Lat" , "standard_name", "latitude")
  ncatt_put(nc.file, "Lat" , "long_name", "latitude")
  
  return(nc.file)
  
}