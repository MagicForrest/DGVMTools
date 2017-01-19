

#' Write a netCDF file
#' 
#' This function gives more flexibility (for example it can write more meta-data and can re-order the dimensions for efficient LPJ-GUESS reading) 
#' than the similar raster::\code{writeRaster} function.  It is also intended that this function can write single sites/time series functionality 
#' is not included yet.  Also might need some work to be CF compliant on the time dimension.
#' 
#' @param data.in The data as a Raster* object.  This should be broadened to also include data.frames and data.tables for writing 
#' time series from a particular point.
#' @param var.name A character string containing the name of the variable as used in the netCDF file produced.
#' @param var.units A character string containing the units of the variable as used in the netCDF file produced.
#' @param time.resolution A character string denoting the time resolution of the data. Currently can be "monthly" or "annual".
#' @param time.units.string A string to represent the time units.  CHECK THIS.
#' @param long.name A charcter string for the "long_name" of the netCDF file.
#' @param filename A character string (including the path) specifiying the name of the netCDF file.
#' @param ordering A character string specifying the ordering of the dimension in the resulting netCDF file, can be "standard" for normal ordering 
#' or "lpj" for ordering for fast LPJ-GUESS reading.
#' @param missing.value A numeric value for the "missing_value" of the netCDF file.
#'
#' @return Noting, writes a netCDF file to disk
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
#' @import ncdf4 raster
writeNetCDF <- function(data.in, 
                        var.name, 
                        var.units, 
                        time.resolution = "annual", 
                        time.units.string = NULL, 
                        long.name = "", 
                        filename = "test.nc", 
                        ordering = "standard", 
                        missing.value = -999999){
  
  
  input.class <- class(data.in)[1]
  
  # IF INPUT IS A RASTER
  if(input.class == "RasterLayer" || input.class == "RasterStack" || input.class == "RasterBrick") {
    
    
    # MAKE LONGITUDE AND LATITUDE DIMENSION
    lon.list <- seq(from = xmin(data.in) + xres(data.in)/2 , to = xmax(data.in) - xres(data.in)/2, by = xres(data.in))
    lon.dim <- ncdim_def("Lon", "degrees", lon.list, unlim=FALSE, create_dimvar=TRUE)
    lat.list <- seq(from = ymin(data.in) + yres(data.in)/2 , to = ymax(data.in) - yres(data.in)/2, by = yres(data.in))
    lat.dim <- ncdim_def("Lat", "degrees", lat.list, unlim=FALSE, create_dimvar=TRUE)
    
    # MAKE TIME DIMENSION
    # monthly - format as "days since YYYY-MM-DD HH:MM:SS"
    if(tolower(time.resolution) == "monthly" || tolower(time.resolution) == "month"){ 
      midpoints <- c(16,44,75,105,136,166,197,228,258,289,319,350)
      ncycles <- ceiling(nlayers(data.in)/12)
      long.midpoints <- c()
      for(cycle in 0:(ncycles-1)){
        long.midpoints <- append(long.midpoints,midpoints  + (365*cycle))
      }
      time.list <- long.midpoints[1:nlayers(data.in)]
      time.dim <- ncdim_def("Time", time.units.string, time.list, unlim=FALSE, create_dimvar=TRUE)
    }
    # annual - format as "days since YYYY-MM-DD HH:MM:SS"
    else if(tolower(time.resolution) == "annual" || tolower(time.resolution) == "yearly"){
      
      time.list <- seq(from = 0, to = nlayers(data.in)-1, by = 1)*365
      time.dim <- ncdim_def("Time", time.units.string, time.list, unlim=FALSE, create_dimvar=TRUE)
      
    }
    else if(tolower(time.resolution) == "none"){
      # if we have a third dimension but no time resolution we have a multivariable netCDF file
      # in this case check that the var.name matches 
      
      ## TOO COMPLICATED, ABORT
      
    }
    
    # convert raster to an array
    array.out <- drop(as.array(data.in))
    
  }
  
  else if(input.class == "matrix"){
    
    # MAKE LONGITUDE AND LATITUDE DIMENSION
    lon.list <- as.numeric(colnames(data.in))
    lon.dim <- ncdim_def("Lon", "degrees", lon.list, unlim=FALSE, create_dimvar=TRUE)
    lat.list <- as.numeric(row.names(data.in))
    lat.dim <- ncdim_def("Lat", "degrees", lat.list, unlim=FALSE, create_dimvar=TRUE)
    
    array.out <- data.in[dim(data.in)[1]:1,]
    
  }
  
  # PERMUTE THE DATA AND SET UP THE VARIABLE DIMENSION
  
  # For time series
  if(length(dim(array.out)) == 3) {
    
    # reverse latitudes, necessary, but I don't know why
    array.out <- array.out[dim(array.out)[1]:1,,]
    
    # order dimensions as requested and create empty netCDF file
    if(tolower(ordering) == "standard" || tolower(ordering) == "std") {
      array.out <- aperm(array.out, c(2,1,3))  
      var.out <- ncvar_def(var.name, var.units, list(lon.dim, lat.dim,  time.dim), longname = long.name, missing.value)  # standard
    }
    else if(tolower(ordering) == "lpj" || tolower(ordering) == "lpj-guess"){
      array.out <- aperm(array.out, c(3,2,1))
      var.out <- ncvar_def(var.name, var.units, list(time.dim, lon.dim, lat.dim ), missing.value)  # order for fast access in LPJ-GUESS
    }
    else {
      stop(paste("writeNetCDF: Unknown ordering requested: ", ordering))
    }
    
  }
  # For a single map (no-time series)
  else if(length(dim(array.out)) == 2){
    
    # reverse latitudes, necessary, but I don't know why
    array.out <- array.out[dim(array.out)[1]:1,]
    
    # here lon and lat are standard because it is the time axis that cause somplication in LPJ guess
    array.out <- aperm(array.out, c(2,1))  
    var.out <- ncvar_def(var.name, var.units, list(lon.dim, lat.dim), longname = long.name, missing.value)  # standard
    
    
  }
  
  
  
  # CREATE FILE AND ADD THE VARIABLE
  outfile <- nc_create(filename, var.out, verbose=FALSE)
  ncvar_put(outfile, var.name,  array.out, start=NA, count=NA, verbose=FALSE)
  print(paste("Saving variable", var.name, "to file",  filename, sep =" " ), quote=FALSE)
  
  # ADD ATTRIBUTES
  ncatt_put(outfile, "Lon" , "units", "degrees_east")
  ncatt_put(outfile, "Lon" , "axis", "X")
  ncatt_put(outfile, "Lon" , "standard_name", "longitude")
  ncatt_put(outfile, "Lon" , "long_name", "longitude")
  
  ncatt_put(outfile, "Lat" , "units", "degrees_north")
  ncatt_put(outfile, "Lat" , "axis", "Y")
  ncatt_put(outfile, "Lat" , "standard_name", "latitude")
  ncatt_put(outfile, "Lat" , "long_name", "latitude")
  
  if(length(dim(array.out)) == 3){
    ncatt_put(outfile, "Time" , "units", time.units.string)
    ncatt_put(outfile, "Time" , "axis", "T")
    ncatt_put(outfile, "Time" , "standard_name", "time")
    ncatt_put(outfile, "Time" , "long_name", "Time")
    ncatt_put(outfile, "Time" , "calendar", "365_day")
  }
  
  ncatt_put(outfile, 0, "Conventions", "CF-1.6")
  
  
  # CLOSE
  nc_close(outfile)
  
}


