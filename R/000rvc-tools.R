#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
###################################### 

###
### Matt Forrest 2016-01-26
### Need a new way to handle this stuff with file paths etc




###############################################################################
### SPECIFY SYSTEM DEPENDENT PATHS - CHANGE THESE FOR YOUR SYSTEM
################################################################################


####### SPECIFY LOCATION OF AUXILIARY DATA HERE 
auxiliary.data.dir <- "/home/forrest/AuxiliaryData" # NOTE: must be the full path or OGR gets confused :-(


###############################################################################
### BUILD COMPOUND PATHS TO SHAPEFILES, GRIDLISTS, BENCHMARKING DATA ETC.
###############################################################################


####### DIRECTORIES FOR ALL BENCHMARKING DATA, SHAPEFILES, GRIDLISTS
benchmarking.data.dir <- file.path(auxiliary.data.dir, "BenchmarkingData")
shapefile.dir <- file.path(auxiliary.data.dir, "Shapefiles") # NOTE: must be the full path or OGR gets confused :-(
gridlist.dir <- file.path(auxiliary.data.dir, "Gridlists/")


######
#gfed4.annual <-  file.path(benchmarking.data.dir, "GFED4/GFED4.0_HD_Annual_BA.nc")
#saatchi2011.original.data.path <- ("/senckenberg.de/DATEN_PBE/PB-E/PBE-ALLG/Datasets/Biomass/2011_Saatchi/www-radar.jpl.nasa.gov/projects/carbon/datasets")

is.VegSpatial <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegSpatial" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}

is.VegTemporal <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegTemporal" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}

is.VegRun <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegRun" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}

is.VegQuant <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegQuant" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}


## check if two classes are comparable
setGeneric("is.equal", function(a, b) standardGeneric("is.equal")) 
setMethod("is.equal", signature("VegQuant", "VegQuant"), function(a, b) {
  if (a@type==b@type && a@units==b@units && a@aggregate.method==b@aggregate.method)
    return(TRUE)
  return(FALSE)
})
setMethod("is.equal", signature("TemporalExtent", "TemporalExtent"), function(a, b) {
  if (a@start==b@start && a@end==b@end)
    return(TRUE)
  return(FALSE)
})
setMethod("is.equal", signature("SpatialExtent", "SpatialExtent"), function(a, b) {
  if (all(!is.finite(c(a@extent@xmin, b@extent@xmin, a@extent@xmax, b@extent@xmax, 
                       a@extent@ymin, b@extent@ymin, a@extent@ymax, b@extent@ymax))))
    return(TRUE)
  if (a@extent@xmin==b@extent@xmin && a@extent@xmax==b@extent@xmax && 
      a@extent@ymin==b@extent@ymin && a@extent@ymax==b@extent@ymax)
    return(TRUE)
  return(FALSE)
})


cropRVC <- function(input, extent){
  
  input.class <- class(input)[1]
  extent.class <- class(extent)[1]
  
  if(extent.class == "Extent") {
    this.extent <- extent
  }
  else if(extent.class == "SpatialExtent"){
    this.extent <- extent@extent
  }
  
  if(input.class == "RasterBrick" | input.class == "RasterStack" | input.class == "RasterLayer"){
    return(crop(input, this.extent))    
  }
  else if(input.class == "data.table"){
    return(input[Lat < this.extent@ymax & Lat > this.extent@ymin & Lon < this.extent@xmax & Lon > this.extent@xmin,])
  }
  
}

cropLPJ <- cropRVC




intersectionRVC <- function(object1, object2){
  
  object1.class <- class(object1)[1]
  object2.class <- class(object2)[1]
  
  # CASE 1 - object 1 is raster/layer/stack
  if(object1.class == "RasterLayer" | object1.class == "RasterStack"  | object1.class == "RasterStack"){
    
    # CASE 1a - object 2 is also a raster/layer/stack
    if(object2.class == "RasterLayer" | object2.class == "RasterStack"  | object2.class == "RasterStack"){
      
      intersection.extent <- intersect(object1, object2)
      return(list(crop(object1, intersection.extent), crop(object2, intersection.extent)))
        
    }
    
    
  }
  
  else {
    
    
    stop(paste("IntersectionRVC not defined for combination of classes ", object1.class, " and ", object2.class))
    
    
  }
  
  
  
  
}




writeNetCDF <- function(raster.in, var.name, var.units, time.resolution = "annual", time.units.string = NULL, long.name = "", filename = "test.nc", ordering = "standard", missing.value = -999999){
  
  # MAKE LONGITUDE AND LATITUDE DIMENSION
  lon.list <- seq(from = xmin(raster.in) + xres(raster.in)/2 , to = xmax(raster.in) - xres(raster.in)/2, by = xres(raster.in))
  lon.dim <- ncdim_def("Lon", "degrees", lon.list, unlim=FALSE, create_dimvar=TRUE)
  lat.list <- seq(from = ymin(raster.in) + yres(raster.in)/2 , to = ymax(raster.in) - yres(raster.in)/2, by = yres(raster.in))
  lat.dim <- ncdim_def("Lat", "degrees", lat.list, unlim=FALSE, create_dimvar=TRUE)
  
  # MAKE TIME DIMENSION
  # monthly - format as "days since YYYY-MM-DD HH:MM:SS"
  if(tolower(time.resolution) == "monthly" || tolower(time.resolution) == "month"){ 
    midpoints <- c(16,44,75,105,136,166,197,228,258,289,319,350)
    ncycles <- ceiling(nlayers(raster.in)/12)
    long.midpoints <- c()
    for(cycle in 0:(ncycles-1)){
      long.midpoints <- append(long.midpoints,midpoints  + (365*cycle))
    }
    time.list <- long.midpoints[1:nlayers(raster.in)]
    time.dim <- ncdim_def("Time", time.units.string, time.list, unlim=FALSE, create_dimvar=TRUE)
  }
  # annual - format as "days since YYYY-MM-DD HH:MM:SS"
  else if(tolower(time.resolution) == "annual" || tolower(time.resolution) == "yearly"){
    
    time.list <- seq(from = 0, to = nlayers(raster.in)-1, by = 1)*365
    time.dim <- ncdim_def("Time", time.units.string, time.list, unlim=FALSE, create_dimvar=TRUE)
    
  }
  
  # PERMUTE THE DATA AND SET UP THE VARIABLE DIMENSION
  
  # convert raster to an array
  array.out <- drop(as.array(raster.in))
  
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



