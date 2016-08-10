#!/usr/bin/Rscript

###
### Matt Forrest 2016-01-26
### Need a new way to handle this stuff with file paths etc


#' Check is an object is a \code{VegObject}.   
#'
#' Returns TRUE if an object is a \code{VegObject}, and FALSE otherwise 
#' 
#' @param input Any R object to bec checked
#' @param spatial check if input is a spatial object
#' @param temporal check if input is a temporal object
#' @param site check if input is a site object
#' @return logical
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

is.VegObject <- function(input, spatial=FALSE, temporal=FALSE, site=FALSE) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package"))) {
    if (class.def[1] == "VegObject" && attr(class.def, "package")=="DGVMTools") {
      ## JS: check more carefully if this if structure makes sense
      if (spatial && !temporal && !site) {
        if (!input@is.site && !input@is.spatially.averaged)
          return(TRUE)
      } else if (!spatial && temporal) {
        if (!input@is.temporally.averaged)
          return(TRUE)
      } else if (site) {
        if (input@is.site)
          return(TRUE)
      } else if (!spatial && !temporal && !site) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
  return(FALSE)
}



#' Check is an object is a \code{VegRun}.   
#'
#' Returns TRUE if an object is a \code{VegRun}, and FALSE otherwise 
#' 
#' @param input Any R object to be checked
#' @return logical
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
is.VegRun <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegRun" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}


#' Check is an object is a \code{VegQuant}.   
#'
#' Returns TRUE if an object is a \code{VegQuant}, and FALSE otherwise 
#' 
#' @param input Any R object to be checked
#' @return logical
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
is.VegQuant <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegQuant" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}


#' Check if an object is a \code{SpatialComparison}.   
#'
#' Returns TRUE if an object is a \code{SpatialComparison}, and FALSE otherwise 
#' 
#' @param input Any R object to be checked
#' @return logical
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
is.SpatialComparison <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "SpatialComparison" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}



## check if two classes are comparable
#' Checks two DGVMTools metadata objects for equality
#' 
#' @description checks if two objects have the same quantity, spatial or temporal extent
#' 
#' @param a a DGVMTools metadata object
#' @param b another DGVMTools metadata object of the same type
#' @return logical
#' @name is.equal
#' @rdname is.equal
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
setGeneric("is.equal", function(a, b) standardGeneric("is.equal"))

#' @describeIn is.equal Checks two VegQuant objects for equality
setMethod("is.equal", signature("VegQuant", "VegQuant"), function(a, b) {
  if (a@type==b@type && a@units==b@units && a@aggregate.method==b@aggregate.method)
    return(TRUE)
  return(FALSE)
})

#' @describeIn is.equal Checks two TemoralExtent objects for equality
setMethod("is.equal", signature("TemporalExtent", "TemporalExtent"), function(a, b) {
  if (a@start==b@start && a@end==b@end)
    return(TRUE)
  return(FALSE)
})

#' @describeIn is.equal Checks two SpatialExtent objects for equality
setMethod("is.equal", signature("SpatialExtent", "SpatialExtent"), function(a, b) {
  if (all(!is.finite(c(a@extent@xmin, b@extent@xmin, a@extent@xmax, b@extent@xmax, 
                       a@extent@ymin, b@extent@ymin, a@extent@ymax, b@extent@ymax))))
    return(TRUE)
  if (a@extent@xmin==b@extent@xmin && a@extent@xmax==b@extent@xmax && 
      a@extent@ymin==b@extent@ymin && a@extent@ymax==b@extent@ymax)
    return(TRUE)
  return(FALSE)
})

## print an experimental summary of the class VegRun.
##
## Don't know, which were the really important information to return,
## therfore this is just a template.
## so far simply returns a list. 
## Needs to also include a generic "print" for pretty output
##

#' Summary methods
#' 
#' Print easy to read summaries of DGVMTools objects
#' 
#' @param object a DGVMTools object
#' @param ... Other arguments, not currently used
#' @return A list of strings
#' @name Summary-methods
#' @rdname Summary-methods
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}         
setGeneric("summary", function(object,...) standardGeneric("summary"))

#' @rdname Summary-methods
#' @aliases summary
setMethod("summary", signature("VegRun"), function(object, ...) {
  ret <- list(id=object@id, description=object@description, model=object@model)
  
  spatial=NULL
  temporal=NULL
  full=NULL
  for (n in names(object@objects)) {
    if (object@objects[[n]]@is.spatially.averaged) {
      temporal[[n]]=list(name=n,
                         description=object@objects[[n]]@quant@name,
                         units=object@objects[[n]]@quant@units,
                         colnames=colnames(object@objects[[n]]@data))
    } else if (object@objects[[n]]@is.temporally.averaged) {
      spatial[[n]]=list(name=n,
                        description=object@objects[[n]]@quant@name,
                        units=object@objects[[n]]@quant@units,
                        colnames=colnames(object@objects[[n]]@data))
    } else {
      full[[n]]=list(name=n,
                     description=object@objects[[n]]@quant@name,
                     units=object@objects[[n]]@quant@units,
                     colnames=colnames(object@objects[[n]]@data))
    }
  }
  if (!is.null(spatial)) {
    ret[['spatial']] = spatial
  }
  if (!is.null(temporal)) {
    ret[['temporal']] = temporal
  }
  if (!is.null(full)) {
    ret[['full']] = full
  }
  
  return(ret)
})


#' Crop VegObjects (or data.tables, or Raster* objects)
#' 
#' A more flexible version of raster::crop() which also take VegObjects and data.tables for cropping, 
#' and can use a SpatialExtent object to define the domain.  SHOULD BE DEFINED AS A METHOD EXTENDING raster::crop()!
#' 
#' @param input The VegObject, data.table or Raster* object to be cropped
#' @param extent The spatial extent to be be cropped to, defined as a SpatialExtent or raster::extent
#' 
#' @return A VegObject, data.table or Raster* object cropped to the desired extent.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster data.table
#' @export

cropDGVM <- function(input, extent){
  
  Lat = Lon = NULL
  
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


##### RETRIEVES AN OBJECT FROM A LIST BASED ON THE 'id' SLOTS
#
#' Retrieves an object from a list based on it's \code{id} slot
#' 
#' Looks through a list of arbitary object until it finds one which has a slot called "id" whose value matches the imput argument.
#' The idea is that you can use this fucntion to pull a particular PFT from a list of PFT objects, or a a particular run from a list of VegRun objects etc.
#' 
#' @param id The id sought (must be string)
#' @param list The list to be scanned (must be a list)
#' 
#' @return The matching object from the list or NULL.  If NULL it will give a message and a warning, the following code will probably fail. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
#' 
byIDfromList <- function(id, list) {
  
  
  # Error checking
  if(class(id)[1] != "character") {
    stop(paste("From function IDfromList(): Argument 'id' not a string therefore not valid.  It has class = ",  class(id)[1], sep = ""))
  }
  
  if(class(list)[1] != "list") {
    stop(paste("From function IDfromList(): Argument 'list' is not a list therefore not valid.  It has class =",  class(list)[1], sep = ""))
  }
  
  for(item in list){
    
    tryCatch(
      {
        if(item@id == id) return(item)
      },
      error= function(cond){
        message(paste("Caught an expection. Function byIDfromList(id, list) found an item in the list argument with no slots. ", sep = ""))
      },
      warning=function(cond) {
      },
      finally={}
    )    
    
  }
  
  message(paste("ATTENTION! No object with id = ", id, " found in list ", deparse(substitute(list)), ", so probably your script will now fail.", sep = ""))
  
  
}

#' Write a netCDF file
#' 
#' This function gives more flexibility (for example it can write more meta-data and can re-order the dimensions for efficient LPJ-GUESS reading) 
#' than the similar raster::\code{writeRaster} function.  It is also intended that this function can write single sites/time series functionality 
#' is not included yet.  Also might need some work to be CF compliant on the time dimension.
#' 
#' @param raster.in The data as a Raster* object.  This should be broadened to also include data.frames and data.tables for writing 
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
writeNetCDF <- function(raster.in, 
                        var.name, 
                        var.units, 
                        time.resolution = "annual", 
                        time.units.string = NULL, 
                        long.name = "", 
                        filename = "test.nc", 
                        ordering = "standard", 
                        missing.value = -999999){
  
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
  else if(tolower(time.resolution) == "none"){
    # if we have a third dimension but no time resolution we have a multivariable netCDF file
    # in this case check that the var.name matches 
    
    ## TOO COMPLICATED, ABORT
    
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



