#!/usr/bin/Rscript
#' writeNetCDF methods
#' 
#' Methods for cropping DGVMTools objects (and data.tables) to each other, to raster and sp objects (and basically anything that can be coerced in a raster::Extent object). 
#' The data.table in the "data" slot of the DGVMTools object (or the stand-alone data.table), must have columns named "Lon" and "Lat" otherwise these methods fail.  Unpleasantly.  
#' 
#' @param x A Field or ComparisonLayer or a stand-alone data.table to be cropped
#' @param y Anything from which raster::Extent object can be derived
#' @param ... Other arguments, not currently used
#' @return A spatially cropped object
#' @name writeNetCDF-methods
#' @rdname writeNetCDF-methods
#' @aliases writeNetCDF
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
#' 
#' 
#' 
# first define (redfine) the generic
if (!isGeneric("writeNetCDF")) {
  setGeneric("writeNetCDF", function(x, filename, verbose = FALSE, ...) standardGeneric("writeNetCDF"))
}

#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="Field", filename = "character"), function(x, filename, ...) {
  
  array.slice <- modelObject2Array(x@data) 
  writeNetCDF(array.slice, filename, ...)
  
})


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="list", filename = "character"), function(x, filename, ...) {
  
  # check input list and get layer names
  for(list.element in x) {
     if(!is.numeric(list.element)) stop("Got a non-numeric item in the list of layers.  Failing!")
     dims <- dim(list.element)
     if(exists("last.dims")) {
       if(!identical(dims,last.dims)) stop("Arrays not the same size for each layer, can't make a netCDF like this!")
     }
     last.dims <- dims
  }
  
  layers <- names(x)
  if(length(layers) != length(x)) stop("Layers not correctly named.  The layer names are taken from the name of each element in the input list, so the elements must be named.")
  
  
  # MAKE DIMENSIONS
  all.dimnames <- dimnames(x[[1]])
  all.dims <- list()
  all.dims[["Lon"]] <- ncdim_def(name = "Lon", units = "degrees", vals = as.numeric(all.dimnames[[1]]), unlim=FALSE, create_dimvar=TRUE)
  all.dims[["Lat"]] <- ncdim_def(name = "Lat", units = "degrees", vals = as.numeric(all.dimnames[[2]]), unlim=FALSE, create_dimvar=TRUE)
  if(length(all.dimnames) > 2 ) all.dims[["Time"]] <- ncdim_def(name = "Time", units = "timeunit", vals = as.numeric(all.dimnames[[3]]), unlim=TRUE, create_dimvar=TRUE)
  

  # PREPARE THE VARIABLES, ONE FOR EACH LAYER
  all.vars <- list()
  for(layer in layers) {
    all.vars[[layer]] <- ncvar_def(name = layer, units = "temp.unit", dim = all.dims, longname = layer, -99999)  # standard
  }
  

  # MAKE THE NETCDF FILE
  if(verbose) print(paste("Creating the output file", filename))
  outfile <- nc_create(filename, all.vars, verbose=verbose)
  
  # PUT EACH VARIABLE INTO THE FILE
  for(layer in layers) {
    if(verbose) print(paste("Saving variable", layer, "to file",  filename, sep =" " ), quote=FALSE)
    ncvar_put(outfile, layer,  x[[layer]], start=NA, count=NA, verbose=verbose)
  }
  
  # STANDARD SPATIAL ATTRIBUTES
  outfile <- addStandardSpatialAttributes(outfile)
  
  # CLOSE
  nc_close(outfile)
 
  
})