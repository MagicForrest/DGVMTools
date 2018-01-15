#' Resample methods
#' 
#' Methods for resampling Fields and Data objects to other resolutions based on Raster* objects or netCDF files on disk 
#' (especially useful for guassian grids with irregularly spaced latitudes)
#' 
#' @param x A Field or DataObject to be resample
#' @param y A raster define the rather grid for the filename
#' @param method The resampling method, can be "bil" or "nn" for raster.
#' @param filename If supplied, the resulting resampled raster is written to disk. 
#' @param ... Other arguments, not currently used
#' @return A vector of character strings of the names of the layers
#' @name resample
#' @rdname resample
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
NULL
#' @rdname resample
setGeneric("resample.DGVM", function(x,y,method,filename) standardGeneric("resample.DGVM"))


#' @rdname resample
setMethod("resample.DGVM", signature(x="Field", y="Raster", method = "character", filename="character"), function(x,y,method,filename) {
  
  # first convert the x to be resample to a raster
  x.raster <- as.Raster(x)
  
  # second do the resampling
  x.resampled <- raster::resample(x.raster, y, method, filename)
  
  # finally resample and return
  return(x.resampled)
  
})

#' @rdname resample
setMethod("resample.DGVM", signature(x="DataObject", y="Raster", method = "character", filename="character"), function(x,y,method,filename) {
  
  # first convert the x to be resample to a raster
  x.raster <- as.Raster(x)
  
  # second do the resampling
  x.resampled <- raster::resample(x.raster, y, method, filename)
  
  # finally resample and return
  return(x.resampled)
  
})


#' @rdname resample
setMethod("resample.DGVM", signature(x="Field", y="character", method = "character", filename="character"), function(x,y,method,filename) {
  
  # write the data to be resampled to disk as a netCDF file
  
  # call the cdo command to do the regridding
  
  # read the resulting netCDF file and convert it to a data.table
  
  # finally put the new data in the Field and return
  return(x)
  
})