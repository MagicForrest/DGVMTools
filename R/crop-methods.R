#' Crop methods
#' 
#' Methods for cropping DGVMTools objects (and data.tables) to each other, to raster and sp objects (and basically anything that can be coerced in a raster::Extent object). 
#' The data.table in the "data" slot of the DGVMTools object (or the stand-alone data.table), must have columns named "Lon" and "Lat" otherwise these methods fail.  Unpleasantly.  
#' 
#' @param x A Field or Comparison or a stand-alone data.table to be cropped
#' @param y Anything from which raster::Extent object can be derived
#' @param spatial.extent.id A character string to describe the spatial extent (y) required for meta-data consistency.
#' Must be provided when cropping \code{\linkS4class{Field}} or \code{\linkS4class{Comparison}} objects
#' @param ... Other arguments, not currently used
#' @return A spatially cropped object
#' @name crop-methods
#' @rdname crop-methods
#' @aliases crop
#' @exportMethod crop
#' @importMethodsFrom raster crop
#' @importMethodsFrom terra ext
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
#' 
#' 
#' 
# first define (redfine) the generic
if (!isGeneric("crop")) {
  setGeneric("crop", function(x, y, ...) standardGeneric("crop"))
}


###################################################################################################################################
######### Methods for cropping Fields to a raster::Extent object (or anything from which one may be derived) ######################
###################################################################################################################################

#' Get an extent from anything
#' 
#' Internal function to try to get a raster::Extent object from any potential y argument to a \code{crop} command.
#' 
#' @param y Something to crop to  (ie the \code{y} argument of a crop command)
#' 
#' TODO One day this should be converted to terra::SpatExtent.  Probably whenever raster is really deprecated and shall be purged from DGVMTools.
#' 
#' @return A raster::Extent object
#' @keywords internal
#' 
extractRasterExtent <- function(y) {
  
  # get raster::Extent for cropping
  y_extent <- try ( raster::extent(y), silent=TRUE )
  if (inherits(y_extent, "try-error")) {
    y_SpatExtent <- try ( terra::ext(y), silent=TRUE ) 
    if(!inherits(y_SpatExtent, "try-error")){
      y_extent <- raster::extent(as.vector(y_SpatExtent)) 
    }
    else {
      stop('Cannot get an raster::Extent object or terra::SpatExtent from argument y')
    }
  }
  methods::validObject(y_extent)
  return(y_extent)
  
}


#' @rdname crop-methods
setMethod("crop", signature(x="Field", y = "ANY"), function(x, y, spatial.extent.id = NULL, ...) {
  
  if(is.null(spatial.extent.id)){
    stop("When cropping a DGVMTools object, please provide a spatial.extent.id when cropping a DGVMTools object to maintain meta-data integrity.")
  }
  
  Lon = Lat = NULL
  
  # get raster::Extent for cropping
  y <- extractRasterExtent(y)
 
  # crop the data and set the key 
  dt <- x@data
  x@data <- setKeyDGVM(dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,])


  # adjust the meta-data to reflext the new cropped extent
  x@spatial.extent <- extent(y)
  x@spatial.extent.id <- paste0(spatial.extent.id)
  x@id <- makeFieldID(x)
  
  return(x)
  
})


#' @rdname crop-methods
setMethod("crop", signature(x="Comparison", y = "ANY"), function(x, y, spatial.extent.id = NULL, ...) {
 
  if(is.null(spatial.extent.id)){
    stop("When cropping a DGVMTools object, please provide a spatial.extent.id when cropping a DGVMTools object to maintain meta-data integrity.")
  }
   
  Lon = Lat = NULL
 
  # get raster::Extent for cropping
  y <- extractRasterExtent(y)
  
  # crop the data and set the key
  dt <- x@data
  x@data <- setKeyDGVM(dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,])
 
 
  # adjust the meta-data to reflext the new cropped extent
  x@spatial.extent <- extent(y)
  x@spatial.extent.id <-paste0(spatial.extent.id)
  x@id <- makeFieldID(x)
  
  return(x)
  
})


#' @rdname crop-methods
setMethod("crop", signature(x="data.table", y = "ANY"), function(x, y, spatial.extent.id = NULL, ...) {
  
  # get raster::Extent for cropping
  y <- extractRasterExtent(y)
  
  Lon = Lat = NULL

  x <- setKeyDGVM(x[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,])

  return(x)
  
})

