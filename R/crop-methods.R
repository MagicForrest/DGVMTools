#' Crop methods
#' 
#' Methods for cropping DGVMTools objects (and data.tables) to each other, to raster and sp objects (and basically anything that can be coerced in a raster::Extent object). 
#' The data.table in the "data" slot of the DGVMTools object (or the stand-alone data.table), must have columns named "Lon" and "Lat" otherwise these methods fail.  Unpleasantly.  
#' 
#' @param x A Field or Comparison or a stand-alone data.table to be cropped
#' @param y Anything from which raster::Extent object can be derived
#' @param spatial.extent.id A character string to describe the spatial extent (y) required for meta-data consistency.
#' Must be provided when cropping \code{Fields} or \code{Comparisons}.
#' @param ... Other arguments, not currently used
#' @return A spatially cropped object
#' @name crop-methods
#' @rdname crop-methods
#' @aliases crop
#' @exportMethod crop
#' @importMethodsFrom raster crop
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
#' 
#' 
#' 
# first define (redfine) the generic
if (!isGeneric("crop")) {
  setGeneric("crop", function(x, y, ...) standardGeneric("crop"))
}


#######################################################################################
###### Methods for cropping Fields to a raster::Extent object ####################
#######################################################################################


#' @rdname crop-methods
setMethod("crop", signature(x="Field", y = "ANY"), function(x, y, spatial.extent.id = NULL, ...) {
  
  if(is.null(spatial.extent.id)){
    stop("When cropping a DGVMTools object, please provide a spatial.extent.id when cropping a DGVMTools object to maintain meta-data integrity.")
  }
  
  Lon = Lat = NULL
  
  # get raster::Extent for cropping
  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get an raster::Extent object from argument y')
  }
  methods::validObject(y)
 
  # crop the data
  dt <- x@data
  x@data <- dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]


  # adjust the meta-data to reflext the new cropped extent
  x@spatial.extent <- extent(x@data)
  x@spatial.extent.id <- paste0(spatial.extent.id)
  
  
  return(x)
  
})


#' @rdname crop-methods
setMethod("crop", signature(x="Comparison", y = "ANY"), function(x, y, spatial.extent.id = NULL, ...) {
 
  if(is.null(spatial.extent.id)){
    stop("When cropping a DGVMTools object, please provide a spatial.extent.id when cropping a DGVMTools object to maintain meta-data integrity.")
  }
   
  Lon = Lat = NULL
 
  # get raster::Extent for cropping
  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get a raster::Extent object from argument y')
  }
  methods::validObject(y)
  
  # crop the data
  dt <- x@data
  x@data <- dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]
 
 
  # adjust the meta-data to reflext the new cropped extent
  x@spatial.extent <- extent(x@data)
  x@spatial.extent.id <-paste0(spatial.extent.id)
  
  return(x)
  
})


#' @rdname crop-methods
setMethod("crop", signature(x="data.table", y = "ANY"), function(x, y) {

  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get a raster::Extent object from argument y')
  }
  methods::validObject(y)
  
  Lon = Lat = NULL

  x <- x[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]

  return(x)
  
})

