#' Crop methods
#' 
#' Methods for cropping DGVMTools objects (and data.tables) to each other, to raster and sp objects (and basically anything that can be coerced in a raster::Extent object). 
#' The data.table in the "data" slot of the DGVMTools object (or the stand-alone data.table), must have columns named "Lon" and "Lat" otherwise these methods fail.  Unpleasantly.  
#' 
#' @param x A ModelObject, DataObject or ComparisonLayer or a stand-alone data.table to be cropped
#' @param y Anything from which raster::Extent object can be derived
#' @param ... Other arguments, not currently used
#' @return A spatially cropped object
#' @name crop-methods
#' @rdname crop-methods
#' @aliases crop
#' @importFrom raster crop
#' @importMethodsFrom raster crop
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
#' 
#' 
#' 
# first define (redfine) the generic
if (!isGeneric("crop")) {
  setGeneric("crop", function(x, y, ...) standardGeneric("crop"))
}


#######################################################################################
###### Methods for cropping ModelObjectsto a raster::Extent object ####################
#######################################################################################


#' @rdname crop-methods
setMethod("crop", signature(x="ModelObject", y = "ANY"), function(x, y) {
  
  Lon = Lat = NULL
  
  original.y <- y
  
  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get an Extent object from argument y')
  }
  methods::validObject(y)
 
  
  # crop the data
  dt <- x@data
  x@data <- dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]

  # adjust the meta-data
  new.sp.ex <- x@spatial.extent
  #new.sp.ex@extent <- extent(x@data)
  new.sp.ex@xmin <- extent(x@data)@xmin
  new.sp.ex@xmax <- extent(x@data)@xmax
  new.sp.ex@ymin <- extent(x@data)@ymin
  new.sp.ex@ymax <- extent(x@data)@ymax
  

  # If y was a SpatialExtent object it has a handy name and id which can be used to indicate the spatial domain
  if(class(original.y)[1] == "SpatialExtent") {
    new.sp.ex@name <- original.y@name
    new.sp.ex@id <-original.y@id
  }
  # else simply indicate that this has been cropped from the original
  else {
    object.name <-  deparse(substitute(y))
    new.sp.ex@name <- paste0(new.sp.ex@name, " (cropped)")
    new.sp.ex@id <- paste0(new.sp.ex@id, ".cropped")
  }
    
  x@spatial.extent <- new.sp.ex
  
  return(x)
  
})


#' @rdname crop-methods
setMethod("crop", signature(x="DataObject", y = "ANY"), function(x, y) {
  
  Lon = Lat = NULL
  
  original.y <- y
  
  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get an Extent object from argument y')
  }
  methods::validObject(y)
  
  # crop the data
  dt <- x@data
  x@data <- dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]
  
  # adjust the meta-data
  new.sp.ex <- x@spatial.extent
  #new.sp.ex@extent <- extent(x@data)
  new.sp.ex@xmin <- extent(x@data)@xmin
  new.sp.ex@xmax <- extent(x@data)@xmax
  new.sp.ex@ymin <- extent(x@data)@ymin
  new.sp.ex@ymax <- extent(x@data)@ymax
  
  # If y was a SpatialExtent object it has a handy name and id which can be used to indicate the spatial domain
  if(class(original.y)[1] == "SpatialExtent") {
    new.sp.ex@name <- original.y@name
    new.sp.ex@id <-original.y@id
  }
  # else simply indicate that this has been cropped from the original
  else {
    new.sp.ex@name <- paste0(new.sp.ex@name, " (cropped)")
    new.sp.ex@id <- paste0(new.sp.ex@id, ".cropped")
  }
 
  x@spatial.extent <- new.sp.ex
  
  return(x)
  
})

#' @rdname crop-methods
setMethod("crop", signature(x="ComparisonLayer", y = "ANY"), function(x, y) {
  
  Lon = Lat = NULL
  
  original.y <- y
  
  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get an Extent object from argument y')
  }
  methods::validObject(y)
  
  # crop the data
  dt <- x@data
  x@data <- dt[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]
  
  # adjust the meta-data
  new.sp.ex <- x@spatial.extent
  #new.sp.ex@extent <- extent(x@data)
  new.sp.ex@xmin <- extent(x@data)@xmin
  new.sp.ex@xmax <- extent(x@data)@xmax
  new.sp.ex@ymin <- extent(x@data)@ymin
  new.sp.ex@ymax <- extent(x@data)@ymax
  
  # If y was a SpatialExtent object it has a handy name and id which can be used to indicate the spatial domain
  if(class(original.y)[1] == "SpatialExtent") {
    new.sp.ex@name <- original.y@name
    new.sp.ex@id <-original.y@id
  }
  # else simply indicate that this has been cropped from the original
  else {
    new.sp.ex@name <- paste0(new.sp.ex@name, " (cropped)")
    new.sp.ex@id <- paste0(new.sp.ex@id, ".cropped")
  }
  
  x@spatial.extent <- new.sp.ex
  
  return(x)
  
})

#' @rdname crop-methods
setMethod("crop", signature(x="data.table", y = "ANY"), function(x, y) {

  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get an Extent object from argument y')
  }
  methods::validObject(y)
  
  Lon = Lat = NULL

  x <- x[Lat < y@ymax & Lat > y@ymin & Lon < y@xmax & Lon > y@xmin,]

  return(x)
  
})


#' @rdname crop-methods
setMethod("crop", signature(x="SpatialExtent", y = "ANY"), function(x, y) {
  
  y <- try ( extent(y), silent=TRUE )
  if (class(y) == "try-error") {
    stop('Cannot get an Extent object from argument y')
  }
  methods::validObject(y)
  
  # xmin
  x@xmin <- max(x@xmin, y@xmin)
  x@xmax <- min(x@xmax, y@xmax)
  x@ymin <- max(x@ymin, y@ymin)
  x@ymax <- min(x@ymax, y@ymax)
  
  if(x@xmin > x@xmax) {warning("Cropped to create a non-physical extent (x)")} 
  if(x@ymin > x@ymax) {warning("Cropped to create a non-physical extent (y)")} 
  
  return(x)
  
})