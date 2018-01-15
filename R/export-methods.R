# Methods to coerce Fields and DataObjects to other R objects

#' Coerce from Fields and DataObjects
#' 
#' Functions to coerce Fields and DataObjects into other objects (data.frame, data.table, RasterBrick, SpatialPixelsDataFrame)
#' 
#' Note that for coercing to a Raster* object (RasterLayer or RasterBrick) the function is called "as.Raster" (capital "R") to avoid conflict with  
#' another function in the raster package called "as.raster"
#'
#'
#' @param x A Field or a DataObject
#' @param keep.rownames	If ... is a matrix or data.frame, TRUE will retain the rownames of that object in a column named rn.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column names (to syntactic names: see make.names) is optional. 
#' Note that all of R's base package as.data.frame() methods use optional only for column names treatment, 
#' basically with the meaning of data.frame(*, check.names = !optional).
#' @param ...	Just as ... in data.frame. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.
#' @name export-methods
#' @importMethodsFrom base as.data.frame
#' @importMethodsFrom data.table as.data.frame
#' @importMethodsFrom sp as.data.frame
#' @importMethodsFrom raster as.data.frame
#' 
#' 
#' 
NULL


###############    data.frame

#' @name export-methods
#' @export
setAs("Field", "data.frame", function(from) as.data.frame(from@data))


#' @rdname export-methods
#' @method as.data.frame Field
#' @export
as.data.frame.Field = function(x, row.names, optional, ...) as(x, "data.frame") 


#' @name export-methods
#' @export
setAs("ComparisonLayer", "data.frame", function(from) as.data.frame(from@data))


#' @rdname export-methods
#' @method as.data.frame ComparisonLayer
#' @export
as.data.frame.ComparisonLayer = function(x, row.names, optional, ...) as(x, "data.frame") 

#############  data.table

#' @name export-methods
#' @export
setAs("Field", "data.table", function(from) from@data)

#' @rdname export-methods
#' @export
as.data.table.Field = function(x, keep.rownames, ...) as(x, "data.table") 

#' @name export-methods
#' @export
setAs("ComparisonLayer", "data.table", function(from) from@data)

#' @rdname export-methods
#' @export
as.data.table.ComparisonLayer = function(x, keep.rownames, ...) as(x, "data.table") 


############# raster

#' @name export-methods
#' @export
setAs("Field", "Raster", function(from) promoteToRaster(from@data))

#' @name export-methods
#' @export
setAs("ComparisonLayer", "Raster", function(from) promoteToRaster(from@data))



#' @name as.Raster
#' @rdname export-methods
#' @export
#' @exportMethod as.Raster
#' Generic method for coercing to raster
setGeneric("as.Raster", function(x) {
  standardGeneric("as.Raster")
})

#' @rdname export-methods
#' @export
#' @exportMethod as.Raster
setMethod("as.Raster", signature("Field"),   function(x) promoteToRaster(x@data))

#' @rdname export-methods
#' @export
#' @exportMethod as.Raster
setMethod("as.Raster", signature("ComparisonLayer"),   function(x) promoteToRaster(x@data))
