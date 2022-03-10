## S4

# Raster

#' Generic method for coercing to raster
#' @name merge
#' @rdname merge
#' @exportMethod merge
setGeneric("merge", function(x, y, source = NULL, ...) {
  standardGeneric("merge")
})

#' @rdname merge
#' @export
setMethod("merge", signature("Field", "Field"),   function(x, y, ...) {
  
 return("Cheese")
  
})
