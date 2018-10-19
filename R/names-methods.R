#' Layer name methods
#' 
#' Methods for returning the names of the layers in a Field or a Comparison.  Note, for changing the names of layers see \code{renameLayers}.
#' 
#' @param x A Field or Comparison.
#' 
#' @details For convenience both \code{names()} and \code{layers()} are defined, but they both give the same result.  There is a logical equivalency here since 
#' layer names in a field are actually the column names in underlying data.table.  
#' AS such,  \code{layers()} is more consistent with the terminology and framework of DGVMTools, whereas \code{names()} is more consistent with the data.table approach.  
#' But, in the end, both of these functions return exactly the same thing via exactly the same method.
#' 
#' @return A vector of character strings of the names of the layers
#' @name layer-names-methods
#' @rdname names
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @exportMethod names
#' @exportMethod layers
NULL

#' @rdname names 
setMethod("names", signature(x="Field"), function(x) {

  # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% getDimInfo(x)]
  return(col.names)
  
})


#' @rdname names
setMethod("names", signature(x="Comparison"), function(x) {
  
   # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% getDimInfo(x)]
  return(col.names)
  
})

# first define (redefine) the generic
#' @rdname names 
#' @name layers
#' @exportMethod layers
if (!isGeneric("layers")) {
  setGeneric("layers", function(x) standardGeneric("layers"))
}


#' @rdname names 
setMethod("layers", signature(x="Field"), function(x) {
  
  # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% getDimInfo(x)]
  return(col.names)
  
})


#' @rdname names
setMethod("layers", signature(x="Comparison"), function(x) {
  
  # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% getDimInfo(x)]
  return(col.names)
  
})

#' @rdname names
setMethod("layers", signature(x="data.table"), function(x) {
  
  # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x)
  col.names <- col.names[!col.names %in% getDimInfo(x)]
  return(col.names)
  
})
