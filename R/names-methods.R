#' Names methods
#' 
#' Methods for returning the names of the layers in a Field or a DataObject.
#' 
#' @param x A Field or DataObject 
#' @param ... Other arguments, not currently used
#' @return A vector of character strings of the names of the layers
#' @name names-methods
#' @rdname names
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
NULL

#' @rdname names 
setMethod("names", signature(x="Field"), function(x) {

  # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% getSTInfo(x)]
  return(col.names)
  
})



#' @rdname names
setMethod("names", signature(x="ComparisonLayer"), function(x) {
  
   # get all column names, remove the spatial and temporal dimensions and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% getSTInfo(x)]
  return(col.names)
  
})
