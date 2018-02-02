
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

#' @describeIn is.equal Checks two Quantity objects for equality
setMethod("is.equal", signature("Quantity", "Quantity"), function(a, b) {
  if (a@type==b@type && a@units==b@units && a@aggregate.method==b@aggregate.method)
    return(TRUE)
  return(FALSE)
})

