#' Write methods
#' 
#' Simply writes a field to disk (using the saveRDS() command), with a specific filename, based on the Field's id.
#' 
#' @param x A Field
#' @param ... Other arguments, not currently used
#' @return The name of the file written out (probably not so useful, just want to return something)
#' @name writeField-methods
#' @rdname writeField-methods
#' @aliases writeField
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
if (!isGeneric("writeField")) {
  setGeneric("writeField", function(x) standardGeneric("writeField"))
}
#' must have columns named "Lon" and "Lat" otherwise these methods fail.  


#' @rdname writeField-methods
setMethod("writeField", signature(x="Field"), function(x) {
  
 file.name <- file.path(x@source@dir, paste(x@id, "DGVMField", sep = "."))
 saveRDS(x, file = file.name) 
 invisible(file.name)
  
})