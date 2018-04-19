#' Write methods
#' 
#' Simply writes a field to disk (using the saveRDS() command), with a specific filename, based on the Field's id.
#' 
#' @param x A Field
#' @param ... Other arguments, not currently used
#' @return The name of the file written out (probably not so useful, just want to return something)
#' @name write-methods
#' @rdname write-methods
#' @aliases write
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
if (!isGeneric("writeField")) {
  setGeneric("writeField", function(x) standardGeneric("writeField"))
}
#' must have columns named "Lon" and "Lat" otherwise these methods fail.  


#' @rdname write-methods
setMethod("writeField", signature(x="Field"), function(x) {
  
 file.name <- file.path(x@source@dir, paste(x@id, "DGVMField", sep = "."))
 saveRDS(x, file = file.name) 
 invisible(file.name)
  
})