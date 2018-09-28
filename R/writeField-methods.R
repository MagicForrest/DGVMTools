#' Write methods
#' 
#' Simply writes a field to disk (using the saveRDS() command), with a specific filename, based on the Field's id and the Source's directory.
#' 
#' @param x A Field
#' @param ... Other arguments, not currently used
#' @return The name of the file written out (probably not so useful, just want to return something)
#' @name writeField-methods
#' @rdname writeField-methods
#' @aliases writeField
#' @exportMethod writeField
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
if (!isGeneric("writeField")) {
  setGeneric("writeField", function(x) standardGeneric("writeField"))
}


#' @rdname writeField-methods
setMethod("writeField", signature(x="Field"), function(x) {
  
 file.name <- file.path(x@source@dir, paste(x@id, "DGVMField", sep = "."))
 saveRDS(x, file = file.name) 
 invisible(file.name)
  
})
