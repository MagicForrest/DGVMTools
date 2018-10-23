#' Rename layers
#' 
#' Used to rename layers in a Field object.  
#'  
#' @param x The Field object whose layers should be renamed.
#' @param old When \code{new} is provided, character names or numeric positions of column names to change. When \code{new} is not provided, the new column names, which must be the same length as the number of columns. See examples. 
#' @param new Optional. New column names, must be the same length as columns provided to \code{old} argument. 
#'   
#' Syntax copied from data.table::setnames(), for which this function is essentially a wrapper.
#'   
#' @name renameLayers
#' @rdname renameLayers
#' @export
#' @return A Field (but not this is not strictly necessary since the Field object is changed in place)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'  
renameLayers <- function(x, old, new) {
  
  if(!is.Field(x)) {
    warning("Can't rename layers since argument is not a DGVMTools::Field object.  Returning x (unmodified)")
    invisible(x)
  }
  else {
    setnames(x@data, old, new)
  }
  
}
