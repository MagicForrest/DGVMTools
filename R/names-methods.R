#' Names methods
#' 
#' Methods for returning the names of the layers in a ModelObject or a DataObject.
#' 
#' @param x A ModelObject or DataObject 
#' @param ... Other arguments, not currently used
#' @return A vector of character strings of the names of the layers
#' @name names
#' @rdname names
#' @exportMethod 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
NULL

#' @rdname names 
setMethod("names", signature(x="ModelObject"), function(x) {

  # columns to be removed because they contain spatial and temporal information rather than actual data layers
  remove <- c("Lon", "Lat", "Year", "Month", "Day")
  
  # get all column names, remove the spatial and temporal and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% remove]
  return(col.names)
  
})

#' @rdname names
setMethod("names", signature(x="DataObject"), function(x) {
  
  # columns to be removed because they contain spatial and temporal information rather than actual data layers
  remove <- c("Lon", "Lat", "Year", "Month", "Day")
  
  # get all column names, remove the spatial and temporal and return
  col.names <- names(x@data)
  col.names <- col.names[!col.names %in% remove]
  return(col.names)
  
})
