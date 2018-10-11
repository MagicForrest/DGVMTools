#' List available quantities
#' 
#' This function looks at the files on disk associated with a source object and determines what variables are available
#' 
#' @param source The DGVMTools::Source object for which the avaiable variables are to be determined.
#' @param names Logical, if TRUE (default) return the ids of the DGVMTools::Quantity objects, otherwise return the Quantity objects themselves. 
#' @return A list of the available Quantites. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

availableQuantities <- function(source, names = TRUE) {
  
    return(source@format@availableQuantities(source, names = names))
  
}