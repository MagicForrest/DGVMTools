#' List available quantities
#' 
#' This function looks at the files on disk associated with a source object and determines what variables are available
#' 
#' @param source The DGVMTools::Source object for which the avaiable variables are to be determined.
#' @param names Logical, if TRUE (default) return the ids of the DGVMTools::Quantity objects, otherwise return the Quantity objects themselves. 
#' @param ...  Other arguments that are passed to the getField function for the specific Format.Currently this is only 'adgvm.scheme' (for the aDGVM Format) which can be 1 or 2.
#' @return A list of the available Quantites. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

availableQuantities <- function(source, names = TRUE, ...) {
  
    return(source@format@availableQuantities(source, names = names, ...))
  
}
