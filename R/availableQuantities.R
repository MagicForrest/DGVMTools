#' List available quantities
#' 
#' This function looks at the files on disk associated with a Source object (reminder, a Source representrsis a particular dataset or model run)
#' and determines what Quantities are available.  The data for these Quanties can be read by \code{getField()} 
#' 
#' @param source The DGVMTools::Source object for which the avaiable variables are to be determined.
#' @param names Logical, if TRUE (default) return the ids of the DGVMTools::Quantity objects, otherwise return the Quantity objects themselves. 
#' @param ...  Other arguments that are passed to the getField function for the specific Format. Currently this is only 'adgvm.scheme' (for the aDGVM Format) which can be 1 or 2.
#'
#' @details This exact behaviour of this function will depend on the Format of the Source object.  Different Format objects have different
#' implementations of \code{availableQuantites()} because the files and file structure of each Format are, by definition, different.  
#'
#' @return A list of the available Quantites. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
#' 
#' @examples
#' \donttest{
#'  
#' # Define an example Source
#' test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(name = "LPJ-GUESS", dir = test.dir,  format = GUESS)
#' 
#' # list the available Quantities as list of ids
#' availableQuantities(test.Source)
#' 
#' # list the available Quantities as list of Quantity objects
#' availableQuantities(test.Source, names = FALSE)
#' 
#' }
availableQuantities <- function(source, names = TRUE, ...) {
  
    return(source@format@availableQuantities(source, names = names, ...))
  
}
