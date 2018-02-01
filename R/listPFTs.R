#!/usr/bin/Rscript


#' Get PFTs present
#' 
#' @param x The Source object for which to determine the PFTs present
#' @param ...  Extra arguments to the format-specific functions that this function wraps around.
#' 
#' 
#' 
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

listPFTs <- function(x, ...) {
  
  # Call the function for the relevant format type
  
  # LPJ-GUESS or LPJ-GUESS SPITFIRE
  if(x@format == "LPJ-GUESS" || x@format == "LPJ-GUESS-SPITFIRE") {  return(listPFTs_LPJ(x, ...)) }
  
  # Else warn and return and empty list
  else{ 
    warning(paste("listPFTs() function not implemented yet for format", x@format, "so I am returning the original PFT list.",  sep = " "))
    
  }
  
  
}