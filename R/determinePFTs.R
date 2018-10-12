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

determinePFTs <- function(x, ...) {
  
  # Call the function for the relevant format type
  if(is.function(x@format@determinePFTs)) {
    return(x@format@determinePFTs(x, ...))
  }
  
  # else 
  else{
    warning("Malformed determinePFTs slot of the Format object. Returning the fill list of PFTs")
    return(x@default.pfts)
  }
  

}