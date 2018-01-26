#!/usr/bin/Rscript


#' Get PFTs present
#' 
#' 
#' 
#' 
#' 
#' 


listPFTs <- function(x, ...) {
  
  # Call the function for the relevant format type
  
  # LPJ-GUESS or LPJ-GUESS SPITFIRE
  if(x@format == "LPJ-GUESS" || x@format == "LPJ-GUESS-SPITFIRE") {  return(listPFTs_LPJ(x, ...)) }
  
  # Else warn and return and empty list
  else{ 
    warning(paste("listPFTs() function not implemented yet for format", x@format, "so I am returning the original PFT list.",  sep = " "))
    
  }
  
  
}