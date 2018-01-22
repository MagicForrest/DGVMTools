
########### CONVERSION OF ABOVE-GROUND BIOMASS TO TOTAL CARBON ####################
#' Calculate total Carbon given above ground biomass.
#'
#' Equation from Baccini et al . 2012. 
#' Used when reading the original Baccini et al. 2012 and Avitabile et al. 2015 dataset
#' 
#' @param AGB Above ground biomass (carbon)
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
AGBtoTotalCarbon <- function(AGB){
  
  BGB <- 0.489 * AGB^0.89
  total.carbon <- (AGB + BGB) / 2
  return(total.carbon)
  
}