#!/usr/bin/Rscript


###############################################
########### aDGVM PFTS ########################
###############################################

#' aDGVM PFTs (under development)
#' 
#' The list contains PFTs which could be used to classify individuals coming from the aDGVM 
#' 
#' @format A list of \code{PFT} objects that store meta-data for PFTs classified from aDGVM model output.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aDGVM.PFTs <- list(
  
  C4G = new("PFT",
            id = "C4G",
            name = "Tropical Grass",
            lifeform = "Grass",
            leafform = "Broadleaved",
            phenology = "GrassPhenology",
            zone = "NA",
            colour = "sienna2",
            combine = "no"
  ),
    
  Tr= new("PFT",
          id = "Tr",
          name = "Tropical Tree",
          lifeform = "Tree",
          leafform = "Broadleaved",
          phenology = "Evergreen",
          zone = "Tropical",
          colour = "palevioletred",
          combine = "no"
  ),
  
  TrBE = new("PFT",
             id = "TrBE",
             name = "Tropical Broadleaved Evergreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Evergreen",
             zone = "Tropical",
             colour = "orchid4",
             combine = "no"
  ),
  
  TrBR = new("PFT",
             id = "TrBR",
             name = "Tropical Broadleaved Raingreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Raingreen",
             zone = "Tropical",
             colour = "palevioletred",
             combine = "no"
  ),
  
  
  Total = new("PFT",
              id = "Total",
              name = "Total",
              lifeform = "NA",
              leafform = "NA",
              phenology = "NA",
              zone = "NA", 
              colour = "Black",
              combine = "no"
  ),
  
  Bare = new("PFT",
             id = "Bare",
             name = "Bare ground",
             lifeform = "NA",
             leafform = "NA",
             phenology = "NA",
             zone = "NA",
             colour = "grey50",
             combine = "no"
  )
  
)






