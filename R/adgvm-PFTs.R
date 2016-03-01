#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
###################################### 

###########################################################################
###
### Master list of PFT names, properties, preferred plot colours etc
###
### Author: Matthew Forrest 
### Date: 08/04/2013
### Version: v1.0
###
### 'Stable' version for Allan
###
###########################################################################

# now make a big list of the global PFTs 
aDGVM.PFTs <- list()


C4G <- new("PFT",
           name = "C4G",
           longname = "Tropical Grass",
           lifeform = "Grass",
           leafform = "Broadleaved",
           phenology = "GrassPhenology",
           zone = "NA",
           colour = "sienna2",
           combine = "no"
)

aDGVM.PFTs[[C4G@name]] <- C4G


Tr <- new("PFT",
            name = "Tr",
            longname = "Tropical Tree",
            lifeform = "Tree",
            leafform = "Broadleaved",
            phenology = "Evergreen",
            zone = "Tropical",
            colour = "palevioletred",
            combine = "no"
)

aDGVM.PFTs[[Tr@name]] <- Tr

TrBE <- new("PFT",
            name = "TrBE",
            longname = "Tropical Broadleaved Evergreen Tree",
            lifeform = "Tree",
            leafform = "Broadleaved",
            phenology = "Evergreen",
            zone = "Tropical",
            colour = "orchid4",
            combine = "no"
)

aDGVM.PFTs[[TrBE@name]] <- TrBE


TrBR <- new("PFT",
            name = "TrBR",
            longname = "Tropical Broadleaved Raingreen Tree",
            lifeform = "Tree",
            leafform = "Broadleaved",
            phenology = "Raingreen",
            zone = "Tropical",
            colour = "palevioletred",
            combine = "no"
)

aDGVM.PFTs[[TrBR@name]] <- TrBR




Total <- new("PFT",
             name = "Total",
             longname = "Total",
             lifeform = "NA",
             leafform = "NA",
             phenology = "NA",
             zone = "NA", 
             colour = "Black",
             combine = "no"
)

aDGVM.PFTs[[Total@name]] <- Total


Bare <- new("PFT",
            name = "Bare",
            longname = "Bare ground",
            lifeform = "NA",
            leafform = "NA",
            phenology = "NA",
            zone = "NA",
            colour = "grey50",
            combine = "no"
)

aDGVM.PFTs[[Bare@name]] <- Bare







