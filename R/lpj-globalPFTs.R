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
### Master list of PFT ids, properties, preferred plot colours etc
###
### Author: Matthew Forrest 
### Date: 08/04/2013
### Version: v1.0
###
### 'Stable' version for Allan
###
###########################################################################

# now make a big list of the global PFTs 
global.PFTs <- list()

# BOREAL TREES

BNE <- new("PFT",
           id = "BNE",
           name = "Boreal Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "Boreal",
           colour = "darkblue",
           combine = "no"
           )

global.PFTs[[BNE@id]] <- BNE

BINE <- new("PFT",
           id = "BINE",
           name = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "Boreal",
           colour = "darkblue",
           combine = "BNE"
           )


global.PFTs[[BINE@id]] <- BINE

BNS <- new("PFT",
           id = "BNS",
           name = "Boreal Needleleaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Summergreen",
           zone = "Boreal",
           colour = "cornflowerblue",
           combine = "no"
           )

global.PFTs[[BNS@id]] <- BNS


BIBS <- new("PFT",
           id = "BIBS",
           name = "Boreal Shade-intolerant B/leaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           zone = "Boreal",
           colour = "cyan",
           combine = "no"
)

global.PFTs[[BIBS@id]] <- BIBS

IBS <- new("PFT",
           id = "IBS",
           name = "Shade-intolerant B/leaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           zone = "Temperate",
           colour = "chartreuse",
           combine = "no"
)

global.PFTs[[IBS@id]] <- IBS


# TEMPERATE TREES

TeBE <- new("PFT",
           id = "TeBE",
           name = "Temperate B/leaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Evergreen",
           zone = "Temperate",
           colour = "darkgreen",
           combine = "no"
           )

global.PFTs[[TeBE@id]] <- TeBE

TeNE <- new("PFT",
           id = "TeNE",
           name = "Temperate Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "Temperate",
           colour = "lightseagreen",
           combine = "no"
           )

global.PFTs[[TeNE@id]] <- TeNE


TeBS <- new("PFT",
           id = "TeBS",
           name = "Temperate Broadleaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           colour = "darkolivegreen3",
           zone = "Temperate",
           combine = "no"
           )

global.PFTs[[TeBS@id]] <- TeBS

TeIBS <- new("PFT",
            id = "TeIBS",
            name = "Temperate Shade-Intolerant B/leaved S/green Tree",
            lifeform = "Tree",
            leafform = "Broadleaved",
            phenology = "Summergreen",
            colour = "darkolivegreen3",
            zone = "Temperate",
            combine = "TeBS"
)

global.PFTs[[TeIBS@id]] <- TeIBS


# TROPICAL TREES

TrBE <- new("PFT",
           id = "TrBE",
           name = "Tropical Broadleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Evergreen",
           zone = "Tropical",
           colour = "orchid4",
           combine = "no"
           )

global.PFTs[[TrBE@id]] <- TrBE



TrIBE <- new("PFT",
           id = "TrIBE",
           name = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Evergreen",
           zone = "Tropical", 
           colour = "orchid4",
           combine = "TrBE"
           )
global.PFTs[[TrIBE@id]] <- TrIBE


TrBR <- new("PFT",
           id = "TrBR",
           name = "Tropical Broadleaved Raingreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Raingreen",
           zone = "Tropical",
           colour = "palevioletred",
           combine = "no"
           )

global.PFTs[[TrBR@id]] <- TrBR


TrTBR <- new("PFT",
             id = "TrTBR",
             name = "Tropical Broadleaved Raingreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Raingreen",
             zone = "Tropical",
             colour = "maroon",
             combine = "no"
)

global.PFTs[[TrTBR@id]] <- TrTBR



# GRASSES

C3G <- new("PFT",
           id = "C3G",
           name = "Boreal/Temperate Grass",
           lifeform = "Grass",
           leafform = "Broadleaved",
           phenology = "GrassPhenology",
           zone = "NA",
           colour = "lightgoldenrod1",
           combine = "no"
           )

global.PFTs[[C3G@id]] <- C3G

C4G <- new("PFT",
           id = "C4G",
           name = "Tropical Grass",
           lifeform = "Grass",
           leafform = "Broadleaved",
           phenology = "GrassPhenology",
           zone = "NA",
           colour = "sienna2",
           combine = "no"
           )

global.PFTs[[C4G@id]] <- C4G

# SHRUBS

BES <- new("PFT",
           id = "BES",
           name = "Boreal Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Evergreen",
           zone = "Boreal",
           colour = "darkred",
           combine = "no"
           )

global.PFTs[[BES@id]] <- BES

MRS <- new("PFT",
           id = "MRS",
           name = "Mediterranean Raingreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Raingreen",
           zone = "Mediterranean",
           colour = "red",
           combine = "no"
           )

global.PFTs[[MRS@id]] <- MRS


MESb <- new("PFT",
           id = "MESb",
           name = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Evergreen",
           zone = "Mediterranean",
           colour = "red",
           combine = "no"
           )

global.PFTs[[MESb@id]] <- MESb

MeES <- new("PFT",
           id = "MeES",
           name = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Evergreen",
           zone = "Mediterranean",
           colour = "red",
           combine = "no"
           )

global.PFTs[[MeES@id]] <- MeES



MRSb <- new("PFT",
           id = "MRSb",
           name = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Raingreen",
           zone = "Mediterranean",
           colour = "pink",
           combine = "no"
           )

global.PFTs[[MRSb@id]] <- MRSb

MeRS <- new("PFT",
           id = "MeRS",
           name = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Raingreen",
           zone = "Mediterranean",
           colour = "pink",
           combine = "no"
           )

global.PFTs[[MeRS@id]] <- MeRS


Total <- new("PFT",
           id = "Total",
           name = "Total",
           lifeform = "NA",
           leafform = "NA",
           phenology = "NA",
           zone = "NA", 
           colour = "Black",
           combine = "no"
           )

global.PFTs[[Total@id]] <- Total




TeESh <- new("PFT",
            id = "TeESh",
            name = "Temperate Evergreen Shrub",
            lifeform = "Shrub",
            leafform = "Any",
            phenology = "Evergreen",
            zone = "Temperate",
            colour = "red",
            combine = "no"
)

global.PFTs[[TeESh@id]] <- TeESh

TeRSh <- new("PFT",
             id = "TeRSh",
             name = "Temperate Raingreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Raingreen",
             zone = "Temperate",
             colour = "pink",
             combine = "no"
)

global.PFTs[[TeRSh@id]] <- TeRSh

TeSSh <- new("PFT",
             id = "TeSSh",
             name = "Temperate Summergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Summergreen",
             zone = "Temperate",
             colour = "maroon1",
             combine = "no"
)

global.PFTs[[TeSSh@id]] <- TeSSh



BESh <- new("PFT",
             id = "BESh",
             name = "Boreal Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Boreal",
             colour = "lightsteelblue4",
             combine = "no"
)

global.PFTs[[BESh@id]] <- BESh

BSSh <- new("PFT",
             id = "BSSh",
             name = "Boreal Summergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Summergreen",
             zone = "Boreal",
             colour = "lightsteelblue1",
             combine = "no"
)


global.PFTs[[BSSh@id]] <- BSSh



TrESh <- new("PFT",
             id = "TrESh",
             name = "Tropical Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Tropical",
             colour = "saddlebrown",
             combine = "no"
)

global.PFTs[[TrESh@id]] <- TrESh

TrRSh <- new("PFT",
             id = "TrRSh",
             name = "Tropical Raingreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Raingreen",
             zone = "Tropical",
             colour = "navajowhite3",
             combine = "no"
)


global.PFTs[[TrRSh@id]] <- TrRSh


Bare <- new("PFT",
             id = "Bare",
             name = "Bare ground",
             lifeform = "NA",
             leafform = "NA",
             phenology = "NA",
             zone = "NA",
             colour = "grey50",
             combine = "no"
)


global.PFTs[[Bare@id]] <- Bare



