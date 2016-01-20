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
global.PFTs <- list()

# BOREAL TREES

BNE <- new("PFT",
           name = "BNE",
           longname = "Boreal Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "Boreal",
           colour = "darkblue",
           combine = "no"
           )

global.PFTs[[BNE@name]] <- BNE

BINE <- new("PFT",
           name = "BINE",
           longname = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "Boreal",
           colour = "darkblue",
           combine = "BNE"
           )


global.PFTs[[BINE@name]] <- BINE

BNS <- new("PFT",
           name = "BNS",
           longname = "Boreal Needleleaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Summergreen",
           zone = "Boreal",
           colour = "cornflowerblue",
           combine = "no"
           )

global.PFTs[[BNS@name]] <- BNS


BIBS <- new("PFT",
           name = "BIBS",
           longname = "Boreal Shade-intolerant B/leaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           zone = "Boreal",
           colour = "cyan",
           combine = "no"
)

global.PFTs[[BIBS@name]] <- BIBS

IBS <- new("PFT",
           name = "IBS",
           longname = "Shade-intolerant B/leaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           zone = "Temperate",
           colour = "chartreuse",
           combine = "no"
)

global.PFTs[[IBS@name]] <- IBS


# TEMPERATE TREES

TeBE <- new("PFT",
           name = "TeBE",
           longname = "Temperate B/leaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Evergreen",
           zone = "Temperate",
           colour = "darkgreen",
           combine = "no"
           )

global.PFTs[[TeBE@name]] <- TeBE

TeNE <- new("PFT",
           name = "TeNE",
           longname = "Temperate Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "Temperate",
           colour = "lightseagreen",
           combine = "no"
           )

global.PFTs[[TeNE@name]] <- TeNE


TeBS <- new("PFT",
           name = "TeBS",
           longname = "Temperate Broadleaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           colour = "darkolivegreen3",
           zone = "Temperate",
           combine = "no"
           )

global.PFTs[[TeBS@name]] <- TeBS

TeIBS <- new("PFT",
            name = "TeIBS",
            longname = "Temperate Shade-Intolerant B/leaved S/green Tree",
            lifeform = "Tree",
            leafform = "Broadleaved",
            phenology = "Summergreen",
            colour = "darkolivegreen3",
            zone = "Temperate",
            combine = "TeBS"
)

global.PFTs[[TeIBS@name]] <- TeIBS


# TROPICAL TREES

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

global.PFTs[[TrBE@name]] <- TrBE



TrIBE <- new("PFT",
           name = "TrIBE",
           longname = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Evergreen",
           zone = "Tropical", 
           colour = "orchid4",
           combine = "TrBE"
           )
global.PFTs[[TrIBE@name]] <- TrIBE


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

global.PFTs[[TrBR@name]] <- TrBR


TrTBR <- new("PFT",
             name = "TrTBR",
             longname = "Tropical Broadleaved Raingreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Raingreen",
             zone = "Tropical",
             colour = "maroon",
             combine = "no"
)

global.PFTs[[TrTBR@name]] <- TrTBR



# GRASSES

C3G <- new("PFT",
           name = "C3G",
           longname = "Boreal/Temperate Grass",
           lifeform = "Grass",
           leafform = "Broadleaved",
           phenology = "Deciduous",
           zone = "NA",
           colour = "lightgoldenrod1",
           combine = "no"
           )

global.PFTs[[C3G@name]] <- C3G

C4G <- new("PFT",
           name = "C4G",
           longname = "Tropical Grass",
           lifeform = "Grass",
           leafform = "Broadleaved",
           phenology = "Deciduous",
           zone = "NA",
           colour = "sienna2",
           combine = "no"
           )

global.PFTs[[C4G@name]] <- C4G

# SHRUBS

BES <- new("PFT",
           name = "BES",
           longname = "Boreal Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Evergreen",
           zone = "Boreal",
           colour = "darkred",
           combine = "no"
           )

global.PFTs[[BES@name]] <- BES

MRS <- new("PFT",
           name = "MRS",
           longname = "Mediterranean Raingreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Raingreen",
           zone = "Mediterranean",
           colour = "red",
           combine = "no"
           )

global.PFTs[[MRS@name]] <- MRS


MESb <- new("PFT",
           name = "MESb",
           longname = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Evergreen",
           zone = "Mediterranean",
           colour = "red",
           combine = "no"
           )

global.PFTs[[MESb@name]] <- MESb

MeES <- new("PFT",
           name = "MeES",
           longname = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Evergreen",
           zone = "Mediterranean",
           colour = "red",
           combine = "no"
           )

global.PFTs[[MeES@name]] <- MeES



MRSb <- new("PFT",
           name = "MRSb",
           longname = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Raingreen",
           zone = "Mediterranean",
           colour = "pink",
           combine = "no"
           )

global.PFTs[[MRSb@name]] <- MRSb

MeRS <- new("PFT",
           name = "MeRS",
           longname = "Mediterranean Evergreen Shrub",
           lifeform = "Shrub",
           leafform = "Any",
           phenology = "Raingreen",
           zone = "Mediterranean",
           colour = "pink",
           combine = "no"
           )

global.PFTs[[MeRS@name]] <- MeRS


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

global.PFTs[[Total@name]] <- Total




TeESh <- new("PFT",
            name = "TeESh",
            longname = "Temperate Evergreen Shrub",
            lifeform = "Shrub",
            leafform = "Any",
            phenology = "Evergreen",
            zone = "Temperate",
            colour = "red",
            combine = "no"
)

global.PFTs[[TeESh@name]] <- TeESh

TeRSh <- new("PFT",
             name = "TeRSh",
             longname = "Temperate Raingreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Raingreen",
             zone = "Temperate",
             colour = "pink",
             combine = "no"
)

global.PFTs[[TeRSh@name]] <- TeRSh

TeSSh <- new("PFT",
             name = "TeSSh",
             longname = "Temperate Summergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Summergreen",
             zone = "Temperate",
             colour = "maroon1",
             combine = "no"
)

global.PFTs[[TeSSh@name]] <- TeSSh



BESh <- new("PFT",
             name = "BESh",
             longname = "Boreal Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Boreal",
             colour = "lightsteelblue4",
             combine = "no"
)

global.PFTs[[BESh@name]] <- BESh

BSSh <- new("PFT",
             name = "BSSh",
             longname = "Boreal Summergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Summergreen",
             zone = "Boreal",
             colour = "lightsteelblue1",
             combine = "no"
)


global.PFTs[[BSSh@name]] <- BSSh



TrESh <- new("PFT",
             name = "TrESh",
             longname = "Tropical Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Tropical",
             colour = "saddlebrown",
             combine = "no"
)

global.PFTs[[TrESh@name]] <- TrESh

TrRSh <- new("PFT",
             name = "TrRSh",
             longname = "Tropical Raingreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Raingreen",
             zone = "Tropical",
             colour = "navajowhite3",
             combine = "no"
)


global.PFTs[[TrRSh@name]] <- TrRSh


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


global.PFTs[[Bare@name]] <- Bare

PFT_MASTER_LIST <- global.PFTs

