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
euro.PFTs <- list()

# BOREAL TREES

euro.PFTs[["Abi_alb"]] <- new("PFT",
                              id = "Abi_alb",
                              name = "Abies alba",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Temperate",
                              colour = "blue",
                              combine = "no"
)

euro.PFTs[["BES"]] <- new("PFT",
                          id = "BES",
                          name = "Boreal Evergreen Shrub",
                          lifeform = "Shrub",
                          leafform = "Unspecified_Leafform",
                          phenology = "Evergreen",
                          zone = "Boreal",
                          colour = "cyan",
                          combine = "no"
)

euro.PFTs[["Bet_pen"]] <- new("PFT",
                              id = "Bet_pen",
                              name = "Betula pendula",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "darkcyan",
                              combine = "no"
)

euro.PFTs[["Bet_pub"]] <- new("PFT",
                              id = "Bet_pub",
                              name = "Betula pubescens",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Boreal",
                              colour = "lightskyblue1",
                              combine = "no"
)

euro.PFTs[["Car_bet"]] <- new("PFT",
                              id = "Car_bet",
                              name = "Carpinus Betula",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "peru",
                              combine = "no"
)

euro.PFTs[["Cor_ave"]] <- new("PFT",
                              id = "Cor_ave",
                              name = "Corylus avellana",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "saddlebrown",
                              combine = "no"
)

euro.PFTs[["Fag_syl"]] <- new("PFT",
                              id = "Fag_syl",
                              name = "Fagus Sylvatica",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "darkgreen",
                              combine = "no"
)

euro.PFTs[["Fra_exc"]] <- new("PFT",
                              id = "Fra_exc",
                              name = "Fraxinus excelsior",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "darkgray",
                              combine = "no"
)

euro.PFTs[["Jun_oxy"]] <- new("PFT",
                              id = "Jun_oxy",
                              name = "Juniperus oxycedrus",
                              lifeform = "Shrub",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "darkseagreen4",
                              combine = "no"
)

euro.PFTs[["MRS"]] <- new("PFT",
                          id = "MRS",
                          name = "Mediterranean Raingreen Shrub",
                          lifeform = "Shrub",
                          leafform = "Unspecified_Leafform",
                          phenology = "Raingreen",
                          zone = "Mediterranean",
                          colour = "pink",
                          combine = "no"
)

euro.PFTs[["Pic_abi"]] <- new("PFT",
                              id = "Pic_abi",
                              name = "Picea abies",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Boreal",
                              colour = "darkslateblue",
                              combine = "no"
)

euro.PFTs[["Pic_sit"]] <- new("PFT",
                              id = "Pic_sit",
                              name = "Picea sitchensis",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Boreal",
                              colour = "black",
                              combine = "no"
)

euro.PFTs[["Pin_syl"]] <- new("PFT",
                              id = "Pin_syl",
                              name = "Pinus sylvestris",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Boreal",
                              colour = "darkorchid4",
                              combine = "no"
)

euro.PFTs[["Pin_hal"]] <- new("PFT",
                              id = "Pin_hal",
                              name = "Pinus halepensis",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "orangered",
                              combine = "no"
)

euro.PFTs[["Pop_tre"]] <- new("PFT",
                              id = "Pop_tre",
                              name = "Populus tremula",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "lightsteelblue3",
                              combine = "no"
)

euro.PFTs[["Que_coc"]] <- new("PFT",
                              id = "Que_coc",
                              name = "Quercus coccifera",
                              lifeform = "Shrub",
                              leafform = "Unspecified_Leafform",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "magenta",
                              combine = "no"
)

euro.PFTs[["Que_ile"]] <- new("PFT",
                              id = "Que_ile",
                              name = "Quercus ilex",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "violetred",
                              combine = "no"
)

euro.PFTs[["Que_pub"]] <- new("PFT",
                              id = "Que_pub",
                              name = "Quercus pubescens",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Supra-mediterranean",
                              colour = "springgreen4",
                              combine = "no"
)

euro.PFTs[["Que_rob"]] <- new("PFT",
                              id = "Que_rob",
                              name = "Quercus robur",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "seagreen3",
                              combine = "no"
)

euro.PFTs[["Til_cor"]] <- new("PFT",
                              id = "Til_cor",
                              name = "Tilia cordata",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "limegreen",
                              combine = "no"
)

euro.PFTs[["Ulm_gla"]] <- new("PFT",
                              id = "Ulm_gla",
                              name = "Ulmus glabra",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "olivedrab",
                              combine = "no"
)




euro.PFTs[["C3_gr"]] <- new("PFT",
                          id = "C3_gr",
                          name = "C3 herbacious",
                          lifeform = "Grass",
                          leafform = "Unspecified_Leafform",
                          phenology = "GrassPhenology",
                          zone = "Temperate-boreal",
                          colour = "lightgoldenrod1",
                          combine = "no"
)



euro.PFTs[["C4_gr"]] <- new("PFT",
                          id = "C4_gr",
                          name = "C4 herbacious",
                          lifeform = "Grass",
                          leafform = "Unspecified_Leafform",
                          phenology = "GrassPhenology",
                          zone = "Tropical",
                          colour = "sienna2",
                          combine = "no"
)

euro.PFTs[["Bare"]] <- new("PFT",
            id = "Bare",
            name = "Bare ground",
            lifeform = "NA",
            leafform = "NA",
            phenology = "NA",
            zone = "NA",
            colour = "grey50",
            combine = "no"
)


euro.PFTs[["Total"]] <- new("PFT",
    id = "Total",
    name = "Total",
    lifeform = "NA",
    leafform = "NA",
    phenology = "NA",
    zone = "NA", 
    colour = "black",
    combine = "no"
)

