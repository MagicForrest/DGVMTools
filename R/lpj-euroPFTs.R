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
euro.PFTs <- list()

# BOREAL TREES

euro.PFTs[["Abi_alb"]] <- new("PFT",
                              name = "Abi_alb",
                              longname = "Abies alba",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Temperate",
                              colour = "blue",
                              combine = "no"
)

euro.PFTs[["BES"]] <- new("PFT",
                          name = "BES",
                          longname = "Boreal Evergreen Shrub",
                          lifeform = "Shrub",
                          leafform = "Unspecified_Leafform",
                          phenology = "Evergreen",
                          zone = "Boreal",
                          colour = "cyan",
                          combine = "no"
)

euro.PFTs[["Bet_pen"]] <- new("PFT",
                              name = "Bet_pen",
                              longname = "Betula pendula",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "darkcyan",
                              combine = "no"
)

euro.PFTs[["Bet_pub"]] <- new("PFT",
                              name = "Bet_pub",
                              longname = "Betula pubescens",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Boreal",
                              colour = "lightskyblue1",
                              combine = "no"
)

euro.PFTs[["Car_bet"]] <- new("PFT",
                              name = "Car_bet",
                              longname = "Carpinus Betula",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "peru",
                              combine = "no"
)

euro.PFTs[["Cor_ave"]] <- new("PFT",
                              name = "Cor_ave",
                              longname = "Corylus avellana",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "saddlebrown",
                              combine = "no"
)

euro.PFTs[["Fag_syl"]] <- new("PFT",
                              name = "Fag_syl",
                              longname = "Fagus Sylvatica",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "darkgreen",
                              combine = "no"
)

euro.PFTs[["Fra_exc"]] <- new("PFT",
                              name = "Fra_exc",
                              longname = "Fraxinus excelsior",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "darkgray",
                              combine = "no"
)

euro.PFTs[["Jun_oxy"]] <- new("PFT",
                              name = "Jun_oxy",
                              longname = "Juniperus oxycedrus",
                              lifeform = "Shrub",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "darkseagreen4",
                              combine = "no"
)

euro.PFTs[["MRS"]] <- new("PFT",
                          name = "MRS",
                          longname = "Mediterranean Raingreen Shrub",
                          lifeform = "Shrub",
                          leafform = "Unspecified_Leafform",
                          phenology = "Raingreen",
                          zone = "Mediterranean",
                          colour = "pink",
                          combine = "no"
)

euro.PFTs[["Pic_abi"]] <- new("PFT",
                              name = "Pic_abi",
                              longname = "Picea abies",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Boreal",
                              colour = "darkslateblue",
                              combine = "no"
)

euro.PFTs[["Pic_sit"]] <- new("PFT",
                              name = "Pic_sit",
                              longname = "Picea sitchensis",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Boreal",
                              colour = "black",
                              combine = "no"
)

euro.PFTs[["Pin_syl"]] <- new("PFT",
                              name = "Pin_syl",
                              longname = "Pinus sylvestris",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Boreal",
                              colour = "darkorchid4",
                              combine = "no"
)

euro.PFTs[["Pin_hal"]] <- new("PFT",
                              name = "Pin_hal",
                              longname = "Pinus halepensis",
                              lifeform = "Tree",
                              leafform = "Needleleaved",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "orangered",
                              combine = "no"
)

euro.PFTs[["Pop_tre"]] <- new("PFT",
                              name = "Pop_tre",
                              longname = "Populus tremula",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "lightsteelblue3",
                              combine = "no"
)

euro.PFTs[["Que_coc"]] <- new("PFT",
                              name = "Que_coc",
                              longname = "Quercus coccifera",
                              lifeform = "Shrub",
                              leafform = "Unspecified_Leafform",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "magenta",
                              combine = "no"
)

euro.PFTs[["Que_ile"]] <- new("PFT",
                              name = "Que_ile",
                              longname = "Quercus ilex",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Evergreen",
                              zone = "Mediterranean",
                              colour = "violetred",
                              combine = "no"
)

euro.PFTs[["Que_pub"]] <- new("PFT",
                              name = "Que_pub",
                              longname = "Quercus pubescens",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Supra-mediterranean",
                              colour = "springgreen4",
                              combine = "no"
)

euro.PFTs[["Que_rob"]] <- new("PFT",
                              name = "Que_rob",
                              longname = "Quercus robur",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "seagreen3",
                              combine = "no"
)

euro.PFTs[["Til_cor"]] <- new("PFT",
                              name = "Til_cor",
                              longname = "Tilia cordata",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "limegreen",
                              combine = "no"
)

euro.PFTs[["Ulm_gla"]] <- new("PFT",
                              name = "Ulm_gla",
                              longname = "Ulmus glabra",
                              lifeform = "Tree",
                              leafform = "Broadleaved",
                              phenology = "Summergreen",
                              zone = "Temperate",
                              colour = "olivedrab",
                              combine = "no"
)




euro.PFTs[["C3_gr"]] <- new("PFT",
                          name = "C3_gr",
                          longname = "C3 herbacious",
                          lifeform = "Grass",
                          leafform = "Unspecified_Leafform",
                          phenology = "GrassPhenology",
                          zone = "Temperate-boreal",
                          colour = "lightgoldenrod1",
                          combine = "no"
)



euro.PFTs[["C4_gr"]] <- new("PFT",
                          name = "C4_gr",
                          longname = "C4 herbacious",
                          lifeform = "Grass",
                          leafform = "Unspecified_Leafform",
                          phenology = "GrassPhenology",
                          zone = "Tropical",
                          colour = "sienna2",
                          combine = "no"
)

euro.PFTs[["Bare"]] <- new("PFT",
            name = "Bare",
            longname = "Bare ground",
            lifeform = "NA",
            leafform = "NA",
            phenology = "NA",
            zone = "NA",
            colour = "grey50",
            combine = "no"
)


euro.PFTs[["Total"]] <- new("PFT",
    name = "Total",
    longname = "Total",
    lifeform = "NA",
    leafform = "NA",
    phenology = "NA",
    zone = "NA", 
    colour = "black",
    combine = "no"
)

