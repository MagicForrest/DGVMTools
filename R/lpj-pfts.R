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

# blahblah


# now make a big list of the global PFTs 
global.PFTs <- list(
  
  # BOREAL TREES
  
  BNE = new("PFT",
            id = "BNE",
            name = "Boreal Needleleaved Evergreen Tree",
            lifeform = "Tree",
            leafform = "Needleleaved",
            phenology = "Evergreen",
            zone = "Boreal",
            colour = "darkblue",
            combine = "no"
  ),
  
  BINE = new("PFT",
             id = "BINE",
             name = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
             lifeform = "Tree",
             leafform = "Needleleaved",
             phenology = "Evergreen",
             zone = "Boreal",
             colour = "darkblue",
             combine = "BNE"
  ),
  
  BNS = new("PFT",
            id = "BNS",
            name = "Boreal Needleleaved Summergreen Tree",
            lifeform = "Tree",
            leafform = "Needleleaved",
            phenology = "Summergreen",
            zone = "Boreal",
            colour = "cornflowerblue",
            combine = "no"
  ),
  
  BIBS = new("PFT",
             id = "BIBS",
             name = "Boreal Shade-intolerant B/leaved Summergreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Summergreen",
             zone = "Boreal",
             colour = "cyan",
             combine = "no"
  ),
  
  IBS = new("PFT",
            id = "IBS",
            name = "Shade-intolerant B/leaved Summergreen Tree",
            lifeform = "Tree",
            leafform = "Broadleaved",
            phenology = "Summergreen",
            zone = "Temperate",
            colour = "chartreuse",
            combine = "no"
  ),
  
  # TEMPERATE TREES
  
  TeBE = new("PFT",
             id = "TeBE",
             name = "Temperate B/leaved Evergreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Evergreen",
             zone = "Temperate",
             colour = "darkgreen",
             combine = "no"
  ),
  
  TeNE = new("PFT",
             id = "TeNE",
             name = "Temperate Needleleaved Evergreen Tree",
             lifeform = "Tree",
             leafform = "Needleleaved",
             phenology = "Evergreen",
             zone = "Temperate",
             colour = "lightseagreen",
             combine = "no"
  ),
  
  TeBS = new("PFT",
             id = "TeBS",
             name = "Temperate Broadleaved Summergreen Tree",
             lifeform = "Tree",
             leafform = "Broadleaved",
             phenology = "Summergreen",
             colour = "darkolivegreen3",
             zone = "Temperate",
             combine = "no"
  ),
  TeIBS = new("PFT",
              id = "TeIBS",
              name = "Temperate Shade-Intolerant B/leaved S/green Tree",
              lifeform = "Tree",
              leafform = "Broadleaved",
              phenology = "Summergreen",
              colour = "darkolivegreen3",
              zone = "Temperate",
              combine = "TeBS"
  ),
  
  # TROPICAL TREES
  
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
  
  
  TrIBE = new("PFT",
              id = "TrIBE",
              name = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
              lifeform = "Tree",
              leafform = "Broadleaved",
              phenology = "Evergreen",
              zone = "Tropical", 
              colour = "orchid4",
              combine = "TrBE"
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
  
  TrTBR = new("PFT",
              id = "TrTBR",
              name = "Tropical Broadleaved Raingreen Tree",
              lifeform = "Tree",
              leafform = "Broadleaved",
              phenology = "Raingreen",
              zone = "Tropical",
              colour = "maroon",
              combine = "no"
  ),
  
  # GRASSES
  
  C3G = new("PFT",
            id = "C3G",
            name = "Boreal/Temperate Grass",
            lifeform = "Grass",
            leafform = "Broadleaved",
            phenology = "GrassPhenology",
            zone = "NA",
            colour = "lightgoldenrod1",
            combine = "no"
  ),
  
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
  
  # SHRUBS
  
  BES = new("PFT",
            id = "BES",
            name = "Boreal Evergreen Shrub",
            lifeform = "Shrub",
            leafform = "Any",
            phenology = "Evergreen",
            zone = "Boreal",
            colour = "darkred",
            combine = "no"
  ),
  
  MRS = new("PFT",
            id = "MRS",
            name = "Mediterranean Raingreen Shrub",
            lifeform = "Shrub",
            leafform = "Any",
            phenology = "Raingreen",
            zone = "Mediterranean",
            colour = "red",
            combine = "no"
  ),
  
  
  MESb = new("PFT",
             id = "MESb",
             name = "Mediterranean Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Mediterranean",
             colour = "red",
             combine = "no"
  ),
  
  MeES = new("PFT",
             id = "MeES",
             name = "Mediterranean Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Mediterranean",
             colour = "red",
             combine = "no"
  ),
  
  MRSb = new("PFT",
             id = "MRSb",
             name = "Mediterranean Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Raingreen",
             zone = "Mediterranean",
             colour = "pink",
             combine = "no"
  ),
  
  MeRS = new("PFT",
             id = "MeRS",
             name = "Mediterranean Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Raingreen",
             zone = "Mediterranean",
             colour = "pink",
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
  
  TeESh = new("PFT",
              id = "TeESh",
              name = "Temperate Evergreen Shrub",
              lifeform = "Shrub",
              leafform = "Any",
              phenology = "Evergreen",
              zone = "Temperate",
              colour = "red",
              combine = "no"
  ),
  
  TeRSh = new("PFT",
              id = "TeRSh",
              name = "Temperate Raingreen Shrub",
              lifeform = "Shrub",
              leafform = "Any",
              phenology = "Raingreen",
              zone = "Temperate",
              colour = "pink",
              combine = "no"
  ),
  
  TeSSh = new("PFT",
              id = "TeSSh",
              name = "Temperate Summergreen Shrub",
              lifeform = "Shrub",
              leafform = "Any",
              phenology = "Summergreen",
              zone = "Temperate",
              colour = "maroon1",
              combine = "no"
  ),
  
  BESh = new("PFT",
             id = "BESh",
             name = "Boreal Evergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Evergreen",
             zone = "Boreal",
             colour = "lightsteelblue4",
             combine = "no"
  ),
  
  BSSh = new("PFT",
             id = "BSSh",
             name = "Boreal Summergreen Shrub",
             lifeform = "Shrub",
             leafform = "Any",
             phenology = "Summergreen",
             zone = "Boreal",
             colour = "lightsteelblue1",
             combine = "no"
  ),
  
  TrESh = new("PFT",
              id = "TrESh",
              name = "Tropical Evergreen Shrub",
              lifeform = "Shrub",
              leafform = "Any",
              phenology = "Evergreen",
              zone = "Tropical",
              colour = "saddlebrown",
              combine = "no"
  ),
  
  TrRSh = new("PFT",
              id = "TrRSh",
              name = "Tropical Raingreen Shrub",
              lifeform = "Shrub",
              leafform = "Any",
              phenology = "Raingreen",
              zone = "Tropical",
              colour = "navajowhite3",
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


# now make a big list of the european PFTs 
euro.PFTs <- list(
  
  # BOREAL TREES
  
  Abi_alb = new("PFT",
                id = "Abi_alb",
                name = "Abies alba",
                lifeform = "Tree",
                leafform = "Needleleaved",
                phenology = "Evergreen",
                zone = "Temperate",
                colour = "blue",
                combine = "no"
  ),
  
  BES = new("PFT",
            id = "BES",
            name = "Boreal Evergreen Shrub",
            lifeform = "Shrub",
            leafform = "Unspecified_Leafform",
            phenology = "Evergreen",
            zone = "Boreal",
            colour = "cyan",
            combine = "no"
  ),
  
  "Bet_pen" = new("PFT",
                  id = "Bet_pen",
                  name = "Betula pendula",
                  lifeform = "Tree",
                  leafform = "Broadleaved",
                  phenology = "Summergreen",
                  zone = "Temperate",
                  colour = "darkcyan",
                  combine = "no"
  ),
  
  Bet_pub = new("PFT",
                id = "Bet_pub",
                name = "Betula pubescens",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Boreal",
                colour = "lightskyblue1",
                combine = "no"
  ),
  
  Car_bet = new("PFT",
                id = "Car_bet",
                name = "Carpinus Betula",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "peru",
                combine = "no"
  ),
  
  Cor_ave = new("PFT",
                id = "Cor_ave",
                name = "Corylus avellana",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "saddlebrown",
                combine = "no"
  ),
  
  Fag_syl = new("PFT",
                id = "Fag_syl",
                name = "Fagus Sylvatica",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "darkgreen",
                combine = "no"
  ),
  
  Fra_exc = new("PFT",
                id = "Fra_exc",
                name = "Fraxinus excelsior",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "darkgray",
                combine = "no"
  ),
  
  Jun_oxy = new("PFT",
                id = "Jun_oxy",
                name = "Juniperus oxycedrus",
                lifeform = "Shrub",
                leafform = "Needleleaved",
                phenology = "Evergreen",
                zone = "Mediterranean",
                colour = "darkseagreen4",
                combine = "no"
  ),
  
  MRS = new("PFT",
            id = "MRS",
            name = "Mediterranean Raingreen Shrub",
            lifeform = "Shrub",
            leafform = "Unspecified_Leafform",
            phenology = "Raingreen",
            zone = "Mediterranean",
            colour = "pink",
            combine = "no"
  ),
  
  Pic_abi = new("PFT",
                id = "Pic_abi",
                name = "Picea abies",
                lifeform = "Tree",
                leafform = "Needleleaved",
                phenology = "Evergreen",
                zone = "Boreal",
                colour = "darkslateblue",
                combine = "no"
  ),
  
  Pic_sit = new("PFT",
                id = "Pic_sit",
                name = "Picea sitchensis",
                lifeform = "Tree",
                leafform = "Needleleaved",
                phenology = "Evergreen",
                zone = "Boreal",
                colour = "black",
                combine = "no"
  ),
  
  Pin_syl = new("PFT",
                id = "Pin_syl",
                name = "Pinus sylvestris",
                lifeform = "Tree",
                leafform = "Needleleaved",
                phenology = "Evergreen",
                zone = "Boreal",
                colour = "darkorchid4",
                combine = "no"
  ),
  
  Pin_hal = new("PFT",
                id = "Pin_hal",
                name = "Pinus halepensis",
                lifeform = "Tree",
                leafform = "Needleleaved",
                phenology = "Evergreen",
                zone = "Mediterranean",
                colour = "orangered",
                combine = "no"
  ),
  
  Pop_tre = new("PFT",
                id = "Pop_tre",
                name = "Populus tremula",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "lightsteelblue3",
                combine = "no"
  ),
  
  Que_coc = new("PFT",
                id = "Que_coc",
                name = "Quercus coccifera",
                lifeform = "Shrub",
                leafform = "Unspecified_Leafform",
                phenology = "Evergreen",
                zone = "Mediterranean",
                colour = "magenta",
                combine = "no"
  ),
  
  Que_ile = new("PFT",
                id = "Que_ile",
                name = "Quercus ilex",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Evergreen",
                zone = "Mediterranean",
                colour = "violetred",
                combine = "no"
  ),
  
  Que_pub = new("PFT",
                id = "Que_pub",
                name = "Quercus pubescens",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Supra-mediterranean",
                colour = "springgreen4",
                combine = "no"
  ),
  
  Que_rob = new("PFT",
                id = "Que_rob",
                name = "Quercus robur",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "seagreen3",
                combine = "no"
  ),
  
  Til_cor = new("PFT",
                id = "Til_cor",
                name = "Tilia cordata",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "limegreen",
                combine = "no"
  ),
  
  Ulm_gla = new("PFT",
                id = "Ulm_gla",
                name = "Ulmus glabra",
                lifeform = "Tree",
                leafform = "Broadleaved",
                phenology = "Summergreen",
                zone = "Temperate",
                colour = "olivedrab",
                combine = "no"
  ),
  
  C3_gr = new("PFT",
              id = "C3_gr",
              name = "C3 herbacious",
              lifeform = "Grass",
              leafform = "Unspecified_Leafform",
              phenology = "GrassPhenology",
              zone = "Temperate-boreal",
              colour = "lightgoldenrod1",
              combine = "no"
  ),
  
  C4_gr = new("PFT",
              id = "C4_gr",
              name = "C4 herbacious",
              lifeform = "Grass",
              leafform = "Unspecified_Leafform",
              phenology = "GrassPhenology",
              zone = "Tropical",
              colour = "sienna2",
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
  ),
  
  Total = new("PFT",
              id = "Total",
              name = "Total",
              lifeform = "NA",
              leafform = "NA",
              phenology = "NA",
              zone = "NA", 
              colour = "black",
              combine = "no"
  )
)


