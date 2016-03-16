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
#####################################


veg.palette <- colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black"))



#' Global scope list of all LPJ-GUESS (and SPITFIRE) quantities defined as \code{VegQuant} object 
#' from the default LPJ-GUESS model config and a few extra.
#'
#' These quantities have sensible colour schemes, long names and cut ranges for standard global runs,
#' but they will likely need to be modified and new ones aded for a specific analysis.
#' Once the package is loaded this is just a standard R list so can be modified and extended as you wish.
#' 
#' @format A list of \code{VegQuant} objects that store meta-data for output variable commonly used from LPJ-GUESS (and LPJ-GUESS-SPITFIRE)
lpj.quantities <- list()


#' Function to get an LPJ-GUESS quantity based on a string
#' 
lookupVegQuantity <- function(quant.str, verbose = FALSE){
  
  # check if the quantity we are storing here is already in the list of pre-defined quantities (LAI, Cmass etc)
  # if not use the 'generic' quantity
  this.quantity <- lpj.quantities[[quant.str]]
  
  if(length(this.quantity) != 0) {
    if(verbose) message(paste("Found quantity ", this.quantity@id, sep = ""))
  }   
  else{
    warning(paste("Didn't find a defined VegQuant for", tolower(quant.str), "using generic values for colors, units etc.", sep = " "))
    this.quantity <- lpj.quantities[["generic"]]
  } 
  
  return(this.quantity)
  
}



####################
##### FRACTION #####
####################

# The is the implementation of a standard fraction quantity 
# It contains no useful information, but sets the colour scheme to a greyscale with 20 level


quant.name <- "fraction"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             type = "unknown",
                                             short.string = "Fraction",
                                             full.string = "Fraction",
                                             units = "",
                                             colours = colorRampPalette(c("grey85", "black")), 
                                             cuts = seq(0,1,0.05)
)


###################
##### GENERIC #####
###################

# The is the implementation of a 'generic' quantity in the LPJ-GUESS output
# It contains no useful information, but sets the colour scheme the standard tim.colors with 20 level

quant.name <- "generic"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "GenVar",
                                             full.string = "Generic Variable",
                                             type = "unknown",
                                             units = "",
                                             colours = tim.colors,
                                             cuts = integer(0)
                                             
)

quant.name <- "biomes"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "biomes",
                                             full.string = "biomes",
                                             type = "categorical",
                                             units = "",
                                             colours = tim.colors,
                                             cuts = integer(0)
                                             
)




#############################
##### LAI PER PFT (lai) #####
#############################

# Here define the the colours and limits

lai.palette = colorRampPalette(c("blue", "lightskyblue1", "palevioletred", "khaki1", "yellowgreen", "forestgreen", "saddlebrown","black" )) #this is a function which returns a list of colours
lai.diff.palette = colorRampPalette(c("green", "blue", "white", "red", "yellow")) #this is a function which returns a list of colours


quant.name <- "lai"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "lai",
                                             short.string = "LAI",
                                             full.string = "LAI",
                                             type = "PFT",
                                             units = "m^2/m^2",
                                             colours = lai.palette,
                                             cuts = seq(0,7, 0.01)
)

lpj.quantities[["mlai"]] <- new("VegQuant",
                                id = "mlai",
                                short.string = "mLAI",
                                full.string = "Monthly LAI",
                                type = "monthly",
                                units = "m^2/m^2",
                                colours = lai.palette,
                                cuts = seq(0,10, 0.2)
)

lpj.quantities[["mlai_grass"]] <- new("VegQuant",
                                      id = "mlai_grass",
                                      short.string = "mLAI_grass",
                                      full.string = "Monthly LAI of grasses",
                                      type = "monthly",
                                      units = "m^2/m^2",
                                      colours = lai.palette,
                                      cuts = seq(0,10, 0.2)
)

lpj.quantities[["lai.diff"]] <- new("VegQuant",
                                    id = "laidiff",
                                    short.string = "LAI.Diff",
                                    full.string = "LAI Difference",
                                    type = "annual",
                                    units = "m^2/m^2",
                                    colours = lai.diff.palette,
                                    cuts = seq(-1.5,1.5, 0.05)
)



#############################
##### FPC PER PFT (fpc) #####
##### VEGCOVER ############## 
#############################


quant.name <- "fpc"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "fpc",
                                             short.string = "FPC",
                                             full.string = "Foliar Projective Cover",
                                             type = "PFT",
                                             units = "m^2/m^2",
                                             colours = veg.palette,
                                             cuts = seq(0,1.5, 0.02)
)

quant.name <- "mfpc"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "mfpc",
                                             short.string = "mFPC",
                                             full.string = "Monthly Foliar Projective Cover",
                                             type = "Monthly",
                                             units = "m^2/m^2",
                                             colours = veg.palette,
                                             cuts = seq(0,1.5, 0.02)
)

quant.name <- "vegcover"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "vegcover",
                                             short.string = "vegcover",
                                             full.string = "Vegetation Cover",
                                             type = "",
                                             units = "m^2/m^2",
                                             colours = veg.palette,
                                             cuts = seq(0,1.0, 0.02)
)



#############################
##### ANNUAL GPP #####
#############################


quant.name <- "agpp"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "agpp",
                                             short.string = "aGPP",
                                             full.string = "Annual GPP",
                                             type = "PFT",
                                             units = "kgC/m2/year",
                                             colours = tim.colors,
                                             cuts = seq(0, 3, 0.05)
)


#######################################
##### CARBON MASS PER PFT (cmass) #####
#######################################

# Here define the the colours and limits
cmass.palette = colorRampPalette(c("lemonchiffon","peru", "forestgreen", "dodgerblue4", "orchid4", "hotpink", "red4"))
cmass.diff.palette = colorRampPalette(c("green","blue","white","red", "yellow")) #this is a function which returns a list of colours

quant.name <- "cmass"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "cmass",
                                             short.string = "CMass",
                                             full.string = "Vegetation Carbon Mass",
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = cmass.palette,
                                             cuts = seq(0,40,1)
)

quant.name <- "cmass.diff"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "CMass.Diff",
                                             full.string = "Carbon Mass Difference",
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = cmass.diff.palette,
                                             cuts = seq(-26,26,2)
)

quant.name <- "agb"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = "agb",
                                             short.string = "ABG",
                                             full.string = "Above Ground Biomass",
                                             type = "PFT",
                                             units = "tonnes/hectare",
                                             colours = cmass.palette,
                                             cuts = seq(0,800,10)
)
###############################
##### CARBON POOLS(cpool) #####
###############################

quant.name <- "cpool"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "CPool",
                                             full.string = "Carbon Pool",
                                             type = "pools",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0,70,1)
)



quant.name <- "litter_wood"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "WoodLitter",
                                             full.string = "Wood Litter",
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0,2,0.1)
)

quant.name <- "litter_leaf"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "LeafLitter",
                                             full.string = "Leaf Litter",
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0,1,0.002)
)

quant.name <- "litter_repr"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "ReproductiveLitter",
                                             full.string = "Reproductive Litter",
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0,1,0.05)
)

quant.name <- "fine_fuel"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "FineFuel",
                                             full.string = "Fine Fuel",
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0,1,0.05)
)

########################################################
##### MONTHLY CARBON FLUXES VARIABLES              #####
##### mnpp - monthly net primary produtcivity      #####
##### mgpp - monthly gross primay productivity     #####
##### mnee - monthly evaporation from soil         #####
##### mrh  - monthly heterophic respiration        #####
##### mra  - monthly autophic respiration          #####
########################################################


quant.name <- "mnpp"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mNPP",
                                             full.string = "Month NPP",
                                             type = "monthly",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(-0.1, 0.3, 0.01)
)

quant.name <- "mgpp"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mGPP",
                                             full.string = "Monthly GPP", 
                                             type = "monthly",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0, 0.4, 0.01)
)

quant.name <- "mnee"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mNEE",
                                             full.string = "Monthly NEE", 
                                             type = "monthly",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(-0.1, 0.2, 0.01)
)

quant.name <- "mrh"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mRh",
                                             full.string = "Monthly Heterotrophic Respiration", 
                                             type = "monthly",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0.0, 0.3, 0.005)
)

quant.name <- "mra"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mRa",
                                             full.string = "Monthly Autotrophic Respiration", 
                                             type = "monthly",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0.0, 0.3, 0.005)
)





#############################
##### ANNUAL NPP (anpp) #####
#############################

quant.name <- "anpp"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "aNPP",
                                             full.string = "Annual NPP", 
                                             type = "PFT",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(-0.5, 2, 0.1)
)

lpj.quantities[["anpp.diff"]] <- new("VegQuant",
                                     id = "anppdiff",
                                     short.string = "aNPP.Diff",
                                     full.string = "Difference in Annual NPP", 
                                     type = "annual",
                                     units = "kgC/m^2",
                                     colours = lai.diff.palette,
                                     cuts = seq(-0.05,0.05, 0.0005)
)

#################################
##### CARBON FLUXES (cflux) #####
#################################

quant.name <- "cflux"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "CFlux",
                                             full.string = "Carbon Flux", 
                                             type = "flux",
                                             units = "kgC/m^2/y",
                                             colours = tim.colors,
                                             cuts = seq(-3, 3, 0.1)
)


###############################
##### TREE DENSITY (dens) #####
###############################

quant.name <- "dens"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "Dens",
                                             full.string = "PFT Density", 
                                             type = "PFT",
                                             units = "indiv/m^2",
                                             colours = tim.colors,
                                             cuts = seq(0, 0.1, 0.002)
)

lpj.quantities[["speciesheights"]] <- new("VegQuant",
                                          id = "speciesheights",
                                          short.string = "SpeciesHeights",
                                          full.string = "PFT Average Heights", 
                                          type = "PFT",
                                          units = "m",
                                          colours = tim.colors,
                                          cuts = seq(0, 40, 1)
)




#########################################################
##### MONTHLY HYDROLOGICAL VARIABLES                #####
##### maet - monthly actual evapotranspiration      #####
##### mpet - monthly potential evapotranspiration   #####
##### mevap - monthly evaporation from soil         #####
##### mrunoff - monthly runoff                      #####
##### mintercep - monthly interception              #####
##### mwcont_upper - water content upper soil layer #####
##### mwcont_lower - water content lower soil layer #####
#########################################################

quant.name <- "maet"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mAET",
                                             full.string = "Monthly Actual Evapotranspiration", 
                                             type = "monthly",
                                             units = "mm/month",
                                             colours = tim.colors,
                                             cuts = seq(0, 200, 10)
)

quant.name <- "mpet"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mPET",
                                             full.string = "Monthly Potential Evapotranspiration", 
                                             type = "monthly",
                                             units = "mm/month",
                                             colours = tim.colors,
                                             cuts = seq(0, 300, 10)
)
quant.name <- "mevap"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mEvap",
                                             full.string = "Monthly Evaporation", 
                                             type = "monthly",
                                             units = "mm/month",
                                             colours = tim.colors,
                                             cuts = seq(0, 100, 2)
)

quant.name <- "mrunoff"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mRunoff",
                                             full.string = "Monthly Runoff", 
                                             type = "monthly",
                                             units = "mm/month",
                                             colours = tim.colors,
                                             cuts = seq(0, 500, 10)
)

quant.name <- "mintercep"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "mIntercep",
                                             full.string = "Monthly Interception", 
                                             type = "monthly",
                                             units = "mm/month",
                                             colours = tim.colors,
                                             cuts = seq(0, 50, 1)
)

lpj.quantities[["mwcont_upper"]] <- new("VegQuant",
                                        id = "mwcont_upper",
                                        short.string = "mWC_upper",
                                        full.string = "Monthly Upper Soil Layer WC", 
                                        type = "monthly",
                                        units = "",
                                        colours = rev.tim.colors,
                                        cuts = seq(0, 1, 0.02)
)

lpj.quantities[["mwcont_lower"]] <- new("VegQuant",
                                        id = "mwcont_lower",
                                        short.string = "mWC_lower",
                                        full.string = "Monthly Lower Soil Layer", 
                                        type = "monthly",
                                        units = "",
                                        colours = rev.tim.colors,
                                        cuts = seq(0, 1, 0.02)
)

lpj.quantities[["msnowpack"]] <- new("VegQuant",
                                        id = "msnowpack",
                                        short.string = "mSnowPack",
                                        full.string = "Monthly Snow Pack", 
                                        type = "monthly",
                                        units = "mm H20",
                                        colours = rev.tim.colors,
                                        cuts = seq(0, 5, 0.02)
)




#############################################
##### FIRE RETURN INTERVAL (firert) #####
#############################################

firert.palette  = colorRampPalette(c("red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3"))
fireseason.palette  = colorRampPalette(rev(c("red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3")))
firert.cuts <- c(0,3,6,12,25,50,100,200,400,800,1000)



quant.name <- "firert"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "FireRT",
                                             full.string = "Fire Return Interval", 
                                             type = "annual",
                                             units = "years",
                                             colours = firert.palette,
                                             cuts = firert.cuts
)

quant.name <- "fireseason"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "FireSL",
                                             full.string = "Fire Season Length", 
                                             type = "annual",
                                             units = "days",
                                             colours = fireseason.palette,
                                             cuts = seq(0, 365, 5)
)

quant.name <- "firesl"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "FireSL",
                                             full.string = "Fire Season Length", 
                                             type = "annual",
                                             units = "days",
                                             colours = fireseason.palette,
                                             cuts = seq(0, 365, 5)
)



quant.name <- "burntarea"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "BA",
                                             full.string = "Annual Area Burned", 
                                             type = "annual",
                                             units = "fraction of gridcell",
                                             colours = fireseason.palette,
                                             cuts = seq(0, 1.0, 0.01)
)


###########################################
##### TOTAL ANNUAL RUNOFF (tot_runff) #####
###########################################

lpj.quantities[["tot_runoff"]] <- new("VegQuant",
                                      id = "tot_runoff",
                                      short.string = "Tot_Runoff",
                                      full.string = "Annual Total Runoff", 
                                      type = "annual",
                                      units = "mm/year",
                                      colours = tim.colors,
                                      cuts = seq(0, 1000, 50)
)







##############################
##### BIOCLIMATIC LIMITS #####
##############################

quant.name <- "bioclim"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "BiomClim",
                                             full.string = "Bioclimatic Limit Variables",
                                             type = "irregular",
                                             units = "kgC/m^2",
                                             colours = tim.colors,
                                             cuts = seq(-1, 3, 0.1)
)



quant.name <- "gdd5"

lpj.quantities[[tolower(quant.name)]] <- new("VegQuant",
                                             id = quant.name,
                                             short.string = "GDD5",
                                             full.string = "Growing Degree Days (5deg C base)",
                                             type = "annual",
                                             units = "degree days",
                                             colours = tim.colors,
                                             cuts = seq(0, 5000, 100)
)





#############################################################################################
############################# SPITFIRE QUANTITES ############################################
#############################################################################################


# move to a separate file when finished

lpj.quantities[["mfirefrac"]] <- new("VegQuant",
                                     id = "mfirefrac",
                                     short.string = "mFireFrac",
                                     full.string = "Monthly Burned Area Fraction", 
                                     type = "monthly",
                                     units = "",
                                     colours = tim.colors,
                                     cuts = seq(0,1,0.05)
)

lpj.quantities[["mtau_l"]] <- new("VegQuant",
                                  id = "mtau_l",
                                  short.string = "mTau_l",
                                  full.string = "Monthly Residence Time", 
                                  type = "monthly",
                                  units = "mins",
                                  colours = tim.colors,
                                  cuts = seq(0,8,0.2)
)

lpj.quantities[["pyro_flux"]] <- new("VegQuant",
                                     id = "pyro_flux",
                                     short.string = "PyroFlux",
                                     full.string = "Pyrogenic Emmisions", 
                                     type = "irregular",
                                     units = "kg species/m^2",
                                     colours = tim.colors,
                                     cuts = seq(0,2000,20)
)




##########################
##### FIRE INTENSITY #####
##########################


mfireintens <- new("VegQuant",
                   id = "mfireintens",
                   short.string = "mFireIntensity",
                   full.string = "Monthly Fire Intensity", 
                   type = "monthly",
                   units = "kW/m^2",
                   colours = tim.colors,
                   cuts = seq(0, 5000, 50)
)

lpj.quantities[[tolower(mfireintens@id)]] <- mfireintens



##################################################
##### MONTHLY 1HR, 10HR, 100HS, 1000HR FUELS #####
##################################################


quant.name <- "mlivegrass_fuel"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "mlivegrass_Fuel",
                                              full.string = "Mean Monthly Live Grass Fuel", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)


quant.name <- "m1hr_fuel"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m1hr_Fuel",
                                              full.string = "Mean Monthly 1hr Fuel", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)

quant.name <- "m10hr_fuel"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m19hr_Fuel",
                                              full.string = "Mean Monthly 10hr Fuel", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)

quant.name <- "m100hr_fuel"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m100hr_Fuel",
                                              full.string = "Mean Monthly 100hr Fuel", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)
quant.name <- "m1000hr_fuel"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m1000hr_Fuel",
                                              full.string = "Mean Monthly 1000hr Fuel", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)




####################################################################
##### MONTHLY 1HR, 10HR, 100HS, 1000HR COMBUSTION COMPLETENESS #####
####################################################################


quant.name <- "mlivegrass_cc"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "mlivegrass_cc",
                                              full.string = "Mean Monthly Live Grass Combustion Completeness", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)


quant.name <- "m1hr_cc"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m1hr_cc",
                                              full.string = "Mean Monthly 1hr Combustion Completeness", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)

quant.name <- "m10hr_cc"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m19hr_cc",
                                              full.string = "Mean Monthly 10hr Combustion Completeness", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)

quant.name <- "m100hr_cc"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m100hr_cc",
                                              full.string = "Mean Monthly 100hr Combustion Completeness", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)
quant.name <- "m1000hr_cc"

lpj.quantities[[tolower(quant.name)]]  <- new("VegQuant",
                                              id = quant.name,
                                              short.string = "m1000hr_cc",
                                              full.string = "Mean Monthly 1000hr Combustion Completeness", 
                                              type = "monthly",
                                              units = "kgC/m^2",
                                              colours = tim.colors,
                                              cuts = seq(0,1,0.01)
)





############################
##### ANNUAL FIRE DAYS #####
############################

quant.name <- "afdays"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "aFireDays",
                                    full.string = "Fire Days per Year", 
                                    type = "annual",
                                    units = "days",
                                    colours = tim.colors,
                                    cuts = seq(0, 366, 12)
)



####################################
##### MONTHLY AVERAGE NESTEROV #####
####################################

quant.name <- "mavenest"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mNesterov",
                                    full.string = "Average Monthly Nesterov", 
                                    type = "monthly",
                                    units = "",
                                    colours = tim.colors,
                                    cuts = seq(0, 100000, 5000)
)


####################################
##### MONTHLY LITTER MOISTURES #####
####################################

quant.name <- "mdlm_livegrass"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mDLM_livegrass",
                                    full.string = "Monthly Litter Moisture of Live Grass", 
                                    type = "monthly",
                                    units = "",
                                    colours = tim.colors,
                                    cuts = seq(0, 1, 0.02)
)

quant.name <- "mdlm_deadfuel"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mDLM_deadfuel",
                                    full.string = "Monthly Litter Moisture of Dead Fuel", 
                                    type = "monthly",
                                    units = "",
                                    colours = tim.colors,
                                    cuts = seq(0, 1, 0.02)
)

#######################################
##### MONTHLY EFFECTIVE WINDSPEED #####
##### AND MONTHLY NESTEROV        #####
#######################################


lpj.quantities[["meff_wind"]] <- new("VegQuant",
                                     id = "meff_wind",
                                     short.string = "mEffWind",
                                     full.string = "Monthly Effective Windspeed", 
                                     type = "monthly",
                                     units = "m/min",
                                     colours = tim.colors,
                                     cuts = seq(0, 100, 0.2)
)


lpj.quantities[["mnfdi"]] <- new("VegQuant",
                                 id = "mnfdi",
                                 short.string = "mNFDI",
                                 full.string = "Monthly Nesterov Fire Danger Index", 
                                 type = "monthly",
                                 units = "m/min",
                                 colours = tim.colors,
                                 cuts = seq(0, 1, 0.02)
)


###################################
##### MONTHLY FUEL PROPERTIES #####
###################################


lpj.quantities[["mFBD"]] <- new("VegQuant",
                                id = "mFBD",
                                short.string = "mFBD",
                                full.string = "Monthly Fuel Bulk Density", 
                                type = "monthly",
                                units = "m^3/m^3",
                                colours = tim.colors,
                                cuts = seq(0, 30, 1)
)


lpj.quantities[["mSAV"]] <- new("VegQuant",
                                id = "mSAV",
                                short.string = "mSAV",
                                full.string = "Monthly Surface Area to Volume Ratio", 
                                type = "monthly",
                                units = "m^2/m^3",
                                colours = tim.colors,
                                cuts = seq(25, 100, 5)
)



lpj.quantities[["mMoE"]] <- new("VegQuant",
                                id = "mMoE",
                                short.string = "mMOE",
                                full.string = "Monthly Moisture of Extinction", 
                                type = "monthly",
                                units = "",
                                colours = tim.colors,
                                cuts = seq(0.2, 0.4, 0.01)
)


lpj.quantities[["mmcont"]] <- new("VegQuant",
                                  id = "mmcont",
                                  short.string = "mFMCont",
                                  full.string = "Monthly Fuel Moisture Content", 
                                  type = "monthly",
                                  units = "",
                                  colours = tim.colors,
                                  cuts = seq(0, 1, 0.05)
)





###################################
##### MONTHLY FIRE PROPERTIES #####
###################################

quant.name <- "mfire_durat"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mFire_Duration",
                                    full.string = "Monthly Fire Duration", 
                                    type = "monthly",
                                    units = "mins",
                                    colours = tim.colors,
                                    cuts = seq(0, 250, 5)
)

quant.name <- "mscorch_height"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mScorch_Height",
                                    full.string = "Monthly Scorch Height", 
                                    type = "monthly",
                                    units = "m",
                                    colours = tim.colors,
                                    cuts = seq(0, 30, 1)
)

lpj.quantities[["mros"]] <- new("VegQuant",
                                id = "mros",
                                short.string = "mROS",
                                full.string = "Monthly Rate of Spread", 
                                type = "monthly",
                                units = "m/min",
                                colours = tim.colors,
                                cuts = seq(0, 20, 1)
)

lpj.quantities[["mfire_size"]] <- new("VegQuant",
                                id = "mfire_size",
                                short.string = "mfire_size",
                                full.string = "Monthly Fire Size", 
                                type = "monthly",
                                units = "ha",
                                colours = tim.colors,
                                cuts = seq(0, 100, 1)
)

lpj.quantities[["mhuman_ign"]] <- new("VegQuant",
                                      id = "mhuman_ign",
                                      short.string = "mhuman_ign",
                                      full.string = "Monthly average of human ignition rate", 
                                      type = "monthly",
                                      units = "ign/day",
                                      colours = tim.colors,
                                      cuts = seq(0, 100, 1)
)

lpj.quantities[["mlightning_ign"]] <- new("VegQuant",
                                      id = "mlightning_ign",
                                      short.string = "mlightning_ign",
                                      full.string = "Monthly average of lightning ignition rate", 
                                      type = "monthly",
                                      units = "ign/day",
                                      colours = tim.colors,
                                      cuts = seq(0, 5, 0.1)
)


###################################
##### MONTHLY FIREDAY PROPERTIES #####
###################################


quant.name <- "mfireday_duration"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mFire_Duration_FireDays",
                                    full.string = "Monthly Fire Duration (Fire Days Only)", 
                                    type = "monthly",
                                    units = "mins",
                                    colours = tim.colors,
                                    cuts = seq(0, 250, 5)
)


quant.name <- "mfireday_scorch_height"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mScotch_Height_FireDays",
                                    full.string = "Monthly Scorch Height (Fire Days Only)", 
                                    type = "monthly",
                                    units = "m",
                                    colours = tim.colors,
                                    cuts = seq(0, 30, 1)
)


quant.name <- "mfireday_intensity"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mFire_Intensity_FireDays",
                                    full.string = "Monthly Fire Intensity (Fire Days Only)", 
                                    type = "monthly",
                                    units = "kW/m",
                                    colours = tim.colors,
                                    cuts = seq(0, 500000, 5000)
)

quant.name <- "mfireday_nesterov"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mNesterov_FireDays",
                                    full.string = "Monthly Nesterov (Fire Days Only)", 
                                    type = "monthly",
                                    units = "",
                                    colours = tim.colors,
                                    cuts = seq(0, 5000000, 5000)
)


quant.name <- "mfireday_residence_time"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = quant.name,
                                    short.string = "mResidence_Time_FireDays",
                                    full.string = "Monthly Residence Time (Fire Days Only)", 
                                    type = "monthly",
                                    units = "min",
                                    colours = tim.colors,
                                    cuts = seq(0, 10, 0.1)
)





#################################################
##### ALLOCATION FAILS AND ALLOCATION ITERS #####
#################################################

quant.name <- "allocation_fails"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = "allocation_fails",
                                    short.string = "mAllocationFails",
                                    full.string = "Monthly Allocation Fails", 
                                    type = "annual",
                                    units = "days",
                                    colours = tim.colors,
                                    cuts = seq(0,31,1)
)

quant.name <- "allocation_iters"

lpj.quantities[[quant.name]] <- new("VegQuant",
                                    id = "allocation_iters",
                                    short.string = "mAllocationIters",
                                    full.string = "Monthly Iteration To Allocation Fire", 
                                    type = "annual",
                                    units = "days",
                                    colours = tim.colors,
                                    cuts = seq(0,20,1)
)



