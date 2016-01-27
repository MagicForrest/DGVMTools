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

##############################################################################
########  GENERAL CLASSES ####################################################
##############################################################################


##### TimeSpan - class to hold the start and end years of a time span (often an period of years over which to average)
#####            averaging period and a name to identify it
setClass("TimeSpan",
         slots = c(name = "character",
                   start = "numeric",
                   end = "numeric"
         )
)

##### Period - class to hold the metadata about a month, seasonal or annual time period
setClass("Period",
         slots = c(name = "character",
                   abbreviation = "character",
                   index = "numeric",
                   padded.index = "character",
                   days = "numeric",
                   days.leap = "numeric"
         )
)
 
##### Spatial Extent - class to hold a spatial extent the start (often an area over which to average and study a time series,
#####                  for example Europe)

setClass("SpatialExtent",
         slots = c(id = "character",
                   name = "character",
                   extent = "Extent"
         ),
         prototype= c(id = "Global",
                      name = "Global",
                      extent = extent(-180, 180, -90 ,90)
         )
)



###############################################################################
########  LPJ-GUESS SPECIFIC CLASSES ##########################################
###############################################################################

##### PFT  - class to hold the data for an LPJ-GUESS PFT
setClass("PFT", 
         slots = c(name = "character",
                   longname = "character",
                   lifeform = "character",
                   leafform = "character",
                   phenology = "character",
                   zone = "character",
                   colour = "character",
                   combine = "character"
         )
)


##### VegRun - class to hold the metadata for an LPJ-GUESS run 



checkVegRun <- function(object){
  
  support.veg.models <- c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE", "aDGVM")
    
  errors <- character()
  if (!(object@model  %in% support.veg.models)) {
    msg <- paste("Unsupported model type", object@model, sep = " ")
    errors <- c(errors, msg)
  }
    
  if (length(errors) == 0) TRUE else errors
  
}


setClass("VegRun", 
         slots = c(id = "character",
                   model = "character",
                   pft.set = "list",
                   description = "character",
                   run.dir = "character",                              
                   driving.data = "character",
                   map.overlay = "character",
                   lonlat.offset = "numeric",
                   year.offset = "numeric",
                   tolerance = "numeric",
                   london.centre = "logical",
                   fill.col = "character", # not commonly needed, only for more complex run comparisons
                   line.col = "character", # # not commonly needed, only for more complex run comparisons
                   line.width = "numeric", # not commonly needed, only for more complex run comparisons
                   line.style = "numeric", # not commonly needed, only for more complex run comparisons
                   line.type = "numeric", # not commonly needed, only for more complex run comparisons
                   correct.for.landuse = "logical",
                   vars = "list" # not currently used
         ),
         prototype = c(model = "LPJ-GUESS",
                       pft.set = NULL,
                       description = "?",
                       run.dir = "r",                              
                       driving.data = "?",
                       map.overlay = "lowres",
                       lonlat.offset = c(0.0, 0.0),
                       year.offset = 0,
                       tolerance = 0.0000001,
                       london.centre = TRUE,
                       fill.col = "transparent",
                       line.col = "black",
                       line.width = 1,
                       line.style = 1,
                       correct.for.landuse = TRUE,
                       vars = list()
         ),
         validity = checkVegRun
         
)


##### VegQuant - class to hold the data for an LPJ quantity
setClass("VegQuant", 
         slots = c(id = "character",
                   short.string = "character",
                   full.string = "character",
                   type = "character",
                   units = "character",
                   colours = "function",
                   cuts = "numeric"
         ),
         prototype = c(id = "UnknownID",
                       short.string = "UnknownID",
                       full.string = "UnknownString",
                       type = "UnknownType",
                       units = "-",
                       colours = tim.colors,
                       cuts = integer(0)
         )
         
         
)



##### VegQuant - class to hold the data for an LPJ quantity
setClass("VegObj", 
         slots = c(id = "character",
                   data = "data.table",
                   time.span = "TimeSpan",
                   quant = "VegQuant",
                   run = "VegRun"
          ),
          contains = "VegRun"
      
)


############################################################################
########  CLIMATE DATASET CLASSES ##########################################
############################################################################


##### ClimDS - class to hold data about a climate dataset
setClass("ClimDS", 
         slots = c(id = "character",
                   longname = "character",
                   path = "character",
                   filename = "character",
                   first.year = "numeric",
                   last.year = "numeric",
                   type = "character",
                   data = "Raster",
                   map.overlay = "character",
                   var.list = "list",
                   kelvin = "logical",
                   prec.mode = "character",
                   colour = "character"
         )
)

##### ClimVar - class to hold data about a climate variable
setClass("ClimVar", 
         slots = c(id = "character",
                   varcode = "character",
                   longname = "character",
                   unit = "character",
                   aggregate.method = "character",
                   abs.clrs = "function",
                   abs.intervals = "numeric", 
                   anom.clrs = "function", 
                   anom.intervals = "numeric",
                   anom.type = "character",
                   anom.min.val = "numeric",
                   anom.crit.val = "numeric",
                   data.type = "character"
                   
                   
         )
)

##########################################################################################
########  BENCHMARKING DATASET SPECIFIC CLASSES ##########################################
##########################################################################################



##### Spatial dataset - class to hold the data and metadata about spatial dataset
setClass("SpatialDataset",
         slots = c(id = "character",
                   name = "character",
                   abbreviation = "character",
                   time.span = "TimeSpan",
                   data = "ANY",
                   veg.quant = "VegQuant",
                   units = "character"
         )
)

##### Temporal dataset - class to hold the data and metadata about temporal dataset
setClass("TemporalDataset",
         slots = c(id = "character",
                   name = "character",
                   abbreviation = "character",
                   time.span = "TimeSpan",
                   data = "ANY",
                   extent = "SpatialExtent",
                   veg.quant = "VegQuant",
                   units = "character"
         )
)

##########################################################################################
########  BIOME CLASSIFICATION #########################################################
##########################################################################################



##### Biome calssification - class to hold the data and metadata about biome classification
setClass("BiomeClassification",
         slots = c(id = "character",
                   name = "character", 
                   substitution = "data.frame",
                   rules = "function",
                   combineShadeTolerance = "logical",
                   totals.needed = "character",
                   fraction.of.total = "character",
                   fraction.of.tree = "character",
                   fraction.of.woody = "character",
                   needGDD5 = "logical",
                   strings = "character",
                   cols = "character",
                   data.reference = "character",
                   published.reference = "character"
         )
)



