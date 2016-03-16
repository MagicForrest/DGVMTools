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


# TODO Document all classes!

##############################################################################
########  GENERAL CLASSES ####################################################
##############################################################################


##### TemporalExtent - class to hold the start and end years of a time span (often an period of years over which to average)
#####            averaging period and a name to identify it
setClass("TemporalExtent",
         slots = c(id= "character",
                   name = "character",
                   start = "numeric",
                   end = "numeric"
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

##### Period - class to hold the metadata about a month, seasonal or annual time period
setClass("Period",
         slots = c(id = "character",
                   name = "character",
                   abbreviation = "character",
                   index = "numeric",
                   padded.index = "character",
                   contains = "character",
                   days = "numeric",
                   days.leap = "numeric"
         )
)


###############################################################################
########  LPJ-GUESS SPECIFIC CLASSES ##########################################
###############################################################################

##### PFT  - class to hold the data for an LPJ-GUESS PFT
setClass("PFT", 
         slots = c(id = "character",
                   name = "character",
                   lifeform = "character",
                   leafform = "character",
                   phenology = "character",
                   zone = "character",
                   colour = "character",
                   combine = "character"
         )
)


##### VegRunInfo - class to hold the metadata for an LPJ-GUESS run 



checkVegRun <- function(object){
        
  errors <- character()
  
  # Check model types is supported
  support.veg.models <- c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE", "aDGVM")
  
  if (!length(object@model) > 0) {
    msg <- "Error defining VegRun, you must define a model type!"
    errors <- c(errors, msg)
  }
  else if (!(object@model  %in% support.veg.models)) {
    msg <- paste("Unsupported model type", object@model, sep = " ")
    errors <- c(errors, msg)
  }
  
  # Check run.dir exists
  if (!file_test("-d", object@run.dir)) {
    msg <- paste("Run directory not found:", object@run.dir, sep = " ")
    errors <- c(errors, msg)
  }
  
  # Check id is sensible 
  if (!length(object@id) > 0 | object@id == "") {
    msg <- paste("Not a sensible run id:", object@id, sep = " ")
    errors <- c(errors, msg)
  }
  
  # Other things are set to sensible/empty defaults in defineVegRun()
  
  if (length(errors) == 0) TRUE else errors
  
}

setClass("VegRunInfo", 
         slots = c(id = "character",
                   model = "character",
                   pft.set = "list",
                   description = "character",
                   run.dir = "character",                              
                   driving.data = "character",
                   map.overlay = "ANY",
                   lonlat.offset = "numeric",
                   year.offset = "numeric",
                   tolerance = "numeric",
                   london.centre = "logical",
                   fill.col = "character", # not commonly needed, only for more complex run comparisons
                   line.col = "character", # # not commonly needed, only for more complex run comparisons
                   line.width = "numeric", # not commonly needed, only for more complex run comparisons
                   line.type = "numeric", # not commonly needed, only for more complex run comparisons
                   landuseSimulated = "logical"
         ),
         validity = checkVegRun
         
)

#' An S4 class to contain metadata and, optionally, data and benchmarks for a single vegetation run
#' 
#' A \code{VegRun} object contains the metadata concerning the an inherited \code{VegRunInfo} object
#'  and the actual model data run as \code{VegObjects} in a list in slot \code{objects} and comparisions to datasets 
#'  as \code{BiomeComparison} and \code{RasterComparison} in a list in slot \code{benchmarks}.
#' Such objects can be built by calls to \code{getVegObject()}, \code{getVegSpatial()}, \code{getVegTemporal()}, \code{calcNewVegObj}, \code{compareRunToSpatialDataset()}
#'  and \code{compareBiomes()} and saved to the \code{VegRun} using \code{addToVegRun()}. 
#'  
#' Slots can be accessed by user directly, but more easily and usefully by functions \code{XXXX}
#' 
#' @slot objects List of \code{VegObjects} saved in this run
#' @slot benchmarks List of benchmarks (\code{BiomeComparisons} and \code{RasterComparisons}) performed and saved for this run.
#' @exportClass VegRun
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("VegRun", 
         slots = c(objects = "list",
                   benchmarks = "list"
         ),
         prototype = c(objects = list(),
                      benchmarks = list()
         ),
         contains = "VegRunInfo"
         
)


##### VegQuant - class to hold the data for an LPJ quantity
setClass("VegQuant", 
         slots = c(id = "character",
                   short.string = "character",
                   full.string = "character",
                   type = "character",
                   units = "character",
                   colours = "function",
                   cuts = "numeric",
                   aggregate.method = "character"
         ),
         prototype = c(id = "UnknownID",
                       short.string = "UnknownID",
                       full.string = "UnknownString",
                       type = "UnknownType",
                       units = "-",
                       colours = tim.colors,
                       cuts = integer(0),
                       aggregate.method = "sum"
         )
         
         
)

setClass("VegObject", 
         slots = c(id = "character",
                   data = "data.table",
                   quant = "VegQuant",
                   spatial.extent = "SpatialExtent",
                   temporal.extent = "TemporalExtent",
                   is.site = "logical",
                   is.spatially.averaged = "logical",
                   is.temporally.averaged = "logical",
                   run = "VegRunInfo"
                   
         ),
         
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
                   temporal.extent = "TemporalExtent",
                   data = "ANY",
                   veg.quant = "VegQuant",
                   units = "character",
                   correction.raster = "RasterLayer"
         )
)

##### Temporal dataset - class to hold the data and metadata about temporal dataset
setClass("TemporalDataset",
         slots = c(id = "character",
                   name = "character",
                   abbreviation = "character",
                   temporal.extent = "TemporalExtent",
                   data = "ANY",
                   extent = "SpatialExtent",
                   veg.quant = "VegQuant",
                   units = "character"
         )
)


setClass("RasterComparison",
          slots = c(id = "character",
                    diff.raster = "RasterLayer", 
                    perc.diff.raster = "RasterLayer", 
                    data.raster = "RasterLayer", 
                    model.raster = "RasterLayer", 
                    R.squ = "numeric", 
                    P.cor = "numeric", 
                    RMSE = "numeric", 
                    mean.diff = "numeric", 
                    sd.diff = "numeric"
                    )
)






##########################################################################################
########  BIOME CLASSIFICATION AND COMPARISON CLASSES ####################################
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

setClass("BiomeComparison",
         slots = c(id = "character",
                   data.raster = "RasterLayer", 
                   model.raster = "RasterLayer", 
                   scheme = "BiomeClassification",
                   Kappa = "numeric", 
                   individual.Kappas = "numeric"  
         )
)



