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


#' Temporal extent for defining the duration of a model run and selecting time periods 
#' 
#' A simple S4 class to define a time period ie 1961-1990, but called here \code{TemporalExtent} to be analagous to \code{SpatialExtent} and distinct from the \code{Period} class (which contains months and seasons)
#' 
#' @slot id A unique character string to identify this particular time period.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the time period. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot start First year of the temporal extent (a year, eg 1961)
#' @slot end Last year of the temporal extent (a year, eg 1990)
#' @exportClass TemporalExtent
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("TemporalExtent",
         slots = c(id = "character",
                   name = "character",
                   start = "numeric",
                   end = "numeric"
         )
)



#' Spatial extent for defining the spatial area covered by model output or other spatial data.
#' 
#' A simple S4 class to define a spatial extent.
#' 
#' @slot id A unique character string to identify this particular spatial extent.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the spatial extent. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot extent Currently can be a raster \code{Extent} object, but should be expanded to include a raster mask (to define an irregularly shaped spatial extent), or a c(Lon,Lat) coordinate to define a site.
#' @exportClass SpatialExtent
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("SpatialExtent",
         slots = c(id = "character",
                   name = "character",
                   extent = "ANY"
         ),
         prototype= c(id = "Global",
                      name = "Global",
                      extent = extent(-180, 180, -90 ,90)
         )
)

#' Time periods - eg. a month or a season or a year.
#' 
#' A simple S4 class to define a month, a season or a year used for aggregating monthly values to seasonal or annual values and for making monthly plots with nice labels etc.
#' Since these are for the most part standard (ie. people commonly need the months, the seasons (DJF, MAM, JJA, SON) and annual) these are defined in simple list that might be commonly used and can be looped through.  These are:
#' 
#' \itemize{
#'   \item \code{months} which contains all the months.
#'   \item \code{seasons} which contains all the seasons
#'   \item \code{annual} which only contains only the annual period
#'   \item \code{all.periods} or just \code{periods} which contains all of the above
#' }
#'  
#' However other periods can be defined for specific growing seasons etc.
#' 
#' @slot id A unique character string to identify this particular time period.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the spatial extent. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot abbreviation Not really sure if this is necessary, think is it covered by the "id" slot
#' @slot index The index of the months of the year for this period.  For example, Jan has index = 1, DJF has index =c (12,1,2) and annual has index = c(1,2,3,4,5,6,7,8,9,10,11,12) 
#' @slot padded.index As slot \code{index} but zero-padded strings for use in when encoding/decoding file names.
#' @slot contains Holds the names of the other periods included in this period.  For example for Jan just contains "Jan", but DJF it contains c("Dec","Jan,"Feb)
#' @slot days Number of days in this time period during a normal (non-leap) year
#' @slot days.leap Number of days on this period during a leap year
#' 
#' @exportClass Period
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
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
########  VEGETATION SPECIFIC CLASSES #########################################
###############################################################################

#' Class to hold the metadata for a Plant Functional Type (PFT)
#' 
#' These will are defined in lists for the default PFTs (or common post-processing schemes in the case of aDGVM) for default model setups, but the user may well need to define their own 
#' 
#' @slot id A unique character string to identify this particular PFT.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the PFT. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot lifeform A string defining the lifeform of the PFT, typically either "Tree", "Grass" or "Shrub"
#' @slot leafform A string defining the leafform of the PFT, typically either "Broadleaved" or "Needleleaved"
#' @slot phenology A string defining the phenology of the PFT, typically "Evergreen", "Summergreen", "Raingreen" or "GrassPhenology"
#' @slot zone A string defining the climate zone of a PFT, typically "Boreal", "Temperate" or "Tropical" (could go crazy and also have "Mediterranean", for example)
#' @slot colour A string defining a preferred R colour to plot this PFT (for line graphs etc)
#' @slot combine A string defining the \code{id} of a PFT that this PFT should be combined with when combining PFT with similar functioning but different shade tolerance.  Used primarily with LPJ-GUESS.
#' @exportClass PFT
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
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


########### VegRunInfo - class to hold the metadata for an LPJ-GUESS run

#' Checks validity of a \code{VegRunInfo}.
#' 
#' Called internally as the validity slot of the \code{VegRunInfo}.  It checks that the essential slots are filled with sensible values ie a run.dir that
#' exists on the file system; a model type which is valid and an \code{id} that is a non-empty character string.  It doesn't check that this is alphanumeric, this would be a useful addition.
#' 
#' @param object The \code{VegRunInfo} object to check for vailidity.
#' @return Empty string if the essential slots are fine, a string containing an error message if not.
#'    
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

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



#' Class to hold the metadata for a vegetation model run
#' 
#' This class describes a vegetation run, including its location on disk, the model used, the PFT set used, an unique id and a description, offsets to apply to the longitudes and latitudes to make the co-rordinates gridcell centered and so on.
#' It is not primarily intended to be used by itself. Instead it is inherited by \code{VegRun} object (due to this inheritance the slots can be accessed directly)
#' and included in a \code{VegObject} in the \code{run} slot (not inherited, so needs to be access be \code{@@run}).
#' 
#' @slot id A unique character string to identify this particular model un.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot model A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE" or "aDGVM". (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @slot description A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @slot run.dir The location of this run on the file system (Mandatory)
#' @slot driving.data A character string identifying the climate or other data used to produce this model run
#' @slot map.overlay A character defining which map overlay to plot as standard on for this model run.  Can be:
#'  \itemize{
#'   \item \code{hires} for a full high resolution outline of all countires of the world 
#'   \item \code{lowres} for a low resolution outline of all countires of the world  (much faster to plot that \code{hires}, generally sufficient for global plot)
#'   \item \code{hires-continents} for a full high resolution outline of the land masses only
#'   \item \code{lowres-continents} for a low resolution outline of the land masses only
#' }
#' These data come from the \code{mapdata} package
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot tolerance The tolerance arguement when converting uneven spaced grids to regular rasters for plotting
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @slot line.col  A string to define an R colour used when plotting this runs as a line graph
#' @slot line.width A numeric to define the width of a line representing this model run
#' @slot line.type A numeric to define the line style representing this model run
#' @slot landuseSimulated If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @exportClass VegRunInfo
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
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
#'  and \code{compareBiomes()}, and saved to the \code{VegRun} using \code{addToVegRun()}. 
#'  
#' Slots can be accessed by user directly, but more easily and usefully by functions \code{XXXX}
#' 
#' @slot objects List of \code{VegObjects} saved in this run
#' @slot benchmarks List of benchmarks (\code{BiomeComparisons} and \code{RasterComparisons}) performed and saved for this run.
#' The following slots are inherited from \code{VegRunInfo}.
#' @slot id A unique character string to identify this particular model run.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot model A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE" or "aDGVM". (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @slot description A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @slot run.dir The location of this run on the file system (Mandatory)
#' @slot driving.data A character string identifying the climate or other data used to produce this model run
#' @slot map.overlay A character defining which map overlay to plot as standard on for this model run.  Can be:
#'  \itemize{
#'   \item \code{hires} for a full high resolution outline of all countires of the world 
#'   \item \code{lowres} for a low resolution outline of all countires of the world  (much faster to plot that \code{hires}, generally sufficient for global plot)
#'   \item \code{hires-continents} for a full high resolution outline of the land masses only
#'   \item \code{lowres-continents} for a low resolution outline of the land masses only
#' }
#' These data come from the \code{mapdata} package
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot tolerance The tolerance arguement when converting uneven spaced grids to regular rasters for plotting
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @slot line.col  A string to define an R colour used when plotting this runs as a line graph
#' @slot line.width A numeric to define the width of a line representing this model run
#' @slot line.type A numeric to define the line style representing this model run
#' @slot landuseSimulated If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
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



#' Class to hold the data for a vegetation quantity
#' 
#' Defines specific vegetation quantities like "lai" (LAI) or "mwcont_upper" (mean monthly water).  
#' Includes metadata about the quantity (the units it is measured in for exampel), values for default plot proprties (color scales, plot ranges) and the aggragating method - ie whether the quantity shoudd generally be summed (for example monthly burnt area or C mass per PFT) or averaged (for example monthly soil water content).
#' Note that the used will probably need to define their own, and modify these for their own analysis and plots
#' 
#' 
#' @slot id A unique character string to identify this particular vegetation quantity, should match with the name of a particular model output variable.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot short.string A short character string to refer to this quantity
#' @slot full.string A longer character string to provide a more complete description of this quantity
#' @slot type A character string defining if this quantity is defined per PFT ("PFT"), per month ("monthly"), or something else.  The first two have a specific meaning to RVCTools, but in principle the use can define anything.  
#' @slot units A character string defining the units this quantity is defined in.  Possibly formally link to udunits2?
#' @slot colours A fucntion that retutns a colour scale suited for plotting this quantity.
#' @slot cuts A numerical sequence defining the plot range and colour breakpoint when plotting the quantity
#' @slot aggregate.method A character string defining the default method for how to aggregate the quantity, either "sum" or "average"
#' @exportClass VegQuant
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
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



#' Contains one aspect of the model output, eg. LAI
#' 
#' A key class of the package.  A \code{VegObject} stores the data and metadata for one quantity that comes from a vegetation model run (including information about the run iself).
#' For example LAI (Leaf Area Index), or monthly evapotranspiration.  The data can be averaged spatially or temporally, or neither ot both, and manipulated and plotted by mayn funtions in this package.
#' 
#' Generally these are not created directly by the user, but rather by functions like \code{getVegObject}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A VegQuant object to define what output this VegObject contains
#' @slot spatial.extent A SpatialExtent object which describes the area covered by this VegObject.  Particularly useful if the data has been spatially averaged.
#' @slot temporal.extent A TemporalExtent object which describes the time periog covered by this VegObject.  Particularly useful if the data has been temporally averaged.
#' @slot is.site Set to TRUE is this VegObject describes a single site
#' @slot is.spatially.averaged Set to TRUE is this VegObject has been spatially averaged
#' @slot is.temporally.averaged Set to TRUE is this VegObject has been temporally averaged
#' @slot run A VegRunInfo object which contains the metadata about the run which this VegObject belongs too.
#' @exportClass VegObject
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
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

### MF: NOT CURRENTLY USED, COMMENTED OUT TO AVOID DOCUMENTING!

# ##### ClimDS - class to hold data about a climate dataset
# setClass("ClimDS", 
#          slots = c(id = "character",
#                    longname = "character",
#                    path = "character",
#                    filename = "character",
#                    first.year = "numeric",
#                    last.year = "numeric",
#                    type = "character",
#                    data = "Raster",
#                    map.overlay = "character",
#                    var.list = "list",
#                    kelvin = "logical",
#                    prec.mode = "character",
#                    colour = "character"
#          )
# )
# 
# ##### ClimVar - class to hold data about a climate variable
# setClass("ClimVar", 
#          slots = c(id = "character",
#                    varcode = "character",
#                    longname = "character",
#                    unit = "character",
#                    aggregate.method = "character",
#                    abs.clrs = "function",
#                    abs.intervals = "numeric", 
#                    anom.clrs = "function", 
#                    anom.intervals = "numeric",
#                    anom.type = "character",
#                    anom.min.val = "numeric",
#                    anom.crit.val = "numeric",
#                    data.type = "character"
#                    
#                    
#          )
# )

##########################################################################################
########  BENCHMARKING DATASET SPECIFIC CLASSES ##########################################
##########################################################################################

########## SPATIAL DATASET CLASS

#' Class to hold the data and metadata about spatial dataset
#' 
#' Gathers up the meta-data and a raster layer for one particular layer of spatial environmental data.  Not intended for model output, rather for a dataset like a above ground biomass, or MODIS tree cover.
#' 
#' @slot id A unique character string to identify this particular spatial dataset.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to provide a fuller description of this dataset
#' @slot temporal.extent A \code{TemporalExtent} oject to describe when this environmental data was measured.
#' @slot data The actual data.  The slot type is currently "ANY" but perhaps it should be restricted to a \code{RasterLayer}.  That is what the benchmarking code certainly expects
#' @slot veg.quant A \code{VegQuant} object to describe exactly what this dataset is
#' @slot units A character string defining the units in which this data is measured.  Probably obselete because of the units slot in the veg.quant slot
#' @slot correction.raster A \code{RasterLayer} to apply multiplicatively to correct model data before comparing to this dataset.  For example a land-use correction.
#' 
#' @exportClass SpatialDataset
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
setClass("SpatialDataset",
         slots = c(id = "character",
                   name = "character",
                   temporal.extent = "TemporalExtent",
                   data = "ANY",
                   veg.quant = "VegQuant",
                   units = "character",
                   correction.raster = "RasterLayer"
         )
)


########## TEMPORAL DATASET CLASS

#' Class to hold the data and metadata about temporal dataset
#' 
#' Gathers up the meta-data and data for one particular layer of temporal environmental data, for eample a burnt area times series a flux tower measurement.  Not intended for model output, rather for a dataset like a above ground biomass, or MODIS tree cover.
#' 
#' @slot id A unique character string to identify this particular spatial dataset.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to provide a fuller description of this dataset
#' @slot temporal.extent A \code{TemporalExtent} object to describe the length of the data times series
#' @slot data The actual data.  The slot type is currently "ANY" but perhaps it should be restricted to a \code{RasterLayer}.  That is what the benchmarking code certainly expects
#' @slot extent A \code{SpatialExtent} object define the spatial region over which this time series data applies.
#' @slot veg.quant A \code{VegQuant} object to describe exactly what this dataset is
#' @slot units A character string defining the units in which this data is measured.  Probably obselete because of the units slot in the veg.quant slot
#' 
#' @exportClass TemporalDataset
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
setClass("TemporalDataset",
         slots = c(id = "character",
                   name = "character",
                   temporal.extent = "TemporalExtent",
                   data = "ANY",
                   extent = "SpatialExtent",
                   veg.quant = "VegQuant",
                   units = "character"
         )
)


#' Result of comparing a model raster to a data raster
#' 
#' This class stores the rasters (model, data, difference and precentage difference) and the statistics (R^2 etc) resulting when two rasters are compared using \code{compareRunToSpatialDataset}
#' 
#' @slot id A unique character string to identify this particular raster compariosn.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot diff.raster A "RasterLayer" of the absolute difference (model - data)
#' @slot perc.diff.raster A "RasterLayer" of the absolute difference (model - data / data) * 100
#' @slot data.raster A "RasterLayer" holding the data
#' @slot model.raster A "RasterLayer" holding the mode
#' @slot R.squ The R squared between the model and the data
#' @slot P.cor The Pearsons product moment correlation between the model and the the data,
#' @slot RMSE The Root Mean Squared Error etween the model and the the data
#' @slot mean.diff The difference between the model and the the data (all gridcells)
#' @slot sd.diff The standard deviation of the difference between the model and the the data (all gridcells)
#' @exportClass RasterComparison
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
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


########## BIOME SCHEME
#
#' A biome scheme
#' 
#' This class stores the information about a biome scheme.  It describes what how the model output (in the form of a data.table) must be prepared, and then the rules which are used to classify the biomes.
#' 
#' This is ultimately not flexible enough for all conceivable biome schemes from all models and will need to be somehow generalised and expanded at some point.  This is most certainly a challenge for another day!
#' 
#' 
#' @slot id A unique character string to identify this particular biome scheme.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string that can be more descriptive.
#' @slot substitution A "data.frame" describing how the classifications should be substited in the data.  Eh?  Probably not appropriate here
#' @slot rules A function which is applied to every row of the data.table and describes the biome classification rules.
#' @slot combineShadeTolerance If TRUE, call combineShadeTolerance before doing the biome classification
#' @slot totals.needed List of vegetation totals needed to calculate biomes, for example c("Tree", "Grass")
#' @slot fraction.of.total List of vegetation fraction of totals needed to calculate biomes
#' @slot fraction.of.tree List of vegetation fraction of tree total needed to calculate biomes
#' @slot fraction.of.woody List of vegetation fraction of woody total needed to calculate biomes
#' @slot needGDD5 If TRUE the biome rules require GDD5 for the classification
#' @slot strings List of character strings containing the names of the biomes
#' @slot cols List of character strings containing the names of R colous for the biomes (ordering matching the string slot)
#' @slot data.reference Character string giving a reference where the data for this biome scheme comes from
#' @slot published.reference Character string giving a reference where this model output classification scheme was published
#' @exportClass BiomeScheme
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("BiomeScheme",
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

########### BIOME COMPARISON
# 
#' Result of comparing a modelled biome map to a data biome map
#' 
#' This class stores the rasters (model, data), the biome scheme used and the Cohen's Kappa statistics resulting when two rasters of with identical biome categorisations are compared using \code{compareBiomes}
#' 
#' @slot id A unique character string to identify this particular biome compariosn.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot data.raster A "RasterLayer" holding the data biome map
#' @slot model.raster A "RasterLayer" holding the model biome map
#' @slot scheme A "BiomeScheme" object that desrcibes the biome scheme used.
#' @slot Kappa The overall Cohen's Kappa obtained when comparing modelled biomes to data biomes.
#' @slot individual.Kappas The individual Cohen's Kappas for each biome obtained when comparing modelled to data biomes
#' @exportClass BiomeComparison
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
setClass("BiomeComparison",
         slots = c(id = "character",
                   data.raster = "RasterLayer", 
                   model.raster = "RasterLayer", 
                   scheme = "BiomeScheme",
                   Kappa = "numeric", 
                   individual.Kappas = "numeric"  
         )
)



