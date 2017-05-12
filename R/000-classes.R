#!/usr/bin/Rscript


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
#' 
#' @slot id A unique character string to identify this particular time period.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the spatial extent. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot abbreviation Not really sure if this is necessary, think is it covered by the "id" slot
#' @slot index The index of the months of the year for this period.  For example, Jan has index = 1, DJF has index =c (12,1,2) and annual has index = c(1,2,3,4,5,6,7,8,9,10,11,12) 
#' @slot padded.index As slot \code{index} but zero-padded strings for use in when encoding/decoding file names.
#' @slot contains Holds the names of the other periods included in this period.  For example for Jan just contains "Jan", but DJF it contains c("Dec","Jan,"Feb)
#' @slot days Number of days in this time period during a normal (non-leap) year
#' @slot days.leap Number of days on this period during a leap year
#' @slot col A colour (as a character string) for this particular period
#' 
#' @details 
#' Since these are for the most part standard (ie. people commonly need the months, the seasons (DJF, MAM, JJA, SON) and annual) these are defined in simple list that might be commonly used and can be looped through.  These are:
#' \describe{
#'   \item{\code{months}}{which contains all the months.}
#'   \item{\code{seasons}}{which contains all the seasons.}
#'   \item{\code{annual}}{which only contains only the annual period.}
#'   \item{\code{all.periods, periods}}{which contains all of the above.}
#' }
#'  
#' However other periods can be defined for specific growing seasons etc.
#' 
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
                   days.leap = "numeric",
                   col = "character"
         )
)


###############################################################################
########  VEGETATION SPECIFIC CLASSES #########################################
###############################################################################

#' Class to hold the metadata for a Plant Functional Type (PFT)
#' 
#' @description   This is a class to hold meta-data about PFTs.  As detailed in the 'Slots' section below, this includes an id (should be unique) and a name, as well as their lifeform, phenology, leaftype, climate zone etc, and a default plot colour.
#' These are defined in lists for the default PFTs for supported models (see below 'Usage' and 'Format' below) but the user may well need to define their own.  Such a list must be passed to a ModelRunInfo object
#' to define which PFTs might be in a run (but they don't all need to be present in a given run)   
#' 
#' @slot id A unique character string to identify this particular PFT.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the PFT. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot lifeform A string defining the lifeform of the PFT, typically either "Tree", "Grass" or "Shrub"
#' @slot leafform A string defining the leafform of the PFT, typically either "Broadleaved" or "Needleleaved"
#' @slot phenology A string defining the phenology of the PFT, typically "Evergreen", "Summergreen", "Raingreen" or "GrassPhenology"
#' @slot zone A string defining the climate zone of a PFT, typically "Boreal", "Temperate" or "Tropical" (could go crazy and also have "Mediterranean", for example)
#' @slot colour A string defining a preferred R colour to plot this PFT (for line graphs etc)
#' @slot combine A string defining the \code{id} of a PFT that this PFT should be combined with when combining PFT with similar functioning but different shade tolerance.  Used primarily with LPJ-GUESS.
#' 
#' @details The following PFT list are already defined for standard model output:
#' 
#' @name PFT-class
#' @rdname PFT-class
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

#' Supported Veg Models
#' 
#' List of supported vegetation models
#'  
#' @format A list of character strings for each of the supported vegetation models
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
supported.models <- c("LPJ-GUESS", 
                      "LPJ-GUESS-SPITFIRE", 
                      "aDGVM", 
                      "LPJ-GUESS-SPITFIRE-FireMIP",
                      "LPJ-GUESS-SIMFIRE-BLAZE-FireMIP",
                      "LPJ-GUESS-GlobFIRM-FireMIP",
                      "CLM-FireMIP",
                      "CTEM-FireMIP",
                      "JSBACH-FireMIP",
                      "Inferno-FireMIP",
                      "ORCHIDEE-FireMIP")



########### ModelRunInfo - class to hold the metadata for an LPJ-GUESS run

#' Checks validity of a \code{ModelRunInfo}.
#' 
#' Called internally as the validity slot of the \code{ModelRunInfo}.  It checks that the essential slots are filled with sensible values ie a run.dir that
#' exists on the file system; a model type which is valid and an \code{id} that is a non-empty character string.  It doesn't check that this is alphanumeric, this would be a useful addition.
#' 
#' @param object The \code{ModelRunInfo} object to check for vailidity.
#' @return Empty string if the essential slots are fine, a string containing an error message if not.
#' @keywords internal
#'    
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

checkModelRun <- function(object){
  
  errors <- character()
  
  # Check model types is supported
  if (!length(object@model) > 0) {
    msg <- "Error defining ModelRun, you must define a model type!"
    errors <- c(errors, msg)
  }
  else if (!(object@model  %in% supported.models)) {
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
  
  # Other things are set to sensible/empty defaults in defineModelRun()
  
  if (length(errors) == 0) TRUE else errors
  
}



#' Class to hold the metadata for a vegetation model run
#' 
#' This class describes a vegetation run, including its location on disk, the model used, the PFT set used, an unique id and a description, offsets to apply to the longitudes and latitudes to make the co-rordinates gridcell centered and so on.
#' It is not primarily intended to be used by itself. Instead it is inherited by \code{ModelRun} object (due to this inheritance the slots can be accessed directly)
#' and included in a \code{ModelObject} in the \code{run} slot (not inherited, so needs to be access be \code{@@run}).
#' 
#' @slot id A unique character string to identify this particular model un.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot model A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE" or "aDGVM". (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @slot name A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @slot run.dir The location of this run on the file system (Mandatory)
#' @slot driving.data A character string identifying the climate or other data used to produce this model run
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot tolerance The tolerance arguement when converting uneven spaced grids to regular rasters for plotting
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @slot line.col  A string to define an R colour used when plotting this runs as a line graph
#' @slot line.width A numeric to define the width of a line representing this model run
#' @slot line.type A numeric to define the line style representing this model run
#' @slot landuseSimulated If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @slot contact Name and email address of responsible person (default to OS username).
#' @slot institute Name of the institute (default "none").
#' @exportClass ModelRunInfo
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("ModelRunInfo", 
         slots = c(id = "character",
                   model = "character",
                   pft.set = "list",
                   name = "character",
                   run.dir = "character",                              
                   driving.data = "character",
                   lonlat.offset = "numeric",
                   year.offset = "numeric",
                   tolerance = "numeric",
                   london.centre = "logical",
                   fill.col = "character", # not commonly needed, only for more complex run comparisons
                   line.col = "character", # # not commonly needed, only for more complex run comparisons
                   line.width = "numeric", # not commonly needed, only for more complex run comparisons
                   line.type = "ANY", #numeric", # not commonly needed, only for more complex run comparisons
                   landuseSimulated = "logical",
                   contact = "character",
                   institute = "character"
         ),
         validity = checkModelRun
         
)


#' An S4 class to contain metadata and, optionally, data and benchmarks for a single vegetation run
#' 
#' A \code{ModelRun} object contains the metadata concerning the an inherited \code{ModelRunInfo} object
#'  and the actual model data run as \code{ModelObjects} in a list in slot \code{objects} and comparisions to datasets 
#'  as \code{BiomeComparison} and \code{RasterComparison} in a list in slot \code{benchmarks}.
#' Such objects can be built by calls to \code{getModelObject()}, \code{getVegSpatial()}, \code{getVegTemporal()}, \code{calcNewVegObj},
#'  and \code{compareBiomes()}, and saved to the \code{ModelRun} using \code{addToModelRun()}. 
#'  
#' Slots can be accessed by user directly, but more easily and usefully by functions \code{XXXX}
#' 
#' @slot objects List of \code{ModelObjects} saved in this run
#' @slot id A unique character string to identify this particular model run.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot model A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE", "FireMIP" or "aDGVM". (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @slot name A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @slot run.dir The location of this run on the file system (Mandatory)
#' @slot driving.data A character string identifying the climate or other data used to produce this model run
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot tolerance The tolerance arguement when converting uneven spaced grids to regular rasters for plotting
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @slot line.col  A string to define an R colour used when plotting this runs as a line graph
#' @slot line.width A numeric to define the width of a line representing this model run
#' @slot line.type A numeric to define the line style representing this model run
#' @slot landuseSimulated If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @exportClass ModelRun
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("ModelRun", 
         slots = c(objects = "list"
         ),
         prototype = c(objects = list()
         ),
         contains = "ModelRunInfo"
         
)



#' Class to hold meta-data for a vegetation quantity
#' 
#' This class hold meta-data for specific vegetation or land surface quantities like "lai" (LAI) or "mwcont_upper" (mean monthly water).  
#' Includes metadata about the quantity (the units it is measured in for example), values for default plot proprties (color scales, plot ranges) and the aggragating method - ie whether the quantity shoudd generally be summed (for example monthly burnt area or C mass per PFT) or averaged (for example monthly soil water content).
#' See the 'Slots' documentation below.
#' Note that the user will probably need to define their own, and modify these for their own analysis and plots
#' 
#' @slot id A unique character string to identify this particular vegetation quantity, should match with the name of a particular model output variable.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A longer character string to provide a more complete description of this quantity
#' @slot type A character string defining if this quantity is defined per PFT ("PFT"), per month ("monthly"), or something else.  The first two have a specific meaning to DGVMTools, but in principle the use can define anything.  
#' @slot units A character string defining the units this quantity is defined in.  Possibly formally link to udunits2?
#' @slot colours A function that returns a colour scale suited for plotting this quantity.
#' @slot aggregate.method A character string defining the default method for how to aggregate the quantity, either "sum" or "average"
#' @slot model Either a the string "Standard" to denote that this is a  standard quantity to be compared across all model and data, or vector of model names to denote to which models this Quantity is applicable.
#'
#' @importFrom fields tim.colors  
#' @exportClass Quantity
#' @name Quantity-class
#' @rdname Quantity-class
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Quantity", 
         slots = c(id = "character",
                   name = "character",
                   type = "character",
                   units = "character",
                   colours = "function",
                   aggregate.method = "character",
                   model = "character"
         ),
         prototype = c(id = "UnknownID",
                       name = "UnknownString",
                       type = "UnknownType",
                       units = "-",
                       colours = fields::tim.colors,
                       aggregate.method = "sum",
                       model = "Standard"
         )
         
         
)



#' Contains one aspect of the model output, eg. LAI
#' 
#' A key class of the package.  A \code{ModelObject} stores the data and metadata for one quantity that comes from a vegetation model run (including information about the run iself).
#' For example LAI (Leaf Area Index), or monthly evapotranspiration.  The data can be averaged spatially or temporally, or neither ot both, and manipulated and plotted by mayn funtions in this package.
#' 
#' Generally these are not created directly by the user, but rather by functions like \code{getModelObject}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A Quantity object to define what output this ModelObject contains
#' @slot spatial.extent A SpatialExtent object which describes the area covered by this ModelObject.  Particularly useful if the data has been spatially averaged.
#' @slot temporal.extent A TemporalExtent object which describes the time periog covered by this ModelObject.  Particularly useful if the data has been temporally averaged.
#' @slot is.site Set to TRUE is this ModelObject describes a single site
#' @slot is.spatially.averaged Set to TRUE is this ModelObject has been spatially averaged
#' @slot is.temporally.averaged Set to TRUE is this ModelObject has been temporally averaged
#' @slot run A ModelRunInfo object which contains the metadata about the run which this ModelObject belongs too.
#' @exportClass ModelObject
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("ModelObject", 
         slots = c(id = "character",
                   data = "data.table",
                   quant = "Quantity",
                   spatial.extent = "SpatialExtent",
                   temporal.extent = "TemporalExtent",
                   is.site = "logical",
                   is.spatially.averaged = "logical",
                   is.temporally.averaged = "logical",
                   run = "ModelRunInfo"
         )
         
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

#' Class to hold the metadata for a dataset
#' 
#' This class describes a vegetation run, including its location on disk, the model used, the PFT set used, an unique id and a description, offsets to apply to the longitudes and latitudes to make the co-rordinates gridcell centered and so on.
#' It is not primarily intended to be used by itself. Instead it is inherited by \code{ModelRun} object (due to this inheritance the slots can be accessed directly)
#' and included in a \code{ModelObject} in the \code{run} slot (not inherited, so needs to be access be \code{@@run}).
#' 
#' @slot id A unique character string to identify this particular model un.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot name A character string giving the name of this dataset (eg MODIS GPP). (Mandatory)
#' @slot quant A Quantity object describing the physical quantity thatthe dataset describes
#' @slot fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @slot line.col  A string to define an R colour used when plotting this runs as a line graph
#' @slot line.width A numeric to define the width of a line representing this model run
#' @slot line.type A numeric to define the line style representing this model run
#' @exportClass DatasetInfo
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("DatasetInfo", 
         slots = c(id = "character",
                   name = "character",
                   quant = "Quantity",
                   fill.col = "character", # not commonly needed, only for more complex run comparisons
                   line.col = "character", # # not commonly needed, only for more complex run comparisons
                   line.width = "numeric", # not commonly needed, only for more complex run comparisons
                   line.type = "numeric" # not commonly needed, only for more complex run comparisons
         )
         
)




#### DATA OBJECT CLASS

#' Contains one dataset (ie. measured, not model-generated).
#' 
#' A key class of the package.  A \code{DataObject} stores the data and metadata for one dataset (temporal or spatial).
#' For example 
#' 
#' Generally these are not created directly by the user, but rather by functions like \code{getMODISTreecover} or \code{getSaatchi2011} or so, but the use can create them directly if desired.
#' 
#' @slot id A unique character string to identify this particular DataObject.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to more fully describe this DataObject
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A Quantity object to define what output this DataObject contains
#' @slot spatial.extent A SpatialExtent object which describes the area covered by this DataObject.  Particularly useful if the data has been spatially averaged.
#' @slot temporal.extent A TemporalExtent object which describes the time periog covered by this DataObject.  Particularly useful if the data has been temporally averaged.
#' @slot correction.layer A character string defining a multiplicative corection layer that can be used for the data set
#' @exportClass DataObject
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("DataObject", 
         slots = c(data = "data.table",
                   spatial.extent = "SpatialExtent",
                   temporal.extent = "TemporalExtent",
                   correction.layer =  "character"
         ),
         contains = "DatasetInfo" 
         
)


#' Result of comparing a model raster to a data raster
#' 
#' This class stores the rasters (model, data, difference and precentage difference) and the statistics (R^2 etc) resulting when two rasters are compared using \code{compareRunToDataObject}
#' 
#' @slot id A unique character string to identify this particular raster compariosn.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot R2 The R^2 between the model and the data
#' @slot R2.eff The model efficiency of the model compared to the data
#' @slot P.cor The Pearsons product moment correlation between the model and the the data,
#' @slot ME The Mean Error (mean of residuals) between the model and the data
#' @slot NME The Normalised Mean Error (mean of residuals after the residuals have been divided the value of the data point) between the model and the data
#' @slot NMSE The Normalised Square Mean Error (mean of the square of the residuals after the residuals have been divided the value of the data point) between the model and the data
#' @slot RMSE The Root Mean Squared Error between the model and the the data
#' @slot sd.diff The standard deviation of the difference between the model and the the data (all gridcells)
#' @slot MM The Manhattan Metric (for comparisons of relative proportions)
#' @slot SCD The Manhattan Metric (for comparisons of relative proportions)
#' @slot Kappa The overall Cohen's Kappa obtained when comparing categorical variables.
#' @slot individual.Kappas The individual Cohen's Kappas for each category when comparing categorical data
#' @exportClass RasterComparison
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
setClass("SpatialComparison",
         slots = c(id = "character",
                   R2 = "numeric", 
                   R2.eff = "numeric",
                   P.cor = "numeric",
                   ME = "numeric", 
                   NME = "numeric",
                   NMSE = "numeric",
                   RMSE = "numeric", 
                   sd.diff = "numeric",
                   Kappa = "numeric", 
                   individual.Kappas = "numeric",
                   MM = "numeric",
                   SCD = "numeric"
         ),
         prototype = c(id = "-",
                       R2 = NA,
                       R2.eff = NA, 
                       P.cor = NA,
                       ME = NA, 
                       NME = NA,
                       NMSE = NA,
                       RMSE = NA, 
                       sd.diff = NA,
                       Kappa = NA, 
                       individual.Kappas = NA,
                       MM = NA,
                       SCD = NA
         ) 
)








#' Contains a comparison between two layers 
#' 
#' This object is produced as the result of a call to the functions \code{compareLayers()} and shouldn't be directly created by a user.
#'  
#' It contains a data slot with each of the compared layers and the difference between them (layer 1 - layer 2), a SpatialComaprisons object with the calculated values of 
#' various statistical metric, meta-data about the source of the two compared layers and spatio-temporal meta-data describing where the comparison is valid.
#' 
#' It can be plotted by functions like   
#'    
#' Generally these are not created directly by the user, but rather by functions like \code{getModelObject}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string describing this comaprison layer, is automatically generated
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A Quantity object to define what output this ModelObject contains
#' @slot stats An SpatialComaprison object giving ther statistical comparison metric between these two layers
#' @slot info1 Either a ModeulRunInfo object or a DatasetInfo object describing the source of the first layer in the comparison
#' @slot info2 Either a ModeulRunInfo object or a DatasetInfo object describing the source of the second layer in the comparison
#' @slot spatial.extent A SpatialExtent object which describes the area covered by this ComparisonLayer.  Particularly useful if the data has been spatially averaged.
#' @slot temporal.extent A TemporalExtent object which describes the time period covered by this ComparisonLayer.  Particularly useful if the data has been temporally averaged.
#' @slot is.site Set to TRUE is this ComparisonLayer describes a single site
#' @slot is.spatially.averaged Set to TRUE is this ComparisonLayer has been spatially averaged
#' @slot is.temporally.averaged Set to TRUE is this ComparisonLayer has been temporally averaged
#' @exportClass ComparisonLayer
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

setClass("ComparisonLayer", 
         slots = c(id = "character",
                   name = "character",
                   data = "data.table",
                   quant = "Quantity",
                   stats = "SpatialComparison",
                   info1 = "ANY",
                   info2 = "ANY",
                   spatial.extent = "SpatialExtent",
                   temporal.extent = "TemporalExtent",
                   is.site = "logical",
                   is.spatially.averaged = "logical",
                   is.temporally.averaged = "logical"
                   
         )#,
         #validity = checkComparison
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
#' @slot id A unique character string to identify this particular biome scheme.  Recommended to be alphanumeric because it is used to construct file names. (Inherited from Quantity via "contains")
#' @slot name A character string that can be more descriptive of the biome scheme. (Inherited from Quantity via "contains")
#' @slot type A character string defining the type of Quantity, here should always be "categorical" (Inherited from Quantity via "contains")
#' @slot units A list of character strings giving the names of categories (biomes). (Inherited from Quantity via "contains")
#' @slot colours A function that returns the colour scale for this BiomeScheme. (Inherited from Quantity via "contains")
#' @slot aggregate.method A character string defining the default method for how to aggregate the quantity, should be set to "categorical" for BiomeSchemes (Inherited from Quantity via "contains")
#' @slot model Either a the string "Standard" to denote that this is a  standard quantity to be compared across all model and data, or vector of model names to denote to which models this BiomeScheme can generally be applied to. (Inherited from Quantity via "contains")
#' @slot rules A function which is applied to every row of the data.table and describes the biome classification rules.
#' @slot combineShadeTolerance If TRUE, call combineShadeTolerance before doing the biome classification
#' @slot totals.needed List of vegetation totals needed to calculate biomes, for example c("Tree", "Grass")
#' @slot max.needed List of maximum values needed to calculate biomes, for example c("PFT", "Tree")
#' @slot fraction.of.total List of vegetation fraction of totals needed to calculate biomes
#' @slot fraction.of.tree List of vegetation fraction of tree total needed to calculate biomes
#' @slot fraction.of.woody List of vegetation fraction of woody total needed to calculate biomes
#' @slot needGDD5 If TRUE the biome rules require GDD5 for the classification
#' @slot data.reference Character string giving a reference where the data for this biome scheme comes from
#' @slot published.reference Character string giving a reference where this model output classification scheme was published
#' @exportClass BiomeScheme
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("BiomeScheme",
         contains = "Quantity",
         slots = c(rules = "function",
                   combineShadeTolerance = "logical",
                   max.needed = "character",
                   totals.needed = "character",
                   fraction.of.total = "character",
                   fraction.of.tree = "character",
                   fraction.of.woody = "character",
                   needGDD5 = "logical",
                   data.reference = "character",
                   published.reference = "character"
         )
)





