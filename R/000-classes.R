#!/usr/bin/Rscript


##############################################################################
########  GENERAL CLASSES ####################################################
##############################################################################


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
#' These are defined in lists for the default PFTs for supported models (see below 'Usage' and 'Format' below) but the user may well need to define their own.  Such a list must be passed to a SourceInfo object
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

#' Supported Rub/Data Formats
#' 
#' List of supported data formats can be either a DGVM models type (say "LPJ-GUESS") or another standardised format 
#' (for example the "DGVMData" format used by the DGVMData package).  Dev question: Implement CF compliant netCDF as a format??
#' Possible dev answer, that is covered by the DGVMData package perhaps? 
#'  
#' @format A list of character strings for each of the supported vegetation models
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
supported.formats <- c("LPJ-GUESS", 
                       "LPJ-GUESS-SPITFIRE", 
                       "aDGVM", 
                       "LPJ-GUESS-SPITFIRE-OLD-FireMIP", 
                       "LPJ-GUESS-SPITFIRE-FireMIP",
                       "LPJ-GUESS-SIMFIRE-BLAZE-FireMIP",
                       "LPJ-GUESS-GlobFIRM-FireMIP",
                       "CLM-FireMIP",
                       "CTEM-FireMIP",
                       "JSBACH-FireMIP",
                       "Inferno-FireMIP",
                       "ORCHIDEE-FireMIP",
                       "DGVMData",
                       "Data")

########### SourceInfo - class to hold the metadata for an LPJ-GUESS run

#' Checks validity of a \code{SourceInfo}.
#' 
#' Called internally as the validity slot of the \code{SourceInfo}.  It checks that the essential slots are filled with sensible values ie a dir that
#' exists on the file system; a model type which is valid and an \code{id} that is a non-empty character string.  It doesn't check that this is alphanumeric, this would be a useful addition.
#' 
#' @param object The \code{SourceInfo} object to check for vailidity.
#' @return Empty string if the essential slots are fine, a string containing an error message if not.
#' @keywords internal
#'    
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

checkSourceInfo <- function(object){
  
  errors <- character()
  warnings <- character()
  
  # Check format type is supported
  if (!length(object@format) > 0) {
    msg <- "Error defining Source, you must define a format type!"
    errors <- c(errors, msg)
  }
  else if (!(object@format  %in% supported.formats)) {
    msg <- paste("Unsupported model type", object@format, sep = " ")
    errors <- c(errors, msg)
  }
  
  
  # Check dir exists
  if (!utils::file_test("-d", object@dir)) {
    msg <- paste("Directory not found:", object@dir, sep = " ")
    errors <- c(errors, msg)
  }
  
  # Check id is sensible 
  if (!length(object@id) > 0 | object@id == "") {
    msg <- paste("Not a sensible source id:", object@id, sep = " ")
    errors <- c(errors, msg)
  }
  
  # Check name is sensible 
  if (!length(object@name) > 0 | object@name == "") {
    object@name <- object@id
    msg <- paste("Not a sensible source name:", object@name, sep = " ")
   # errors <- c(errors, msg)
  }
  
  # Other things are set to sensible/empty defaults in defineSource()
  
  if (length(errors) == 0) TRUE else errors

}


#' Class to hold the metadata for a vegetation model run
#' 
#' This class describes a vegetation run, including its location on disk, the model used, the PFT set used, an unique id and a description, offsets to apply to the longitudes and latitudes to make the co-rordinates gridcell centered and so on.
#' It is not primarily intended to be used by itself. Instead it is inherited by \code{Source} object (due to this inheritance the slots can be accessed directly)
#' and included in a \code{Field} in the \code{run} slot (not inherited, so needs to be access be \code{@@source}).
#' 
#' @slot id A unique character string to identify this particular model un.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot format A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE" or "aDGVM". (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @slot name A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @slot dir The location of this run on the file system (Mandatory)
#' @slot forcing.data A character string identifying the climate or other data used to produce this model run
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot land.use.included If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @slot contact Name and email address of responsible person (default to OS username).
#' @slot institute Name of the institute (default "none").
#' @exportClass SourceInfo
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("SourceInfo", 
         slots = c(id = "character",
                   format = "character",
                   pft.set = "list",
                   name = "character",
                   dir = "character",                              
                   forcing.data = "character",
                   lonlat.offset = "numeric",
                   year.offset = "numeric",
                   london.centre = "logical",
                   land.use.included = "logical",
                   contact = "character",
                   institute = "character"
         ),
         prototype = c(pft.set = list(),
                       forcing.data = "not specified",
                       lonlat.offset = c(0,0),
                       year.offset = 0,
                       london.centre = TRUE,
                       land.use.included = TRUE,
                       contact = "not specified",
                       institute = "not specified"
         ),
         validity = checkSourceInfo
         
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
#' @slot cf.name A character string for the "standard_name" or "long_name" attribute for a CF-compliant netCDF file.  Won't make sense for all variables (not all DGVM quantities have a CF defined variable),
#' but it makes sense to use this where possible.  
#'
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
                   model = "character",
                   cf.name = "character"
         ),
         prototype = c(id = "UnknownID",
                       name = "UnknownString",
                       type = "UnknownType",
                       units = "-",
                       colours = fields::tim.colors,
                       aggregate.method = "sum",
                       model = "Standard",
                       cf.name = "unknown"
         )
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
#' Generally these are not created directly by the user, but rather by functions like \code{getField}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string describing this comaprison layer, is automatically generated
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A Quantity object to define what output this.Field contains
#' @slot stats An SpatialComaprison object giving ther statistical comparison metric between these two layers
#' @slot info1 A SourceInfo object describing the source of the first layer in the comparison
#' @slot info2 A SourceInfo object describing the source of the second layer in the comparison
#' @slot spatial.extent An object which describes the area covered by this ComparisonLayer.  Particularly useful if the data has been spatially averaged.
#' @slot spatial.extent.id  Lalala
#' @slot spatial.aggregate.method  Set to TRUE is this ComparisonLayer has been spatially averaged
#' @slot subannual.aggregate.method Lalala
#' @slot subannual.original Lalala
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
                   spatial.extent = "ANY",
                   spatial.extent.id = "character",
                   spatial.aggregate.method = "character",
                   subannual.aggregate.method = "character",
                   subannual.original = "character"
                   
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





#' An S4 class to contain metadata and, optionally, data and benchmarks for a single vegetation run
#' 
#' A \code{Source} object contains the metadata concerning the an inherited \code{SourceInfo} object
#'  and the actual model data run as \code{Fields} in a list in slot \code{objects} and comparisions to datasets 
#'  as \code{BiomeComparison} and \code{RasterComparison} in a list in slot \code{benchmarks}.
#' Such objects can be built by calls to \code{getField()}, \code{calcNewVegObj},
#'  and \code{compareBiomes()}, and saved to the \code{Source} using \code{addToSource()}. 
#'  
#' Slots can be accessed by user directly, but more easily and usefully by functions \code{XXXX}
#' 
#' @slot objects List of \code{Fields} saved in this run
#' @slot id A unique character string to identify this particular model run.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot format A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE", "FireMIP" or "aDGVM". (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @slot name A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @slot dir The location of this run on the file system (Mandatory)
#' @slot forcing.data A character string identifying the climate or other data used to produce this model run
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot land.use.included If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @slot contact Name and email address of responsible person (default to OS username).
#' @slot institute Name of the institute (default "none").
#' @exportClass Source
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Source", 
         slots = c(objects = "list"
         ),
         prototype = c(objects = list()
         ),
         contains = "SourceInfo"
         
)


#' Contains one aspect of the model output, eg. LAI
#' 
#' A key class of the package.  A \code{Field} stores the data and metadata for one quantity that comes from a vegetation model run (including information about the run iself).
#' For example LAI (Leaf Area Index), or monthly evapotranspiration.  The data can be averaged spatially or temporally, or neither ot both, and manipulated and plotted by mayn funtions in this package.
#' 
#' Generally these are not created directly by the user, but rather by functions like \code{getField}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A Quantity object to define what output this.Field contains
#' @slot first.year Lalala
#' @slot last.year Lalala
#' @slot year.aggregate.method Set to TRUE is this.Field has been anually averaged
#' @slot spatial.extent An object which can be used to crop or subselect gridcells.  Can be anything from which a raster::extent can be derived (in which case raster::crop is 
#' used) or a list of gridcells used by DGVMTools::selectGridcels (see that documentation for how to format the gridcell list).
#' @slot spatial.extent.id A character id to handily record this spatial domain if some spatial subselection has been called, for example "Europe" or "Duke_Forest" or whatever
#' @slot spatial.aggregate.method Set to TRUE is this.Field has been spatially averaged
#' @slot subannual.aggregate.method Method by which this Field has been subannually aggregated
#' @slot subannual.original Original subannual resolution of this field
#' @slot source A SourceInfo object which contains the metadata about the run which this Field belongs too.
#' @exportClass Field
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

setClass("Field", 
         slots = c(id = "character",
                   data = "data.table",
                   quant = "Quantity",
                   first.year = "numeric",
                   last.year = "numeric",
                   year.aggregate.method = "character",
                   spatial.extent = "ANY",
                   spatial.extent.id = "character",
                   spatial.aggregate.method = "character",
                   subannual.aggregate.method = "character",
                   subannual.original = "character",
                   source = "SourceInfo"
         )
)



## define for older R versions, where this function does not exist.
## Just copied from a recent R version
if (!exists("OlsonNames")) {
  OlsonNames <- function()
  {
    if (.Platform$OS.type == "windows")
      tzdir <- Sys.getenv("TZDIR", file.path(R.home("share"),
                                             "zoneinfo"))
    else {
      tzdirs <- c(Sys.getenv("TZDIR"), file.path(R.home("share"),
                                                 "zoneinfo"), "/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
                  "/usr/lib/zoneinfo", "/usr/local/etc/zoneinfo", "/etc/zoneinfo",
                  "/usr/etc/zoneinfo")
      tzdirs <- tzdirs[file.exists(tzdirs)]
      if (!length(tzdirs)) {
        warning("no Olson database found")
        return(character())
      }
      else tzdir <- tzdirs[1]
    }
    x <- list.files(tzdir, recursive = TRUE)
    grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", x, value = TRUE)
  }
}
