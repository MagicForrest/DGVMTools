#!/usr/bin/Rscript


#' Format class
#'
#' This class encapsulates all the specific details of a supported model or dataset.  It stores meta-data and methods (just functions, not technically
#'  methods in R terms) for a DGVM output format (or other data format).  Note that the default.pfts and  'quantities' arguments are just default 
#'  values, it is easy to add new ones.  Equally they don't all need to to be available for a particular run or dataset.  You can define your own for your 
#'  own data format if you, for example, want to include a new model type.    
#'  Format objects which are  included in the package by default are listed below:
#'
#' @slot id Simple character string to gave an uniquely identify this format
#' @slot default.pfts 'Standard' vegetation types (PFTs, Plant Functional Type) that this format uses, as a list of DGVMTools::PFT objects.  
#' This is mostly likely applicable to formats decsribing DGVM output, but also for some data sets.  
#' This is just a default PFT set available for convenience, can be easily over-ridden when defining a Source object (see defineSource()).
#' @slot quantities 'Standard' quantities (as a list of DGVMTools::Quantity objects) which might be availably from the model output or dataset.
#' @slot determinePFTs A function to determine which PFTs are present in a run or dataset.
#' @slot availableQuantities A function to determine which quantities \emph{are actually available} from the model run or dataset.
#' @slot getField A function to retrieve actually data from the model output or dataset.  This is likely to be a fairly complex function, and really depends on the specifics 
#' and idiosynchrasies of the model output format or dataset.
#' 
#' @details Normally a user won't need to deal with this class since it defines model/dataset spcific metadata and functions which should be defined once and then
#' 'just work' (haha) in the future. If someone wants their model to be supported by DGVMTools then this is the object that needs to be defined correctly.
#' 
#' @name Format-class
#' @rdname Format-class
#' @exportClass Format
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Format", 
         slots = c(id = "character",
                   default.pfts = "list",
                   quantities = "list",
                   determinePFTs = "function",
                   availableQuantities = "function",
                   getField = "function"
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

#' STAInfo class
#'
#' This class encapsulations all the Spatial (S, longitude and latidude), Temporal (T, monthly, daily etc.) and Annual (A, years included) 
#' Information  (hence the name 'STAInfo') about a particular DGVMTools::Field.  Normally the user won't have to deal with this (it is mostly used internally),
#' but it can be handy, for example to extract the STAInfo from one Field, and use it extract another field with the same dimensions. 
#'     
#' @param first.year A single numeric, the first year.
#' @param last.year A single numeric, the last year.
#' @param year.aggregate.method A character specifying how the years have been aggregated, for example "mean", or "sum" or "var". See aggregateYears.
#' If no yearly aggregation has been applied it should be NULL.
#' @param spatial.extent This can be of any type that can be used but DGVMTools::crop, and stores the current spatial extent.  
#' But default (and with no cropping) it is simple teh raster::Extent object of the whole domain.
#' @param spatial.aggregate.method A character method specifying how the spatial extent has been aggregated, for eample "mean" or "sum",
#' see aggregateSpatial.  If no spatial aggregation has been applied it should be NULL.
#' @param subannual.original A character string specifying the original sub-annual resolution of this data, eg. "Year", Month", "Day"
#' @param subannual.resolution A character string specifying the current sub-annual resolution of this data, eg. "Year", Month", "Day"
#' @param subannual.aggregate.method A character specifying how the subannual perods have been aggregated, for example "mean", "max", "sum" or "var". 
#' See aggregateSubannual(). If no sub-annual aggregation has been applied it should be NULL.
#' 
#' @details This is mostly a behind-the-scenes class which is partly just created to bundle together a lot of dimension information in a tidy form. 
#' 
#' @name STAInfo-class
#' @rdname STAInfo-class
#' @exportClass STAInfo
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("STAInfo",
         slots = c(first.year = "numeric",
                   last.year = "numeric",
                   year.aggregate.method = "character",
                   spatial.extent = "ANY",
                   spatial.extent.id = "character",
                   spatial.aggregate.method = "character",
                   subannual.resolution = "character",
                   subannual.aggregate.method = "character",
                   subannual.original = "character"
         ),
         prototype  = list(first.year = numeric(0),
                        last.year = numeric(0),
                        year.aggregate.method = "none",
                        spatial.extent = NULL,
                        spatial.extent.id = character(0),
                        spatial.aggregate.method = "none",
                        subannual.resolution = character(0),
                        subannual.aggregate.method = "none",
                        subannual.original = character(0)
         )
         
)



#' Class to hold the metadata for a Plant Functional Type (PFT)
#' 
#' @description   This is a class to hold meta-data about PFTs.  As detailed in the 'Slots' section below, this includes an id (should be unique) and a name, 
#' as well as their growth form, phenology, leaftype, climate zone, shade tolerance and a default plot colour.
#' These are defined in lists for the default PFTs for supported model format (see'Usage' below). 
#' 
#' @slot id A unique character string to identify this particular PFT.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string to describe the PFT. Used for building plot labels, not file names, so doesn't need to be alphanumeric and can so can be prettier.
#' @slot growth.form A string defining the growth.form of the PFT, typically either "Tree", "Grass" or "Shrub"
#' @slot leaf.form A string defining the leaf.form of the PFT, typically either "Broadleaved" or "Needleleaved"
#' @slot phenology A string defining the phenology of the PFT, typically "Evergreen", "Summergreen", "Raingreen" or "GrassPhenology"
#' @slot climate.zone A string defining the climate climate.zone of a PFT, typically "Boreal", "Temperate" or "Tropical" (could go crazy and also have "Mediterranean", for example)
#' @slot shade.tolerance A string defining the \code{id} of a shade tolerance characteristric of a PFT. 
#' @slot colour A string defining a preferred R colour to plot this PFT (for line graphs etc)
#' 
#' @details The \code{PFT-class} is only meta-data but is very important as it allows users to calculate, for example, the total Tree LAI with a simple command. 
#' The standard PFTs for some models are included (see above), but if you have other PFTs then you simply need define them.  You then combine the PFTs into an R list
#' and then provide them as the 'pft.set' argument to the \code{defineSource} call and bingo! you are using your custom PFTs. 
#' 

#' @name PFT-class
#' @rdname PFT-class
#' @exportClass PFT
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("PFT", 
         slots = c(id = "character",
                   name = "character",
                   growth.form = "character",
                   leaf.form = "character",
                   phenology = "character",
                   climate.zone = "character",
                   shade.tolerance = "character",
                   colour = "character"
         )
)



########### Source - class to hold the metadata for an LPJ-GUESS run

#' Checks validity of a \code{Source}.
#' 
#' Called internally as the validity slot of the \code{Source}.  It checks that the essential slots are filled with sensible values ie a dir that
#' exists on the file system; the format type is specified and an \code{id} that is a non-empty character string.  It doesn't check that this is alphanumeric, this would be a useful addition.
#' 
#' @param object The \code{Source} object to check for vailidity.
#' @return Empty string if the essential slots are fine, a string containing an error message if not.
#' @keywords internal
#'    
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

checkSource <- function(object){
  
  errors <- character()
  warnings <- character()
  
  # Check format type is supported
  if (!length(object@format) > 0) {
    msg <- "Error defining Source, you must define a format type!"
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


#' Metadata for a data source
#' 
#' This class describes a data source, including its location on disk, the format (ie what model or dataset it cam from), the PFT set used, an unique id and a description, offsets to apply to the longitudes and latitudes to make the co-rordinates gridcell centered and so on.
#' It is not primarily intended to be used by itself. Instead it is inherited by \code{Source} object (due to this inheritance the slots can be accessed directly)
#' and included in a \code{Field} in the \code{run} slot (not inherited, so needs to be access be \code{@@source}).
#' 
#'  
#' Slots can be accessed by user directly, but more easily and usefully by functions \code{XXXX}
#' 
#' @slot id A unique character string to identify this particular data source.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @slot dir The location of this run on the file system (Mandatory)
#' @slot format A character string or a DGVMTools::Format pbject to describe how this data is stored on disk.  This will depend on either the model 
#' that was used to produce it or the dataset type.  Currently defined Format objects are \code{GUESS}, \code{aDGVM} and \code{DGVMData} but for convenience the 
#' strings, "GUESS", "LPJ-GUESS", "LPJ-GUESS-SPITFIRE", "aDGVM" and "DGVMData" make be used. (Mandatory)
#' @slot pft.set A list of PFT objects which includes all the PFTs included in this model run/dataset.
#' @slot name A character string describing this data source, ie. "LPJ-GUESS v3.1" (can be omitted, in which case the id will be used instead)
#' @slot forcing.data A character string identifying the climate or other data used to produce this data
#' @slot lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @slot year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @slot london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @slot land.use.included If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @slot contact Name and email address of responsible person (default to OS username).
#' @slot institute Name of the institute (default "none").
#' @exportClass Source
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Source", 
         slots = c(id = "character",
                   format = "Format",
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
         validity = checkSource
         
)




#' Class to hold meta-data for a vegetation quantity
#' 
#' This class hold meta-data for specific vegetation or land surface quantities like "lai" (LAI) or "mwcont_upper" (mean monthly water).  
#' Includes metadata about the quantity (the units it is measured in for example), values for default plot proprties (color scales, plot ranges) and the aggragating method - ie whether the quantity shoudd generally be summed (for example monthly burnt area or C mass per PFT) or averaged (for example monthly soil water content).
#' See the 'Slots' documentation below.
#' Note that for some analyses  user will probably need to define their own, and modify these for their own analysis and plots.  However the standard quantities 
#' that one expects to be available from different models are listed below in the 'Usage' section.
#' 
#' @slot id A unique character string to identify this particular vegetation quantity.  This will be interpreted for an output-format specific fucntion, but should
#' ideally match with the name of a particular model output/dataset variable.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A longer character string to provide a more complete description of this quantity
#' @slot units A character string defining the units this quantity is defined in.  Possibly formally link to udunits2?
#' @slot colours A function that returns a colour scale suited for plotting this quantity.
#' @slot format Either a the string "Standard" to denote that this is a standard quantity to be compared across all model and data, the id of the Format object with which this Quantity is associated.
#' @slot cf.name A character string for the "standard_name" or "long_name" attribute for a CF-compliant netCDF file.  Won't make sense for all variables (not all DGVM quantities have a CF defined variable),
#' but it makes sense to use this where possible.  
#' 
#' @details Note that quantities in the \code{Standard.quantities} list should ideally be defined for all model types. 
#'
#' @exportClass Quantity
#' @name Quantity-class
#' @rdname Quantity-class
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("Quantity", 
         slots = c(id = "character",
                   name = "character",
                   units = "character",
                   colours = "function",
                   format = "character",
                   cf.name = "character"
         ),
         prototype = c(id = "UnknownID",
                       name = "UnknownString",
                       units = "-",
                       colours = fields::tim.colors,
                       format = "Standard",
                       cf.name = "unknown"
         )
)






#' Comparison statistics
#' 
#' This class stores the statistics resulting from the comparison of two layers from DGVMTools::Field objects.
#' 
#' @slot id A unique character string to identify this particular raster compariosn.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot R2 The R^2 between the two datasets
#' @slot R2.eff The model efficiency of the two datasets
#' @slot P.cor The Pearsons product moment correlation between the two datasets
#' @slot ME The Mean Error (mean of residuals) between two datasets
#' @slot NME The Normalised Mean Error (mean of residuals after the residuals have been divided the value of the data point) between two datasets
#' @slot NMSE The Normalised Square Mean Error (mean of the square of the residuals after the residuals have been divided the value of the data point) two datasets
#' @slot RMSE The Root Mean Squared Error between the two datasets
#' @slot sd.diff The standard deviation of the difference between the two datasets (all gridcells)
#' @slot MM The Manhattan Metric (for comparisons of relative proportions)
#' @slot SCD The Manhattan Metric (for comparisons of relative proportions)
#' @slot Kappa The overall Cohen's Kappa obtained when comparing categorical variables.
#' @slot individual.Kappas The individual Cohen's Kappas for each category when comparing categorical data
#' @exportClass Statistics
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
setClass("Statistics",
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
#' It contains a data slot with each of the compared layers and the difference between them (layer 1 - layer 2), a Comparisons object with the calculated values of 
#' various statistical metric, meta-data about the source of the two compared layers and spatio-temporal meta-data describing where the comparison is valid.
#' 
#' It can be plotted by functions like   
#'    
#' Generally these are not created directly by the user, but rather by functions like \code{getField}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot name A character string describing this comaprison layer, is automatically generated
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant1 A Quantity object to define what quantity the data from first field represents
#' @slot quant2 A Quantity object to define what quantity the data from second field represents
#' @slot layers1 A character string (or vector thereof) of the layer(s) from the first field that were compared
#' @slot layers2 A character string (or vector thereof) of the layer(s) from the second field that were compared
#' @slot stats An SpatialComaprison object giving ther statistical comparison metric between these two layers
#' @slot source1 A Source object describing the source of the first layer in the comparison
#' @slot source2 A Source object describing the source of the second layer in the comparison
#' @slot sta.info1 The STAInfo object for the first field
#' @slot sta.info2 The STAInfo object for the second field
#' @exportClass Comparison
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

setClass("Comparison", 
         slots = c(id = "character",
                   name = "character",
                   data = "data.table",
                   quant1 = "Quantity",
                   quant2 = "Quantity",
                   layers1 = "character",
                   layers2 = "character",
                   source1 = "Source",
                   source2 = "Source",
                   sta.info1 = "STAInfo",
                   sta.info2 = "STAInfo",
                   stats = "Statistics"
                   
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
#' @slot id A unique character string to identify this particular biome scheme.  Recommended to be alphanumeric because it is used to construct file names. (Inherited from Quantity via "contains")
#' @slot name A character string that can be more descriptive of the biome scheme. (Inherited from Quantity via "contains")
#' @slot units A list of character strings giving the names of categories (biomes). (Inherited from Quantity via "contains")
#' @slot colours A function that returns the colour scale for this BiomeScheme. (Inherited from Quantity via "contains")
#' @slot format Either a the string "Standard" to denote that this is a standard quantity to be compared across all model and data, the id of the Format object with which this Quantity is associated.
#' @slot rules A function which is applied to every row of the data.table and describes the biome classification rules.
#' @slot layers.needed List of vegetation layers needed to calculate biomes and the name of the new layer, to be interpreted by layerOp(), specifified as a list of three- or four-item list 
#' whose elements first element id the id of a DGVMTools::Quantity and whose other elements are passed as arguements to the layerOp() function.  
#' For example one element could be \code{Woody = list(quantity = "LAI_std", operator = "+", layers = c(".Tree", ".Shrubs"), new.layer = "Woody")}, 
#' which would make a layer called "Woody" which would be the sum of all LAI trees and shrubs.
#' @slot data.reference Character string giving a reference where the data for this biome scheme comes from
#' @slot published.reference Character string giving a reference where this model output classification scheme was published
#' @name BiomeScheme-class
#' @rdname BiomeScheme-class
#' @exportClass BiomeScheme
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
setClass("BiomeScheme",
         contains = "Quantity",
         slots = c(rules = "function",
                   layers.needed = "list",
                   data.reference = "character",
                   published.reference = "character"
         )
)





#' Contains one aspect of the output, eg. LAI
#' 
#' A key class of the package as it actually holds the data (most other classes are for metadata).  A \code{Field} stores the data and metadata for one quantity
#' that comes from a dataset or vegetation model run (including information about the run iself). For example LAI (Leaf Area Index), or evapotranspiration. 
#' The data can be aggragted across space, and across and within years, and manipulated and plotted by mayn funtions in this package.
#' 
#' Generally these are not created directly by the user, but rather by functions like \code{getField}.
#' 
#' @slot id A unique character string to identify this particular vegetation object.  Recommended to be alphanumeric because it is used to construct file names.
#' @slot data A data.table object.  This is used because is it very much faster for calculations that data.frame or raster layers.
#' @slot quant A Quantity object to define what output this.Field contains
#' @slot first.year The first year of data
#' @slot last.year The last year of data
#' @slot year.aggregate.method A charecter string describing the method by which the years have been aggregated.  Will be the zero character ("character(0)") if data it not 
#' yearly aggreagted
#' @slot spatial.extent An object which can be used to crop or subselect gridcells.  Can be anything from which a raster::extent can be derived (in which case raster::crop is 
#' used) or a list of gridcells used by DGVMTools::selectGridcels (see that documentation for how to format the gridcell list).
#' @slot spatial.extent.id A character id to handily record this spatial domain if some spatial subselection has been called, for example "Europe" or "Duke_Forest" or whatever
#' @slot spatial.aggregate.method Set to TRUE is this.Field has been spatially averaged
#' @slot subannual.aggregate.method Method by which this Field has been subannually aggregated
#' @slot subannual.original Original subannual resolution of this field
#' @slot source A Source object which contains the metadata about the run which this Field belongs too.
#' @exportClass Field
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

setClass("Field", 
         slots = c(id = "character",
                   data = "data.table",
                   quant = "Quantity",
                   source = "Source"
         ),
         contains = "STAInfo"
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


