#' Read Avitabile et al. 2015 total biomass map
#' 
#' Total biomass across the tropics.  
#' Original dataset is 1km, also provided here at 0.5 degrees or T63.
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "HD" and "T63" and "original"
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster

getMOD44B <- function(location = "/data/forrest/TreeCover/", resolution = "HD") {
  
  
  Lon = Lat = NULL
 
  # read the standard dataset
  MOD44B.dt <- readData("MOD44B",
                         location = location,
                         resolution = resolution,
                         start.year = NULL,
                         end.year = NULL,
                         temporally.average = TRUE,
                         verbose = FALSE)
  
  
  # set the name to something equivalent in the model
  setnames(MOD44B.dt, c("DATALAYER"), c("Tree"))
  
  # also set the key 
  setkey(MOD44B.dt, Lon, Lat)
  
  # build the DataObject with the metadata
  MOD44B.dataset <- new("DataObject",
                           id = "MOD44B",
                           name = "MOD44B",
                           temporal.extent = new("TemporalExtent", name = "Avitabile Period", start = 2000, end = 2015),
                           data = MOD44B.dt,
                           quant = lookupQuantity("vegcover_std", "Standard"),
                           spatial.extent = new("SpatialExtent", id = "MODISExtent", name = "MODIS extent", extent(MOD44B.dt)),
                           correction.layer =  "")
  
  
  
  return(MOD44B.dataset)
  
}
