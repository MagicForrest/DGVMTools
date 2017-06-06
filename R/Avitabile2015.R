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

getAvitabile2015 <- function(location = "/data/forrest/Biomass/", resolution = "HD") {
  
  
  #### NEW ORDER, CODE BELOW NOW REDUNDANT
  
  # read the standard dataset
  Avitabile.dt <- readData("Avitabile2015",
                         location = location,
                         resolution = resolution,
                         start.year = NULL,
                         end.year = NULL,
                         temporally.average = TRUE,
                         verbose = FALSE)
  
  
  # set the name to something equivalent in the model
  setnames(Avitabile.dt, c("DATALAYER"), c("Tree"))
  
  # also set the key 
  setkey(Avitabile.dt, Lon, Lat)
  
  # build the DataObject with the metadata
  Avitabile.dataset <- new("DataObject",
                           id = "Avitabile2015",
                           name = "Avitabile et al. 2015",
                           temporal.extent = new("TemporalExtent", name = "Avitabile Period", start = 2000, end = 2010),
                           data = Avitabile.dt,
                           quant = lookupQuantity("vegC_std", "Standard"),
                           spatial.extent = new("SpatialExtent", id = "AvitabileExtent", name = "Avitabile extent", extent(Avitabile.dt)),
                           correction.layer =  "")
  
  
  
  return(Avitabile.dataset)
  
  
  
  Lon = Lat = NULL
  
  if(resolution == "original"){
    
    # read the original data
    Avitabile.raster <- raster::raster(file.path(location, "Avitabile_AGB_Map/Avitabile_AGB_Map.tif"))
    
    # convert from AGB (dry matter) to total carbon
    Avitabile.raster <- raster::calc(Avitabile.raster, AGBtoTotalCarbon)
    
    # divide by 10 to go from tC/Ha to kgC/m^2
    Avitabile.raster <- Avitabile.raster/10
    
  }
  else if(resolution == "HD"){
    Avitabile.raster <- raster::raster(file.path(location, "Avitabile2015.HD.nc"))
  }
  else if(resolution == "T63"){
    Avitabile.raster <- raster::trim(raster::rotate(raster::raster(file.path(location, "Avitabile2015.T63.nc"))))
  }
  
  
  Avitabile.dt <- data.table(as.data.frame(Avitabile.raster,xy = TRUE))
  Avitabile.dt <- stats::na.omit(Avitabile.dt)
  setnames(Avitabile.dt, c("Lon", "Lat", "Tree"))
  setkey(Avitabile.dt, Lon, Lat)
  
  
  
  Avitabile.dataset <- new("DataObject",
                           id = "Avitabile2015",
                           name = "Avitabile et al. 2015 Biomass",
                           temporal.extent = new("TemporalExtent", name = "Avitabile Period", start = 2000, end = 2010),
                           data = Avitabile.dt,
                           quant = lookupQuantity("vegC_std", "Standard"),
                           spatial.extent = new("SpatialExtent", id = "AvitabileExtent", name = "Avitabile extent", extent(Avitabile.dt)),
                           correction.layer =  "")
  
  
  
  return(Avitabile.dataset)
  
}
