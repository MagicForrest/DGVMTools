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

getAvitabile2015 <- function(location = "/data/forrest/Biomass/Avitabile2015/", resolution = "HD") {

  Lon = Lat = NULL
    
  if(resolution == "original"){
    
    # read the original data
    Avitabile.raster <- raster(file.path(location, "Avitabile_AGB_Map/Avitabile_AGB_Map.tif"))
    
    # convert from AGB (dry matter) to total carbon
    Avitabile.raster <- calc(Avitabile.raster, AGBtoTotalCarbon)
    
    # divide by 10 to go from tC/Ha to kgC/m^2
    Avitabile.raster <- Avitabile.raster/10
    
  }
  else if(resolution == "HD"){
    Avitabile.raster <- raster(file.path(location, "Avitabile2015.HD.nc"))
  }
  else if(resolution == "T63"){
    Avitabile.raster <- trim(rotate(raster(file.path(location, "Avitabile2015.T63.nc"))))
  }
  
  
  Avitabile.dt <- data.table(as.data.frame(Avitabile.raster,xy = TRUE))
  Avitabile.dt <- na.omit(Avitabile.dt)
  setnames(Avitabile.dt, c("Lon", "Lat", "Avitabile2015"))
  setkey(Avitabile.dt, Lon, Lat)

  
  
  Avitabile.dataset <- new("DataObject",
                         id = "Avitabile2015",
                         name = "Avitabile et al. 2015 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Avitabile Period", start = 2000, end = 2010),
                         data = Avitabile.dt,
                         quant = lookupQuantity("vegC_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "AvitabileExtent", name = "Avitabile extent", extent = getExtentFromDT(Avitabile.dt)),
                         correction.layer =  "")
  
  
  
  return(Avitabile.dataset)
  
}

