#' Read Thurner et al. 2013 total biomass map
#' 
#' Total biomass across the nothern temperate and boreal zones.
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

getThurner2013 <- function(location = "/data/forrest/Biomass/Thurner2013/", resolution = "original") {

  Lon = Lat = NULL
    
  if(resolution == "original"){
    
    # read the original data
    Thurner.raster <- raster(file.path(location, "original/2013125152231biomass_v3_total.nc"), varname = "biomass_total")
    print(Thurner.raster)
  
  }
  else if(resolution == "HD"){
    Thurner.raster <- raster(file.path(location, "processed/Thurner2013.HD.nc"))
  }
  else if(resolution == "T63"){
    Thurner.raster <- trim(rotate(raster(file.path(location, "processed/Thurner2013.T63.nc"))))
  }
  else if(resolution == "1km-Turkey"){
    Thurner.raster <- trim(rotate(raster(file.path(location, "processed/Thurner2013.1km-Turkey.nc"))))
  }
  
  
  Thurner.dt <- data.table(as.data.frame(Thurner.raster,xy = TRUE))
  setnames(Thurner.dt, c("Lon", "Lat", "Tree"))
  Thurner.dt[,Lon:= round(Lon, 3)]
  Thurner.dt[,Lat:= round(Lat, 3)]

  setkey(Thurner.dt, Lon, Lat)

  Thurner.dt <- na.omit(Thurner.dt)
  
  quant <- lookupQuantity("vegC_std", "Standard")
  quant@cuts <- seq(0,25,1)
  
  Thurner.dataset <- new("DataObject",
                         id = "Thurner2013",
                         name = "Thurner et al. 2013",
                         temporal.extent = new("TemporalExtent", id = "ThurnerPeriod", name = "Thurner Period", start = 2000, end = 2010),
                         data = Thurner.dt,
                         quant = quant,
                         spatial.extent = new("SpatialExtent", id = "ThurnerExtent", name = "Thurner extent", extent = extent(Thurner.dt)),
                         correction.layer =  "")
  
  
  
  return(Thurner.dataset)
  
}

# Thurner2013.full <- getThurner2013()
# Thurner2013.raster <- promoteToRaster(Thurner2013.full@data)
# Thurner2013.raster.HD <- aggregate(Thurner2013.raster.HD, 50) 
# writeNetCDF(Thurner2013.raster.HD, 
#             filename = "/data/forrest/Biomass/Thurner2013/processed/Thurner2013.HD.nc", 
#             var.name = "Biomass", 
#             var.units = "kg[C]/m^2",
#             time.resolution = "annual", 
#             time.units.string = "year")
#Thurner2013.raster.HD <- getThurner2013(resolution = "HD")



