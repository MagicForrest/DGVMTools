#' Read MODIS GPP map
#' 
#' Blah
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "1km-Turkey"
#' @param average Logical, if true return average of all years, if false return all years
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster

getMODISGPP <- function(location = "/data/forrest/Productivity/MODIS/V55", resolution = "original", average = TRUE) {

  Lon = Lat = Year = NULL
    
  if(resolution == "original"){
    
  }
  else if(resolution == "1km-Turkey"){
    if(average) raster <- trim(rotate(raster(file.path(location, "processed/MODISGPP.TA.2000-2010.1km-Turkey.nc"))))
    else raster <- trim(rotate(brick(file.path(location, "processed/MODISGPP.2000-2010.1km-Turkey.nc"))))
    print(raster)
  }
  
  
  dt <- data.table(as.data.frame(raster,xy = TRUE))
  if(average) {
    setnames(dt, c("Lon", "Lat", "MODISGPP"))
  }
  else {
    setnames(dt, c("Lon", "Lat", paste(2000:2010)))
    dt <- melt(dt, id=c("Lon","Lat")) 
    setnames(dt, c("Lon", "Lat", "Year", "MODISGPP"))
    dt[,Year:= as.numeric(as.character(Year))]
  }
  dt[,Lon:= round(Lon, 3)]
  dt[,Lat:= round(Lat, 3)]

  setkey(dt, Lon, Lat)

  quant <- lookupQuantity("aGPP_std", "Standard")
  quant@cuts <- seq(0,3,0.1)
  
  
  dataset <- new("DataObject",
                         id = "MODISGPP",
                         name = "MODIS MOD17A2",
                         temporal.extent = new("TemporalExtent", id = "MODISPeriod", name = "MODIS Period", start = 2000, end = 2010),
                         data = dt,
                         quant = quant,
                         spatial.extent = new("SpatialExtent", id = "TurkeyExtent", name = "Turkey 1km extent", extent = getExtentFromDT(dt)),
                         correction.layer =  "")
  
  
  
  return(dataset)
  
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



