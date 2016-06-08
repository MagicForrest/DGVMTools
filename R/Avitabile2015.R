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
  setnames(Avitabile.dt, c("Lon", "Lat", "Avitabile2015"))
  setkey(Avitabile.dt, Lon, Lat)

  
  
  Avitabile.dataset <- new("DataObject",
                         id = "Avitabile2015",
                         name = "Avitabile et al. 2015 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Avitabile Period", start = 2000, end = 2010),
                         data = Avitabile.dt,
                         quant = lookupVegQuantity("cmass"),
                         spatial.extent = new("SpatialExtent", id = "AvitabileExtent", name = "Avitabile extent", extent = extentFromDT(Avitabile.dt)),
                         correction.layer =  "")
  
  
  
  return(Avitabile.dataset)
  
}

