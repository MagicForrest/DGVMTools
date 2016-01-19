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

require("raster")

getAvitabile2015 <- function(resolution = "HD") {
  
  if(resolution == "original"){
    
    # read the original data
    Avitabile.raster <- raster(file.path(benchmarking.data.dir, "Avitabile2015/Avitabile_AGB_Map/Avitabile_AGB_Map.tif"))
    
    # convert from AGB (dry matter) to total carbon
    Avitabile.raster <- calc(Avitabile.raster, AGBtoTotalCarbon)
    
    # divide by 10 to go from tC/Ha to kgC/m^2
    Avitabile.raster <- Avitabile.raster/10
    
  }
  else if(resolution == "HD"){
    Avitabile.raster <- raster(file.path(benchmarking.data.dir, "Avitabile2015/Avitabile2015.HD.nc"))
  }
  else if(resolution == "T63"){
    Avitabile.raster <- trim(rotate(raster(file.path(benchmarking.data.dir, "Avitabile2015/Avitabile2015.T63.nc"))))
  }
  
  
  
  Avitabile.dataset <- new("SpatialDataset",
                         id = "Avitabile2015",
                         name = "Avitabile et al. 2015 Biomass",
                         abbreviation = "Avitabile et al. 2015",   
                         time.span = new("TimeSpan", name = "Avitabile Period", start = 2000, end = 2010),
                         data = Avitabile.raster,
                         veg.quant = getVegQuantity("cmass"),
                         units = "kgC/m^2")
  
  
  return(Avitabile.dataset)
  
}



# all <- getAvitabile2015(resolution = "original")
# all.60km <- aggregate(all@data, 60, mean)
# writeNetCDF(all.60km, "biomass", "kgC m-1", time.resolution = "annual", time.units.string = "year", long.name = "", filename = "/data/forrest/Biomass/Avitabile2015/Avitabile2015.60km.nc", ordering = "standard")
# 
# all.HD <- raster()
# xmin(all.HD) <- -180
# xmax(all.HD) <- 180
# ymin(all.HD) <- -90
# ymax(all.HD) <- 90
# projection(all.HD) <- "+proj=longlat +datum=WGS84"
# res(all.HD) <- 0.5
# 
# 
# all.HD <- resample(all.60km, all.HD, method = "bilinear" )
# all.HD[all.HD < 0 ] <- 0
# writeNetCDF(all.HD, "biomass", "kgC m-1", time.resolution = "annual", time.units.string = "year", long.name = "", filename = "/data/forrest/Biomass/Avitabile2015/Avitabile2015.HD.nc", ordering = "standard")
# 
