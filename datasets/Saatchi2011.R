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


readSaatchi2011 <- function(lonlat.offset = c(0,0),
                            plot = FALSE,
                            tolerance = 0.00001,
                            london.centre=TRUE) {
  
  Saatchi.data <- NULL
  Saatchi.data <- read.table(saatchi2011.file, header = TRUE, stringsAsFactors=FALSE)
  
  names(Saatchi.data)[which(names(Saatchi.data)=="lon")] <- "Lon"
  names(Saatchi.data)[which(names(Saatchi.data)=="lat")] <- "Lat"
  
  # remove any rows with NaN
  #row.has.na <- apply(Saatchi.data, 1, function(x){any(is.na(x))})
  #Saatchi.data <- Saatchi.data[!row.has.na,]
  
  # apply offset
  Saatchi.data$Lon <- Saatchi.data$Lon + lonlat.offset[1]
  Saatchi.data$Lat <- Saatchi.data$Lat + lonlat.offset[2]
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(london.centre){
    Saatchi.data$Lon <- vapply(Saatchi.data$Lon, 1, FUN = LondonCentre)    
  }
  
  # remove the extra column
  Saatchi.data[["total_MgC_per_ha"]] <- NULL
  
  # add coordinates
  coordinates(Saatchi.data) = ~Lon+Lat
  
  # make gridded
  gridded(Saatchi.data) = TRUE
  
  # make full SpatialGridDataFrame
  Saatchi.data = as(Saatchi.data, "SpatialGridDataFrame") # to full grid
  
  # make raster 
  Saatchi.raster <- raster(Saatchi.data)
  
  # for Saatchi data need to build a mask
  Saatchi.Africa.extent <- extent(-18, 51.5, -36, 22)
  Saatchi.Africa.raster <- raster(Saatchi.Africa.extent)
  res(Saatchi.Africa.raster) <- 0.5
  Saatchi.Africa.raster[] <- 1
  
  Saatchi.America.extent <- extent(-113, -35, -55.5, 30.5)
  Saatchi.America.raster <- raster(Saatchi.America.extent)
  res(Saatchi.America.raster) <- 0.5
  Saatchi.America.raster[] <- 1  
  
  Saatchi.Asia.extent <- extent(60, 155, -30, 40)
  Saatchi.Asia.raster <- raster(Saatchi.Asia.extent)
  res(Saatchi.Asia.raster) <- 0.5
  Saatchi.Asia.raster[] <- 1 
  
  
  temp <- merge(Saatchi.Africa.raster, Saatchi.America.raster)
  Saatchi.mask <- merge(temp, Saatchi.Asia.raster)
  #Saatchi.mask <- extend(Saatchi.mask , Saatchi.raster)
  Saatchi.raster <- crop(Saatchi.raster, Saatchi.mask)
  
  
  Saatchi.raster <- mask(Saatchi.raster, Saatchi.mask)
  
  
  if(plot){
    pdf("Saatchi.Data.pdf")
    print(spplot(Saatchi.raster, "total_kgC_per_sqm"))
    dev.off()
  }
  
  Saatchi.dataset <- new("SpatialDataset",
                         id = "Saatchi2011",
                         name = "Saatchi et al. 2011 Biomass",
                         abbreviation = "Saatchi et al. 2011",   
                         time.span = new("TimeSpan", name = "Saatchi Period", start = 1999, end = 2001),
                         data = Saatchi.raster,
                         veg.quant = getVegQuantity("cmass"),
                         units = "kgC/m^2")
  
  
  return(Saatchi.dataset)
  
}


benchmarkVsSaatchi<-function(model.raster, model.name="LPJ-GUESS", run, doScatterPlots = TRUE, doHistoPlots = TRUE){
  
  dataset.id <- "Saatchi2011"
  diff.breaks <- seq(-60, 60, 1)
  plot.breaks <- seq(-35,35,1)
  histo.plot.range <- c(-20,30)
  Saatchi.period <- new("TimeSpan", name = "Saatchi", start = 1999, end = 2001)
  Saatchi.dataset <- readSaatchi2011()	
  original.data <- Saatchi.dataset@data
  full.correction.data <- readNaturalLandCoverGC2005()
  
  
  
  # Land use correction
  correction.raster <- crop(full.correction.data, model.raster)
  model.raster <- model.raster * correction.raster
  
  # Do the generic analysis	 
  #comparison.results <- compareTwoRasters(stack(model.raster,data.raster),  Saatchi.dataset@abbreviation, model.name, diff.breaks = diff.breaks, plot.breaks = plot.breaks,  histo.plot.range = histo.plot.range, doScatterPlots=doScatterPlots, doHistoPlots=doHistoPlots, run = run, period = Saatchi.period, quant = getVegQuantity("cmass"))				
  comparison.results <- compareRunToSpatialDataset(Saatchi.dataset, 
                                                   model.raster,
                                                   run,
                                                   diff.breaks = diff.breaks, 
                                                   plot.breaks = plot.breaks,  
                                                   histo.plot.range = histo.plot.range, 
                                                   doScatterPlots=doScatterPlots, 
                                                   doHistoPlots=doHistoPlots, 
                                                   quant = getVegQuantity("cmass")
  )				
  
  
  
  rm(original.data, full.correction.data, common.extent, model.raster, data.raster,  correction.raster )
  
  return(comparison.results)
  
}

getSaatchi2011 <- function(resolution = "HD"){
  
  
  if(resolution == "original"){
    Saatchi.Africa <- raster(file.path(saatchi2011.original.data.path, "africa_carbon_1km.tif"))
    Saatchi.Asia <- raster(file.path(saatchi2011.original.data.path, "asia_carbon_1km.tif"))
    Saatchi.America <- raster(file.path(saatchi2011.original.data.path, "america_carbon_1km.tif"))
        
    Saatchi.raster <- merge(Saatchi.Africa, Saatchi.Asia, tolerance = 0.1)
    Saatchi.raster <- merge(Saatchi.raster, Saatchi.America, tolerance = 0.1)
  }
  else if(resolution == "T63"){
    Saatchi.raster <- trim(rotate(raster(file.path(benchmarking.data.dir, "Saatchi2011/Saatchi2011.T63.nc"))/10))
  }
  else if(resolution == "HD"){
    Saatchi.raster <- trim(raster(file.path(benchmarking.data.dir, "Saatchi2011/Saatchi2011.HD.nc"))/10)
  }
  
  Saatchi.dataset <- new("SpatialDataset",
                         id = "Saatchi2011",
                         name = "Saatchi et al. 2011 Biomass",
                         abbreviation = "Saatchi et al. 2011",   
                         time.span = new("TimeSpan", name = "Saatchi Period", start = 1999, end = 2001),
                         data = Saatchi.raster,
                         veg.quant = getVegQuantity("cmass"),
                         units = "kgC/m^2")
  
  
  return(Saatchi.dataset)
  
  
}

#  
#all <- getSaatchi2011("original")
#  
# # writeRaster(all, "/data/forrest/BiomassData/Saatchi2011/Saatchi2011.OriginalResolution.nc")
# # 
# # all.10km <- aggregate(all, 10, mean)
# # 
# # writeRaster(all.10km, "/data/forrest/BiomassData/Saatchi2011/Saatchi2011.10km.nc")
# # writeNetCDF(all.10km, "biomass", "TC ha -1", time.resolution = "annual", time.units.string = "year", long.name = "", filename = "/data/forrest/BiomassData/Saatchi2011/Saatchi2011.10km.alt.nc", ordering = "standard")
# #   
# # 
# #all.50km <- aggregate(all, 50, mean)
#all.60km <- aggregate(all@data, 60, mean)
#writeRaster(all.60km, file.path(benchmarking.data.dir, "Saatchi2011/Saatchi2011.60km.nc"))

# # 
# 
# 
# 
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
# writeRaster(all.HD, file.path(benchmarking.data.dir, "Saatchi2011/Saatchi2011.HD.nc"))
# # 
# # all.T63 <- raster("/data/forrest/BiomassData/Saatchi2011/Saatchi2011.T63.con.nc")
# #                   

