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

readBaccini2012 <- function(lonlat.offset = c(0,0),
                            plot = FALSE,
                            tolerance = 0.00001,
                            london.centre=FALSE) {
  
  Baccini.data <- NULL
  Baccini.data <- read.table(baccini2012.file, header = TRUE, stringsAsFactors=FALSE)
  
  # remove any rows with NaN
  #row.has.na <- apply(Baccini.data, 1, function(x){any(is.na(x))})
  #Baccini.data <- Baccini.data[!row.has.na,]
  
  # apply offset
  Baccini.data$Lon <- Baccini.data$Lon + lonlat.offset[1]
  Baccini.data$Lat <- Baccini.data$Lat + lonlat.offset[2]
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(london.centre){
    Baccini.data$Lon <- vapply(Baccini.data$Lon, 1, FUN = LondonCentre)    
  }
  
  # add coordinates
  coordinates(Baccini.data) = ~Lon+Lat
  
  # make gridded
  gridded(Baccini.data) = TRUE
  
  # make full SpatialGridDataFrame
  Baccini.data = as(Baccini.data, "SpatialGridDataFrame") # to full grid
  
  # make raster 
  Baccini.raster <- brick(Baccini.data)
  
  # subset to get the mean layer only
  Baccini.raster <-subset(Baccini.raster, "MEAN")
  
  # convert from AGB to total
  Baccini.raster <- calc(Baccini.raster, AGBtoTotalCarbon)
  
  # divide by 10 to go from tC/Ha to kgC/m^2
  Baccini.raster <- Baccini.raster/10
  
  if(plot){
    pdf("Baccini.Data.pdf")
    print(spplot(Baccini.raster, "layer"))
    dev.off()
  }
  
  
  Baccini.dataset <- new("SpatialDataset",
                         id = "Baccini2012",
                         name = "Baccini et al. 2012 Biomass",
                         abbreviation = "Baccini et al. 2012",   
                         time.span = new("TimeSpan", name = "Baccini Period", start = 2006, end = 2008),
                         data = Baccini.raster,
                         veg.quant = getVegQuantity("cmass"),
                         units = "kgC/m^2")
  
  
  return(Baccini.dataset)
  
}

benchmarkVsBaccini<-function(model.raster, model.name="LPJ-GUESS", run = run, doScatterPlots = TRUE, doHistoPlots = TRUE){
  
  dataset.id <- "Baccini2012"
  diff.breaks <- seq(-60, 60, 1)
  plot.breaks <- seq(-35,35,1)
  histo.plot.range <- c(-20,30)
  Baccini.period = new("TimeSpan", name = "Baccini", start = 2006, end = 2008)     
  Baccini.dataset <- readBaccini2012()	
  original.data <- Baccini.dataset@data
  full.correction.data <- readNaturalLandCoverGC2009()
  
  # crop all rasters to the same size
  common.extent <- intersect(model.raster, original.data)
  model.raster <- crop(model.raster, common.extent)
  data.raster <- crop(original.data, common.extent)
  correction.raster <- crop(full.correction.data, common.extent)
  
  # Land use correction
  model.raster <- model.raster * correction.raster
  
  # Do the generic analysis	 
  comparison.results <- compareTwoRasters(stack(model.raster,data.raster),  Baccini.dataset@abbreviation, model.name, diff.breaks = diff.breaks, plot.breaks = plot.breaks, histo.plot.range = histo.plot.range,  doScatterPlots=doScatterPlots, doHistoPlots=doHistoPlots, run = run, period = Baccini.period, quant = getVegQuantity("cmass"))				
  
  rm(original.data, full.correction.data, common.extent, model.raster, data.raster,  correction.raster )
  
  return(comparison.results)
  
}


getBaccini2012 <- function(resolution = "HD") {
  
  if(resolution == "original"){
    
    tolerance = 0.00001
    
    Baccini.data <- NULL
    Baccini.data <- read.table(file.path(benchmarking.data.dir, "Baccini2012/Baccini_220912.txt"), header = TRUE, stringsAsFactors=FALSE)
    
    # remove any rows with NaN
    #row.has.na <- apply(Baccini.data, 1, function(x){any(is.na(x))})
    #Baccini.data <- Baccini.data[!row.has.na,]
    
    # add coordinates
    coordinates(Baccini.data) = ~Lon+Lat
    
    # make gridded
    gridded(Baccini.data) = TRUE
    
    # make full SpatialGridDataFrame
    Baccini.data = as(Baccini.data, "SpatialGridDataFrame") # to full grid
    
    # make raster 
    Baccini.raster <- brick(Baccini.data)
    
    # subset to get the mean layer only
    Baccini.raster <-subset(Baccini.raster, "MEAN")
    
    # convert from AGB to total
    Baccini.raster <- calc(Baccini.raster, AGBtoTotalCarbon)
    
    # divide by 10 to go from tC/Ha to kgC/m^2
    Baccini.raster <- Baccini.raster/10
    
  }
  else if(resolution == "HD"){
    Baccini.raster <- raster(file.path(benchmarking.data.dir, "Baccini2012/Baccini2012.HD.nc"))
  }
  else if(resolution == "T63"){
    Baccini.raster <- trim(rotate(raster(file.path(benchmarking.data.dir, "Baccini2012/Baccini2012.T63.nc"))))
  }
  
  
  
  Baccini.dataset <- new("SpatialDataset",
                         id = "Baccini2012",
                         name = "Baccini et al. 2012 Biomass",
                         abbreviation = "Baccini et al. 2012",   
                         time.span = new("TimeSpan", name = "Baccini Period", start = 2006, end = 2008),
                         data = Baccini.raster,
                         veg.quant = getVegQuantity("cmass"),
                         units = "kgC/m^2")
  
  
  return(Baccini.dataset)
  
}




#all <- readBaccini2012()
#writeNetCDF(all@data, "biomass", "kgC m-1", time.resolution = "annual", time.units.string = "year", long.name = "", filename = "/data/forrest/BiomassData/Baccini2012/Baccini2012.HD.nc", ordering = "standard")
