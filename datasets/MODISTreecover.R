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

modis.treecover.each.year <-  file.path(benchmarking.data.dir, "MODISTreecover/VCF_r0.500000_")


getMODISTreecover <- function(resolution = "HD", first.year = 2000, last.year = 2010) {
  
  
  if(resolution == "HD"){
    # if we want the whole time period, here it is
    if(first.year == 2000 & last.year == 2010){
      MODIS.treecover.data <- raster(file.path(benchmarking.data.dir, "/MODISTreecover/VCF_2000-2010avg.nc"))/100
      
    }
    
    # else average between certain years
    else{
      all.years <- brick()
      for(year in first.year:last.year){
        all.years <- addLayer(all.years, raster(paste(modis.treecover.each.year, year, ".nc", sep = "")))
      }
      MODIS.treecover.data <- calc(all.years, mean)/100
    }
  }
  else if(resolution == "T63"){
    
    # if we want the whole time period, here it is
    if(first.year == 2000 & last.year == 2010){
      MODIS.treecover.data <- rotate(raster(file.path(benchmarking.data.dir, "/MODISTreecover/VCF_2000-2010avg.T63.nc")))/100
      
    }
    
    # else average between certain years
    else{
      all.years <- brick()
      for(year in first.year:last.year){
        all.years <- addLayer(all.years, raster(paste(modis.treecover.each.year, year, ".nc", sep = "")))
      }
      MODIS.treecover.data <- calc(all.years, mean)/100
      stop("MODISTreccover:  Not available at that resolution/time period combination")
    }
    
  }
  
  
  MODIS.treecover.dataset <- new("SpatialDataset",
                                 id = "MODIS.Treecover",
                                 name = "MODIS Treecover",
                                 abbreviation = "MODIS VCF",   
                                 time.span = new("TimeSpan", name = "MODIS Treecover Period", start = first.year, end = last.year),
                                 data = MODIS.treecover.data,
                                 veg.quant = getVegQuantity("vegcover"),
                                 units = "fraction of gridcell")
  
  
  return(MODIS.treecover.dataset)
  
  
  
  
}


benchmarkVsMODISTreecover<-function(model.raster, model.name="LPJ-GUESS", run = run, doScatterPlots = TRUE, doHistoPlots = TRUE, region = NULL){
  
  dataset.id <- "MODISTreecover"
  diff.breaks <- seq(-2, 2, 0.05)
  plot.breaks <- seq(-1,1,0.02)
  histo.plot.range <- c(-1, 1.5)
  MODISTreecover.period = new("TimeSpan", name = "MODISTreecover", start = 2000, end = 2010)                                                                                                                 
  original.data <- readMODISTreecover()	 
  full.correction.data <- readNaturalLandCoverGC2005()
  
  # crop all rasters to the same size
  common.extent <- intersect(model.raster, original.data)
  model.raster <- crop(model.raster, common.extent)
  data.raster <- crop(original.data, common.extent)
  correction.raster <- crop(full.correction.data, common.extent)
  
  # Land use correction
  model.raster <- model.raster * correction.raster
  
  # Crop to desired region if specified
  if(!is.null(region)){
    model.raster <- crop(model.raster, region)
    data.raster <- crop(data.raster, region)
  }
  
  
  #ignore.raster <- correction.raster
  #ignore.raster[] <- 1
  #ignore.raster[correction.raster < 0.9] <- NA
  
  # Do the generic analysis	 
  comparison.results <- compareTwoRasters(stack(model.raster, data.raster),  dataset.id, model.name, diff.breaks = diff.breaks, plot.breaks = plot.breaks,  histo.plot.range = histo.plot.range, doScatterPlots=doScatterPlots, doHistoPlots=doHistoPlots, run = run, period = MODISTreecover.period, quant = getVegQuantity("vegcover"), par.settings = list(panel.background=list(col="grey")))				
  
  rm(original.data, full.correction.data, common.extent, model.raster, data.raster,  ignore.raster )
  
  return(comparison.results)
  
}