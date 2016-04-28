
processGlobcover2009 <- function(location = "/data/forrest/LandUseLandCover/Globcover2009/", resolution = "HD"){
  
  if(resolution == "HD") target.grid <- raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90)
  else if(resolution == "QD") target.grid <- raster(nrows=720, ncols=1440, xmn=-180, xmx=180, ymn=-90, ymx=90)
  
  # hard coded categories
  file.name <- "GLOBCOVER_L4_200901_200912_V2.3.tif"
  categories <- c(11,14,20,30,40,50,60,70,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230)
  input.raster <- raster(file.path(location, file.name))
  
  GlobCover2009.aggregated <- countCategoricalData(input.raster, target.grid, categories)
 
}


