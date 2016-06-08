getBaccini2012 <- function(location = "/data/forrest/Biomass/Baccini2012", resolution = "original") {
  
  
  Lon = Lat = NULL
  
  if(resolution == "original"){
    
    Baccini.data <- read.table(file.path(location, "Baccini_220912.txt"), header = TRUE, stringsAsFactors=FALSE)
    
    # if london.centre is requested, make sure all negative longitudes are shifted to positive
    Baccini.data$Lon <- vapply(Baccini.data$Lon, 1, FUN = LondonCentre)    

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
    Baccini.raster <- raster(file.path(location, "Baccini2012.HD.nc"))
  }
  else if(resolution == "T63"){
    Baccini.raster <- trim(rotate(raster(file.path(location, "Baccini2012.T63.nc"))))
  }
  
  
  # convert to a date.table
  Baccini.dt <- data.table(as.data.frame(Baccini.raster,xy = TRUE))
  setnames(Baccini.dt, c("Lon", "Lat", "Baccini2012"))
  setkey(Baccini.dt, Lon, Lat)
  
  
  
  Baccini.dataset <- new("DataObject",
                         id = "Baccini2012",
                         name = "Baccini et al. 2012 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Baccini Period", start = 2006, end = 2008),
                         data = Baccini.dt,
                         quant = lookupVegQuantity("cmass"),
                         spatial.extent = new("SpatialExtent", id = "BacciniExtent", name = "Baccini extent", extent = extentFromDT(Baccini.dt)),
                         correction.layer =  "")
  
  
  
  return(Baccini.dataset)
  
}