

readHYDE <- function(location = "/home/forrest/Data/LandUseLandCover/HYDE/netCDF", layer,  temporal.extent) {
  
  # Check input and set meta-data accordingly
  
  if(layer == "CropFraction") {
    
    Quant <- new("Quantity",
                 id = "CropFraction",
                 name = "Crop Fraction",
                 type = "-",
                 units = "",
                 colours = gray.colors,
                 cuts = seq(0, 1, 0.05),
                 model = c("Data"))
    
    name <- "HYDE Crop Fraction" 
    file.string <- layer
  }
  
  else if(layer == "PastureFraction"){
    
    Quant <- new("Quantity",
                 id = "PastureFraction",
                 name = "Pasture Fraction",
                 type = "-",
                 units = "",
                 colours = gray.colors,
                 cuts = seq(0, 1, 0.05),
                 model = c("Data"))
    
    name <- "HYDE Pasture Fraction" 
    file.string <- layer
    
  }
  
  else if(layer == "PopulationDensity"){
    
    Quant <- new("Quantity",
                 id = "PopulationDensity",
                 name = "PopulationDensity",
                 type = "-",
                 units = "",
                 colours = fields::tim.colors,
                 cuts = seq(0, 1000, 10),
                 model = c("Data"))
    
    name <- "HYDE Pasture Fraction" 
    file.string <- "Population_Density"
    
  }
  
  
  
  
  # get pasture and and crop cover masks (and interpolate to this particular model grid)
  print(file.path("/home/forrest/Data/LandUseLandCover/HYDE/netCDF", paste0(file.string, "_AH_1500-2013.nc")))
  HYDE.nc <- nc_open(file.path("/home/forrest/Data/LandUseLandCover/HYDE/netCDF", paste0(file.string, "_AH_1500-2013.nc")), readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  this.lat <- ncvar_get(HYDE.nc,"Lat",verbose=verbose)
  this.lon <- ncvar_get(HYDE.nc,"Lon",verbose=verbose)
  this.time <- ncvar_get(HYDE.nc,"Time",verbose=verbose)
  
  # get relevant the time period 
  start.index <- 1
  end.index <- length(this.time)
  for(time in 1:length(this.time)) {
    if(this.time[time] <= comparison.period@start) start.index <- time
  }
  for(time in (length(this.time)-1):0) {
    if(this.time[time+1] > comparison.period@end) end.index <- time
  }
  
  HYDE.slice <- ncvar_get(HYDE.nc, start = c(start.index,1,1), count = c((end.index-start.index+1),-1,-1))
  HYDE.slice <- apply(HYDE.slice, c(2,3), mean)
  dimnames(HYDE.slice) <- list(this.lon, this.lat)
  HYDE.dt <- as.data.table(melt(HYDE.slice))
  HYDE.dt <- na.omit( HYDE.dt)
  setnames(HYDE.dt, c("Lon", "Lat", layer))
  setkey(HYDE.dt, Lon, Lat)
  
  
  
  HYDE.dataset <- new("DataObject",
                         id = paste0("HYDE.", layer),
                         name = "HYDE",
                         temporal.extent = temporal.extent,
                         data = HYDE.dt,
                         quant = Quant,
                         spatial.extent = new("SpatialExtent", id = "HYDEExtent", name = "HYDE extent", extent = getExtentFromDT(HYDE.dt)),
                         correction.layer =  "")
  
  return(HYDE.dataset)
  
}
