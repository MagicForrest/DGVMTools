#' Read HYDE land use dataset
#' 
#' Reads either croplands fraction, pasture fraction or population density from the HYDE land use dataset (0.5 degree)
#' @param location A character string defining where on the disk the data is.  
#' @param layer A character string, should be one of "CropFraction", "PastureFraction" or "PopulationDensity"
#' @param temporal.extent A TemporalExtent object to define the period you want the data for.  It will be averaged between these years.
#' @param verbose Logical, if TRUE spew forth a lot of output to the screen
#'
#' @details This functions reads some netCDF files (not the original HYDE data), produced by the author and available on request. 
#'
#' @return A DataObject
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 

readHYDE <- function(location = "/home/forrest/Data/LandUseLandCover/HYDE/netCDF", layer,  temporal.extent, verbose = FALSE) {
  
  
  Lon = Lat =  NULL
  
  # Check input and set meta-data accordingly
  
  if(layer == "CropFraction") {
    
    Quant <- new("Quantity",
                 id = "CropFraction",
                 name = "Crop Fraction",
                 type = "-",
                 units = "",
                 colours = grDevices::gray.colors,
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
                 colours = grDevices::gray.colors,
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
    if(this.time[time] <= temporal.extent@start) start.index <- time
  }
  for(time in (length(this.time)-1):0) {
    if(this.time[time+1] > temporal.extent@end) end.index <- time
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
                         spatial.extent = new("SpatialExtent", id = "HYDEExtent", name = "HYDE extent", extent = extent(HYDE.dt)),
                         correction.layer =  "")
  
  return(HYDE.dataset)
  
}
