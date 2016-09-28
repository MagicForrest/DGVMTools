#' Read dataset et al. 2015 total biomass map
#' 
#' Global average top of canopy height
#' Original dataset is 1km, but provided here at 0.5 degrees.
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "HD".
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster

getSimard2011 <- function(location = "/data/forrest/CanopyHeight/Simard2011/", resolution = "HD") {

  Lon = Lat = NULL
    
  if(resolution == "original"){
    
    stop("Simard 2011 data not currently available at original resolution")
    
  }
  else if(resolution == "HD"){
    dataset.raster <- raster(file.path(location, "Simard2011.HD.nc"))
  }
  else if(resolution == "T63"){
    stop("Simard 2011 data not currently available at T63 resolution")
  }
  
  
  dataset.dt <- data.table(as.data.frame(dataset.raster,xy = TRUE))
  dataset.dt <- na.omit(dataset.dt)
  setnames(dataset.dt, c("Lon", "Lat", "Simard2011"))
  setkey(dataset.dt, Lon, Lat)

  
  
  dataset.dataset <- new("DataObject",
                         id = "Simard2011",
                         name = "Simard et al. 2011 Canopy Height",
                         temporal.extent = new("TemporalExtent", name = "Simard Period", start = 2005, end = 2005),
                         data = dataset.dt,
                         quant = lookupQuantity("canopyheight_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "SimardExtent", name = "Simard extent", extent = getExtentFromDT(dataset.dt)),
                         correction.layer =  "")
  
  
  
  return(dataset.dataset)
  
}

