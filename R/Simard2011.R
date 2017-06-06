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

getSimard2011 <- function(location = "/data/forrest/CanopyHeight/", resolution = "HD") {
  
  
  #### NEW ORDER, CODE BELOW NOW REDUNDANT
  
  # read the standard dataset
  Simard.dt <- readData("Simard2011",
                         location = location,
                         resolution = resolution,
                         start.year = NULL,
                         end.year = NULL,
                         temporally.average = TRUE,
                         verbose = FALSE)

  # set the name to something equivalent in the model
  setnames(Simard.dt, c("DATALAYER"), c("CanHght"))
  
  # also set the key 
  setkey(Simard.dt, Lon, Lat)
  
  # build the DataObject with the metadata
  Simard.dataset <- new("DataObject",
                         id = "Simard2011",
                         name = "Simard et al. 2011 Canopy Height",
                         temporal.extent = new("TemporalExtent", name = "Simard Period", start = 2003, end = 2009),
                         data = Simard.dt,
                         quant = lookupQuantity("canopyheight_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "SimardExtent", name = "Simard extent", extent(Simard.dt)),
                         correction.layer =  "")

  return(Simard.dataset)
  
  

  Lon = Lat = NULL
    
  if(resolution == "original"){
    
    stop("Simard 2011 data not currently available at original resolution")
    
  }
  else if(resolution == "1D"){
    dataset.raster <- raster::raster(file.path(location, "Simard2011.1D.nc"))
  }
  else if(resolution == "HD"){
    dataset.raster <- raster::raster(file.path(location, "Simard2011.HD.nc"))
  }
  else if(resolution == "T63"){
    stop("Simard 2011 data not currently available at T63 resolution")
  }
  
  print(dataset.raster)
  
  dataset.dt <- data.table(as.data.frame(dataset.raster,xy = TRUE))
  dataset.dt <- stats::na.omit(dataset.dt)
  print(dataset.dt)
  setnames(dataset.dt, c("Lon", "Lat", "CanHght"))
  setkey(dataset.dt, Lon, Lat)

  
  
  dataset.dataset <- new("DataObject",
                         id = "Simard2011",
                         name = "Simard et al. 2011 Canopy Height",
                         temporal.extent = new("TemporalExtent", name = "Simard Period", start = 2003, end = 2009),
                         data = dataset.dt,
                         quant = lookupQuantity("canopyheight_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "SimardExtent", name = "Simard extent", extent(dataset.dt)),
                         correction.layer =  "")
  
  
  
  return(dataset.dataset)
  
}

