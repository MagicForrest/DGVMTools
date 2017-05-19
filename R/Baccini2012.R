#' Read Baccini et al. 2012 total biomass map
#' 
#' Total biomass across the tropics.  
#' Original dataset is 1km, also provided here at 0.5 degrees or T63.
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "HD" and "T63" and "original"
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster


getBaccini2012 <- function(location = "/data/forrest/Biomass", resolution = "HD") {
  
  
  #### NEW ORDER, CODE BELOW NOW REDUNDANT
  
  # read the standard dataset
  Baccini.dt <- readData("Baccini2012",
                       location = location,
                       resolution = resolution,
                       start.year = NULL,
                       end.year = NULL,
                       temporally.average = TRUE,
                       verbose = FALSE)
  
  
  # set the name to something equivalent in the model
  setnames(Baccini.dt, c("DATALAYER"), c("Tree"))
  
  # also set the key 
  setkey(Baccini.dt, Lon, Lat)
  
  # build the DataObject with the metadata
  Baccini.dataset <- new("DataObject",
                         id = "Baccini2012",
                         name = "Baccini et al. 2012 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Baccini Period", start = 2006, end = 2008),
                         data = Baccini.dt,
                         quant = lookupQuantity("vegC_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "BacciniExtent", name = "Baccini extent", extent = extent(Baccini.dt)),
                         correction.layer =  "")
  
  
  
  return(Baccini.dataset)
  
  
  
  
  Lon = Lat = NULL
  
  if(resolution == "original"){
    
    Baccini.data <- utils::read.table(file.path(location, "Baccini_220912.txt"), header = TRUE, stringsAsFactors=FALSE)
    
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
  setnames(Baccini.dt, c("Lon", "Lat", "Tree"))
  setkey(Baccini.dt, Lon, Lat)
  
  Baccini.dt <- stats::na.omit(Baccini.dt)
  
  
  Baccini.dataset <- new("DataObject",
                         id = "Baccini2012",
                         name = "Baccini et al. 2012 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Baccini Period", start = 2006, end = 2008),
                         data = Baccini.dt,
                         quant = lookupQuantity("vegC_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "BacciniExtent", name = "Baccini extent", extent = extent(Baccini.dt)),
                         correction.layer =  "")
  
  
  
  return(Baccini.dataset)
  
}