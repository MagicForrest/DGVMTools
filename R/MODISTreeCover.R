#' Read MODIS TreeCover map
#' 
#' Blah
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "1km-Turkey"
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{SpatialDataset} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster

getMODISTreeCover <- function(location = "/data/forrest/TreeCover/MOD44B/Collection5", resolution = "original") {

  Lon = Lat = NULL
    
  if(resolution == "original"){
    
  }
  else if(resolution == "1km-Turkey"){
    raster <- trim(rotate(raster(file.path(location, "processed/MODISTreeCover.2000-2010.1km-Turkey.nc"))))
  }
  
  
  dt <- data.table(as.data.frame(raster,xy = TRUE))
  setnames(dt, c("Lon", "Lat", "MODISTreeCover"))
  dt[,Lon:= round(Lon, 3)]
  dt[,Lat:= round(Lat, 3)]

  setkey(dt, Lon, Lat)

  quant <- lookupVegQuant("vegcover_std", "Standard")
  
  dataset <- new("DataObject",
                         id = "MODISTreeCover",
                         name = "MODIS MOD44B Tree",
                         temporal.extent = new("TemporalExtent", id = "MODISPeriod", name = "MODIS Period", start = 2000, end = 2010),
                         data = dt,
                         quant = quant,
                         spatial.extent = new("SpatialExtent", id = "TurkeyExtent", name = "Turkey 1km extent", extent = getExtentFromDT(dt)),
                         correction.layer =  "")
  
  
  
  return(dataset)
  
}

