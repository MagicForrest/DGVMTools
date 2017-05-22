#' Read MODIS TreeCover map
#' 
#' Blah
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "1km-Turkey" and "HD"
#' @param start First year of data to read (only for "HD" not for "1km-Turkey" which is fixed between 2000 and 2010). Data starts in 2000.
#' @param end Last year of data to read (only for "HD" not for "1km-Turkey" which is fixed between 2000 and 2010). Data ends in 2015
#' @param average If true, temporally average the years
#' @param layer Which layer do you want? Either "treecover", "bare" or "other" (Only for "HD", "other" means low vegetation < 5m)
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster

getMODISTreeCover <- function(location = "/data/forrest/TreeCover/MOD44B/Collection5", resolution = "original", start = 2000, end = 2015, average = TRUE, layer = "treecover") {

  Lon = Lat = Tree = NULL
    
  if(resolution == "original"){
    
  }
  if(resolution == "HD"){
    raster <- raster::brick("/home/forrest/Data/TreeCover/MOD44B/v051/processed/MOD44B.2000-2015.HD.nc", varname = layer)
    if(start != 2000 | end != 2015) raster <- subset(raster, (start-1999):(end-1999))
    if(average) raster <- raster::calc(raster, mean)
    t.extent <- new("TemporalExtent", id = "MODISPeriod", name = "MODIS Period", start = start, end = end)
    s.extent.id <- "Global"
    s.extent.name <- "Global"
  }
  else if(resolution == "1km-Turkey"){
    raster <- raster::trim(raster::rotate(raster::raster(file.path(location, "processed/MODISTreeCover.2000-2010.1km-Turkey.nc"))))
    t.extent <- new("TemporalExtent", id = "MODISPeriod", name = "MODIS Period", start = 2000, end = 2010)
    s.extent.id = "TurkeyExtent"
    s.extent.name = "Turkey 1km extent"
  }
  
  
  dt <- data.table(as.data.frame(raster,xy = TRUE))
  setnames(dt, c("Lon", "Lat", "Tree"))
  dt[,Lon:= round(Lon, 3)]
  dt[,Lat:= round(Lat, 3)]

  # Any values of 200.0 mean water, set these to NA 
  dt[Tree > 110.0] <- NA
  
  setkey(dt, Lon, Lat)

  quant <- lookupQuantity("vegcover_std", "Standard")
  
  dataset <- new("DataObject",
                         id = "MODISTreeCover",
                         name = "MODIS MOD44B",
                         temporal.extent = t.extent,
                         data = dt,
                         quant = quant,
                         spatial.extent = new("SpatialExtent",  id  = s.extent.id, name = s.extent.name, extent = extent(dt)),
                         correction.layer =  "")
  
  
  
  return(dataset)
  
}

