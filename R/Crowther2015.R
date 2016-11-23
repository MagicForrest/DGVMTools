#' Read the tree count/tree density of Crwother et al. (2015)
#' 
#' @param location path to the files
#' @param resolution string of resolutions. So far 'original' (tree count, very large 1km^2!) and 'HD' (trees/ha, 0.5deg) are available.
#' @return A \code{DataObject}
#' 
#' @importFrom  raster raster as.data.table
#' @import data.table
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' 
getCrowther2015 <- function(location="", resolution="HD") {
  Lat=Lon=NULL

  if(resolution == "original") {
    warning("Reading a very big file. Be patient!")
    message("Reading a very big file. Be patient!")
    Crowther.raster <- raster(file.path(location, "Crowther_Nature_Files_Revision_01_WGS84_GeoTiff", "Crowther_Nature_Biome_Revision_01_WGS84_GeoTiff.tif"))
  } else {
    Crowther.raster <- raster(file.path(location, paste0("Crowther2015.", resolution, ".nc")))
  }

  Crowther.dt <- data.table(as.data.frame(Crowther.raster,xy = TRUE))
  setnames(Crowther.dt, c("Lon", "Lat", "Crowther2015"))
  setkey(Crowther.dt, Lon, Lat)

  Crowther.dataset <- new("DataObject",
                         id = "Crowther2015",
                         name = "Crowther et al. 2015 Tree density",
                         temporal.extent = new("TemporalExtent", name = "Crowther Period", start = 2006, end = 2008),
                         data = Crowther.dt,
                         quant = lookupQuantity("vegC_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "CrowtherExtent", name = "Crowther extent", extent = extent(Crowther.dt)),
                         correction.layer =  "")
  return(Crowther.dataset)
}