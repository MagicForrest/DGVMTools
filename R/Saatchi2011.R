#' Read Saatchi et al. 2015 total biomass map
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

getSaatchi2011 <- function(location = "/data/forrest/Biomass/", resolution = "HD") {
  
  
  #### NEW ORDER, CODE BELOW NOW REDUNDANT
  
  # read the standard dataset
  Saatchi.dt <- readData("Saatchi2011",
                         location = location,
                         resolution = resolution,
                         start.year = NULL,
                         end.year = NULL,
                         temporally.average = TRUE,
                         verbose = FALSE)
  
  # set the name to something equivalent in the model
  setnames(Saatchi.dt, c("DATALAYER"), c("Tree"))
  
  # also set the key 
  setkey(Saatchi.dt, Lon, Lat)
  
  # build the DataObject with the metadata
  Saatchi.dataset <- new("DataObject",
                         id = "Saatchi2011",
                         name = "Saatchi et al. 2011 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Saatchi Period", start = 1999, end = 2001),
                         data = Saatchi.dt,
                         quant = lookupQuantity("vegC_std", "Standard"),
                         spatial.extent = new("SpatialExtent", id = "SaatchiExtent", name = "Saatchi extent", extent = extent(Saatchi.dt)),
                         correction.layer =  "")
  
  
  
  return(Saatchi.dataset)
  
}
