#' Read HandPBiomes et al. 2015 total biomass map
#' 
#' Total biomass across the tropics.  
#' Original dataset is 1km, also provided here at 0.5 degrees or T63.
#'  
#' @param location A character string specifying the the location of the original data files
#' @param resolution A character string specifying the resolution, currently supported
#' are "HD" and "T63" and "original"
#' @param classification A character string describing how to aggregate and sort the biomes.  Can be "Smith2014", "Forrest2015", "Megabiomes_dev" or "original" (which follows Hickler et al. 2006)
#' 
#' Should be moved to DGVMDatasets, not distributed with DGVMTools
#' 
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster

getHandPBiomes <- function(location = "/data/forrest/Biomes/", resolution = "HD", classification = "Smith2014") {
  
  Lon = Lat = NULL
  
  
  #### NEW ORDER, CODE BELOW NOW REDUNDANT
  
  # read the standard dataset
  HandPBiomes.dt <- readData("HandPBiomes",
                         location = location,
                         resolution = resolution,
                         start.year = NULL,
                         end.year = NULL,
                         temporally.average = TRUE,
                         verbose = TRUE)
  
  # set the name to something equivalent in the model
  setnames(HandPBiomes.dt, c("DATALAYER"), c(classification))
  
  # from above orderings
  if(classification == "Smith2014") {
    #subs.rules <- c(5,4,8,9,7,6,9,3,1,2,11,12,14,15,10,16,17,13)
    subs.rules <- c("Boreal Deciduous Forest/Woodland",
                    "Boreal Evergreen Forest/Woodland",
                    "Temperate/Boreal Mixed Forest",
                    "Temperate Mixed Forest",
                    "Temperate Deciduous Forest",
                    "Temperate Broadleaved Evergreen Forest",
                    "Temperate Mixed Forest",
                    "Tropical Seasonal Forest",
                    "Tropical Rain Forest",
                    "Tropical Deciduous Forest",
                    "Moist Savanna",
                    "Dry Savanna",
                    "Tall Grassland",
                    "Dry Grassland",
                    "Xeric Woodland/Shrubland",
                    "Arid Shrubland/Steppe",
                    "Desert",
                    "Arctic/Alpine Tundra")
    classification.str <- "Smith et al. 2014"
  }
  else if(classification == "Forrest2015"){
    subs.rules <- c(4,4,3,3,3,2,3,1,1,5,5,6,6,6,5,6,8,7)
    classification.str <- "Forrest et al. 2015"
  }
  # MF: Note: This is a potentially useful Megabiomes scheme 
  else if(classification == "Megabiomes_dev"){
    #subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,3,1,2,9,10,12,13,8,13,14,11))  
    #subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,1,1,2,9,10,12,13,8,13,14,11))
    subs.rules <- c(4,3,6,5,6,5,7,1,1,2,8,9,11,12,7,12,13,10)
    #subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,2,1,2,9,9,12,13,8,13,14,11))
  }
  else if(classification == "original"){
    subs.rules <- 1:18
    classification.str <- "Hickler et al. 2006"
  }
  
  
  HandPBiomes.dt[, paste(classification) := as.factor(plyr::mapvalues(HandPBiomes.dt[[classification]], from = 1:18, to = subs.rules))]
  HandPBiomes.dt <- na.omit(HandPBiomes.dt)
  
  
  # also set the key 
  setkey(HandPBiomes.dt, Lon, Lat)
  
  # build the DataObject with the metadata
  HandPBiomes.dataset <- new("DataObject",
      id =  paste("HandPBiomes", classification, sep = "."),
      name = paste("H&P PNV Biomes classified as in", classification, sep = " "),
      data = HandPBiomes.dt,
      quant = as(byIDfromList(classification, supported.biome.schemes), "Quantity"),
      spatial.extent = new("SpatialExtent", id = "PNVExtent", name = "PNV Extent", extent = extent(HandPBiomes.dt)),
      temporal.extent = new("TemporalExtent", id = "PNVPeriod", name = "PNV Period", start = 1961, end = 1990),
      correction.layer =  ""
  )
  
  
  return(HandPBiomes.dataset)
  
}
