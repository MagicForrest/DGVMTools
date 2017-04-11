

############# EXAMPLE DATA SET ONE - POTENTIAL NATURAL VEGETATION BIOMES
#' Read Haxeltine and Prentice 1996 PNV biome map
#' 
#' An example dataset of distribution of global potential natural vegetation classified into biomes.
#' 
#' @param resolution A character string specifying the resolution, currently supported
#' are "HD", "T63" and "original" (original is at half degree so equivalent to "HD")
#' @param classification A character string specifying how the biomes should be classified. 
#' Options are "original" (direct as per the H&P 1996 map), "Smith2014" (following Smith et al. 2014 and the ordering
#' following the figure in that paper) and "Forrest2015" (an aggregated megabiomes scheme based and Harrsion and Presntice 2006,
#' see the supplementary material in Forrest et al. 2015 for details and ordering).
#' 
#' This dataset has been used in various publications, but just because we include it here 
#' doesn't mean we specifically recommend it's use.  It is included here as an example, judgement of the scientific merit is left 
#' to the user.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table raster
#' @importFrom plyr mapvalues

readHandPBiomes <- function(resolution = "HD", classification = "Smith2014"){
  
  dataset.id = "HandPBiomes"
  
  Lon = Lat = Year = NULL
  
  original.data <- system.file("extdata", "vegmap18_fromTH_Hickler2006.out", package = "DGVMTools")
  HD.data <- system.file("extdata", "vegmap18_fromTH_Hickler2006.HD.nc", package = "DGVMTools")
  T63.data <- system.file("extdata", "vegmap18_fromTH_Hickler2006.T63.nc", package = "DGVMTools")
  
  # If original is required, open the origina text file
  if(resolution == "original"){
    
    PNV.dt <- fread(original.data, sep = " ", header=T)
    
    # divide Lon and Lat by 10, offset and London centre
    PNV.dt[,Lon := (Lon/10) + 0.25]
    PNV.dt[,Lat := (Lat/10) + 0.25]
    PNV.dt[, Lon := vapply(PNV.dt[,Lon], 1, FUN = LondonCentre)]   
    
  }
  # Return half degree data from netCDF file (probably slightly faster)
  else if(resolution == "HD"){
    PNV.raster <- raster(HD.data)
    PNV.dt <- data.table(as.data.frame(PNV.raster,xy = TRUE))
  }
  # Return T63 data from netCDF file 
  else if(resolution == "T63"){
    PNV.raster <- raster(T63.data)
    PNV.raster <- rotate(PNV.raster)
    PNV.dt <- data.table(as.data.frame(PNV.raster,xy = TRUE))
  }
  # Else default to original resolution
  else{
    message(paste("readPNVBiomes: Unknown resolution ", resolution, "returned half degree (native resolution)"))
    PNV.raster <- raster(HD.data)
    PNV.dt <- data.table(as.data.frame(PNV.raster,xy = TRUE))
  }
  
  # set names and key
  setnames(PNV.dt, c("Lon", "Lat", paste(classification)))
  setkey(PNV.dt, Lon, Lat)
  
  
  ################### Smith2014 ##########################
  
  #           ORIGINAL ORDERING IN DATA (HICKLER ET AL. 2006)     SMITH ET AL. 2014   
  # BIOME 1 - Boreal deciduous forest/woodland                    BIOME 5
  # BIOME 2 - Boreal evergreen forest/woodland                    BIOME 4
  # BIOME 3 - Temperate/Boreal mixed forest                       BIOME 8
  # BIOME 4 - Temperate conifer forest                            BIOME 9
  # BIOME 5 - Temperate deciduous forest                          BIOME 7
  # BIOME 6 - Temperate broadleaved evergreen forest              BIOME 6
  # BIOME 7 - Temperate mixed forest                              BIOME 9
  # BIOME 8 - Tropical seasonal forest                            BIOME 3
  # BIOME 9 - Tropical rain forest                                BIOME 1
  # BIOME 10 - Tropical Deciduous forest                          BIOME 2
  # BIOME 11 - Moist Savannas                                     BIOME 11
  # BIOME 12 - Dry Savannas                                       BIOME 12
  # BIOME 13 - Tall Grassland                                     BIOME 14
  # BIOME 14 - Dry Grassland                                      BIOME 15
  # BIOME 15 - Xeric woodland/shrub                               BIOME 10
  # BIOME 16 - Arid shrubland/steppe                              BIOME 16
  # BIOME 17 - Desert                                             BIOME 17
  # BIOME 18 - Arctic/alpine tundra                               BIOME 13
  
  ################### Megabiomes_dev ##########################
  
  #           ORIGINAL ORDERING IN DATA (HICKLER ET AL. 2006)     NEW ORDERING FOR 'MEGA BIOMES'    
  # BIOME 1 - Boreal deciduous forest/woodland                    BIOME 5
  # BIOME 2 - Boreal evergreen forest/woodland                    BIOME 4
  # BIOME 3 - Temperate/Boreal mixed forest                       BIOME 7 - Temperate Deciduous
  # BIOME 4 - Temperate conifer forest                            BIOME 6 - Temperate Evergreen
  # BIOME 5 - Temperate deciduous forest                          BIOME 7 - Temperate Deciduous
  # BIOME 6 - Temperate evergreen forest                          BIOME 6 - Temperate Evergreen
  # BIOME 7 - Temperate mixed forest                              BIOME 7 - Temperate Deciduoud
  # BIOME 8 - Tropical seasonal forest                            BIOME 3
  # BIOME 9 - Tropical rain forest                                BIOME 1
  # BIOME 10 - Tropical Deciduous forest                          BIOME 2
  # BIOME 11 - Moist Savannas                                     BIOME 9
  # BIOME 12 - Dry Savannas                                       BIOME 10
  # BIOME 13 - Tall Grassland                                     BIOME 12 
  # BIOME 14 - Dry Grassland                                      BIOME 13 - Arid shrubland/grasslands
  # BIOME 15 - Xeric woodland/shrub                               BIOME 8  
  # BIOME 16 - Arid shrubland/steppe                              BIOME 13 - Arid shrubland/grasslands
  # BIOME 17 - Desert                                             BIOME 14
  # BIOME 18 - Arctic/alpine tundra                               BIOME 11
  
  ################### Megabiomes (Harrison and Prentice) ##########################
  
  #           ORIGINAL ORDERING IN DATA (HICKLER ET AL. 2006)     NEW ORDERING FOR 'MEGA' PALEO BIOMES    
  # BIOME 1 - Boreal deciduous forest/woodland                    BIOME 4 - Boreal Forest
  # BIOME 2 - Boreal evergreen forest/woodland                    BIOME 4 - Boreal Forest
  # BIOME 3 - Temperate/Boreal mixed forest                       BIOME 3 - Temperate Deciduous
  # BIOME 4 - Temperate conifer forest                            BIOME 3 - Temperate Deciduous
  # BIOME 5 - Temperate deciduous forest                          BIOME 3 - Temperate Deciduous
  # BIOME 6 - Temperate evergreen forest                          BIOME 2 - Temperate Evergreen
  # BIOME 7 - Temperate mixed forest                              BIOME 3 - Temperate Deciduous
  # BIOME 8 - Tropical seasonal forest                            BIOME 1 - Tropical Forest
  # BIOME 9 - Tropical rain forest                                BIOME 1 - Tropical Forest
  # BIOME 10 - Tropical Deciduous forest                          BIOME 5 - Savanna and dry woodlands
  # BIOME 11 - Moist Savannas                                     BIOME 5 - Savanna and dry woodlands
  # BIOME 12 - Dry Savannas                                       BIOME 6 - Grasslands and dry shrublands
  # BIOME 13 - Tall Grassland                                     BIOME 6 - Grasslands and dry shrublands
  # BIOME 14 - Dry Grassland                                      BIOME 6 - Arid shrubland/grasslands
  # BIOME 15 - Xeric woodland/shrub                               BIOME 5 - Savanna and dry woodlands  
  # BIOME 16 - Arid shrubland/steppe                              BIOME 6 - Arid shrubland/grasslands
  # BIOME 17 - Desert                                             BIOME 8 - Desert
  # BIOME 18 - Arctic/alpine tundra                               BIOME 7 - Tundra
  
  
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
  
  
  PNV.dt[, paste(classification) := as.factor(plyr::mapvalues(PNV.dt[[classification]], from = 1:18, to = subs.rules))]
  PNV.dt <- na.omit(PNV.dt)

 
  return(
    new("DataObject",
        id =  paste(dataset.id, classification, sep = "."),
        name = paste("H&P PNV Biomes classified as in", classification.str, sep = " "),
        data = PNV.dt,
        quant = as(byIDfromList(classification, supported.biome.schemes), "Quantity"),
        spatial.extent = new("SpatialExtent", id = "PNVExtent", name = "PNV Extent", extent = extent(PNV.dt)),
        temporal.extent = new("TemporalExtent", id = "PNVPeriod", name = "PNV Period", start = 1961, end = 1990),
        correction.layer =  ""
    )
  )
  
  
  
}

############# EXAMPLE DATA SET TWO - TROPICAL BIOMASS
#' Read Saatchi et al. 2011 total biomass map
#' 
#' An example dataset of distribution total biomass across the tropics.  
#' Original dataset is 1km, provided here at 0.5 degrees or T63.
#'  
#' @param resolution A character string specifying the resolution, currently supported
#' are "HD" and "T63"
#' 
#' This dataset has been used in various publications, but just because we include it here 
#' doesn't mean we specifically recommend it's use.  It is included here as an example, judgement of the scientific merit is left 
#' to the user.
#' @return A \code{DataObject} object.  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
getSaatchi2011_example <- function(resolution = "HD"){
  
  Lat = Lon = NULL
  
  if(resolution == "T63"){
    Saatchi.raster <- trim(rotate(raster(system.file("extdata", "Saatchi2011.T63.nc", package = "DGVMTools"))/10))
  }
  else if(resolution == "HD"){
    Saatchi.raster <- trim(raster(system.file("extdata", "Saatchi2011.HD.nc", package = "DGVMTools"))/10)
  }
  
  Saatchi.dt <- data.table(as.data.frame(Saatchi.raster,xy = TRUE))
  setnames(Saatchi.dt, c("Lon", "Lat", "Tree"))
  setkey(Saatchi.dt, Lon, Lat)
  Saatchi.dt <- na.omit(Saatchi.dt)
  
  
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
