

############# EXAMPLE DATA SET ONE - POTENTIAL NATURAL VEGETATION BIOMES


readHandPBiomes <- function(resolution = "HD", classification = "Smith2014"){
  
  Lon = Lat = Year = NULL
  
  original.data <- system.file("extdata", "vegmap18_fromTH_Hickler2006.out", package = "RVCTools")
  HD.data <- system.file("extdata", "vegmap18_fromTH_Hickler2006.HD.nc", package = "RVCTools")
  T63.data <- system.file("extdata", "vegmap18_fromTH_Hickler2006.T63.nc", package = "RVCTools")
  
  # If original is required, open the origina text file
  if(resolution == "original"){
   
    PNV.dt <- fread(original.data, sep = "", header=T)
    
    # divide Lon and Lat by 10, offset and London centre
    PNV.dt[,Lon := (Lon/10) + 0.25]
    PNV.dt[,Lat := (Lat/10) + 0.25]
    PNV.dt[, Lon := vapply(PNV.dt[,Lon], 1, FUN = LondonCentre)]    
    
    # Make into a raster
    PNV.spdf <- makeSPDFfromDT(PNV.dt, tolerance = 0.00001)
    PNV.raster <- raster(PNV.spdf, "Biome")
  }
  # Return half degree data from netCDF file (probably slightly faster)
  else if(resolution == "HD"){
    PNV.raster <- raster(HD.data)
  }
  # Return T63 data from netCDF file 
  else if(resolution == "T63"){
    PNV.raster <- raster(T63.data)
    PNV.raster <- rotate(PNV.raster)
  }
  # Else default to original resolution
  else{
    message(paste("readPNVBiomes: Unknown resolution ", resolution, "returned half degree (native resolution)"))
    PNV.raster <- raster(HD.data)
  }
  
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
    subs.rules <- data.frame(id=1:18, v=c(5,4,8,9,7,6,9,3,1,2,11,12,14,15,10,16,17,13))
  }
  else if(classification == "Forrest2015"){
    subs.rules <- data.frame(id=1:18, v=c(4,4,3,3,3,2,3,1,1,5,5,6,6,6,5,6,8,7))
  }
  # MF: Note: This is a potentially useful Megabiomes scheme 
  else if(classification == "Megabiomes_dev"){
    #subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,3,1,2,9,10,12,13,8,13,14,11))  
    #subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,1,1,2,9,10,12,13,8,13,14,11))
    subs.rules <- data.frame(id=1:18, v=c(4,3,6,5,6,5,7,1,1,2,8,9,11,12,7,12,13,10))
    #subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,2,1,2,9,9,12,13,8,13,14,11))
  }
  else if(classification == "original"){
    subs.rules <- data.frame(id=1:18, v=1:18)
  }
  PNV.raster <- subs(PNV.raster, subs.rules)
  
  return(PNV.raster)
  
}




getSaatchi2011 <- function(resolution = "HD"){
  
  
  if(resolution == "T63"){
    Saatchi.raster <- trim(rotate(raster(system.file("extdata", "Saatchi2011.T63.nc", package = "RVCTools"))/10))
  }
  else if(resolution == "HD"){
    Saatchi.raster <- trim(raster(system.file("extdata", "Saatchi2011.HD.nc", package = "RVCTools"))/10)
  }
  
  Saatchi.dataset <- new("SpatialDataset",
                         id = "Saatchi2011",
                         name = "Saatchi et al. 2011 Biomass",
                         temporal.extent = new("TemporalExtent", name = "Saatchi Period", start = 1999, end = 2001),
                         data = Saatchi.raster,
                         veg.quant = lookupVegQuantity("cmass"),
                         units = "kgC/m^2")
  
  
  return(Saatchi.dataset)
  
}
