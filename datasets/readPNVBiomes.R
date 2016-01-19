#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
#####################################

require("raster")


readPNVBiomes <- function(resolution = "HD", classification = "Smith2014", plot.dir = "."){
  
  
  original.data <- file.path(auxiliary.data.dir, "GlobalBiomes/HaxeltineAndPrentice1996/vegmap18_fromTH_Hickler2006.out")
  HD.data <- file.path(auxiliary.data.dir, "GlobalBiomes/HaxeltineAndPrentice1996/vegmap18_fromTH_Hickler2006.HD.nc")
  T63.data <- file.path(auxiliary.data.dir, "GlobalBiomes/HaxeltineAndPrentice1996/vegmap18_fromTH_Hickler2006.T63.nc")
  
  # If original is required, open the origina text file
  if(resolution == "original"){
    # get the number of columns which is 
    ncols <- length(names(read.table(file.string, header=TRUE, dec=".",  colClasses="numeric", comment.char="", nrows = 1)))
    
    # read using fread function from data.table but with the white spaces handled correctly using an awk command
    # because at time of writing fread does not handles multiple whitespaces as separators
    PNV.dt <- awkFread(file.string, colNums = c(1:ncols), header=T)
    
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
  
  ################### Megabiomes ##########################
  
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
  
  
  
  
  
  
  
  # from above orderings
  if(classification == "Smith2014") {
   subs.rules <- data.frame(id=1:18, v=c(5,4,8,9,7,6,9,3,1,2,11,12,14,15,10,16,17,13))
  }
  else if(classification == "Megabiomes"){
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




###
Smith2014BiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionofTree']]) > 0.6 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionofTree']]) > 0.6 | as.numeric(lai[['TrBRFractionofTree']])) & (lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(2)}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionofTree']] )> 0.5 &  (lai[['DominantTree']] == "TrBE" | lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(3)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(4)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  lai[['DominantTree']] == "BNS") {return(5)}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionofTree']]) > 0.5 | as.numeric(lai[['TeBSFractionofTree']]) > 0.5) & lai[['DominantTree']] == "TeBE") {return(6)}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionofTree']]) > 0.5 | as.numeric(lai[['TeBSFractionofTree']]) > 0.5) & lai[['DominantTree']] == "TeBS") {return(7)}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(8) }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(9)}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(10)}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return(11)}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return(12)}
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & as.numeric(lai[['Lat']]) >= 54) {return(13)}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(14)}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Tree']]) > 0.2 & as.numeric(lai[['Grass']]) < 1.0) {return(16)}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(lai[['Grass']]) > 0.2) {return(15)}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Total']]) > 0.2) {return(16)}
  
  # BIOME 17 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(17)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
  
  
}





MegaBiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  #if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionofTree']]) > 0.6 &  lai[['DominantTree']] == "TrBE") {return(1)}
  if(as.numeric(lai[['Tree']]) > 2.5 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  #else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionofTree']]) > 0.6 | as.numeric(lai[['TrBRFractionofTree']])) & (lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(2)}
  else if(as.numeric(lai[['Tree']]) > 2.5 & lai[['DominantTree']] == "TrBR") {return(2)}
  
  
  # BIOME 3 - Tropical Seasonal Forest
  #else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionofTree']] )> 0.5 &  (lai[['DominantTree']] == "TrBE" | lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(3)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(3)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  lai[['DominantTree']] == "BNS") {return(4)}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TemperateFractionofTree']]) > 0.5 & (lai[['DominantTree']] == "TeBE" | lai[['DominantTree']] == "TeNE")) {return(5)}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  as.numeric(lai[['TemperateFractionofTree']]) > 0.5 & lai[['DominantTree']] == "TeBS") {return(6)}
  
  # BIOME 8 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(7)}
  #else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(8)}
  
  # BIOME 9 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return(8)}
  #else if(as.numeric(lai[['Tree']]) > 0.5) {return(9)}
  
  # BIOME 10 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return(9)}
  
  # BIOME 11 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & (as.numeric(lai[['Lat']]) >= 54 | as.numeric(lai[['GDD5']]) < 400)) {return(10)}
  
  # BIOME 12 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(11)}
  
  # BIOME 13  - Arid Shrubland/Grassland
  else if(as.numeric(lai[['Total']]) > 0.2) {return(12)}
  
  # BIOME 14 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(13)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
  
  
}


Smith2014.scheme <- new("BiomeClassification",
                  id = "Smith2014",
                  name = "Smith et al. 2014", 
                  substitution = data.frame(id=1:18, v=c(5,4,8,9,7,6,9,3,1,2,11,12,14,15,10,16,17,13)),
                  rules = Smith2014BiomeRules,
                  cols = c("Tropical Rain Forest" = "seagreen",                     
                            "Tropical Deciduous Forest" = "orange3", 
                            "Tropical Seasonal Forest" = "green3", 
                            "Boreal Evergreen Forest/Woodland" = "turquoise4",
                            "Boreal Deciduous Forest/Woodland"= "cyan",
                            "Temperate Broadleaved Evergreen Forest" = "dodgerblue3",
                            "Temperate Deciduous Forest" = "chartreuse",
                            "Temperate/Boreal Mixed Forest" = "seagreen1",
                            "Temperate Mixed Forest" =  "darkseagreen1",
                            "Xeric Woodland/Shrubland" = "deeppink3",
                            "Moist Savanna" = "olivedrab2",
                            "Dry Savanna" = "goldenrod2",
                            "Arctic/Alpine Tundra" = "mediumpurple1",
                            "Tall Grassland" =  "gold",                     
                            "Dry Grassland" = "lightgoldenrod2",
                            "Arid Shrubland/Steppe"= "lightcyan",
                            "Desert" = "grey75"),
                  strings = c("Tropical Rain Forest",                     
                              "Tropical Deciduous Forest", 
                              "Tropical Seasonal Forest", 
                              "Boreal Evergreen Forest/Woodland", 
                              "Boreal Deciduous Forest/Woodland",
                              "Temperate Broadleaved Evergreen Forest",
                              "Temperate Deciduous Forest",
                              "Temperate/Boreal Mixed Forest",
                              "Temperate Mixed Forest",
                              "Xeric Woodland/Shrubland",
                              "Moist Savanna",
                              "Dry Savanna",
                              "Arctic/Alpine Tundra",
                              "Tall Grassland",
                              "Dry Grassland",
                              "Arid Shrubland/Steppe",
                              "Desert"),
                  data.reference = "Haxeltime and Prentice 1996",
                  published.reference = "Smith et al. 2014")


Megabiomes.scheme <- new("BiomeClassification",
                 id = "Megabiomes",
                 name = "Megabiomes", 
                 substitution = data.frame(id=1:18, v=c(4,3,6,5,6,5,7,1,1,2,8,9,11,12,8,12,13,10)),
                 rules = MegaBiomeRules,
                 cols = c("Tropical Rain Forest" = "seagreen",                     
                          "Tropical Deciduous Forest" = "orange3", 
                          "Boreal Evergreen Forest/Woodland" = "turquoise4",
                          "Boreal Deciduous Forest/Woodland"= "cyan",
                          "Temperate Evergreen Forest" = "dodgerblue3",
                          "Temperate Deciduous Forest" = "green3",
                          "Xeric Woodland/Shrubland" = "deeppink3",
                          "Moist Savanna" = "olivedrab2",
                          "Dry Savanna" = "goldenrod2",
                          "Arctic/Alpine Tundra" = "mediumpurple1",
                          "Tall Grassland" =  "gold",                     
                          "Arid Shrubland/Grassland"= "lightcyan",
                          "Desert" = "grey75"),
                 strings = c("Tropical Rain Forest",                     
                             "Tropical Deciduous Forest", 
                             "Boreal Evergreen Forest/Woodland", 
                             "Boreal Deciduous Forest/Woodland",
                             "Temperate Evergreen Forest",
                             "Temperate Deciduous Forest",
                             "Xeric Woodland/Shrubland",
                             "Moist Savanna",
                             "Dry Savanna",
                             "Arctic/Alpine Tundra",
                             "Tall Grassland",
                             "Arid Shrubland/Grassland",
                             "Desert"),
                 data.reference = "Haxeltime and Prentice 1996",
                 published.reference = "Forrest et al. 2015")



PaleoBiome.str <- c("Tropical Rain Forest",                     
                    "Tropical Deciduous Forest", 
                    "Tropical Seasonal Forest", 
                    "Boreal Evergreen Forest/Woodland", 
                    "Boreal Deciduous Forest/Woodland",
                    "Temperate Evergreen Forest",
                    "Temperate Deciduous Forest",
                    "Xeric Woodland/Shrubland",
                    "Moist Savanna",
                    "Dry Savanna",
                    "Arctic/Alpine Tundra",
                    "Tall Grassland",
                    "Arid Shrubland/Grassland",
                    "Desert")


PaleoBiome.cols <- c("Tropical Rain Forest" = "seagreen",                     
                     "Tropical Deciduous Forest" = "orange3", 
                     "Tropical Seasonal Forest" = "green3", 
                     "Boreal Evergreen Forest/Woodland" = "turquoise4",
                     "Boreal Deciduous Forest/Woodland"= "cyan",
                     "Temperate Evergreen Forest" = "dodgerblue3",
                     "Temperate Deciduous Forest" = "chartreuse",
                     "Xeric Woodland/Shrubland" = "deeppink3",
                     "Moist Savanna" = "olivedrab2",
                     "Dry Savanna" = "goldenrod2",
                     "Arctic/Alpine Tundra" = "mediumpurple1",
                     "Tall Grassland" =  "gold",                     
                     "Arid Shrubland/Grassland"= "lightcyan",
                     "Desert" = "grey75")



# Transfer to netCDF
#original <- readPNVBiomes("original")
#names(original) <- c("originalHaxeltineAndPrenticeBiomes")
#writeRaster(original, file.path(benchmarking.data.dir, "GlobalBiomes/vegmap18_fromTH_Hickler2006.nc"))


#lala <- readPNVBiomes(resolution = "T63", classification = "Megabiomes")