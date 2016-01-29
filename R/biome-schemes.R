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

#####################################
### Author: M. Forrest (matthew.forrest@senckenberg.de)
### 
### Biome classifications to go from LPJ-GUESS output to different biome schemes 




###############################################################################
########### SMITH ET AL. 2014 BIOME CLASSIFICATION ############################
###############################################################################

Smith2014BiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionofTree']]) > 0.6 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionofTree']]) > 0.6 | as.numeric(lai[['TrBRFractionofTree']])) & (lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(2)}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionofTree']] )> 0.5 &  (lai[['DominantTree']] == "TrBE" | lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(3)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['BorealFractionofTree']] )> 0.5 & (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(4)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  as.numeric(lai[['BorealFractionofTree']] )> 0.5 & lai[['DominantTree']] == "BNS") {return(5)}
  
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


Smith2014.scheme <- new("BiomeClassification",
                        id = "Smith2014",
                        name = "Smith et al. 2014", 
                        substitution = data.frame(id=1:18, v=c(5,4,8,9,7,6,9,3,1,2,11,12,14,15,10,16,17,13)),
                        rules = Smith2014BiomeRules,
                        totals.needed = c("lifeforms", "zones"),
                        fraction.of.total = c("Grass"),
                        fraction.of.tree = c("pft", "zones"),
                        fraction.of.woody = c("NA"),
                        combineShadeTolerance = TRUE,
                        needGDD5 = FALSE,
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







###############################################################################
########### HICKLER ET AL. 2012 BIOME CLASSIFICATION ##########################
###############################################################################

Hickler2012Rules <- function(lai){
  
  Mediterranean.Total <- as.numeric(lai[['Mediterranean']]) + as.numeric(lai[['Supra-mediterranean']])  
  
  ###### DESERTS (if LAI < 0.2)
  if(as.numeric(lai[['Total']] < 0.2)){
    
    # BIOME 1 - Arctic/alpine desert
    if(as.numeric(lai[['GDD5']]) < 1200 & (lai[['Dominant']] == "BES" | lai[['Dominant']] == "C3_gr" | lai[['Dominant']] == "Barren")) {return(1)}
    # BIOME 13 - Desert
    else if (as.numeric(lai[['GDD5']]) > 1200) {return(13)}
    
  } 
  
  
  ###### TEMPERATE BIOMES (temperate woody fraction > 80%)
  if(as.numeric(lai[['TemperateFractionofWoody']]) > 0.8){
    
    ###### FORESTS (if tree LAI > 2.0)
    if(as.numeric(lai[['Tree']]) > 2.0){
      
      # BIOME 6 - Temperate beech and mixed beech forest
      if(lai[['DominantTree']] == "Fag_syl") {return(6)}
      # BIOME 7 - Temperate mixed broad-leaved forest
      else {return(7)}
      
    }
  }
  
  
  ###### BOREAL BIOMES (boreal woody fraction > 80%)
  if(as.numeric(lai[['BorealFractionofWoody']]) > 0.8){
    
    # BIOME 4 - Boreal/alpine conifer forest
    if(as.numeric(lai[['Tree']]) > 2.0) {return(4)}
    # BIOME 3 -  Boreal/alpine mixed woodland
    if(as.numeric(lai[['Tree']]) > 0.5) {return(3)}
    
  }
  
  
  ###### MEDITERRANEAN BIOMES (mediterranean woody fraction > 80%)
  if(as.numeric(lai[['MediterraneanFractionofWoody']]) > 0.8){
    
    # BIOME 9 - Mediterranean sclerophyllous forest/woodland
    if(as.numeric(lai[['Tree']]) > 1.5) {return(9)}
    # BIOME 10 -  Mediterranean sclerophyllous shrubland
    if(Mediterranean.Total > 0.5 & Mediterranean.Total > (0.5 * as.numeric(lai[['Woody']])) & (Mediterranean.Total > as.numeric(lai[['Grass']]))) {return(10)}
    
  }
  
  
  ###### TRANSITIONAL FOREST BIOMES 
  if(as.numeric(lai[['Tree']]) > 2.0) {
    
    # HEMIBOREAL CLASSIFICATION APPLIED WHERE LAT > 52, LON > 3 
    
    # BIOME 5 - Hemiboreal mixed forest - only defined for Lat > 52, Lon > 3 as in Hickler 2012
    if(as.numeric(lai[['Boreal']]) > Mediterranean.Total & as.numeric(lai[["Lat"]]) > 52 & as.numeric(lai[["Lon"]]) > 3) {return(5)}
    
    # ALTERNATE CLASSIFICATION OUTSIDE LAT > 52, LON > 3
    
    # If boreal fraction of woody > 0.5 classify as a boreal type
    else if(as.numeric(lai[['Boreal']]) > Mediterranean.Total & as.numeric(lai[['Boreal']]) > as.numeric(lai[['Temperate']])) {
      
      # BIOME 4-  Boreal/alpine conifer forest
      return(4)
      
    }
    
    # If temperate fraction of woody >= 0.5 classify as a temperate type
    else if(as.numeric(lai[['Boreal']]) > Mediterranean.Total & as.numeric(lai[['Boreal']]) <= as.numeric(lai[['Temperate']])){
      
      # BIOME 6 - Temperate beech and mixed beech forest
      if(lai[['DominantTree']] == "Fag_syl") {return(6)}
      # BIOME 7 - Temperate mixed broad-leaved forest
      else {return(7)}
      
    }
    
    
    # BIOME 8 - Themophillous mixed broad-leaved forest
    else if(as.numeric(lai[['Boreal']]) < Mediterranean.Total) {return(8)}  
    
  }
  
  
  ###### MISCELLENEOUS  
  
  # BIOME 11  - Steppe Woodland
  if(as.numeric(lai[['Woody']]) > 0.5 & as.numeric(lai[['Grass']]) > 0.5 & as.numeric(lai[['GDD5']]) > 1200) {return(11)}
  
  # BIOME 2 - Arctic/Alpine Tundra
  if(as.numeric(lai[['Tree']]) <= 0.5 & as.numeric(lai[['GDD5']]) < 1200 & (lai[['Dominant']] == "BES" | lai[['Dominant']] == "C3_gr")) {return(2)}
  
  # BIOME 12 - Steppe
  if(as.numeric(lai[['Total']]) > 0.2 & as.numeric(lai[['GDD5']]) > 1200) {return(12)}
  
  
  
  ###### REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    print(lai)
    return(as.numeric(NA))
  }
  
  
}



Hickler2012.scheme <- new("BiomeClassification",
                          id = "Hickler2012",
                          name = "Hickler2012", 
                          substitution = data.frame(id=1:18, v=1:18),
                          rules = Hickler2012Rules,
                          combineShadeTolerance = FALSE,
                          totals.needed = c("Tree", "Woody", "Grass"),
                          fraction.of.total = c("NA"),
                          fraction.of.tree = c("NA"),
                          fraction.of.woody = c("Temperate", "Boreal", "Mediterranean", "Supra-mediterranean"),
                          needGDD5 = TRUE,
                          cols = c("Arctic/alpine desert" = "lightblue1",                     
                                   "Arctic/alpine tundra" = "lightsteelblue", 
                                   "Boreal/alpine mixed woodland" = "deepskyblue",
                                   "Boreal/alpine conifer forest"= "royalblue4",
                                   "Hemiboreal mixed forest" = "grey40",
                                   "Temperate beech and mixed beech forest" = "green4",
                                   "Temperate mixed broad-leaved forest" = "chartreuse3",
                                   "Thermophilous mixed broad-leaved forest" = "yellowgreen",
                                   "Mediterranean sclerophyllous forest/woodland" = "orange2",
                                   "Mediterranean sclerophyllous shrubland" = "hotpink4",
                                   "Steppe woodland" = "yellow3",
                                   "Steppe" =  "lemonchiffon1",                     
                                   "Desert" = "grey75"),
                          strings = c("Arctic/alpine desert" ,                     
                                      "Arctic/alpine tundra", 
                                      "Boreal/alpine mixed woodland",
                                      "Boreal/alpine conifer forest",
                                      "Hemiboreal mixed forest",
                                      "Temperate beech and mixed beech forest",
                                      "Temperate mixed broad-leaved forest",
                                      "Thermophilous mixed broad-leaved forest",
                                      "Mediterranean sclerophyllous forest/woodland",
                                      "Mediterranean sclerophyllous shrubland",
                                      "Steppe woodland",
                                      "Steppe",                     
                                      "Desert"),
                          data.reference = "- (Bohn)",
                          published.reference = "Hickler et al. 2012")





#################################################################################################################################
########### UNPUBLISHED BUT POSSIBLY USEFUL GLOBAL SCHEME BASED ON FEWER TYPES THAN SMITH ET AL. 2014 ##########################
#################################################################################################################################



MegaBiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & lai[['DominantTree']] == "TrBR") {return(2)}
  
  # BIOME 3 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(3)}
  
  # BIOME 4 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  lai[['DominantTree']] == "BNS") {return(4)}
  
  # BIOME 5 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (lai[['DominantTree']] == "TeBE" | lai[['DominantTree']] == "TeNE")) {return(5)}
  
  # BIOME 6 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  lai[['DominantTree']] == "TeBS") {return(6)}
  
  # BIOME 10 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.1 & as.numeric(lai[['Total']]) > 0.5 & (as.numeric(lai[['Lat']]) >= 54 | as.numeric(lai[['GDD5']]) < 400)) {return(10)}
  
  # BIOME 7 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(7)}
  
  # BIOME 8 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Total']]) > 2.5) {return(8)}
  #else if(as.numeric(lai[['Tree']]) > 0.5) {return(9)}
  
  # BIOME 9 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5  & as.numeric(lai[['Total']]) <= 2.5) {return(9)}
  
  # BIOME 11 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(11)}
  
  # BIOME 12  - Arid Shrubland/Grassland
  else if(as.numeric(lai[['Total']]) > 0.2) {return(12)}
  
  # BIOME 13 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(13)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
  
  
}

Megabiomes.scheme <- new("BiomeClassification",
                         id = "Megabiomes",
                         name = "Megabiomes", 
                         substitution = data.frame(id=1:18, v=c(4,3,6,5,6,5,7,1,1,2,8,9,11,12,8,12,13,10)),
                         rules = MegaBiomeRules,
                         combineShadeTolerance = TRUE,
                         fraction.of.total = c("NA"),
                         fraction.of.tree = c("NA"),
                         fraction.of.woody = c("Temperate", "Boreal", "Mediterranean"),
                         needGDD5 = TRUE,
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
                         published.reference = "-")





