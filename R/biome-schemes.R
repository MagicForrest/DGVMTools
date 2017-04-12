#!/usr/bin/Rscript

#####################################
### Author: M. Forrest (matthew.forrest@senckenberg.de)
### 
### Biome classifications to go from LPJ-GUESS output to different biome schemes 



###############################################################################
########### SMITH ET AL. 2014 BIOME CLASSIFICATION ############################
###############################################################################

#' Rules to classify biomes as per Smith et al. 2014
#' 
#' Based on LAI only, see paper for details.
#' 
#' @param lai Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-17, ordering as in Smith et al 2014 Figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
Smith2014BiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionOfTree']]) > 0.6 &  lai[['MaxTree']] == "TrBE") {return("Tropical Rain Forest")}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionOfTree']]) > 0.6) & (lai[['MaxTree']] == "TrBR" | lai[['MaxTree']] == "TrTBR")) {return("Tropical Deciduous Forest")}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionOfTree']] )> 0.5 &  (lai[['MaxTree']] == "TrBE" | lai[['MaxTree']] == "TrBR" | lai[['MaxTree']] == "TrTBR")) {return("Tropical Seasonal Forest")}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['BorealFractionOfTree']]) > 0.5 & (lai[['MaxTree']] == "BNE" | lai[['MaxTree']] == "IBS" | lai[['MaxTree']] == "BIBS")) {return("Boreal Evergreen Forest/Woodland")}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  as.numeric(lai[['BorealFractionOfTree']]) > 0.5 & lai[['MaxTree']] == "BNS") {return("Boreal Deciduous Forest/Woodland")}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TeBEFractionOfTree']]) > 0.5 | as.numeric(lai[['TeBSFractionOfTree']]) > 0.5) & lai[['MaxTree']] == "TeBE") {return("Temperate Broadleaved Evergreen Forest")}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TeBEFractionOfTree']]) > 0.5 | as.numeric(lai[['TeBSFractionOfTree']]) > 0.5) & lai[['MaxTree']] == "TeBS") {return("Temperate Deciduous Forest")}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['BorealFractionOfTree']]) < 0.8 & as.numeric(lai[['BorealFractionOfTree']]) > 0.2 & as.numeric(lai[['TemperateFractionOfTree']]) < 0.8 & as.numeric(lai[['TemperateFractionOfTree']]) > 0.2) {return("Temperate/Boreal Mixed Forest") }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return("Temperate Mixed Forest")}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return("Xeric Woodland/Shrubland")}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return("Moist Savanna")}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return("Dry Savanna")}
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & as.numeric(lai[['Lat']]) >= 54) {return("Arctic/Alpine Tundra")}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return("Tall Grassland")}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Tree']]) > 0.2 & as.numeric(lai[['Grass']]) < 1.0) {return("Arid Shrubland/Steppe")}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(lai[['Grass']]) > 0.2) {return("Dry Grassland")}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Total']]) > 0.2) {return("Arid Shrubland/Steppe")}
  
  # BIOME 17 - Desert
  else if(as.numeric(lai[['Total']]) <= 0.2) {return("Desert")}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
}

#' Meta-data describing the Smith 2014 et al. 2014 biome scheme for LPJ-GUESS output.
#' 
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
Smith2014.scheme <- new("BiomeScheme",
                        new("Quantity",
                            id = "Smith2014",
                            type = "categorical",
                            name = "Smith et al. 2014 Biomes", 
                            colours = colorRampPalette(c("Tropical Rain Forest" = "seagreen",                     
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
                                                         "Desert" = "grey75")),
                            units = c("Tropical Rain Forest",                     
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
                            model = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                        rules = Smith2014BiomeRules,
                        totals.needed = c("lifeforms", "zones"),
                        max.needed = "Tree",
                        fraction.of.total = c("Grass"),
                        fraction.of.tree = c("pft", "zones"),
                        fraction.of.woody = c("NA"),
                        combineShadeTolerance = TRUE,
                        needGDD5 = FALSE,
                        data.reference = "Haxeltime and Prentice 1996",
                        published.reference = "Smith et al. 2014")






###############################################################################
########### HICKLER ET AL. 2012 BIOME CLASSIFICATION ##########################
###############################################################################


#' Rules to classify biomes as per Hickler et al. 2012
#' 
#' Based on LAI, GDD5 and lon/lat only, see paper for details.
#' 
#' @param lai Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-13, ordering as in Hickler et al 2012 figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
Hickler2012Rules <- function(lai){
  
  Mediterranean.Total <- as.numeric(lai[['Mediterranean']]) + as.numeric(lai[['Supra-mediterranean']])  
  
  ###### DESERTS (if LAI < 0.2)
  if(as.numeric(lai[['Total']] < 0.2)){
    
    # BIOME 1 - Arctic/alpine desert
    if(as.numeric(lai[['gdd5']]) < 1200 & (lai[['MaxPFT']] == "BES" | lai[['MaxPFT']] == "C3_gr" | lai[['MaxPFT']] == "Barren")) {return(1)}
    # BIOME 13 - Desert
    else if (as.numeric(lai[['gdd5']]) > 1200) {return(13)}
    
  } 
  
  
  ###### TEMPERATE BIOMES (temperate woody fraction > 80%)
  if(as.numeric(lai[['TemperateFractionOfWoody']]) > 0.8){
    
    ###### FORESTS (if tree LAI > 2.0)
    if(as.numeric(lai[['Tree']]) > 2.0){
      
      # BIOME 6 - Temperate beech and mixed beech forest
      if(lai[['MaxTree']] == "Fag_syl") {return(6)}
      # BIOME 7 - Temperate mixed broad-leaved forest
      else {return(7)}
      
    }
  }
  
  
  ###### BOREAL BIOMES (boreal woody fraction > 80%)
  if(as.numeric(lai[['BorealFractionOfWoody']]) > 0.8){
    
    # BIOME 4 - Boreal/alpine conifer forest
    if(as.numeric(lai[['Tree']]) > 2.0) {return(4)}
    # BIOME 3 -  Boreal/alpine mixed woodland
    if(as.numeric(lai[['Tree']]) > 0.5) {return(3)}
    
  }
  
  
  ###### MEDITERRANEAN BIOMES (mediterranean woody fraction > 80%)
  if(as.numeric(lai[['MediterraneanFractionOfWoody']]) > 0.8){
    
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
      if(lai[['MaxTree']] == "Fag_syl") {return(6)}
      # BIOME 7 - Temperate mixed broad-leaved forest
      else {return(7)}
      
    }
    
    
    # BIOME 8 - Themophillous mixed broad-leaved forest
    else if(as.numeric(lai[['Boreal']]) < Mediterranean.Total) {return(8)}  
    
  }
  
  
  ###### MISCELLENEOUS  
  
  # BIOME 11  - Steppe Woodland
  if(as.numeric(lai[['Woody']]) > 0.5 & as.numeric(lai[['Grass']]) > 0.5 & as.numeric(lai[['gdd5']]) > 1200) {return(11)}
  
  # BIOME 2 - Arctic/Alpine Tundra
  if(as.numeric(lai[['Tree']]) <= 0.5 & as.numeric(lai[['gdd5']]) < 1200 & (lai[['MaxPFT']] == "BES" | lai[['MaxPFT']] == "C3_gr")) {return(2)}
  
  # BIOME 12 - Steppe
  if(as.numeric(lai[['Total']]) > 0.2 & as.numeric(lai[['gdd5']]) > 1200) {return(12)}
  
  
  
  ###### REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    print(lai)
    return(as.numeric(NA))
  }
  
  
}

#' Meta-data describing the Hickler et al. 2012 european biome scheme for LPJ-GUESS output.
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

Hickler2012.scheme <- new("BiomeScheme",
                          new("Quantity",
                              id = "Hickler2012",
                              name = "Hickler2012", 
                              type = "categorical",
                              colours = colorRampPalette(c("Arctic/alpine desert" = "lightblue1",                     
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
                                                           "Desert" = "grey75")),
                              units = c("Arctic/alpine desert" ,                     
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
                              model = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                          rules = Hickler2012Rules,
                          combineShadeTolerance = FALSE,
                          totals.needed = c("Tree", "Woody", "Grass"),
                          max.needed = c("Tree", "PFT"),
                          fraction.of.total = c("NA"),
                          fraction.of.tree = c("NA"),
                          fraction.of.woody = c("Temperate", "Boreal", "Mediterranean", "Supra-mediterranean"),
                          needGDD5 = TRUE,
                          data.reference = "- (Bohn)",
                          published.reference = "Hickler et al. 2012")



###############################################################################
########### FORREST ET AL. 2015 MEGABIOME CLASSIFICATION ######################
###############################################################################

#' Rules to classify coarses "mega biomes" as per Forrest et al. 2015
#' 
#' # This is described in Forrest et al. 2015 Climates of the Past.  Basically it calculates the Smith et al. 2014 biomes, 
#' but assigns them to broader categories as per Harrison and Prentice 2006
#' 
#' @param lai Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-8, ordering as in the Forrest et al. 2015 figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

Forrest2015MegaBiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionOfTree']]) > 0.6 &  lai[['MaxTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionOfTree']]) > 0.6 | as.numeric(lai[['TrBRFractionOfTree']])) & (lai[['MaxTree']] == "TrBR" | lai[['MaxTree']] == "TrTBR")) {return(5)}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionOfTree']] )> 0.5 &  (lai[['MaxTree']] == "TrBE" | lai[['MaxTree']] == "TrBR" | lai[['MaxTree']] == "TrTBR")) {return(1)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['BorealFractionOfTree']]) > 0.5 & (lai[['MaxTree']] == "BNE" | lai[['MaxTree']] == "IBS" | lai[['MaxTree']] == "BIBS")) {return(4)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['BorealFractionOfTree']]) > 0.5 & lai[['MaxTree']] == "BNS") {return(4)}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionOfTree']]) > 0.5 | as.numeric(lai[['TeBSFractionOfTree']]) > 0.5) & lai[['MaxTree']] == "TeBE") {return(2)}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionOfTree']]) > 0.5 | as.numeric(lai[['TeBSFractionOfTree']]) > 0.5) & lai[['MaxTree']] == "TeBS") {return(3)}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(3) }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(3)}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(5)}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return(5)}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return(6)}
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & as.numeric(lai[['Lat']]) >= 54) {return(7)}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(6)}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Tree']]) > 0.2 & as.numeric(lai[['Grass']]) < 1.0) {return(6)}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(lai[['Grass']]) > 0.2) {return(6)}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Total']]) > 0.2) {return(6)}
  
  # BIOME 17 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(8)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
  
}

#' Meta-data describing the Forrest et al. 2015 "mega biome" scheme for LPJ-GUESS output.
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
Forrest2015.scheme <- new("BiomeScheme",
                          new("Quantity",
                              id = "Forrest2015",
                              name = "Forrest et al. 2015", 
                              type = "categorical",
                              colours = colorRampPalette(c("Tropical Forest" = "seagreen",                     
                                                           "Temperate Evergeen Forest"= "dodgerblue3", 
                                                           "Temperate Deciduous Forest "= "green3", 
                                                           "Boreal Forest" = "turquoise4",
                                                           "Savanna and Dry Woodlands" = "olivedrab2",
                                                           "Grasslands and Dry Shrublands" = "goldenrod2",
                                                           "Tundra" = "mediumpurple1",
                                                           "Desert" = "grey75")),
                              units =  c("Tropical Forest",                     
                                         "Temperate Evergeen Forest", 
                                         "Temperate Deciduous Forest", 
                                         "Boreal Forest",
                                         "Savanna and Dry Woodlands",
                                         "Grasslands and Dry Shrublands",
                                         "Tundra",
                                         "Desert"),
                              model = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                          rules = Forrest2015MegaBiomeRules,
                          totals.needed = c("lifeforms", "zones"),
                          max.needed = "Tree",
                          fraction.of.total = c("Grass"),
                          fraction.of.tree = c("pft", "zones"),
                          fraction.of.woody = c("NA"),
                          combineShadeTolerance = TRUE,
                          needGDD5 = FALSE,
                          data.reference = "Haxeltime and Prentice 1996",
                          published.reference = "Forrest et al 2015, Smith et al. 2014")





#####################################################################
########### MEDITERRANEAN BIOME CLASSIFICATION ######################
#####################################################################

#' Rules to classify coarses Mediterranean biomes
#' 
#' No reference yet
#' 
#' @param lai Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-8, ordering as in the Forrest et al. 2015 figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

MeditBiomeRules <- function(lai){
  
  # BIOME 1 - Grass Steppe  
  #else if(as.numeric(lai[['Grass']]) > 1.0 & as.numeric(lai[['Woody']] < 1.0)) {return(7)}
  if(as.numeric(lai[['Grass']]) > as.numeric(lai[['Woody']])) {return(1)}
  # BIOME 4 - Deciduous forest
  #if(as.numeric(lai[['Woody']]) > 2.5 && as.numeric(lai[['SummergreenFractionOfTree']]) > 0.5) {return(4)}  # 2
  else if(as.numeric(lai[['Woody']]) > 2.0 && (lai[['MaxWoody']] == "TeBS" || lai[['MaxWoody']] == "TeBS")) {return(4)}  # 2
  # BIOME 5 - Cold Montane forest
  else if(as.numeric(lai[['Woody']]) > 1.5 && (lai[['MaxWoody']] == "BIBS" || lai[['MaxWoody']] == "BNE" )) {return(5)} # 1.5
  # BIOME 2 - Needle-leaved evergreen forest
  else if(as.numeric(lai[['Woody']]) > 1.0 && lai[['MaxWoody']] == "TeNE") {return(2)} # 1.5
  # BIOME 6 - Pre-steppe deciduous woodlands
  #else if(as.numeric(lai[['Woody']]) > 1.0  && as.numeric(lai[['Grass']]) > as.numeric(lai[['Woody']]) * 0.2 && (lai[['MaxWoody']] == "TeBS" || lai[['MaxWoody']] == "TeNE")) {return(6)}
  else if(as.numeric(lai[['Woody']]) > 1.0  && as.numeric(lai[['Grass']]) > as.numeric(lai[['Woody']]) * 0.2 && lai[['MaxWoody']] == "TeBS") {return(6)}
  # BIOME 3 - Mediterranean woodland/scrub
  else if(as.numeric(lai[['Woody']]) > 1 && (lai[['MaxWoody']] == "TeBE" || lai[['MaxWoody']] == "MeES" || lai[['MaxWoody']] == "MeRS")) {return(3)} # 1.5
   # BIOME 7 - Remainder, Unclassified
  else {
    #print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    #print(lai)
    return(7)
  }
  
  
}

#' Meta-data describing a Mediterranean scheme for LPJ-GUESS output.
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
MeditBiomes.scheme <- new("BiomeScheme",
                          new("Quantity",
                              id = "MeditBiomes",
                              name = "Mediterranean Biomes", 
                              type = "categorical",
                              colours = colorRampPalette(c("darkseagreen1",
                                                           "darkolivegreen",
                                                           "orangered4",
                                                           "green3",
                                                           "royalblue4",
                                                           "sandybrown",
                                                           "grey75")),
                              units =  c("Grass Steppe or\nMontane Grassland",
                                         "Needle-leaved\nWoodlands/Forest",
                                         "Mediterranean\nSclerophyllous\nWoodlands/Forest",
                                         "Deciduous\nForest",
                                         "Cold Montane\nForest",
                                         "Deciduous\nSteppe-Woodlands",
                                         "Unclassifiable/\nOther"),
                              model = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                          rules = MeditBiomeRules,
                          totals.needed = c("lifeforms", "zones"),
                          max.needed = "Woody",
                          fraction.of.total = c("Grass"),
                          fraction.of.tree = c("Boreal", "Summergreen"),
                          fraction.of.woody = c("NA"),
                          combineShadeTolerance = TRUE,
                          needGDD5 = FALSE,
                          data.reference = "-",
                          published.reference = "-")


#################################################################################################################################
########### UNPUBLISHED BUT POSSIBLY USEFUL GLOBAL SCHEME BASED ON FEWER TYPES THAN SMITH ET AL. 2014 ##########################
#################################################################################################################################

#' Rules to classify coarser "mega biomes" from LAI, latitude and GDD5
#' 
#' Unpublished but possibly useful in principle.  Simpler classes that Smith et al 2014,
#' but more complex that Forrest et al 2015.  
#' 
#' @param lai Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @keywords internal
#' @return Biomes code (1-13)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
MegaBiomeRules_dev <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 &  lai[['MaxTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & lai[['MaxTree']] == "TrBR") {return(2)}
  
  # BIOME 3 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  (lai[['MaxTree']] == "BNE" | lai[['MaxTree']] == "IBS" | lai[['MaxTree']] == "BIBS")) {return(3)}
  
  # BIOME 4 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  lai[['MaxTree']] == "BNS") {return(4)}
  
  # BIOME 5 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (lai[['MaxTree']] == "TeBE" | lai[['MaxTree']] == "TeNE")) {return(5)}
  
  # BIOME 6 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  lai[['MaxTree']] == "TeBS") {return(6)}
  
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


#' Meta-data describing an unpublished "mega biome" scheme for LPJ-GUESS output.
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
Megabiomes_dev.scheme <- new("BiomeScheme",
                             new("Quantity",
                                 id = "Megabiomes",
                                 name = "Megabiomes", 
                                 type = "categorical",
                                 colours = colorRampPalette(c("Tropical Rain Forest" = "seagreen",                     
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
                                                              "Desert" = "grey75")),
                                 units =  c("Tropical Rain Forest",                     
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
                                 model = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                             rules = MegaBiomeRules_dev,
                             combineShadeTolerance = TRUE,
                             max.needed = "Tree",
                             fraction.of.total = c("NA"),
                             fraction.of.tree = c("NA"),
                             fraction.of.woody = c("Temperate", "Boreal", "Mediterranean"),
                             needGDD5 = TRUE,
                             data.reference = "Haxeltime and Prentice 1996",
                             published.reference = "-")



#################################################################################################################################
########### FIREMIP BIOMES ##########################
#################################################################################################################################

#' Rules to classify FireMIP biomes from fractionalcover of PFTs
#' 
#' Unpublished but possibly useful in principle.  Simpler classes that Smith et al 2014,
#' but more complex that Forrest et al 2015.  
#' 
#' @param fpc Numerical vector of FPC values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @keywords internal
#' @return Biomes code (1-10)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
FireMIPBiomeRules <- function(fpc){
  
  # BIOME 1 & 2 - croplands mosaics
  
  if(as.numeric(fpc[['Crops']]) > 0.5) { return(1) }
  
  if(as.numeric(fpc[['Crops']]) > 0.2) { return(2) }
  
  # BIOMES 2-6 - Forests
  #if(as.numeric(fpc[['Tree']]) > 0.6 &  fpc[['Grass']] < 0.4) {
  if(as.numeric(fpc[['Tree']]) > 0.6) {
    
    
    # BIOME 2 Needleleaved forest
    if(fpc[['MaxTree']] == "NE") { return(3) }
    
    # BIOME 3 NS forest
    else if(fpc[['MaxTree']] == "NS") { return(4) }
    
    # BIOME 4 Summergreen forest
    else if(fpc[['MaxTree']] == "BS") { return(5) }
    
    # BIOME 5 Evergreen forest
    else if(fpc[['MaxTree']] == "BE") { return(6) }
    
    # BIOME 6 Raingreen forest
    else if(fpc[['MaxTree']] == "BR") { return(7) }
    
  }
  
  # BIOME 3 Additional - Lower tree cover requirement for Needle-leaved Summergreen Forest
  else if(as.numeric(fpc[['Tree']]) > 0.4 && fpc[['MaxTree']] == "NS") { return(4) }
  
  
  # BIOMES 7 and 8 - Grassy systems
  else if(as.numeric(fpc[['Grass']]) > 0.4 ) {
    
    # BIOME 7 C4 grassy system
    if(fpc[['MaxGrass']] == "C4G") { return(8) }
    
    # BIOME 8 C3 grassy system
    else if(fpc[['MaxGrass']] == "C3G") { return(9) }
    
  }
  
  # BIOMES 9 - Shrublands
  else if(as.numeric(fpc[['Shrub']]) > 0.3 ) { return(10) }
  
  # BIOME 10 - Sparse vegetation
  else if(as.numeric(fpc[['Total']]) > 0.2 ) { return(11) }
  
  # BIOME 11 Barren/Unclassified 
  else { return(12) }
  
  
}


#' Meta-data describing an unpublished "mega biome" scheme for LPJ-GUESS output.
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
FireMIPBiomes.scheme <- new("BiomeScheme",
                            new("Quantity",
                                id = "FireMIP",
                                name = "FireMIP Biomes", 
                                type = "categorical",
                                colours = colorRampPalette(c("Croplands Dominated \n(Croplands > 50%)" = "chartreuse",
                                                             "Croplands Mosaic \n(20% < Croplands < 50%)" = "darkseagreen1",
                                                             "Evergreen Needle-leafed Forest" = "darkblue",   
                                                             "Summergreen Needle-leafed Forest" = "skyblue2",
                                                             "Broadleafed Summergreen Forest" = "darkgreen", 
                                                             "Broadleafed Evergreen Forest" = "orchid4",
                                                             "Broadleafed Raingreen Forest" = "lightsalmon4", 
                                                             "C4 Grassy System" = "orange",
                                                             "C3 Grassy System" = "lightgoldenrod",
                                                             "Shrubland" = "indianred3",
                                                             "Sparse/Other Vegetation" = "mistyrose2",
                                                             "Barren" = "gray75")),
                                units =  c("Croplands Dominated \n(Croplands > 50%)",
                                           "Croplands Mosaic \n(20% < Croplands < 50%)",
                                           "Needle-leafed  \nEvergreen Forest",      
                                           "Needle-leafed  \nSummergreen Forest",
                                           "Broadleafed \nSummergreen Forest", 
                                           "Broadleafed \nEvergreen Forest",
                                           "Broadleafed \nRaingreen Forest", 
                                           "C4 Grassy \nSystem",
                                           "C3 Grassy \nSystem",
                                           "Shrubland",
                                           "Sparse/Other \nVegetation",
                                           "Barren"),
                                model = c("LPJ-GUESS-SPITFIRE-FireMIP",
                                          "LPJ-GUESS-BLAZE-FireMIP",
                                          "LPJ-GUESS-GlobFIRM-FireMIP",
                                          "CLM-FireMIP",
                                          "CTEM-FireMIP",
                                          "JSBACH-FireMIP",
                                          "Inferno-FireMIP",
                                          "ORCHIDEE-FireMIP")),
                            rules = FireMIPBiomeRules,
                            combineShadeTolerance = FALSE,
                            max.needed = c("Tree", "Grass"),
                            fraction.of.total = c("Tree"),
                            fraction.of.tree = c("Phenology", "Leafform"),
                            fraction.of.woody = c("NA"),
                            needGDD5 = FALSE,
                            data.reference = "-",
                            published.reference = "-")



#################################################################################################################################
###########  ##########################
#################################################################################################################################

#' Rules for biome scheme based on FPC
#' 
#' Joerg fill this in
#' 
#' @param fpc Vector of FPC values
#' @keywords internal
FPCMegaBiomeRules <- function(fpc) {
  
  # 9 - Desert
  if (as.numeric(fpc[['Total']]) <= 0.2 &
      as.numeric(fpc[['GDD5']]) >= 1200) {return(9)}
  
  # 10 - Arctic desert
  else if (as.numeric(fpc[['Total']]) <= 0.2 &
           as.numeric(fpc[['GDD5']]) < 1200) {return(10)}
  
  # 1 - Tropical Forest
  else if (as.numeric(fpc[['Tree']]) > 0.6 &
           as.numeric(fpc[['Tropical']]) > as.numeric(fpc[['Temperate']]) &
           as.numeric(fpc[['Tropical']]) > as.numeric(fpc[['Boreal']])) {return(1)}
  
  # 2 - Temperate Forest
  else if (as.numeric(fpc[['Tree']]) > 0.6 &
           as.numeric(fpc[['Temperate']]) > as.numeric(fpc[['Tropical']]) &
           as.numeric(fpc[['Temperate']]) > as.numeric(fpc[['Boreal']])) {return(2)}
  
  # 3 - Boreal Forest
  else if (as.numeric(fpc[['Tree']]) > 0.2 &
           as.numeric(fpc[['Boreal']]) > as.numeric(fpc[['Temperate']]) &
           as.numeric(fpc[['Boreal']]) > as.numeric(fpc[['Tropical']])) {return(3)}
  
  # 4 - Savannah
  else if (as.numeric(fpc[['Tree']]) > 0.1 &
           as.numeric(fpc[['Tree']]) <= 0.6 &
           as.numeric(fpc[['C4G']]) >= as.numeric(fpc[['C3G']])) {return(4)}
  
  # 5 - Temperate woodland
  else if (as.numeric(fpc[['Tree']]) > 0.1 &
           as.numeric(fpc[['Tree']]) <= 0.6 &
           as.numeric(fpc[['C4G']]) < as.numeric(fpc[['C3G']]) &
           as.numeric(fpc[['GDD5']]) > 1800) {return(5)}
  
  # 6 - Tropical grassland
  else if (as.numeric(fpc[['Tree']]) <= 0.1 &
           as.numeric(fpc[['C4G']]) >= as.numeric(fpc[['C3G']])) {return(6)}
  
  # 7 - Temperate grassland
  else if (as.numeric(fpc[['Tree']]) <= 0.1 &
           as.numeric(fpc[['C4G']]) < as.numeric(fpc[['C3G']]) &
           as.numeric(fpc[['GDD5']]) > 1200) {return(7)}
  
  # 8 - Tundra
  else if (as.numeric(fpc[['Tree']]) <= 0.2) {return(8)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(fpc[['Lon']]), ",", as.numeric(fpc[['Lat']]), ")" ))
    return(NA)
  }
}

#' Meta-data for an FPC-based mega biome scheme
#' 
#' Joerg stuff here
#'
#' @keywords internal
#' 
FPCMegabiomes.scheme <- new("BiomeScheme",
                            new("Quantity",
                                id = "FPCMegabiomes",
                                name = "FPCMegabiomes", 
                                type = "categorical",
                                colours = colorRampPalette(c("Tropical forest" = "darkgreen",
                                                             "Temperate forest" = "seagreen",
                                                             "Boreal forest" = "turquoise4",
                                                             "Savannah" = "tan",
                                                             "Woodland" = "tan2",
                                                             #                                     "Taiga" = "tan4",
                                                             "C4-grassland" = "gold",
                                                             "C3-grassland" = "yellow",
                                                             "Tundra" = "seagreen2",
                                                             "Desert" = "lightgrey",
                                                             "Arctic desert" = "grey")),
                                units = c("Tropical forest",
                                          "Temperate forest",
                                          "Boreal forest",
                                          "Savannah",
                                          "Woodland",
                                          #                                     "Taiga",
                                          "C4-grassland",
                                          "C3-grassland",
                                          "Tundra",
                                          "Desert",
                                          "Arctic desert"),
                                model = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                            rules = FPCMegaBiomeRules,
                            totals.needed = c("lifeforms", "zones"),
                            max.needed = c("NA"),
                            combineShadeTolerance = TRUE,
                            fraction.of.total = c("NA"),
                            fraction.of.tree = c("NA"),
                            fraction.of.woody = c("NA"),
                            needGDD5 = TRUE,
                            data.reference = "-",
                            published.reference = "-")

#' Currently supported biome schemes
#' 
#' The biomes schemes that are currently supported for LPJ-GUESS output.  
#' It a list of biomes schemes with elements with the followinf names/ids:
#' 
#' \describe{
#' \item{Smith2014}{Classification of global output as per Smith et al. 2014. 
#'  Designed to be compared to Haxeltine and Prentice (1996) map of potential natural biomes.}
#'  \item{Forrest2015}{Classification of of global output into coarser mega biomes as per Forrest et al. 2015 (see appendix), 
#'  which is really just an aggregation of biomes following Harrison and Prentice 2006.  Can be compared to aggregated Haxeltine and Prentice (1996) map.}
#'  \item{Hickler2012}{Classification of european output as per Hickler et al. 2012.}
#'  \item{Megabiomes_dev}{Unpublished for potentially useful classification of global biomes 
#'  which is slightly simpler than Smith et al. 2014.}
#'  \item{FPCMegabiomes}{Global biomes classification based on FPC.  Ask Joerg Steinkamp 
#'  about this one.} 
#' }
#' 
#' The mechanics of all this could do with a serious overhaul to deal with other model outputs
#' and classifications involving more that one 'primary variable', ie FPC and biomass.
#'   
#' @seealso \code{BiomeClassification}, \code{readHandPBiomes}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
supported.biome.schemes <- c("Smith2014" = Smith2014.scheme,
                             "Hickler2012" = Hickler2012.scheme,
                             "Forrest2015" = Forrest2015.scheme,
                             "Megabiomes_dev" = Megabiomes_dev.scheme,
                             "FPCMegabiomes" = FPCMegabiomes.scheme,
                             "MeditBiomes" = MeditBiomes.scheme, 
                             "FireMIP" = FireMIPBiomes.scheme)
