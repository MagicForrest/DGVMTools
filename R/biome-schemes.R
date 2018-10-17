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
#' @param x Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-17, ordering as in Smith et al 2014 Figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
Smith2014BiomeRules <- function(x){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & as.numeric(x[['LAI_std_TrBEFractionOfTree']]) > 0.6 &  x[['LAI_std_MaxTree']] == "TrBE") {return("Tropical Rain Forest")}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & (as.numeric(x[['LAI_std_TrBRFractionOfTree']]) > 0.6) & (x[['LAI_std_MaxTree']] == "TrBR" | x[['LAI_std_MaxTree']] == "TrTBR")) {return("Tropical Deciduous Forest")}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & as.numeric(x[['LAI_std_TropicalFractionOfTree']] )> 0.5 &  (x[['LAI_std_MaxTree']] == "TrBE" | x[['LAI_std_MaxTree']] == "TrBR" | x[['LAI_std_MaxTree']] == "TrTBR")) {return("Tropical Seasonal Forest")}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_BorealFractionOfTree']]) > 0.5 & (x[['LAI_std_MaxTree']] == "BNE" | x[['LAI_std_MaxTree']] == "IBS" | x[['LAI_std_MaxTree']] == "BIBS")) {return("Boreal Evergreen Forest/Woodland")}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 &  as.numeric(x[['LAI_std_BorealFractionOfTree']]) > 0.5 & x[['LAI_std_MaxTree']] == "BNS") {return("Boreal Deciduous Forest/Woodland")}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & (as.numeric(x[['LAI_std_TeBEFractionOfTree']]) > 0.5 | as.numeric(x[['LAI_std_TeBSFractionOfTree']]) > 0.5) & x[['LAI_std_MaxTree']] == "TeBE") {return("Temperate Broadleaved Evergreen Forest")}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & (as.numeric(x[['LAI_std_TeBEFractionOfTree']]) > 0.5 | as.numeric(x[['LAI_std_TeBSFractionOfTree']]) > 0.5) & x[['LAI_std_MaxTree']] == "TeBS") {return("Temperate Deciduous Forest")}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & as.numeric(x[['LAI_std_BorealFractionOfTree']]) < 0.8 & as.numeric(x[['LAI_std_BorealFractionOfTree']]) > 0.2 & as.numeric(x[['LAI_std_TemperateFractionOfTree']]) < 0.8 & as.numeric(x[['LAI_std_TemperateFractionOfTree']]) > 0.2) {return("Temperate/Boreal Mixed Forest") }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5) {return("Temperate Mixed Forest")}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Tree']]) < 2.5 & as.numeric(x[['LAI_std_GrassFraction']]) < 0.2) {return("Xeric Woodland/Shrubland")}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Tree']]) < 2.5 & as.numeric(x[['LAI_std_Total']]) > 2.5) {return("Moist Savanna")}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Tree']]) < 2.5 & as.numeric(x[['LAI_std_Total']]) <= 2.5) {return("Dry Savanna")}
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(x[['LAI_std_Tree']]) < 0.5 & as.numeric(x[['LAI_std_Total']]) > 0.5 & as.numeric(x[['Lat']]) >= 54) {return("Arctic/Alpine Tundra")}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(x[['LAI_std_Grass']]) > 2.0) {return("Tall Grassland")}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.2 & as.numeric(x[['LAI_std_Grass']]) < 1.0) {return("Arid Shrubland/Steppe")}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(x[['LAI_std_Grass']]) > 0.2) {return("Dry Grassland")}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(x[['LAI_std_Total']]) > 0.2) {return("Arid Shrubland/Steppe")}
  
  # BIOME 17 - Desert
  else if(as.numeric(x[['LAI_std_Total']]) <= 0.2) {return("Desert")}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(x[['Lon']]), ",", as.numeric(x[['Lat']]), ")" ))
    return(NA)
  }
  
}

#' Smith et al 2014
#'  
#' \strong{Smith2014BiomeScheme} Classification of global output as per Smith et al. 2014. Designed to be compared to Haxeltine and Prentice (1996) 
#' map of potential natural biomes.
#' 
#' @rdname BiomeScheme-class
#' @export
Smith2014BiomeScheme <- new("BiomeScheme",
                        new("Quantity",
                            id = "Smith2014",
                            name = "Smith et al. 2014 Biomes", 
                            colours = grDevices::colorRampPalette(c("Tropical Rain Forest" = "seagreen",                     
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
                            format = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                        rules = Smith2014BiomeRules,
                        layers.needed = list(
                          
                          ### Combine shade tolerances
                          list(quantity = "LAI_std", operator = "+", layers = c("BNE", "BINE", "BIBS"), new.layer = "BNE"),
                          list(quantity = "LAI_std", operator = 0, layers = "BINE"),
                          list(quantity = "LAI_std", operator = 0, layers = "BIBS"),
                          list(quantity = "LAI_std", operator = "+", layers = c("TeBS", "TeIBS", "IBS"), new.layer = "TeBS"),
                          list(quantity = "LAI_std", operator = 0, layers = "TeIBS"),
                          list(quantity = "LAI_std", operator = 0, layers = "IBS"),
                          list(quantity = "LAI_std", operator = "+", layers = c("TrBE", "TrIBE"), new.layer = "TrBE"),
                          list(quantity = "LAI_std", operator = 0, layers = "TrIBE"),
                          list(quantity = "LAI_std", operator = 0, layers = "IBS"),
                          
                          ### Make totals
                          # Tree
                          list(quantity = "LAI_std", operator = "+", layers = ".Tree", new.layer = "Tree"),
                          # Grass
                          list(quantity = "LAI_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                          # Boreal
                          list(quantity = "LAI_std", operator = "+", layers = ".Boreal", new.layer = "Boreal"),
                          # Temperate
                          list(quantity = "LAI_std", operator = "+", layers = ".Temperate", new.layer = "Temperate"),
                          # Tropical
                          list(quantity = "LAI_std", operator = "+", layers = ".Tropical", new.layer = "Tropical"),
                          # Total
                          list(quantity = "LAI_std", operator = "+", layers = ".PFTs", new.layer = "Total"),
                          
                          ### Get max tree
                          list(quantity = "LAI_std", operator = "max.layer", layers = ".Tree", new.layer = "MaxTree"),
                          
                          # Make fractions
                          # GrassFraction
                          list(quantity = "LAI_std", operator = "/", layers = c("Grass", "Total"), new.layer = "GrassFraction"),
                          # BorealFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("Boreal", "Tree"), new.layer = "BorealFractionOfTree"),
                          # TemperateFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("Temperate", "Tree"), new.layer = "TemperateFractionOfTree"),
                          # TropicalFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("Tropical", "Tree"), new.layer = "TropicalFractionOfTree"),
                          # TrBEFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("TrBE", "Tree"), new.layer = "TrBEFractionOfTree"),
                          # TrBRFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("TrBR", "Tree"), new.layer = "TrBRFractionOfTree"),
                          # TeBEFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("TeBE", "Tree"), new.layer = "TeBEFractionOfTree"),
                          # TeBSFractionOfTree
                          list(quantity = "LAI_std", operator = "/", layers = c("TeBS", "Tree"), new.layer = "TeBSFractionOfTree")
                          
                        ),
                        data.reference = "Haxeltine and Prentice 1996",
                        published.reference = "Smith et al. 2014")






###############################################################################
########### HICKLER ET AL. 2012 BIOME CLASSIFICATION ##########################
###############################################################################


#' Rules to classify biomes as per Hickler et al. 2012
#' 
#' Based on LAI, GDD5 and lon/lat only, see paper for details.
#' 
#' @param x Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-13, ordering as in Hickler et al 2012 figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
Hickler2012Rules <- function(x){
  
  Mediterranean.Total <- as.numeric(x[['LAI_std_Mediterranean']]) + as.numeric(x[['LAI_std_Supra-mediterranean']])  
  
  ###### DESERTS (if LAI < 0.2)
  if(as.numeric(x[['LAI_std_Total']] < 0.2)){
    
    # BIOME 1 - Arctic/alpine desert
    if(as.numeric(x[['LAI_std_gdd5']]) < 1200 & (x[['LAI_std_MaxPFT']] == "BES" | x[['LAI_std_MaxPFT']] == "C3_gr" | x[['LAI_std_MaxPFT']] == "Barren")) {return(1)}
    # BIOME 13 - Desert
    else if (as.numeric(x[['LAI_std_gdd5']]) > 1200) {return(13)}
    
  } 
  
  
  ###### TEMPERATE BIOMES (temperate woody fraction > 80%)
  if(as.numeric(x[['LAI_std_TemperateFractionOfWoody']]) > 0.8){
    
    ###### FORESTS (if tree LAI > 2.0)
    if(as.numeric(x[['LAI_std_Tree']]) > 2.0){
      
      # BIOME 6 - Temperate beech and mixed beech forest
      if(x[['LAI_std_MaxTree']] == "Fag_syl") {return(6)}
      # BIOME 7 - Temperate mixed broad-leaved forest
      else {return(7)}
      
    }
  }
  
  
  ###### BOREAL BIOMES (boreal woody fraction > 80%)
  if(as.numeric(x[['LAI_std_BorealFractionOfWoody']]) > 0.8){
    
    # BIOME 4 - Boreal/alpine conifer forest
    if(as.numeric(x[['LAI_std_Tree']]) > 2.0) {return(4)}
    # BIOME 3 -  Boreal/alpine mixed woodland
    if(as.numeric(x[['LAI_std_Tree']]) > 0.5) {return(3)}
    
  }
  
  
  ###### MEDITERRANEAN BIOMES (mediterranean woody fraction > 80%)
  if(as.numeric(x[['LAI_std_MediterraneanFractionOfWoody']]) > 0.8){
    
    # BIOME 9 - Mediterranean sclerophyllous forest/woodland
    if(as.numeric(x[['LAI_std_Tree']]) > 1.5) {return(9)}
    # BIOME 10 -  Mediterranean sclerophyllous shrubland
    if(Mediterranean.Total > 0.5 & Mediterranean.Total > (0.5 * as.numeric(x[['LAI_std_Woody']])) & (Mediterranean.Total > as.numeric(x[['LAI_std_Grass']]))) {return(10)}
    
  }
  
  
  ###### TRANSITIONAL FOREST BIOMES 
  if(as.numeric(x[['LAI_std_Tree']]) > 2.0) {
    
    # HEMIBOREAL CLASSIFICATION APPLIED WHERE LAT > 52, LON > 3 
    
    # BIOME 5 - Hemiboreal mixed forest - only defined for Lat > 52, Lon > 3 as in Hickler 2012
    if(as.numeric(x[['LAI_std_Boreal']]) > Mediterranean.Total & as.numeric(x[["Lat"]]) > 52 & as.numeric(x[["Lon"]]) > 3) {return(5)}
    
    # ALTERNATE CLASSIFICATION OUTSIDE LAT > 52, LON > 3
    
    # If boreal fraction of woody > 0.5 classify as a boreal type
    else if(as.numeric(x[['LAI_std_Boreal']]) > Mediterranean.Total & as.numeric(x[['LAI_std_Boreal']]) > as.numeric(x[['LAI_std_Temperate']])) {
      
      # BIOME 4-  Boreal/alpine conifer forest
      return(4)
      
    }
    
    # If temperate fraction of woody >= 0.5 classify as a temperate type
    else if(as.numeric(x[['LAI_std_Boreal']]) > Mediterranean.Total & as.numeric(x[['LAI_std_Boreal']]) <= as.numeric(x[['LAI_std_Temperate']])){
      
      # BIOME 6 - Temperate beech and mixed beech forest
      if(x[['LAI_std_MaxTree']] == "Fag_syl") {return(6)}
      # BIOME 7 - Temperate mixed broad-leaved forest
      else {return(7)}
      
    }
    
    
    # BIOME 8 - Themophillous mixed broad-leaved forest
    else if(as.numeric(x[['LAI_std_Boreal']]) < Mediterranean.Total) {return(8)}  
    
  }
  
  
  ###### MISCELLENEOUS  
  
  # BIOME 11  - Steppe Woodland
  if(as.numeric(x[['LAI_std_Woody']]) > 0.5 & as.numeric(x[['LAI_std_Grass']]) > 0.5 & as.numeric(x[['LAI_std_gdd5']]) > 1200) {return(11)}
  
  # BIOME 2 - Arctic/Alpine Tundra
  if(as.numeric(x[['LAI_std_Tree']]) <= 0.5 & as.numeric(x[['LAI_std_gdd5']]) < 1200 & (x[['LAI_std_MaxPFT']] == "BES" | x[['LAI_std_MaxPFT']] == "C3_gr")) {return(2)}
  
  # BIOME 12 - Steppe
  if(as.numeric(x[['LAI_std_Total']]) > 0.2 & as.numeric(x[['LAI_std_gdd5']]) > 1200) {return(12)}
  
  
  
  ###### REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(x[['LAI_std_Lon']]), ",", as.numeric(x[['Lat']]), ")" ))
    print(x)
    return(as.numeric(NA))
  }
  
  
}

#' Meta-data describing the Hickler et al. 2012 european biome scheme for LPJ-GUESS output.
#' 
#' \strong{Hickler2012BiomeScheme}  Classification of european output as per Hickler et al. 2012.
#' 
#' @rdname BiomeScheme-class
#' @export

Hickler2012BiomeScheme <- new("BiomeScheme",
                          new("Quantity",
                              id = "Hickler2012",
                              name = "Hickler2012",
                              colours = grDevices::colorRampPalette(c("Arctic/alpine desert" = "lightblue1",
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
                              format = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                          rules = Hickler2012Rules,
                          layers.needed = list(
                            # Tree
                            list(quantity = "LAI_std", operator = "+", layers = ".Tree", new.layer = "Tree"),
                            # Grass
                            list(quantity = "LAI_std", operator = "+", layers =".Grass", new.layer = "Grass"),
                            # Woody
                            list(quantity = "LAI_std", operator = "+", layers = c(".Tree", ".Shrub"), new.layer = "Woody"),
                            # Mediterranean
                            list(quantity = "LAI_std", operator = "+", layers =c(".Mediterranean"), "new.layer = Mediterranean"),
                            # SupraMediterranean
                            list(quantity = "LAI_std", operator = "+", layers =c(".Supra-mediterranean"), new.layer = "Supra-mediterranean"),
                            # MaxTree
                            list(quantity = "LAI_std", operator = "max.layer", layers =".Tree", new.layer = "MaxTree"),
                            # MaxPFT
                            list(quantity = "LAI_std", operator = "max.layer","layers =.PFTs", new.layer = "MaxPFT"),
                            # GrassFraction
                            list(quantity = "LAI_std", operator = "/", layers =c("Grass", "Total"), new.layer = "GrassFraction"),
                            # BorealFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("Boreal", "Woody"), new.layer = "BorealFractionOfTree"),
                            # TemperateFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers =c("Temperate", "Tree"), new.layer = "TemperateFractionOfTree"),
                            # MediterraneanFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("Mediterranean", "Tree"), new.layer = "MediterraneanFractionOfTree")),
                          # needGDD5 = TRUE, !!! Need to implement this as a layer.needed in the style above above
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
#' @param x Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-8, ordering as in the Forrest et al. 2015 figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

Forrest2015MegaBiomeRules <- function(x){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & as.numeric(x[['LAI_std_TrBEFractionOfTree']]) > 0.6 &  x[['LAI_std_MaxTree']] == "TrBE") {return("Tropical Forest")}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & (as.numeric(x[['LAI_std_TrBRFractionOfTree']]) > 0.6 | as.numeric(x[['LAI_std_TrBRFractionOfTree']])) & (x[['LAI_std_MaxTree']] == "TrBR" | x[['LAI_std_MaxTree']] == "TrTBR")) {return("Savanna and Dry Woodlands")}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & as.numeric(x[['LAI_std_TropicalFractionOfTree']] )> 0.5 &  (x[['LAI_std_MaxTree']] == "TrBE" | x[['LAI_std_MaxTree']] == "TrBR" | x[['LAI_std_MaxTree']] == "TrTBR")) {return("Tropical Forest")}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_BorealFractionOfTree']]) > 0.5 & (x[['LAI_std_MaxTree']] == "BNE" | x[['LAI_std_MaxTree']] == "IBS" | x[['LAI_std_MaxTree']] == "BIBS")) {return("Boreal Forest")}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_BorealFractionOfTree']]) > 0.5 & x[['LAI_std_MaxTree']] == "BNS") {return("Boreal Forest")}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 &  (as.numeric(x[['LAI_std_TeBEFractionOfTree']]) > 0.5 | as.numeric(x[['LAI_std_TeBSFractionOfTree']]) > 0.5) & x[['LAI_std_MaxTree']] == "TeBE") {return("Temperate Evergreen Forest")}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 &  (as.numeric(x[['LAI_std_TeBEFractionOfTree']]) > 0.5 | as.numeric(x[['LAI_std_TeBSFractionOfTree']]) > 0.5) & x[['LAI_std_MaxTree']] == "TeBS") {return("Temperate Deciduous Forest")}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5) {return("Temperate Deciduous Forest") }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5) {return("Temperate Deciduous Forest")}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Tree']]) < 2.5 & as.numeric(x[['LAI_std_GrassFraction']]) < 0.2) {return("Savanna and Dry Woodlands")}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Tree']]) < 2.5 & as.numeric(x[['LAI_std_Total']]) > 2.5) {return("Savanna and Dry Woodlands")}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Tree']]) < 2.5 & as.numeric(x[['LAI_std_Total']]) <= 2.5) {return("Grasslands and Dry Shrublands")}
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(x[['LAI_std_Tree']]) < 0.5 & as.numeric(x[['LAI_std_Total']]) > 0.5 & as.numeric(x[['Lat']]) >= 54) {return("Tundra")}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(x[['LAI_std_Grass']]) > 2.0) {return("Grasslands and Dry Shrublands")}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.2 & as.numeric(x[['LAI_std_Grass']]) < 1.0) {return("Grasslands and Dry Shrublands")}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(x[['LAI_std_Grass']]) > 0.2) {return("Grasslands and Dry Shrublands")}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(x[['LAI_std_Total']]) > 0.2) {return("Grasslands and Dry Shrublands")}
  
  # BIOME 17 - Desert
  else if(as.numeric(x[['LAI_std_Total']]) < 0.2) {return("Desert")}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(x[['LAI_std_Lon']]), ",", as.numeric(x[['LAI_std_Lat']]), ")" ))
    return(NA)
  }
  
  
}

#' Meta-data describing the Forrest et al. 2015 "mega biome" scheme for LPJ-GUESS output.
#' 
#' \strong{Forrest2015BiomeScheme}  Classification of global output into coarser mega biomes as per Forrest et al. 2015 (see appendix), 
#'  which is really just an aggregation of biomes following Harrison and Prentice 2006.  Can be compared to aggregated Haxeltine and Prentice (1996) map.
#'
#' @rdname BiomeScheme-class
#' @export
#'
Forrest2015BiomeScheme <- new("BiomeScheme",
                          new("Quantity",
                              id = "Forrest2015",
                              name = "Forrest et al. 2015",
                              colours = grDevices::colorRampPalette(c("Tropical Forest" = "seagreen",
                                                                      "Temperate Evergreen Forest"= "dodgerblue3",
                                                                      "Temperate Deciduous Forest "= "green3",
                                                                      "Boreal Forest" = "turquoise4",
                                                                      "Savanna and Dry Woodlands" = "olivedrab2",
                                                                      "Grasslands and Dry Shrublands" = "goldenrod2",
                                                                      "Tundra" = "mediumpurple1",
                                                                      "Desert" = "grey75")),
                              units =  c("Tropical Forest",
                                         "Temperate Evergreen Forest",
                                         "Temperate Deciduous Forest",
                                         "Boreal Forest",
                                         "Savanna and Dry Woodlands",
                                         "Grasslands and Dry Shrublands",
                                         "Tundra",
                                         "Desert"),
                              format = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                          rules = Forrest2015MegaBiomeRules,
                          layers.needed = list(  
                            
                            ### Combine shade tolerances
                            list(quantity = "LAI_std", operator = "+", layers = c("BNE", "BINE", "BIBS"), new.layer = "BNE"),
                            list(quantity = "LAI_std", operator = 0, layers = "BINE"),
                            list(quantity = "LAI_std", operator = 0, layers = "BIBS"),
                            list(quantity = "LAI_std", operator = "+", layers = c("TeBS", "TeIBS", "IBS"), new.layer = "TeBS"),
                            list(quantity = "LAI_std", operator = 0, layers = "TeIBS"),
                            list(quantity = "LAI_std", operator = 0, layers = "IBS"),
                            list(quantity = "LAI_std", operator = "+", layers = c("TrBE", "TrIBE"), new.layer = "TrBE"),
                            list(quantity = "LAI_std", operator = 0, layers = "TrIBE"),
                            list(quantity = "LAI_std", operator = 0, layers = "IBS"),    
                            
                            ### Make totals
                            # Tree
                            list(quantity = "LAI_std", operator = "+", layers = ".Tree", new.layer = "Tree"),
                            # Grass
                            list(quantity = "LAI_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                            # Boreal
                            list(quantity = "LAI_std", operator = "+", layers = ".Boreal", new.layer = "Boreal"),
                            # Temperate
                            list(quantity = "LAI_std", operator = "+", layers = ".Temperate", new.layer = "Temperate"),
                            # Tropical
                            list(quantity = "LAI_std", operator = "+", layers = ".Tropical", new.layer = "Tropical"),
                            # Total
                            list(quantity = "LAI_std", operator = "+", layers = ".PFTs", new.layer = "Total"),
                            
                            
                            ### Max tree
                            list(quantity = "LAI_std", operator = "max.layer", layers = ".Tree", new.layer = "MaxTree"),
                            
                            # Make fractions
                            # GrassFraction
                            list(quantity = "LAI_std", operator = "/", layers = c("Grass", "Total"), new.layer = "GrassFraction"),
                            # BorealFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("Boreal", "Tree"), new.layer = "BorealFractionOfTree"),
                            # TemperateFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("Temperate", "Tree"), new.layer = "TemperateFractionOfTree"),
                            # TropicalFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("Tropical", "Tree"), new.layer = "TropicalFractionOfTree"),
                            # TrBEFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("TrBE", "Tree"), new.layer = "TrBEFractionOfTree"),
                            # TrBRFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("TrBR", "Tree"), new.layer = "TrBRFractionOfTree"),
                            # TeBEFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("TeBE", "Tree"), new.layer = "TeBEFractionOfTree"),
                            # TeBSFractionOfTree
                            list(quantity = "LAI_std", operator = "/", layers = c("TeBS", "Tree"), new.layer = "TeBSFractionOfTree")
                            
                          ),
                          data.reference = "Haxeltine and Prentice 1996",
                          published.reference = "Forrest et al 2015, Smith et al. 2014")





#####################################################################
########### MEDITERRANEAN BIOME CLASSIFICATION ######################
#####################################################################

#' Rules to classify coarses Mediterranean biomes
#' 
#' No reference yet
#' 
#' @param x Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-8, ordering as in the Forrest et al. 2015 figure)
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

MeditBiomeRules <- function(x){
  
  # BIOME 1 - Grass Steppe  
  #else if(as.numeric(x[['LAI_std_Grass']]) > 1.0 & as.numeric(x[['LAI_std_Woody']] < 1.0)) {return(7)}
  if(as.numeric(x[['LAI_std_Grass']]) > as.numeric(x[['LAI_std_Woody']])) {return("Grass Steppe or Montane Grassland")}
  # BIOME 4 - Deciduous forest
  #if(as.numeric(x[['LAI_std_Woody']]) > 2.5 && as.numeric(x[['LAI_std_SummergreenFractionOfTree']]) > 0.5) {return(4)}  # 2
  else if(as.numeric(x[['LAI_std_Woody']]) > 2.0 && (x[['LAI_std_MaxWoody']] == "TeBS")) {return("Deciduous Forest")}  # 2
  # BIOME 5 - Cold Montane forest
  else if(as.numeric(x[['LAI_std_Woody']]) > 1.5 && (x[['LAI_std_MaxWoody']] == "BIBS" || x[['LAI_std_MaxWoody']] == "BNE")) {return("Cold Montane Forest")} # 1.5
  # BIOME 2 - Needle-leaved evergreen forest
  else if(as.numeric(x[['LAI_std_Woody']]) > 1.0 && x[['LAI_std_MaxWoody']] == "TeNE") {return("Needle-leaved Woodlands/Forest")} # 1.5
  # BIOME 6 - Pre-steppe deciduous woodlands
  #else if(as.numeric(x[['LAI_std_Woody']]) > 1.0  && as.numeric(x[['LAI_std_Grass']]) > as.numeric(x[['LAI_std_Woody']]) * 0.2 && (x[['LAI_std_MaxWoody']] == "TeBS" || x[['LAI_std_MaxWoody']] == "TeNE")) {return(6)}
  else if(as.numeric(x[['LAI_std_Woody']]) > 1.0  && as.numeric(x[['LAI_std_Grass']]) > as.numeric(x[['LAI_std_Woody']]) * 0.2 && x[['LAI_std_MaxWoody']] == "TeBS") {return("Deciduous Steppe-Woodlands")}
  # BIOME 6 - Pre-steppe deciduous woodlands
  #else if(as.numeric(x[['LAI_std_Woody']]) > 1.0  && as.numeric(x[['LAI_std_Grass']]) > as.numeric(x[['LAI_std_Woody']]) * 0.2 && (x[['LAI_std_MaxWoody']] == "TeBS" || x[['LAI_std_MaxWoody']] == "TeNE")) {return(6)}
  else if(as.numeric(x[['LAI_std_Woody']]) > 1.0  && as.numeric(x[['LAI_std_Grass']]) > as.numeric(x[['LAI_std_Woody']]) * 0.2 && x[['LAI_std_MaxWoody']] == "TeBE") {return("Evergreen Steppe-Woodlands")}
  # BIOME 3 - Mediterranean woodland/scrub
  else if(as.numeric(x[['LAI_std_Woody']]) > 1 && (x[['LAI_std_MaxWoody']] == "TeBE" || x[['LAI_std_MaxWoody']] == "MeES" || x[['LAI_std_MaxWoody']] == "MeRS")) {return("Mediterranean Sclerophyllous Woodlands/Forest")} # 1.5
  # BIOME 7 - Remainder, Unclassified
  else {
    #print(paste("Oops, not classified: Location (", as.numeric(x[['LAI_std_Lon']]), ",", as.numeric(x[['LAI_std_Lat']]), ")" ))
    #print(x)
    return("Unclassifiable/Other")
  }
  
  
}

#' Meta-data describing a Mediterranean scheme for LPJ-GUESS output.
#' 
#' \strong{MeditBiomeScheme} A biome classification scheme used for Turkey.
#' 
#' @rdname BiomeScheme-class
#' @export
#' 
MeditBiomeScheme <- new("BiomeScheme",
                          new("Quantity",
                              id = "MeditBiomeScheme",
                              name = "Mediterranean Biomes",
                              colours = grDevices::colorRampPalette(c("Cold Montane Forest" = "royalblue4",
                                                                      "Deciduous Forest" = "darkgreen",
                                                                      "Deciduous Steppe-Woodlands" = "forestgreen",
                                                                      "Grass Steppe or Montane Grassland" = "lightgreen",
                                                                      "Mediterranean Sclerophyllous Woodlands/Forest" = "red4",
                                                                      "Needle-leaved Woodlands/Forest" = "goldenrod4",
                                                                      "Unclassifiable/Other" = "grey75",
                                                                      "Evergreen Steppe-Woodlands" = "black")),
                              # units =  c("Grass Steppe or Montane Grassland",
                              #            "Needle-leaved Woodlands/Forest",
                              #            "Mediterranean Sclerophyllous Woodlands/Forest",
                              #            "Deciduous Forest",
                              #            "Cold Montane Forest",
                              #            "Deciduous Steppe-Woodlands",
                              #            "Evergreen Steppe-Woodlands",
                              #            "Unclassifiable/Other"),
                              units = c("Cold Montane Forest",
                                        "Deciduous Forest",
                                        "Deciduous Steppe-Woodlands",
                                        "Grass Steppe or Montane Grassland",
                                        "Mediterranean Sclerophyllous Woodlands/Forest",
                                        "Needle-leaved Woodlands/Forest",
                                        "Unclassifiable/Other",
                                        "Evergreen Steppe-Woodlands"),
                              format = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                          rules = MeditBiomeRules,
                          layers.needed = list(
                            ### Combine Shade Tolerances
                            list(quantity = "LAI_std", operator = "+", layers = c("BNE", "BINE", "BIBS"), new.layer = "BNE"),
                            list(quantity = "LAI_std", operator = 0, layers = "BINE"),
                            list(quantity = "LAI_std", operator = 0, layers = "BIBS"),
                            list(quantity = "LAI_std", operator = "+", layers = c("TeBS", "TeIBS", "IBS"), new.layer = "TeBS"),
                            list(quantity = "LAI_std", operator = 0, layers = "TeIBS"),
                            list(quantity = "LAI_std", operator = 0, layers = "IBS"),
                            ### Calculate Totals
                            # Tree
                            list(quantity = "LAI_std", operator = "+", layers = ".Tree", new.layer = "Tree"),
                            # Grass
                            list(quantity = "LAI_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                            # Woody
                            list(quantity = "LAI_std", operator = "+", layers = c(".Tree", ".Shrub"), new.layer = "Woody"),
                            ### Max woody
                            list(quantity = "LAI_std", operator = "max.layer", layers = c(".Tree", ".Shrub"), new.layer = "MaxWoody")
                          ),
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
#' @param x Numerical vector of LAI values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @keywords internal
#' @return Biomes code (1-13)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
MegaBiomeRules_dev <- function(x){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(x[['LAI_std_Tree']]) > 2.5 &  x[['LAI_std_MaxTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & x[['LAI_std_MaxTree']] == "TrBR") {return(2)}
  
  # BIOME 3 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 &  (x[['LAI_std_MaxTree']] == "BNE" | x[['LAI_std_MaxTree']] == "IBS" | x[['LAI_std_MaxTree']] == "BIBS")) {return(3)}
  
  # BIOME 4 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 &  x[['LAI_std_MaxTree']] == "BNS") {return(4)}
  
  # BIOME 5 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 & (x[['LAI_std_MaxTree']] == "TeBE" | x[['LAI_std_MaxTree']] == "TeNE")) {return(5)}
  
  # BIOME 6 - Temperate Deciduous Forest
  else if(as.numeric(x[['LAI_std_Tree']]) > 2.5 &  x[['LAI_std_MaxTree']] == "TeBS") {return(6)}
  
  # BIOME 10 - Arctic/alpine Tundra
  else if(as.numeric(x[['LAI_std_Tree']]) < 0.1 & as.numeric(x[['LAI_std_Total']]) > 0.5 & (as.numeric(x[['LAI_std_Lat']]) >= 54 | as.numeric(x[['LAI_std_GDD5']]) < 400)) {return(10)}
  
  # BIOME 7 - Xeric Woodland/Shrubland
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_GrassFraction']]) < 0.2) {return(7)}
  
  # BIOME 8 - Moist Savanna
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5 & as.numeric(x[['LAI_std_Total']]) > 2.5) {return(8)}
  #else if(as.numeric(x[['LAI_std_Tree']]) > 0.5) {return(9)}
  
  # BIOME 9 - Dry Savanna
  else if(as.numeric(x[['LAI_std_Tree']]) > 0.5  & as.numeric(x[['LAI_std_Total']]) <= 2.5) {return(9)}
  
  # BIOME 11 - Tall Grassland
  else if(as.numeric(x[['LAI_std_Grass']]) > 2.0) {return(11)}
  
  # BIOME 12  - Arid Shrubland/Grassland
  else if(as.numeric(x[['LAI_std_Total']]) > 0.2) {return(12)}
  
  # BIOME 13 - Desert
  else if(as.numeric(x[['LAI_std_Total']]) < 0.2) {return(13)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(x[['LAI_std_Lon']]), ",", as.numeric(x[['LAI_std_Lat']]), ")" ))
    return(NA)
  }
  
}


#' Meta-data describing an unpublished "mega biome" scheme for LPJ-GUESS output.
#' 
#' \strong{DevMegaBiomeScheme}  Unpublished for potentially useful classification of global biomes which is slightly simpler than Smith et al. 2014.
#' 
#' @rdname BiomeScheme-class
#' @export
#' 
DevMegaBiomeScheme <- new("BiomeScheme",
                             new("Quantity",
                                 id = "Megabiomes",
                                 name = "Megabiomes",
                                 colours = grDevices::colorRampPalette(c("Tropical Rain Forest" = "seagreen",
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
                                 format = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                             rules = MegaBiomeRules_dev,
                             # needGDD5 = TRUE, !!! Need to implement this as a layer.needed in the style above above
                             layers.needed = list(
                               # Max Tree
                               list(quantity = "LAI_std", operator = "max.layer", layers = c(".Tree"), new.layer = "MaxTree")
                             ),
                             data.reference = "Haxeltine and Prentice 1996",
                             published.reference = "-")



#################################################################################################################################
########### FIREMIP BIOMES ##########################
#################################################################################################################################

#' Rules to classify FireMIP biomes from fractionalcover of PFTs
#' 
#' Unpublished but possibly useful in principle.  Simpler classes that Smith et al 2014,
#' but more complex that Forrest et al 2015.  
#' 
#' @param x Numerical vector of FPC values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @keywords internal
#' @return Biomes code (1-10)
FireMIPBiomeRules <- function(x){
  
  # BIOME 1 & 2 - croplands mosaics
  
  if(as.numeric(x[['landCoverFrac_Crops']]) > 0.5) { return(1) }
  
  if(as.numeric(x[['landCoverFrac_Crops']]) > 0.2) { return(2) }
  
  # BIOMES 2-6 - Forests
  #if(as.numeric(x[['landCoverFrac_Tree']]) > 0.6 &  x[['landCoverFrac_Grass']] < 0.4) {
  if(as.numeric(x[['landCoverFrac_Tree']]) > 0.6) {
    
    
    # BIOME 2 Needleleaved forest
    if(x[['landCoverFrac_MaxTree']] == "NE") { return(3) }
    
    # BIOME 3 NS forest
    else if(x[['landCoverFrac_MaxTree']] == "NS") { return(4) }
    
    # BIOME 4 Summergreen forest
    else if(x[['landCoverFrac_MaxTree']] == "BS") { return(5) }
    
    # BIOME 5 Evergreen forest
    else if(x[['landCoverFrac_MaxTree']] == "BE") { return(6) }
    
    # BIOME 6 Raingreen forest
    else if(x[['landCoverFrac_MaxTree']] == "BR") { return(7) }
    
  }
  
  # BIOME 3 Additional - Lower tree cover requirement for Needle-leaved Summergreen Forest
  else if(as.numeric(x[['landCoverFrac_Tree']]) > 0.4 && x[['landCoverFrac_MaxTree']] == "NS") { return(4) }
  
  
  # BIOMES 7 and 8 - Grassy systems
  else if(as.numeric(x[['landCoverFrac_Grass']]) > 0.4 ) {
    
    # BIOME 7 C4 grassy system
    if(x[['landCoverFrac_MaxGrass']] == "landCoverFrac_C4G") { return(8) }
    
    # BIOME 8 C3 grassy system
    else if(x[['landCoverFrac_MaxGrass']] == "landCoverFrac_C3G") { return(9) }
    
  }
  
  # BIOMES 9 - Shrublands
  else if(as.numeric(x[['landCoverFrac_Shrub']]) > 0.3 ) { return(10) }
  
  # BIOME 10 - Sparse vegetation
  else if(as.numeric(x[['landCoverFrac_Total']]) > 0.2 ) { return(11) }
  
  # BIOME 11 Barren/Unclassified 
  else { return(12) }
  
  
}


#' Meta-data describing FireMIP biomes
#' 
#' \strong{FireMIPBiomeScheme}  Simple biomes based on a common PFT set for the FireMIP models
#' 
#' @rdname BiomeScheme-class
#' @export
#' 
FireMIPBiomeScheme <- new("BiomeScheme",
                            new("Quantity",
                                id = "FireMIP",
                                name = "FireMIP Biomes",
                                colours = grDevices::colorRampPalette(c("Croplands Dominated \n(Croplands > 50%)" = "chartreuse",
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
                                format = c("LPJ-GUESS-SPITFIRE-FireMIP",
                                           "LPJ-GUESS-BLAZE-FireMIP",
                                           "LPJ-GUESS-GlobFIRM-FireMIP",
                                           "CLM-FireMIP",
                                           "CTEM-FireMIP",
                                           "JSBACH-FireMIP",
                                           "Inferno-FireMIP",
                                           "ORCHIDEE-FireMIP")),
                            rules = FireMIPBiomeRules,
                            layers.needed = list(
                              ## Totals
                              # Tree
                              list(quantity = "landCoverFrac", operator = "+", layers = ".Tree", new.layer = "Tree"),
                              # Grass
                              list(quantity = "landCoverFrac", operator = "+", layers = ".Grass", new.layer ="Grass"),
                              ### Maxes
                              # Tree
                              list(quantity = "landCoverFrac", operator = "max.layer", layers = ".Tree", new.layer = "MaxTree"),
                              # Grass
                              list(quantity = "landCoverFrac", operator = "max.layer", layers = ".Grass", new.layer ="MaxGrass")
                              ),
                            data.reference = "-",
                            published.reference = "-")



#################################################################################################################################
###########  ##########################
#################################################################################################################################

#' Rules for biome scheme based on FPC
#' 
#' lalala
#' 
#' @param fpc Vector of FPC values
#' @keywords internal
FPCMegaBiomeRules <- function(x) {
  
  # 9 - Desert
  if (as.numeric(x[['fpc_Total']]) <= 0.2 &
      as.numeric(x[['fpc_GDD5']]) >= 1200) {return(9)}
  
  # 10 - Arctic desert
  else if (as.numeric(x[['fpc_Total']]) <= 0.2 &
           as.numeric(x[['fpc_GDD5']]) < 1200) {return(10)}
  
  # 1 - Tropical Forest
  else if (as.numeric(x[['fpc_Tree']]) > 0.6 &
           as.numeric(x[['fpc_Tropical']]) > as.numeric(x[['fpc_Temperate']]) &
           as.numeric(x[['fpc_Tropical']]) > as.numeric(x[['fpc_Boreal']])) {return(1)}
  
  # 2 - Temperate Forest
  else if (as.numeric(x[['fpc_Tree']]) > 0.6 &
           as.numeric(x[['fpc_Temperate']]) > as.numeric(x[['fpc_Tropical']]) &
           as.numeric(x[['fpc_Temperate']]) > as.numeric(x[['fpc_Boreal']])) {return(2)}
  
  # 3 - Boreal Forest
  else if (as.numeric(x[['fpc_Tree']]) > 0.2 &
           as.numeric(x[['fpc_Boreal']]) > as.numeric(x[['fpc_Temperate']]) &
           as.numeric(x[['fpc_Boreal']]) > as.numeric(x[['fpc_Tropical']])) {return(3)}
  
  # 4 - Savannah
  else if (as.numeric(x[['fpc_Tree']]) > 0.1 &
           as.numeric(x[['fpc_Tree']]) <= 0.6 &
           as.numeric(x[['fpc_C4G']]) >= as.numeric(x[['fpc_C3G']])) {return(4)}
  
  # 5 - Temperate woodland
  else if (as.numeric(x[['fpc_Tree']]) > 0.1 &
           as.numeric(x[['fpc_Tree']]) <= 0.6 &
           as.numeric(x[['fpc_C4G']]) < as.numeric(x[['fpc_C3G']]) &
           as.numeric(x[['fpc_GDD5']]) > 1800) {return(5)}
  
  # 6 - Tropical grassland
  else if (as.numeric(x[['fpc_Tree']]) <= 0.1 &
           as.numeric(x[['fpc_C4G']]) >= as.numeric(x[['fpc_C3G']])) {return(6)}
  
  # 7 - Temperate grassland
  else if (as.numeric(x[['fpc_Tree']]) <= 0.1 &
           as.numeric(x[['fpc_C4G']]) < as.numeric(x[['fpc_C3G']]) &
           as.numeric(x[['fpc_GDD5']]) > 1200) {return(7)}
  
  # 8 - Tundra
  else if (as.numeric(x[['fpc_Tree']]) <= 0.2) {return(8)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(x[['Lon']]), ",", as.numeric(x[['Lat']]), ")" ))
    return(NA)
  }
}

#' Meta-data for an FPC-based mega biome scheme
#' 
#' \strong{FPCMegaBiome} an unpublished scheme based on fractional cover developed by Joerg Steinkamp
#'
#' @rdname BiomeScheme-class
#' @export
#' 
FPCMegaBiomeScheme <- new("BiomeScheme",
                            new("Quantity",
                                id = "FPCMegaBiomeScheme",
                                name = "FPCMegaBiomeScheme",
                                colours = grDevices::colorRampPalette(c("Tropical forest" = "darkgreen",
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
                                format = c("LPJ-GUESS", "LPJ-GUESS-SPITFIRE")),
                            rules = FPCMegaBiomeRules,
                            layers.needed = list(
                              # Tree
                              list(quantity = "fpc", operator = "+", layers = ".Tree", new.layer = "Tree"),
                              # Grass
                              list(quantity = "fpc", operator = "+", layers = ".Grass", new.layer = "Grass"),
                              # Boreal
                              list(quantity = "fpc", operator = "+", layers = ".Boreal", new.layer = "Boreal"),
                              # Temperate
                              list(quantity = "fpc", operator = "+", layers = ".Temperate", new.layer = "Temperate"),
                              # Tropical
                              list(quantity = "fpc", operator = "+", layers = ".Tropical", new.layer = "Tropical")
                            ),
                            # needGDD5 = TRUE, !!! Need to implement this as a layer.needed in the style above above
                            data.reference = "-",
                            published.reference = "-")




#####################################################################
########### ADGVM2 BIOME CLASSIFICATION SCHEMES #####################
#####################################################################



#####################################################################
########### SIMPLE BIOME CLASSIFICATION #############################
#####################################################################

#' Rules to classify coarse tropical biomes
#' 
#' No reference yet
#' 
#' @param x Numerical vector of vegetation over values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-5, baren, C4 grassland, C3 grassland, woodland, forest)
#' @keywords internal
#' @author Simon Scheiter \email{Simon.Scheiter@@senckenberg.de}

SimpleAdgvm2BiomeRules <- function(x){
  # BIOME 1 - Baren/Desert
  if(      as.numeric(x[['vegcover_std_Grass']])<=2 & as.numeric(x[['vegcover_std_Tree']])<=5) {return("Baren/Desert")}
  # BIOME 2 - C4 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Tree']])<=5 & as.numeric(x[['vegcover_std_C3G']])<=as.numeric(x[['vegcover_std_C4G']])) {return("C4 Grassland")}
  # BIOME 3 - C3 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Tree']])<=5 & as.numeric(x[['vegcover_std_C3G']])> as.numeric(x[['vegcover_std_C4G']])) {return("C3 Grassland")}
  # BIOME 4 - Woodland
  else if(as.numeric(x[['vegcover_std_Tree']])> 5 & as.numeric(x[['vegcover_std_Tree']])<=60) {return("Woodland")}
  # BIOME 5 - Forest
  else if(as.numeric(x[['vegcover_std_Tree']])> 60 ) {return("Forest")}
  # BIOME 6 - Remainder, Unclassified
  else {
    return("Unclassifiable/Other")
  }
}

#' Meta-data describing a simple scheme for aDGVM2 output.
#' 
#' \strong{SimpleAdgvm2BiomeScheme} SS document here
#' 
#' @rdname BiomeScheme-class
#' @export
#' @author Simon Scheiter \email{Simon.Scheiter@@senckenberg.de}
SimpleAdgvm2BiomeScheme <- new("BiomeScheme",
                                 new("Quantity",
                                     id = "SimpleAdgvm2BiomeScheme",
                                     name = "Simple aDGVM2 Biomes",
                                     colours = grDevices::colorRampPalette(c("Baren/Desert" = '#cccccc',
                                                                             "C4 Grassland" = '#fff700',
                                                                             "C3 Grassland" = '#ffcf0f',
                                                                             "Woodland" = '#94b6ff',
                                                                             "Forest" = '#308a0a',
                                                                             "Unclassifiable/Other" = "grey75")),
                                     units = c("Baren/Desert",
                                               "C4 Grassland",
                                               "C3 Grassland",
                                               "Woodland",
                                               "Forest",
                                               "Unclassifiable/Other"),
                                     format = c("aDGVM")),
                                 rules = SimpleAdgvm2BiomeRules,
                                 layers.needed = list( list(quantity = "vegcover_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                                                       list(quantity = "vegcover_std", operator = "+", layers = ".Tree", new.layer = "Tree")),
                                 data.reference = "-",
                                 published.reference = "-")




#####################################################################
########### SIMPLE BIOME CLASSIFICATION WITH HEIGHT #################
#####################################################################

#' Rules to classify coarse tropical biomes
#' 
#' No reference yet
#' 
#' @param x Numerical vector of vegetation over values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-5, baren, C4 grassland, C3 grassland, small forest, tall forest)
#' @keywords internal
#' @author Simon Scheiter \email{Simon.Scheiter@@senckenberg.de}
SimpleHeightAdgvm2BiomeRules <- function(x){
  # BIOME 1 - Baren/Desert
  if(      as.numeric(x[['vegcover_std_Grass']])<=2 & as.numeric(x[['vegcover_std_Tree']])<=5) {return("Baren/Desert")}
  # BIOME 2 - C4 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Tree']])<=5 & as.numeric(x[['vegcover_std_C3G']])<=as.numeric(x[['vegcover_std_C4G']])) {return("C4 Grassland")}
  # BIOME 3 - C3 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Tree']])<=5 & as.numeric(x[['vegcover_std_C3G']])> as.numeric(x[['vegcover_std_C4G']])) {return("C3 Grassland")}
  # BIOME 4 - Small forest
  else if(as.numeric(x[['vegcover_std_Tree']])> 5 & as.numeric(x[['canopyheight_std_Tree']])<=5) {return("Small forest")}
  # BIOME 5 - Tall forest
  else if(as.numeric(x[['vegcover_std_Tree']])> 5 & as.numeric(x[['canopyheight_std_Tree']])> 5) {return("Tall forest")}
  # BIOME 6 - Remainder, Unclassified
  else {
    return("Unclassifiable/Other")
  }
}

#' Meta-data describing a simple scheme for aDGVM2 output.
#' 
#' \strong{SimpleHeightAdgvm2BiomeScheme} SS document here
#' @rdname BiomeScheme-class
#' @export
#' 
SimpleHeightAdgvm2BiomeScheme <- new("BiomeScheme",
                                       new("Quantity",
                                           id = "SimpleHeightAdgvm2BiomeScheme",
                                           name = "Simple aDGVM2 Biomes",
                                           colours = grDevices::colorRampPalette(c("Baren/Desert" = '#cccccc',
                                                                                   "C4 Grassland" = '#fff700',
                                                                                   "C3 Grassland" = '#ffcf0f',
                                                                                   "Small forest" = '#94b6ff',
                                                                                   "Tall forest" = '#308a0a',
                                                                                   "Unclassifiable/Other" = "grey75")),
                                           units = c("Baren/Desert",
                                                     "C4 Grassland",
                                                     "C3 Grassland",
                                                     "Small forest",
                                                     "Tall forest",
                                                     "Unclassifiable/Other"),
                                           format = c("aDGVM")),
                                       rules = SimpleHeightAdgvm2BiomeRules,
                                       layers.needed = list( list(quantity = "vegcover_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                                                             list(quantity = "vegcover_std", operator = "+", layers = ".Tree", new.layer = "Tree"),
                                                             list(quantity = "canopyheight_std", operator = "+", layers = ".Tree", new.layer = "Tree"),
                                                             list(quantity = "canopyheight_std", operator = "+", layers = ".Grass", new.layer = "Grass")),
                                       data.reference = "-",
                                       published.reference = "-")



#####################################################################
########### BIOME CLASSIFICATION, GROWTH FORM #######################
#####################################################################

#' Rules to classify coarse tropical biomes
#' 
#' No reference yet
#' 
#' @param x Numerical vector of vegetation over values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-5, baren, C4 grassland, C3 grassland, woodland, shrubland)
#' @keywords internal
#' @author Simon Scheiter \email{Simon.Scheiter@@senckenberg.de}
GrowthFormAdgvm2BiomeRules <- function(x){
  # BIOME 1 - Baren/Desert
  if(      as.numeric(x[['vegcover_std_Grass']])<=2 & as.numeric(x[['vegcover_std_Woody']])<=5) {return("Baren/Desert")}
  # BIOME 2 - C4 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Woody']])<=5 & as.numeric(x[['vegcover_std_C3G']])<=as.numeric(x[['vegcover_std_C4G']])) {return("C4 Grassland")}
  # BIOME 3 - C3 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Woody']])<=5 & as.numeric(x[['vegcover_std_C3G']])> as.numeric(x[['vegcover_std_C4G']])) {return("C3 Grassland")}
  # BIOME 4 - Woodland
  else if( as.numeric(x[['vegcover_std_Woody']])> 5 & as.numeric(x[['vegcover_std_Tree']])> as.numeric(x[['vegcover_std_Shrub']])) {return("Woodland")}
  # BIOME 5 - Shrubland
  else if( as.numeric(x[['vegcover_std_Woody']])> 5 & as.numeric(x[['vegcover_std_Tree']])<=as.numeric(x[['vegcover_std_Shrub']])) {return("Shrubland")}
  # BIOME 6 - Remainder, Unclassified
  else {
    return("Unclassifiable/Other")
  }
}

#' Meta-data describing a simple scheme for aDGVM2 output.
#' 
#'\strong{GrowthFormAdgvm2BiomeSchemes} SS document here
#'
#' @rdname BiomeScheme-class
#' @export
#' 
GrowthFormAdgvm2BiomeScheme <- new("BiomeScheme",
                                     new("Quantity",
                                         id = "GrowthFormAdgvm2BiomeScheme",
                                         name = "Growth Form aDGVM2 Biomes",
                                         colours = grDevices::colorRampPalette(c("Baren/Desert" = '#cccccc',
                                                                                 "C4 Grassland" = '#fff700',
                                                                                 "C3 Grassland" = '#ffcf0f',
                                                                                 "Woodland" = '#94b6ff',
                                                                                 "Shrubland" = '#d6c100',
                                                                                 "Unclassifiable/Other" = "grey75")),
                                         units = c("Baren/Desert",
                                                   "C4 Grassland",
                                                   "C3 Grassland",
                                                   "Woodland",
                                                   "Shrubland",
                                                   "Unclassifiable/Other"),
                                         format = c("aDGVM")),
                                     rules = GrowthFormAdgvm2BiomeRules,
                                     layers.needed = list( list(quantity = "vegcover_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                                                           list(quantity = "vegcover_std", operator = "+", layers = c(".Tree", ".Shrub"), new.layer = "Woody"),
                                                           list(quantity = "vegcover_std", operator = "+", layers = ".Shrub", new.layer = "Shrub"),
                                                           list(quantity = "vegcover_std", operator = "+", layers = ".Tree",  new.layer = "Tree")),
                                     data.reference = "-",
                                     published.reference = "-")

#####################################################################
########### BIOME CLASSIFICATION, PHENOLOGY #########################
#####################################################################

#' Rules to classify coarse tropical biomes
#' 
#' No reference yet
#' 
#' @param x Numerical vector of vegetation over values for a particular location. 
#' Certain fractions and quantities should have been pre-calculated.
#' 
#' @return Biomes code (1-6, baren, C4 grassland, C3 grassland, woodland, evergreen forest, deciduous forest)
#' @keywords internal
#' @author Simon Scheiter \email{Simon.Scheiter@@senckenberg.de}
PhenologyAdgvm2BiomeRules <- function(x){
  # BIOME 1 - Baren/Desert
  if(      as.numeric(x[['vegcover_std_Grass']])<=2 & as.numeric(x[['vegcover_std_Woody']])<=5) {return("Baren/Desert")}
  # BIOME 2 - C4 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Woody']])<=5 & as.numeric(x[['vegcover_std_C3G']])<=as.numeric(x[['vegcover_std_C4G']])) {return("C4 Grassland")}
  # BIOME 3 - C3 grassland
  else if( as.numeric(x[['vegcover_std_Grass']])> 2 & as.numeric(x[['vegcover_std_Woody']])<=5 & as.numeric(x[['vegcover_std_C3G']])> as.numeric(x[['vegcover_std_C4G']])) {return("C3 Grassland")}
  # BIOME 4 - Woodland
  else if( as.numeric(x[['vegcover_std_Woody']])> 5 & as.numeric(x[['vegcover_std_Woody']])<=60) {return("Woodland")}
  # BIOME 5 - Deciduous forest
  else if( as.numeric(x[['vegcover_std_Woody']])>60 & as.numeric(x[['vegcover_std_Evergreen']])<=as.numeric(x[['vegcover_std_Raingreen']])) {return("Deciduous Forest")}
  # BIOME 6 - Evergreen forest
  else if( as.numeric(x[['vegcover_std_Woody']])>60 & as.numeric(x[['vegcover_std_Evergreen']])> as.numeric(x[['vegcover_std_Raingreen']])) {return("Evergreen Forest")}
  # BIOME 7 - Remainder, Unclassified
  else {
    return("Unclassifiable/Other")
  }
}

#' Meta-data describing a simple scheme for aDGVM2 output.
#' 
#'\strong{PhenologyAdgvm2BiomeScheme} SS document here
#' 
#' 
#' @rdname BiomeScheme-class
#' @export
#' 
PhenologyAdgvm2BiomeScheme <- new("BiomeScheme",
                                    new("Quantity",
                                        id = "PhenologyAdgvm2BiomeScheme",
                                        name = "Phenology aDGVM2 Biomes",
                                        colours = grDevices::colorRampPalette(c("Baren/Desert" = '#cccccc',
                                                                                "C4 Grassland" = '#fff700',
                                                                                "C3 Grassland" = '#ffcf0f',
                                                                                "Woodland" = '#94b6ff',
                                                                                "Deciduous Forest" = '#9c6007',
                                                                                "Evergreen Forest" = '#308a0a',
                                                                                "Unclassifiable/Other" = "grey75")),
                                        units = c("Baren/Desert",
                                                  "C4 Grassland",
                                                  "C3 Grassland",
                                                  "Woodland",
                                                  "Deciduous Forest",
                                                  "Evergreen Forest",
                                                  "Unclassifiable/Other"),
                                        format = c("aDGVM")),
                                    rules = PhenologyAdgvm2BiomeRules,
                                    layers.needed = list( list(quantity = "vegcover_std", operator = "+", layers = ".Grass", new.layer = "Grass"),
                                                          list(quantity = "vegcover_std", operator = "+", layers = c(".Tree", ".Shrub"), new.layer = "Woody"),
                                                          list(quantity = "vegcover_std", operator = "+", layers = ".Evergreen", new.layer = "Evergreen"),
                                                          list(quantity = "vegcover_std", operator = "+", layers = ".Raingreen",  new.layer = "Raingreen")),
                                    data.reference = "-",
                                    published.reference = "-")





#' Currently supported biome schemes
#' 
#' This is a list of all BiomeSchemes defined by DGVMTools.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
supported.biome.schemes <- c("Smith2014" = Smith2014BiomeScheme,
                             "Hickler2012" = Hickler2012BiomeScheme,
                             "Forrest2015" = Forrest2015BiomeScheme,
                             "DevMegaBiomes" = DevMegaBiomeScheme,
                             "FPCMegaBiomes" = FPCMegaBiomeScheme,
                             "MeditBiomes" = MeditBiomeScheme,
                             "FireMIPBiomes" = FireMIPBiomeScheme,
                             "SimpleAdgvm2Biomes" = SimpleAdgvm2BiomeScheme,
                             "SimpleHeightAdgvm2Biomes" = SimpleHeightAdgvm2BiomeScheme,
                             "GrowthFormAdgvm2Biomes" = GrowthFormAdgvm2BiomeScheme,
                             "PhenologyAdgvm2Biomes" = PhenologyAdgvm2BiomeScheme)
