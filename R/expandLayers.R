
############# EXPAND LAYERS
#'
#' Expand plotting/processing layers from convenient short hand codes.
#' 
#' Expands characters strings with particular meaning (eg. "lifefoms", "pfts", "seasons", etc...).  
#' This allows the user to simply specify, for example, "lifeforms" to a plotting command instead of c("Tree","Grass","Shrub") when plotting or processing.
#'
#' @details Supported short-hand strings are:
#' \itemize{
#'   \item{"PFTs"}{ expands to a vector of each of the ids of PFTs present in the data (requires the \code{PFT.set} argument).}
#'   \item{"lifeforms"}{ expands all the different PFT lifeforms present as determined by the lifeform slots of the \code{PFTs} argument (and optionally "Woody").  For example c("Tree", "Grass", "Woody")}
#'   \item{"zones"}{ expands all the different PFT climactic zones present as determined by the zone slots of the \code{PFTs} argument.  For example c("Temperature", "Boreal", "Tropical")}
#'   \item{"leafforms"}{ expands all the different PFT leaf forms present as determined by the leafforms slot of the \code{PFTs} argument.  For example c("Needleleved", "Broadleaved")}
#'   \item{"phenologies"}{ expands all the different PFT leaf forms present as determined by the phenology slot of the \code{PFTs} argument.  For example c("Evergreen", "Summergreen", "Raingreen", "GrassPhenology)}
#' }
#' 
#' 
#' @param layers Character vector of layers to expand
#' @param input.data The data to which the layers will be applied, may be a \code{Field}, a Raster* object, data.table or data.frame, a Spatial*DataFrame. 
#' @param PFT.set List of a superset of PFTs that might be found in this run (only necessary if *not* passing a \code{Field}).
#' @param type Character string identifying if this is a monthly  (= "monthly") or per PFT (="pft") variable.  Ignored for \code{Fields} which supply this data themselves.
#' The function attempts to determine this argument if it is not provided.  
#' @param include.woody If TRUE and "lifeform" is included in the layer list "Woody" is also returned
#' @return A list of layers
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
expandLayers <- function(layers, input.data, PFT.set = NULL, type = "unknown", include.woody = TRUE){
  
  # remove "Lon", "Lat" and "Year" if present
  for(remove.header.from.header in c("Lat", "Lon", "Year")){
    if(remove.header.from.header %in% layers) layers <- layers[-which(layers == remove.header.from.header)]
  }
  
  # read meta-data form Field if possible
  if(is.Field(input.data)) {
    type <- input.data@quant@type
    if(is.null(PFT.set)) PFT.set <- input.data@source@pft.set
    header <- names(input.data@data)
  }
  else{
    header <- names(input.data)
  }
  
  # get PFTs present in data
  PFTs <- getPFTs(input.data, PFT.set)
  print(PFTs)
  # if type is undertermined, try to figure it out
  # if found at least one PFT and that PFT is not "Total" then assume we have a per PFT variable
  if(type == "unknown"){
    if(length(PFTs) > 0) {
      if(PFTs[[1]]@id != "Total") {
        type <- "pft"
      }
    }
  }
  
  if(tolower(type) == "pft") {
    
    
    
    # expands "pfts"
    if("pft" %in% tolower(layers) || "pfts" %in% tolower(layers)) {
      for(PFT in PFTs) {
        layers <- append(layers, PFT@id)
      }
      
      if("pft" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "pft")]
      if("pfts" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "pfts")]
    }
    
    
    # expand "lifeforms" 
    if("lifeforms" %in% tolower(layers) | "lifeform" %in% tolower(layers)){
      
      # Find all lifeforms present in PFTs 
      all.lifeforms <- vector()
      for(PFT in PFTs) {all.lifeforms <- append(all.lifeforms, PFT@lifeform)}
      all.lifeforms <- unique(all.lifeforms)
      # MF: Special case to combine Trees and Shrubs inoo Woody category
      if(include.woody) all.lifeforms <- append(all.lifeforms, "Woody")
      
      layers <- append(layers, all.lifeforms)
      
      if("lifeforms" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "lifeforms")]
      if("lifeform" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "lifeform")]
      
    }
    
    
    # expand "zones" 
    if("zones" %in% tolower(layers) | "zone" %in% tolower(layers)){
      
      # Find all zones present in PFTs 
      all.zones <- vector()
      for(PFT in PFTs) {all.zones <- append(all.zones, PFT@zone)}
      all.zones <- unique(all.zones)
      
      layers <- append(layers, all.zones)
      
      if("zones" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "zones")]
      if("zone" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "zone")]    
      
    }
    
    # Expand "leafforms" 
    if("leafforms" %in% tolower(layers) | "leafform" %in% tolower(layers)){
      print("leaf")
      # Find all leafforms present in PFTs 
      all.leafforms <- vector()
      for(PFT in PFTs) {all.leafforms <- append(all.leafforms, PFT@leafform)}
      all.leafforms <- unique(all.leafforms)
      
      layers <- append(layers, all.leafforms)
      
      if("leafforms" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "leafforms")]
      if("leafform" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "leafform")] 
      
    }
    
    # Expand "phenologies" 
    if("phenologies" %in% tolower(layers) | "phenology" %in% tolower(layers)){
      
      # Find all phenologys present in PFTs 
      all.phenologies <- vector()
      for(PFT in PFTs) {all.phenologies <- append(all.phenologies, PFT@phenology)}
      all.phenologies <- unique(all.phenologies)
      #if("NA" in all.phenologies) all.phenologies <- all.phenologies[-which(tolower(all.phenologies) == "NA")]
      
      layers <- append(layers, all.phenologies)
      
      if("phenologies" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "phenologies")]
      if("phenology" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "phenology")] 
      
    }
    
    
  }
  
  if("NA" %in% layers) layers <- layers[-which(layers == "NA")]
  
  
  return(layers)
  
}
