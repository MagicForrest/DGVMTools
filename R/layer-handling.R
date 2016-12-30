#' Copy layers from one VegObject or ModelObject to another
#' 
#' This function allows layers (reminder, they are implemented as columns in a data.table) to be copied from one ModelObject/DataObject to another.  
#' This is particularly useful for colouring or facetting a plot of one variable by another one say.  To give a more concrete example, one could use a biome classification 
#' to split (facet) a data-vs-model scatter plot.
#' 
#' @param from The ModelObject/DataObject that the layers are to be copied from.
#' @param to The ModelObject/DataObject that the layers are to be copied to.
#' @param layer.names The layers to be copied from the "from" argument
#' @param new.layer.names The new names that the layers should have in the 'to' object. Use this to avoid naming conflict whereby, for 
#' example, if a layer "Total" is copied to an object which already has a "Total" layer then they layers will be names "Total.x" and "Total.y".  
#' @param keep.all.to Boolean, if set to FALSE, all points in the 'to' object which don't have corresponding points in the 'from' object are removed.
#' @param keep.all.from Boolean, if set to FALSE, all points in the 'from' object which don't have corresponding points in the 'to' object are removed.
#' @param dec.places Numeric, how many decimal places to rounds the coordinates to inorder to get a match.  Default is no rounding (value is NULL) and if dine for most regularing spaced grids.  
#' But setting this can be useful to force matching of coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' 
#' @description This function does not check the Lon, Lat and Year columns are identical.  Any points in the 'from' object which are not in the 'to' object are ignored, 
#' and any points in the 'to' object which don't have corresponding points in the 'from' object are assigned NA, unless keep.all.to is set to FALSE .
#'
#' @return A ModelObject (or data.table) comprising the 'to' object with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
copyLayers <- function(from, to, layer.names, new.layer.names = NULL, keep.all.to = TRUE, keep.all.from = TRUE, dec.places = NULL) {
  
  Lon = Lat = NULL
  
  # first some pre-amble and checks
  common.layers <- c()
  from.dt <- from@data
  for(dim in c("Lon", "Lat", "Year")) {
    
    if(dim %in% names(from@data) && dim %in% names(to@data)) { common.layers <- append(common.layers, dim)}         
    else if(!(dim %in% names(from@data)) && (dim %in% names(to@data))) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"to\" argument but not the \"from\" argument") 
    }
    else if(dim %in% names(from@data) && !(dim %in% names(to@data))) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"from\" argument but not the \"to\" argument") 
    }
  }
  
  
  layers.to.add.dt <- from@data[, append(common.layers, layer.names), with=FALSE]
  if(!is.null(new.layer.names)) setnames(layers.to.add.dt, append(common.layers, new.layer.names))
  if(!is.null(dec.places)) {
    to.dt <- copy(to@data)
    to.dt[, Lon := round(Lon, dec.places)]
    to.dt[, Lat := round(Lat, dec.places)]
    setKeyDGVM(to.dt)
    layers.to.add.dt[, Lon := round(Lon, dec.places)]
    layers.to.add.dt[, Lat := round(Lat, dec.places)]
    
   Temp.dt <- merge(x = round(to.dt, dec.places), y = round(layers.to.add.dt, dec.places), all.x = keep.all.from, all.y = keep.all.to)
  }
  else {
    Temp.dt <- merge(x = to@data, y = layers.to.add.dt, all.x = keep.all.from, all.y = keep.all.to)
  }
  to@data <- Temp.dt
  return(to)
  
}

#' Select a subset of layers from a DataObject or ModelObject
#' 
#' This function returns a copy of a DataObject or ModelObject containing only a selected subset of layers
#' 
#' @param x A DataObject or ModelObject
#' @param layers A vector of characters strings specifying the layers to be selected
#' 
#'
#' @return A ModelObject or DataObject
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

selectLayers <- function(x, layers) {
  
  all.layers <- names(x)
  st.layers <- getSTInfo(x)
  
  # check if layer is present
  for(layer in layers) {
    if(!(layer %in% all.layers)) stop(paste("Can't subset layer", layer, "from object with id =", x@id, "since it is not present", sep = " "))
  }
  
  x.new <- copy(x)
  dt.new <- x.new@data
  dt.new <- dt.new[, append(st.layers, layers), with = FALSE]
  x.new@data <- dt.new
  
  return(x.new)
  
}


############# EXPAND LAYERS
#'
#' Expand plotting/processing layers from convenient short hand codes.
#' 
#' Expands characters strings with particular meaning (eg. "lifefoms", "pfts", "seasons", etc...).  
#' This allows the user to simply specify, for example, "lifeforms" to a plotting command instead of c("Tree","Grass","Shrub") when plotting or processing.
#'
#' @details Supported short-hand strings are:
#' \itemize{
#'   \item{"seasons"}{ expands to c("DJF","MAM","JJA","SON")}
#'   \item{"PFTs"}{ expands to a vector of each of the ids of PFTs present in the data (requires the \code{PFT.set} argument).}
#'   \item{"lifeforms"}{ expands all the different PFT lifeforms present as determined by the lifeform slots of the \code{PFTs} argument (and optionally "Woody").  For example c("Tree", "Grass", "Woody")}
#'   \item{"zones"}{ expands all the different PFT climactic zones present as determined by the zone slots of the \code{PFTs} argument.  For example c("Temperature", "Boreal", "Tropical")}
#'   \item{"leafforms"}{ expands all the different PFT leaf forms present as determined by the leafforms slot of the \code{PFTs} argument.  For example c("Needleleved", "Broadleaved")}
#'   \item{"phenologies"}{ expands all the different PFT leaf forms present as determined by the phenology slot of the \code{PFTs} argument.  For example c("Evergreen", "Summergreen", "Raingreen", "GrassPhenology)}
#' }
#' 
#' 
#' @param layers Character vector of layers to expand
#' @param input.data The data to which the layers will be applied, may be a \code{ModelObject}, a Raster* object, data.table or data.frame, a Spatial*DataFrame. 
#' @param PFT.set List of a superset of PFTs that might be found in this run (only necessary if *not* passing a \code{ModelObject}).
#' @param type Character string identifying if this is a monthly  (= "monthly") or per PFT (="pft") variable.  Ignored for \code{ModelObjects} which supply this data themselves.
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
  
  # read meta-data form ModelObject if possible
  if(is.ModelObject(input.data)) {
    type <- input.data@quant@type
    if(is.null(PFT.set)) PFT.set <- input.data@run@pft.set
    header <- names(input.data@data)
  }
  else{
    header <- names(input.data)
  }
  
  # get PFTs present in data
  PFTs <- getPFTs(input.data, PFT.set)
  
  # if type is undertermined, try to figure it out
  # if found at least one PFT and that PFT is not "Total" then assume we have a per PFT variable
  if(type == "unknown"){
    if(length(PFTs) > 0) {
      if(PFTs[[1]]@id != "Total") {
        type <- "pft"
      }
    }
    else{
      type <- "monthly"
      for(month in months) {
        if(!(month@id %in% header)){
          type <- "unknown"
        }
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
  
  else if(tolower(type) == "monthly"){
    
    # Expand seasons
    if("seasons" %in% tolower(layers) | "season" %in% tolower(layers) | "seasonal" %in% tolower(layers)){
      
      layers <- append(layers, c("DJF", "MAM", "JJA", "SON"))
      
      if("seasons" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "seasons")]
      if("season" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "season")] 
      if("seasonal" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "seasonal")] 
      
    }
    
    # Expand months/monthly
    if("months" %in% tolower(layers) | "monthly" %in% tolower(layers)){
      
      for(month in months) {
        layers <- append(layers, month@id)
      }
      
      if("months" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "months")]
      if("monthly" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "monthly")] 
      
    }
    
  }
  
  if("NA" %in% layers) layers <- layers[-which(layers == "NA")]
  
  
  return(layers)
  
}