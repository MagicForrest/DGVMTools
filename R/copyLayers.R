#' Copy layers from one VegObject or Field to another
#' 
#' This function allows layers (reminder, they are implemented as columns in a data.table) to be copied from one Field/DataObject to another.  
#' This is particularly useful for colouring or facetting a plot of one variable by another one say.  To give a more concrete example, one could use a biome classification 
#' to split (facet) a data-vs-model scatter plot.
#' 
#' @param from The Field/DataObject that the layers are to be copied from.
#' @param to The Field/DataObject that the layers are to be copied to.
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
#' @return A Field (or data.table) comprising the 'to' object with the new layers added
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
    
    Temp.dt <- merge(x = to.dt, y = layers.to.add.dt,  all.y = keep.all.from, all.x = keep.all.to)
    
    
  }
  else {
    
    Temp.dt <- merge(x = to@data, y = layers.to.add.dt, all.y = keep.all.from, all.x = keep.all.to)
    
  }
  to@data <- Temp.dt
  return(to)
  
}