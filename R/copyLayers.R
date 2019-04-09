#' Copy layers from one Field to another
#' 
#' This function allows layers (reminder, they are implemented as columns in a data.table) to be copied from one Field (or data.table)t to another.  
#' This is particularly useful for colouring or facetting a plot of one variable by another one say.  To give a more concrete example, one could use a biome classification 
#' to split (facet) a data-vs-model scatter plot.
#' 
#' @param from The Field/data.table that the layers are to be copied from.
#' @param to The Field/data.table that the layers are to be copied to.
#' @param layer.names The layers to be copied from the "from" argument
#' @param new.layer.names The new names that the layers should have in the 'to' object. Use this to avoid naming conflict whereby, for 
#' example, if a layer "Total" is copied to an object which already has a "Total" layer then they layers will be names "Total.x" and "Total.y".  
#' @param keep.all.to Boolean, if set to FALSE, all points in the 'to' object which don't have corresponding points in the 'from' object are removed.
#' @param keep.all.from Boolean, if set to FALSE, all points in the 'from' object which don't have corresponding points in the 'to' object are removed.
#' @param dec.places Numeric, how many decimal places to rounds the coordinates (Lon anf Lat dimensions) to in order to get a match.  Default is no rounding (value is NULL) and if dine for most regularing spaced grids.  
#' But setting this can be useful to force matching of coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' 
#' @description This function does not check the dimensions columns are identical.  Any points in the 'from' object which are not in the 'to' object are ignored, 
#' and any points in the 'to' object which don't have corresponding points in the 'from' object are assigned NA, unless keep.all.to is set to FALSE .
#'
#' @return A Field (or data.table) comprising the 'to' object with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
copyLayers <- function(from, to, layer.names, new.layer.names = NULL, keep.all.to = TRUE, keep.all.from = TRUE, dec.places = NULL, fill.dims = TRUE) {
  
  Lon = Lat = NULL
  
  # first some pre-amble and checks
  common.dims <- c()
  from.dims <- getDimInfo(from)
  to.dims <- getDimInfo(to)
  for(dim in c("Lon", "Lat", "Year", "Month", "Day", "Season")) {
    
    if(dim %in% from.dims && dim %in% to.dims) { common.dims <- append(common.dims, dim)}         
    else if(!dim %in% from.dims && dim %in% to.dims & !fill.dims) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"to\" argument but not the \"from\" argument (and fill.dims is not set to TRUE)") 
    }
    else if(dim %in% from.dims && !dim %in% to.dims) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"from\" argument but not the \"to\" argument") 
    }
    
  }
  
  # extract the data.tables
  if(is.Field(to)) to.dt <- copy(to@data)
  else to.dt <- copy(to)
  if(is.Field(from)) layers.to.add.dt <- copy(from@data)
  else layers.to.add.dt <- copy(from)
  layers.to.add.dt <- layers.to.add.dt[, append(common.dims, layer.names), with=FALSE]
  
  # if requested, fill missing dimensions in the data to be added - doesn't seem to be necessary
  # if(fill.dims){
  #   for(fill.dim in to.dims[which(!to.dims %in% from.dims)]) {
  #     print(fill.dim)
  #   }
  # }
  
  if(!is.null(new.layer.names)) setnames(layers.to.add.dt, append(common.dims, new.layer.names))
  if(!is.null(dec.places)) {
    
    
    to.dt[, Lon := round(Lon, dec.places)]
    to.dt[, Lat := round(Lat, dec.places)]
    
    setKeyDGVM(to.dt)
    layers.to.add.dt[, Lon := round(Lon, dec.places)]
    layers.to.add.dt[, Lat := round(Lat, dec.places)]
    
    Temp.dt <- merge(x = to.dt, y = layers.to.add.dt,  all.y = keep.all.from, all.x = keep.all.to)
    
    
  }
  else {
    
    Temp.dt <- merge(x = to.dt, y = layers.to.add.dt, all.y = keep.all.from, all.x = keep.all.to)
    
  }
  to@data <- setKeyDGVM(Temp.dt)
  return(to)
  
}