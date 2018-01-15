#####################################################################################################################
################ CORRECTS AN ARTEFACT FROM MAPS PACKAGE WHERE EASTERN ASIA IS WRONGLY PLACED ########################
#####################################################################################################################

#' 
#' Fixes a spatial lines object where some of eastern Russia transposed to the other side of the world
#' 
#' 
#' @param spl SpatialLines object to fix
#' @return a the SpatialLines object 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @keywords internal
#' @import raster
correct.map.offset <- function(spl) {
  
  we <- raster::crop(spl, raster::extent(-180, 180, -90, 90))
  ww <- raster::crop(spl, raster::extent(179.999, 200, -90, 90))
  
  if(!is.null(ww) & !is.null(we)) {
    
    ww <- raster::shift(ww, -360)
    spl <- raster::bind(we, ww)  
    
  }
  return(spl)
}


#####################################################################################################################
################### HELPER FUNCTIONS FOR MAKING  SPATAIL/TEMPORAL PLOT TITLES #######################################
#####################################################################################################################

#' Make a plot title
#' 
#' Build an appropriate plot title from some possibly relevant variables.  
#' It will use a string to represent the quantity (obligatory), and optionally a period and an ID.
#' 
#' @param quantity.str Character string for the quantity plotted
#' @param layer The names of the layer (or another identifier)
#' @param source The Field, DataObject or ComparisonLayer that is being plotted
#' @param period The time period plotted as TemporalExtent (optional)
#' @param extent The spatial extent plotted as SpatialExtent (optional)
#' @return A character string for use as a plot title
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(quantity.str, layer = NULL, source = NULL, period = NULL, extent = NULL){
  
  # quantity.str must be supplied
  string <- quantity.str
  
  # A layer name may be supplied
  if(!is.null(layer)) string <- paste(string, layer, sep = " ")
  
  # A source may be supplied (either a Field or ComparisonLayer)
  if(!is.null(source)) {
    if(is.Field(source)) string <- paste(string, source@source@name, sep = " ")
    if(is.ComparisonLayer(source)) string <- paste(string, source@name, sep = " ")
  }
  
  # And a period and/or spatial extent may be supplied  
  if(!is.null(period) && !is.null(extent)) string <- paste(string, paste("(", extent@name, " ", period@start, "-", period@end, ")", sep = ""), sep = " ")
  else if(!is.null(period) ) string <- paste(string, paste("(", period@start, "-", period@end, ")", sep = ""), sep = " ")
  else if(!is.null(extent) ) string <- paste(string, paste("(", extent@name, ")", sep = ""), sep = " ")
  
  return(string)
  
}
