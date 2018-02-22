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
#' @param source The Field or ComparisonLayer that is being plotted
#' @param period The time period plotted as TemporalExtent (optional)
#' @param extent.str The spatial extent plotted as described by a character string (optional)
#' @return A character string for use as a plot title
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(quantity.str, layer = NULL, source = NULL, first.year = NULL, last.year = NULL, extent.str = NULL){
  
  
  print(extent.str)
  
  # quantity.str must be supplied
  string <- quantity.str
  
  # A layer name may be supplied
  if(!is.null(layer)) string <- paste(string, layer, sep = " ")
  print(string)
  # A source may be supplied (either a Field or ComparisonLayer)
  if(!is.null(source)) {
    if(is.Field(source)) string <- paste(string, source@source@name, sep = " ")
    if(is.ComparisonLayer(source)) string <- paste(string, source@name, sep = " ")
  }
  print(string)
  # A temporal extent may be supplied
  in.brackets <- NULL
  if(!is.null(extent.str)) {
    in.brackets <- extent.str
  }
  print(in.brackets)
  # a first and last year may be supplied
  if(!is.null(first.year) & !is.null(last.year)){
    if(is.null(in.brackets)) in.brackets <- paste(first.year, last.year, sep = "-")
    else in.brackets <- paste0(in.brackets, ", ", paste(first.year, last.year, sep = "-"))
  }
  print(in.brackets)
  # add year and spatial extent if present
  print(in.brackets)
  if(!is.null(in.brackets)){
    string <- paste0(string, " (", in.brackets, ")")
  }
  
 
  return(string)
  
}
