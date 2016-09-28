


######################### CORRECTS AN ARTEFACT FROM MAPS PACKAGE WHERE EASTERN ASIA IS WRONGLY PLACED #####################################################################
#' 
#' Fixes a spatial lines object where some of eastern Russia transposed to the other side of the world
#' 
#' 
#' @param spl SpatialLines object to fix
#' @return a the SpatialLines object 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import raster
correct.map.offset <- function(spl) {
  we <- crop(spl, extent(-180, 180, -90, 90))
  ww <- crop(spl, extent(179.999, 200, -90, 90))
  if(!is.null(ww) & !is.null(we)) {
    ww <- raster::shift(ww, -360)
    spl <- raster::bind(we, ww)  
  }
  return(spl)
}

######################### MAKES A MAP OVERLAY - A SPATAIL LINES OF CONTINENTS OR COUNTY OUTLINES FOR PLOTTING   #####################################################################
#' Returns a SpatialLines object (of, for example, country or continent outlines) for overlaying on an spplot with (for example) plotSpatial
#' 
#' @param which A string specifiying the name of one of the standard maps in from the map packages the packages directly (see packages maps and mapdata for documentation) or a special (eg paleo maps or other particular cases, currently undefined).
#' @param interior.lines A boolean specifying whether on not to plot the interior lines (countires or provences or other administraive regions) if available
#' @param lwd Width to plot the lines
#' @param col Colour to plot the lines
#' @return a SpatialLines object
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}, Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @importFrom maps maps
#' @importFrom maptools map2SpatialLines
#' @import mapdata
#' @importFrom sp CRS
#' @export
makeOverlay <- function(which, interior.lines = TRUE, lwd = 0.5, col = NULL){
  
  proj4str <- "+proj=longlat +datum=WGS84"
  
  # make rivers fatter and blue
  if(which == "rivers" & is.null(col)) { 
    lwd = 1
    col = "blue"
  }
  # otherwise make the lines black if not specificed
  else if(is.null(col)){
    col <- "black"
  }
  # here have special cases for other maps 
  
    
  # if no special case then we have standard map data
  map.sp.lines <- map2SpatialLines(map(which, plot = FALSE, interior = interior.lines), proj4string = CRS(proj4str))
  map.sp.lines <- correct.map.offset(map.sp.lines)
  return(list(list("sp.lines", map.sp.lines, lwd = lwd, col = col)))
  
  
}


 # temp changes to FireMIP branch



