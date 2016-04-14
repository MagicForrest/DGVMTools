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

###### MF 2016-03-03
# Much code commented out here because I updated the old way of making map overlays.
# I left the code here because those files might be useful some day 


##########################################################################################
### SYSTEM SPECIFIC PATHS TO DATAFILES AND 

# Turkey.extent <- extent(c(26.05144,44.81644,35.82306,42.09606))
# 
# #Modern countries at high-res
# modern.countries.hires.file <- file.path(shapefile.dir, 'countries', 'countries.shp')
# modern.countries.hires.layer <- 'countries'
# modern.countries.hires.splayout <- NULL
# 
# #Modern countries
# modern.countries.file <- file.path(shapefile.dir, 'countries-lowres')
# modern.countries.layer <- 'world_by_country'
# modern.countries.splayout <- NULL
# 
# 
# #Modern coastlines at low-res
# modern.coastlines.file <- file.path(shapefile.dir, 'ne_110m_coastline.shp')
# modern.coastlines.layer <- 'ne_110m_coastline'
# modern.coastlines.splayout <- NULL
# 
# 
# #Tortonian continents
# tortonian.continents.file <- file.path(shapefile.dir, 'Coastlines_Tortonian_9.43Ma.shp')
# tortonian.continents.layer <- 'Coastlines_Tortonian_9.43Ma'
# tortonian.continents.splayout <- NULL
# 
# #Turkey outline
# turkey.splayout <- NULL
# 
# 
# makeOverlayOld <- function(coastlines, verbose = FALSE) {
#    
#   if(is.null(coastlines) || coastlines == "") {
#     if(verbose) message("Adding No Coastlines")
#     return(NULL)
#   }
#   
#   else if((is.logical(coastlines) && coastlines == TRUE) || tolower(coastlines) == "coastlines"){
#     if(verbose) message("Adding Modern Coastlines")
#     if(is.null(modern.coastlines.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(modern.coastlines.file, modern.coastlines.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       modern.coastlines.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl)
#       gc()
#      }
#     return(modern.coastlines.splayout)
#   }
#   
#   else if(tolower(coastlines) == "countries" || tolower(coastlines) == "lowres"|| tolower(coastlines) == "low-res"){
#     if(verbose) message("Adding Modern Coastlines")
#     if(is.null(modern.countries.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       print(modern.countries.file)
#       print(modern.countries.layer)
#        polygons <- readOGR(dsn = modern.countries.file, layer = modern.countries.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       modern.countries.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl)
#       gc()
#     }
#     return(modern.countries.splayout)
#   }
#   
#   
#   else if(tolower(coastlines) == "hires" || tolower(coastlines) == "hi-res"){
#     if(verbose) message("Adding Modern Countries at Hi-Res")
#     if(is.null(modern.countries.hires.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(modern.countries.hires.file, modern.countries.hires.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       modern.countries.hires.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl) 
#       gc()
#     }
#     return(modern.countries.hires.splayout)
#   }
#  
#   else if(tolower(coastlines) == "tortonian"){
#     if(verbose) message("Adding Tortonian Coastlines")
#     if(is.null(tortonian.continents.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(tortonian.continents.file, tortonian.continents.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       tortonian.continents.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl)
#       gc()
#     }
#     return(tortonian.continents.splayout)
#   }
# 
#   else if(tolower(coastlines) == "turkey"){
#     if(verbose) message("Adding Turkish Coastlines")
#     if(is.null(turkey.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(modern.countries.hires.file, modern.countries.hires.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       turkey.splayout <<-list("sp.lines", crop(SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), Turkey.extent), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl) 
#       gc()
#     }
#     return(turkey.splayout)
#   }
#   
#   
#   return(NULL)
#   
# }



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
#' Returns a SpatialLines object (of, for example, country or continent outlines) for overlaying on an spplot with (for example) plotVegMaps
#' 
#' @param which A string specifiying either: "hires" or "lowres" for a full global overlay  (these corresponds to the 'world' and 'worldHires' maps from the mapdata package); 
#' the name of one of the standard maps in fromt he map packages the packages directly (see packages maps and mapdata for documentation) or a special (eg paleo maps or other particular cases, currently undefined).
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
  return(list("sp.lines", map.sp.lines, lwd = lwd, col = col))
  
  
}



