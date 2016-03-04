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

.correct.map.offset <- function(spl) {
  we <- crop(spl, extent(-180, 180, -90, 90))
  ww <- crop(spl, extent(179.999, 200, -90, 90))
  ww <- raster::shift(ww, -360)
  spl <- bind(we, ww)  
  return(spl)
}

# returns 
makeOverlay <- function(which){
  proj4str <- "+proj=longlat +datum=WGS84"
  if(which == "lowres") {
    map.sp.lines <- map2SpatialLines(map('world', plot = FALSE), proj4string = CRS(proj4str))
    map.sp.lines <- .correct.map.offset(map.sp.lines)
    return(list("sp.lines", map.sp.lines, lwd = 0.5))
  }
  if(which == "hires") {
    map.sp.lines <-  map2SpatialLines(map('worldHires', plot = FALSE), proj4string = CRS(proj4str))
    map.sp.lines <- .correct.map.offset(map.sp.lines)
    return(list("sp.lines", map.sp.lines, lwd = 0.5))
  }
  if(which == "lowres-continents") {
    map.sp.lines <-  map2SpatialLines(map('world', plot = FALSE, interior = FALSE), proj4string = CRS(proj4str))
    map.sp.lines <- .correct.map.offset(map.sp.lines)
    return(list("sp.lines", map.sp.lines, lwd = 0.5))
  }
  if(which == "hires-continents") {
    map.sp.lines <-  map2SpatialLines(map('worldHires', plot = FALSE, interior = FALSE), proj4string = CRS(proj4str))
    map.sp.lines <- .correct.map.offset(map.sp.lines)
    return(list("sp.lines", map.sp.lines, lwd = 0.5))
  }
  else{
    warning(paste("Overlay", which, "not recognised", sep = " "))
    return(list())
  }
  
  
  
}



