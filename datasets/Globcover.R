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
######################################

require("raster")
require("rgdal")

readNaturalLandCoverGC2005 <- function(resolution = "HD"){
  
  if(resolution == "HD" | resolution == "original"){
    return(raster(file.path(benchmarking.data.dir, "Globcover2005/naturalFraction.Globcover2005.nc")))
  }
  else if(resolution == "T63"){
    return(rotate(raster(file.path(benchmarking.data.dir, "Globcover2005/naturalFraction.Globcover2005.T63.nc"))))
  }
  
}

readNaturalLandCoverGC2009 <- function(resolution = "HD"){
  
  if(resolution == "HD" | resolution == "original"){
    return(raster(file.path(benchmarking.data.dir, "Globcover2009/latest/naturalLandcoverFraction.Globcover2009.nc")))
  }
  else if(resolution == "T63"){
    return(rotate(raster(file.path(benchmarking.data.dir, "Globcover2009/latest/naturalLandcoverFraction.Globcover2009.T63.nc"))))
  }
  
}


readFireUnsuitableLandCoverGC2005 <- function(){
  
  file.location <- "/data/forrest/EO-data/Globcover2005/fireUnsuitableFraction.Globcover2005.nc"
  
  return(raster(file.location))
  
}

