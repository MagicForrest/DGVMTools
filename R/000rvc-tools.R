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

###
### Matt Forrest 2016-01-26
### Need a new way to handle this stuff with file paths etc




###############################################################################
### SPECIFY SYSTEM DEPENDENT PATHS - CHANGE THESE FOR YOUR SYSTEM
################################################################################


####### SPECIFY LOCATION OF AUXILIARY DATA HERE 
auxiliary.data.dir <- "/home/forrest/AuxiliaryData" # NOTE: must be the full path or OGR gets confused :-(


###############################################################################
### BUILD COMPOUND PATHS TO SHAPEFILES, GRIDLISTS, BENCHMARKING DATA ETC.
###############################################################################


####### DIRECTORIES FOR ALL BENCHMARKING DATA, SHAPEFILES, GRIDLISTS
benchmarking.data.dir <- file.path(auxiliary.data.dir, "BenchmarkingData")
shapefile.dir <- file.path(auxiliary.data.dir, "Shapefiles") # NOTE: must be the full path or OGR gets confused :-(
gridlist.dir <- file.path(auxiliary.data.dir, "Gridlists/")


######
#gfed4.annual <-  file.path(benchmarking.data.dir, "GFED4/GFED4.0_HD_Annual_BA.nc")
#saatchi2011.original.data.path <- ("/senckenberg.de/DATEN_PBE/PB-E/PBE-ALLG/Datasets/Biomass/2011_Saatchi/www-radar.jpl.nasa.gov/projects/carbon/datasets")

is.VegSpatial <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegSpatial" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}

is.VegTemporal <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegTemporal" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}

is.VegRun <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegRun" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}

is.VegQuant <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "VegQuant" && attr(class.def, "package")=="RVCTools")
      return(TRUE)
  return(FALSE)
}


## check if to classes are comparable
setGeneric("is.equal", function(a, b) standardGeneric("is.equal")) 
setMethod("is.equal", signature("VegQuant", "VegQuant"), function(a, b) {
  if (a@type==b@type && a@units==b@units && a@aggregate.method==b@aggregate.method)
    return(TRUE)
  return(FALSE)
})
setMethod("is.equal", signature("TemporalExtent", "TemporalExtent"), function(a, b) {
  if (a@start==b@start && a@end==b@end)
    return(TRUE)
  return(FALSE)
})
setMethod("is.equal", signature("SpatialExtent", "SpatialExtent"), function(a, b) {
  if (all(!is.finite(c(a@extent@xmin, b@extent@xmin, a@extent@xmax, b@extent@xmax, 
                       a@extent@ymin, b@extent@ymin, a@extent@ymax, b@extent@ymax))))
    return(TRUE)
  if (a@extent@xmin==b@extent@xmin && a@extent@xmax==b@extent@xmax && 
      a@extent@ymin==b@extent@ymin && a@extent@ymax==b@extent@ymax)
    return(TRUE)
  return(FALSE)
})


cropRVC <- function(input, extent){
  
  input.class <- class(input)[1]
  extent.class <- class(extent)[1]
  
  if(extent.class == "Extent") {
    this.extent <- extent
  }
  else if(extent.class == "SpatialExtent"){
    this.extent <- extent@extent
  }
  
  if(input.class == "RasterBrick" | input.class == "RasterStack" | input.class == "RasterLayer"){
    return(crop(input, this.extent))    
  }
  else if(input.class == "data.table"){
    return(input[Lat < this.extent@ymax & Lat > this.extent@ymin & Lon < this.extent@xmax & Lon > this.extent@xmin,])
  }
  
}

cropLPJ <- cropRVC




intersectionRVC <- function(object1, object2){
  
  object1.class <- class(object1)[1]
  object2.class <- class(object2)[1]
  
  # CASE 1 - object 1 is raster/layer/stack
  if(object1.class == "RasterLayer" | object1.class == "RasterStack"  | object1.class == "RasterStack"){
    
    # CASE 1a - object 2 is also a raster/layer/stack
    if(object2.class == "RasterLayer" | object2.class == "RasterStack"  | object2.class == "RasterStack"){
      
      intersection.extent <- intersect(object1, object2)
      return(list(crop(object1, intersection.extent), crop(object2, intersection.extent)))
        
    }
    
    
  }
  
  else {
    
    
    stop(paste("IntersectionRVC not defined for combination of classes ", object1.class, " and ", object2.class))
    
    
  }
  
  
  
  
}





