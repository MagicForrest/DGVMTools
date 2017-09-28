
#' Process the Globcover2009 data 
#' 
#' Read the Globcover2009 landcover dataset at its original resution and process it by counting the number of original resolution pixels to fall into each 
#' gridcell of the target grid for each landcover. Then return the fraction of each landcover type per class in a RasterBrick.
#' 
#' @param location A character string specifying the location of the original Globcover2009 data
#' @param target.grid A RasterLayer definging the target extent and resoutuion of the processed data
#' 
#' @return A RasterBrick with one layer for each landover catorgory
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export

processGlobcover2009 <- function(location = "/data/forrest/LandUseLandCover/Globcover2009/", 
                                 target.grid = raster::raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90)
){
  
  # hard coded categories
  categories <- c(11,14,20,30,40,50,60,70,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230)
  
  # get the input data
  input.raster <- readGlobcover2009(location)@data
  
  GlobCover2009.aggregated <- countCategoricalData(input.raster, target.grid, categories)
  
}


#' Read the original Globcover2009 data
#' 
#' This function simply returns the original data, but packaged into a DataObject object with metadata.  This data is at 300m and is categorical so processing it to more manageable resolutions is non-trivial and time consuming.  
#' Therefore, this function does not do this it just returns the original data (still stored on disk, not in memory).  If you want to process by 
#' by aggregeting to fractions of a larger, more workable resolution, please see \code{processGlobcover2009}
#' 
#' @param location Character string specifying the directory in which the original data is stored
#'  
#' @return DataObject object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
#'
#' @seealso code{processGlobcover2009}, \code{countCategoricalData} 

readGlobcover2009 <- function(location){
  
  # open the oririginal data, note that it should remain on disk
  file.name <- "GLOBCOVER_L4_200901_200912_V2.3.tif"
  full.original.data <- raster::raster(file.path(location, file.name))
  
  Globcover2009 <- new("DataObject",
                       id = "Globcover2009",
                       name = "Globcover2009 Spatial Dataset",
                       temporal.extent = new("TemporalExtent", id = "Globcover2009", name = "Globcover2009", start = 2009, end = 2009),
                       data = full.original.data,
                       veg.quant = lookupQuantity("landCoverClass_std", "Standard"),
                       units = "categorical",
                       correction.raster = raster::raster()
  )
  
  return(Globcover2009)
  
}

#' Read Globcover2009 correction data
#' 
#' This function simply returns the correction factor (at 0.5 degrees) to take into account the precentage of each gridcell which is not natural vegetation.  
#' 
#' @return DataObject object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#'
#' No arguments, this is very quick and dirty
#'
#' @seealso code{processGlobcover2009}, \code{countCategoricalData} 
getGlobcover2009HDCorrection <- function() {
  
  Lon = Lat = NULL
  
  # do class 40 first to set up the raster
  natural.raster <- raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", 40, "HD.nc", sep = ".")))

    # note that is list doesn't include class 40
  natural.landcover.classes <- c(50, 60, 70, 90, 100, 110, 120, 130, 140)
  
  for(lc.class in natural.landcover.classes){
    natural.raster <- natural.raster + raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", lc.class, "HD.nc", sep = ".")))
  }

  # Also add the semi-natural mosaic pixels 
  natural.raster <- natural.raster + raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", 20, "HD.nc", sep = "."))) * 0.35
  natural.raster <- natural.raster + raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", 30, "HD.nc", sep = "."))) * 0.65

  # Make into a data.table
  correction.dt  <- data.table(raster::as.data.frame(natural.raster,xy = TRUE))
  setnames(correction.dt , c("Lon", "Lat", "Correction"))
  setkey(correction.dt , Lon, Lat)
  
  Globcover2009.DObj <- new("DataObject",
                            id = "Globcover2009",
                            name = "Globcover2009-derived LU correction",
                            temporal.extent = new("TemporalExtent", id = "Globcover2009", name = "Globcover2009", start = 2009, end = 2009),
                            data = correction.dt,
                            quant = lookupQuantity("fraction"),
                            spatial.extent = new("SpatialExtent", id = "Global", name = "Global", extent(correction.dt)),
                            correction.layer =  "")
  

  return(Globcover2009.DObj)  
  
}

#' Read Globcover2009 correction data
#' 
#' This function simply returns the correction factor (at 0.5 degrees) to take into account the precentage of each gridcell which is not natural vegetation.  
#' 
#' @param resolution A character string specifying the resolution desired.  Can currently be "HD", "T42" or "T63".
#' @return DataObject object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @seealso code{processGlobcover2009}, \code{countCategoricalData} 
getGlobcover2009Correction <- function(resolution = "HD") {
  
  Lon = Lat = Sum = Correction = Class.20 = Class.30 = NULL
  
  if(resolution == "HD"){
  
 
  
  # do class 40 first to set up the raster
  natural.raster <- raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", 40, "HD.nc", sep = ".")))
  
  # note that is list doesn't include class 40
  natural.landcover.classes <- c(50, 60, 70, 90, 100, 110, 120, 130, 140)
  
  for(lc.class in natural.landcover.classes){
    natural.raster <- natural.raster + raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", lc.class, "HD.nc", sep = ".")))
  }
  
  # Also add the semi-natural mosaic pixels 
  natural.raster <- natural.raster + raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", 20, "HD.nc", sep = "."))) * 0.35
  natural.raster <- natural.raster + raster::raster(file.path("/home/forrest/Data/LandUseLandCover/Globcover2009/processed", paste("Globcover2009.Class", 30, "HD.nc", sep = "."))) * 0.65
  
  # Make into a data.table
  correction.dt  <- data.table(raster::as.data.frame(natural.raster,xy = TRUE))
  setnames(correction.dt , c("Lon", "Lat", "Correction"))
  setkey(correction.dt , Lon, Lat)
  
  }
  
  else {
    
    if(resolution == "T63") {
      
      all.dt <- data.table(utils::read.table("/home/forrest/Data/LandUseLandCover/Globcover2009/PixelsCounts.T63.txt", header = TRUE))
      
    }
    
    else if(resolution == "T42") {
      
      all.dt <- data.table(utils::read.table("/home/forrest/Data/LandUseLandCover/Globcover2009/PixelsCounts.T42.txt", header = TRUE))
      
    }
      
      # add sum of all classes 
      all.classes = c(11,14,20,30,40,50,60,70,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230)
      allClassesToSum <- paste0("Class.", all.classes)
      all.dt[, Sum := rowSums(.SD), .SDcols=allClassesToSum]
      
      # add fully natural classes
      natural.landcover.classes <- c(40, 50, 60, 70, 90, 100, 110, 120, 130, 140)
      colsToSum <- paste0("Class.",  natural.landcover.classes )
      all.dt[, Correction := rowSums(.SD), .SDcols=colsToSum]
      
      # add mosaic classes
      all.dt[, Correction := Correction + (Class.20 * 0.35)]
      all.dt[, Correction := Correction + (Class.30 * 0.65)]
      
      # now make the correction
      all.dt[, Correction := Correction/Sum]
      correction.dt <- stats::na.omit(all.dt[, list(Lon, Lat, Correction)])
  
    
  }
  
  Globcover2009.DObj <- new("DataObject",
                            id = "Globcover2009",
                            name = "Globcover2009-derived LU correction",
                            temporal.extent = new("TemporalExtent", id = "Globcover2009", name = "Globcover2009", start = 2009, end = 2009),
                            data = correction.dt,
                            quant = lookupQuantity("fraction"),
                            spatial.extent = new("SpatialExtent", id = "Global", name = "Global", extent(correction.dt)),
                            correction.layer =  "")
  
  
  return(Globcover2009.DObj)  
  
}





