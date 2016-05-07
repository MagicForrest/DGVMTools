
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
                                 target.grid = raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90)
                                 ){
  
  # hard coded categories
  categories <- c(11,14,20,30,40,50,60,70,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230)
  
  # get the input data
  input.raster <- readGlobcover2009(location)@data
  
  GlobCover2009.aggregated <- countCategoricalData(input.raster, target.grid, categories)
 
}


#' Read the original Globcover2009 data
#' 
#' This function simply returns the original data, but packaged into a SpatialDataset object with metadata.  This data is at 300m and is categorical so processing it to more manageable resolutions is non-trivial and time consuming.  
#' Therefore, this function does not do this it just returns the original data (still stored on disk, not in memory).  If you want to process by 
#' by aggregeting to fractions of a larger, more workable resolution, please see \code{processGlobcover2009}
#' 
#' @param location Character string specifying the directory in which the original data is stored
#'  
#' @return SpatialDataset object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export
#'
#' @seealso code{processGlobcover2009}, \code{countCategoricalData} 

readGlobcover2009 <- function(location){
  
  # open the oririginal data, note that it should remain on disk
  file.name <- "GLOBCOVER_L4_200901_200912_V2.3.tif"
  full.original.data <- raster(file.path(location, file.name))
  
  Globcover2009 <- new("SpatialDataset",
                       id = "Globcover2009",
                       name = "Globcover2009 Spatial Dataset",
                       temporal.extent = new("TemporalExtent", id = "Globcover2009", name = "Globcover2009", start = 2009, end = 2009),
                       data = full.original.data,
                       veg.quant = NULL, # define a VegQuant quant here?
                       units = "categorical",
                       correction.raster = NULL
                       )
  
  return(Globcover2009)
  
}


