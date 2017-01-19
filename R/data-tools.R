#' Aggregate categorical data to a larger resolution by counting the number of small gridcells in a larger gridcell.
#' 
#' Given a categorical raster (with \emph{n} factors) and a lower resolution template, return a RasterStack with the \emph{n} layers, 
#' where the value in each cell of the \emph{n} layers in the number of original data gridcells with that value folling in that high resolution
#' gridcell.
#' 
#' @param original.data A raster containing the original categorical data (integer values)
#' @param output.raster A raster used to specify the resolution and extent of the returned data.
#' @param categories A numeric vector of the integer codes defining the categories in the original data.  
#' Can be calculated automatically if not supplied, but can be very slow so it is recommended to supply this meta data 
#' for large input rasters.
#' 
#' Uncertainty in this method will be zero if the gridlines in the larger output raster lie exactly on the gridlines of the smaller input raster.
#' Otherwise it will be low if resolution oforiginal.data is very much lower than the output.raster, and will increase 
#' This is because the gridcells in the original raster which stradle two output grid cells are placed into a gridcell in the 
#' closer output raster cell, not divided with weighted contributions to each
#' 
#' @return A RasterStack recording (one layer for each category in the original data) how many of the original data gridcells with a particular
#' fell into each output raster gridcell,  
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export   
#'  

countCategoricalData <- function(original.data, output.raster, categories = NULL){
  
  
  if(is.null(categories)) categories <- unique(original.data)
  
  # the dataframe
  output.df <- data.frame()
  
  # get the resolution 
  lon.res.div.2 <- xres(output.raster)/2
  lat.res.div.2 <- yres(output.raster)/2
  
  t1 <- Sys.time()
  for(cell in 1:ncell(output.raster)) {
    #for(cell in 1:1000) {
    
    # look up lon and lat
    coords <- xyFromCell(output.raster,cell)
    lon <- coords[1]
    lat <- coords[2]
    
    # get the frequency table (as a data.table) for this extent
    this.extent <- extent(lon-lon.res.div.2,lon+lon.res.div.2, lat-lat.res.div.2,lat+lat.res.div.2)
    
    # check for non-NULL extent which will cause the code to fail.
    if(!is.null(intersect(this.extent, original.data))) {
      
      
      # make a frequency table for the data in the extent.  
      # This is the rate limiting step (by about one of order of magnitude), so if anyone can optimise this it would be great.
      #freq1 <- Sys.time()
      this.freq.df <- data.frame(table(extract(original.data, this.extent)))
      #freq2 <- Sys.time()
      #print("Extract")
      #print(freq2-freq1)
      
      # convert the frequency table into a vector and slot it into the overall raster
      temp.vector <- rep(0, 23)
      for(entry in 1:nrow(this.freq.df)){
        temp.vector[which(categories == this.freq.df$Var1[entry])] <- this.freq.df$Freq[entry]
      }
      output.df  <- rbind(output.df, c(lon, lat, temp.vector))
      
      # remove unused stuff to save memory
      rm(coords, lat, lon, this.freq.df, temp.vector)
      
    }
    
    rm(this.extent)
    
    # show progress and garbage collect every so often
    if(cell %% 10 == 0) {
      print(paste("Progress: ", signif(cell/ncell(output.raster),3) * 100, "%, time elapsed:", sep = ""))
      print(Sys.time()-t1)
      gc()
    }
    
  }
  
  # fix names and return as a raster
  names(output.df) <- c("Lon", "Lat",  paste("Class", categories, sep = "."))           
  final.output <- promoteToRaster(data.table(output.df), paste("Class", categories, sep = "."))
  
  return(final.output)
  
}

############################## MAKE THE 'id' STRING FOR A DATASET 
#
#' Make an ID string for a \code{ModelObject}
#' 
#' Given a string for the quantity and temporal and spatial extents and averaging flags, build an appropriate (and unique) ID string
#' for use in the \code{id} slot of a \code{ModelObject} and for filenames etc.
#' 
#' @param data.string Character string to describe the dataset
#' @param units Character string to describe the units in which the data are measured
#' @param temporal.resolution Character string to describe the temporal resolution of this data, eg. "Annual" or "Daily"
#' @param spatial.resolution Character string to describe the spatial resolution of this data eg "HD" (for half degree) or "QD" (for quarter degree)
#' @param temporal.extent The temporal extent of this object if it has been cropped from the orginal duration, otherwise NULL
#' @param spatial.extent The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param temporally.averaged Logical, should be TRUE if the data represent a snapshot or data averaged over a time period
#' @param spatially.averaged Logical, should be TRUE if the data have been averaged over spatial domain 
#' @return A character string 
#' @export
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeDatasetID <- function(data.string, 
                          units = NULL, 
                          temporal.resolution = NULL, 
                          spatial.resolution = NULL, 
                          temporal.extent = NULL, 
                          spatial.extent = NULL, 
                          temporally.averaged = FALSE, 
                          spatially.averaged = FALSE){
  
  
  dataset.id <- data.string
  if(!is.null(units))  dataset.id <- paste(dataset.id, units, sep = ".")
  if(!is.null(temporal.resolution))  dataset.id <- paste(dataset.id, temporal.resolution, sep = ".")
  if(!is.null(spatial.resolution))  dataset.id <- paste(dataset.id, spatial.resolution, sep = ".")
  if(spatially.averaged)  dataset.id <- paste(dataset.id, "SA", sep = ".")
  if(!is.null(spatial.extent)) dataset.id <- paste(dataset.id, spatial.extent@id, sep = ".")
  if(temporally.averaged)  datasett.id <- paste(dataset.id, "TA", sep = ".")
  if(!is.null(temporal.extent)) dataset.id <- paste(dataset.id, paste(temporal.extent@start, temporal.extent@end, sep = "-"), sep =".")
  
  return(dataset.id)
  
}

