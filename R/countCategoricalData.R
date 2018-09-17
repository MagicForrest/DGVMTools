#' Aggregate categorical data to a larger resolution by counting the number of small gridcells in a larger gridcell.
#' 
#' Given a categorical raster (with \emph{n} factors) and a lower resolution template, return a RasterStack with the \emph{n} layers, 
#' where the value in each cell of the \emph{n} layers in the number of original data gridcells with that value folling in that high resolution
#' gridcell.
#' 
#' @param original.data A raster containing the original categorical data (integer values)
#' @param overlay.objects A list of "overlay objects" for which the contributing pixels should be coounted.  An "overlay object"  is itself a list
#' with entries "lon" (longitude of centre of the overlay obect ), "lat" (similar) and "extent" which can be a Raster Extent object, or a SpatialObject or
#' anything else which can be used with raster::extract to extract data.  There is a further special case, one can use a Raster object used to specify the 
#' gridcells/pixles in which the categorical data are counted.
#' @param return.type A character string specifying the form of the output.  Default is "data.table", but "raster" can also be attempted although this will give unpredicatble results 
#' if the "overlay.objects" argument does not define a regular grid.
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
#' @export   
#'  

countCategoricalData <- function(original.data, overlay.objects, return.type = "data.table", categories = NULL){

  if(is.null(categories)) categories <- unique(original.data)
  
  # the dataframe
  output.df <- data.frame()
  
  ### Make a list of the cells of the output grid that we want to produce
  if(class(overlay.objects)[1] == "RasterLayer") {
    
    temp.list <- list()
    
    # get the resolution 
    lon.res.div.2 <- raster::xres(overlay.objects)/2
    lat.res.div.2 <- raster::yres(overlay.objects)/2
    
    
    for(cell in 1:raster::ncell(overlay.objects)) {
      #for(cell in 1:1000) {
      
      # look up lon and lat
      coords <- raster::xyFromCell(overlay.objects,cell)
      lon <- coords[1]
      lat <- coords[2]
      
      # get the frequency table (as a data.table) for this extent
      this.extent <- raster::extent(lon-lon.res.div.2,lon+lon.res.div.2, lat-lat.res.div.2,lat+lat.res.div.2)
      
      # add this extent to the list
      temp.list[[length(temp.list)+1]] <- list("lon" = lon, "lat" = lat, extent = list(this.extent)) 
      
    }
    
    overlay.objects <- temp.list
    
  }
  
  
  
  t1 <- Sys.time()
  
  counter <- 1
  for(overlay.object in overlay.objects) {
    
    counter <- counter + 1
    
    lat <- overlay.object$lat
    lon <- overlay.object$lon
    all.extents <- overlay.object$extent
    
    # vector to store the extracted numbers of cells
    temp.vector <- rep(0, length(categories))
    
    for(this.extent in all.extents) {
     
      # check for non-NULL extent which will cause the code to fail.
      if(!is.null(raster::intersect(this.extent, original.data))) {
        
        
        # make a frequency table for the data in the extent.  
        # This is the rate limiting step (by about one of order of magnitude), so if anyone can optimise this it would be great.
        #freq1 <- Sys.time()
        this.freq.df <- data.frame(table(raster::extract(original.data, this.extent)))
        #freq2 <- Sys.time()
        #print("Extract")
        #print(freq2-freq1)
        
        #print(this.freq.df)
        
        # add the frequency table to the vector of numbers of cells
        #print(paste0("lon = ", lon, " lat = ", lat))
        for(entry in 1:nrow(this.freq.df)){
          temp.vector[which(categories == this.freq.df$Var1[entry])] <- temp.vector[which(categories == this.freq.df$Var1[entry])] + this.freq.df$Freq[entry]
          #print(temp.vector)
        }
        
      }
      
      rm(this.extent)
      
    }
    
    # add to the output data.frame
    output.df  <- rbind(output.df, c(lon, lat, temp.vector))
    
    # remove unused stuff to save memory
    rm(coords, lat, lon, this.freq.df, temp.vector)
  
    # show progress and garbage collect every so often
    if(counter %% 10 == 0) {
      print(paste("Progress: ", signif(counter/length(overlay.objects),3) * 100, "%, time elapsed:", sep = ""))
      print(Sys.time()-t1)
      gc()
    }
    
  } # for each overlay.object loop
  
  # fix names and return as a raster
  names(output.df) <- c("Lon", "Lat",  paste("Class", categories, sep = "."))           
  
  if(tolower(return.type) == "data.table") {
    return(data.table(output.df))  
  }
  else if(tolower(return.type) == "raster") {
    final.output <- promoteToRaster(data.table(output.df), paste("Class", categories, sep = "."))
    return(final.output)
  }
  else {
    warning("Unknown return type, returning a data.table")
    return(data.table(output.df))
  }
  
}


