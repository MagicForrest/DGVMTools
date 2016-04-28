

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
    
    # make a frequency table for the data in the extent.  This is the rate limiting step.
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
    rm(coords, lat, lon, this.extent, this.freq.df, temp.vector)
    
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