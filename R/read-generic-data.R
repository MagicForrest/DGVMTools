


readData <- function(dataset.id,
                     location,
                     resolution = "HD",
                     start.year = NULL,
                     end.year = NULL,
                     temporally.average = TRUE,
                     verbose = TRUE) {

  if(dataset.id == "GFED4") {
   stop("GFED4 not implemented yet")
  }
  else {
      file.string <- file.path(location, dataset.id, paste(dataset.id, resolution, "nc", sep = "."))
  }

  message(paste0("Opening file ", file.string))                    
                         
  this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
  this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
  
  print(this.nc)
  print("Doit")          
  this.slice <- ncvar_get(this.nc, start = c(1,1), count = c(-1,-1))
  dimnames(this.slice) <- list(this.lon, this.lat)
  
  
  # melt to a data.table, via data.frame
  this.slice.dt <- as.data.table(melt(this.slice))
  
  # setnames
  setnames(this.slice.dt, c("Lon", "Lat", "DATALAYER"))
  
  print(this.slice.dt)
           

}