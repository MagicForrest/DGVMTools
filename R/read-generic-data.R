#' Read standardised data from disk
#' 
#' This function reads some of the standard data from disk that has already been saved as a netCDF file
#' @param dataset.id Character string specifying which dataset you want
#' @param location Character string specifying where the dataset is stored (in a subfolder named by the dataset.id)
#' @param resolution Character string specifying the resolution.  Can be "HD" (half degree), "QD" (quarter degree), "1D" (one degree), "2D" (two degree), 
#' "T42" (T42 spectral resoltion) or "T63" (T63 spectral reolution) 
#' @param start.year Numeric for the first year of data, currently ignored
#' @param end.year Numeric for the last year of data, currently ignored
#' @param temporally.average Boolean for whether or not to aferaget the years of data (currently ignored)
#' @param verbose Boolean, if TRUE write out the blurb from the netCDF opening commands
#' 
#' This function is only useful if you have the directory with all the pre-processed datasets on your sysem.  Contact the author for this!
#' Because of this, it should probably be split off into a DGVMData package (or something).
#' 
#' @return DataObject object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#'
#' @seealso code{processGlobcover2009}, \code{countCategoricalData} 


readData <- function(dataset.id,
                     location,
                     resolution = "HD",
                     start.year = NULL,
                     end.year = NULL,
                     temporally.average = TRUE,
                     verbose = FALSE) {
  
  Lon = NULL

  if(dataset.id == "GFED4") {
   stop("GFED4 not implemented yet")
  }
  else {
      file.string <- file.path(location, dataset.id, paste(dataset.id, resolution, "nc", sep = "."))
  }

  message(paste0("Opening file ", file.string))                    
                         
  this.nc <- ncdf4::nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  this.lat <- ncdf4::ncvar_get(this.nc,"lat",verbose=verbose)
  this.lon <- ncdf4::ncvar_get(this.nc,"lon",verbose=verbose)

  # Get the actual data and set the dimension names    
  this.slice <- ncdf4::ncvar_get(this.nc, start = c(1,1), count = c(-1,-1))
  dimnames(this.slice) <- list(this.lon, this.lat)
  
  # melt to a data.table, via data.frame
  this.slice.dt <- as.data.table(melt(this.slice))
  
  # set names
  setnames(this.slice.dt, c("Lon", "Lat", "DATALAYER"))
  
  # remove NAs
  this.slice.dt <- stats::na.omit(this.slice.dt)
  
  # London centre
  this.slice.dt[,Lon := LondonCentre(Lon)]
  
  # return the data.table
  return(this.slice.dt)
           

}