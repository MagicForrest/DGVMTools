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


getDataObject <- function(dataset.id = NULL,
                          quantity = NULL,
                          layers = NULL,
                          location = NULL,
                          filename = NULL,
                          resolution = NULL,
                          start.year = NULL,
                          spatial.extent.id = NULL,
                          end.year = NULL,
                          temporal.aggregate.method = "none",
                          spatial.aggregate.method = "bilinear",
                          verbose = FALSE) {
  
  Lon = NULL
  
  # sort out quantity
  if(class(quantity) == "Quantity") {
    quantity.str <- quantity@id
  }
  else if(class(quantity) == "character") {
    quantity.str <- quantity
    quantity <- lookupQuantity(quantity.str, "Standard")
  }

  if(!is.null(filename)){
    message(paste("Exact filename specified so looking for file", filename, "on disk, other options will be ignoned", sep = " "))
    file.string <- filename
  }
  else {
    
    if(dataset.id == "GFED4") {
      stop("GFED4 not implemented yet")
    }
    
    temporal.extent <- NULL
    if(!is.null(start.year) &&!is.null(start.year)) temporal.extent <- new("TemporalExtent", id = "Dummy", name = "Dummy", start = start.year, end  = end.year)
    
    full.id <- makeModelObjectID(var.string = quantity.str,
                                 temporal.extent = temporal.extent, 
                                 spatial.extent = new("SpatialExtent", id = spatial.extent.id, name = spatial.extent.id, xmin = 0, ymin = 0, xmax = 1, ymax = 1), 
                                 temporal.aggregate.method = temporal.aggregate.method,
                                 spatial.aggregate.method = spatial.aggregate.method
    )
    
    file.string <- file.path(location, dataset.id, paste(full.id, resolution, "nc", sep = "."))
    
  }
  
  
  message(paste0("Opening file ", file.string))     
  
  
  this.nc <- ncdf4::nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  this.lat <- ncdf4::ncvar_get(this.nc,"lat",verbose=verbose)
  this.lon <- ncdf4::ncvar_get(this.nc,"lon",verbose=verbose)
  
  if(missing(layers)) {
    all.vars <- c()
    for(variable in this.nc$var) {
      all.vars <- append(all.vars,  paste(quantity.str, variable, sep = "_"))
    }
  }
  else {
    all.vars <- paste(quantity.str, layers, sep = "_")
  }
  
  all.dt <- data.table(Lon = numeric(0), Lat = numeric(0))
  setkey(all.dt, Lon, Lat)
  
  for(this.var in all.vars) {
    
    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = c(1,1), count = c(-1,-1))
    dimnames(this.slice) <- list(this.lon, this.lat)
    
    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))
  
    # remove NAs
    this.slice.dt <- stats::na.omit(this.slice.dt)
 
    # look up attributes for meta-data for this layer/variable
    
    # quantity
    quant.str <- ncatt_get(this.nc, this.var, attname="DGVMTools_DataObject_quant", verbose=FALSE)$value

    # set the name to something equivalent in the model
    layer.name <- ncatt_get(this.nc, this.var, attname="DGVMTools_DataObject_layer.name", verbose=FALSE)$value
    setnames(this.slice.dt, c("Lon", "Lat", layer.name))
    
    # London centre
    this.slice.dt[, Lon := LondonCentre(Lon)]
    
    # also set the key 
    setkey(this.slice.dt, Lon, Lat)

    # now join this to all.dt
    all.dt <- all.dt[this.slice.dt]
    
  }
  
  # overall meta-data for this DataObject
  
  # dataset name
  dataset.name <- ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_name", verbose=FALSE)$value

  # dataset id
  dataset.id <- ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_id", verbose=FALSE)$value

  
  # spatial extent
  spatial.extent.id <- ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_spatial.extent.id", verbose=FALSE)$value
  spatial.extent <- new("SpatialExtent", 
                        id = paste0(spatial.extent.id, "Extent"), 
                        name =  paste(spatial.extent.id, "Extent", sep = " "), 
                        extent(this.slice.dt))
  
  # temporal extent
  temporal.extent <- new("TemporalExtent", 
                         id = ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_temporal.extent.id", verbose=FALSE)$value,
                         name = ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_temporal.extent.name", verbose=FALSE)$value,
                         start = ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_temporal.extent.start", verbose=FALSE)$value,
                         end = ncatt_get(this.nc, 0, attname="DGVMTools_DataObject_temporal.extent.end", verbose=FALSE)$value
  )
  
  this.DataObject <- new("DataObject",
                         id = dataset.id ,
                         name = dataset.name,
                         temporal.extent = temporal.extent,
                         data = this.slice.dt,
                         quant = quantity,
                         spatial.extent = spatial.extent,
                         correction.layer =  "")
  
  return(this.DataObject)
  
  
}