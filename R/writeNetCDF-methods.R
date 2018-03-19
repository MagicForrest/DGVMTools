#!/usr/bin/Rscript
#' writeNetCDF methods
#' 
#' Methods for cropping DGVMTools objects (and data.tables) to each other, to raster and sp objects (and basically anything that can be coerced in a raster::Extent object). 
#' The data.table in the "data" slot of the DGVMTools object (or the stand-alone data.table), must have columns named "Lon" and "Lat" otherwise these methods fail.  Unpleasantly.  
#' 
#' @param x A Field or ComparisonLayer or a stand-alone data.table to be cropped
#' @param y Anything from which raster::Extent object can be derived
#' @param ... Other arguments, not currently used
#' @return A spatially cropped object
#' @name writeNetCDF-methods
#' @rdname writeNetCDF-methods
#' @aliases writeNetCDF
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
#' 
#' 
#' 
# first define (redfine) the generic
if (!isGeneric("writeNetCDF")) {
  setGeneric("writeNetCDF", function(x, 
                                     filename, 
                                     start.date = NULL, 
                                     monthly = FALSE, 
                                     verbose = FALSE, 
                                     quantity = NULL, 
                                     source = NULL, 
                                     layer.dim.name = NULL,
                                     ...) standardGeneric("writeNetCDF"))
}

#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="Field", filename = "character"), function(x, filename, ...) {
  
  st.names <- getSTInfo(x)
  if(!"Lon" %in% st.names || !"Lat" %in% st.names) stop("Don't have a Lon or Lat dimension in the field for writing netCDF.  Currently writing netCDF assumes a full Lon-Lat grid.  So failing now.  Contact the author if you want this feature implemented.")
  if("Month" %in% st.names) monthly <- TRUE
  array.list <- FieldToArray(x@data) 

  layers <- names(x)
  if(is.array(array.list)) { 
    array.list <- list(array.list)
    names(array.list) <- layers
  }
  
  writeNetCDF(array.list, 
              filename, 
              monthly = monthly, 
              verbose = verbose, 
              start.date = start.date,
              quantity = x@quant,
              source = x@source,
              layer.dim.name = layer.dim.name,
              ...)
  
})


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="list", filename = "character"), function(x, filename, ...) {

  ### GET METADATA FROM QUANTITY IF PRESENT
  if(!is.null(quantity)) {
    quantity.units <- quantity@units
    quantity.id <- quantity@id
    standard.name <- quantity@cf.name
  }
  
  ### CHECK INPUT LIST AND GET LAYER NAMES
  for(list.element in x) {
    if(!is.numeric(list.element)) stop("Got a non-numeric item in the list of layers.  Failing!")
    dims <- dim(list.element)
    if(exists("last.dims")) {
      if(!identical(dims,last.dims)) stop("Arrays not the same size for each layer, can't make a netCDF like this!")
    }
    last.dims <- dims
  }
  
  layers <- names(x)
  if(length(layers) != length(x)) stop("Layers not correctly named.  The layer names are taken from the name of each element in the input list, so the elements must be named.")
  
  
  ### MAKE DIMENSIONS
  all.dimnames <- dimnames(x[[1]])
  all.dims <- list()
  
  # Lon and Lat - easy-peasy
  all.dims[["Lon"]] <- ncdim_def(name = "Lon", units = "degrees", vals = as.numeric(all.dimnames[[1]]), unlim=FALSE, create_dimvar=TRUE)
  all.dims[["Lat"]] <- ncdim_def(name = "Lat", units = "degrees", vals = as.numeric(all.dimnames[[2]]), unlim=FALSE, create_dimvar=TRUE)
  
  # Layer - only if a layer.dim.name has been specifed which mean collapse all the diffents layers as values along a dimension
  if(!is.null(layer.dim.name)) {
    
    if(!is.character(layer.dim.name)) stop("layer.dim.name must be NULL or a character string (For example, \"VegType\" or \"CarbonPool\"")
    all.dims[[layer.dim.name]] <- ncdim_def(name = layer.dim.name, units = "categorical", vals = 1:length(layers), create_dimvar=TRUE)
    
  }
  
  # Time
  if(length(all.dimnames) > 2 ) {
    if(monthly) {
      if(verbose) print("Got monthly data")
      
      # get the year and month as a numeric code with format "Year.Month"
      all.year.months <- as.numeric(all.dimnames[[3]]) / 100
      all.years <- sort(unique((trunc(all.year.months))))
      
      # if no start date is set start at Jan 1st of the first year of data
      if(is.null(start.date)) start.date <- as.Date(paste(all.years[1], "01", "01", sep = "-"))
      
      # make the time values, taking of the inconveient fact that months have different numbers of days
      month.cumulative.additions <- c(0)
      for(month in all.months[1:11]) {
        month.cumulative.additions <- append(month.cumulative.additions, month.cumulative.additions[length(month.cumulative.additions)] + month@days)
      }
      
      time.vals <- c()
      start.year <- as.numeric(format(start.date,"%Y"))
      for(year.month in all.year.months) {
        
        year <- trunc(year.month)
        month <- as.integer((year.month - year) * 100) 
        time.vals <- append(time.vals, (year-start.year) * 365 + month.cumulative.additions[month+1])
        
      }
      
    }
    
    # else is yearly, so make annual axis of entries 365 days apart
    else {
      if(verbose) print("Got annual data")
      
      all.years <- as.numeric(all.dimnames[[3]])
      
      # if no start date is set start at Jan 1st of the first year of data
      if(is.null(start.date)) start.date <- as.Date(paste(all.years[1], "01", "01", sep = "-"))
      
      time.vals <- (all.years - as.numeric(format(start.date,"%Y"))) * 365
      
    }
    
    # make start date and calendar
    calendar <- "365_day" 
    time.unit <- paste("days since", start.date)
    
    all.dims[["Time"]] <- ncdim_def(name = "Time", units = time.unit, vals = time.vals , calendar = calendar, unlim=TRUE, create_dimvar=TRUE)
    
  }
  
  
  ### PREPARE THE VARIABLES: ONE FOR EACH LAYER OR COMBINE THEM IF WE WANT LAYERS TO BE DEFINED ALONG A DIMENSION AXIS
  all.vars <- list()
  # individual layers
  if(is.null(layer.dim.name)) {  
    for(layer in layers) {
      all.vars[[layer]] <- ncvar_def(name = layer, units = quantity.units, dim = all.dims, longname = standard.name, -99999)  # standard
    }
  }
  # else turn layers into a dimension
  else {
  
    all.vars <- ncvar_def(name = quantity@id, units = quantity.units, dim = all.dims, longname = standard.name, -99999)  # standard
    old.layers <- layers # for storing the key from dimension values to layer
 
  } 
  
  
  ### MAKE THE NETCDF FILE
  if(verbose) print(paste("Creating the output file", filename))
  outfile <- nc_create(filename, all.vars, verbose=verbose, force_v4=TRUE)
  
  
  ### PUT EACH VARIABLE INTO THE FILE
  
  for(layer in layers) {
    if(verbose) print(paste("Saving variable", layer, "to file",  filename, sep =" " ), quote=FALSE)
    # simple case of one variable per layer
    if(is.null(layer.dim.name)) {
      ncvar_put(nc = outfile, varid = layer,  vals = x[[layer]], start=NA, count=NA, verbose=verbose)
      ncatt_put(outfile, layer, "standard_name", standard.name)
    }
    # else slightly more complicated case of all layers going into one variable
    else{
      this.layer.index <- which(layer == layers)
      start.indices <- c(1,1,this.layer.index,1)
      count.indices <- c(-1,-1,1,-1)
      ncvar_put(nc = outfile, varid = all.vars,  vals = x[[layer]], start=start.indices, count=count.indices, verbose=verbose)
    }

  }
  
  # add meta-data if layers collapsed to a dimension
  if(!is.null(layer.dim.name)) {
    ncatt_put(outfile, quantity@id, "standard_name", standard.name)
    for(counter in 1:length(old.layers)){
      ncatt_put(outfile, all.vars, paste(layer.dim.name, counter, sep ="_"), old.layers[[counter]])
    }
  }
 
  
  # STANDARD SPATIAL ATTRIBUTES
  outfile <- addStandardSpatialAttributes(outfile)
  
  
  # ADD GENERAL ATTRIBUTES
  ncatt_put(outfile, 0, "Conventions", "CF-1.6")
  if(!is.null(source)) {
    ncatt_put(outfile, 0, "Source_Format", source@format)
    ncatt_put(outfile, 0, "Name", source@name)
    ncatt_put(outfile, 0, "Forcing_data", source@forcing.data)
    if (source@contact != "")
      ncatt_put(outfile, 0, "Contact", source@contact)
    if (source@institute != "" || source@institute != "none")
      ncatt_put(outfile, 0, "Institute", source@institute)
  }
  
  # ADD DGVMDATA ATTRIBUTES
  ncatt_put(outfile, 0, "DGVMData_quant", quantity.id)
  #ncatt_put(outfile, 0, "DGVMData_name", name)
  #ncatt_put(outfile, 0, "DGVMData_id", id)
  #ncatt_put(outfile, 0, "DGVMData_spatial.extent", "Original")
  #ncatt_put(outfile, 0, "DGVMData_first.year", first.year)
  #ncatt_put(outfile, 0, "DGVMData_last.year", last.year)
  #ncatt_put(outfile, 0, "DGVMData_year.aggregation.method", "mean")
  #ncatt_put(outfile, 0, "DGVMData_quantity", quantity.id)
  
  # CLOSE
  nc_close(outfile)
  
  
})