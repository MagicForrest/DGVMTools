#!/usr/bin/Rscript
#' writeNetCDF methods
#' 
#' Methods for writing DGVMTools::Field and raster::Raster* objects to disk as netCDF files.  Uses the ncdf4 R-package behind the scenes.
#'  
#' @param x A Field or Raster* object to be written as a netCDF file.  The method can also handle a list of arrays (each array in the list represents one layer)
#' but this is more of a technical, internal use of this method.
#' @param filename A character specifying the name of the netCDF file to be written.
#' @param start.date A data (as a POSIX data) for the start of the time dimension.  Can be omitted for a Field (), \emph{must} be included if you want to write 
#' a RasterStack or RasterBrick with a time dimension. 
#' @param monthly Logical specifying if the data has has a monthly time resolution. Is ignored for a Field, but it is necessary to set it to TRUE if you want to make a netCDF file out
#' of a RasterStack or RasterBrick where each layer represents a month.
#' @param verbose Logical, if TRUE print a bunch of progress and debug output
#' @param quantity A DGVMTools::Quantity object.  This is to provide meta-data (units, names) if saving a Raster* object, it is ignored in the case of a Field (but not
#' if you want to use different meta-data for a Field just alter the Quantity object in the Field object before you call writeNetCDF)
#' @param source A DGVMTools::Source object.  This is to provide meta-data about teh source of the data (model run/dataset, climate forcing, cintact person etc)
#' if saving a Raster* object. It is ignored in the case of a Field (but not if you want to use different meta-data for a Field just alter the Source object in the Field object before you call writeNetCDF)
#' @param layer.dim.name A character string specifing the name of the fourth 'layers' dimension (ie not lon, lat or time).  If not specified (or NULL) then no fourth dimension
#' is created and each layer gets its own variable in the netCDF.  If it is specified, the layers are 'collapsed' on to elements on this fourth, layers, dimension. 
#' @param lat.dim.name Character, the latitude dimension name. Defaults to "Lat".  
#' @param lon.dim.name Character, the longitude dimension name. Defaults to "Lon". 
#' @param time.dim.name Character, the time dimension name. Defaults to "Time".
#' @param time.values The time dimension information (in the case of Raster*, is ignored for a Field).  Format is 'days since' the 'start.date' argument.  NOT CURRENTLY IMPLEMENT, CAN MAYBE REPLACE 'monthly'? 
#' @param precision Character, the output precision (alternatively type, confusingly) to use in the netCDF file (see the 'prec' argument of ncdf4::ncvar_def, can be  'short', 'integer', 'float', 'double', 'char', 'byte').  Default is 'float'.
#' @param ... Other arguments, not currently used
#' 
#' 
#' 
#' These methods offers two very convienent things.  Firsly, it allows the exporting of a DGVMTools::Field object as a standard netCDF file.  Secondly, it provides a more
#' convenient way to write Raster* objects as netCDF objects than writeRaster (at least in the eye's of the author).  This because is allows specifying of meta data and a time axis, 
#' allows some flexibility in the formatting, should write CF-standard compliant files and doesn't invert the latitudes. 
#' 
#' 
#' @return Nothing
#' @name writeNetCDF-methods
#' @rdname writeNetCDF-methods
#' @aliases writeNetCDF
#' @exportMethod writeNetCDF
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
                                     lat.dim.name = "Lat",
                                     lon.dim.name = "Lon",
                                     time.dim.name = "Time",
                                     precision = "float",
                                     time.values = NULL, 
                                     ...) standardGeneric("writeNetCDF"))
}

#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="Field", filename = "character"), function(x, filename, ...) {
  
  monthly <- FALSE
  st.names <- getDimInfo(x)
  if(!"Lon" %in% st.names || !"Lat" %in% st.names) stop("Don't have a Lon or Lat dimension in the field for writing netCDF.  Currently writing netCDF assumes a full Lon-Lat grid.  So failing now.  Contact the author if you want this feature implemented.")
  if("Month" %in% st.names) monthly <- TRUE
  array.list <- FieldToArray(x@data) 
  
  layers <- names(x)
  if(is.array(array.list)) { 
    array.list <- list(array.list)
    names(array.list) <- layers
  }
  
  this.quant <- x@quant
  this.source <- x@source
  rm(x)
  gc()
  
  writeNetCDF(array.list, 
              filename, 
              monthly = monthly, 
              verbose = verbose, 
              start.date = start.date,
              quantity = this.quant ,
              source = this.source,
              layer.dim.name = layer.dim.name,
              lat.dim.name = lat.dim.name,
              lon.dim.name = lon.dim.name,
              time.dim.name = time.dim.name,
              precision = precision,
              ...)
  
})


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="Raster", filename = "character"), function(x, filename, ...) {
  
  
  # check arguments for interpreting time vs layers
  multiple.variables <- FALSE
  single.time.slice <- FALSE
  multiple.time.slices <- FALSE
  
  # If only a single layer, for sure we have only a single variable, check to see if we want to include a timestep.
  if(raster::nlayers(x) == 1) {
    message("Got a single layer, so we have a single variable")
    if(!is.null(start.date)) {
      single.time.slice <- TRUE
      message("Also got a start date (\'start.date\' argument) so setting the time info for this valiable to be simply this date")
    }
  }
  # Else got multiple layers, can be multiple variable or multiples timesteps, here determine which.
  else {
    
    # if a start date
    if(!is.null(start.date)) {
     
      if(is.null(time.values)) {
        single.time.slice <- TRUE
        message("Got a start date (\'start.date\' argument) but no \'time.values\' argument so assuming that each raster layer corresponds to a different variable, each covering the same time period.")
      }
      else {
        message("Got a start date (\'start.date\' argument) and \'time.values\' argument so assuming that the raster layers corresponds to different time slices of a single variable")
        multiple.time.slices <- TRUE
        
       } 
      
      
    }
    
    
  }
  
  
  
  
  # put the Raster* object into a list of arrays
  layers <- names(x)
  array.list <- list()
  for(layer in layers) {
    
    # make the array
    this.layer <- raster::subset(x, layer)
    this.array <- raster::as.matrix(this.layer)
    this.array <- t(this.array)
    this.array <- this.array[, dim(this.array)[2]:1]
    ## MF consider instead reversing the order of lat.list below?
    
    # make the list of lons and lats
    xres <- raster::xres(this.layer)
    lon.list <- seq(from =  raster::xmin(this.layer) + xres/2 , to = raster::xmax(this.layer) - xres/2,  by = xres)
    yres <- raster::yres(this.layer)
    lat.list <- seq(from =  raster::ymin(this.layer) + yres/2, to = raster::ymax(this.layer) - yres/2,  by = yres)
    
    
    colnames(this.array) <- lat.list
    rownames(this.array) <- lon.list
    
    if(single.time.slice) {
      # if we have single time slice add a new dimension for that single timeslice
      dim(this.array) <- append(dim(this.array),1)
      dimnames(this.array) <- list(lon = lon.list, lat = lat.list, time = as.numeric(format(start.date,"%Y")))
    }
    
    array.list[[layer]] <- this.array
    
    rm(this.layer)
    rm()
  }
  
  
  writeNetCDF(array.list, 
              filename, 
              monthly = monthly, 
              verbose = verbose, 
              start.date = start.date,
              quantity = quantity,
              source = source,
              layer.dim.name = layer.dim.name,
              lat.dim.name = lat.dim.name,
              lon.dim.name = lon.dim.name,
              time.dim.name = time.dim.name,
              precision = precision,
              ...)
  
})


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="list", filename = "character"), function(x, filename, ...) {
  
  ### GET METADATA FROM QUANTITY IF PRESENT
  quantity.units <- "Not_defined"
  quantity.id <- "Not_defined"
  standard.name <- "Not_defined"
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
  all.dims[["Lon"]] <- ncdim_def(name = lon.dim.name, units = "degrees", vals = as.numeric(all.dimnames[[1]]), unlim=FALSE, create_dimvar=TRUE)
  all.dims[["Lat"]] <- ncdim_def(name = lat.dim.name, units = "degrees", vals = as.numeric(all.dimnames[[2]]), unlim=FALSE, create_dimvar=TRUE)
  
  # Layer - only if a layer.dim.name has been specifed which mean collapse all the different layers as values along a dimension
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
      
      # make the time values, taking of the inconvenient fact that months have different numbers of days
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
    
    all.dims[["Time"]] <- ncdim_def(name = time.dim.name, units = time.unit, vals = time.vals , calendar = calendar, unlim=TRUE, create_dimvar=TRUE)
    
  }
  
  
  ### PREPARE THE VARIABLES: ONE FOR EACH LAYER OR COMBINE THEM IF WE WANT LAYERS TO BE DEFINED ALONG A DIMENSION AXIS
  all.vars <- list()
  # individual layers
  if(is.null(layer.dim.name)) {  
    for(layer in layers) {
      all.vars[[layer]] <- ncvar_def(name = layer, units = quantity.units, dim = all.dims, longname = standard.name, prec = precision, -99999)  # standard
    }
  }
  # else turn layers into a dimension
  else {
    
    all.vars <- ncvar_def(name = quantity@id, units = quantity.units, dim = all.dims, longname = standard.name, prec = precision, -99999)  # standard
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
    ncatt_put(outfile, 0, "Source_Format", source@format@id)
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