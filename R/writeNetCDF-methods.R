#!/usr/bin/Rscript
#' writeNetCDF methods
#' 
#' Methods for writing DGVMTools::Field and raster::Raster* objects to disk as netCDF files.  Uses the ncdf4 R-package behind the scenes.  NOTE: when using the function on a raster::Raster object,
#' you cannot provide both multiple variables and multiple time period, you must do one or the other.   
#' 
#'  
#' @param x A Field or Raster* object to be written as a netCDF file.  The method can also handle a list of arrays (each array in the list represents one layer)
#' but this is more of a technical, internal use of this method.
#' @param filename A character specifying the name of the netCDF file to be written.
#' @param start.date A date (as a POSIX date) for the start of the time dimension.  Can be omitted for a Field (), \emph{must} be included if you want to write 
#' a RasterStack or RasterBrick with a time dimension. 
#' @param verbose Logical, if TRUE print a bunch of progress and debug output
#' @param quantity A DGVMTools::Quantity object.  This is to provide meta-data (units, names) if saving a Raster* object, it is ignored in the case of a Field (but note that
#' if you want to use different meta-data for a Field just alter the Quantity object in the Field object before you call writeNetCDF)
#' @param source A DGVMTools::Source object.  This is to provide meta-data about the source of the data (model run/dataset, climate forcing, contact person etc)
#' if saving a Raster* object. It is ignored in the case of a Field (but note  that if you want to use different meta-data for a Field just alter the Source object in the Field object before you call writeNetCDF)
#' @param layer.names A vector of characters to specify the names of the variables in the netCDF file, ignored for a Field.  
#' Should be length 1 or the number of layers in the input raster. 
#' @param layer.dim.name A character string specifing the name of the fourth 'layers' dimension (ie not lon, lat or time).  If not specified (or NULL) then no fourth dimension
#' is created and each layer gets its own variable in the netCDF.  If it is specified, the layers are 'collapsed' on to elements on this fourth, layers, dimension. 
#' @param lat.dim.name Character, the latitude dimension name. Defaults to "Lat".  
#' @param lon.dim.name Character, the longitude dimension name. Defaults to "Lon". 
#' @param time.dim.name Character, the time dimension name. Defaults to "Time".
#' @param time.values The values along the time dimension (in the case of Raster*, is ignored for a Field) defined as 'days since' the 'start.date' argument.
#' If not supplied, data is assumed not to have a time axis. In such case if the raster has multiple layers they will be interpreted  
#' @param precision Character, the output precision (although that is confusing terminology, 'type' would be more descriptive) to use in the netCDF file.  See the 'prec' argument of ncdf4::ncvar_def, can be  'short', 'integer', 'float', 'double', 'char', 'byte').  Default is 'float'.
#' @param ... Other arguments, not currently used
#' 
#' 
#' 
#' These methods offers two very convienent things.  Firsly, it allows the exporting of a DGVMTools::Field object as a standard netCDF file.  Secondly, it provides a more
#' convenient way to write Raster* objects as netCDF objects than writeRaster (at least in the eye's of the author).  This because is allows specifying of meta data and a time axis, 
#' allows some flexibility in the formatting, should write CF-standard compliant files and doesn't invert the latitudes. 
#' 
#' Note that to maintain some parsimony and flexibility, the methods can write *either* a netCDF file with a time dimension *or* one with multiple variables(layers).  
#'  
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
                                     verbose = FALSE, 
                                     quantity = NULL, 
                                     source = NULL, 
                                     layer.names = NULL,
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
  
  # ignore parameters and give warnings
  if(!is.null(layer.names)) warning("Argument layer.names is ignored when calling writeNetCDF() for a Field. Layer names are taken from the layer names in the Field.")
  st.names <- getDimInfo(x)
  if(!"Lon" %in% st.names || !"Lat" %in% st.names) stop("Don't have a Lon or Lat dimension in the field for writing netCDF.  Currently writing netCDF assumes a full Lon-Lat grid.  So failing now.  Contact the author if you want this feature implemented.")
  
  # make a list of arrays from the Field (one array per Layer in the Field)
  if(verbose) {
    message(paste0("Calling funtion FieldToArray() on a Field with ", nrow(x@data), " spatio-temporal data points and a total of ", length(layers(x)), " layers"))
    t1 <- Sys.time()
  }
  array.list <- FieldToArray(x, verbose = verbose) 
  if(verbose) {
    t2 <- Sys.time()
    message("FieldToArray took:")
    print(t2-t1)
  }
  
  # determine start date from the Field if it contains a Year dimension
  if("Year" %in% st.names | "Month" %in% st.names | "Day" %in% st.names) {
    
    # easy to get first year
    if("Year" %in% st.names) first.year <- sort(unique(getDimInfo(x, "values")[["Year"]]))[1]
    else first.year <- 01
  
    # now check for either a Month or Day dimension
    if("Month" %in% st.names) {
      # for first month, we first need to subset the first year
      if("Year" %in% st.names) {
        first.year.Field <- selectYears(x, first.year, first.year)
        first.month <- sort(unique(getDimInfo(first.year.Field, "values")[["Month"]]))[1]
      }
      else {
        first.month <- sort(unique(getDimInfo(x, "values")[["Month"]]))[1]
      }
      start.date <- as.POSIXct(as.Date(paste(first.year, first.month, "01", sep = "-"), format='%Y-%m-%d'))
    }
    else if("Day" %in% st.names) {
      # to find the first day, we first need to subset the first year
      if("Year" %in% st.names) {
        first.year.Field <- selectYears(x, first.year, first.year)
        first.day <- sort(unique(getDimInfo(first.year.Field, "values")[["Day"]]))[1]
      }
      else {
        first.month <- sort(unique(getDimInfo(x, "values")[["Day"]]))[1]
      }
      start.date <- as.POSIXct(as.Date(first.day-1, origin = paste0(first.year, "-01-01")))
      
    }
    else {
      start.date <- as.POSIXct(as.Date(paste(first.year, "1", "1", sep = "-"), format='%Y-%m-%d'))
    }
    
  }
  
  # grab other metadata from the ci
  this.quant <- x@quant
  this.source <- x@source
  rm(x); gc()
  
  writeNetCDF(array.list, 
              filename, 
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
  
  if(is.null(quantity) || missing(quantity)) stop("When calling a writeNetCDF() on a raster you *need* to pass in a quantity object supply metadata. \n Fortunately the is easy to define if you don't have one, see Quantity-class")
  if(is.null(layer.names) || missing(layer.names)) stop("When calling a writeNetCDF() on a raster you *need* to provide a 'layer.names' argument to determine how to name the layers.")
  
  # check input arguments and the number of layers in the input raster to determine the structure of the netCDF file that we should be building
  
  # initial important check, can't have mutiple layers *and* multiple time steps
  if(length(time.values) > 1 && length(layer.names) > 1) {
    stop("When calling writeNetCDF for Rasters, you cannot write multiple layers and multiple time steps at the same time.")
  }
  
  # If only a single layer or  if multiple layers and the length of layer.names matches then assume a single time slice with one variabe per layer
  single.time.slice <- FALSE
  #  If only a single layer it must be a single time slice for a single variable
  if(raster::nlayers(x) == 1) {
    if(verbose) message("writeNetCDF called on a raster with a single layer, so writing a netCDF file with a single time step for a single variable")
    single.time.slice <- TRUE
  }
  # if time values supplied 
  else if(length(time.values) > 0 ){
    
    # firstly, we must have a start date
    if(is.null(start.date)) {
      stop("When calling writeNetCDF for multi-layered Rasters, you must supply the start.date argument")
    }
    
    # secondly, the number of layers in the raster must be the same as the number of time.values
    if(length(time.values) != raster::nlayers(x)) {
      stop("When calling writeNetCDF for multi-layered Rasters with time.values, the length of time.values must be the same as the number of layers in the Raster")
    }
    
    # okay then we are probably fine
    if(verbose) message("writeNetCDF called on a raster with a multiple layers and corrsponding time strps, so writing a netCDF file with a one variable and one time slice per input raster layer") 
    
  }
  # final case is multiple layers (variables) for a single time-step
  else if(raster::nlayers(x) == length(layer.names)) {
    
    if(verbose) message("writeNetCDF called on a raster with multiple layers , so writing a netCDF file with a single time step for a multiple variables")
    single.time.slice <- TRUE
    
  }
  # else if the number of layers and layers name don;t match then fail
  else if(raster::nlayers(x) != length(layer.names)) {
    
    stop("When calling writeNetCDF for multi-layered Rasters where each layer corresponds to one variable, the length of the layer.names argument must be the same as the number of layers in the raster.")    
    
  }
  
  
  
  # make a list of lons and lats
  xres <- raster::xres(x)
  lon.list <- seq(from =  raster::xmin(x) + xres/2 , to = raster::xmax(x) - xres/2,  by = xres)
  yres <- raster::yres(x)
  lat.list <- seq(from =  raster::ymin(x) + yres/2, to = raster::ymax(x) - yres/2,  by = yres)
  
  # if a single time slice (but potentially multiple layers) put the Raster* object into a list of arrays
  # with one array in the list per layer
  array.list <- list()
  if(single.time.slice) {
    
    if(verbose) message("Got a single layer, so we have a single variable")
    
    layers <- names(x)
    for(layer.index in 1:length(layers)) {
      
      # make the array
      this.layer <- raster::subset(x, layer.index)
      this.array <- raster::as.matrix(this.layer)
      this.array <- t(this.array)
      this.array <- this.array[, dim(this.array)[2]:1]
      
      # add a new dimensinsion for the time info
      if(!is.null(start.date)) {
        dim(this.array) <- append(dim(this.array),1)
        start.year <- as.numeric(format(start.date,"%Y"))
        start.day <- as.numeric(format(start.date,"%d")) 
        dimnames(this.array) <- list(lon = lon.list, lat = lat.list, time = (start.year * 1000) + start.day)
      }
      else{
        dimnames(this.array) <- list(lon = lon.list, lat = lat.list)
      }
      
      # append the array to the list
      array.list[[layer.names[layer.index]]] <- this.array
      rm(this.layer)
      
    }
  }
  
  # else each layer corresponds to a time step
  else  {
    
    if(verbose) message("Got a start date (\'start.date\' argument) and \'monthly\' argument is TRUE so assuming that each raster layer corresponds to consecutive months, starting from the month of \'start.date\'")
    
    
    # what the whole raster into an array (taking care of the stupid inverted lats)
    this.array <- aperm(raster::as.array(x), c(2,1,3))
    this.array <- this.array[, dim(this.array)[2]:1, ]
    
    # make the time dimension values
    # maybe make this more vectorised and elegant
    time.list <- c()
    start.year <- as.numeric(format(start.date,"%Y"))
    start.day <- as.numeric(format(start.date,"%d")) 
    
    for(counter in 1:(raster::nlayers(x))) {
      
      # calculate how may days since the start of the first year
      ndays.since.start <- time.values[counter] + start.day
      
      # calculate the number of complete years and the year of this layer
      # *** Assuming 365 day calendar here
      year.length <- 365
      nyears <- ndays.since.start %/% year.length 
      final.year <- nyears + start.year
      
      # Calculate the number of days into that year
      # the while leap calendat thing will be even more complicated here
      final.day <- ndays.since.start %% year.length
      
      
      final.date <- (final.year * 1000) + final.day
      time.list <- append(time.list, as.character(final.date))
      
    }
    
    dimnames(this.array) <- list(lon = lon.list, lat = lat.list, time = time.list)
    array.list[[layer.names[1]]] <- this.array
    rm(this.array)
    gc()
    
  }
  
  
  writeNetCDF(array.list, 
              filename, 
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
  
  missing.fill.value <- -99999
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to write netCDF files.")
  
  ### GET METADATA FROM QUANTITY IF PRESENT
  quantity.units <- "Not_defined"
  quantity.id <- "Not_defined"
  standard.name <- "Not_defined"
  long.name <- "Not_defined"
  if(!is.null(quantity)) {
    quantity.units <- quantity@units
    quantity.id <- quantity@id
    standard.name <- quantity@cf.name
    long.name <- quantity@name
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
  all.dims[["Lon"]] <- ncdf4::ncdim_def(name = lon.dim.name, units = "degrees", vals = as.numeric(all.dimnames[[1]]), unlim=FALSE, create_dimvar=TRUE)
  all.dims[["Lat"]] <- ncdf4::ncdim_def(name = lat.dim.name, units = "degrees", vals = as.numeric(all.dimnames[[2]]), unlim=FALSE, create_dimvar=TRUE)

  # Layer - only if a layer.dim.name has been specified which mean collapse all the different layers as values along a dimension
  if(!is.null(layer.dim.name)) {
    
    if(!is.character(layer.dim.name)) stop("layer.dim.name must be NULL or a character string (For example, \"VegType\" or \"CarbonPool\"")
    all.dims[[layer.dim.name]] <- ncdf4::ncdim_def(name = layer.dim.name, units = "categorical", vals = 1:length(layers), create_dimvar=TRUE)
    
  }
  
  # Time - only if start.date has been provided (and it not NULL)
  if(!is.null(start.date)) {
    
    # make start date and calendar
    calendar <- "365_day" 
    time.unit <- paste("days since", start.date, "12:00:00")
    
    # decode the time dimension names (which should be Year * 1000 + Day) into 'days since start date' 
    years <- as.integer(round(trunc(as.numeric(all.dimnames[[3]])/1000)))
    days <- as.integer(round(as.numeric(all.dimnames[[3]]) - years * 1000)) 
    intervals <- (years - (as.numeric(format(start.date,"%Y")))) * 365 + days - 1
    all.dims[["Time"]] <- ncdf4::ncdim_def(name = time.dim.name, units = time.unit, vals = intervals , calendar = calendar, unlim=TRUE, create_dimvar=TRUE)
    
  }
  
  # # Time - if more than two dimensions one must be time, so hand that
  # if(length(all.dimnames) > 2 ){
  #   if(monthly) {
  #     if(verbose) print("Got monthly data")
  #     
  #     # get the year and month as a numeric code with format "Year.Month"
  #     all.year.months <- as.numeric(all.dimnames[[3]]) / 100
  #     all.years <- sort(unique((trunc(all.year.months))))
  #     
  #     # if no start date is set start at Jan 1st of the first year of data
  #     if(is.null(start.date)) start.date <- as.Date(paste(all.years[1], "01", "01", sep = "-"))
  #     
  #     # make the time values, taking of the inconvenient fact that months have different numbers of days
  #     month.cumulative.additions <- c(0)
  #     for(month in all.months[1:11]) {
  #       month.cumulative.additions <- append(month.cumulative.additions, month.cumulative.additions[length(month.cumulative.additions)] + month@days)
  #     }
  #     
  #     time.vals <- c()
  #     start.year <- as.numeric(format(start.date,"%Y"))
  #     for(year.month in all.year.months) {
  #       
  #       year <- trunc(year.month)
  #       month <- as.integer((year.month - year) * 100) 
  #       time.vals <- append(time.vals, (year-start.year) * 365 + month.cumulative.additions[month+1])
  #       
  #     }
  #     
  #   }
  #   
  #   # else is yearly, so make annual axis of entries 365 days apart
  #   else {
  #     if(verbose) print("Got annual data")
  #     
  #     all.years <- as.numeric(all.dimnames[[3]])
  #     
  #     # if no start date is set start at Jan 1st of the first year of data
  #     if(is.null(start.date)) start.date <- as.Date(paste(all.years[1], "01", "01", sep = "-"))
  #     
  #     time.vals <- (all.years - as.numeric(format(start.date,"%Y"))) * 365
  #     
  #   }
  #   
  #   # make start date and calendar
  #   calendar <- "365_day" 
  #   time.unit <- paste("days since", start.date)
  #   
  #   all.dims[["Time"]] <- ncdf4::ncdim_def(name = time.dim.name, units = time.unit, vals = time.vals , calendar = calendar, unlim=TRUE, create_dimvar=TRUE)
  #   
  # }
  
  ### PREPARE THE VARIABLES: ONE FOR EACH LAYER OR COMBINE THEM IF WE WANT LAYERS TO BE DEFINED ALONG A DIMENSION AXIS
  all.vars <- list()
  # individual layers
  if(is.null(layer.dim.name)) {  
    for(layer in layers) {
      all.vars[[layer]] <- ncdf4::ncvar_def(name = layer, units = quantity.units, dim = all.dims, longname = long.name, prec = precision, missing.fill.value)  # standard
    }
  }
  # else turn layers into a dimension
  else {
    
    all.vars <- ncdf4::ncvar_def(name = quantity@id, units = quantity.units, dim = all.dims, longname = long.name, prec = precision, missing.fill.value)  # standard
    old.layers <- layers # for storing the key from dimension values to layer
    
  } 
  
  
  ### MAKE THE NETCDF FILE
  if(verbose) print(paste("Creating the output file", filename))
  outfile <- ncdf4::nc_create(filename, all.vars, verbose=verbose, force_v4=TRUE)
  
  
  ### PUT EACH VARIABLE INTO THE FILE
  
  for(layer in layers) {
    if(verbose) print(paste("Saving variable", layer, "to file",  filename, sep =" " ), quote=FALSE)
    # simple case of one variable per layer
    if(is.null(layer.dim.name)) {
      ncdf4::ncvar_put(nc = outfile, varid = layer,  vals = x[[layer]], start=NA, count=NA, verbose=verbose)
      ncdf4::ncatt_put(outfile, layer, "standard_name", standard.name)
    }
    # else slightly more complicated case of all layers going into one variable
    else{
      this.layer.index <- which(layer == layers)
      start.indices <- c(1,1,this.layer.index,1)
      count.indices <- c(-1,-1,1,-1)
      ncdf4::ncvar_put(nc = outfile, varid = all.vars,  vals = x[[layer]], start=start.indices, count=count.indices, verbose=verbose)
    }
    
  }
  
  rm(x); gc()
  
  # add meta-data if layers collapsed to a dimension
  if(!is.null(layer.dim.name)) {
    ncdf4::ncatt_put(outfile, quantity@id, "standard_name", standard.name)
    for(counter in 1:length(old.layers)){
      ncdf4::ncatt_put(outfile, all.vars, paste(layer.dim.name, counter, sep ="_"), old.layers[[counter]])
    }
  }
  
  
  # STANDARD SPATIAL ATTRIBUTES
  outfile <- addStandardSpatialAttributes(outfile)
  
  
  # ADD GENERAL ATTRIBUTES
  ncdf4::ncatt_put(outfile, 0, "Conventions", "CF-1.6")
  if(!is.null(source)) {
    ncdf4::ncatt_put(outfile, 0, "Source_Format", source@format@id)
    ncdf4::ncatt_put(outfile, 0, "Name", source@name)
    ncdf4::ncatt_put(outfile, 0, "Forcing_data", source@forcing.data)
    if (source@contact != "")
      ncdf4::ncatt_put(outfile, 0, "Contact", source@contact)
    if (source@institute != "" || source@institute != "none")
      ncdf4::ncatt_put(outfile, 0, "Institute", source@institute)
  }
  
  # ADD DGVMDATA ATTRIBUTES
  ncdf4::ncatt_put(outfile, 0, "DGVMData_quant", quantity.id)
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_name", name)
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_id", id)
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_spatial.extent", "Original")
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_first.year", first.year)
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_last.year", last.year)
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_year.aggregation.method", "mean")
  #ncdf4::ncatt_put(outfile, 0, "DGVMData_quantity", quantity.id)
  
  # CLOSE
  ncdf4::nc_close(outfile)
  
  
})