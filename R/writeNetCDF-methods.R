#!/usr/bin/Rscript
#' writeNetCDF methods
#' 
#' Methods for writing DGVMTools::Field and raster::Raster* objects to disk as netCDF files.  Uses the ncdf4 R-package behind the scenes. 
#' NOTE: when using the function on a raster::Raster object, you cannot provide both multiple variables and multiple time periods, 
#' you can only do one or the other.   
#' 
#'  
#' @param x A Field or Raster* object to be written as a netCDF file.  The method can also handle a list of arrays (each array in the list represents one layer)
#' but this is more of a technical, internal use of this method.
#' @param filename A character specifying the name of the netCDF file to be written.
#' @param start.date A date (as a Date object) for the start of the time dimension.  Can be omitted for a Field, \emph{must} be included if you want to write 
#' a RasterStack or RasterBrick with a time dimension. 
#' @param calendar Character, describing the calendar to be used (required for a  Raster*, optional for a Field), can currently be "standard" or "365_day" (default), 
#' @param global.extent logical, if TRUE extend the netCDF file to the entire global extent, default is FALSE.
#' @param verbose Logical, if TRUE print progress and debug output (from DGVMTools not from netCDF)
#' @param nc.verbose Logical, if TRUE print progress and debug output from the NetCDF function calls.  This can be rather a lot, so is controlled 
#' separately to the DGVMTools output.
#' @param quantity A DGVMTools::Quantity object.  This is to provide meta-data (units, names) if saving a Raster* object, it is ignored in
#' the case of a Field (but note that if you want to use different meta-data for a Field just alter the Quantity object in the Field object
#' before you callwriteNetCDF)
#' @param source A DGVMTools::Source object.  This is to provide meta-data about the source of the data (model run/dataset, climate forcing, contact person etc)
#' if saving a Raster* object. It is ignored in the case of a Field (but note  that if you want to use different meta-data for a Field just alter the Source object in the Field object before you call writeNetCDF)
#' @param layer.names A vector of characters to specify the names of the variables in the netCDF file, ignored for a Field.  
#' Should be length 1 or the number of layers in the input raster. 
#' @param layer.dim.name A character string specifying the name of the fourth 'layers' dimension (ie not lon, lat or time).  If not specified (or NULL) then no fourth dimension
#' is created and each layer gets its own variable in the netCDF.  If it is specified, the layers are 'collapsed' on to elements on this fourth, layers, dimension. 
#' @param layer.dim.values A named vector of numerics which will be used if writing a 'layer' dimension.  The number corresponds to the value on the dimension, the name corresponds to the layer name.
#' Can be omitted in which case every layer will be written in the order they appear in the Field.
#' @param lat.dim.name Character, the latitude dimension name. Defaults to "lat".  
#' @param lon.dim.name Character, the longitude dimension name. Defaults to "lon". 
#' @param time.dim.name Character, the time dimension name. Defaults to "time".
#' @param time.values The values along the time dimension (in the case of Raster*, is ignored for a Field) defined as 'days since' the 'start.date' argument.
#' If not supplied, data is assumed not to have a time axis. In such case if the raster has multiple layers they will be interpreted  
#' @param add.missing.cols,add.missing.rows Logical, if TRUE (default), fill missing columns of longitude/rows of latitude.  For guassian/spectral
#' grids with non-equal latitude spacing, set add.missing.rows to FALSE. 
#' @param .sta.info Advanced. An object of STAInfo for tracking spatio-temporal metadata.  IGNORED for for a Field as it is handled
#' automatically when writing a Field. But can be added manually for a raster if you wish.  
#' @param ... Other arguments that can usefully be passed to ncdf4::ncvar_def, in particular:
#' \itemize{
#'  \item{compression} {Integer to define compression level when define netCDF variables (1 = a little compression, 9 = a lot of compression). 
#'  Set to NA (default) for no compression.  Using compression forces netCDF version 4.}
#'  \item{missval} {Numeric, for the missing value.  Default is NA which gives NaN as the missing value. NULL can be used to specify "no missing value"
#'  although it is not clear what that does in the resultant netcdf file in practice.  TRENDY/GCP likes -99999.0 for missing values.}
#'  \item{prec} {Character, the output precision (although that is confusing terminology, 'type' would be more descriptive) to use in the netCDF file.  
#'  See the 'prec' argument of ncdf4::ncvar_def, can be  'short', 'integer', 'float', 'double', 'char', 'byte').  Default is 'float'.}
#'  \item{shuffle} {Logical, if TRUE turn on the shuffle filter see netCDF docs and ncdf4::ncvar_def for details}
#'  \item{chunksize} {If set, this must be a vector of integers with a length equal to the number of dimensions in the variable. Potentially very useful 
#'  to optimise the read and write time,  but rather advanced, see netCDF docs and ncdf4::ncvar_def for details}
#'  }
#' 
#' These methods offers two very convenient things.  Firstly, it allows the exporting of a DGVMTools::Field object as a standard netCDF (CF) file. 
#' Secondly, it provides a more convenient way to write Raster* objects as netCDF objects than writeRaster (at least in the eye's of the author).  
#' This because is allows specification a time axis, writes additional metadata, allows some flexibility in the formatting, doesn't invert the latitudes,
#' and should generally write CF-standard compliant files.
#' 
#' @return Nothing
#' @name writeNetCDF-methods
#' @rdname writeNetCDF-methods
#' @aliases writeNetCDF
#' @exportMethod writeNetCDF
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}   
#' 
#' @docType methods
# first define generic (note had to remove if(!isGeneric()) statement to run units tests outside Check)
setGeneric("writeNetCDF", function(x, 
                                   filename, 
                                   start.date = NULL, 
                                   verbose = TRUE, 
                                   quantity = NULL, 
                                   source = NULL, 
                                   layer.names = NULL,
                                   layer.dim.name = NULL,
                                   layer.dim.values = NULL,
                                   lat.dim.name = "lat",
                                   lon.dim.name = "lon",
                                   time.dim.name = "time",
                                   time.values = NULL, 
                                   calendar = "365_day",
                                   global.extent = FALSE,
                                   add.missing.cols = TRUE,
                                   add.missing.rows = TRUE,
                                   nc.verbose = FALSE, 
                                   .sta.info = NULL,
                                   ...) standardGeneric("writeNetCDF"))


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="Field", filename = "character"), function(x, filename, ...) {
  
  # ignore parameters and give warnings
  if(!is.null(layer.names)) warning("Argument layer.names is ignored when calling writeNetCDF() for a Field. Layer names are taken from the layer names in the Field.")
  st.names <- getDimInfo(x)
  if(!"Lon" %in% st.names || !"Lat" %in% st.names) stop("Don't have a Lon or Lat dimension in the field for writing netCDF.  Currently writing netCDF assumes a full Lon-Lat grid.  So failing now.  Contact the author if you want this feature implemented.")
  
  # If layer.dim.values provided check that each named layer is present in the Field and check that the values numeric
  if(!missing(layer.dim.values ) & !is.null(layer.dim.values)) {
    if(is.null(layer.dim.name)) stop("Specifying 'layer.dim.values' only makes sense if you specify a layer.dim.name, please check nyour arguments.")
    if(is.null(names(layer.dim.values)) || !is.numeric(layer.dim.values)) stop("If provided, the argument layer.dim.values must be a vector of numerics with uniques names (corresponding to the layer of x)")
    for(this_name in names(layer.dim.values)) {
      if(!this_name %in% layers(x)) stop(paste0("Layer ", this_name, " requested for writing to the NetCDF file in lay.dim.values, but no layer of that name is present in the Field to be written."))
    }
  }
  # else if it wasn't provided, make one with all layers in the order that they come
  else {
    layer.dim.values <- 1:length(layers(x))
    names(layer.dim.values) <- layers(x)
  }
  
  # determine start.date if it is not specified
  if(missing(start.date)  || is.null(start.date)) {
    
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
        start.date <- as.Date(paste(first.year, first.month, "01", sep = "-"), format='%Y-%m-%d')
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
        start.date <- as.Date(first.day-1, origin = paste0(first.year, "-01-01"))
        
      }
      else {
        start.date <- as.Date(paste(first.year, "1", "1", sep = "-"), format='%Y-%m-%d')
      }
      
    }
    
  }
  
  
  # make a list of arrays from the Field (one array per Layer in the Field)
  # note that the labelling along the time dimension of the array(s) should correspond to "days since start.date", respecting the calendar argument 
  if(verbose) {
    message(paste0("Calling function FieldToArray() on a Field with ", nrow(x@data), " spatio-temporal data points and a total of ", length(layers(x)), " layers"))
    t1 <- Sys.time()
  }
  array.list <- FieldToArray(x, cname = names(layer.dim.values), start.date = start.date, calendar = calendar, add.missing.cols = add.missing.cols, add.missing.rows = add.missing.rows, global.extent = global.extent, verbose = verbose) 
  if(verbose) {
    t2 <- Sys.time()
    message("FieldToArray took:")
    print(t2-t1)
  } 
  
  
  
  # grab other metadata from the Field
  this.quant <- x@quant
  this.source <- x@source
  this.sta.info <- as(x, "STAInfo")
  rm(x); gc()
  
  writeNetCDF(array.list, 
              filename, 
              verbose = verbose, 
              nc.verbose = nc.verbose, 
              start.date = start.date,
              quantity = this.quant ,
              source = this.source,
              layer.dim.name = layer.dim.name,
              layer.dim.values = layer.dim.values,
              lat.dim.name = lat.dim.name,
              lon.dim.name = lon.dim.name,
              time.dim.name = time.dim.name,
              calendar = calendar,
              .sta.info = this.sta.info,
              ...)
  
})


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="Raster", filename = "character"), function(x, filename, ...) {
  
  if(is.null(quantity) || missing(quantity)) stop("When calling a writeNetCDF() on a raster you *need* to pass in a quantity object supply metadata. \n Fortunately the is easy to define if you don't have one, see Quantity-class")
  if(is.null(layer.names) || missing(layer.names)) stop("When calling a writeNetCDF() on a raster you *need* to provide a 'layer.names' argument to determine how to name the layers.")
  
  if(!missing(layer.dim.values)) warning("Argument 'layer.dim.values' to writeNetCDF is ignored when writing Raster objects.")
  
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
        dimnames(this.array) <- list(lon = lon.list, lat = lat.list, time = 0)
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
    
    # whack the whole raster into an array (taking care of the stupid inverted lats)
    this.array <- aperm(raster::as.array(x), c(2,1,3))
    this.array <- this.array[, dim(this.array)[2]:1, ]
    
    
    dimnames(this.array) <- list(lon = lon.list, lat = lat.list, time = time.values)
    array.list[[layer.names[1]]] <- this.array
    rm(this.array)
    gc()
    
  }
  
  
  writeNetCDF(array.list, 
              filename, 
              verbose = verbose, 
              nc.verbose = nc.verbose,
              start.date = start.date,
              quantity = quantity,
              source = source,
              layer.dim.name = layer.dim.name,
              layer.dim.values = layer.dim.values,
              lat.dim.name = lat.dim.name,
              lon.dim.name = lon.dim.name,
              time.dim.name = time.dim.name,
              calendar = calendar,
              .sta.info = .sta.info,
              ...)
  
})

# Support for terra package here simply enabled by converting a terra::SpatRaster object to a raster::Raster object and proceeding from there
# Eventually this should be replaced with a terra only pathway as raster becomes deprectaed
#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="SpatRaster", filename = "character"), function(x, filename, ...) {
  
  if(is.null(quantity) || missing(quantity)) stop("When calling a writeNetCDF() on a SpatRaster you *need* to pass in a quantity object supply metadata. \n Fortunately the is easy to define if you don't have one, see Quantity-class")
  if(is.null(layer.names) || missing(layer.names)) stop("When calling a writeNetCDF() on a SpatRaster you *need* to provide a 'layer.names' argument to determine how to name the layers.")
  
  if(!missing(layer.dim.values)) warning("Argument 'layer.dim.values' to writeNetCDF is ignored when writing SpatRaster objects.")

  print(x)
  x_raster <- as(x, "Raster")
  print(x_raster)
  writeNetCDF(as(x, "Raster"), 
              filename, 
              verbose = verbose, 
              nc.verbose = nc.verbose,
              start.date = start.date,
              quantity = quantity,
              source = source,
              layer.names = layer.names,
              layer.dim.name = layer.dim.name,
              layer.dim.values = layer.dim.values,
              lat.dim.name = lat.dim.name,
              lon.dim.name = lon.dim.name,
              time.dim.name = time.dim.name,
              calendar = calendar,
              .sta.info = .sta.info,
              ...)
  
})


#' @rdname writeNetCDF-methods
setMethod("writeNetCDF", signature(x="list", filename = "character"), function(x, filename, ...) {
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to write netCDF files.")
  
  ### GET METADATA FROM QUANTITY IF PRESENT
  # TODO - a quantity must be defined, so this can likely be simplified
  quantity.units <- "Not_defined"
  quantity.id <- "Not_defined"
  standard_name <- "Not_defined"
  long_name <- "Not_defined"
  if(!is.null(quantity)) {
    quantity.units <- quantity@units
    quantity.id <- quantity@id
    standard_name <- quantity@standard_name
    long_name <- quantity@name
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
  
  # if layer.dim.values supplied then use them, else use the names that come on the list of arrays
  if(!missing(layer.dim.values) & !is.null(layer.dim.values))  layers <- names(layer.dim.values)
  else layers <- names(x)
  
  
  ### MAKE DIMENSIONS ####
  all.dimnames <- dimnames(x[[1]])
  all.dims <- list()
  
  # LON and LAT - easy-peasy
  all.dims[["Lon"]] <- ncdf4::ncdim_def(name = lon.dim.name, units = "degrees", vals = as.numeric(all.dimnames[[1]]), unlim=FALSE, create_dimvar=TRUE)
  all.dims[["Lat"]] <- ncdf4::ncdim_def(name = lat.dim.name, units = "degrees", vals = as.numeric(all.dimnames[[2]]), unlim=FALSE, create_dimvar=TRUE)
  
  # LAYER - only if a layer.dim.name has been specifed which means collapse all the different layers as values along a dimension
  if(!is.null(layer.dim.name)) {
    
    if(!is.character(layer.dim.name)) stop("layer.dim.name must be NULL or a character string (For example, \"VegType\" or \"CarbonPool\"")
    all.dims[[layer.dim.name]] <- ncdf4::ncdim_def(name = layer.dim.name, units = "categorical", vals = layer.dim.values, create_dimvar=TRUE)
    
  }
  
  # TIME - only if start.date has been provided (and it not NULL)
  # also now very easy since the labels on the time dimension of the incoming arrays are exactly what we want: days since start date 
  if(!is.null(start.date)) {
    all.dims[["Time"]] <- ncdf4::ncdim_def(name = time.dim.name,  
                                           units = paste("days since", start.date, "12:00:00"), 
                                           vals = as.numeric(all.dimnames[[3]]), 
                                           calendar = calendar, 
                                           unlim=TRUE, 
                                           create_dimvar=TRUE)
  }
  
  
  ### DEFINE THE VARIABLES IN THE NETCDF FILE: ONE FOR EACH LAYER, OR COMBINE THEM IF WE WANT LAYERS TO BE DEFINED ALONG A DIMENSION AXIS
  all.vars <- list()
  # individual layers
  if(is.null(layer.dim.name)) {  
    for(layer in layers) {
      all.vars[[layer]] <- ncdf4::ncvar_def(name = layer, units = quantity.units, dim = all.dims, longname = long_name, ...)  # standard
    }
  }
  # else turn layers into a dimension
  else {
    all.vars <- ncdf4::ncvar_def(name = quantity@id, units = quantity.units, dim = all.dims, longname = long_name, ...)  # standard
  } 
  
  
  ### MAKE THE NETCDF FILE
  if(verbose) message(paste("** Creating the output file", filename))
  outfile <- ncdf4::nc_create(filename, all.vars, verbose=nc.verbose, force_v4=FALSE)
  
  
  ### PUT EACH VARIABLE INTO THE FILE
  
  for(layer in layers) {
    if(verbose) message(paste(" **** Saving variable", layer, "to file",  filename, sep = " "))
    # simple case of one variable per layer
    if(is.null(layer.dim.name)) {
      ncdf4::ncvar_put(nc = outfile, varid = layer,  vals = x[[layer]], start=NA, count=NA, verbose=nc.verbose)
      ncdf4::ncatt_put(outfile, layer, "standard_name", standard_name)
      
    }
    # else slightly more complicated case of all layers going into one variable
    else{
      this.layer.index <- which(layer == layers)
      start.indices <- c(1,1,this.layer.index,1)
      count.indices <- c(-1,-1,1,-1)
      ncdf4::ncvar_put(nc = outfile, varid = all.vars,  vals = x[[layer]], start=start.indices, count=count.indices, verbose=nc.verbose)
    }
    
  }
  
  rm(x); gc()
  
  # ADD METADATA ATTRIBUTES
  
  # if layers collapsed to a dimension, add the metadata (layer names) to the dimension axis
  if(!is.null(layer.dim.name)) {
    # standard name
    ncdf4::ncatt_put(outfile, quantity@id, "standard_name", standard_name)
    for(this_layer_name in names(layer.dim.values)){
      ncdf4::ncatt_put(outfile, layer.dim.name, paste(layer.dim.name, layer.dim.values[this_layer_name], sep ="_"), this_layer_name)
    }
  }
  
  
  # standard spatial attributes
  outfile <- addStandardSpatialAttributes(outfile)
  
  # general attributes
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
  
  # DGVMTools attributes
  ncdf4::ncatt_put(outfile, 0, "DGVMTools_quant", quantity.id)
  # Spatio-temporal iinfo if available
  if(!missing(.sta.info) & !is.null(.sta.info)) {
    
    if(inherits(.sta.info@spatial.extent, "Extent")){
      ncdf4::ncatt_put(outfile, 0, "xmin", .sta.info@spatial.extent@xmin)
      ncdf4::ncatt_put(outfile, 0, "xmax", .sta.info@spatial.extent@xmax)
      ncdf4::ncatt_put(outfile, 0, "ymin", .sta.info@spatial.extent@ymin)
      ncdf4::ncatt_put(outfile, 0, "ymax", .sta.info@spatial.extent@ymax)
    }
    if(!is.null(.sta.info@spatial.extent.id) && length(.sta.info@spatial.extent.id) > 0) ncdf4::ncatt_put(outfile, 0, "spatial.extent.id", .sta.info@spatial.extent.id)
    if(!is.null(.sta.info@spatial.aggregate.method) && length(.sta.info@spatial.aggregate.method) > 0) ncdf4::ncatt_put(outfile, 0, "spatial.aggregate.method", .sta.info@spatial.aggregate.method)
    if(!is.null(.sta.info@first.year) && length(.sta.info@first.year) > 0) ncdf4::ncatt_put(outfile, 0, "first.year", .sta.info@first.year)
    if(!is.null(.sta.info@last.year) && length(.sta.info@last.year) > 0)ncdf4::ncatt_put(outfile, 0, "last.year", .sta.info@last.year)
    if(!is.null(.sta.info@year.aggregate.method) && length(.sta.info@year.aggregate.method) > 0) ncdf4::ncatt_put(outfile, 0, "year.aggregate.method", .sta.info@year.aggregate.method)
    if(!is.null(.sta.info@subannual.resolution) && length(.sta.info@subannual.resolution) > 0) ncdf4::ncatt_put(outfile, 0, "subannual.resolution", .sta.info@subannual.resolution)
    if(!is.null(.sta.info@subannual.aggregate.method) && length(.sta.info@subannual.aggregate.method) > 0) ncdf4::ncatt_put(outfile, 0, "subannual.aggregate.method", .sta.info@subannual.aggregate.method)
    if(!is.null(.sta.info@subannual.original) && length(.sta.info@subannual.original) > 0) ncdf4::ncatt_put(outfile, 0, "subannual.original", .sta.info@subannual.original)
    
  }
  
  
  # CLOSE
  ncdf4::nc_close(outfile)
  
  if(verbose) message("Closed file.")
  
})