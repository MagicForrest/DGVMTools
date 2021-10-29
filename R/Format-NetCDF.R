#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE NetCDF FILES ###########################################################
############################################################################################################################


#' Get a Field for NetCDF
#' 
#' 
#' An internal function that reads data from an NetCDF .nc file.  
#' 
#' @param source A \code{Source} containing the meta-data about the NetCDF source
#' @param quant A Quantity object to specify what quantity should be opened. 
#' @param target.sta.info An STAInfo object defining the spatial-temporal-annual extent over which we want the data
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is automatically generated
#' @param calendar Character string, sometimes the calendar string on the time axis can be incorrect or missing.  Here you can manually provide it.
#' Note: A common error in paleo files is "standard" instead of "proleptic_gregorian". Specifically, if you have dates with years 1582 
#' (the start of the Gregorian calendar) and it includes leap years the calendar needs to be set to "proleptic_gregorian".
#' @param vars.to.ignore A list of character strings specifying which cvariables *not* to read.  These are typically metadata about the coordinate data
#' (hence the defaults ""time_bnds", "lon_bnds", "lat_bnds", "lat" and "lon"), but you can also use this argument to ignore a particular data variable from a file.
#' that typically hold coordinate metadata 
#' @param verbose A logical, set to true to give progress/debug information
#' @param nc.verbose A logical, set to true to give progress/debug information from the ncdf4 package functions.  This can be a lot, 
#' so it is handy to control that separately.
#' @return A list containing firstly the data.table containing the data, and secondly the STAInfo for the data that we have
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @include classes.R
#' @keywords internal
getField_NetCDF <- function(source,
                            quant,
                            target.STAInfo,
                            file.name,
                            verbose = FALSE,
                            nc.verbose = FALSE,
                            vars.to.ignore = c("time_bnds", "lon_bnds", "lat_bnds", "lat", "lon"),
                            calendar = "standard") {
  
  # timing
  t1 <- Sys.time()
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to read NetCDF files.")
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Day = Time = Temp =NULL
  
  # supported calendars
  supported.calendars <- c("standard", "gregorian", "proleptic_gregorian", "360_day", "366_day", "365_day", "uniform30day", "no_leap", "all_leap", "noleap", "allleap")
  
  # possible dimensions names
  # TODO maybe move these to arguments for greater flexibility
  possible.lat.dim.names <- c("lat", "Lat", "latitude", "Latitude", "y", "Y")
  possible.lon.dim.names <- c("lon", "Lon", "longitude", "Longitude", "x", "X")
  possible.time.dim.names <- c("time", "Time", "times", "Times", "year", "Year", "month", "Month", "t", "T")
  
  
  # handy function to check for attributes, also including the kind of deprecated "DGVMTools_" and "DGVMData_" variants for global attributes
  lookupAttribute <- function(nc, var, attname, verbose) {
    this_try <- ncdf4::ncatt_get(nc, var, attname, verbose)
    if(this_try$hasatt) return(this_try$value)
    this_try <- ncdf4::ncatt_get(nc, var, paste0("DGVMData_",  attname), verbose)
    if(this_try$hasatt) return(this_try$value)
    this_try <- ncdf4::ncatt_get(nc, var, paste0("DGVMTools_", attname), verbose)
    if(this_try$hasatt) return(this_try$value)
    return("Unspecified")
  }
  
  
  # get first and last year for use later on
  first.year = target.STAInfo@first.year
  last.year = target.STAInfo@last.year
  
  # STAInfo object describing the data
  sta.info = new("STAInfo")
  
  # Make the filename (if necessary) and check for the file, gunzip if necessary, fail if not present
  if(!is.null(file.name)) file.name.nc <- file.path(source@dir, file.name)
  else file.name.nc <- file.path(source@dir, paste(quant@id, "nc", sep = "."))
  file.name.nc.gz <- paste(file.name.nc, "gz", sep = ".")
  zipped <- FALSE
  if(file.exists(file.name.nc)){ 
    if(verbose) message(paste("Found and opening file", file.name.nc, sep = " "))
  }
  else if(file.exists(file.name.nc.gz)){
    zipped <- TRUE
    R.utils::gunzip(file.name.nc.gz)
    if(verbose) message(paste("Found, gunzipping and opening file", file.name.nc.gz, sep = " "))
  }
  
  # Open file
  if(verbose) message(paste0("Opening file ", file.name.nc))     
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=nc.verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=nc.verbose, suppress_dimvals=FALSE ))
  
  #### EXAMINE VARIABLES PRESENT IN FILE ####
  
  # Build a vector of variables which have dimensions associated with them and which aren't flagged as dimension variables
  all_vars_names <- c()
  for(this_var in this.nc$var){
    if(this_var$ndims > 0 && !this_var$id$isdimvar) all_vars_names <- append(all_vars_names, this_var$name)
  }
  
  if(verbose) message(paste("* NetCDF file contains potentially useful variables:", paste(all_vars_names, collapse = ", ")))
  vars.to.read <- list()
  if(quant@id %in% all_vars_names) {
    vars.to.read <- quant@id
    if(verbose) message(paste0("* Reading variable ", vars.to.read, ", which matches requested quant string or ID"))
  }
  else {
    vars.to.read <- all_vars_names[!all_vars_names %in% vars.to.ignore]
    if(verbose) {
      message(paste0("* No variable in netCDF file matches requested quant ID (", quant@id, ") so reading *all* netCDF variables that look suitable: ", paste(vars.to.read, collapse = ", ")))
      message(paste0("  (TIP: If you don't want to try to read all these variables (say because this function later fails) use the 'vars.to.ignore' argument in your 'getField()' call)"))
    }
  }
  
  
  #### FIND DIMENSIONS USED BY VARIABLES ####
  # having established which variables we want to read, check which dimensions they have 
  # and, if we have got multiple variables, check that they have the same dimensions
  # potential TODO - this can probably be factored out
  
  # for each variable to read
  all_required_dims <- list()
  for(this_var in vars.to.read) {
    
    # find the "var" object corresponding to it
    for(this_var_obj in this.nc$var) {
      if(this_var_obj$name == this_var) break
    }
    
    # make a vector of the dimensions required
    these_required_dims <- c()   
    for(this_dim in this_var_obj$dim) {
      these_required_dims <- append(these_required_dims, this_dim$name)
    }
    all_required_dims[[this_var_obj$name]] <- these_required_dims
    
  }
  
  # check all variables have the same dimensions and make the final list of required dimensions
  if(length(unique(all_required_dims)) != 1) stop("In a single getField() call (NetCDF Format), trying to read variables with different dimensions.  Please read these separately in *different* getField() calls.")
  all_required_dims <- unlist(unique(all_required_dims))
  if(verbose) message(paste("* All variables indexed by dimensions:", paste(all_required_dims, collapse = ", ")))
  
  #### READ AND VERFIY DIMENSION INFO ####
  
  
  # set dimensions names and values to zero/zero-length strings until a matching dimension is found
  all.lats <- numeric(0)
  all.lons <- numeric(0)
  all.time.intervals <- numeric(0)
  all.layer.vals <- numeric(0)
  lat.string <- ""
  lon.string <- ""
  time.string <- ""
  layer.string <- ""
  
  # assign each of the required dimensions to lon, lat, time or 'layer'
  for(this.dimension in all_required_dims) {
    
    # pick up Lat
    if(this.dimension %in% possible.lat.dim.names) {
      lat.string <- this.dimension
      all.lats <- ncdf4::ncvar_get(this.nc, lat.string) 
      if(verbose) message(paste0("** Confirmed latitude dimension: '", lat.string, "', with ", length(all.lats), " values."))
    }
    
    # pick up Lon
    else if(this.dimension %in% possible.lon.dim.names) {
      lon.string <- this.dimension
      all.lons <-  ncdf4::ncvar_get(this.nc, lon.string) 
      if(verbose) message(paste0("** Confirmed longitude dimension: '", lon.string, "', with ", length(all.lons), " values."))
    }
    
    # pick up Time
    else if(this.dimension %in% possible.time.dim.names) {
      time.string <- this.dimension
      all.time.intervals <- ncdf4::ncvar_get(this.nc, time.string)
      if(verbose) message(paste0("** Confirmed time dimension: '", time.string, "', with ", length(all.time.intervals), " values."))
    }                   
    
    # pick up 'layer' dimension (ie not one of the previous)
    else if(layer.string == ""){
      layer.string <- this.dimension
      
      ## SPECIAL CASE: for layer type dimensions in case they they have no corresponding variable defined
      # for try with exception handling
      all.layer.vals <- tryCatch({
        ncdf4::ncvar_get(this.nc, layer.string)
      }, warning = function(war) {
        print("getField_NetCDF: **** SAVED **** No it won't, exception caught! Boom! But maybe you want to consider adding netcdf 'variables' for each 'dimension' in the file.")
        1:this.nc$dim[[this.dimension]]$len
      }, error = function(err) {
        print("getField_NetCDF: **** SAVED **** No it won't, exception caught! Boom! But maybe you want to consider adding netcdf 'variables' for each 'dimension' in the file.")
        1:this.nc$dim[[this.dimension]]$len
      }, finally = {
      })
      
      if(verbose) message(paste0("** Confirmed layer-type dimension: '", layer.string, "', with ", length(all.layer.vals), " values."))
    }
    # If two "layer" dimensions found then fail
    else {
      stop(paste0("Found two 'layer'-type dimensions (i.e. not lon, lat or time): ", this.dimension, " and ", layer.string,  ". Can't deal with that, so stopping."))
    }
    
  }
  
  # check for at least one dimension
  if( lat.string == "" &&  lon.string == "" &&  time.string == "" &&  layer.string == "") {
    stop("Not picked up any dimensions, failing...")
  }
  
  # check if we have multiple layers from both a "vegtype" style axis *and* multiple requested variable inside the netcdf file
  if(length(layer.string) > 0 & length(vars.to.read) > 1) {
    
  }
  
  
  
  #### LON AXIS AND LAT AXIS
  
  # default values if no longitude or latitude selection is required
  start.lon <- 1
  count.lon <- -1
  start.lat <- 1
  count.lat <- -1
  
  # TODO potential lon/lat cropping here
  
  
  #### TIME AXIS - calculate the time labels and the start and count values for the time axis
  
  
  # First, let's determine some details about the time axis if there is one
  if(length(all.time.intervals) > 0) { 
    
    
    # extract the details about the time axis, if the units attributes exists
    relative.time.axis <- FALSE
    time.units.attr <- ncdf4::ncatt_get(this.nc, time.string, "units")
    if(time.units.attr$hasatt) {
      time.units.string.vec <- unlist(strsplit(time.units.attr$value, " "))
      timestep.unit <- time.units.string.vec[1]
      if(length(time.units.string.vec) > 1) {
        if(tolower(time.units.string.vec[2]) == "since"  || tolower(time.units.string.vec[2]) == "after") relative.time.axis <- TRUE
        else if(tolower(time.units.string.vec[2]) == "as") stop("Absolute time axis with 'day as ...' not currently implemented.  Contact the package author is this becomes important for you.")
      }
    }
    # if no unit attributes make some assumptions (based on the axis name) only or fail
    else {
      if(tolower(time.string) == "years"  || tolower(time.string) == "year") {
        timestep.unit <- "year"
      }
      else if(tolower(time.string) == "months"  || tolower(time.string) == "month") {
        timestep.unit <- "month"
      }
      else stop(paste0("Unclear time axis in file ", file.name.nc,", which appears to have a time axis named ", time.string, " but no units attribute to help me interpret it."))
    }
    
    
    #### RELATIVE TIME AXIS ####
    if(relative.time.axis) {
      
      # extract the start day, month and year and return it as custom date type (vector with $year, $month and $day)
      # the reason for using this custom format is to support years < 0 and > 9999
      start.date <- parseStartDate(time.units.attr$value)
      
      # if time step unit is seconds convert to days and proceed
      if(timestep.unit == "seconds") {
        all.time.intervals <- all.time.intervals/(60*60*24)
        timestep.unit <- "days"
      }
      
      
      # if time step unit is days
      if(timestep.unit == "days") {
        
        # calculate the difference between adjacent time intervals to determine the time resolution of the data
        diff.all.time.intervals <- diff(all.time.intervals)
        
        # calculate the mean to guess if is monthly or yearly 
        mean.timestep.differences <- mean(diff.all.time.intervals)
        
        
        # if the mean is between 28 and 31 (inclusive) assume monthly   
        if(mean.timestep.differences >= 28 & mean.timestep.differences <= 31 ) time.res <- "Month"
        # if the mean is between 359 and 367 (inclusive) assume yearly  
        else if(mean.timestep.differences >= 360 & mean.timestep.differences <= 367 ) time.res <- "Year"   
        # if all the time steps are integer values, assume daily (but potentially with missing days) 
        else if(isTRUE(all(diff.all.time.intervals == floor(diff.all.time.intervals))) || mean.timestep.differences == 1) time.res <- "Day"
        else stop("Data doesn't appear to be on daily, monthly, or yearly timesteps.  Other options are not currently supported by the DGVMTools, but could potentially be.  So if you need this please contact the author.")
        
        
        # lookup the calendar attribute (if not provided)
        if(missing(calendar)) {
          calendar.att <- ncdf4::ncatt_get(this.nc, time.string, "calendar")
          if(calendar.att$hasatt) calendar <- calendar.att$value
          else if(!calendar %in% supported.calendars) stop(paste("Unsupported calendar.  Supported calendars:", paste(supported.calendars, collapse = ", ")))
          else warning("DGVMTools requires a correctly defined 'calendar' attribute on a daily relative time axis.  None has been found in the netCDF file, so I am assuming 'standard'.  Note that you also can specify one in your getField() call.")
        }
        
        
        # default values if no time selection is required
        start.time <- 1
        count.time <- -1
        
        # PROCESS RELATIVE DAILY TIME AXIS AND RETURN ALL INFO IN A DATA.TABLE (including cropping to the years that are wanted)
        # This function call processes a relative time axis with unit of "days since ..."
        # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
        # and the actual years,months  and days corresponding to this selection.
        axis.info.dt <- processDailyRelativeNCAxis(axis.values = all.time.intervals, 
                                                   start.year = start.date["year"], 
                                                   start.month = start.date["month"],
                                                   start.day = start.date["day"],
                                                   target.STAInfo = target.STAInfo,
                                                   calendar = calendar,
                                                   year.offset = source@year.offset)  
        
        if(verbose) {
          message("Time axis information table:") 
          print(axis.info.dt)      
          message("You can compare the above to the netCDF time axis in your file to check everything alighns as expected.")  
          message("(The cdo command 'showdate' and ncdump with the option '-c' are very useful in this regard.)")
        }
        
        # pull out start index and count for when reading the netCDF slice
        start.time <- axis.info.dt[["NetCDFIndex"]][1]
        count.time <- nrow(axis.info.dt)
        
        # also a vector of years for establishing first and last year later on (could be streamlined)
        years.vector <- axis.info.dt[["Year"]]
        
        
        # make the "Time" labels depending on whether data is monthly, yearly or daily
        # this is used for labelling the time dimension of the slice once it is read
        if(time.res == "Year") {
          all.times <- paste(years.vector)
          if(verbose) message("Processed time axis as relative time axis with units of days and yearly values.")
        }
        else if(time.res == "Month") {
          all.times  <- paste(axis.info.dt[["Year"]], sprintf("%02d", axis.info.dt[["Month"]]), sep = ".")
          if(verbose) message("Processed time axis as relative time axis with units of days and monthly values.")
        }
        else {
          all.times  <- paste(axis.info.dt[["Year"]], sprintf("%03d", axis.info.dt[["Day"]]), sep = ".")
          if(verbose) message("Processed time axis as relative time axis with units of days and daily values.")
        }
        
      } # end if time step is days
      
      # if time step unit is month
      else if(timestep.unit == "months") {
        
        time.res <- "Month"
        
        if(verbose) message("Processing time axis as relative time axis with units of month and monthly values.")
        
        # first enforce that we have all integer months
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) {
          warning("Found non-integer months values on relative time axis with unit of months.  I am truncating these values to give integer values. Please check this aligns the months as you expect.")
          if(verbose)  message("WARNING: Found non-integer months values on relative time axis with unit of months.  I am truncating these values to give integer values. Please check this aligns the months as you expect.")
          all.time.intervals <- floor(all.time.intervals)
        } 
        
        # This function call processes a time axis which is assumed to be monthly
        # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
        # and the actual years and months corresponding to this selection.
        month.axis.info <- processMonthlyNCAxis(axis.values = all.time.intervals, 
                                                start.year = start.date["year"], 
                                                start.month = start.date["month"],
                                                target.STAInfo = target.STAInfo,
                                                year.offset = source@year.offset)  
        start.time <- month.axis.info$start.time
        count.time <- month.axis.info$count.time
        years.vector <- month.axis.info$years.vector 
        months.vector <- month.axis.info$months.vector 
        
        # make the vector of all times as a code "year.month" for labelling this dimension once it is read
        all.times  <- paste(years.vector, months.vector, sep = ".")
        
      }
      else if(timestep.unit == "years") {
        
        time.res <- "Year"
        
        # first enforce that we have all integer years
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) {
          warning("Found non-integer years values on relative time axis with unit of years.  I am truncating these values to give integer values. Please check this aligns the years as you expect.")
          if(verbose)  message("WARNING: Found non-integer years values on relative time axis with unit of years.  I am truncating these values to give integer values. Please check this aligns the years as you expect.")
          all.time.intervals <- floor(all.time.intervals)
        }
        
        
        # This function call processes a time axis which is assumed to be yearly
        # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
        # and the actual years and months corresponding to this selection.
        year.axis.info <- processYearlyNCAxis(axis.values = all.time.intervals, 
                                              start.year = start.date["year"], 
                                              target.STAInfo = target.STAInfo,
                                              year.offset = source@year.offset)  
        start.time <- year.axis.info$start.time
        count.time <- year.axis.info$count.time
        years.vector <- year.axis.info$years.vector 
        
        # make the vector of all years as character vectors for labelling this dimension once it is read
        all.times  <- paste(years.vector)
        
      }
      # else catch whatever else
      else stop('When reading relative time axes, DGVMTools only accepts unit strings of the form = "seconds/days/months/years since/as <start_time>".')
      
    } # end if relative time axis
    
    # else an absolute time axis
    else {
      
      # timestep unit "year(s)", which is pretty easy
      if(tolower(timestep.unit) == "year" || tolower(timestep.unit) == "years") {
        
        
        if(verbose) message("Processing time axis as absolute time axis with units of year and yearly values.")
        
        time.res <- "Year"
        
        # check they are all integer values (ie no confusing fractional year)
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) stop("For an absolute time axis, only integer values of years are supported")
        
        # check the values on time dimension are unique, if not something is wrong and make an assumption to make them unique
        if(length(all.time.intervals) != length(unique(all.time.intervals))) {
          warning(paste0("Don't have unique values on time (year) dimension in netCDF file ", file.name.nc, ". This should not be, so I am improvising.  Please check the years are as expected in the resulting Field and consider setting sensibles values for the time dimension."))
          print(paste0("Don't have unique values on time (year) dimension in netCDF file ", file.name.nc, ". This should not be, so I am improvising.  Please check the years are as expected in the resulting Field and consider setting sensibles values for the time dimension."))
          all.time.intervals <- all.time.intervals:(all.time.intervals+length(all.time.intervals)-1)
        }
        
        # apply year offset prior to cropping 
        if(source@year.offset != 0) all.time.intervals <- all.time.intervals + source@year.offset
        
        # get the indices consider cropping
        cropped.year.indices <- calculateYearCroppingIndices(target.STAInfo, all.time.intervals)
        start.time <- cropped.year.indices$first
        count.time <- cropped.year.indices$last - cropped.year.indices$first + 1 
        
        # get the years.vector by cropping the full years
        years.vector <- all.time.intervals[cropped.year.indices$first:cropped.year.indices$last]
        
        # make a numeric code of the years for labelling this dimension once it is read
        all.times <- paste(years.vector)
        
      }
      # timestep unit "month(s)", which is pretty easy
      else if(tolower(timestep.unit) == "month" || tolower(timestep.unit) == "months") {
        
        if(verbose) message("Processing time axis as absolute time axis with units of month and monthly values.")
        
        
        time.res <- "Month"
        # check they are all integer values (ie no confusing fractions of a month)
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) stop("For an absolute time axis, only integer values of monthd are supported")
        
        
        # a value of "0" will mess things up, so offset it 
        if(min(all.time.intervals) == 0) all.time.intervals <- all.time.intervals +1
        
        # This function call processes a time axis which is assumed to be monthly
        # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
        # and the actual years and months corresponding to this selection.
        month.axis.info <- processMonthlyNCAxis(axis.values = all.time.intervals, 
                                                start.year = 1, # absolute time axis, start in "year 1" because we have no other reference 
                                                start.month = all.time.intervals[1], # again absolute time axis, so take the first month as the start month
                                                target.STAInfo = target.STAInfo,
                                                year.offset = source@year.offset)  
        start.time <- month.axis.info$start.time
        count.time <- month.axis.info$count.time
        years.vector <- month.axis.info$years.vector 
        months.vector <- month.axis.info$months.vector 
        
        # make the vector of all times as a code "year.month" for labelling this dimension once it is read
        all.times  <- paste(years.vector, months.vector, sep = ".")
        
      }
      else {
        stop("Absolute time axes with any unit other than 'year' or 'month' not currently supported")
      }
      
    }
    
    
    # set year and subannual meta-data - note that years.vector (ie. the final years of data that will be selected) must be defined above
    
    # time resolution and start and end years are determined from the file and cropping
    sta.info@first.year <- years.vector[1]
    sta.info@last.year <- years.vector[length(years.vector)]
    sta.info@subannual.resolution <- time.res
    
    # original subannual resolution and subannual aggregation method can be determined from metadata if they exist
    sta.info@subannual.aggregate.method <- lookupAttribute(this.nc, 0, "subannual.aggregate.method", nc.verbose)
    
    sta.info@subannual.original <- lookupAttribute(this.nc, 0, "subannual.original", nc.verbose)
    if(sta.info@subannual.original == "none") sta.info@subannual.original <- time.res
    
  }
  
  
  
  #### RETRIEVE THE DATA
  
  # list of data.tables, one for each netCDF var (which will be interpreted as a 'Layer' in DGVMTools terms)
  dt.list <- list()
  
  # keep track of all the units, long names and standard names for later
  assumed.units <- c()
  all.standard_names <- c()
  all.long_names <- c()
  
  
  
  for(this.var in vars.to.read) {
    
    if(verbose) message(paste0("* Reading variable: '", this.var, "'"))
    
    for(this_var_obj in this.nc$var) {
      if(this_var_obj$name == this.var) {
        break
      }
    }
    
    # set start and count appropriately
    # note this is done slightly clumsily with a for loop to make sure the ordering is correct
    start <- numeric()
    count <- numeric()
    # also set up the dimension list for naming the dimensions once the data cube is read
    dimension.names <- list()
    
    for(this.dim in this_var_obj$dim) {
      
      if(this.dim$name == lat.string) {
        start <- append(start, start.lat)
        count <- append(count, count.lat)
        if(length(all.lats) > 0) dimension.names[["Lat"]] <- all.lats
        if(verbose) message(paste("* Reading data for", count.lat, "latitude values ('-1' means reading the full range available)"))
      }
      else if(this.dim$name  == lon.string) {
        start <- append(start, start.lon)
        count <- append(count, count.lon)
        if(length(all.lons) > 0) dimension.names[["Lon"]] <- all.lons
        if(verbose) message(paste("* Reading data for", count.lon, "longitude values ('-1' means reading the full range available)"))
      }
      else if(this.dim$name  == time.string) {
        start <- append(start, start.time)
        count <- append(count, count.time)
        if(length(all.time.intervals) > 0) dimension.names[["Time"]] <- all.times
        if(verbose) message(paste("* Reading data for", count.time, "time values ('-1' means reading the full range available)"))
      }
      else if(this.dim$name  == layer.string) {
        start <- append(start, 1)
        count <- append(count, -1)
        if(length(all.layer.vals) > 0) dimension.names[["Layers"]] <- all.layer.vals
        if(verbose) message(paste("* Reading data for", -1, "layer values ('-1' means reading the full range available"))
      }
      
    }
    
    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this_var_obj, start = start, count = count, verbose = nc.verbose, collapse_degen=FALSE)
    if(verbose) message(paste0("Read slice of netCDF file with dimensions: ", paste(dim(this.slice), collapse = ", ")))
    if(verbose) message(paste("Setting dimension names: ", paste(names(dimension.names), collapse = ", ")))
    dimnames(this.slice) <- dimension.names
    if(ncdf4::ncatt_get(this.nc, this_var_obj, "units")$hasatt) {
      # read the units for this layer
      these.units <- ncdf4::ncatt_get(this.nc, this_var_obj, "units")$value
      # if first units hasn't been defined take these ones
      if(length(assumed.units) == 0) assumed.units <- these.units
      # else check if the next units are the same, if not issue a warnings
      else {
        if(!identical(these.units, assumed.units)) warning(paste0("Multiple units found when reading data from NetCDF file ", file.name.nc, ", using the first one (", paste(assumed.units, collapse = ", "), ") and ignoring the later one (", paste(these.units, collapse = ", "), ")."))
      }
    }
    if(ncdf4::ncatt_get(this.nc, this_var_obj, "standard_name")$hasatt) all.standard_names <- append(all.standard_names,  ncdf4::ncatt_get(this.nc, this_var_obj, "standard_name")$value)
    if(ncdf4::ncatt_get(this.nc, this_var_obj, "long_name")$hasatt) all.long_names <- append(all.long_names,  ncdf4::ncatt_get(this.nc, this_var_obj, "long_name")$value)
    
    
    # if only a 2-D 'matrix', convert to a 3-D 'array' to that it melts properly with the as.data.table command below
    if(length(dim(this.slice)) < 3) {
      dim(this.slice) <- c(dim(this.slice), 1)
      dimension.names[["Temp"]] <- 1
      dimnames(this.slice) <- dimension.names
    }
    
    # 'melt' and remove the Temp column if necessary 
    if(verbose) message(paste("Melting array slice to data.table.  This might take a while (depending on the size of the slice)..."))
    # TODO This is pretty much the rate-limiting step and it can gobble up a huge amount of memory.  
    # But there is a possible solution.  It should be possible to split up this.slice (totally arbitrarily) in to smaller slices
    # which came be melted separated and then rbind-ed together at the end.  That will definely help with the memory issue, 
    # not sure about speed
    this.slice.dt <- as.data.table(this.slice, na.rm = TRUE, keep.rownames = TRUE)
    if("Temp" %in% names(this.slice.dt)) this.slice.dt[ , Temp := NULL]
    if("Lon" %in% names(this.slice.dt)) this.slice.dt[ , Lon := as.numeric(Lon)]
    if("Lat" %in% names(this.slice.dt)) this.slice.dt[ , Lat := as.numeric(Lat)]
    if("Time" %in% names(this.slice.dt)) this.slice.dt[ , Time := as.numeric(Time)]
    if(verbose) message(paste("... done!"))
    
    
    # clear up 
    rm(this.slice)
    gc()
    
    # if necessary dcast the Layers column back to make separate columns for each layer
    if("Layers" %in% names(this.slice.dt)) this.slice.dt <- dcast(this.slice.dt, ... ~ Layers, value.var = "value")
    
    this.slice.dt <- stats::na.omit(this.slice.dt)
    if("value" %in% names(this.slice.dt)) setnames(this.slice.dt, "value", this_var_obj$name)
    
    
    # ###  HANDLE TIME AXIS 
    if(length(all.time.intervals) > 0) {
      
      if(verbose) message("Translating Time axis to Year/Month/Day as required...")
      
      if(time.res == "Year") {
        if("Time" %in% names(this.slice.dt)) setnames(this.slice.dt, "Time", "Year")
      }
      else if(time.res == "Month") {
        toYearandMonth <- function(x) {
          Year <- as.integer(trunc(x))
          return(list("Year" = Year, "Month" = as.integer(round((x-Year)*100))) )
        }
        this.slice.dt[, c("Year", "Month") := toYearandMonth(Time)]
        if(min(this.slice.dt[["Month"]]) < 0)  this.slice.dt[, Month := abs(Month)]
        this.slice.dt[, Time := NULL]
        
      }
      else if(time.res == "Day") {
        toYearandDay <- function(x) {
          Year <- as.integer(trunc(x))
          return(list("Year" = Year, "Day" = abs(as.integer(round((x-Year)*1000)))) )
        }
        this.slice.dt[, c("Year", "Day") := toYearandDay(Time)]
        if(min(this.slice.dt[["Day"]]) < 0)  this.slice.dt[, Day := abs(Day)]
        this.slice.dt[, Time := NULL]
        
      }
      
      if(verbose) message("... done!")
      
    }
    
    # handle column order and names
    # if no layer dimensions
    if(length(layer.string) == 0  || layer.string == "") {
      # make new column order so that data is last
      all.col.names <- names(this.slice.dt)
      all.col.names <- all.col.names[-which(all.col.names == this_var_obj$name)]
      all.col.names <- append(all.col.names, this_var_obj$name)
      setcolorder(this.slice.dt, all.col.names)
      
      # If quantity is a categorical scheme (i.e. more than one entry in the @units slot), convert it to a factor
      if(length(quant@units) > 1) {
        this.slice.dt[,this_var_obj$name := factor(this.slice.dt[[this_var_obj$name]], labels = quant@units)]
      }
      
    }
    # for the case of a layer dimension, rename each layer and move it to the end layers
    else {
      
      for(this_layer_value in all.layer.vals) {
        
        
        # look for attribute with name "<layername>_<value>" or "<value>", on either the data variable or the dimension variable
        final_layer_name <- tryCatch({
          utils::capture.output(invisible(final_layer_name <- lookupAttribute(this.nc, var = layer.string, attname = paste(layer.string, this_layer_value, sep = "_"), verbose = nc.verbose)))
          if(final_layer_name == "Unspecified") invisible(utils::capture.output(final_layer_name <- lookupAttribute(this.nc, var = layer.string,  attname = paste(this_layer_value), verbose = nc.verbose)))
          else if(final_layer_name == "Unspecified")  invisible(utils::capture.output(final_layer_name <- lookupAttribute(this.nc, var = this_var_obj, attname = paste(layer.string, this_layer_value, sep = "_"), verbose = nc.verbose)))
          else if(final_layer_name == "Unspecified") invisible(utils::capture.output(final_layer_name <- lookupAttribute(this.nc, var = this_var_obj,  attname = paste(this_layer_value), verbose = nc.verbose)))
          # if didn't find something, return to the original value 
          else if(final_layer_name == "Unspecified")  final_layer_name <- this_layer_value
          final_layer_name
        }, warning = function(war){
          this_layer_value
        }, error = function(err){
          this_layer_value
        }, finally = {
        })
        
        # set name its final name (if specified, otherwise keep the original)  
        if(final_layer_name == "Unspecified") final_layer_name <- paste0(layer.string, this_layer_value)
        setnames(this.slice.dt, paste(this_layer_value), paste(final_layer_name))
        
        # and move it to the end
        all.col.names <- names(this.slice.dt)
        all.col.names <- all.col.names[-which(all.col.names == final_layer_name)]
        all.col.names <- append(all.col.names, final_layer_name)
        setcolorder(this.slice.dt, all.col.names)
        
        # If quantity is a categorical scheme (i.e. more than one entry in the @units slot), convert it to a factor
        if(length(quant@units) > 1) {
          this.slice.dt[,this_var_obj$name := factor(this.slice.dt[[final_layer_name]], labels = quant@units)]
        }
        
      }  # for each value on the layer dimensions
      
    } # if got a layer dimension
    
    # now join this slice to dt.list
    setKeyDGVM(this.slice.dt)
    dt.list[[length(dt.list)+1]] <- this.slice.dt
    
  }
  
  
  # join all together and set key
  dt <- dt.list[[1]]
  if(length(dt.list) > 1) {
    for(this.dt in dt.list[2:length(dt.list)]) {
      dt <- merge(x = dt, y = this.dt)
    }
  }
  
  # clean up and set key
  rm(dt.list); gc()
  if(verbose) message("Setting key")
  dt <- setKeyDGVM(dt)
  
  # special case for exactly one layer in total, set the name that the Quant ID
  if(length(layers(dt)) == 1) setnames(dt, layers(dt), quant@id)
  
  
  # STInfo
  st.info <- getDimInfo(dt)
  
  
  # Correct lons and lats
  # Note: Year offset has been applied above, this is necessary because it needs to be applied *before* cropping.
  #       Also, if lon and lat cropping is implemented at the stage of reading the NetCDF (for extra efficiency), 
  #       these corrections will also need to be done earlier.
  
  if(verbose) message("Correcting lons and lats with offsets...")
  if(length(source@lonlat.offset) == 2 ){
    if(source@lonlat.offset[1] != 0) dt[, Lon := Lon + source@lonlat.offset[1]]
    if(source@lonlat.offset[2] != 0) dt[, Lat := Lat + source@lonlat.offset[2]]
  }
  
  else if(length(source@lonlat.offset) == 1 ){
    if(source@lonlat.offset[1] != 0) dt[, Lon := Lon + source@lonlat.offset[1]]
    if(source@lonlat.offset[1] != 0) dt[, Lat := Lat + source@lonlat.offset[1]]
  }
  
  if(verbose) {
    message("Offsets applied. Head of full .out file (after offsets):")
    print(utils::head(dt))
  }
  
  
  #### DETERMINE SPATIAL EXTENT AND RELATED METADATA ####
  
  # for the extent, look at the lons and lats originally determined from the NetCDF files
  if(length(all.lons) > 1 && length(all.lats) > 1) {
    lon.diffs <- diff(all.lons)
    lat.diffs <- diff(all.lats)
    if(!length(unique(lon.diffs)) == 1)  warning(paste("Longitude differences in file", file.name.nc, "are not equal, so guessing the overall longitude bounds, which might not be 100% accurate."))
    if(!length(unique(lat.diffs)) == 1)  warning(paste("Latitude differences in file", file.name.nc, "are not equal, so guessing the overall latitude bounds, which might not be 100% accurate."))
    
    # calculate the extent (including London centering) 
    if(source@london.centre && max(all.lons) >= 180)  temp.lons <- LondonCentre(all.lons)   
    else temp.lons <- all.lons
    
    this.extent <- raster::extent(min(temp.lons) - mean(lon.diffs)/2, 
                                  max(temp.lons) + mean(lon.diffs)/2, 
                                  min(all.lats) - mean(lat.diffs)/2,
                                  max(all.lats) + mean(lat.diffs)/2)
    
    sta.info@spatial.extent <- this.extent      
    
  }
  else {
    warning(paste("No spatial extent determinable from netcdf file", file.name.nc,"file. A filler extent will be set."))
    sta.info@spatial.extent <- raster::extent(-1,1,-1,1)
  }
  
  # Look up spatial metadata for things that cannot be determined from the lons and lats 
  sta.info@spatial.extent.id <- lookupAttribute(this.nc, 0, "spatial.extent.id", nc.verbose)
  sta.info@spatial.aggregate.method <- lookupAttribute(this.nc, 0, "spatial.aggregate.method", nc.verbose)
  if(sta.info@spatial.aggregate.method == "Unspecified")  sta.info@spatial.aggregate.method <- "none"
  
  #### PROCESS QUANTITY RELATED METADATA ####
  
  # units
  # if units from the requested Quantity and the NetCDF file don't match
  if(!identical(assumed.units, quant@units)) {
    
    # if we have valid units from the netCDF files *and* the Quantity units are NOT categorical (ie. length > 1)
    # then take the netCDF file units
    if(length(assumed.units) > 0 & length(quant@units) <= 1) {
      warning(paste0("Overriding requested units when reading NetCDF file ", file.name.nc, ". '", paste(quant@units, collapse = ", "), "' was specified but '",  paste(assumed.units, collapse = ", "), "' was found (so using that).\n"))
      quant@units <- assumed.units
    }
    
  }
  
  # standard names (CF standard_name)
  unique.standard_name <- unique(all.standard_names) 
  final.standard_name <- unique(all.standard_names)[1]
  if(length(final.standard_name) > 0 & !identical(tolower(final.standard_name), "unknown")) {
    if(length(unique.standard_name) > 1) warning(paste0("Multiple standard_name found when reading data from NetCDF file ", file.name.nc, ", using the first one (", final.standard_name, ").Overriding requested "))
    if(final.standard_name != quant@standard_name & quant@standard_name != "undefined unit"){
      warning(paste0("Overriding requested standard_name when reading NetCDF file ", file.name.nc, ". '", quant@standard_name, "' was specified but '",final.standard_name, "' was found (so using that).\n"))
      quant@standard_name <- final.standard_name
    }
  }
  
  # long names (CF long_name which is mapped to DGVMTools @name)
  unique.long_name <- unique(all.long_names) 
  final.long_name <- unique(all.long_names)[1]
  if(length(final.long_name) > 0 & !identical(tolower(final.long_name), "unknown")) {
    if(length(unique.long_name) > 1) warning(paste0("Multiple long_name found when reading data from NetCDF file ", file.name.nc, ", using the first one (", final.long_name, ").Overriding requested "))
    if(final.long_name != quant@name & quant@name != "undefined unit"){
      warning(paste0("Overriding requested long_name when reading NetCDF file ", file.name.nc, ". '", quant@name, "' was specified but '",final.long_name, "' was found (so using that).\n"))
      quant@name <- final.long_name
    }
  }
  
  
  
  #### LONDON CENTRE ####
  # if london.centre is requested, make sure all longitudes greater than 180 are shifted to negative
  if(source@london.centre){
    if(max(all.lons) > 180) {
      dt[, Lon := LondonCentre(Lon)]
    }
  }
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  # close the netcdf file and if necessary zip again
  ncdf4::nc_close(this.nc)
  if(zipped) {
    R.utils::gzip(file.name.nc)
  }
  
  # make the ID and then make and return Field
  field.id <- makeFieldID(source = source, var.string = quant@id, sta.info = sta.info)
  message(paste0("Reading of NetCDF file ", file.name.nc, " sucessful!"))
  if(verbose) {
    message("Reading of file took:")
    print(Sys.time() - t1)
  }
  
  new("Field",
      id = field.id,
      data = dt,
      quant = quant,
      source = source,
      sta.info 
      
  )
  
}





######################### LIST ALL LPJ-GUESS OUTPUT VARIABLES (STORED AS *.out FILES) IN AN source DIRECTORY  #####################################################################
#' List all LPJ-GUESS *.out files in a source directory
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "guess.out" and "*.out" 
#' 
#' @param source A \code{Source} containing the meta-data about the NetCDF source
#' @param names A boolean, if TRUE return a character vector of names of available quantities, if FALSE return a list of the actual Quantities.
#' @return A list of all the .out files present, with the ".out" removed. 
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


availableQuantities_NetCDF <- function(source, names){
  
  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")
  
  if(!names) quantities.present <- list()
  else quantities.present <- c()
  for(file in files.present) {
    
    # check if file contains paste(".", source@id, ".nc")
    # and if so, get the part before it
    this.quantity <- "lsls" # TODO
    quantities.present <- append(quantities.present, this.quantity)
    
    
  }
  
  return(quantities.present)
  
}



########################################################
########### NetCDF QUANTITIES ########################
########################################################


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' 
#' 
NetCDF.quantities <- list(
  
  
  new("Quantity",
      id = "fraction",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      format = c("NetCDF"),
      standard_name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      name = "Area Fraction",
      units = "%",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")), 
      format = c("NetCDF"),
      standard_name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "landcover_std",
      name = "Area Fraction",
      units = "%",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")), 
      format = c("NetCDF"),
      standard_name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = function(n) rev(viridis::viridis(n)),
      format = c("NetCDF"),
      standard_name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      units = "1",
      colours = viridis::viridis,
      format = c("NetCDF"),
      standard_name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      units = "kgC/m^2",
      colours = viridis::turbo,
      format = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      units = "kgC m-2 year-1",
      colours = viridis::inferno,
      format = c("NetCDF"),
      standard_name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      units = "kgC/m^2/year",
      colours = viridis::turbo,
      format = c("NetCDF")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = function(n) rev(viridis::magma(n)),
      format = c("NetCDF"),
      standard_name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = function(n) rev(viridis::turbo(n)),
      format = c("NetCDF"),
      standard_name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black")),
      format = c("NetCDF"),
      standard_name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = viridis::turbo,
      format = c("NetCDF"))
  
)


####################################################
########### NetCDF FORMAT ########################
####################################################


#' @description \code{NetCDF} - a Format object defined here for reading NetCDF files.
#' It is essentially CF-compliant netCDF with a couple of extra attributes defined. 
#' 
#' @format A \code{Format} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @export
NetCDF <- new("Format",
              
              # UNIQUE ID
              id = "NetCDF",
              
              # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
              availableQuantities = availableQuantities_NetCDF,
              
              # FUNCTION TO READ A FIELD 
              getField = getField_NetCDF,
              
              # DEFAULT GLOBAL LAYERS  
              predefined.layers = list(),
              
              # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
              quantities = NetCDF.quantities
              
)

