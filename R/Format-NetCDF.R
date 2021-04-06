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
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.table containing the data, and secondly the STAInfo for the data that we have
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_NetCDF <- function(source,
                            quant,
                            target.STAInfo,
                            file.name,
                            verbose = FALSE) {
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to read NetCDF files.")
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Day = Time = Temp =NULL
  
  # Variables to ignore in any netCDF file
  # TODO - the time_bnds information could of course be used
  vars.to.ignore <- c("time_bnds")
  
  # handy function to check for attributes, also including the kind of deprecated "DGVMTools_" and "DGVMData_" variants for global attributes
  lookupAttribute <- function(nc, var, attname, verbose = FALSE) {
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
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE ))
  
  #### EXAMINE VARIABLES PRESENT IN FILE ####
  
  # Build a vector of variables which have dimensions associated with them and which aren't flagged as dimension variables
  all_vars_names <- c()
  for(this_var in this.nc$var){
    if(this_var$ndims > 0 && !this_var$id$isdimvar) all_vars_names <- append(all_vars_names, this_var$name)
  }
  
  if(verbose) message(paste("* NetCDF file contains useful variables:", paste(all_vars_names, collapse = ", ")))
  vars.to.read <- list()
  if(quant@id %in% all_vars_names) {
    vars.to.read <- quant@id
    if(verbose) message(paste0("* Reading variable ", vars.to.read, ", which matches requested quant ID"))
  }
  else {
    vars.to.read <- all_vars_names[!all_vars_names %in% vars.to.ignore]
    if(verbose) message(paste0("* No variable in netCDF file matches requested quant ID (", quant@id, ") so reading *all* netCDF variables that look suitable: ", paste(vars.to.read, collapse = ", ")))
  }
  
  # Check out dimensions and ignore bnds dimension if present
  dims.present <- names(this.nc$dim)
  if(verbose) message(paste("* Dimensions present in file:", paste(dims.present, collapse = ","), sep = " "))
  dims.present <- dims.present[which(dims.present != "bnds")]
  
  all.lats <- numeric(0)
  all.lons <- numeric(0)
  all.time.intervals <- numeric(0)
  all.layer.vals <- numeric(0)
  lat.string <- ""
  lon.string <- ""
  time.string <- ""
  layer.string <- ""
  for(this.dimension in dims.present) {
    
    matched <- FALSE
    
    # pick up Lat
    for(possible.dim.name in c("lat", "Lat", "latitude", "Latitude")) {
      if(this.dimension == possible.dim.name) {
        lat.string <- possible.dim.name
        all.lats <- ncdf4::ncvar_get(this.nc, lat.string) 
        matched <- TRUE
        if(verbose) message(paste0("** Confirmed latitude dimension: '", lat.string, "', with ", length(all.lats), " values."))
      }
    }
    
    # pick up Lon
    for(possible.dim.name in c("lon", "Lon", "longitude", "Longitude")) {
      if(this.dimension == possible.dim.name) {
        lon.string <- possible.dim.name
        all.lons <-  ncdf4::ncvar_get(this.nc, lon.string) 
        matched <- TRUE
        if(verbose) message(paste0("** Confirmed longitude dimension: '", lon.string, "', with ", length(all.lons), " values."))
        
      }
    }
    
    # pick up Time
    for(possible.dim.name in c("time", "Time", "year", "Year", "month", "Month", "t", "T")) {
      if(this.dimension == possible.dim.name) {
        time.string <- possible.dim.name
        all.time.intervals <- ncdf4::ncvar_get(this.nc, time.string)
        matched <- TRUE
        if(verbose) message(paste0("** Confirmed time dimension: '", time.string, "', with ", length(all.time.intervals), " values."))
      }
    }                   
    
    # pick up Layers
    for(possible.dim.name in c("pft", "PFT", "PFTs", "PFTS", "pfts", "vegtype", "vegtypes", "layer", "layers", "z", "Z")) {
      if(this.dimension == possible.dim.name) {
        layer.string <- possible.dim.name
        all.layer.vals <- ncdf4::ncvar_get(this.nc, layer.string)
        matched <- TRUE
        if(verbose) message(paste0("** Confirmed layer-type dimension: '", layer.string, "', with ", length(all.layer.vals), " values."))
      }
    }     
    
    # Catch the rest
    if(!matched) {
      stop(paste("Unknown dimension found", this.dimension, "which I don't know what to do with."))
    }
    
  }
  
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
    
    
    # extract the details about the time axis, if it the units attributes exists
    relative.time.axis <- FALSE
    time.units.attr <- ncdf4::ncatt_get(this.nc, time.string, "units")
    if(time.units.attr$hasatt) {
      time.units.string.vec <- unlist(strsplit(time.units.attr$value, " "))
      timestep.unit <- time.units.string.vec[1]
      if(length(time.units.string.vec) > 1) {
        if(tolower(time.units.string.vec[2]) == "since"  || tolower(time.units.string.vec[2]) == "after") relative.time.axis <- TRUE
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
    
    if(relative.time.axis) {
      
      #if(timestep.unit != "days") stop('When reading relative time axes, DGVMTools only accepts units = "days since ..."')
      
      # extract the start day, month and year and return it as custom date type (vector with $year, $month and $day)
      # the reason for using this custom format is to support years < 0 and > 9999
      start.date <- parseStartDate(time.units.attr$value)
      
      # if time step unit is days
      if(timestep.unit == "days") {
        
        # look at the time steps
        mean.timestep.differences <- mean(diff(all.time.intervals))
        # if the mean is exactly unity then the data is daily 
        if(mean.timestep.differences == 1 ) time.res <- "Day"
        # if the mean is between 28 and 31 (inclusive) assume monthly   
        else if(mean.timestep.differences >= 28 & mean.timestep.differences <= 31 ) time.res <- "Month"
        # if the mean is between 359 and 367 (inclusive) assume yearly  
        else if(mean.timestep.differences >= 360 & mean.timestep.differences <= 367 ) time.res <- "Year"       
        else stop("Data doesn't appear to be on daily, monthly, or yearly timesteps.  Other options are not currently supported by the DGVMTools, but could potentially be.  So if you need this please contact the author.")
        
        
        # default values if no time selection is required
        start.time <- 1
        count.time <- -1
        
        if(time.res == "Year") {
          
          # This function call processes a time axis which is assumed to be yearly
          # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
          # and the actual years corresponding to this selection.
          year.axis.info <- processYearlyNCAxis(axis.values = all.time.intervals, 
                                                start.year = start.date["year"], 
                                                target.STAInfo = target.STAInfo)  
          start.time <- year.axis.info$start.time
          count.time <- year.axis.info$count.time
          years.vector <- year.axis.info$years.vector 
          
          # make a numeric code of the years for labelling this dimension once it is read
          all.times <- paste(years.vector)
          
        }
        
        else if(time.res == "Month") {
          
          # This function call processes a time axis which is assumed to be monthly
          # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
          # and the actual years and months corresponding to this selection.
          month.axis.info <- processMonthlyNCAxis(axis.values = all.time.intervals, 
                                                  start.year = start.date["year"], 
                                                  start.month = start.date["month"],
                                                  target.STAInfo = target.STAInfo)  
          start.time <- month.axis.info$start.time
          count.time <- month.axis.info$count.time
          years.vector <- month.axis.info$years.vector 
          months.vector <- month.axis.info$months.vector 
          
          # make the vector of all times as a code "year.month" for labelling this dimension once it is read
          all.times  <- paste(years.vector, months.vector, sep = ".")
          
        }
        
        else if(time.res == "Day") {
          
          # First, lookup the calandar attribute and determine if it is proleptic
          calendar <- ncdf4::ncatt_get(this.nc, time.string, "calendar")
          if(calendar$hasatt) calendar <- calendar$value
          else stop("DGVMTools requires a 'calendar' attribute on the time axis for daily netCDF data")
          
          
          # This function call processes a time axis which is assumed to be daily
          # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
          # and the actual years and days corresponding to this selection.
          daily.axis.info <- processDailyNCAxis(axis.values = all.time.intervals, 
                                                start.year = start.date["year"], 
                                                start.day = start.date["day"], 
                                                target.STAInfo = target.STAInfo,
                                                calendar = calendar)  
          start.time <- daily.axis.info$start.time
          count.time <- daily.axis.info$count.time
          years.vector <- daily.axis.info$years.vector 
          days.vector <- daily.axis.info$days.vector 
          
          
          # make a numeric code of Year.Day
          all.times  <- paste(years.vector, days.vector, sep = ".")
          
        }  # end if-else statement for different time resolutions
        
      } # end if time step is days
      
      # if time step unit is month
      else if(timestep.unit == "months") {
        
        time.res <- "Month"
        
        # first check that we have all integer months, nothing too crazy
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) stop("For an relative time axis with unit of months, only integer values are supported.")
        
        
        # This function call processes a time axis which is assumed to be monthly
        # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
        # and the actual years and months corresponding to this selection.
        month.axis.info <- processMonthlyNCAxis(axis.values = all.time.intervals, 
                                                start.year = start.date["year"], 
                                                start.month = start.date["month"],
                                                target.STAInfo = target.STAInfo)  
        start.time <- month.axis.info$start.time
        count.time <- month.axis.info$count.time
        years.vector <- month.axis.info$years.vector 
        months.vector <- month.axis.info$months.vector 
        
        # make the vector of all times as a code "year.month" for labelling this dimension once it is read
        all.times  <- paste(years.vector, months.vector, sep = ".")
        
      }
      else if(timestep.unit == "years") {
        
        time.res <- "Year"
        
        # first check that we have all integer years, nothing too crazy
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) stop("For an relative time axis with unit of years, only integer values are supported.")
        
        
        # This function call processes a time axis which is assumed to be yearly
        # It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
        # and the actual years and months corresponding to this selection.
        year.axis.info <- processYearlyNCAxis(axis.values = all.time.intervals, 
                                                start.year = start.date["year"], 
                                                target.STAInfo = target.STAInfo)  
        start.time <- year.axis.info$start.time
        count.time <- year.axis.info$count.time
        years.vector <- year.axis.info$years.vector 

        # make the vector of all years as character vectors for labelling this dimension once it is read
        all.times  <- paste(years.vector)
        
      }
      # else catch whatever else
      else stop('When reading relative time axes, DGVMTools only accepts units = "days since ..."/"months since ..."/"years since ...".')
      
    } # end if relative time axis
    
    # else an absolute time axis
    else {
      
      # timestep unit "year(s)", which is pretty easy
      if(tolower(timestep.unit) == "year" || tolower(timestep.unit) == "years") {
        time.res <- "Year"
        # check they are all integer values (ie no confusing fractional year)
        if(!isTRUE(all(all.time.intervals == floor(all.time.intervals)))) stop("For an absolute time axis, only integer values of years are supported")
        
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
                                                target.STAInfo = target.STAInfo)  
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
    sta.info@subannual.aggregate.method <- lookupAttribute(this.nc, 0, "subannual.aggregate.method", verbose)
    
    sta.info@subannual.original <- lookupAttribute(this.nc, 0, "subannual.original", verbose)
    if(sta.info@subannual.original == "none") sta.info@subannual.original <- time.res
    
  }
  
  
  
  #### RETRIEVE THE DATA
  
  # list of data.tables, one for each netCDF var (which will be interpreted as a 'Layer' in DGVMTools terms)
  dt.list <- list()
  
  # keep track of all the units, long names and standard names for later
  all.units <- c()
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
        if(verbose) message(paste("* Reading data for", count.lat, "latitude values ('-1' means reading the full range available"))
      }
      else if(this.dim$name  == lon.string) {
        start <- append(start, start.lon)
        count <- append(count, count.lon)
        if(length(all.lons) > 0) dimension.names[["Lon"]] <- all.lons
        if(verbose) message(paste("* Reading data for", count.lon, "longitude values ('-1' means reading the full range available"))
      }
      else if(this.dim$name  == time.string) {
        start <- append(start, start.time)
        count <- append(count, count.time)
        if(length(all.time.intervals) > 0) dimension.names[["Time"]] <- all.times
        if(verbose) message(paste("* Reading data for", count.time, "time values ('-1' means reading the full range available"))
      }
      else if(this.dim$name  == layer.string) {
        start <- append(start, 1)
        count <- append(count, -1)
        if(length(all.layer.vals) > 0) dimension.names[["Layers"]] <- all.layer.vals
        if(verbose) message(paste("* Reading data for", -1, "layer values ('-1' means reading the full range available"))
      }
      
    }
    
    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this_var_obj, start = start, count = count, verbose = verbose, collapse_degen=FALSE)
    if(verbose) message(paste0("Read slice of netCDF file with dimensions: ", paste(dim(this.slice), collapse = ", ")))
    if(verbose) message(paste("Setting dimension names: ", paste(names(dimension.names), collapse = ", ")))
    dimnames(this.slice) <- dimension.names
    if(ncdf4::ncatt_get(this.nc, this_var_obj, "units")$hasatt) all.units <- append(all.units,  ncdf4::ncatt_get(this.nc, this_var_obj, "units")$value)
    if(ncdf4::ncatt_get(this.nc, this_var_obj, "standard_name")$hasatt) all.standard_names <- append(all.standard_names,  ncdf4::ncatt_get(this.nc, this_var_obj, "standard_name")$value)
    if(ncdf4::ncatt_get(this.nc, this_var_obj, "long_name")$hasatt) all.long_names <- append(all.long_names,  ncdf4::ncatt_get(this.nc, this_var_obj, "long_name")$value)
    
    
    # if only a 2-D 'matrix', convert to a 3-D 'array' to that it melts properly with the as.data.table command below
    if(length(dim(this.slice)) < 3) {
      dim(this.slice) <- c(dim(this.slice), 1)
      dimension.names[["Temp"]] <- 1
      dimnames(this.slice) <- dimension.names
    }
    
    # 'melt' and remove the Temp column if necessary 
    this.slice.dt <- as.data.table(this.slice, na.rm = TRUE, keep.rownames = TRUE)
    if("Temp" %in% names(this.slice.dt)) this.slice.dt[ , Temp := NULL]
    if("Lon" %in% names(this.slice.dt)) this.slice.dt[ , Lon := as.numeric(Lon)]
    if("Lat" %in% names(this.slice.dt)) this.slice.dt[ , Lat := as.numeric(Lat)]
    if("Time" %in% names(this.slice.dt)) this.slice.dt[ , Time := as.numeric(Time)]
    
    
    # clear up 
    rm(this.slice)
    gc()
    
    # if necessary dcast the Layers column back to make separate columns for each layer
    if("Layers" %in% names(this.slice.dt)) this.slice.dt <- dcast(this.slice.dt, ... ~ Layers, value.var = "value")
    
    this.slice.dt <- stats::na.omit(this.slice.dt)
    if("value" %in% names(this.slice.dt)) setnames(this.slice.dt, "value", this_var_obj$name)
    
    
    # ###  HANDLE TIME AXIS 
    if(length(all.time.intervals) > 0) {
      
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
        
        ## SPECIAL CASE FOR MONTHLY CLIMATOLGIES - delete Year column if it has only one value and months are 1:12
        ## and set metadata
        if(length(unique(this.slice.dt[["Year"]])) ==1 && all(sort(unique(this.slice.dt[["Month"]])) == (1:12)))  {
          this.slice.dt[, Year := NULL]
          sta.info@year.aggregate.method <- "Climatology"
        }
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
        final_layer_name <- lookupAttribute(this.nc, var = this_var_obj, attname = paste(layer.string, this_layer_value, sep = "_"), verbose = verbose)
        if(final_layer_name == "Unspecified") final_layer_name <- lookupAttribute(this.nc, var = this_var_obj,  attname = paste(this_layer_value), verbose = verbose)
        if(final_layer_name == "Unspecified")  final_layer_name <- lookupAttribute(this.nc, var =layer.string, attname = paste(layer.string, this_layer_value, sep = "_"), verbose = verbose)
        if(final_layer_name == "Unspecified") final_layer_name <- lookupAttribute(this.nc, var = layer.string,  attname = paste(this_layer_value), verbose = verbose)
        
        # if didn't find something, return to the original value 
        if(final_layer_name == "Unspecified")  final_layer_name <- this_layer_value
        
        # If reading multiple variables from the NetCDF file, prepend the variable name to the layer name 
        if(length(vars.to.read) > 1) final_layer_name <- paste(this.var, final_layer_name, sep = "_")
        
        # set name its final name    
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
  
  
  # Correct year, lons and lats
  if(verbose) message("Correcting years, lons and lats with offsets...")
  if(source@year.offset != 0 && "Year" %in% st.info) dt[,Year := Year + source@year.offset]
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
  sta.info@spatial.extent.id <- lookupAttribute(this.nc, 0, "spatial.extent.id", verbose)
  sta.info@spatial.aggregate.method <- lookupAttribute(this.nc, 0, "spatial.aggregate.method", verbose)
  if(sta.info@spatial.aggregate.method == "Unspecified")  sta.info@spatial.aggregate.method <- "none"
  
  #### PROCESS QUANTITY RELATED METADATA ####
  
  # units
  unique.units <- unique(all.units) 
  final.units <- unique(all.units)[1]
  if(length(final.units) > 0) {
    if(length(unique.units) > 1) warning(paste0("Multiple units found when reading data from NetCDF file ", file.name.nc, ", using the first one (", final.units, ")."))
    if(final.units != quant@units & quant@units != "undefined unit"){
      warning(paste0("Overriding requested units when reading NetCDF file ", file.name.nc, ". ", quant@units, " was specified but ", final.units, " was found (so using that)."))
      quant@units <- final.units
    }
  }
  
  # standard names (CF standard_name)
  unique.standard_name <- unique(all.standard_names) 
  final.standard_name <- unique(all.standard_names)[1]
  if(length(final.standard_name) > 0) {
    if(length(unique.standard_name) > 1) warning(paste0("Multiple standard_name found when reading data from NetCDF file ", file.name.nc, ", using the first one (", final.standard_name, ")."))
    if(final.standard_name != quant@standard_name & quant@standard_name != "undefined unit"){
      warning(paste0("Overriding requested standard_name when reading NetCDF file ", file.name.nc, ". ", quant@standard_name, " was specified but ",final.standard_name, " was found (so using that)."))
      quant@standard_name <- final.standard_name
    }
  }
  
  # long names (CF long_name which is mapped to DGVMTools @name)
  unique.long_name <- unique(all.long_names) 
  final.long_name <- unique(all.long_names)[1]
  if(length(final.long_name) > 0) {
    if(length(unique.long_name) > 1) warning(paste0("Multiple long_name found when reading data from NetCDF file ", file.name.nc, ", using the first one (", final.long_name, ")."))
    if(final.long_name != quant@name & quant@name != "undefined unit"){
      warning(paste0("Overriding requested long_name when reading NetCDF file ", file.name.nc, ". ", quant@name, " was specified but ",final.long_name, " was found (so using that)."))
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
#' @param source A \code{Source} containing the meta-data about the DGVMData source
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
########### DGVMData QUANTITIES ########################
########################################################


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' @include colour-palettes.R
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
      colours = veg.palette, 
      format = c("NetCDF"),
      standard_name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "landcover_std",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("NetCDF"),
      standard_name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = reversed.viridis,
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
      colours = fields::tim.colors,
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
      colours = fields::tim.colors,
      format = c("NetCDF")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("NetCDF"),
      standard_name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("NetCDF"),
      standard_name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = veg.palette,
      format = c("NetCDF"),
      standard_name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = veg.palette,
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
#' @include colour-palettes.R
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

