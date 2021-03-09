#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE DGVMData FILES ###########################################################
############################################################################################################################


#' Get a Field for DGVMData
#' 
#' 
#' An internal function that reads data from an DGVMData .nc file.  
#' 
#' @param source A \code{Source} containing the meta-data about the DGVMData source
#' @param quant A Quantity object to specify what quantity should be opened. 
#' @param target.sta.info An STAInfo object defining the spatial-temporal-annual extent over which we want the data
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is automatically generated
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.table containing the data, and secondly the STAInfo for the data that we have
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_DGVMData <- function(source,
                              quant,
                              target.STAInfo,
                              file.name,
                              verbose = FALSE) {
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to read DGVMData files.")
  
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Time = NULL
  
  # get first and last year for use later on
  first.year = target.STAInfo@first.year
  last.year = target.STAInfo@last.year
  
  # function read attributes and warn if they are not present.
  getGlobalAttribute <- function(attr.name, global.attributes) {
    dgvm.attr.name <- paste("DGVMData", attr.name, sep = "_")
    if(dgvm.attr.name %in% names(global.attributes)) {
      return(global.attributes[[dgvm.attr.name]]) 
    }
    else {
      warning(paste0("No ", attr.name,  " attribute (", dgvm.attr.name, " ) found in file ", file.name.nc, ".  This is not DGVMData compliant.  Right now setting attribute to NULL, but this might cause problems later"))
      return(NULL)
    }
  }
  
  
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
  
  # Open file and get global attributes
  if(verbose) message(paste0("Opening file ", file.name.nc))     
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE ))
  global.attributes <- ncdf4::ncatt_get(this.nc, 0, attname=NA, verbose=FALSE)
  
  # Check out dimensions and ignore bnds dimension if present
  dims.present <- names(this.nc$dim)
  if(verbose) message(paste("File has dimensions", paste(dims.present, collapse = ","), sep = " "))
  dims.present <- dims.present[which(dims.present != "bnds")]
  
  all.lats <- numeric(0)
  all.lons <- numeric(0)
  all.time.intervals <- numeric(0)
  lat.string <- ""
  lon.string <- ""
  time.string <- ""
  for(this.dimension in dims.present) {
    
    matched <- FALSE
    
    # pick up Lat
    for(possible.dim.name in c("lat", "Lat", "latitude", "Latitude")) {
      if(this.dimension == possible.dim.name) {
        lat.string <- possible.dim.name
        all.lats <- ncdf4::ncvar_get(this.nc, lat.string) 
        matched <- TRUE
      }
    }
    
    # pick up Lon
    for(possible.dim.name in c("lon", "Lon", "longitude", "Longitude")) {
      if(this.dimension == possible.dim.name) {
        lon.string <- possible.dim.name
        all.lons <-  ncdf4::ncvar_get(this.nc, lon.string) 
        matched <- TRUE
      }
    }
    
    # pick up Time
    for(possible.dim.name in c("time", "Time")) {
      if(this.dimension == possible.dim.name) {
        time.string <- possible.dim.name
        all.time.intervals <- ncdf4::ncvar_get(this.nc, time.string)
        matched <- TRUE
      }
    }                   
    
    # Catch the rest
    if(!matched) {
      stop(paste("Unknown dimension found", this.dimension, "which I don't know what to do with."))
    }
    
  }
  
  
  #### LON AXIS AND LAT AXIS
  
  # default values if no longitude or latitude selection is required
  start.lon <- 1
  count.lon <- -1
  start.lat <- 1
  count.lat <- -1
  
  # TODO potential lon/lat cropping here
  
  
  #### TIME AXIS - calculate the time labels and the start and count values for the time axis
  
  # first look up year-related attributes
  first.year.present  <- getGlobalAttribute("first.year", global.attributes)
  last.year.present  <- getGlobalAttribute("last.year", global.attributes)
  year.aggregate.method <- getGlobalAttribute("year.aggregate.method", global.attributes)
  
  # next let's determine some details about the time axis
  if(length(all.time.intervals) > 0) { 
    
    # extract the start data from the time units string
    time.units.attr <- ncdf4::ncatt_get(this.nc, time.string, "units")
    time.units.string.list <- strsplit(time.units.attr$value, " ")
    tmp.unit <- unlist(time.units.string.list[1]) # Isolate the unit .i.e. seconds, hours, days etc.
    timestep.unit <- tmp.unit[which(tmp.unit %in% c("seconds","hours","days"))[1]] # Check unit
    if(timestep.unit != "days") stop('Currently DGVMData only accepts data with a units of time axis = "days"')
    
    # extract the start day, month and year
    start.date <- as.POSIXct(gsub(paste(timestep.unit,'since '),'',time.units.attr$value),format="%Y-%m-%d %H:%M:%S") ## Extract the start / origin date
    start.date.year <- as.numeric(format(start.date,"%Y"))
    start.date.month <- as.numeric(format(start.date,"%m"))
    start.date.day <- as.numeric(format(start.date,"%d"))
    
    # look at the time steps
    mean.timestep.differences <- mean(diff(all.time.intervals))
    # if the mean is exactly unity then the data is daily 
    if(mean.timestep.differences == 1 ) time.res <- "Day"
    # if the mean is between 28 and 31 (inclusive) assume monthly   
    else if(mean.timestep.differences >= 28 & mean.timestep.differences <= 31 ) time.res <- "Month"
    # if the mean is between 359 and 367 (inclusive) assume yearly  
    else if(mean.timestep.differences >= 360 & mean.timestep.differences <= 367 ) time.res <- "Year"       
    else stop("Data doesn't appear to be daily, monthly, or yearly.  Other options are not currently supported by the DGVMData Format, but could potentially be.  So if you need this please contact the author.")
    
    
    # default values if no time selection is required
    start.time <- 1
    count.time <- -1
    
    if(time.res == "Year") {
      
      # convert the netcdf time increments into years
      end.date.year <- start.date.year+length(all.time.intervals)-1
      years.vector <- start.date.year:end.date.year
      
      # get the first and last indices based on the years requested
      # returns a two-element list, with elements "first" and "last"
      first.last.indices <- calculateYearCroppingIndices(target.STAInfo, years.vector)
      
      # make the index/count for reading the netCDF files
      start.time <- first.last.indices$first
      count.time <- first.last.indices$last - first.last.indices$first + 1
      
      # crop the vectors to match 
      years.vector <- years.vector[first.last.indices$first:first.last.indices$last]
      
      # make a numeric code of Year
      all.times <- paste(years.vector)
      
    }
    
    else if(time.res == "Month") {
      
      # convert the netcdf time increments into years and months
      
      # first make a vector of the years and months
      nyears.to.cover <- ceiling(length(all.time.intervals) / 12)
      years.to.cover <- 0:(nyears.to.cover-1) + start.date.year
      years.vector <- rep(years.to.cover, each = 12)
      months.vector <- rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), times = nyears.to.cover)
      
      # here crop to match the actual length of the data (in case of not complete years)
      years.vector <- years.vector[start.date.month:length(all.time.intervals)]
      months.vector <- months.vector[start.date.month:length(all.time.intervals)]
    
      # get the first and last indices based on the years requested
      # returns a two-element list, with elements "first" and "last"
      first.last.indices <- calculateYearCroppingIndices(target.STAInfo, years.vector)

      # make the index/count for reading the netCDF files
      start.time <- first.last.indices$first
      count.time <- first.last.indices$last - first.last.indices$first + 1
       
      # crop the vectors to match 
      years.vector <- years.vector[first.last.indices$first:first.last.indices$last]
      months.vector <- months.vector[first.last.indices$first:first.last.indices$last]
      
       # make a numeric code of Year.Month for labelling this dimension
      all.times  <- paste(years.vector, months.vector, sep = ".")
      
    }
    
    else if(time.res == "Day") {
      
      
      # lookup the calandar attribute and determine if it is proleptic
      calendar <- ncdf4::ncatt_get(this.nc, time.string, "calendar")
      if(calendar$hasatt) calendar <- calendar$value
      else stop("DGVMData format requires a 'calendar' attribute on the time axis for daily data")
      proleptic <- FALSE
      if(calendar == "proleptic_gregorian")  proleptic <- TRUE
      
      # make some strings vectors for the days in years of different lengths 
      # (these are converted to codes "Year.Day" codes later on, hence the use of zero-padded strings)
      days.365 <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:365)) 
      days.366 <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:366)) 
      days.360 <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:360)) 
      
      # take truncof the times to give integer values 
      # (the fraction, if present, is assumed to be the time of day of the measurement, ie a fraction of a day but this can be removed
      # since a few hours into a day is still the same day
      all.time.intervals <- trunc(all.time.intervals)

      # make a big vector of the years and days from the start date (defined by the netCDF time axis) to the beyond the end of the data
      years.vector <- c()
      days.vector<- c()
      day.counter <- start.date.day
      year <- start.date.year
      while(day.counter < all.time.intervals[length(all.time.intervals)]) {
        
        # select year length based on the calendar
        # 365 day
        if(calendar == "365_day" | (calendar == "standard" & !is.leapyear(year)) | (calendar == "proleptic_gregorian" & !is.leapyear(year, proleptic = TRUE))) {
          this.year.days <- days.365
        }
        # 366 day
        else if(calendar == "366_day" | (calendar == "standard" & is.leapyear(year)) | (calendar == "proleptic_gregorian" & is.leapyear(year, proleptic = TRUE))) {
          this.year.days <- days.366
        }
        # 360 day
        else if(calendar == "360_day") {
          this.year.days <- days.360
        }
        else{
          stop(paste0("Calendar", calendar, "not supported by DGVMData format in DGVMTools package"))
        }
        
        # append to the vectors
        years.vector <- append(years.vector, rep(year, times = length(this.year.days)))
        days.vector <- append(days.vector, this.year.days)
        
        # add to the counters
        day.counter <- day.counter + length(this.year.days)
        year <- year + 1
        
      }
      
 
      # now select values from the massive vector using the all.time.intervals as the indices
      #  -note that we need to add one to all the time intervals since first time step (from start date) will have an interval of zero 
      #   but for that time step we need to have an array index of 1
      years.vector <- years.vector[all.time.intervals+1]
      days.vector <- days.vector[all.time.intervals+1]
      print(days.vector)
      print(years.vector)
      
      years.days.vector <- makeYearsAndDaysFromDailyTimeAxis(this.nc, time.string, start.date)

        print(years.days.vector)
      

      # get the first and last indices based on the years requested
      # returns a two-element list, with elements "first" and "last"
      first.last.indices <- calculateYearCroppingIndices(target.STAInfo, years.vector)
      
      # make the index/count for reading the netCDF files
      start.time <- first.last.indices$first
      count.time <- first.last.indices$last - first.last.indices$first + 1
      
      # crop the vectors to match 
      years.vector <- years.vector[first.last.indices$first:first.last.indices$last]
      days.vector <- days.vector[first.last.indices$first:first.last.indices$last]
      
      # make a numeric code of Year.Day
      all.times  <- paste(years.vector, days.vector, sep = ".")
      
    }  # end if-else statement for different time resolutions
    
    
    
    
    # set year and subannual meta-data - note that years.vector (ie. the final years of data that will be selected) must be defined above
    sta.info@first.year <- years.vector[1]
    sta.info@last.year <- years.vector[length(years.vector)]
    sta.info@subannual.resolution <- time.res
    subannual.original.attr <- ncdf4::ncatt_get(nc = this.nc, varid = 0, attname = "DGVMData_subannual.original")
    if(subannual.original.attr$hasatt)  sta.info@subannual.original <- subannual.original.attr$value
    else sta.info@subannual.original <- time.res
    
    
  }
  
  
  
  
  
  #### RETRIEVE THE DATA
  
  # list of data.tables, one for each netCDF var (or 'layer' in DGVMTools terms)
  dt.list <- list()
  
  # set up the dimension list for naming the dimensions
  dimension.names <- list()
  if(length(all.lons) > 0) dimension.names[["Lon"]] <- all.lons
  if(length(all.lats) > 0) dimension.names[["Lat"]] <- all.lats
  if(length(all.time.intervals) > 0) dimension.names[["Time"]] <- all.times
  
  for(this.var in this.nc$var) {
    
    # ignore the "Time_bands"/time_bnds" variable that CDO creates with time aggregation
    if(tolower(this.var$name) != "time_bnds") {
      
      # set start and count appropriately
      start <- numeric()
      count <- numeric()
      
      for(this.dim in this.var$dim) {
        
        if(this.dim$name == lat.string) {
          start <- append(start, start.lat)
          count <- append(count, count.lat)
        }
        else if(this.dim$name  == lon.string) {
          start <- append(start, start.lon)
          count <- append(count, count.lon)
        }
        else if(this.dim$name  == time.string) {
          start <- append(start, start.time)
          count <- append(count, count.time)
        }
        
      }
      
      # Get the actual data and set the dimension names    
      this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = start, count = count, verbose = verbose, collapse_degen=FALSE)
      dimnames(this.slice) <- dimension.names
      
      # prepare data.table from the slice (array)
      this.slice.dt <- as.data.table(reshape2::melt(this.slice))
      rm(this.slice)
      gc()
      this.slice.dt <- stats::na.omit(this.slice.dt)
      setnames(this.slice.dt, "value", this.var$name)
      
      
      
      # ###  HANDLE TIME AXIS 
      if(length(all.time.intervals) > 0) {
        
        if(time.res == "Year") {
          this.slice.dt[, Year := as.numeric(Time)]
          this.slice.dt[, Time := NULL]
        }
        else if(time.res == "Month") {
          toYearandMonth <- function(x) {
            Year <- as.integer(trunc(x))
            return(list("Year" = Year, "Month" = as.integer(round((x-Year)*100))) )
          }
          this.slice.dt[, c("Year", "Month") := toYearandMonth(as.numeric(Time))]
          this.slice.dt[, Time := NULL]
          
        }
        else if(time.res == "Day") {
          toYearandDay <- function(x) {
            Year <- as.integer(trunc(x))
            return(list("Year" = Year, "Day" = as.integer(round((x-Year)*1000))) )
          }
          this.slice.dt[, c("Year", "Day") := toYearandDay(as.numeric(Time))]
          this.slice.dt[, Time := NULL]
          
        }
        
        # make new column order so that the quant is last
        all.names <- names(this.slice.dt)
        all.names <- all.names[-which(all.names == this.var$name)]
        all.names <- append(all.names, this.var$name)
        setcolorder(this.slice.dt, all.names)
        
      }
      
      setKeyDGVM(this.slice.dt)
      
      
      if(length(quant@units) > 1) {
        
        # # remove categories which aren't present
        categories.present <- unique(this.slice.dt[[this.var$name]])
        all.categories <- quant@units
        
        this.slice.dt[,this.var$name := factor(this.slice.dt[[this.var$name]], labels = all.categories[sort(categories.present)])]
      }
      
      # now join this to all.dt
      dt.list[[length(dt.list)+1]] <- this.slice.dt
      
      
    } # end if statement for ignoring "time_bnds" variable
    
  }
  
  
  # join all together and set key
  dt <- dt.list[[1]]
  
  
  if(length(dt.list) > 1) {
    for(this.dt in dt.list[2:length(dt.list)]) {
      dt <- merge(x = dt, y = this.dt)
    }
  }
  
  rm(dt.list)
  gc()
  
  if(verbose) message("Setting key")
  dt <- setKeyDGVM(dt)
  
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
  
  
  ### DETERMINE SPATIAL EXTENT 
  
  # simple ones
  spatial.extent.id  <- getGlobalAttribute("spatial.extent.id", global.attributes)
  if(!is.null(spatial.extent.id)) { sta.info@spatial.extent.id <- spatial.extent.id  }
  else { sta.info@spatial.extent.id <- "Full"  }
  spatial.aggregate.method  <- getGlobalAttribute("spatial.aggregate.method", global.attributes)
  if(!is.null(spatial.aggregate.method)) sta.info@spatial.aggregate.method <- spatial.aggregate.method
  else { sta.info@spatial.aggregate.method <- "none"  }
  
  # first attempt to use the attributes from the NetCDF file
  xmin  <- getGlobalAttribute("xmin", global.attributes)
  xmax  <- getGlobalAttribute("xmax", global.attributes)
  ymin  <- getGlobalAttribute("ymin", global.attributes)
  ymax  <- getGlobalAttribute("ymax", global.attributes)
  # else attempt to lookm of from Lon and Lat columns
  if(!is.null(xmin) & !is.null(xmax) & !is.null(ymin) & !is.null(ymax)){
    sta.info@spatial.extent <- raster::extent(xmin, xmax, ymin,ymax)
    if(verbose) message("Setting spatial extent from DGVMData attributes")
  }
  else if("Lon" %in% st.info && "Lat" %in% st.info) {
    sta.info@spatial.extent <- extent(dt)
    if(verbose) message("Setting spatial extent from Lon and Lat dimensions")
  }
  else{
    if(verbose) message(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    warning(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    sta.info@spatial.extent <- numeric(0)
  }
  
  
  # if london.centre is requested, make sure all longitudes greater than 180 are shifted to negative
  if(source@london.centre){
    if(max(all.lons) > 180) {
      dt[, Lon := LondonCentre(Lon)]
    }
  }
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs, complete list and return
  dt <- stats::na.omit(dt)
  
  # close the netcdf file and if necessary zip again
  ncdf4::nc_close(this.nc)
  if(zipped) {
    R.utils::gzip(file.name.nc)
  }
  
  # make the ID and then make and return Field
  field.id <- makeFieldID(source = source, var.string = quant@id, sta.info = sta.info)
  
  return(
    
    new("Field",
        id = field.id,
        data = dt,
        quant = quant,
        source = source,
        sta.info 
    )
    
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


availableQuantities_DGVMData <- function(source, names){
  
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
DGVMData.quantities <- list(
  
  
  new("Quantity",
      id = "fraction",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      format = c("DGVMData"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("DGVMData"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "landcover_std",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("DGVMData"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("DGVMData"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      units = "1",
      colours = viridis::viridis,
      format = c("DGVMData"),
      cf.name = "leaf_area_index"),
  
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
      format = c("DGVMData"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      format = c("DGVMData")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("DGVMData"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("DGVMData"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = veg.palette,
      format = c("DGVMData"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = veg.palette,
      format = c("DGVMData"))
  
)


####################################################
########### DGVMDATA FORMAT ########################
####################################################


#' @description \code{DGVMData} - a format defined here to unify the multitude of different datasets into a common format. 
#' It is essentially CF-compliant netCDF with a couple of extra attributes defined. 
#' Can be produced using the companion DGVMData package. 
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @include colour-palettes.R
#' @export
DGVMData <- new("Format",
                
                # UNIQUE ID
                id = "DGVMData",
                
                # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                availableQuantities = availableQuantities_DGVMData,
                
                # FUNCTION TO READ A FIELD 
                getField = getField_DGVMData,
                
                # DEFAULT GLOBAL LAYERS  
                predefined.layers = list(),
                
                # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
                quantities = DGVMData.quantities
                
)

