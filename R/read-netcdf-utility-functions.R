
calculateYearCroppingIndices <- function(target_STAInfo, years_vector) {
  
  # if both target first and last year are defined
  if(length(target_STAInfo@first.year) > 0 & length(target_STAInfo@last.year) > 0) {
    
    # extract the first and last target years 
    target.first.year <- target_STAInfo@first.year
    target.last.year <-  target_STAInfo@last.year
    
    # if first and last years don't already match the beginning and end of the dataset
    if(target.first.year != years_vector[1] | target.last.year != years_vector[length(years_vector)]) {
      
      
      # if first or last year is outisde the dataset then file
      if(target.first.year < years_vector[1]) stop(paste("In DGVMData requested first.year = ", target.first.year, ", but first year in data is", years_vector[1], sep = " "))
      if(target.last.year > years_vector[length(years_vector)]) stop(paste("In DGVMData requested last.year = ", target.last.year, ", but last year in data is", years_vector[length(years_vector)], sep = " "))
      
      # find the first occurence of the target first.year and the last occurance of the target year in the year.vector
      first.index <- match(target.first.year, years_vector)
      last.index <-  match(target.last.year, rev(years_vector))
      last.index <- length(years_vector) - last.index +1
      
      # return these indices
      return(list(first = first.index, last = last.index))
      
    }
    
  }
  
  # otherwise return the full range
  return(list(first = 1, last = length(years_vector)))
  
}
#' makeYearsAndDaysFromDailyTimeAxis 
#' Currently not used, see function processDailyNCAxis in this file
#' CAN PROBABLY BE DELETED
#' @keywords internal
makeYearsAndDaysFromDailyTimeAxis <- function(nc_file, time_axis_string, start.date) {
  
  # take truncof the times to give integer values 
  # (the fraction, if present, is assumed to be the time of day of the measurement, ie a fraction of a day but this can be removed
  # since a few hours into a day is still the same day
  all.time.intervals <- trunc(ncdf4::ncvar_get(nc_file, time_axis_string))

  # lookup the calandar attribute and determine if it is proleptic
  calendar <- ncdf4::ncatt_get(nc_file, time_axis_string, "calendar")
  if(calendar$hasatt) calendar <- calendar$value
  else stop("DGVMData format requires a 'calendar' attribute on the time axis for daily data")
 
  # make some strings vectors for the days in years of different lengths 
  # (these are converted to codes "Year.Day" codes later on, hence the use of zero-padded strings)
  days.365 <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:365)) 
  days.366 <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:366)) 
  days.360 <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:360)) 

  # make a big vector of the years and days from the start date (defined by the netCDF time axis) to the beyond the end of the data
  years.vector <- c()
  days.vector<- c()
  day.counter <-as.numeric(format(start.date,"%d"))
  year <- as.numeric(format(start.date,"%Y"))
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
  
  return(list(years = years.vector, days = days.vector))
  
  
}

#' @keywords internal
getNetCDFDimension <- function(nc, dimension, verbose) {
  
  # determine possible names
  if(tolower(dimension) == "lat") possibles <- c("lat", "Lat", "latitude", "Latitude")
  if(tolower(dimension) == "lon") possibles <- c("lon", "Lon", "longitude", "Longitude")
  if(tolower(dimension) == "time") possibles <- c("time", "Time", "year", "Year")
  
  # get variables and return if present
  dims.present <- names(nc$dim)
  vars.present <- names(nc$var)
  
  for(possible in possibles) {
    
    if(possible %in% vars.present || possible %in% dims.present ) {
      
      # try to get dimension with exception handling
      this.result <- tryCatch({
        this.dim <- ncdf4::ncvar_get(nc, possible,verbose=verbose)
      }, warning = function(war) {
        return(NA)
      }, error = function(err) {
        return(NA)
      }, finally = {
      })
      
      # return only if not an NA (ie not an error or a warning)
      if(length(this.result) > 1) return(this.result)
      
    }
    
  }
  
  warning(paste0("Can't find ", dimension, " dimension.  Variables are: ", paste(vars.present, collapse = " "), ". Dimensions present are:",  paste(dims.present, collapse = " "), ". \n If possible, will now look up the gridcell file for this dimension."))
  
  return(NULL)
  
}


#' processYearlyNCAxis
#' This function processes a time axis which is assumed to be yearly
#' It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
#' and the actual years corresponding to this selection.
#' 
#' @keywords internal
processYearlyNCAxis <- function(axis.values, start.year, target.STAInfo) {
  
  # convert the netcdf time increments into years
  end.year <- start.year+length(axis.values)-1
  years.vector <- start.year:end.year
  
  # get the first and last indices based on the years requested
  # returns a two-element list, with elements "first" and "last"
  first.last.indices <- calculateYearCroppingIndices(target.STAInfo, years.vector)
  
  # make the index/count for reading the netCDF files
  start.time <- first.last.indices$first
  count.time <- first.last.indices$last - first.last.indices$first + 1
  
  # crop the vectors to match 
  years.vector <- years.vector[first.last.indices$first:first.last.indices$last]
  
 
  return(list(years.vector = years.vector,
              start.time = start.time,
              count.time = count.time))
  
}


#' processMonthlyNCAxis
#' This function processes a time axis which is assumed to be monthly
#' It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
#' and the actual years and months corresponding to this selection.
#' 
#' @keywords internal
processMonthlyNCAxis <- function(axis.values, start.date, target.STAInfo) {
  
  # extract the month and year from the start date
  start.year <- as.numeric(format(start.date,"%Y"))
  start.month <- as.numeric(format(start.date,"%m"))
  
  # convert the netcdf time increments into years and months
  
  # first make a vector of the years and months
  nyears.to.cover <- ceiling(length(axis.values) / 12)
  years.to.cover <- 0:(nyears.to.cover-1) + start.year
  years.vector <- rep(years.to.cover, each = 12)
  months.vector <- rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), times = nyears.to.cover)
  
  # here crop to match the actual length of the data (in case of not complete years)
  years.vector <- years.vector[start.month:length(axis.values)]
  months.vector <- months.vector[start.month:length(axis.values)]
  
  # get the first and last indices based on the years requested
  # returns a two-element list, with elements "first" and "last"
  first.last.indices <- calculateYearCroppingIndices(target.STAInfo, years.vector)
  
  # make the index/count for reading the netCDF files
  start.time <- first.last.indices$first
  count.time <- first.last.indices$last - first.last.indices$first + 1
  
  # crop the vectors to match 
  years.vector <- years.vector[first.last.indices$first:first.last.indices$last]
  months.vector <- months.vector[first.last.indices$first:first.last.indices$last]
  
  return(list(years.vector = years.vector,
              months.vector = months.vector,
              start.time = start.time,
              count.time = count.time))
  
}


#' processDailyNCAxis
#' This function processes a time axis which is assumed to be daily
#' It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
#' and the actual years and days corresponding to this selection.
#' 
#' @keywords internal
processDailyNCAxis <- function(axis.values, start.date, target.STAInfo,  calendar)  {
  
  # get start day and year from start.date
  start.date.year <- as.numeric(format(start.date,"%Y"))
  start.date.day <- as.numeric(format(start.date,"%d"))
  
  # is calendar proleptic
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
  axis.values <- trunc(axis.values)
  
  # make a big vector of the years and days from the start date (defined by the netCDF time axis) to the beyond the end of the data
  years.vector <- c()
  days.vector<- c()
  day.counter <- start.date.day
  year <- start.date.year
  while(day.counter < axis.values[length(axis.values)]) {
    
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
  years.vector <- years.vector[axis.values+1]
  days.vector <- days.vector[axis.values+1]
 

  # get the first and last indices based on the years requested
  # returns a two-element list, with elements "first" and "last"
  
  first.last.indices <- calculateYearCroppingIndices(target.STAInfo, years.vector)
  
  # make the index/count for reading the netCDF files
  start.time <- first.last.indices$first
  count.time <- first.last.indices$last - first.last.indices$first + 1
  
  # crop the vectors to match 
  years.vector <- years.vector[first.last.indices$first:first.last.indices$last]
  days.vector <- days.vector[first.last.indices$first:first.last.indices$last]
  
  return(list(start.time = start.time,
              count.time = count.time,
              years.vector = years.vector,
              days.vector = days.vector))
  
  
}