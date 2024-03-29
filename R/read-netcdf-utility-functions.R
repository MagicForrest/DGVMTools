#' Parse start date
#' 
#' Parses a "units" attribute string of the time axis of a netCDF file.  Returns a three-vector of year,month,time.
#' NOTE: Accepts years < 0 and years > 9999, unlike POSIX and "Date" dates. 
#' 
#' @keywords internal

parseStartDate <- function(start.date.string) {
  
  # extract the time step string  (ie. "day"/"month"/"year") so we can gsub it out
  time.units.string.vec <- unlist(strsplit(start.date.string, " "))
  timestep.unit <- time.units.string.vec[1]
  
  # process the attribute string a little to extract the start date
  start.date.string <- gsub(paste0(timestep.unit,' '),'', start.date.string)
  start.date.string <- gsub(paste0("since",' '),'', start.date.string)
  start.date.string <- gsub(paste0("after",' '),'', start.date.string)
  start.date.string <- trimws(start.date.string)
  
  # check for negative start year
  negative.year.multiplier <- 1
  if(substr(start.date.string, 1, 1) == "-") {
    start.date.string <- substr(start.date.string, 2, nchar(start.date.string))
    negative.year.multiplier <- -1
  }
  
  
  # try to get start date with very necessary exception handling
  start.date <- tryCatch({
    as.POSIXct(start.date.string, tryFormats=c("%Y-%m-%d %H:%M:%S", "%Y-%m-%d")) # Extract the start / origin date
  }, warning = function(war) {
    return(NA)
  }, error = function(err) {
    return(NA)
  }, finally = {
  })
  
  # if standard date format worked then easy, happy days
  if(!is.na(start.date)) {
    return(c(year = as.numeric(format(start.date,"%Y")) * negative.year.multiplier,
             month = as.numeric(format(start.date,"%m")),
             day = as.numeric(format(start.date,"%d"))
             
    ))
  }
  # else do some "hard parsing"
  else{
    
    # first split string by " " (space) to remove and select the first part to get the date (discarding the time and whatever else)
    start.date.string.dateonly <- unlist(strsplit(start.date.string, " "))[1]
    
    # now split it by "-" to separate the year, month, day
    start.date.string.dateonly.vec <- unlist(strsplit(start.date.string.dateonly, "-"))
    start.date.year <- as.numeric(start.date.string.dateonly.vec[1]) *  negative.year.multiplier
    if(length(start.date.string.dateonly.vec) > 1) start.date.month <- as.numeric(start.date.string.dateonly.vec[2])
    if(length(start.date.string.dateonly.vec) > 2) start.date.day <- as.numeric(start.date.string.dateonly.vec[3])
    
  }
  
  return( c(year = start.date.year, 
            month = start.date.month, 
            day = start.date.day))
  
}



calculateYearCroppingIndices <- function(target_STAInfo, years_vector) {
  
  # if both target first and last year are defined
  if(length(target_STAInfo@first.year) > 0 & length(target_STAInfo@last.year) > 0) {
    
    # extract the first and last target years and handle the missing cases
    if(is.null(target_STAInfo@first.year) || length(target_STAInfo@first.year) == 0)  target.first.year <- years_vector[1]
    else target.first.year <- target_STAInfo@first.year
    if(is.null(target_STAInfo@last.year) || length(target_STAInfo@last.year) == 0)  target.last.year <- years_vector[length(years_vector)]
    else target.last.year <- target_STAInfo@last.year
    

    # if first and last years don't already match the beginning and end of the dataset
    if(target.first.year != years_vector[1] || target.last.year != years_vector[length(years_vector)]) {
      
      # if first or last year is outside the data set then fail
      if(target.first.year < years_vector[1]) stop(paste("Requested first.year = ", target.first.year, ", but first year in data is", years_vector[1], sep = " "))
      if(target.last.year > years_vector[length(years_vector)]) stop(paste("Requested last.year = ", target.last.year, ", but last year in data is", years_vector[length(years_vector)], sep = " "))
      
      # find the first occurrence of the target first.year and the last occurrence of the target year in the year.vector
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
processYearlyNCAxis <- function(axis.values, start.year, target.STAInfo, year.offset) {
  
  # Potential TODO - rewrite this function to return a data.table with all the time axis information similarly to 
  # the function "processDailyRelativeNCAxis()"
  
  # adjust start.year by number of years which the first time value is after start.year
  start.year <- start.year + axis.values[1]
  
  # convert the netcdf time increments into years
  end.year <- start.year+length(axis.values)-1
  years.vector <- start.year:end.year
  
  # apply the year offset before cropping 
  if(year.offset != 0) years.vector <- years.vector + year.offset
  
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
processMonthlyNCAxis <- function(axis.values, start.year, start.month, target.STAInfo, year.offset) {
  
  # Potential TODO - rewrite this function to return a data.table with all the time axis information similarly to 
  # the function "processDailyRelativeNCAxis()"
  
  # convert the netcdf time increments into years and months
  
  # adjust start.year by number of years which the first time value is after start.year
  start.year <- start.year + axis.values[1] %/% 12
  
  # first make a vector of the years and months
  nyears.to.cover <- ceiling(length(axis.values) / 12)
  years.to.cover <- start.year:((nyears.to.cover-1) + start.year)
  years.vector <- rep(years.to.cover, each = 12)
  months.vector <- rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), times = nyears.to.cover)
  
  # here crop to match the actual length of the data (in case of not complete years)
  years.vector <- years.vector[start.month:length(axis.values)]
  months.vector <- months.vector[start.month:length(axis.values)]
  
  # apply the year offset before cropping 
  if(year.offset != 0) years.vector <- years.vector + year.offset
  
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


#' processDailyRelativeNCAXis
#' This function processes a relative time axis with daily intervals
#' It calculates the day, month and year corresponding to the valies on the axis, and also crops according to the requested years
#' Finally it returns a data.tables with all this information including the netCDF indices (cropped to the years) 
#' 
#' @keywords internal
processDailyRelativeNCAxis <- function(axis.values, start.year, start.month, start.day, target.STAInfo, calendar, year.offset) {
  
  
  # Kill CHECK NOTES
  CumulativeDay = NetCDFIndex = Year = NULL
  
  # vector assigning months to days for different possible years/calendars
  Month.col.360.day <- rep(1:12, each = 30)
  Month.col.365.day <- numeric(0)
  Month.col.366.day <- numeric(0)
  for(month in all.months) {
    Month.col.365.day <- append(Month.col.365.day, rep(month@index, month@days))
    Month.col.366.day <- append(Month.col.366.day, rep(month@index, month@days.leap))
  }
  
  # make a (potentially) long data.table of "year", "month" and "day" starting with start.year
  year <- start.year
  last.day.covered <- 0
  
  # while not full axis loop (note that we go beyond the axis in case the start month and date are not both "01") 
  all.dates <- data.table()
  while(last.day.covered < axis.values[length(axis.values)]){
    
    # select year length based on the calendar
    # 365 day
    if(calendar == "365_day" | 
       calendar == "no_leap" |
       calendar == "noleap" |
       (calendar == "standard" & !is.leapyear(year)) | 
       (calendar == "gregorian" & !is.leapyear(year)) | 
       (calendar == "proleptic_gregorian" & !is.leapyear(year, proleptic = TRUE))) {
      temp.dt <- data.table(CumulativeDay = (1:365)+nrow(all.dates)-(start.day), Day = 1:365, Month = Month.col.365.day, Year = rep(year, 365))
    }
    # 366 day
    else if(calendar == "366_day" | 
            calendar == "all_leap" | 
            calendar == "allleap" | 
            (calendar == "standard" & is.leapyear(year)) | 
            (calendar == "gregorian" & is.leapyear(year)) | 
            (calendar == "proleptic_gregorian" & is.leapyear(year, proleptic = TRUE))) {
      temp.dt <- data.table(CumulativeDay = (1:366)+nrow(all.dates)-(start.day), Day = 1:366, Month = Month.col.366.day, Year = rep(year, 366))
    }
    # 360 day
    else if(calendar == "360_day" |
            calendar == "uniform30day") {
      temp.dt <- data.table(CumulativeDay = (1:360)+nrow(all.dates)-(start.day), Day = 1:360, Month = Month.col.360.day, Year = rep(year, 360))
    }
    else{
      stop(paste0("Calendar ", calendar, " not supported by NetCDF format in DGVMTools package"))
    }
    
    # append to the data.table and increment years
    all.dates <- rbind(all.dates, temp.dt)
    year <- year+1
    last.day.covered <- temp.dt[["CumulativeDay"]][length(temp.dt[["CumulativeDay"]])]
    
  }
  
  # now select the ones that actually correspond to values on the time axis
  # use of "floor" is because some netCDF files use fractional days to represent the time of the day (which we ignore)
  # and so here we are simply trunctating away this fractional time.
  actual.dates <- all.dates[CumulativeDay %in% floor(axis.values),]
  
  # check this worked and that we have a row in the data.table for each entry on the original netCDF time axis!
  if(length(axis.values) != nrow(actual.dates)) stop(paste0("ERROR: Something went wrong when reading relative netCDF axis. Axis had ", length(axis.values), " dates, but the function has calculated ", nrow(actual.dates), ". Teminating."))

  # add a simple index to match the index required for reading netCDF
  actual.dates[, NetCDFIndex := 1:length(axis.values)]
  
  # apply the year offset before cropping 
  if(year.offset != 0) actual.dates[, Year := Year + year.offset]
  
  # get the first and last indices based on the years requested
  # returns a two-element list, with elements "first" and "last"
  first.last.indices <- calculateYearCroppingIndices(target.STAInfo, actual.dates[["Year"]])
  
  # crop the table to match the years we want and return
  actual.dates[NetCDFIndex >= first.last.indices$first & NetCDFIndex <= first.last.indices$last, ]

}


#' processDailyNCAxis
#' This function processes a time axis which is assumed to be daily
#' It compares the time axis to the years requested and return the start and count values for reading up the netCDF file
#' and the actual years and days corresponding to this selection.
#' 
#' UNUSED, and is DEPRECATED in favour of processDailyRelativeNCAxis().  However this function may yet find a use for absolute time axes.
#' 
#' 
#' @keywords internal
processDailyNCAxis <- function(axis.values, start.year, start.day, target.STAInfo,  calendar, year.offset)  {
  
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
  day.counter <- start.day
  year <- start.year
  while(day.counter < axis.values[length(axis.values)]) {
    
    # select year length based on the calendar
    # 365 day
    if(calendar == "365_day" |
       (calendar == "standard" & !is.leapyear(year)) | 
       (calendar == "gregorian" & !is.leapyear(year)) | 
       (calendar == "proleptic_gregorian" & !is.leapyear(year, proleptic = TRUE))) {
      this.year.days <- days.365
    }
    # 366 day
    else if(calendar == "366_day" |
            (calendar == "standard" & is.leapyear(year)) | 
            (calendar == "gregorian" & is.leapyear(year)) |
            (calendar == "proleptic_gregorian" & is.leapyear(year, proleptic = TRUE))) {
      this.year.days <- days.366
    }
    # 360 day
    else if(calendar == "360_day") {
      this.year.days <- days.360
    }
    else{
      stop(paste0("Calendar", calendar, "not supported by NetCDF format in DGVMTools package"))
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
  
  # apply the year offset before cropping 
  if(year.offset != 0) years.vector <- years.vector + year.offset
  
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