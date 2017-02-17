## Some date function
## not yet really needed. However, potentially useful with the daily LPJ-GUESS output.

#' check if a given year is a leap year
#' 
#' @param year year (integer or vector)
#' @param always use leap years even before 1582
#' @param doy return days of year instead logical
#' @return logical or integer, if doy os TRUE
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
is.leapyear <- function(year, always=FALSE, doy=FALSE) {
  leap <- sapply(year, function(x) {
    if (!always && x<1582) return(FALSE)
    if (((x %% 4 == 0) & (x %% 100 != 0)) | (x %% 400 == 0))
      return(TRUE)
    return(FALSE)
  })
  if (doy)
    return(ifelse(leap, 366, 365))
  return(leap)
}

#' day of year to date in the form of MonthDay conversion
#' 
#' @param doy day of the year ('1' for 1. January).
#' @param leap in a leap year of not.
#' @return 4 digit string
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
doy2mmdd <- function(doy, leap=FALSE) {
  dpm <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dpm[3] = 29
  day <- 0
  for (i in 1:length(dpm)) {
    if (doy - sum(dpm[1:i]) <= 0) {
      i <- i-1
      day <- doy - sum(dpm[1:i])
      break
    }
  }
  return(sprintf("%02d%02d", i, day))
}

#' date in the form of MonthDay to day of year conversion
#' @param mmdd string or number interpreted as MMDD
#' @param leap in a leap year of not.
#' @return Day of the year as integer (1. Januar is day 1).
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
mmdd2doy <- function(mmdd, leap=FALSE) {
  mmdd <- as.numeric(mmdd)
  stopifnot(mmdd>100 && mmdd<1232)
  dpm <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dpm[3] = 29
  return(sum(dpm[1:floor(mmdd / 100)]) + mmdd - (floor(mmdd / 100)) * 100)
}




#' Convert a ModelObject to a multi-dimensional array
#' 
#' @param d the data.table of a \code{\link[DGVMTools]{ModelObject}}
#' @param cname the column name to convert, if not set a list is returned
#' @param invertlat start in the north
#' @return a array or a list or arrays
#' 
#' @importFrom reshape2 acast
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
modelObject2Array <- function(d, cname=FALSE, invertlat=FALSE) {
  ## get the full spatial extent
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat, descending=invertlat)
  
  ## check for annual data
  is.temporal <- FALSE
  if (any(colnames(d)=="Year")) {
    time <- sort(unique(d$Year))
    is.temporal <- TRUE
  }
  
  ## check for monthly data
  is.monthly <- FALSE
  if (all(month.abb %in% colnames(d))) {
    cname <- FALSE
    if (is.temporal) {
      d <- data.table::melt(d, id.vars=c("Lon", "Lat", "Year"))
      d[, Year:= Year * 100 + as.numeric(variable)]
      d$variable <- NULL
      setkey(d, Lon, Lat, Year)
    } else {
      d <- data.table::melt(d, id.vars=c("Lon", "Lat", "Year"))
      d[, Year:= as.numeric(variable)]
      d$variable <- NULL
      setkey(d, Lon, Lat, Year)
    }
    time <- sort(unique(d$Year))    
    is.monthly <- TRUE
    is.temporal <- TRUE
  }
  
  ## create the target grid
  if (is.temporal) {
    full.grid <- data.table(Lon=rep(rep(lon, length(lat)), length(time)),
                            Lat=rep(rep(lat, each=length(lon)), length(time)),
                            Year=rep(time, each=length(lon) * length(lat)))                     
    setkey(full.grid, Lon, Lat, Year)
  } else {
    full.grid <- data.table(Lon=rep(lon, length(lat)),
                            Lat=rep(lat, each=length(lon)))
    setkey(full.grid, Lon, Lat)
  }
  
  ## get the desired column name(s) if none was given
  if (is.logical(cname))
    cname <- colnames(d)[!(colnames(d) %in% c("Lon", "Lat", "Year"))]
  
  ## create the array(s)
  rv <- lapply(cname, function(x) {
    if (is.temporal) {
      d <- d[full.grid]
      rv <- acast(d, Lon ~ Lat ~ Year, value.var=x)
      if (invertlat)
        rv <- rv[,length(lat):1,]
    } else {
      d <- d[full.grid]
      rv <- acast(d, Lon ~ Lat, value.var=cname)
      if (invertlat)
        rv <- rv[,length(lat):1]
    }
    return(rv)
  })
  if (length(rv) == 1)
    return(rv[[1]])
  
  names(rv) <- cname
  return(rv)
}
