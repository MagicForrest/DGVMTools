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
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat, descending=invertlat)
  
  time <- FALSE
  
  if (any(colnames(d)=="Year")) {
    year <- extract.seq(d$Year)
    time <- TRUE
  }
  
  full.grid <- data.frame(Lon=rep(lon, length(lat)), Lat=rep(lat, each=length(lon)))
  
  if (is.logical(cname))
    cname <- colnames(d)[!(colnames(d) %in% c("Lon", "Lat", "Year"))]
  
  rv <- lapply(cname, function(x) {
    if (time) {
      full.grid <- data.frame(full.grid, Year=rep(year, each=nrow(full.grid)))
      d <- merge(d, full.grid, by=c("Lon", "Lat", "Year"), all=TRUE)
      rv <- acast(d, Lon ~ Lat ~ Year, value.var=x)
      if (invertlat)
        rv <- rv[,length(lat):1,]
    } else {
      d <- merge(d, full.grid, by=c("Lon", "Lat"), all=TRUE)
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

