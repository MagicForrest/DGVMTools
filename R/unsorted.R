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
