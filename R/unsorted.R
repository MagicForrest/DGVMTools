## Some date function
## not yet really needed. However, potentially useful with the daily LPJ-GUESS output.

#' check if a given year is a leap year
#' 
#' @param year year (integer or vector)
#' @param proleptic use leap years even before 1582.
#' @param doy return days of year instead logical.
#' @return logical or integer, if doy is TRUE
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
is.leapyear <- function(year, proleptic=FALSE, doy=FALSE) {
  leap <- sapply(year, function(x) {
    if (!proleptic && x < 1582) return(FALSE)
    if (((x %% 4 == 0) & (x %% 100 != 0)) | (x %% 400 == 0))
      return(TRUE)
    return(FALSE)
  })
  if (doy)
    return(ifelse(leap, 366, 365))
  return(leap)
}

#' Convert a ModelObject to a multi-dimensional array
#' 
#' @param d the data.table of a \code{\linkS4class{ModelObject}}
#' @param cname the column name to convert, if not set a list is returned
#' @param invertlat start in the north
#' @param verbose print some information
#' @return a array or a list or arrays
#' 
#' @importFrom reshape2 acast
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
modelObject2Array <- function(d, cname=FALSE, invertlat=FALSE, verbose=FALSE) {

  Lon=Lat=Year=variable=NULL

  ## get the full spatial extent
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat, descending=invertlat)
  if (verbose)
    message(paste0("Spatial extent: Lon: ", min(lon), " ", max(lon), " (", length(lon), ")\n", 
                   "                Lat: ", min(lat), " ", max(lat), " (", length(lat), ")"))

  ## check for annual data
  is.temporal <- FALSE
  if (any(colnames(d) == "Year")) {
    if (verbose)
      message("'Year' column present.")
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
      d <- data.table::melt(d, id.vars=c("Lon", "Lat"))
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
      rv <- reshape2::acast(d, Lon ~ Lat ~ Year, value.var=x)
      if (invertlat)
        rv <- rv[,length(lat):1,]
    } else {
      d <- d[full.grid]
      rv <- reshape2::acast(d, Lon ~ Lat, value.var=cname)
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

#' Array methods
#' 
#' Converts a \code{\linkS4class{ModelObject}} to multi-dimensional array(s) all parameters are passed along to \code{\link{modelObject2Array}}.
#' 
#' @param x \code{\linkS4class{ModelObject}}
#' @param ... Other arguments, not currently used
#' @return an lon/lat(/time) array - or a list of arrays - of the modelObjects input data.table.
#' @name Array-methods
#' @rdname Array-methods
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}         
setGeneric("as.array", function(x,...) standardGeneric("as.array"))

#' @rdname Array-methods
#' @aliases as.array
setMethod("as.array", signature("ModelObject"), function(x, ...) {
  modelObject2Array(x@data, ...)
})
