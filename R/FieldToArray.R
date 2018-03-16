
#' Convert a Field to a multi-dimensional array
#' 
#' @param d the data.table of a \code{\linkS4class{Field}}
#' @param cname the column name to convert, if not set a list is returned
#' @param invertlat start in the north
#' @param verbose print some information
#' @return a array or a list or arrays
#' 
#' @importFrom reshape2 acast
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
FieldToArray <- function(d, cname=FALSE, invertlat=FALSE, verbose=FALSE) {
  
  Lon=Lat=Year=Month=variable=NULL
  
  ## get the full spatial extent
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat, descending=invertlat)
  if (verbose) {
    message(paste0("Spatial extent: Lon: ", min(lon), " ", max(lon), " (", length(lon), ")\n", 
                   "                Lat: ", min(lat), " ", max(lat), " (", length(lat), ")"))
  }
  
  ## get temporal info
  st.names <- getSTInfo(d)
  
  ## check for annual data
  is.temporal <- FALSE
  if("Year" %in% st.names) {
     if (verbose)
      message("'Year' column present.")
    time <- sort(unique(d$Year))
    is.temporal <- TRUE
  }
  
  ## check for monthly data
  is.monthly <- FALSE
  if("Month" %in% st.names) {
    cname <- FALSE
   
    # note that replacing the step below with some sort of paste command slows things down a lot, faaaar better to use a numeric here
    if (is.temporal) {  d[, Year:= Year * 100 + as.numeric(Month)]  }
    time <- sort(unique(d$Year))

    d[, Month := NULL]
    is.monthly <- TRUE
    is.temporal <- TRUE
  }
 
  #print(d)
  setKeyDGVM(d)
  
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