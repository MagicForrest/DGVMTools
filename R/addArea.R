.EarthRadius         <- 6371220.0
.EarthRadius.polar   <- 6356752.3142
.EarthRadius.equator <- 6378137.0

#' grid cell area along a vector of latitudes
#'
#' Returns the area in square meters along a vetor of latitudes by equal longitude distance.
#'
#' The function returns a vector of area in square meters along a vector
#' of latitudes. These must not be of equal distance. However, for the longitude
#' will be equal along the given latitude vector. The latitude is assumed
#' to be the gridcell midpoint and the northern and southern edges are
#' calculated as half the distance to the next element in the latitude vector.
#' 
#' @param lat vetor of latitudes
#' @param dlon longitudinal extent
#' @param scale multiplicator. If 1 (default) unit m^2
#' @param ellipse TRUE (polar and equatorial radius differ) or
#' FALSE (default, polar and equatorial radius are the same)
#' @keywords internal
#' @return vector of gridcell area is m^2
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @examples
#' dlon <- 0.5
#' lat <- seq(-89.75,89.75,0.5)
#' sum(gridarea1d(lat,dlon))*720*1.e-12
gridarea1d <- function (lat, dlon, scale=1.0, ellipse=FALSE) {
  nlat <- length(lat)
  area <- array(0.0, nlat)

  lat.border <- array(0.0, nlat+1)
  lat.border[1] = lat[1] - (lat[2] -lat[1])/2.
  for (i in 2:nlat) {
    lat.border[i] = lat[i] - (lat[i] - lat[i-1])/2.
  }
  lat.border[nlat+1] = lat[nlat] + (lat[nlat] - lat[nlat-1])/2.
  
  for (i in 1:nlat) {
# this causes a negligible difference (510.068 compared to 510.1013 10^6 km^2 @ 0.5 degree resolution globally).
    if (ellipse)
        .EarthRadius = .EarthRadius.equator * cos(lat[i]/180.0*pi)^2 + .EarthRadius.polar * sin(lat[i]/180*pi)^2;
    x <- cos(lat[i]/180.0*pi) * 2. * pi * .EarthRadius / (360.0/dlon);
    y <- 2 * pi * .EarthRadius * (abs(lat.border[i+1] - lat.border[i]) / 360.);
    area[i] <- x*y
  }
  return(area*scale)
}

#' Returns the area for a lon/lat grid.
#'
#' The function returns a data.table of area in square meters of a lon-lat grid.
#' Coordinates must be gridcell midpoint and the northern and southern edges are
#' calculated as half of the distance to the next element in the latitude vector.
#' Uses \code{\link{gridarea1d}}.
#' 
#' @param lat vetor of latitudes
#' @param lon vector of longitudes
#' @param scale multiplicator. If 1 (default) unit m^2
#' @param ellipse TRUE (polar and equatorial radius differ) or
#' FALSE (default, polar and equatorial radius are the same)
#' @keywords internal
#' @import data.table
#' @return data.table of gridcells with columns c("Lon", "Lat", "area")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @examples
#' lon <- seq(- 179.75, 179.75, 0.5)
#' lat <- seq(89.75,-89.75,-0.5) 
#' sum(gridarea2d(lon,lat, scale=1.e-12)$area)
#' sum(gridarea2d(lon,lat, scale=1.e-12, ellipse=TRUE)$area)
gridarea2d <- function(lon, lat, scale=1.0, ellipse=FALSE) {
  nlon   <- length(lon)
  nlat   <- length(lat)
  lon2d  <- array(rep(lon, times=nlat), c(nlon, nlat))
  lat2d  <- array(rep(lat, each=nlon), c(nlon, nlat))
  area1d <- gridarea1d(lat, min(lon[2:length(lon)] - lon[1:(length(lon)-1)]), scale=scale, ellipse=ellipse)
  area2d <- array(rep(area1d, each=length(lon)), c(length(lon), length(lat)))
  area   <- data.frame(Lon=as.vector(lon2d),
                       Lat=as.vector(lat2d),
                       area=as.vector(area2d))
  area <- data.table(area, key=c("Lon", "Lat"))
  return(area)
}

#' Extract a regular sequence from unique values in a vector
#'
#' Returns, if possible, an regular spaced sorted vector vector. If more than half of the unique values have unequal distances, the sorted values are returned.    
#' 
#' @param x vetor of values (e.g. longitudes or latitudes)
#' @param force.regular force a regular spaced vector with the smallest distance.
#' @param descending sort in descending order if TRUE (default: ascendind)
#' @export
#' @return data.frame of gridcells with columns c("Lon", "Lat", "area")
#' @keywords internal
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
extract.seq <- function(x, force.regular=FALSE, descending=FALSE) {
  x <- sort(unique(x))
  d <- x[2:length(x)] - x[1:(length(x)-1)]
  if (length(unique(d)) > length(x)/2 && !force.regular) {
    warning("Irregular steps return just sorted values!")
    if (descending) {
      return(x[length(x):1])
    } else {
      return(x)
    }
  }
  if (descending) {
    x <- seq(max(x), min(x), -min(d))
  } else {
    x <- seq(min(x), max(x), min(d))
  }
  return(x)
}

#' Adds the gridcell area to a spatial Field or data.table/data.frame
#' 
#' Adds the gridcell area to a spatial Field or data.table/data.frame.
#' For unit conversion it makes use of \code{\link[udunits2]{ud.convert}}, if installed. If udunits2 is not installed the option is ignored and the returned unit is m^2.
#' 
#' @param input a spatial Field or a data.frame/data.table with at least the columns Lon and Lat.
#' @param unit area unit. Default m^2, if something else is spefified udunits2 must be installed. If udunits2 is not installed this option is ignored.
#' @param ellipse If the eath should be assumed to be a ellipsoid instead of a sphere.
#' @param digits Numeric, number of digits to which to truncate the coordinates when merging the area data.table with the input. This is a technical detail,
#' you only need to use it if you have troubles because of coordinates with a few decimal places.   
#' @param verbose print some information.
#' @export
#' @return same class as input
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
addArea <- function(input, unit="m^2", ellipse=FALSE, verbose=TRUE, digits = 10) {
  ## to avoid "no visible binding for global variable" during check
  Lat = Lon = NULL
  if (is.na(unit))
    unit="m^2"

  if (!is.logical(ellipse)) {
    warning(paste("'ellipse=", ellipse,"' is not boolean. Using FALSE instead.", sep=""))
    ellipse=FALSE
  }

  if (is.data.table(input) || is.data.frame(input)) {
    if (verbose)
      message("Input is a data.table or data.frame.")
    lon <- extract.seq(input$Lon)
    lat <- extract.seq(input$Lat)
  } else if (is.Field(input, spatial=TRUE)) {
    if (verbose)
      message("Input is a spatial Field.")
    lon <- extract.seq(input@data$Lon)
    lat <- extract.seq(input@data$Lat)
  } else {
    stop(paste("addArea: Don't know what to to with class", class(input)))
  }

  area <- gridarea2d(lon, lat, ellipse=ellipse)
 
  if (is.data.table(input) || is.Field(input)) {
    area <- as.data.table(area)
    if (is.data.table(input)) {
      setKeyDGVM(area)
    } else {
      setKeyDGVM(area)
    }
  }

  if (unit!="m^2") {
    if (requireNamespace("udunits2", quietly=TRUE)) {
      if (udunits2::ud.is.parseable(unit)) {
        if (udunits2::ud.are.convertible("m^2", unit)) {
          area$area = udunits2::ud.convert(area$area, "m^2", unit)
        } else {
          warning(paste("m^2 not convertible to '", unit, "'. Using m^2 instead.", sep=""))
          unit="m^2"
        }
      } else {
        warning(paste("Unit '", unit, "' not parseable! Using m^2 instead.", sep=""))
        unit="m^2"
      }
    } else {
      message("Package 'udunits2' not installed! Using m^2 instead.")
      warning("Package 'udunits2' not installed! Using m^2 instead.")
    }  
  }
  

  #input <- na.omit(input)

  if (is.data.table(input)) {
  
    
    setKeyDGVM(area)
    setkeyv(input, key(area))

    if(!is.null(digits)) {
      
      input[, Lon := round(Lon, digits)]
      input[, Lat := round(Lat, digits)]
      area[, Lon := round(Lon, digits)]
      area[, Lat := round(Lat, digits)]
      
    }
 
    input <- merge(area, input, all.x = FALSE, all.y = TRUE)
    
    if (verbose)
      message(paste("Added column 'area' in unit '", unit, "' to data.table.", sep=""))
    return(input)
  } else if (is.data.frame(input)) {
    input <- merge.data.frame(area, input, by=getDimInfo(input))
    if (verbose)
      message(paste("Added column 'area' in unit '", unit, "' to data.frame.", sep=""))
    return(input)
  } else {
    dt <- input@data
    dt <- area[dt]
    input@data <- dt
    if (verbose)
      message(paste("Added column 'area' in unit '", unit, "'' to data.table in slot 'data'.", sep=""))
    return(input)
  }
}
