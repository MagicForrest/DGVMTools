.EarthRadius         <- 6371220.0
.EarthRadius.polar   <- 6356752.3142
.EarthRadius.equator <- 6378137.0

#' gridarea1d: grid cell area along a vector of latitudes
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
#' @keywords
#' @export
#' @return vector of gridcell area is m^2
#' @author Joerg Steinkamp <joergsteinkamp@yahoo.de>
#' @examples
#' dlon <- 0.5
#' lat <- seq(-89.75,89.75,0.5)
#' lat <- seq(89.75,-89.75,-0.5) # equivalent to the above
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
# this causes a negligible difference (510.068 compared to 510.1013 10^6 km^2 @ 0.5° resolution globally).
    if (ellipse)
        .EarthRadius = .EarthRadius.equator * cos(lat[i]/180.0*pi)^2 + .EarthRadius.polar * sin(lat[i]/180*pi)^2;
    x <- cos(lat[i]/180.0*pi) * 2. * pi * .EarthRadius / (360.0/dlon);
    y <- 2 * pi * .EarthRadius * (abs(lat.border[i+1] - lat.border[i]) / 360.);
    area[i] <- x*y
  }
  return(area*scale)
}


#' gridarea1d: grid cell area along a vector of latitudes
#'
#' Returns the area in square meters along a vetor of latitudes by equal longitude distance.
#'
#' The function returns a data.frame of area in square meters of a lon-lat grid.
#' Coordinates must be gridcell midpoint and the northern and southern edges are
#' calculated by as half of the distance to the next element in the latitude vector.
#' 
#' @param lat vetor of latitudes
#' @param lon vector of longitudes
#' @param scale multiplicator. If 1 (default) unit m^2
#' @param ellipse TRUE (polar and equatorial radius differ) or
#' FALSE (default, polar and equatorial radius are the same)
#' @keywords
#' @export
#' @return data.frame of gridcells with columns c("Lon", "Lat", "area")
#' @author Joerg Steinkamp <joergsteinkamp@yahoo.de>
#' @examples
#' lon <- seq(- 179.75, 179.75, 0.5)
#' lat <- seq(89.75,-89.75,-0.5) # equivalent to the above
#' sum(gridarea2d(lat,lon)$area)*1.e-12

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
  return(area)
}
  
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
