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

#' Returns the area in square meters (if scale=1, default) over a lon/lat grid.
#'
#' The function returns a data.table of area in square meters of a lon-lat grid.
#' Coordinates must be gridcell midpoint and the northern and southern edges are
#' calculated by as half of the distance to the next element in the latitude vector.
#' 
#' @param lat vetor of latitudes
#' @param lon vector of longitudes
#' @param scale multiplicator. If 1 (default) unit m^2
#' @param ellipse TRUE (polar and equatorial radius differ) or
#' FALSE (default, polar and equatorial radius are the same)
#' @export
#' @import data.table
#' @return data.table of gridcells with columns c("Lon", "Lat", "area")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @examples
#' lon <- seq(- 179.75, 179.75, 0.5)
#' lat <- seq(89.75,-89.75,-0.5) 
#' sum(gridarea2d(lon,lat, scale=1.e-12)$area)
#' message("equivalent:")
#' sum(gridarea2d(lon,lat)$area)*1.e-12
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

#' extract.seq: Extract a regular sequence from unique values in a vector
#'
#' Returns, if possible, an regular spaced sorted vector vector. If more than half of the unique values have unequal distances, the sorted values are returned.    
#' 
#' @param x vetor of values (e.g. longitudes or latitudes)
#' @param force.regular force a regular spaced vector with the smallest distance.
#' @param descending sort in descending order if TRUE (default: ascendind)
#' @export
#' @return data.frame of gridcells with columns c("Lon", "Lat", "area")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @examples
#' lon <- seq(- 179.75, 179.75, 0.5)
#' lat <- seq(89.75,-89.75,-0.5) # equivalent to the above
#' sum(gridarea2d(lat,lon)$area)*1.e-12
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
#' addArea: Adds the gridcell area to a spatial VegObject or data.table/data.frame
#' 
#' Adds the gridcell area to a spatial VegObject or data.table/data.frame
#' 
#' @param input a spatial VegObject or a data.frame/data.table with at least the columns Lon and Lat.
#' @param unit area unit. Default m^2, if something else is spefified udunits2 must be installed. If udunits2 is not installed this option is ignored.
#' @param ellipse If the eath should be assumed to be a ellipsoid instead of a sphere.
#' @param verbose print some information.
#' @export
#' @return same class as input
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
addArea <- function(input, unit="m^2", ellipse=FALSE, verbose=TRUE) {
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
  } else if (is.VegObject(input, spatial=TRUE)) {
    if (verbose)
      message("Input is a spatial VegObject.")
    lon <- extract.seq(input@data$Lon)
    lat <- extract.seq(input@data$Lat)
  } else {
    stop(paste("addArea: Don't know what to to with class", class(input)))
  }

  area <- gridarea2d(lon, lat, ellipse=ellipse)
  if (is.data.table(input) || is.VegObject(input)) {
    area <- as.data.table(area)
    if (is.data.table(input)) {
      setkey(area, Lon, Lat)
    } else {
      setkey(area, Lon, Lat)
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

  if (is.data.table(input)) {
    input <- area[input]
    if (verbose)
      message(paste("Added column 'area' in unit '", unit, "' to data.table.", sep=""))
    return(input)
  } else if (is.data.frame(input)) {
    input <- merge.data.frame(area, input, by=c("Lon", "Lat"))
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

######################################################################
### arithmetics with VegRun data slots ###############################
######################################################################
#' calcNewVegObj: Simple calculation of new VegObjects
#' 
#' Simple calculation of new VegObjects by operation '+', '-', '*' or '/'
#' 
#' @param run the run the data should be taken from
#' @param targets a list with elements 'x' and 'y' with components VegObject name and optionally column name. If neither x nor y has a column name the columns of x and y must be equal.
#' @param operator which arithmetic should be performed: addition ('+'), substraction ('-'), multiplication ('*') or division ('/')
#' @param quant new VegQuant definition to use, if NULL it will be guessed
#' @param verbose print some messages
#' @return hopefully a new VegObject
#' @export
#' @import data.table
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
calcNewVegObj <- function(run=NULL, targets=NULL, operator=NULL, quant=NULL, verbose=TRUE) {
  ## check if valid arguments are given
  if (!is.VegRun(run))
    stop("'run' is not a valid VegRun.")
  
  if (is.null(targets))
    stop("Don't know what to do, if you are not telling me!")
  if (!is.list(targets))
    stop("'targets' needs to be a list.")
  if (all(names(targets)!="x") && all(names(targets)!="y"))
    stop("List 'targets' doesn't contain x and/or y vector.")

  if (is.null(operator))
    stop("No operator specified.")
  if (grepl("^a", operator, ignore.case=TRUE) || operator=="+") {
    operator="+"
  } else if (grepl("^s", operator, ignore.case=TRUE) || operator=="-") {
    operator="-"
  } else if (grepl("^m", operator, ignore.case=TRUE) || operator=="*") {
    operator="*"
  } else if (grepl("^d", operator, ignore.case=TRUE) || operator=="/") {
    operator="/"
  } else {
    stop(paste("Operator '", operator, "' not implemented yet.", sep=""))
  }

  ## this needs to be further adjusted
  ## targets could become ids
  x <- eval(parse(text=paste("run@objects", "[['", targets[['x']][1], "']]", sep="")))
  y <- eval(parse(text=paste("run@objects", "[['", targets[['y']][1], "']]", sep="")))

  if (!is.VegObject(x))
    stop(paste("target '", targets[['x']][1], "is not a VegObject!", sep=""))
  if (!is.VegObject(y))
    stop(paste("target '", targets[['y']][1], "is not a VegObject!", sep=""))
 
  if (x@is.temporally.averaged != y@is.temporally.averaged ||
      x@is.spatially.averaged != y@is.spatially.averaged ||
      x@is.site != y@is.site)
    stop("'x' and 'y' are averaged differently.")

  if (verbose)
    message("Passed initial checks.")

  ## get the data
  if (verbose)
    message("Getting the data.")

  x.quant     <- x@quant
  x.t.extent  <- x@temporal.extent
  x.sp.extent <- x@spatial.extent
  x.run       <- x@run
  x.dt        <- copy(x@data)
  y.quant     <- y@quant
  y.t.extent  <- y@temporal.extent
  y.sp.extent <- y@spatial.extent
  y.dt        <- copy(y@data)

  if (!is.equal(x.t.extent, y.t.extent)) 
    warning("Temporal extents differ.")
  if (!is.equal(x.sp.extent, y.sp.extent))
    warning("Spatial extents differ.")

  if (!is.equal(x.quant, y.quant) && (operator=="+" || operator=="-"))
    warning("Quantity definitions differ.")

  if (!is.VegQuant(quant) ) {
    quant <- x.quant
    quant@id = paste(targets[[1]][1], operator, targets[[2]][1], sep="")
    quant@short.string = quant@id
    quant@full.string = quant@id
    if (operator=="*" || operator=="/")
      quant@units = paste("(", x.quant@units, ") ", operator, " (", y.quant@units, ")", sep="")
  }

  if (verbose)
    message("Performing calculations.")

  ## perform the calculation
  if (length(targets[[1]])==1 && length(targets[[2]])==1) {
    if (length(colnames(x.dt)) != length(colnames(y.dt)))
      stop(paste("Number of columns in run (", length(colnames(x.dt)), "/", length(colnames(y.dt)), ") differ.", sep=""))
    for (n in colnames(x.dt)) {
      if (!any(colnames(y.dt)==n))
        stop(paste("run Objects have different column names:\n", colnames(x.dt), "\n", colnames(y.dt)))
    }
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))

    list.str <- paste(val.names, "=x.",val.names,operator,"y.",val.names, sep="", collapse=", ")
    if (x@is.temporally.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@is.spatially.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    }

    return(new("VegObject",
               id = paste(targets[[1]][1], operator, targets[[2]][1], sep=""),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               is.site = x@is.site,
               is.temporally.averaged = x@is.temporally.averaged,
               is.spatially.averaged = x@is.spatially.averaged,
               run = as(run, "VegRunInfo")))      

  } else if (length(targets[[1]])==1 && length(targets[[2]])==2) {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))

    list.str <- paste(val.names, "=x.", val.names, operator, targets[[2]][2], sep="", collapse=", ")
    if (x@is.temporally.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@is.spatially.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("VegObject",
               id = paste(targets[[1]][1], operator, targets[[2]][1], sep=""),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               is.site = x@is.site,
               is.temporally.averaged = x@is.temporally.averaged,
               is.spatially.averaged = x@is.spatially.averaged,
               run = as(run, "VegRunInfo")))      

  } else if (length(targets[[1]])==2 && length(targets[[2]])==1) {
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))

    list.str <- paste(val.names, "=",targets[[1]][2], operator, "y.", val.names, sep="", collapse=", ")
    if (x@is.temporally.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@is.spatially.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("VegObject",
               id = paste(targets[[1]][1], operator, targets[[2]][1], sep=""),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               is.site = x@is.site,
               is.temporally.averaged = x@is.temporally.averaged,
               is.spatially.averaged = x@is.spatially.averaged,
               run = as(run, "VegRunInfo")))      
  } else {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))
    
    if (targets[[1]][2]==targets[[2]][2]) {
      list.str <- paste(targets[[1]][2],"=x.",targets[[1]][2], operator, "y.", targets[[2]][2], sep="")
    } else {
      list.str <- paste("value=x.",targets[[1]][2], operator, "y.", targets[[2]][2], sep="")
    }
    if (x@is.temporally.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
    } else if (x@is.spatially.averaged) {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, Year=Year, ", list.str,")]", sep="")))
    }
    return(new("VegObject",
               id = paste(targets[[1]][1], operator, targets[[2]][1], sep=""),
               data = new.dt,
               quant = quant,
               spatial.extent = x.sp.extent,
               temporal.extent = x.t.extent,
               is.site = x@is.site,
               is.temporally.averaged = x@is.temporally.averaged,
               is.spatially.averaged = x@is.spatially.averaged,
               run = as(run, "VegRunInfo")))      
  }
  stop("MISSING: Not implemented yet.")
}
