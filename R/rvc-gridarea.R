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
#' # equivalent to the above
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
#' @keywords
#' @export
#' @return data.frame of gridcells with columns c("Lon", "Lat", "area")
#' @author Joerg Steinkamp <joergsteinkamp@yahoo.de>
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

addArea <- function(input, unit="m^2", ellipse=FALSE, verbose=FALSE) {
  if (is.na(unit))
    unit="m^2"

  if (!is.logical(ellipse)) {
    warning(paste("'ellipse=", ellipse,"' is not boolean. Using FALSE instead.", sep=""))
    ellipse=FALSE
  }

  if (is.data.table(input)) {
    if (verbose)
      message("Input is a data.table.")
    lon <- extract.seq(input$Lon)
    lat <- extract.seq(input$Lat)
  } else {
    if (verbose)
      message("Input is not a data.table. Assuming it is a VegSpatial.")
    lon <- extract.seq(input@data$Lon)
    lat <- extract.seq(input@data$Lat)
  }

  area <- as.data.table(gridarea2d(lon, lat, ellipse=ellipse))
  setkeyv(area, c("Lon", "Lat"))

  if (unit!="m^2") {
    if (ud.is.parseable(unit)) {
      if (ud.are.convertible("m^2", unit)) {
        area$area = ud.convert(area$area, "m^2", unit)
      } else {
        warning(paste("m^2 not convertible to '", unit, "'. Using m^2 instead.", sep=""))
        unit="m^2"
      }
    } else {
      warning(paste("Unit '", unit, "' not parseable. Using m^2 instead.", sep=""))
      unit="m^2"
    }
  }

  if (is.data.table(input)) {
    input <- area[input]
    if (verbose)
      message(paste("Added column 'area' in unit '", unit, "' to data.table.", sep=""))
    return(input)
  } else {
    data <- input@data
    data <- area[data]
    input@data <- data
    if (verbose)
      message(paste("Added column 'area' in unit '", unit, "'' to data.table in slot 'data'.", sep=""))
    return(input)
  }
}

######################################################################
### arithmetics with VegRun data slots ###############################
######################################################################

calcNewVegObj <- function(run=NULL, targets=NULL, operator=NULL, quant=NULL, verbose=TRUE) {
  ## check if valid arguments are given
  if (!is.VegRun(run))
    stop("'run' is not a valid VegRun.")
  
  if (is.null(targets))
    stop("Don't know what to do, if you are not telling me!")
  if (!is.list(targets))
    stop("'targets' needs to be a list.")
  
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
  
  if ((targets[[1]][1] != "spatial" || targets[[2]][1] != "spatial") &&
      (targets[[1]][1] != "temporal" || targets[[2]][1] != "temporal"))
    stop("targets must be from the same slot(either 'spatial' or 'temporal').")

  if (verbose)
    message("Passed initial checks.")
  
  ## get the data
  if (targets[[1]][1] == "spatial") {
    if (!any(names(run@spatial)==targets[[1]][2]))
      stop(paste("No ", targets[[1]][1], " data called '", targets[[1]][2], "'available.", sep=""))
    if (!any(names(run@spatial)==targets[[2]][2]))
      stop(paste("No ", targets[[1]][1], " data called '", targets[[2]][2], "'available.", sep=""))

    if (verbose)
      message("Getting spatial data.")
    
    x.quant  <- run@spatial[[targets[[1]][2]]]@quant
    x.extent <- run@spatial[[targets[[1]][2]]]@temporal.extent
    x.run    <- run@spatial[[targets[[1]][2]]]@run
    x.dt     <- copy(run@spatial[[targets[[1]][2]]]@data)
    y.quant  <- run@spatial[[targets[[2]][2]]]@quant
    y.extent <- run@spatial[[targets[[2]][2]]]@temporal.extent
    y.dt     <- copy(run@spatial[[targets[[2]][2]]]@data)
    
    if (!is.equal(x.extent, y.extent)) {
      if (targets[[1]][1] == "spatial") {
        warning("Temporal extents differ.")
      } else {
        warning("Spatial extents differ.")
      }
    }
    if (!is.equal(x.quant, y.quant) && (operator=="+" || operator=="-"))
      warning("Quantity definitions differ.")
  } else {
    if (!any(names(run@temporal)==targets[[1]][2]))
      stop(paste("No ", targets[[1]][1], " data called '", targets[[1]][2], "'available.", sep=""))
    if (!any(names(run@temporal)==targets[[2]][2]))
      stop(paste("No ", targets[[1]][1], " data called '", targets[[2]][2], "'available.", sep=""))
    
    if (verbose)
      message("Getting temporal data.")
    
    x.quant  <- run@temporal[[targets[[1]][2]]]@quant
    x.extent <- run@temporal[[targets[[1]][2]]]@spatial.extent
    x.run    <- run@temporal[[targets[[1]][2]]]@run
    x.dt     <- copy(run@temporal[[targets[[1]][2]]]@data)
    y.quant  <- run@temporal[[targets[[2]][2]]]@quant
    y.extent <- run@temporal[[targets[[2]][2]]]@spatial.extent
    y.dt     <- copy(run@temporal[[targets[[2]][2]]]@data)
    
    if (!is.equal(x.extent, y.extent))
      warning("Temporal extents differ.")
    if (!is.equal(x.quant, y.quant) && (operator=="+" || operator=="-"))
      warning("Quantity definitions differ.")
    
  }

  if (!is.VegQuant(quant) ) {
    quant <- x.quant
    quant@id = paste(targets[[1]][2], operator, targets[[2]][2], sep="")
    quant@short.string = quant@id
    quant@full.string = quant@id
    if (operator=="*" || operator=="/")
      quant@units = paste("(", x.quant@units, ") ", operator, " (", y.quant@units, ")", sep="")
  }
  
  if (verbose)
    message("Performing calculations.")
  
  ## perform the calculation
  if (length(targets[[1]])==2 && length(targets[[2]])==2) {
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
    if (targets[[1]][1]=="spatial") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
      
      return(new("VegSpatial",
                 id = paste(targets[[1]][2], operator, targets[[2]][2], sep=""),
                 data = new.dt,
                 temporal.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))      
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
      
      return(new("VegTemporal",
                 id = paste(targets[[1]][2], operator, targets[[2]][2], sep=""),
                 data = new.dt,
                 spatial.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))  
    }
  } else if (length(targets[[1]])==2 && length(targets[[2]])==3) {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    
    list.str <- paste(val.names, "=x.", val.names, operator, targets[[2]][3], sep="", collapse=", ")
    if (targets[[1]][1]=="spatial") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
      
      return(new("VegSpatial",
                 id = paste(targets[[1]][2], operator, targets[[2]][2], "$" , targets[[2]][3], sep=""),
                 data = new.dt,
                 temporal.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))      
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
      
      return(new("VegTemporal",
                 id = paste(targets[[1]][2], operator, targets[[2]][2], "$" , targets[[2]][3], sep=""),
                 data = new.dt,
                 spatial.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))
    }
  } else if (length(targets[[1]])==3 && length(targets[[2]])==2) {
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))
    
    list.str <- paste(val.names, "=",targets[[1]][3], operator, "y.", val.names, sep="", collapse=", ")
    if (targets[[1]][1]=="spatial") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
      
      return(new("VegSpatial",
                 id = paste(targets[[1]][2], "$" , targets[[1]][3], operator, targets[[2]][2], sep=""),
                 data = new.dt,
                 temporal.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))      
      
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
      
      return(new("VegTemporal",
                 id = paste(targets[[1]][2], "$" , targets[[1]][3], operator, targets[[2]][2], sep=""),
                 data = new.dt,
                 spatial.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))  
    }
  } else {
    key.names <- key(x.dt)
    val.names <- names(x.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(x.dt, val.names, paste("x.", val.names, sep=""))
    key.names <- key(y.dt)
    val.names <- names(y.dt)
    val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
    setnames(y.dt, val.names, paste("y.", val.names, sep=""))

    if (targets[[1]][3]==targets[[2]][3]) {
      list.str <- paste(targets[[1]][3],"=x.",targets[[1]][3], operator, "y.", targets[[2]][3], sep="")
    } else {
      list.str <- paste("value=x.",targets[[1]][3], operator, "y.", targets[[2]][3], sep="")
    }
    if (targets[[1]][1]=="spatial") {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Lon=Lon, Lat=Lat, ", list.str,")]", sep="")))
      
      return(new("VegSpatial",
                 id = paste(targets[[1]][2], "$" , targets[[1]][3], operator, targets[[2]][2], "$",  targets[[2]][3], sep=""),
                 data = new.dt,
                 temporal.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))
      
    } else {
      new.dt <- eval(parse(text=paste("x.dt[y.dt, list(Year=Year, ", list.str,")]", sep="")))
      
      return(new("VegTemporal",
                 id = paste(targets[[1]][2], "$" , targets[[1]][3], operator, targets[[2]][2], "$", targets[[2]][3], sep=""),
                 data = new.dt,
                 spatial.extent = x.extent,
                 quant = quant,
                 run = as(run, "VegRunInfo")))  
    }
    
    
    stop("MISSING: Not implemented yet.")
    
  }
}


