#' Import external data
#' 
#' Import external raster data and return them as data.frame
#'
#' @param file path to the raster file
#' @param scale scaling factor for the data
#' @param nodata value or threshold for invalid pixels
#' @param nodata.limit Any of "eq", "lt", "le", "ge" or "gt"
#' @param method raster interpolation method
#' @param to target rater grid
#' @param mem.only should the rasterOptions be set for operation in memory only
#' @return data.frame with columns c("Lon", "Lat", "value")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import raster
#' @export
import.raster <- function(file, scale=1, nodata=NA, nodata.limit="eq", method="bilinear", to=raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+proj=longlat +ellps=WGS84")), mem.only=FALSE) {
  ## !!! execution on large memory machine recommended !!!
  ## requires rgdal or ncdf4
  ##
  ## netcdf issues a warning about CRS error,
  ## but data for Saatchi agrees with results from geotiff
  rdat.in <- raster(file)

  rsize <- 8*rdat.in@ncols * rdat.in@nrows

  if (mem.only) {
    rasterOptions(chunksize=rsize, maxmemory=rsize)
    mem.req <- (5*rsize)/1024^3
    if (mem.req>4)
      warning(paste("Warning: need approx.", round(mem.req, 1), "Gb memory!"))
  }

  if (is.finite(nodata)) {

    if (!any(nodata.limit==c("eq", "lt", "le", "ge", "gt")))
      stop("if is.finite(nodata), nodata.limit must be any of c('eq', 'lt', 'le', 'ge', 'gt')")

    if (nodata.limit=="eq") {
      rdat.in[rdat.in==nodata] = NA
    } else if (nodata.limit=="le") {
      rdat.in[rdat.in<=nodata] = NA
    } else if (nodata.limit=="lt") {
      rdat.in[rdat.in<nodata] = NA
    } else if (nodata.limit=="ge") {
      rdat.in[rdat.in>=nodata] = NA
    } else if (nodata.limit=="gt") {
      rdat.in[rdat.in>nodata] = NA
    }
  }
  if (mem.only)
    gc()

  if (scale != 1) {
    rdat.in = scale * rdat.in
  }
  if (mem.only)
    gc()

  rdat.out <- projectRaster(rdat.in, to, method=method)

  if (mem.only) {
    rm(rdat.in)
    gc()
  }

  df.out <- as.data.frame(rdat.out, xy=TRUE)
  colnames(df.out) <- c("Lon", "Lat", "value")

  df.out <- df.out[is.finite(df.out$value), ]

  return(df.out)
}

#' Get Annual values of climate drivers
#' 
#' Get Annual values of climate drivers from netCDF files, files must be CF-compliant
#' 
#' @param run a ModelRun object, if a VegSpatial object should be returned, otherwise NA
#' @param file path to the netCDF file
#' @param operation Any of sum, mean, gdd5
#' @param temporal.extent Either a TemporalExtent object, a vector of years (min/max are choosen) or 'NA' (whole netCDF timespan is used)
#' @param spatial.extent Either a SpatialExtent object or a raster extent
#' @param full should the full dataset be returned or an multiannual average (default).
#' @param monthly logical, if monthly instead of annual values should be returned
#' @param data.name Name of the data field in the netcdf file, if NA it will be guessed
#' @param lon.name Name of the longitude vector in the netcdf file, if NA it will be guessed
#' @param lat.name Name of the latitude vector in the netcdf file, if NA it will be guessed
#' @param time.name Name of the time vector in the netcdf file, if NA it will be guessed
#' @param read.full Recalculate the desired values (default: FALSE)
#' @param write write the calculated values for fture speedup (default: TRUE)
#' @param verbose print some messages
#' @param ... further so far ignored parameters
#' @return data.table if no ModelRun was given, otherwise a ModelObject
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import RNetCDF
#' @export
#' @examples message("See templates/Example.ggplot.R")
## TODO: extract file path to netcdf file from ins file or other model setup
getClimateDriver <- function(run=NA, file=NA, operation="mean", temporal.extent=NA, spatial.extent=NA, full=FALSE, monthly=FALSE, data.name=NA, lon.name=NA, lat.name=NA, time.name=NA, read.full=FALSE, write=TRUE, verbose=TRUE, ...) {
  ## to avoid "no visible binding for global variable" during check
  Lon = Lat = Year = Month = Date = variable = value = NULL
  suppressWarnings(if (!is.na(temporal.extent)) {
    if (class(temporal.extent)[1]=="TemporalExtent" && attr(class(temporal.extent), "package")=="DGVMTools") {
      period <- c(temporal.extent@start, temporal.extent@end)
    } else {
      period <- c(min(temporal.extent, na.rm=TRUE), max(temporal.extent, na.rm=TRUE))
    }
  } else {
    period <- NA
  })

  if (toupper(operation)=="GDD5") {
    operation <- toupper(operation)
  } else {
    operation <- tolower(operation)
  }
    
  ncin <- open.nc(file)

## get the dimension properties
  dim.names <- c()
  dim.len <- list()
  i <- 0
  valid <- TRUE
  while(valid) {
    rval = tryCatch({
      dim.inq.nc(ncin, i)
    }, error = function(e) {
      return(FALSE)
    })
    if (is.logical(rval)) {
      valid=FALSE
    } else {
      dim.names <- append(dim.names, rval$name)
      eval(parse(text=paste("dim.len$",rval$name,"=rval$length", sep="")))
    }
    i <- i+1
  }
  if (verbose)
    message(paste(c("Dimensions:", dim.names), collapse=" "))

## get the variable properties
  var.names <- c()
  var.dims <- list()
  i <- 0
  valid <- TRUE
  while(valid) {
    rval = tryCatch({
      var.inq.nc(ncin, i)
    }, error = function(e) {
      return(FALSE)
    })
    if (is.logical(rval)) {
      valid=FALSE
    } else {
      var.names <- append(var.names, rval$name)
      eval(parse(text=paste("var.dims$",rval$name,"=rval$dimids", sep="")))
    }
    i <- i+1
  }
  if (verbose)
    message(paste(c("Variables:", var.names), collapse=" "))

  ## get the name of the variable containing the data
  is.data.var <- lapply(var.dims, length)==length(dim.names)

  if (!is.na(data.name) && all(var.names!=data.name)) {
    print(var.dims)
    stop(paste("No variable named '", data.name, "' present!", sep=""))
  } else if (sum(is.data.var>1) && is.na(data.name)) {
    print(var.dims)
    stop("More than one possible data variable!")
  } else {
    data.name <- var.names[is.data.var]
  }
  data.dims <- var.inq.nc(ncin, data.name)[['dimids']]

  if (verbose)
    message(paste("Variable to get the data for:", data.name))

  ## get the spatial dimension names
  if (is.na(lon.name) || is.na(lat.name)) {
    if (length(dim.names)==2) {
      if (verbose)
        message("Reduced grid")

      coords <- att.get.nc(ncin, data.name, "coordinates")
      coords <- unlist(strsplit(coords, " "))
      lon.name <- coords[grepl("lon", coords, ignore.case=TRUE)]
      lat.name <- coords[grepl("lat", coords, ignore.case=TRUE)]
    } else if (length(dim.names)==3) {
      if (verbose)
        message("Regular grid")

      lon.name <- dim.names[grepl("lon", dim.names, ignore.case=TRUE)]
      lat.name <- dim.names[grepl("lat", dim.names, ignore.case=TRUE)]
    } else {
      print(dim.names)
      stop("Do not know how to handle files with dimensions other than 2 or 3!")
    }
  }
  if (verbose)
    message(paste("Spatial variables:", lon.name, lat.name))

## get the name of the temporal dimension variable
  if (is.na(time.name)) {
    time.name <- dim.names[grepl("time", dim.names, ignore.case=TRUE)]
    if (length(time.name)==0)
      time.name <- dim.names[grepl("record", dim.names, ignore.case=TRUE)]
    if (length(time.name)==0) {
      print(dim.names)
      stop("Did not find time dimension!")
    }
  }
  if (verbose)
    message(paste("Temporal dimension: ", time.name))

  lon   <- var.get.nc(ncin, lon.name)
  lat   <- var.get.nc(ncin, lat.name)
  if (verbose)
    message(paste("NetCDF spatial extent: Lon: ", min(lon), "-", max(lon), "; Lat:", min(lat), "-", max(lat)))

  if (class(spatial.extent) == "SpatialExtent" && attr(class(spatial.extent), "package") == "DGVMTools") {
    extent <- spatial.extent@extent
  } else if (class(spatial.extent)=="Extent") {
    extent <- spatial.extent
    spatial.extent <- new("SpatialExtent",
                          id="Undefined",
                          name="Undefined",
                          extent=extent)
    warning("Using 'Undefined' as id/name for spatial.extent, setting write=FALSE")
    message("Using 'Undefined' as id/name for spatial.extent, setting write=FALSE")
    write <- FALSE
  } else {
    spatial.extent <- NULL
  }
  if (!is.object(spatial.extent)) {
    ## calculate the resolution of the outer most gridcells (west/south negative; east/north positive)
    xres <- c(sort(unique(lon))[1] - sort(unique(lon))[2],
              sort(unique(lon))[length(unique(lon))] - sort(unique(lon))[length(unique(lon))-1])
    yres <- c(sort(unique(lat))[1] - sort(unique(lat))[2],
              sort(unique(lat))[length(unique(lat))] - sort(unique(lat))[length(unique(lat))-1])
    xlim <- c(min(lon), max(lon)) + xres/2
    ylim <- c(min(lat), max(lat)) + yres/2
    extent <- extent(xlim, ylim)
    spatial.extent <- new("SpatialExtent",
                          id="FullDomain",
                          name="Full simulation extent",
                          extent=extent)
  }

  time  <- var.get.nc(ncin, time.name)
  time.unit <- att.get.nc(ncin, time.name, "units")
  daytime <- gsub("^.* ", " ", time.unit)
  if (grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", daytime))
    daytime=""
  if (grepl("^days", time.unit)) {
    start <- as.Date(time.unit, format=paste("days since %Y-%m-%d", daytime, sep=""))
  } else if (grepl("^hours", time.unit)) {
    start <- as.Date(time.unit, format=paste("hours since %Y-%m-%d", daytime, sep=""))
  } else if (grepl("^minutes", time.unit)) {
    start <- as.Date(time.unit, format=paste("minutes since %Y-%m-%d", daytime, sep=""))
 } else if (grepl("^seconds", time.unit)) {
    start <- as.Date(time.unit, format=paste("seconds since %Y-%m-%d", daytime, sep=""))
  } else {
    stop(paste("Unknown time unit attribute: '", time.unit, "'!", sep=""))
  }
  if (is.na(start))
    stop(paste("Something went wrong during time unit parsing: '", time.unit, "'!", sep=""))

  start <- start + time[1]
  end   <- start + dim.len$time

  if (verbose)
    message(paste("NetCDF period:", start, end))

  if (any(is.na(period)))
    period <- c(year(start), year(end)-1)

  ## need also to include spatial extent in file name
  if (full) {
    fout <- paste(tools::file_path_sans_ext(file),
                  "_", max(year(start), period[1]), "-", min(year(end), period[2]),
                  "_", spatial.extent@id,
                  "_", operation, "_full.Rtable", sep="")
  } else if (monthly) {
    fout <- paste(tools::file_path_sans_ext(file),
                  "_", max(year(start), period[1]), "-", min(year(end), period[2]),
                  "_", spatial.extent@id,
                  "_", operation, "_monthly.Rtable", sep="")
  } else {
    fout <- paste(tools::file_path_sans_ext(file),
                  "_", max(year(start), period[1]), "-", min(year(end), period[2]),
                  "_", spatial.extent@id,
                  "_", operation, ".Rtable", sep="")
  }
  if (file.exists(fout) && !read.full) {
    if (verbose)
      message(paste("Found '", fout, "'", sep=""))
    DT <- data.table(read.table(fout, header=TRUE, stringsAsFactors=FALSE))
    setkey(DT, Lon, Lat, Year)
  } else {
    days <- 1
    if (verbose)
      message("Get data for ", appendLF=FALSE)
    for (y in year(start):(year(end)-1)) {
      dpy <- as.numeric(as.Date(paste(y+1, "-01-01", sep="")) - as.Date(paste(y, "-01-01", sep="")))
      if (y<period[1]) {
        days = days+dpy
        next
      } else if (y>period[2]) {
        break
      }

      if (verbose)
        message(paste(".", y, ".", sep=""), appendLF=FALSE)

      sdcols <- paste("V", 1:dpy, sep="")

      ## get data
      if (length(dim.names)==2) {
        ## from reduced grid
        if (dim.names[data.dims[1]+1]==time.name) {
          data <- var.get.nc(ncin, data.name, start=c(days, NA), count=c(dpy, NA))
          dt.tmp <- data.table(t(data))
        } else {
          data <- var.get.nc(ncin, data.name, start=c(NA, days), count=c(NA, dpy))
          dt.tmp <- data.table(data)
        }
        dt.tmp[, Lon:=lon, ]
        dt.tmp[, Lat:=lat, ]
        setkey(dt.tmp, Lon, Lat)
      } else {
        ## from 3 dimensional grid
        if (dim.names[1]=="lon" && dim.names[2]=="lat") {
          data <- var.get.nc(ncin, data.name, start=c(NA, NA, days), count=c(NA, NA, dpy))
          data <- data.frame(Lon=rep(lon, length(lat)), Lat=rep(lat, each=length(lon)), V=matrix(data[,,], ncol=dpy))
        } else if (dim.names[1]=="lat" && dim.names[2]=="lon") {
          data <- var.get.nc(ncin, data.name, start=c(NA, NA, days), count=c(NA, NA, dpy))
          data <- data.frame(Lon=rep(lon, each=length(lat)), Lat=rep(lat, length(lon)), V=matrix(data[,,], ncol=dpy))                            
        } else if (dim.names[1]=="time" && dim.names[2]=="lon") {
          data <- var.get.nc(ncin, data.name, start=c(days, NA, NA), count=c(dpy, NA, NA))
          data <- aperm(data, 2,3,1)
          data <- data.frame(Lon=rep(lon, length(lat)), Lat=rep(lat, each=length(lon)), V=matrix(data[,,], ncol=dpy))
        } else if (dim.names[1]=="time" && dim.names[2]=="lat") {
          data <- var.get.nc(ncin, data.name, start=c(days, NA, NA), count=c(dpy, NA, NA))
          data <- aperm(data, 3,2,1)
          data <- data.frame(Lon=rep(lon, length(lat)), Lat=rep(lat, each=length(lon)), V=matrix(data[,,], ncol=dpy))
        }
        colnames(data) <- c("Lon", "Lat", sdcols)
        dt.tmp <- data.table(data)
        setkey(dt.tmp, Lon, Lat)
      }

      dt.tmp <- data.table::melt(dt.tmp, id.vars=c("Lon", "Lat"))
      dt.tmp[, Date:=as.Date(paste(y, "-01-01", sep="")) + as.numeric(substring(variable,2)) - 1, ]

      if (monthly) {
        dt.tmp[, Month:=format(Date, "%b"), ]
        dt.tmp[, Year:=y, ]
        agg.by <- c("Month", "Year")
      } else {
        dt.tmp[, Year:=y, ]
        agg.by <- "Year"
      }

      if (att.get.nc(ncin, data.name, "units")=="K" && operation=="GDD5") 
        dt.tmp[, value := value-273.15, ]

      if (operation=="GDD5") {
        dt.tmp[, value := ifelse(value<5, 0, value), ]
        dt.tmp <- dt.tmp[, list(value=sum(value)), by=c("Lon", "Lat", agg.by)]
      } else if (operation=="mean") {
        dt.tmp <- dt.tmp[, list(value=mean(value, na.rm=TRUE)), by=c("Lon", "Lat", agg.by)]
      } else if (operation=="sum") {
        dt.tmp <- dt.tmp[, list(value=sum(value, na.rm=TRUE)), by=c("Lon", "Lat", agg.by)]
      }

      if (exists("DT", where=sys.frames()[[1]], inherits=FALSE)) {
        DT <- rbind(DT, dt.tmp)
      } else {
        DT <- copy(dt.tmp)
      }
      days = days+dpy
    }
    if (verbose)
      message("")

    if (verbose)
      print(head(DT))
    
    setkeyv(DT, c("Lon", "Lat", agg.by))

    if (write) {
      if (verbose)
        message(paste("Write data to:", fout))
      write.table(DT, file=fout, row.names=FALSE, col.names=TRUE)
    }
  }

  DT <- DT[Lon>extent@xmin & Lon<extent@xmax & Lat>extent@ymin & Lat<extent@ymax,,]

  if (!full) {
    if (period[1] != period[2]) {
      if (monthly) {
        DT <- DT[, list(value=mean(value)), by=c("Lon", "Lat", "Month")]
        DT$Month = factor(DT$Month, levels=month.abb)
        DT <- data.table::dcast(DT, Lon + Lat ~ Month, value.var=c("value"))
      } else {
        DT <- DT[, list(value=mean(value)), by=c("Lon", "Lat")]
      }
    } else {
      DT[, Year:=NULL, ]
    }
  } else if (monthly) {
    DT$Month = factor(DT$Month, levels=month.abb)
    DT <- data.table::dcast(DT, Lon + Lat + Year ~ Month, value.var=c("value"))
  }

  if (is.ModelRun(run)) {
    start <- year(start)
    end <- year(end)
    temporal.extent <- new("TemporalExtent",
                           id = paste(max(start, period[1]), "_", min(end, period[2]), sep=""),
                           name =  paste(max(start, period[1]), "-", min(end, period[2]), sep=""),
                           start = max(start, period[1]), end = min(end, period[2]))
    if (operation=="GDD5") {
      id <- operation
      quant <- new("Quantity",
                   id=id,
                   name="Growing degree days",
                   type="",
                   units="\u00B0C days")
    } else {
      id <- paste(operation, ".", data.name)
      quant <- new("Quantity",
                   id=id,
                   name=att.get.nc(ncin, data.name, "long_name"),
                   type=operation,
                   units=att.get.nc(ncin, data.name, "units"))
    }
    if (full) {
    return(new('ModelObject',
               id=id,
               data=DT,
               quant = quant,
               spatial.extent = spatial.extent,
               temporal.extent = temporal.extent,
               is.site = FALSE,
               is.temporally.averaged = FALSE,
               is.spatially.averaged = FALSE,
               run = as(run, "ModelRunInfo")))
    } else {
      return(new('ModelObject',
                 id=id,
                 data=DT,
                 quant = quant,
                 spatial.extent = spatial.extent,
                 temporal.extent = temporal.extent,
                 is.site = FALSE,
                 is.temporally.averaged = TRUE,
                 is.spatially.averaged = FALSE,
                 run = as(run, "ModelRunInfo")))
    }
  } else {
    return(DT)
  }
}

#' Calculates the seasonality index
#'
#' Calculates a seasonality index based on a monthly vector by default, originally developed for precipitation. With specifying an additional wheighting vector different to months can be used.
#' 
#' @param x values, e.g. monthly precipitation values
#' @param wgt weight for the angle width of each value, e.g. days per month (default)
#' @return the seasonality index following Markhan (1970)
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @references Charles G. Markhan (1970) Seasonality of precipitation in the United States, Annals of the Association of American Geographers, 60:3, 593-597, DOI: 10.1111/j.1467-8306.1970.tb00743.x
seasonalityIndex <- function(x, wgt=c(31, 28.2425, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)) {
  if (!is.vector(x) || !is.vector(wgt))
    stop("Any of 'x'/'wgt' is not a vector")
  if (length(wgt)!=length(x)) {
    message("Length of 'x' and 'wgt' differ. Assuming a regular wgt.")
    warning("Length of 'x' and 'wgt' differ. Assuming a regular wgt.")
    wgt <- rep(1, length(x))
  }
  # calculate the wheighted angles based on wgt
  cwgt <- cumsum(wgt) - wgt/2
  cwgt <- append(cwgt, cwgt[length(cwgt)] + wgt[length(cwgt)]/2)
  angle <- 2 * pi * cwgt / cwgt[length(cwgt)]

  # calculate the x and y component of each vector
  vx <- sin(angle[1:(length(angle)-1)]) * x
  vy <- cos(angle[1:(length(angle)-1)]) * x

  # calculate the "seasonality" by summing up the x and y components
  # of the vectors and divivion by the total precipitation
  seasonality = (sqrt(sum(vx)^2 + sum(vy)^2)) / sum(x)
  return(seasonality)
}
#' Adds a seasonality index column
#' 
#' Adds a column of the seasonality index based on Marhan (1970) to the input data
#' 
#' @param input Input data (either data.frame, data.table or ModelObject) with at least the month abbreviations as column names.
#' @param colname Name of the new created seasonality index column.
#' @param verbose print some information
#' @return same type as input with the seasonality index as new data column
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
addSeasonalityIndex <- function(input=NULL, colname="SI", verbose=TRUE) {
  if (is.data.table(input)) {
    if (verbose)
      message("Input is a data.table")
    if (length(setdiff(month.abb, colnames(input)))!=0)
      stop(paste("No month column(s) in input data: '", paste(setdiff(month.abb, colnames(data)), collapse="', '"), sep=""))
    eval(parse(text=paste("input[, ", colname, ":=apply(.SD, 1, seasonalityIndex), .SDcols=month.abb]", sep="")))
    return(input)
  } else if (is.data.frame(input)) {
    if (verbose)
      message("Input is a data.frame")
    if (length(setdiff(month.abb, colnames(input)))!=0)
      stop(paste("No month column(s) in input data: '", paste(setdiff(month.abb, colnames(input)), collapse="', '"), sep=""))
    eval(parse(text=paste("input$", colname, "=apply(input[, month.abb], 1, seasonalityIndex)", sep="")))
    return(input)
  } else if (is.ModelObject(input, spatial=TRUE)) {
    if (verbose)
      message("Input is a spatial ModelObject.")
    if (length(setdiff(month.abb, colnames(input@data)))!=0)
      stop(paste("No month column(s) in input data: '", paste(setdiff(month.abb, colnames(input@data)), collapse="', '"), sep=""))
    eval(parse(text=paste("input@data[, ", colname, ":=apply(.SD, 1, seasonalityIndex), .SDcols=month.abb]", sep="")))
    return(input)
  } else {
    stop(paste("addSeasonalityIndex: Don't know what to to with class", class(input)))
  }
}


#' daylength calculation
#' 
#' daylength calculation depending on latitude and day of year.
#' taken from function daylengthinsoleet (driver.cpp) LPJ-GUESS v2.1
#' 
#' @param lat latitude (vector)
#' @param doy day of the year (vector)
#' @param leap use February 29th or not
#' @return daylength in hours as vector or matrix, depending on input shape
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
daylength <- function(lat, doy, leap=FALSE) {
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29
  deg2rad <- pi / 180.0
  hh      <- array(0., c(length(lat), length(doy)))

  d <- -23.4 * deg2rad * cos(2.0 * pi * (doy + 10.5) / sum(dom))
  u <- sin(lat * deg2rad) %*% t(sin(d))
  v <- cos(lat * deg2rad) %*% t(cos(d))

  hh[u>-v & u<v] = acos(-u[u>-v & u<v] / v[u>-v & u<v]) 
  hh[u<=-v]      = 0.0
  hh[u>=v]       = pi

  ## daylength in hours
  return(24.0 * t(hh) / pi)
}

#' Calculate the incoming solar radiation based of sunshine/cloudcover
#' 
#' incoming net solar radiation (W m^-2) reduced by albedo,
#' cloud coverage or sun shine fraction, latitude and
#' day of the year as done in function daylengthinsoleet
#' (driver.cpp) LPJ-GUESS v2.1
#' 
#' @param lat latitude (vector)
#' @param doy day of the year (vector)
#' @param rad.frac sunshine/cloudcover. Must have the shape lat x doy
#' @param albedo overall land surface albedo value to be applied (default is the the standard LPJ-GUESS value of 0.17)
#' @param cloudcover logical, if rad.frac is cloudcover (TRUE), default FALSE
#' @param leap use February 29th or not
#' @return incoming solar radiation in W m^-2 as vector or matrix, depending on input shape
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
lpj.radiation <- function(lat, doy, rad.frac, albedo=0.17, cloudcover=FALSE, leap=FALSE) {
  dom <- c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  if (leap) dom[3] = 29

  ## convert fraction to percent
  if (max(rad.frac) <= 1)
    rad.frac <- rad.frac*100

  ## Convert to percent sunshine hours if given as cloudcover
  if (cloudcover) {
    sun <- 100. - rad.frac
  } else {
    sun <- rad.frac
  }

  ## print(c(min(sun), max(sun)))

  QOO <- 1360.0
  A   <- 107.0
  B   <- 0.2
  C   <- 0.25
  D   <- 0.5
  K   <- 13750.98708

  deg2rad <- pi / 180.0
  hh      <- array(0., c(length(lat), length(doy)))

  d <- -23.4 * deg2rad * cos(2.0 * pi * (doy + 10.5) / sum(dom))
  u <- sin(lat * deg2rad) %*% t(sin(d))
  v <- cos(lat * deg2rad) %*% t(cos(d))

  hh[u>-v & u<v] = acos(-u[u>-v & u<v] / v[u>-v & u<v]) 
  hh[u<=-v]      = 0.0
  hh[u>=v]       = pi

  u  <- t(u)
  v  <- t(v)
  hh <- t(hh)

  qo <- QOO*(1.0 + 2.0 * 0.01675 * cos(2.0 * pi * (doy + 0.5) / sum(dom)))

  w <- (C + D * sun / 100.0) * (1.0 - albedo) * qo
  rs_day <- 2.0 * w * (u * hh + v * sin(hh)) * K
  return(rs_day / 86400.)
}
