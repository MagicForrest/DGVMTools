

#' Create a NetCDF4 file from a \code{\linkS4class{ModelObject}}.
#'
#' @param filename output file name
#' @param mo the \code{\linkS4class{ModelObject}}.
#' @param columns which colums to write as variable (TODO: option as dimension)
#' @param as.flux Converts the data to fluxes by dividing by the respective number of days, if TRUE or "day[s]". If set to "^s*", the divided by 86400 additionally.
#' @param fill.value value to use for _FillValue variable attribute.
#' @param invert.lat latitude dimension starts in the north.
#' @param globalAttr a named vector of additional global attributes.
#' @param compress should NetCDF4 compression be turned on. Either TRUE (compression level 4) of an integer between 1 and 9.
#' @param chunks chunk size for faster readability. If set make sure its length agrees with the data dimensions.
#' @param reduce reduced grid, only valid points are written. Spatial dimensions are reduced to one landID (TODO, currently not implemented).
#' @param start.year start year for units attribute of time axis.
#' @param time.unit unit for time axis units attribute, so far "days", "months" and "years" are valid. Default: "days".
#' @param leap turn leap years on (default off; TODO, currently not implemented).
#' @param verbose print some information
#'
#' @import data.table
#' @import ncdf4
#' @importFrom reshape2 acast
## TODO: Add daily data compatibility
write.nc <- function(filename=NA, mo=NA, columns=NA, as.flux=FALSE, fill.value=FALSE, invert.lat=FALSE, 
                     globalAttr=NULL, compress=NA, chunks=NA, reduce=FALSE,
                     leap=FALSE, start.year=NA, time.unit="days",
                     verbose=FALSE) {

  Lon=Lat=Year=variable=NULL

  if (!is.null(fill.value)) {
    if (is.na(fill.value)) {
      fill.value = NULL
    } else if (is.logical(fill.value) && !fill.value) {
      fill.value = NULL
    }
  }
  
  ## check mandatory input
  if (is.na(filename))
    stop("No filename given!")
  if (class(mo) != "ModelObject" && attr(class(mo), "package") != "DGVMTools")
    stop("'mo' is not a DGVMTools::ModelObject!")

  ## set compression
  if (!is.na(compress)) {
    if (is.logical(compress) && compress) {
      compress <- 4
    } else if (!is.numeric(compress) || compress < 1 || compress > 9) {
      compress <- 4
    }
  }

  ## check, if flux is desired
  if (as.flux != FALSE) {
    if (grepl("^d", as.flux)) {
      as.flux = 1.0
    } else if (grepl("^h", as.flux)) {
      as.flux = 24.0
    } else if (grepl("^m", as.flux)) {
      as.flux = 24.0 * 60.0
    } else if (grepl("^s", as.flux)) {
      as.flux = 24.0 * 60.0 * 60.0
    } else if (!is.numeric(as.flux)) {
      stop(paste0("Don't what to do with 'as.flux=", as.flux, "'!"))
    }
  }
  
  ## define spatial dimensions
  ## landid if 'reduce' is TRUE, otherwise lat/lon grid
  dims <- list()
  vars <- list()
  if (reduce) {
    if ("Year" %in% colnames(mo@data)) {
      nlandid <- nrow(mo@data[Year==min(mo@data$Year), ])
      lon <- mo@data[Year==min(mo@data$Year), ]$Lon
      lat <- mo@data[Year==min(mo@data$Year), ]$Lat
    } else {
      nlandid <- nrow(mo@data)
      lon <- mo@data$Lon
      lat <- mo@data$Lat
    }
    dims[['landid']] <- ncdim_def("landid", "-", 0:(nlandid - 1), unlim=FALSE, create_dimvar=TRUE, longname="LandID")
    vars[['lon']] <- ncvar_def('lon', 'degrees_east', dims[['landid']], longname="longitude", prec="float")
    vars[['lat']] <- ncvar_def('lat', 'degrees_north', dims[['landid']], longname="latitude", prec="float")
    landid.dt <- data.table(Lon=lon, Lat=lat, landid=0:(nlandid-1))
    setkey(landid.dt, Lon, Lat)
    mo@data = mo@data[landid.dt]    
  } else {
    if (!mo@is.spatially.averaged) {
      lon <- extract.seq(mo@data$Lon)
      dims[['lon']] <- ncdim_def("lon", "degrees_east", lon, unlim=FALSE, create_dimvar=TRUE, longname="longitude")
      if (invert.lat) {
        lat <- extract.seq(mo@data$Lat, descending=TRUE)
      } else {
        lat <- extract.seq(mo@data$Lat)
      }
      dims[['lat']] <- ncdim_def("lat", "degrees_north", lat, unlim=FALSE, create_dimvar=TRUE, longname="latitude")
    }
  }

  ## temporal (unlimited) dimension
  monthly <- FALSE
  if (all(month.abb %in% colnames(mo@data)))
    monthly <- TRUE
  daily <- FALSE
  if (c("Day") %in% colnames(mo@data))
    daily <- TRUE
  if (daily && monthly)
    stop(paste("Something went wrong: 'daily' and 'monthly' are both TRUE.\n",
               paste(colnames(mo@data), collapse=", ")))

  ## TODO: check that if statements are correct for annual, monthly and daily data
  ##       and temporal averaging is on or off respectivly
  ##       Usefull combinations:
  ##       annually:
  ##       !mo@is.temporally.averaged && !monthly && !daily   ok (normal & reduced)
  ##       !mo@is.temporally.averaged && monthly && !daily    ok (normal & reduced)
  ##       !mo@is.temporally.averaged && !monthly && daily
  ##       multi-annual averages:
  ##       mo@is.temporally.averaged && !monthly && !daily    ok (normal & reduced)
  ##       mo@is.temporally.averaged && monthly && !daily     ok (normal & reduced)
  ##       daily makes not really sense, or does it?
  if (!mo@is.temporally.averaged) {
    years <- sort(unique(mo@data$Year))
    if (leap) {
      time <- is.leapyear(years, doy=TRUE)
    } else {
      time <- rep(365, length(years))
    }
    if (is.na(start.year))
      start.year = years[1]
      nc.time.offset = 0
    if (!monthly && !daily) {
      if (verbose)
        message("Annual data.")
      if (time.unit == "days") {
        if (start.year == years[1]) {
          nc.time.offset <- 0
        } else if (leap && start.year < years[1]) {
          time.offset <- sum(is.leapyear(start.year:(years[1]-1), doy=TRUE))
        } else if (start.year < years[1]) {
          nc.time.offset = 365 * (years[1] - start.year)
        } else {
          stop(paste0("start.year (", start.year, ") is later than first record (", years[1],")!"))
        }
        nc.time <- time
      } else if (time.unit == "years") {
        nc.time <- rep(1, length(years))
        if (start.year == years[i]) {
          nc.time.offset <- 0
        } else if (start.year < years[1]) {
          nc.time.offset <- years[i] - start.year
        } else {
          stop(paste0("start.year (", start.year, ") is later than first record (", years[1],")!"))
        }
      } else {
        stop(paste0("time.unit '", time.unit, "' not compatible with annual data."))
      }
      dims[['time']] <- ncdim_def("time", paste0(time.unit, " since ", start.year, "-01-01 00:00:00"),
                                  cumsum(nc.time) + nc.time.offset, unlim=TRUE, create_dimvar=TRUE)
    } else if (monthly) {
      if (verbose)
        message("Monthly data.")
      time <- sapply(years, function(x) {
        if (leap && is.leapyear(x)) {
          c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        } else {
          c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        }
      })
      if (time.unit == "days") {
        if (start.year == years[1]) {
          time.offset <- 0
        } else if (leap && start.year < years[1]) {
          nc.time.offset <- sum(is.leapyear(start.year:(years[1]-1), doy=TRUE))
        } else if (start.year < years[1]) {
          nc.time.offset <- 365 * (years[1] - start.year)
        } else {
          stop(paste0("start.year (", start.year, ") is later than first record (", years[1],")!"))
        }
        nc.time <- time
      } else if (time.unit == "months") {
        nc.time <- rep(1, 12 * length(years))
        if (start.year == years[1]) {
          nc.time.offset <- 0
        } else if (start.year < years[1]) {
          nc.time.offset <- 12 * (years[1] - start.year)
        } else {
          stop(paste0("start.year (", start.year, ") is later than first record (", years[1],")!"))
        }
      } else {
        stop(paste0("time.unit '", time.unit, "' not compatible with monthly data."))
      }
      dims[['time']] <- ncdim_def("time", paste0(time.unit, " since ", start.year, "-01-01 00:00:00"),
                                  cumsum(nc.time) + nc.time.offset, unlim=TRUE, create_dimvar=TRUE)
    } else if (daily) {
      if (verbose)
        message("Daily data.")
      stop("'daily' still work in progress.")
    }
  } else if (!monthly && !daily) {
    stop("multiannual averages currently not working!")
    if (verbose)
      message("Annual data (multi-annual average).")
    time <- 365
    if (leap)
      time <- 365.25
  } else if (monthly) {
    if (verbose)
      message("Monthly data (multi-annual average).")
    if (leap) {
      time <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    } else {
      time <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    }
    dims[['time']] <- ncdim_def("time", paste0("days since ", mo@temporal.extent@end, "-01-01 00:00:00"),
                                cumsum(time), unlim=TRUE, create_dimvar=TRUE)
  } else if (daily) {
    stop("Daily data as multi-annual average does not really make sense!")
  }

  if (verbose) {
    if (reduce) {
      message(paste0(" * dimensions (landid x time): ", nlandid, " x ",
                     ifelse(is.null(dim(time)), length(time), prod(dim(time)))))
    } else {
      message(paste0(" * dimensions (lon x lat x time): ", length(lon), " x ",length(lat), " x ",
                     ifelse(is.null(dim(time)), length(time), prod(dim(time)))))
    }
  }

  ## TODO: convert to other flux units with udunits2.
  ## However, that adds another dependency 
  ## and [CN] must be removed from the units string.
  unit <- mo@quant@units
  if (as.flux==1.0) {
    unit = paste0(unit, " day-1")
    message("*** FLUX conversion still experimental. Not yet validated properly ***")
    warning("*** FLUX conversion still experimental. Not yet validated properly ***")
  } else if (as.flux==24.0) {
    unit = paste0(unit, " h-1")
    message("*** FLUX conversion still experimental. Not yet validated properly ***")
    warning("*** FLUX conversion still experimental. Not yet validated properly ***")
  } else if (as.flux==24.0 * 60.0) {
    unit = paste0(unit, " min-1")
    message("*** FLUX conversion still experimental. Not yet validated properly ***")
    warning("*** FLUX conversion still experimental. Not yet validated properly ***")
  } else if (as.flux==24.0 * 60.0 * 60.0) {
    unit = paste0(unit, " s-1")
    message("*** FLUX conversion still experimental. Not yet validated properly ***")
    warning("*** FLUX conversion still experimental. Not yet validated properly ***")
  } else if (as.flux) {
    unit = paste0(unit, " (", as.flux, ")-1")
    message(paste0("*** FLUX: unknown divisor: '", as.flux,"'  ***"))
    warning(paste0("*** FLUX: unknown divisor: '", as.flux,"'  ***"))
    message("*** FLUX conversion still experimental. Not yet validated properly ***")
    warning("*** FLUX conversion still experimental. Not yet validated properly ***")
  }

  ## define the variables based on columns (only if not monthly),
  ## otherwise take the quant@id
  if (!monthly) {
    if (length(columns)==1)
      if (is.na(columns))
        columns = colnames(mo@data)[! (colnames(mo@data) %in% c("Lon", "Lat", "Year", "Day", "landid"))]
      for (name in columns)
        vars[[name]] <- ncvar_def(name, unit, dims, fill.value, longname=paste0(mo@quant@name, " (", name, ")"), prec="float",
                                  compression=compress, chunksizes=chunks)
  } else {
    vars[[mo@quant@id]] <- ncvar_def(mo@quant@id, unit, dims, fill.value, longname=mo@quant@name,
                                     prec="float", compression=compress, chunksizes=chunks)
  }

  ## create the NetCDF file
  if (verbose)
    message(paste0(" * Opening '", filename, "' for writing."))
  ncout <- nc_create(filename, vars)

  ## write lon/lat as variables for reduced grid
  if (reduce) {
    ncvar_put(ncout, 'lon', lon)
    ncvar_put(ncout, 'lat', lat)
  }

  ## transform the data in a suitable araay format/shape
  if (monthly) {
    if (verbose)
      message(paste0(" * Writing '", mo@quant@id,"'."))
    if (reduce) {
      if (mo@is.temporally.averaged) {
        data <- data.table::melt(mo@data, id.vars=c("landid"), measure.vars=month.abb)
        data <- array(data$value, c(nlandid, length(time)))
        if (as.flux)
          data <- data / array(rep(time * as.flux, each=nlandid), dim(data))
        ncvar_put(ncout, mo@quant@id, data)
        ncatt_put(ncout, mo@quant@id, "coordinates", "lon lat")
      } else {
        data <- data.table::melt(mo@data, id.vars=c("landid", "Year"), measure.vars=month.abb)
        data[, Year:= Year* 100 + as.numeric(variable)]
        data <- acast(data, landid~Year, value.var="value")
        if (as.flux)
          data <- data / array(rep(time * as.flux, each=nlandid), dim(data))
        ncvar_put(ncout, mo@quant@id, data)
      }
    } else {
      data <- modelObject2Array(mo@data, FALSE, invert.lat, verbose=verbose)
      if (as.flux)
        data <- data / array(rep(time * as.flux, each=length(lon) * length(lat)), dim(data))
      ncvar_put(ncout, mo@quant@id, data)
    }
  } else if (daily) {

  } else {
    for (name in columns) {
      if (verbose)
        message(paste0(" * Writing '", name,"'."))
      if (reduce) {
        if (mo@is.temporally.averaged) {
          data <- eval(parse(text=paste0("mo@data$", name)))
          if (as.flux)
            data <- data / (time * as.flux)
          ncvar_put(ncout, name, data)
          ncatt_put(ncout, name, "coordinates", "lon lat")
        } else {
          data <- acast(mo@data, landid~Year, value.var=name)
          if (as.flux)
            data <- data / array(rep(time * as.flux, each=nlandid), dim(data))
          ncvar_put(ncout, name, data)
        }
      } else {
        data <- modelObject2Array(mo@data, name, invert.lat, verbose=verbose)
        if (as.flux && mo@is.temporally.averaged) {
          data <- data / time
        } else if (as.flux) {
          data <- data / array(rep(time * as.flux, each=length(lon) * length(lat)), dim(data))
        }
        ncvar_put(ncout, name, data)
      }
    }
  }

  ## Writing additional attributes
  if (reduce)
    ncatt_put(ncout, "landid", "compress", "lon lat")

  ncatt_put(ncout, "lon" , "standard_name", "longitude")
  ncatt_put(ncout, "lat" , "standard_name", "latitude")
  ncatt_put(ncout, "lon" , "axis", "X")
  ncatt_put(ncout, "lat" , "axis", "Y")
  if (!mo@is.temporally.averaged) {
    ncatt_put(ncout, "time" , "standard_name", "time")
    ncatt_put(ncout, "time" , "axis", "T")
    if (leap) {
      ncatt_put(ncout, "time" , "calendar", "gregorian")
    }else {
      ncatt_put(ncout, "time" , "calendar", "noleap")
    }
  } else {
    ncatt_put(ncout, 0 , "temporal_extent", paste0(mo@temporal.extent@start, "-",mo@temporal.extent@end))
  }
  if (!is.null(globalAttr)) {
    if (length(globalAttr)==1 && is.logical(globalAttr)) {
      if (globalAttr) {
        ncatt_put(ncout, 0, "Model", mo@run@model)
        ncatt_put(ncout, 0, "Name", mo@run@name)
        ncatt_put(ncout, 0, "Driving_data", mo@run@driving.data)
        if (mo@run@contact != "")
          ncatt_put(ncout, 0, "Contact", mo@run@contact)
        if (mo@run@institute != "" || mo@run@institute != "none")
          ncatt_put(ncout, 0, "Institute", mo@run@institute)
      }
    } else if (is.vector(globalAttr)) {
      for (name in names(globalAttr))
        ncatt_put(ncout, 0, name, globalAttr[name])
    } else {
      message("'globalAttr' should be logical or a named vector. Ignoring it!")
      warning("'globalAttr' should be logical or a named vector. Ignoring it!")
    }
  }
  ncatt_put(ncout, 0, "Conventions", "CF-1.6")
  nc_sync(ncout)
  nc_close(ncout)
}


