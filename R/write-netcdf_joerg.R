

#' Create a NetCDF4 file from a \code{\link[DGVMTools]{ModelObject}}.
#'
#' @param filename output file name
#' @param mo the \code{\link[DGVMTools]{ModelObject}.
#' @param columns which colums to write as variable (TODO: option as dimension)
#' @param flux flux unit parseable by \code{\link{udunits2}}. Converts the data to flux. Requires package udunits2 to be installed.
#' @param globalAttr a named vector of additional global attributes.
#' @param compress should NetCDF4 compression be turned on. Either TRUE (compression level 4) of an integer between 1 and 9.
#' @param chunks chunk size for faster readability. If set make sure its length agrees with the data dimensions.
#' @param reduce reduced grid, only valid points are written. Spatial dimensions are reduced to one landID (TODO, currently not implemented).
#' @param leap turn leap years on (default off; TODO, currently not implemented).
#' @param invertlat latitude dimension starts in the north.
#' @param verbose print some information
#'
## TODO: Add daily data compatibility
write.nc <- function(filename=NA, mo=NA, columns=NA, flux=FALSE, globalAttr=NULL,
                     compress=NA, chunks=NA, reduce=FALSE, invertlat=FALSE, leap=FALSE, verbose=FALSE) {
  ## check mandatory input
  if (is.na(filename))
    stop("No filename given!")
  if (class(mo) != "ModelObject" && attr(class(mo), "package") != "DGVMTools")
    stop("'mo' is not a DGVMTools::ModelObject!")
  
  ## set compression
  if (!is.na(compress))
    if (is.logical(compress) && compress)
      compress=4
    
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
        if (invertlat) {
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
      stop(paste("Something went wrong 'daily' and 'monthly' are both TRUE.\n",
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
      years <- extract.seq(mo@data$Year)
      if (!monthly && !daily) {
        if (verbose)
          message("Annual data.")
        time <- is.leapyear(years, doy=TRUE)
        dims[['time']] <- ncdim_def("time", paste0("days since ", years[1], "-01-01 00:00:00"),
                                    cumsum(time), unlim=TRUE, create_dimvar=TRUE)
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
        dims[['time']] <- ncdim_def("time", paste0("days since ", years[1], "-01-01 00:00:00"),
                                    cumsum(time), unlim=TRUE, create_dimvar=TRUE)
      } else if (daily) {
        if (verbose)
          message("Daily data.")
        stop("'daily' still work in progress.")
      }
    } else if (!monthly && !daily) {
      if (verbose)
        message("Annual data (multi-annual average).")
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
    
    
    
    
    ## TODO: convert to flux if requested
    unit <- mo@quant@units
    if (flux) {
      if (!requireNamespace("udunits", quietly = TRUE)) {
        stop("Package 'udunits2' not present. Conversion to flux not possible!")
      } else {
        if (grepl("[CN]", mo@quant@units) && verbose)
          message("Removing 'C' and/or 'N' from unit string.")
        unit <- sub("[CN]", "", unit)
        if (!udunits2::ud.is.parseable(units))
          stop(paste0("Unit string '", unit, "' is not parseable by udunits2!"))
      }
    }
    
    
    
    
    ## define the variables based on columns (only if not monthly),
    ## otherwise take the quant@id
    if (!monthly) {
      if (length(columns)==1)
        if (is.na(columns))
          columns = colnames(mo@data)[! colnames(mo@data) %in% c("Lon", "Lat", "Year", "Day", "landid")]
        for (name in columns)
          vars[[name]] <- ncvar_def(name, unit, dims, longname=paste0(mo@quant@name, " (", name, ")"), prec="float",
                                    compression=compress, chunksizes=chunks)
    } else {
      vars[[mo@quant@id]] <- ncvar_def(mo@quant@id, unit, dims, longname=mo@quant@name,
                                       prec="float", compression=compress, chunksizes=chunks)
    }
    
    ## create the NetCDF file
    ncout <- nc_create(filename, vars)
    
    ## write lon/lat as variables for reduced grid
    if (reduce) {
      ncvar_put(ncout, 'lon', lon)
      ncvar_put(ncout, 'lat', lat)
    }
    
    
    print(str(mo@data))
    
    
    ## transform the data in a suitable araay format/shape
    if (monthly) {
      if (reduce) {
        if (mo@is.temporally.averaged) {
          data <- data.table::melt(mo@data, id.vars=c("landid"), measure.vars=month.abb)
          ncvar_put(ncout, mo@quant@id, data$value)
        } else {
          data <- data.table::melt(mo@data, id.vars=c("landid", "Year"), measure.vars=month.abb)
          data[, Year:= Year* 100 + as.numeric(variable)]
          print(str(data))
          ncvar_put(ncout, mo@quant@id, acast(data, landid~Year, value.var="value"))
        }
      } else {
        data <- modelObject2Array(mo@data, FALSE, invertlat)
        ncvar_put(ncout, mo@quant@id, data)
      }
    } else if (daily) {
      
    } else {
      for (name in columns) {
        if (reduce) {
          if (mo@is.temporally.averaged) {
            ncvar_put(ncout, name, eval(parse(text=paste0("mo@data$", name))))
          } else {
            ncvar_put(ncout, name, acast(mo@data, landid~Year, value.var=name))
          }
        } else {
          data <- modelObject2Array(mo@data, name, invertlat)
          ncvar_put(ncout, name, data)
        }
      }
    }
    
    ncatt_put(ncout, "lon" , "standard_name", "longitude")
    ncatt_put(ncout, "lat" , "standard_name", "latitude")
    if (!mo@is.temporally.averaged) {
      ncatt_put(ncout, "time" , "standard_name", "time")
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
        message("'globalAttr' should be a named vector. Ignoring it!")
        warning("'globalAttr' should be a named vector. Ignoring it!")
      }
    }
    ncatt_put(ncout, 0, "Conventions", "CF-1.6")
    nc_sync(ncout)
    nc_close(ncout)  
}
