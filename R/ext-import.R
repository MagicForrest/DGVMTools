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
#' @param run a VegRun object, if a VegSpatial object should be returned, otherwise NA
#' @param file path to the netCDF file
#' @param operation Any of sum, mean, gdd5
#' @param temporal.extent Either a TemporalExtent object, a vector of years (min/max are choosen) or 'NA' (whole netCDF timespan is used)
#' @param spatial.extent Either a SpatialExtent object of a raster extent
#' @param full should the full annual dataset be returned or an multiannual average (default).
#' @param data.name Name of the data field in the netcdf file, if NA it will be guessed
#' @param lon.name Name of the longitude vector in the netcdf file, if NA it will be guessed
#' @param lat.name Name of the latitude vector in the netcdf file, if NA it will be guessed
#' @param time.name Name of the time vector in the netcdf file, if NA it will be guessed
#' @param forceReCalculation Recalculate the desired values (default: FALSE)
#' @param write write the calculated values for fture speedup (default: TRUE)
#' @param verbose print some messages
#' @param ... further so far ignored parameters
#' @return data.table if no VegRun was given, otherwise a VegObject
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import RNetCDF
#' @export
getAnnualClimate <- function(run=NA, file=NA, operation="mean", temporal.extent=NA, spatial.extent=NA, full=FALSE, data.name=NA, lon.name=NA, lat.name=NA, time.name=NA, forceReCalculation=FALSE, write=TRUE, verbose=TRUE, ...) {
  suppressWarnings(if (!is.na(temporal.extent)) {
    if (class(temporal.extent)[1]=="TemporalExtent" && attr(class(temporal.extent), "package")=="RVCTools") {
      period <- c(period@start, period@end)
    } else {
      period <- c(min(temporal.extent, na.rm=TRUE), max(temporal.extent, na.rm=TRUE))
    }
  } else {
    period <- NA
  })

  operation <- tolower(operation)

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

  if (!is.na(spatial.extent) && class(spatial.extent)=="Spatial.Extent" && names(class(spatial.extent))=="RVCTools") {
    extent <- spatial.extent@extent
  } else if (!is.na(spatial.extent) && class(spatial.extent)=="Extent") {
    extent <- spatial.extent
    spatial.extent <- new("SpatialExtent",
                          id="Undefined",
                          name="Undefined",
                          extent=extent)
    warning("Using 'Undefined' as id/name for spatial.extent")
  } else {
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

  ## need also to include soatial extent in file name
  fout <- paste(tools::file_path_sans_ext(file),
                "_", max(year(start), period[1]), "-", min(year(end), period[2]),
                "_", operation, ".Rtable", sep="")
  if (file.exists(fout) && !forceReCalculation) {
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

      if (att.get.nc(ncin, data.name, "units")=="K" && operation=="gdd5") 
        dt.tmp[ ,(sdcols) := lapply(.SD, function(x){x -273.15 }), .SDcols=sdcols]
      if (operation=="gdd5") {
        dt.tmp[ ,(sdcols) := lapply(.SD, function(x){ifelse(x<5, 0, x) }), .SDcols=sdcols]
        eval(parse(text=paste("dt.tmp[ ,Y", y,":=rowSums(.SD), .SDcols=sdcols]", sep="")))
      } else if (operation=="mean") {
        eval(parse(text=paste("dt.tmp[ ,Y", y,":=rowMeans(.SD), .SDcols=sdcols]", sep="")))
      } else if (operation=="sum") {
        eval(parse(text=paste("dt.tmp[ ,Y", y,":=rowSums(.SD), .SDcols=sdcols]", sep=""))) 
      }

      dt.tmp <- dt.tmp[, !sdcols, with=FALSE]

      if (exists("DT")) {
        DT <- DT[dt.tmp]
      } else {
        DT <- copy(dt.tmp)
      }
      days = days+dpy
    }
    if (verbose)
      message("")

    DT <- melt(DT, id.var=c("Lon", "Lat"))
    DT[, Year:=as.numeric(sub("Y", "", variable)), ]
    DT[, variable:=NULL]
    setkey(DT, Lon, Lat, Year)
    
    if (write) {
      if (verbose)
        message(paste("Write data to:", fout))
      write.table(DT, file=fout, row.names=FALSE, col.names=TRUE)
    }
  }
  
  DT <- DT[Lon>extent@xmin & Lon<extent@xmax & Lat>extent@ymin & Lat<extent@ymax,,]

  if (!full) {
    if (period[1] != period[2]) {
      DT <- DT[, list(value=mean(value)), by=c("Lon", "Lat")]
    } else {
      DT[, Year:=NULL, ]
    }
  }
  
  if (is.VegRun(run)) {
    start <- year(start)
    end <- year(end)
    temporal.extent <- new("TemporalExtent",
                           id = paste(max(start, period[1]), "_", min(end, period[2]), sep=""),
                           name =  paste(max(start, period[1]), "-", min(end, period[2]), sep=""),
                           start = max(start, period[1]), end = min(end, period[2]))
    if (operation=="gdd5") {
      id <- operation
      quant <- new("VegQuant",
                   id=id,
                   short.string="GDD5",
                   full.string="Growing degree days",
                   type="",
                   units="\u00B0C days")
    } else {
      id <- paste(operation, ".", data.name)
      quant <- new("VegQuant",
                   id=id,
                   short.string=att.get.nc(ncin, data.name, "standard_name"),
                   full.string=att.get.nc(ncin, data.name, "long_name"),
                   type=operation,
                   units=att.get.nc(ncin, data.name, "units"))
    }
    if (full) {
    return(new('VegObject',
               id=id,
               data=DT,
               quant = quant,
               spatial.extent = spatial.extent,
               temporal.extent = temporal.extent,
               is.site = FALSE,
               is.temporally.averaged = FALSE,
               is.spatially.averaged = FALSE,
               run = as(run, "VegRunInfo")))
    } else {
      return(new('VegObject',
                 id=id,
                 data=DT,
                 quant = quant,
                 spatial.extent = spatial.extent,
                 temporal.extent = temporal.extent,
                 is.site = FALSE,
                 is.temporally.averaged = TRUE,
                 is.spatially.averaged = FALSE,
                 run = as(run, "VegRunInfo")))
    }
  } else {
    return(DT)
  }
}
