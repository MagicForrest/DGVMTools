import.raster <- function(file, scale=1, nodata=NA, nodata.limit="eq", method="bilinear", to=raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+proj=longlat +ellps=WGS84")), mem.only=FALSE) {
## !!! execution on large memory machine recommended !!!
  require(raster)

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
