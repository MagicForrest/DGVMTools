% File man/import.raster.Rd
\name{import.raster}
\alias{import.raster}
\title{Importing and reprojecting external raster data to data.frame}
\description{
  Importing and external raster data (e.g. GeoTIFF, NetCDF, ...),
  reprojecting it to a target grid and returning it as data.frame with
  the columns c("Lon", "Lat", "value")
}
\usage{
import.raster(file, scale=1, nodata=NA, nodata.limit="eq", method="bilinear", to=raster(nrows=360, ncols=720, xmn=-180, xmx=180, ymn=-90, ymx=90, crs=CRS("+proj=longlat +ellps=WGS84")), mem.only=FALSE)
}
\arguments{
  \item{file}{input file name}
  \item{scale}{scale factor the imported data is multiplied with.}
  \item{nodata}{value containing invalid data.}
  \item{nodata.limit}{is the 'nodata' exact, or a threshold. valid
    values are "eq", "lt", "le", "ge" or "gt".}
  \item{method}{method used by 'projectRaster' for spatial
    interpolation.}
  \item{to}{reference grid the data should be projected to. Default:
    global 0.5° grid.}
  \item{mem.only}{raster is very disk or memory consuming. If TRUE, this
    option sets a high enough limit with 'rasterOptions', that all
    operations are in RAM (or swapped, if you are not lucky). A warning
    is issued when the demand exceeds 4Gb.}
}
\value{
 data.frame with the columns c("Lon", "Lat", "value")
}
\author{
  Joerg Steinkamp <joerg.steinkamp@senckenberg.de>
}
