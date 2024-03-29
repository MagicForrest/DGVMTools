% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getField.R
\name{getField}
\alias{getField}
\title{Get a \code{\linkS4class{Field}} from a \code{\linkS4class{Source}}}
\usage{
getField(
  source,
  quant,
  layers = NULL,
  file.name = NULL,
  first.year = NULL,
  last.year = NULL,
  year.aggregate.method = NULL,
  spatial.extent = NULL,
  spatial.extent.id = NULL,
  spatial.aggregate.method = NULL,
  subannual.resolution = NULL,
  subannual.original = NULL,
  subannual.aggregate.method = NULL,
  sta.info = NULL,
  quick.read.file = NULL,
  quick.read.autodelete = FALSE,
  verbose = FALSE,
  ...
)
}
\arguments{
\item{source}{The \code{\linkS4class{Source}} object for which the \code{\linkS4class{Field}} should be built, typically a model run or a dataset.}

\item{quant}{The \\code{\linkS4class{Quantity}} to be read - either a \code{\linkS4class{Quantity}} object or a string containing its \code{id}.  If it is a character string it will be checked
against the predefined \code{Quantities} in the \code{\linkS4class{Source}} object and failing that a simple dummy \code{\linkS4class{Quantity}} will be be made.  For the \code{NetCDF Format}
most \code{\linkS4class{Quantity}} metadata will be taken from the NetCDF file where possible (thus overriding this option).}

\item{layers}{A list (or vector of character) of character strings to specify which Layers should be read from the file.
If missing or NULL then all Layers are read.
-NOTE- Using this arguments is not recommended for reading gzipped output with the \code{GUESS} Format since it involves gunzipping the file twice, 
which likely makes it less time efficient than simply reading all the layers and then dropping the ones you don't want.}

\item{file.name}{Character string specifying the file name (not the full path, just the file name, or the file name relative to source@dir) where the data is stored.
For the \code{GUESS}, \code{aDGVM} and \code{aDGVM2} Formats this is optional under normal circumstances this is optional since the file names are normally 
standardised (although they have been renamed). However for the \code{NetCDF} Format this is pretty much always essential because random netCDF files don't tend to have 
standardised file names in the same way that model output does.  Leave missing or set to \code{NULL} to use the standard file name for the particular Format.}

\item{first.year}{The first year (as a numeric) of the data to be returned (if not specified or NULL start from the beginning of the data set)}

\item{last.year}{The last year (as a numeric) of the data to be returned (if not specified or NULL take the data to the end of the data set)}

\item{year.aggregate.method}{A character string describing the method by which to annual aggregate the data.  Leave blank to apply no annual aggregation. Can currently be "mean", "sum", "max", "min", "sd", "var and "cv" (= coefficient of variation: sd/mean).
For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
See \code{\link{aggregateYears}}}

\item{spatial.extent}{An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.}

\item{spatial.extent.id}{A character string to give an identifier for the spatial extent this Field covers.}

\item{spatial.aggregate.method}{A character string describing the method by which to spatially aggregate the data.  Leave blank to apply no spatial aggregation. Can currently be "weighted.mean"/"w.mean", "mean", 
"weighted.sum"/"w.sum", "sum", "max", "min", "sd", "var and "cv" (= coefficient of variation: sd/mean).  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
See \code{\link{aggregateSpatial}}}

\item{subannual.resolution}{A character string specifying the subannual resolution that you want.  Can be "Year", "Month" or "Day".}

\item{subannual.original}{A character string specifying the subannual you want the data to be on before applying the subannual.aggregate.method. 
Can be "Year", "Month" or "Day".  Currently ignored.}

\item{subannual.aggregate.method}{A character string specifying the method by which to aggregate the data subannually,  can be "mean", "sum", "max", "min", "sd", "var and "cv" (= coefficient of variation: sd/mean)
See \code{\link{aggregateSubannual}}}

\item{sta.info}{Optionally an \code{\linkS4class{STAInfo}} object defining the exact spatial-temporal-annual domain over which the data should be retrieved.
Can also be a Field object from which the STA info will de derived.
If specified the following 9 arguments are ignored (with a warning)}

\item{quick.read.file}{A character string.  If set, the function will look for a file of this name (plus the extension ".RData") in the run directory.
If if finds one then it reads that.  If it doesn't find the appropriate file, then it reads the raw data, performs whatever cropping and aggregating is necessary,
and then saves the data in a file of that name (in the run directory) for use next time.  (Note: the function also checks that the specified layers, cropping and aggregating 
found in the file match that which was requested by the arguments here).}

\item{quick.read.autodelete}{If TRUE then the file specified by the above "quick.read.file" argument will be deleted, thus ensuring that the raw data will be read afresh 
(and saved again).  Ignored if valid "quick.read.file" argument not supplied.}

\item{verbose}{If TRUE give a lot of information for debugging/checking.}

\item{...}{Other arguments that are passed to the getField function for the specific Format or additional arguements for selecting space/time/years.
For all Formats, the followings arguments apply:
\itemize{
 \item{\code{cover.fraction}}  When selecting gridcells based on a SpatialPolygonsDataFrame (ie from a shapefile) as the \code{spatial.extent} argument, this optional arguement determines 
 how much of the gridcell needs to be in the the polygon for it to be selected. Should be between 0 and 1.
}
For the aDGVM(1) Format, the following arguments apply:
\itemize{
 \item{\code{adgvm.file.type}}  This character string argument specifies from which file to read the data.
 This can be one of "Yearly", "Sys", "Fire", "Soil" or "Size".  The default is "Yearly", which is sensible because the yearly file is always written. 
 \item{\code{adgvm.fire}}  This numeric argument (taking values 0 or 1) specifies to take a run with fire on (1) or off (0). Default is 1.
 \item{\code{adgvm.climate}}   This numeric argument (taking values 0 or 1) specifies to take a run with constant (0) or transient (1) climate.  Default is 0.
 \item{\code{adgvm.header}} If your aDGVM run has been has extra columns added to the output tables, use this argument
 to specify the column names.  For the default column names see the source file \code{aDGVM-Format.R}.
}
For the aDGVM2 Format, the following arguments apply:
\itemize{
 \item{\code{adgvm2.scheme}}  This numeric argument defines the aDGVM PFT scheme which can be 1 or 2.
 \item{\code{adgvm2.daily}}  A logical, set to true to read daily data (only for \code{adgvm2.scheme=1} and if daily data are provided in pop file)
}
For the NetCDF Format, the following arguments apply:
 \itemize{
 \item{\code{nc.verbose}} A logical, set to true to give progress/debug information from the ncdf4 package functions.  This can be a lot, 
so it is handy to control that separately.
 \item{\code{calendar}} Character string, sometimes the calendar string on the time axis can be incorrect or missing.  Here you can manually provide it.
Note: A common error in paleo files is "standard" instead of "proleptic_gregorian". Specifically, if you have dates with years before 1582 
(the start of the Gregorian calendar) and it includes leap years the calendar needs to be set to "proleptic_gregorian".
}}
}
\value{
A \code{\linkS4class{Field}}.
}
\description{
Given a \code{\linkS4class{Source}} object and a \code{\linkS4class{Quantity}} object, return an appropriate spatially/temporal/annually-aggregated \code{\linkS4class{Field}} object, optionally including
spatial, temporal and annual cropping.
}
\details{
Note that because there are three types of aggregating available, the resulting \code{\linkS4class{Field}} object can have a wide select of spatio-temporal dimensions.
To check what dimensions you have you can use \code{\link{getDimInfo}}
}
\seealso{
\code{\link{aggregateSubannual}}, \code{\link{aggregateSpatial}}, \code{\link{aggregateYears}}, \code{\link{getDimInfo}}
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
