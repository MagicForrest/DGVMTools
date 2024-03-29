% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/crop-methods.R
\name{crop-methods}
\alias{crop-methods}
\alias{crop}
\alias{crop,Field-method}
\alias{crop,Comparison-method}
\alias{crop,data.table-method}
\title{Crop methods}
\usage{
\S4method{crop}{Field}(x, y, spatial.extent.id = NULL, ...)

\S4method{crop}{Comparison}(x, y, spatial.extent.id = NULL, ...)

\S4method{crop}{data.table}(x, y, spatial.extent.id = NULL, ...)
}
\arguments{
\item{x}{A Field or Comparison or a stand-alone data.table to be cropped}

\item{y}{Anything from which raster::Extent object can be derived}

\item{spatial.extent.id}{A character string to describe the spatial extent (y) required for meta-data consistency.
Must be provided when cropping \code{\linkS4class{Field}} or \code{\linkS4class{Comparison}} objects}

\item{...}{Other arguments, not currently used}
}
\value{
A spatially cropped object
}
\description{
Methods for cropping DGVMTools objects (and data.tables) to each other, to raster and sp objects (and basically anything that can be coerced in a raster::Extent object). 
The data.table in the "data" slot of the DGVMTools object (or the stand-alone data.table), must have columns named "Lon" and "Lat" otherwise these methods fail.  Unpleasantly.
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
