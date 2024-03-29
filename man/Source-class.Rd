% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{Source-class}
\alias{Source-class}
\title{Metadata for a data source}
\description{
This class describes a data source, including its location on disk, the format (ie from which model or dataset it came), the PFT set used, 
an unique id and a description, offsets to apply to the longitudes and latitudes to make the co-rordinates gridcell centered and so on.
It is not primarily intended to be used by itself. Instead it is inherited by\code{\linkS4class{Source}} object (due to this inheritance the slots can 
be accessed directly) and included in a \linkS4class{Field} in the \code{@source} slot (not inherited, so needs to be access be \code{@source}).
}
\details{
Slots can be accessed and modified by the user directly, but also by functions \link{defineQuantity} and \link{defineLayer} with the \code{add.to} argument.
}
\section{Slots}{

\describe{
\item{\code{id}}{A unique character string to identify this particular data source.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)}

\item{\code{dir}}{The location of this run on the file system (Mandatory)}

\item{\code{format}}{A character string or a \linkS4class{Format} object to describe how this data is stored on disk.  This will depend on either the model 
that was used to produce it or the dataset type.  Currently defined Format objects are \code{GUESS}, \code{aDGVM}, \code{aDGVM2} and \code{NetCDF}. (Mandatory)}

\item{\code{defined.layers}}{A list of \linkS4class{Layer} objects to describe the Layers, includes the PFTs, included in this model run/dataset.}

\item{\code{name}}{A character string describing this data source, ie. "LPJ-GUESS v3.1" (can be omitted, in which case the id will be used instead)}

\item{\code{forcing.data}}{A character string identifying the climate or other data used to produce this data}

\item{\code{lonlat.offset}}{A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.}

\item{\code{year.offset}}{A numeric of length 1 to match be added to the simulation years to convert them to calendar years}

\item{\code{london.centre}}{If TRUE, ensure that the longitudes are (-180,180) instead of (0,360)}

\item{\code{land.use.included}}{If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.}

\item{\code{contact}}{Name and email address of responsible person.}

\item{\code{institute}}{Name of the institute (default "none").}
}}

\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
