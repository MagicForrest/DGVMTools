% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/defineSource.R
\name{defineSource}
\alias{defineSource}
\title{Define a Source object that represents a model run or data set, setting up all the required metadata (only).}
\usage{
defineSource(
  id,
  name,
  dir,
  format,
  quantities = NULL,
  defined.layers = list(),
  lonlat.offset = c(0, 0),
  year.offset = 0,
  london.centre = TRUE,
  forcing.data = "Not specified",
  land.use.included = FALSE,
  contact = "Not specified",
  institute = "Not specified",
  ...
)
}
\arguments{
\item{id}{A unique character string to identify this particular data.  Recommended to be alphanumeric because it is used to construct file names and to use underscores and full stops (periods) 
for separating characters where need be.   For example, "Standard_v4.0" or "Saatchi2011.HD"  (can be derived from \code{name} if a \code{name} is supplied, otherwise mandatory)}

\item{name}{A character string describing this run, ie. "LPJ-GUESS v3.1" (can be derived from \code{id} if an \code{id} is supplied, otherwise mandatory)}

\item{dir}{The location of this source on the file system (Mandatory)}

\item{format}{A Format object to describe the type of source.  Can be GUESS, aDGVM, aDGVM2 or NetCDF (NOTE no quotes since these are actual R objects not strings).}

\item{quantities}{A list of Quantity object what we expect to have in this Source.  The option allows the over-riding the default quantities in the Format object.
Not 100\% sure why this might be useful though.}

\item{defined.layers}{A list of PFT objects which includes all the PFTs used is this model run (Mandatory)}

\item{lonlat.offset}{A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the localities, somewhat legacy for old LPJ-GUESS runs.}

\item{year.offset}{A numeric of length 1 to match be added to the years to convert them to calendar years,  somewhat legacy for old LPJ-GUESS runs.}

\item{london.centre}{If TRUE, ensure that the longitudes are (-180,180) instead of (0,360)}

\item{forcing.data}{A character string identifying the climate or other data used to produce this model run}

\item{land.use.included}{If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.}

\item{contact}{Name and email address of responsible person (default to OS username).}

\item{institute}{Name of the institute (default "none").}

\item{...}{extra arguments, not used yet
Note that that \code{format}, \code{dir} and one of \code{id} or \code{name} are compulsory, the rest will be filled with dummy/default values if left blank.
Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for aDGVM1 and some LPJ-GUESS configurations.}
}
\value{
A Source object
}
\description{
This function is preferred to a \code{new("Source",...)} initialisation because it does both the initialisations (including some defaults)
and also performs some extra checks.
}
\details{
Note that no actual data is stored in the resultant \linkS4class{Source} object, rather this object should be used in calls to \link{getField}
to get data as a \linkS4class{Field} object.
}
\seealso{
Source
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
