% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Format-GUESS.R
\name{openLPJOutputFile}
\alias{openLPJOutputFile}
\title{Open an LPJ-GUESS .out file}
\usage{
openLPJOutputFile(
  run,
  quant,
  layers = NULL,
  target.sta,
  file.name = file.name,
  verbose = FALSE,
  data.table.only = FALSE,
  ...
)
}
\arguments{
\item{run}{A \code{\linkS4class{Source}} containing the meta-data about the LPJ-GUESS run}

\item{quant}{A Quant to define what output file from the LPJ-GUESS run to open,}

\item{layers}{A character string (or a vector of character strings) specifying which layer columns are to be read.  NULL (default) means read all.
can also be a simple string defining the LPJ-GUESS output file if the \code{return.data.table} argument is TRUE}

\item{target.sta}{An STAInfo object describing the target STA}

\item{file.name}{Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
"<quant@id>.out" (also "<quant@id>.out.gz")}

\item{verbose}{A logical, set to true to give progress/debug information}

\item{data.table.only}{A logical, if TRUE return a data.table and not a Field}
}
\value{
a data.table (with the correct tear offset and lon-lat offsets applied)
}
\description{
\code{openLPJOutputFile} returns a data.table object given a string defining a vegetation quantity  from the run (eg. "lai", to read the file "lai.out") 
or a \code{\linkS4class{Quantity}} object, and a \code{\linkS4class{Source}} object which defines where the run is on disk.
}
\details{
Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
\keyword{internal}
