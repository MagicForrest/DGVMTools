% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/writeField-methods.R
\name{writeField-methods}
\alias{writeField-methods}
\alias{writeField}
\alias{writeField,Field-method}
\title{Write methods}
\usage{
writeField(x)

\S4method{writeField}{Field}(x)
}
\arguments{
\item{x}{A Field}
}
\value{
The name of the file written out (probably not so useful, just want to return something)
}
\description{
Simply writes a field to disk (using the saveRDS() command), with a specific filename, based on the Field's id and the Source's directory.
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
