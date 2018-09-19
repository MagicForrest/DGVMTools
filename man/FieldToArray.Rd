% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-methods.R
\name{FieldToArray}
\alias{FieldToArray}
\title{Convert a Field to a multi-dimensional array}
\usage{
FieldToArray(d, cname = FALSE, invertlat = FALSE, verbose = FALSE)
}
\arguments{
\item{d}{the data.table of a \code{\linkS4class{Field}}}

\item{cname}{the column name to convert, if not set a list is returned}

\item{invertlat}{start in the north}

\item{verbose}{print some information}
}
\value{
a array or a list or arrays
}
\description{
Convert a Field to a multi-dimensional array
}
\author{
Joerg Steinkamp \email{joerg.steinkamp@senckenberg.de}
}
\keyword{internal}