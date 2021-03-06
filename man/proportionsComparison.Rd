% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/benchmarking.R
\name{proportionsComparison}
\alias{proportionsComparison}
\title{Compare relative proportions data}
\usage{
proportionsComparison(x, layers1, layers2, additional, verbose = TRUE)
}
\arguments{
\item{x}{A data.table containing the spatial-temporal-annual columns and two columns containg the data to be compared}

\item{layers1}{A vector of character strings giving the layers from the second dataset to compare (should be columns in x and sum to 1 or 100)}

\item{additional}{A list of functions define additions metrics, see the custom.metrics argument of \code{compareLayers()}}

\item{verbose}{A logical, if TRUE print out all the metric scores}
}
\value{
A named list of metric statistics
}
\description{
Compares two datasets of relative proportions of multiple classes (sum of classes equals one at each point) where the total for each  data where the totally value for each. 
Specifically calculates and returns a list with the Manhattan Metric (MM) and Square Chord Distance (SCD).
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
\keyword{internal}
