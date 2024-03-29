% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregateSubannual.R
\name{aggregateSubannual}
\alias{aggregateSubannual}
\title{Sub-annual aggregation}
\usage{
aggregateSubannual(x, method = "mean", target = "Year", verbose = FALSE)
}
\arguments{
\item{x}{data.table or Field}

\item{method}{A character string describing the method by which to aggregate the data.  Can currently be "mean", "mode", "median", "sum", "max", "min", "sd", "var" and "cv" (= coefficient of variation sd/mean).
For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!}

\item{target}{A character string defining the subannual period to which the data should be aggregate. Can be "Month", "Season" or "Year" (also "Annual" is valid). 
Default is year.}

\item{verbose}{If TRUE give some progress update about the averaging.

Input data can be a Field or data.table with appropriate columns.}
}
\value{
A Field or data.table depending on the input object
}
\description{
Aggregates data with sub-annual time resolution to a coarser time resolution.  For example, going from monthly to annual.
}
\details{
Input data can be a Field or data.table with appropriate columns.
}
\examples{
\donttest{
 
# Get an example Field
test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
test.Source <- defineSource(name = "LPJ-GUESS", dir = test.dir,  format = GUESS)
field <- getField(source = test.Source, quant = "mlai", year.aggregate.method = "mean")

# calculate of meteorological seasons (DJF, MAM, JJA, SON)
seasonal.mean <- aggregateSubannual(x = field, method = "mean", 
                                    target = "Season", verbose = TRUE)
print(seasonal.mean@data)
print(plotSpatial(seasonal.mean))

#  calculate annual mean
annual.mean <- aggregateSubannual(x = field, method = "mean", verbose = TRUE)
print(annual.mean@data) 
print(plotSpatial(annual.mean))

#  calculate annual standard deviation
annual.sd <- aggregateSubannual(x = field, method = "sd", verbose = TRUE)
print(annual.sd@data) 
print(plotSpatial(annual.sd))

}
}
\author{
Matthew Forrest \email{matthew.forrest@senckenberg.de}
}
