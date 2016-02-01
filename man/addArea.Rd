\name{addArea}
\alias{addArea}
\title{Adds the area per gridcell to a VegSpatial}
\description{
  Add the column 'area' to the data slot of a VegSpatial.
}
\usage{
addArea(input, unit="m^2", ellipse=FALSE, verbose=FALSE)
}
\arguments{
  \item{input}{VegSpatial}
  \item{unit}{Any udunit2 compliant area unit, default 'm^2'.}
  \item{ellipse}{Should an ellipsoid be used for area calculation or a
    sphere (default).}
  \item{verbose}{print some information.}
}
\value{
  The input VegSpatial with the additional column 'area' in the slot data.
}
\author{
  Joerg Steinkamp <joerg.steinkamp@senckenberg.de>
}
