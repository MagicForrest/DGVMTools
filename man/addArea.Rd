\name{addArea}
\alias{addArea}
\title{Adds the area per gridcell to a vegObj}
\description{
  Add the column 'area' to the data slot of a vegObj.
}
\usage{
addArea(input, unit="m^2", ellipse=FALSE, verbose=FALSE)
}
\arguments{
  \item{input}{vegObj}
  \item{unit}{Any udunit2 compliant area unit, default 'm^2'.}
  \item{ellipse}{Should an ellipsoid be used for area calculation or a
    sphere (default).}
  \item{verbose}{print some information.}
}
\value{
  The input vegObj with the additional column 'area' in the slot data.
}
\author{
  Joerg Steinkamp <joerg.steinkamp@senckenberg.de>
}
