% File LPJ/man/gridarea1d.Rd
\name{gridarea1d}
\alias{gridarea1d}
\title{Calculate the area in all gridcell along a latitudinal belt in
  m^2.}
\description{
  Calculates the area in m^2 of gridcells in a latitudinal belt.
}
\usage{
gridarea1d(lat, dlon, scale, ellipse)
}
\arguments{
  \item{lat}{vector of gridcell latitude midpoints in degree.}
  \item{dlon}{value of longitudinal gridcell extent in degree.}
  \item{scale}{scaling factor (default 1.0).}
  \item{ellipse}{boolean, whether to use ellipsoid or sphere (default FALSE).}
}
\value{
  vector with area in m^2 in latitudinal belt.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
