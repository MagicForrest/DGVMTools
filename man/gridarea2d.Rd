% File LPJ/man/gridarea2d.Rd
\name{gridarea2d}
\alias{gridarea2d}
\title{Calculate the area in a longitude latitude meash in m^2.}
\description{
  Calculates the area in m^2 of a logitude/latitude mesh.
}
\usage{
gridarea2d(lon, lat, scale, ellipse)
}
\arguments{
  \item{lon}{vector of gridcell longitude midpoints in degree.}
  \item{lat}{vector of gridcell latitude midpoints in degree.}
  \item{scale}{scaling factor (default: 1.0).}
  \item{ellipse}{boolean, whether to use ellipsoid or sphere (default FALSE).}
}
\value{
  vector with area in m^2 in latitudinal belt.
}
\author{
  Joerg Steinkamp <joergsteinkamp@yahoo.de>
}
