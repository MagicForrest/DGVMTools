\name{plotGGMmap}
\alias{plotGGMmap}
\title{Plot a map with ggplot2}
\description{
  Plots a map (or a set maps) using the geom_raster from the ggplot2 library.
}
\usage{
plotGGMap(input, column=NA, colors=NA, sym.col=FALSE, wrap=1)
}
\arguments{
  \item{input}{A VegSpatial or a list of VegSpatial from different sensitivity analyses.}
  \item{column}{Name of the column of the data slot of input, which
    should be plotted. Default: 'value'.}
  \item{colors}{A (named) vector of colors or a data.frame with the
    columns 'color' and ('name' or 'value'), to be used for plotting.}
  \item{sym.col}{boolean, if the colors should be distributed
    symetrically around 0.}
  \item{wrap}{Number of facet_wrap columns. Only valid when a list of
    VegSpatial was given as input. Otherwise it is ignored.}
}
\value{
  A ggplot object, which can either be printed directly or further
  modified.
}
\author{
  Joerg Steinkamp <joerg.steinkamp@senckenberg.de>
}
