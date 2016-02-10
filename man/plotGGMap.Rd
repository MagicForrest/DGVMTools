\name{plotGGMmap}
\alias{plotGGMmap}
\title{Plot a map with ggplot2}
\description{
  Plots a map (or a set maps) using the geom_raster from the ggplot2 library.
}
\usage{
plotGGMap(input, column='value', colors=NA, sym.col=FALSE, wrap=1, long.title=TRUE, plot=TRUE, ...)
}
\arguments{
  \item{input}{A VegSpatial or a list of VegSpatial objects.}
  \item{column}{Name of the column in the data slot of input, which
    should be plotted. Default: 'value'.}
  \item{colors}{A (named) vector of colors or a data.frame with the
    columns 'color' and ('name' or 'value'), to be used for plotting.}
  \item{sym.col}{boolean, if the colors should be distributed
    symetrically around 0.}
  \item{wrap}{Number of facet_wrap columns. Only valid when a list of
    VegSpatial was given as input. Otherwise it is ignored.}
  \item{long.title}{If the description (default) should be used as titles or the shorter id.}
  \item{plot}{If FALSE only the data is returned, without drawing the map.}
}
\value{
  A ggplot object, which can either be printed directly or further
  modified, or a data.table if plot is FALSE.
}
\author{
  Joerg Steinkamp <joerg.steinkamp@senckenberg.de>
}
