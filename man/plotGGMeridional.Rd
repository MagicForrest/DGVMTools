\name{plotGGMeridional}
\alias{plotGGMeridional}
\title{Plot a meridional mean with ggplot2}
\description{
  Plots vertical lines as average over latitudinal bands.
}
\usage{
plotGGMeridional(input, column='value', what=list(center="mn", var="sd"), alpha=c(0.8, 0.1), colors=NA, long.title=TRUE, plot=TRUE, ...)
}
\arguments{
  \item{input}{A VegSpatial or a list of VegSpatial objects.}
  \item{column}{Name of the column in the data slot of input, which
    should be plotted. Default: 'value'.}
  \item{cwhat}{A list with the elements 'center' and 'var'. 'center' can be one of 'mn', 'mean', 'md', 'median' for the line. 'var' is for the variability polygon drawn in the background. 'var' can be 'sd' (sandard deviation), 'se' (standard error), 'mm'/'minmax', a numeric value (0-1) of quantile to be shown arround the central line or a vector specifying the upper and lower quantile.}
  \item{alpha}{Transparency value for the line and polygon.}
  \item{colors}{Colors for the diffent VegSpatial objects.}
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
