\name{plotGGCategorialAggregated}
\alias{plotGGCategorialAggregated}
\title{Plot a categorically aggregated summary}
\description{
  Plots vertical lines as average over latitudinal bands.
}
\usage{
plotGGCategorialAggregated(input, targets=NULL, name.map=NA, area.weighted=TRUE, long.title=TRUE, plot=TRUE, ...)
}
\arguments{
  \item{input}{A VegRun object the data should be taken from.}
  \item{targets}{A list or data.frame with columns x/y specifying the VegSpatial slot and column used for categorial aggregation (y) and a numeric VegSpatial slot/column use for the calculation. It can also be a list/data.frame with the columns 'slot'/'column'.}
  \item{name.map}{If the y-slot/column is a numeric vector (integers), this vector can used as map to convert those numbers to names by index.}
  \item{area.weighted}{Should the mean be weighted by cell size (default TRUE).}
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
