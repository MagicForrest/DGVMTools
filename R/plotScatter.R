#' Make a scatter plot
#' 
#' This simple function makes (and returns it as an object, it doesn't print it) a simple scatter plot (using ggplot2). 
#' The data are two layers from either one or two Field objects.  Data points which appear in one Field but not the other are excluded  
#' 
#' @param x The first DGVMTools::Field or Comparison object from which the data to be plotted should be taken.
#' @param y The second DGVMTools::Field or Comparison object from which the data to be plotted should be taken.
#' Default value is x.
#' @param layer.x The first layer to be plotted (taken from x)
#' @param layer.y The second layer to be plotted (taken from y).  Defaults to layer.x.
#' @param alpha Numeric between 0 and 1 specifing the transparency of the points.  Default is 1 (= fully opaque).
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param tolerance Numeric, passed to copyLayers. Defines how close the longitudes and latitudes of the gridcells in \code{x} and \code{y} (if different)
#' need to be to the coordinates in order to get a match.  Can be a single numeric (for the same tolerance for both lon and lat) or a vector of two numerics (for lon and lat separately).
#' Default is no rounding (value is NULL) and so is fine for most regular spaced grids.  However, setting this can be useful to force matching of 
#' coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' 
#' @return A ggplot2 object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}  
plotScatter <- function(x, y = x, layer.x, layer.y = layer.x, alpha = 1, text.multiplier, tolerance = NULL) {
  
  # check
  if(identical(layer.x, layer.y) && identical(x, y)) stop("To make a meaningful scatter plot I need either two different layers, or a two different Fields, or both!)")
  
  # extract the data.tables
  if(is.Field(x) || is.Comparison(x)) x.dt <- copy(x@data)
  else if(is.data.table(x)) x.dt <- copy(x)
  else stop(paste("Cannot plot scatter from object x of type:", paste(class(x), collapse = " ")))
  if(is.Field(y) || is.Comparison(y)) y.dt <- copy(y@data)
  else if(is.data.table(y)) y.dt <- copy(y)
  else stop(paste("Cannot plot scatter object y of type:", paste(class(y), collapse = " ")))
  
  # rename layers if they are the same
  old.layer.y <- layer.y
  old.layer.x <- layer.x
  if(identical(layer.x, layer.y)) {
    old.layer.y <- layer.y
    old.layer.x <- layer.x
    layer.y <- paste(y@source@id, layer.y, sep = "_")
    layer.x <- paste(x@source@id, layer.x, sep = "_")
    setnames(y.dt, old.layer.y, layer.y)
    setnames(x.dt, old.layer.x, layer.x)
  }
  
  # copy layers to one data.table
  if(!identical(x, y)) {
    to.plot <- x.dt[, append(getDimInfo(x.dt), layer.x), with = FALSE]
    to.plot <- copyLayers(from = y.dt, to = to.plot, layer.names = layer.y, keep.all.to = FALSE, keep.all.from = FALSE, tolerance = tolerance)
  }
  else {
    to.plot <- x.dt[, append(getDimInfo(x.dt), c(layer.x, layer.y)), with = FALSE]
  }
  
  
  # remove spaces and -" "from column names otherwise ggplot2 goes a bit flooby (although check out 'tidyevaluation' to maybe do this nicer)
  for(this.character in c(" ", "-")) {
    x.new <- gsub(x = layer.x, pattern = this.character, replacement = "_")
    y.new <- gsub(x = layer.y, pattern = this.character, replacement = "_")
  }
  setnames(to.plot, c(layer.x, layer.y), c(x.new, y.new))
  
  
  # make the scatter plot
  scatter.plot <- ggplot(as.data.frame(stats::na.omit(to.plot)), aes_string(x=x.new, y=y.new)) +  geom_point(size=3, alpha = alpha)
  if(!missing(text.multiplier)) scatter.plot <- scatter.plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  # labels depending on input type
  if(is.Field(x) && is.Field(y))  { 
    scatter.plot <- scatter.plot + labs(y = stringToExpression(paste0(old.layer.y, " ", y@source@name,  " ", y@quant@name, " (", standardiseUnitString(y@quant@units), ")")),
                                        x = stringToExpression(paste0(old.layer.x, " ", x@source@name,  " ", x@quant@name, " (", standardiseUnitString(x@quant@units), ")")))
  }
  else scatter.plot <- scatter.plot + labs(y = layer.y, x = layer.x)
  
  
  
  return(scatter.plot)
  
}