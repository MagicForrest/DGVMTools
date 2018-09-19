#' Make A scatter plot
#' 
#' This simple function makes (and returns it as an object, it doesn't print it) a simple scatter plot (using ggplot2). 
#' The data are two layers from either one or two Field objects.  Data points which appear in one Field but not the other are excluded  
#' 
#' @param field1 The first DGVMTools::Field object from which the data to be plotted should be taken.
#' @param field2 The second DGVMTools::Field object from which the data to be plotted should be taken.
#' Default value is field1.
#' @param layer1 The first layer to be plotted (taken from field1)
#' @param layer2 The second layer to be plotted (taken from field2).  Defaults to layer1.
#' 
#' @return A ggplot2 object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}  
plotScatter <- function(field1, field2 = field1, layer1, layer2 = layer1) {
  
  
  if(identical(layer1, layer2) && identical(field1, field2)) stop("To make a meaningful scatter plot I need either two different layers, or a two different Fields, or both!)")
  
  if(!identical(field1, field2)) {
      to.plot <- selectLayers(field1, layer1)
      to.plot <- copyLayers(from = field2, to = to.plot, layer.names = layer2, keep.all.to = FALSE, keep.all.from = FALSE)
  }
  else {
      to.plot <- selectLayers(field1, c(layer1, layer2))
  }
  
  print(to.plot)
 
  # make the scatter plot
  scatter.plot <- ggplot(as.data.frame(stats::na.omit(to.plot)), aes_string(x=layer1, y=layer2)) +  geom_point(size=3, alpha =1)
  
  # TODO - add automatic axis info
  
  return(scatter.plot)
   
}