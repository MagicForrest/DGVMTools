

plotScatter <- function(field1, field2 = field1, layer1, layer2 = layer1) {
  
  
  if(identical(layer1, layer2) && identical(field1, field2)) stop("To make a scatter plot I need either a difference layer, or a different Field, or both!)")
  
  if(!identical(field1, field2)) {
      to.plot <- selectLayers(field1, layer1)
      print(to.plot)
      print(field2@data)
      to.plot <- copyLayers(from = field2, to = to.plot, layer2, keep.all.to = FALSE, keep.all.from = FALSE)
      print(to.plot)
  }
  else {
      to.plot <- selectLayers(field1, c(layer1, layer2))
      print(to.plot)
  }
  
 
  # make the scatter plot
  scatter.plot <- ggplot(as.data.frame(stats::na.omit(to.plot)), aes_string(x=layer1, y=layer2)) +  geom_point(size=3, alpha =1 )
  
  return(scatter.plot)
   
}