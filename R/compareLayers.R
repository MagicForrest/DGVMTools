#!/usr/bin/Rscript

#' Compare single layers to each other
#' 
#' Compare two layers (each from a Field or DataObject) to calculated various statistic metric and also the error (at every spatial/temporal locality) which is returned as a ComparisonLayer object. 
#'
#' 
#' @param object1 A Field or DataObject from which to get the first layer for comparison
#' @param object2 A Field or DataObject from which to get the second layer for comparison
#' @param layer1 The name of the first layer (character string)
#' @param layer2 The name of the first layer (character string).  If not defined taken to the be the same as layer1
#' @param keepall1 Boolean, keep all data points in layer1 even if there is not corresponding data in layer2 
#' @param keepall2 Boolean, keep all data points in layer2 even if there is not corresponding data in layer2 
#' @param override.quantity Boolean, if TRUE ignore situation where object1 and object2 have non-identical Quantities and use the Quantity from object1 for the returned ComparisonLayer
#' @param match.NAs Boolean, if TRUE copy NAs from one layer to the other.  This is mostly to make sure that when you later plot the data in the final ComparisonLayer side-by-side,
#' that the both have 'no data' (typically grey areas) plotted on both maps.
#' @param dec.places Numeric, passed to copyLayers. Defines to how many decimal places to round the coordinates in order to get a match.  Default is no rounding (value is NULL) and if dine for most regularing spaced grids.  
#' @param verbose Boolean, if TRUE print some informative output
#' 
#' The returned ComparisonLayer object can be plotted using \code{plotSpatail} (to give the absolute difference, original values side-by-side and percentage difference, also the NME spatial - to be implemented)
#' The stats slot (which contains a SpatialComparison object) holds information such as RSME, NME, Nash-Sutcliffe Model Efficiency, etc. between the datasets in the case of a continous data.
#' In the case of comparing categorical data (ie layers of factors) it contains Cohen's Kappa.
#' 
#' 
#' @return A ComparisonLayer object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
compareLayers <- function(object1, object2, layer1, layer2=layer1, keepall1 = FALSE, keepall2 = FALSE, override.quantity = FALSE, verbose = TRUE, match.NAs = FALSE, dec.places = NULL){
  
  ### Check that the object have the same dimensions, if not fail immediately
  if(!identical(getSTInfo(object1), getSTInfo(object2))) stop("Trying to compare layers with different dimenisons.  Definitely can't do this.  Check your dimension and/or averaging")
  
  
  ###  First get the layers - maybe do a nicer error check here   
  layer.object1 <- selectLayers(object1, layer1)
  layer.object2 <- selectLayers(object2, layer2)
  
  
  ###  Set the names by appending the id so that they are not identical
  # object 1
  new.id1 <- paste(object1@source@id, layer1, object1@id, sep = ".")
  info1 <- as(object1@source, "SourceInfo")
  setnames(layer.object1@data, layer1, new.id1) 
  
  # object 2
  new.id2 <- paste(object2@source@id, layer2, object2@id, sep = ".")
  info2 <- as(object2@source, "SourceInfo")
  setnames(layer.object2@data, layer2, new.id2) 
  
  ### Check the case that the longitudes, latitudes and years are identical
  
  ### Easy-life case, both objects are on exactly the same domain
  if(identical(getSTInfo(object1, "full"), getSTInfo(object2, "full"))) {
    
    new.data <- layer.object1@data[layer.object2@data] 
    
  }
  ### Else, not-so-easy-life is having to check the domains and keeping points or not
  else {
    
    new.data <- copyLayers(from = layer.object2, 
                           to = layer.object1, 
                           layer.names = new.id2, 
                           new.layer.names = NULL, 
                           keep.all.to = keepall1, 
                           keep.all.from = keepall2, 
                           dec.places = dec.places)@data
    
    
  }
  
  # match NAs if necessary
  if(match.NAs) {
    are.NAs <- which(is.na(new.data[[new.id1]] * new.data[[new.id2]]))
    new.data[are.NAs, (c(new.id1, new.id2)) := .SD[NA], .SDcols =c(new.id1, new.id2)]
  }
  
  # make meta-data for the ComparisonLayer
  id <- paste0(new.id1, "-", new.id2)
  if(object1@temporal.extent@start == object2@temporal.extent@start & object1@temporal.extent@end == object2@temporal.extent@end){
    te <- object2@temporal.extent
  }
  else {
    te <- object1@temporal.extent
  }
  se <- extent(new.data)
  if(!identical(object1@quant, object1@quant)) {
    if(override.quantity) warning(paste0("Quantity objects from compared objects do not match (", object1@quant@id, " and ", object2@quant@id, "), proceeding using quantity", object1@quant@id))
    else stop("Comparing different Quantity")
  }
  new.name <- paste(info1@name, "-",  info2@name)
  
  ### Calculate the approriate statistical comparisons
  # make vectors of values
  vector1 <- new.data[[new.id1]]
  vector2 <- new.data[[new.id2]]
  
  # check the classes of the data and perform the appropriate comparison
  if(class(vector1) == "numeric" & class(vector2) == "numeric") {
    # Statistics
    stats <- continuousComparison(vector1 = vector1, vector2 = vector2, name1 = info1@name, name2 = info2@name, verbose = verbose)
    # Make the difference layer
    new.data[, "Difference" := get(new.id1) - get(new.id2)]
  }
  else if(class(vector1) == "factor" & class(vector2) == "factor") {
    print("Doing stats comparison")
    stats <- categoricalComparison(vector1 = vector1, vector2 = vector2, name1 = info1@name, name2 = info2@name, verbose = verbose)
    
  }
  else {
    stop("Layers for comparison are of inconsistent types, check your inputs!")
  }
  
  
  
  ### Finally build the layer and return
  comparison.layer <- new("ComparisonLayer",
                          id = id,
                          name = new.name,
                          data = new.data,
                          quant = object1@quant,
                          info1 = info1,
                          info2 = info2,
                          stats = stats,
                          spatial.extent = se,
                          spatial.extent.id = object1@spatial.extent.id ,
                          temporal.extent = te,
                          temporal.extent.id = object1@spatial.extent.id ,
                          spatial.aggregate.method = object1@spatial.aggregate.method,
                          temporal.aggregate.method = object1@temporal.aggregate.method,
                          subannual.aggregate.method = object1@subannual.aggregate.method,
                          subannual.original = object1@subannual.original
                      )
  
  return(comparison.layer)
  
}