#!/usr/bin/Rscript

#' Compare single layers to each other
#' 
#' Compare two layers (each from a Field or DataObject) to calculated various statistic metric and also the error (at every spatial/temporal locality) which is returned as a ComparisonLayer object. 
#'
#' 
#' @param object1 A Field or DataObject from which to get the first layer for comparison
#' @param object2 A Field or DataObject from which to get the second layer for comparison
#' @param layers1 The name of the first layer (character string)
#' @param layers2 The name of the first layer (character string).  If not defined taken to the be the same as layers1
#' @param keepall1 Boolean, keep all data points in layers1 even if there is not corresponding data in layers2 
#' @param keepall2 Boolean, keep all data points in layers2 even if there is not corresponding data in layers2 
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
compareLayers <- function(object1, 
                          object2, 
                          layers1, 
                          layers2=layers1,
                          keepall1 = FALSE, 
                          keepall2 = FALSE, 
                          override.quantity = FALSE, 
                          verbose = FALSE, 
                          match.NAs = FALSE,
                          dec.places = NULL){
  
  ### Check that the object have the same dimensions and that we have the same number of layers, if not fail immediately
  if(!identical(getSTInfo(object1), getSTInfo(object2))) stop("Trying to compare layers with different dimenisons.  Definitely can't do this.  Check your dimension and/or averaging")
  if(length(layers1) != length(layers2)) stop("Trying to compare a different number of layers between two Fields.  Definitely can't do this.  Check you layers1 and layers2 argument.")  
  
  
  ### Find what nature of comparison we are doing
  single = categorical = relative.abundance = FALSE
  # comparing one layer from each
  if(length(layers1) == 1){
    type1 <- class(object1@data[[layers1]])
    type2 <- class(object1@data[[layers2]])
    if(type1 == type2){
      if(type1 == "numeric") {
        single <- TRUE
        if(verbose) message("Comparing two single numeric layers.")
      }
      if(type1 == "factor") {
        categorical <- TRUE
        if(verbose) message("Comparing two single categorical layers.")
      }
    }
    else {
      stop("Layer type don't match, check your layers1 and layers2 arguemnts and your input Fields")
    }
  }
  else {
    relative.abundance <- TRUE
    if(verbose) warning("Doing categorical comparison, note to checks done on layer types...")
  }
  
  
  ###  First get the layers - maybe do a nicer error check here   
  layer.object1 <- selectLayers(object1, layers1)
  layer.object2 <- selectLayers(object2, layers2)
  
  ###  Pull out the source info objects
  info1 <- as(object1@source, "SourceInfo")
  info2 <- as(object2@source, "SourceInfo")
  
  
  ###  Set the names by appending the id so that they are not identical when we combine them, also save the ids so that we can use them later
  new.ids.1 <- c()
  for(layer in layers1) { 
    temp.id <- paste(info1@id, layer, object1@id, sep = ".")
    new.ids.1 <- append(new.ids.1, temp.id)
    setnames(layer.object1@data, layer, temp.id)  
  }
  
  new.ids.2 <- c()
  for(layer in layers2) { 
    temp.id <- paste(info2@id, layer, object2@id, sep = ".")
    new.ids.2 <- append(new.ids.2, temp.id)
    setnames(layer.object2@data, layer, temp.id)  
  }
  
  
  
  ### Check the case that the longitudes, latitudes and years are identical
  
  ### Easy-life case, both objects are on exactly the same domain
  if(identical(getSTInfo(object1, "full"), getSTInfo(object2, "full"))) {
    if(verbose) message("Easy life! Both fields have the same dimensions, can do a data.table join operation.")
    new.data <- layer.object1@data[layer.object2@data] 
    
  }
  ### Else, not-so-easy-life is having to check the domains and keeping points or not
  else {
    if(verbose) message("Not so easy life! Fields don't have the same dimensions, doing a copyLayers() operation.")
    new.data <- copyLayers(from = layer.object2, 
                           to = layer.object1, 
                           layer.names = new.ids.2, 
                           new.layer.names = NULL, 
                           keep.all.to = keepall1, 
                           keep.all.from = keepall2, 
                           dec.places = dec.places)@data
    
    
  }
  
  # match NAs if necessary
  if(match.NAs) {
    are.NAs <- which(is.na(new.data[[new.ids.1]] * new.data[[new.ids.2]]))
    new.data[are.NAs, (c(new.ids.1, new.ids.2)) := .SD[NA], .SDcols =c(new.ids.1, new.ids.2)]
  }
  
  # make meta-data for the ComparisonLayer
  id <- paste0(new.ids.1, "-", new.ids.2)
  se <- extent(new.data)
  if(!identical(object1@quant, object1@quant)) {
    if(override.quantity) warning(paste0("Quantity objects from compared objects do not match (", object1@quant@id, " and ", object2@quant@id, "), proceeding using quantity", object1@quant@id))
    else stop("Comparing different Quantity")
  }
  if(single){ new.name <- paste(info1@name, "-",  info2@name) }
  else if(categorical) { new.name <- paste(info1@name, "vs.",  info2@name) }
  else if(relative.abundance) { new.name <- paste("Relative abundance", info1@name, "vs.",  info2@name) }
  
  ### Calculate the approriate statistical comparisons
  # make vectors of values
  
  if(single || categorical) {
    vector1 <- new.data[[new.ids.1]]
    vector2 <- new.data[[new.ids.2]]
    
    # check the classes of the data and perform the appropriate comparison
    if(single) {
      # Statistics
      stats <- continuousComparison(vector1 = vector1, vector2 = vector2, name1 = info1@name, name2 = info2@name, verbose = verbose)
      # Make the difference layer
      new.data[, "Difference" := get(new.ids.1) - get(new.ids.2)]
    }
    else if(categorical) {
      if(verbose) print("Doing stats comparison")
      stats <- categoricalComparison(vector1 = vector1, vector2 = vector2, name1 = info1@name, name2 = info2@name, verbose = verbose)
      
    }
  }
  else if(relative.abundance) {
    ### Calculate the approriate statistical comparisons
    # make data.table of values
    dt1 <- new.data[,new.ids.1,with=FALSE]
    dt2 <- new.data[,new.ids.2,with=FALSE]
    
    # Calculate Manhattan Metric and Squared Chord Distance
    
    
    stats <- proportionsComparison(dt1 =dt1, dt2 = dt2, name1 = info1@name, name2 = info2@name, verbose = verbose)
    if(verbose) print(stats)
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
                                             spatial.aggregate.method = object1@spatial.aggregate.method,
                          subannual.aggregate.method = object1@subannual.aggregate.method,
                          subannual.original = object1@subannual.original
  )
  
  return(comparison.layer)
  
}