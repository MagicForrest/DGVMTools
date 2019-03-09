

#' Select a subset of layers from a Field
#' 
#' This function returns a copy of a Field containing only a selected subset of layers
#' 
#' @param x A Field
#' @param layers A vector of characters strings specifying the layers to be selected
#' 
#'
#' @return A Field
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

selectLayers <- function(x, layers) {
  
  # check if layer is present
  for(layer in layers) {
    if(!(layer %in% names(x))) stop(paste("Can't subset layer", layer, "from object with id =", x@id, "since it is not present", sep = " "))
  }
  
  # copy the field and replace the data.table with a subsetted one
  x.new <- copy(x)
  dt.new <- x.new@data
  dt.new <- dt.new[, append(getDimInfo(x), layers), with = FALSE]
  x.new@data <- dt.new
  
  return(x.new)
  
}

