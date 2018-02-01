

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
  
  all.layers <- names(x)
  st.layers <- getSTInfo(x)
  
  # check if layer is present
  for(layer in layers) {
    if(!(layer %in% all.layers)) stop(paste("Can't subset layer", layer, "from object with id =", x@id, "since it is not present", sep = " "))
  }
  
  x.new <- copy(x)
  dt.new <- x.new@data
  dt.new <- dt.new[, append(st.layers, layers), with = FALSE]
  x.new@data <- dt.new
  
  return(x.new)
  
}

