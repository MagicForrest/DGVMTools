

#' Select layers
#' 
#' This function returns a copy of a Field (or data.table) containing only a selected subset of layers
#' 
#' @param x A Field (or data.table)
#' @param layers A vector of characters strings specifying the layers to be selected
#' 
#'
#' @return A Field (or data.table)
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

selectLayers <- function(x, layers) {
  
  # check if layer is present
  for(layer in layers) {
    if(!(layer %in% layers(x))) stop(paste("Can't subset layer", layer, "since it is not present", sep = " "))
  }
  
  # if a data.table, do the selection and return
  if(is.data.table(x)) return(x[, append(getDimInfo(x), layers), with = FALSE])
  
  # if a Field or Comparison copy the field and replace the data.table with a subsetted one
  else if(is.Field(x)  || is.Comparison(x)) {
    x.new <- copy(x)
    dt.new <- x.new@data
    dt.new <- dt.new[, append(getDimInfo(x), layers), with = FALSE]
    x.new@data <- dt.new
    return(x.new)
  }
  
  else {
    stop("Function selectLayers() only valid for Fields, Comparisons and data.tables")
  }
  
}

