#' Select Layers
#' 
#' Uses Layer metadata to return all Layers with a particular properties.  For example only tree Layers, or only evergreen Layers.
#' 
#' @param x Either a Field, or a list of Layer objects
#' @param criteria A character string to use to select Layers.  This compared to every the value of every slot of a Layer object.  If one matches, the Layer is inlucded in the return.
#  If left empty or set to NULL the return all Layers.
#' @param return.ids A logical, if TRUE (default) then return the the id of the Layers, if FALSE return the entire Layer objects
#' 
#' @return Either a vector of characters (the ids of the Layers) or a list of Layer object (depending on argument return.ids)
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
#' @examples
#' 
#' ## List Layers from a list of Layer objects
#' 
#' # make a list of Layers for selecting from 
#' layer.list <- GUESS@predefined.layers
#' print(layer.list)
#' 
#' 
#' # return Tree, Grass, Evergreen and Tropical PFTs
#' whichLayers(layer.list, criteria = "Tree")
#' whichLayers(layer.list, criteria = "Grass")
#' whichLayers(layer.list, criteria = "Evergreen")
#' whichLayers(layer.list, criteria = "Tropical")
#' 
#' # return Tropical PFTs as a list of Layers objects (not just the ids)
#' whichLayers(layer.list, criteria = "Tropical", return.ids = FALSE)
#' 
#' \donttest{
#'  
#' ## List Layers from a Field object
#' 
#' # Load a multi-Layer (one layer per-PFT) Field from the example data
#' run.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(id = "LPJ-GUESS_Example", dir = run.dir,  format = GUESS)
#' test.Field.perLayer <- getField(source = test.Source, quant = "lai") 
#' 
#' # Perform the same examples as above but with the Field instead of a list of Layers objects
#' 
#' 
#' # return Tree, Grass, Evergreen and Tropical Layers
#' whichLayers(test.Field.perLayer, criteria = "Tree")
#' whichLayers(test.Field.perLayer, criteria = "Grass")
#' whichLayers(test.Field.perLayer, criteria = "Evergreen")
#' whichLayers(test.Field.perLayer, criteria = "Tropical")
#' 
#' # return Tropical PFTs as a list of Layers objects (not just the ids)
#' whichLayers(test.Field.perLayer, criteria = "Tropical", return.ids = FALSE)
#' 
#' }
#' 
#' @export

whichLayers <- function(x, criteria, return.ids = TRUE) {
  
  
  # first prepare a list of all Layers present
  
  # if Field compare the layers of x with the Layers in the Format object
  if(is.Field(x)) {
    layer.list <- list()
    for(this.Layer in x@source@defined.layers) {
        if(this.Layer@id %in% layers(x)) layer.list[[length(layer.list)+1]] <- this.Layer
    }
  }
  # else if list, check that all elements are a Layer
  else if(is.list(x)){
    for(this in x) {
      if(!is.Layer(this)) stop("At least one item in the input list is not an object Layer class")
    }
    layer.list <- x
  }  
  
  # else fail
  else {
    stop("Unexpected input for argument 'x' in whichLayers().")
  }
  
  
  # now check each Layer in X to see if it matches the criteria
  matched.layers <- list()
  criteria <- tolower(criteria)
  for(Layer in layer.list) {
    
    # check each property in turn
    for(this.property in Layer@properties) {
      if(tolower(this.property) == criteria) {
        if(return.ids) matched.layers[[Layer@id]] <- Layer@id 
        else matched.layers[[Layer@id]] <- Layer
        break()
      }
    }
   
  }
  
  if(return.ids) return(unlist(matched.layers))
  else return(matched.layers)
  
}
