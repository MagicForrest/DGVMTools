#' Select PFTs
#' 
#' Uses PFT metadata to return all PFTs with a particular properties.  For example only tree PFTs, or only evergreen PFTs.
#' 
#' @param x Either a Field, or a list of PFT objects
#' @param criteria A character string to use to select PFTs.  This compared to every the value of every slot of a PFT object.  If one matches, the PFT is inlucded in the return.
#  If left empty or set to NULL the return all PFTs.
#' @param return.ids A logical, if TRUE (default) then return the the id of the PFT, if FALSE return the entire PFT object
#' 
#' @return Either a vector of characters (the ids of the PFTs) or a list of PFT object (depending on argument return.ids)
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
#' @examples
#' 
#' ## List PFTs from a list of PFT objects
#' 
#' # make a list of PFTs for selecting from 
#' PFT.list <- GUESS@default.pfts
#' print(PFT.list)
#' 
#' # return with no criteria (trivial)
#' whichLayers(PFT.list)
#' 
#' # return as the full PFT objects
#' whichLayers(PFT.list, return.ids = FALSE)
#' 
#' # return Tree, Grass, Evergreen and Tropical PFTs
#' whichLayers(PFT.list, criteria = "Tree")
#' whichLayers(PFT.list, criteria = "Grass")
#' whichLayers(PFT.list, criteria = "Evergreen")
#' whichLayers(PFT.list, criteria = "Tropical")
#' 
#' # return Tropical PFTs as a list of PFTs objects (not just the ids)
#' whichLayers(PFT.list, criteria = "Tropical", return.ids = FALSE)
#' 
#' \donttest{
#'  
#' ## List PFTs from a Field object
#' 
#' # Load a per-PFT Field from the example data
#' run.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(id = "LPJ-GUESS_Example", dir = run.dir,  format = GUESS)
#' test.Field.perPFT <- getField(source = test.Source, var = "lai") 
#' 
#' # Perform the same examples as above but with the Field instead of a list of PFTs objects
#' 
#' # return with no criteria (trivial)#' 
#' whichLayers(test.Field.perPFT )
#' 
#' # return as the full PFT objects
#' whichLayers(test.Field.perPFT, return.ids = FALSE)
#' 
#' # return Tree, Grass, Evergreen and Tropical PFTs
#' whichLayers(test.Field.perPFT, criteria = "Tree")
#' whichLayers(test.Field.perPFT, criteria = "Grass")
#' whichLayers(test.Field.perPFT, criteria = "Evergreen")
#' whichLayers(test.Field.perPFT, criteria = "Tropical")
#' 
#' # return Tropical PFTs as a list of PFTs objects (not just the ids)
#' whichLayers(test.Field.perPFT, criteria = "Tropical", return.ids = FALSE)
#' 
#' }
#' 
#' @export

whichLayers <- function(x, criteria = NULL, return.ids = TRUE) {
  
  
  # first prepare a list of all PFTs present
  
  # if Field compare the layers of x with the PFTs in the Format object
  if(is.Field(x)) {
    pft.list <- list()
    for(this.PFT in x@source@pft.set) {
      if(this.PFT@id %in% layers(x)) pft.list <- append(pft.list, this.PFT)
    }
  }
  
  # else if list, check that all elements are a PFT
  else if(is.list(x)){
    for(this in x) {
      if(!is.Layer(this)) stop("At least one item in the input list is not an object PFT class")
    }
    pft.list <- x
  }  
  
  # else fail
  else {
    stop("Unexpected input for argument 'x' in whichLayers().")
  }
  
  
  # now check each PFT in X to see if it matches the criteria
  matched.pfts <- list()
  if(!is.null(criteria)) criteria <- tolower(criteria)
  for(PFT in pft.list) {
    
    if(tolower(PFT@growth.form) == criteria 
       || tolower(PFT@climate.zone) == criteria
       || tolower(PFT@leaf.form) == criteria 
       || tolower(PFT@phenology) == criteria
       || is.null(criteria)) {
      
      if(return.ids) matched.pfts[[PFT@id]] <- PFT@id 
      else matched.pfts[[PFT@id]] <- PFT
      
    }
    
  }
  
  if(return.ids) return(unlist(matched.pfts))
  else return(matched.pfts)
  
}
