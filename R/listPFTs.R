#' Select PFTs
#' 
#' Uses PFT metadata to return all PFTs with a particular properties.  For example only tree PFTs, or only evergreen PFTs.
#' 
#' @param x Either a Field, or a list of PFT objects
#' @param criteria A character string to use to select PFTs.  This compared to every the value of every slot of a PFT object.  If one matches, the PFT is inlucded in the return.
#  If left empty return all PFTs ()
#' @param return.ids A logical, if TRUE (default) then return the the id of the PFT, if FALSE return the entire PFT object
#' 
#' @return Either a list of characters (the ids of the PFTs) or a list of PFT object (depending on argument return.ids)
#' 
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

listPFTs <- function(x, criteria = NULL, return.ids = TRUE) {
  
  
  # check input and get the PFT superset
  # if Field
  if(is.Field(x)) pft.list <- x@source@pft.set
  # if list
  else if(is.list(x)){
    for(this in x) {
      if(!is.PFT(this)) stop("At least one item in the input list is not an object PFT class")
    }
    pft.list <- x
  }  
  # else fail
  else {
    stop("Unexpected input for argument 'x' in listPFTs().")
  }
  
  # get a list of the layers of x, to check that the PFT is actually present
  layers.x <- names(x)
  
  
  # now check each PFT in X to see if it matches the criteria
  matched.pfts <- list()
  if(!is.null(criteria)) criteria <- tolower(criteria)
  for(PFT in pft.list) {
    
    # check PFT is present in data.table 
    if(PFT@id %in% layers.x) {
      
      if(tolower(PFT@growth.form) == criteria 
         || tolower(PFT@climate.zone) == criteria
         || tolower(PFT@leaf.form) == criteria 
         || tolower(PFT@phenology) == criteria
         || is.null(criteria)) {
        
        if(return.ids) matched.pfts[[PFT@id]] <- PFT@id 
        else matched.pfts[[PFT@id]] <- PFT
        
      }
      
    }
    
  }
  
  if(return.ids) return(unlist(matched.pfts))
  else return(matched.pfts)
  
}
