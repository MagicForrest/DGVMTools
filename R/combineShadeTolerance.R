###################################################################################
##### COMBINE SHADE-INTOLERANT PFTS WITH SHADE-TOLERANT PFTS FOR FINDING DOMINANT PFT
##### AND DOING BIOME CLASSIFICATIONS ETC
#'
#' Combine shade-intolerant PFTs with their shade-tolerant cousions
#' 
#' The effects of this depend on the shade-tolerant cousin PFTs being defined in the PFT list.  Note that this function modifies the original Field in place, 
#' so make a copy of the Field first if you want to keep the old one.  Also it means that it doesn't need to be re-asigned with the "<-" syntax, you can actually just call the function. 
#' 
#' 
#' @param input The Field which is to have the shade tolerance classes combined.
#' @param verbose Logical, if TRUE print some progress updates
#' @return Not necessary anything since the Field is modified, but it does return Field with the data for the shade-intolerant PFTs set to zero but their values added to the shade-tolerant versions
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
combineShadeTolerance <- function(input, verbose = FALSE){
  
  # get PFTs present
  PFT.data <- input@source@pft.set
  PFTs.present <- getPFTs(input, PFT.data)
  PFTs.present.names <- names(input)
 
  # for each PFT present identify if it has a shade-tolerate cousin and add it on if present
  for(PFT in PFTs.present){
      
    target.PFT <- PFT@combine
    
    if(target.PFT %in% PFTs.present.names) {
      
      if(verbose) message(paste("Combining PFT", PFT@id, "with PFT", target.PFT))
      input@data[, (target.PFT) := rowSums(.SD), .SDcols=c(target.PFT, PFT@id)]
      input@data[, PFT@id := 0]
      
    }
    
    else if(!(tolower(target.PFT) == "no" || tolower(target.PFT) == "none")){
      warning(paste("PFT", PFT@id, "was supposed to be combined with PFT", target.PFT, "but it is not present, so no shade-tolerance combination done for the PFT"))
    }
    
  }
  
  return(input)
  
}
