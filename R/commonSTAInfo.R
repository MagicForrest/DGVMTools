#' Checks common for STAInfo object
#'
#' Given a list of STAInfo or Field objects, this functions determines the common spatial-temporal-annual info.
#' 
#' @param sta.objects  A list of Field and/or STAInfo objects which should be compared to dermine the common STAInfo
#' @param logical Logical, if TRUE return a simple TRUE/FALSE rather than an STAInfo object of common info.
#' 
#' @return Either a logical or an STAInfo object, depending on the value of 'logical'
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

commonSTAInfo <- function(sta.objects, logical = FALSE) {
  
  sta.infos <- list()
  if(is.Field(sta.objects) || is.STAInfo(sta.objects)) sta.objects <- list(sta.objects)

  for(sta.object in sta.objects) {
    if(is.Field(sta.object)) sta.infos[[length(sta.infos)+1]] <-  as(sta.object, "STAInfo")
    else if(is.STAInfo(sta.object)) sta.infos[[length(sta.infos)+1]] <- sta.object
    else warning(paste("No STAInfo can be derived from class", class(sta.object), "so ignoring it", sep = " "))
  }

  # make a 'NULL' STAInfo
  common.sta.info <- new("STAInfo")
  
  # check each slot for each of the input lists
  all.all.good <- TRUE
  for (this.slot in slotNames(sta.infos[[1]])) {
    
    all.good <- TRUE
    first.value <- NULL
    for(counter in 1:length(sta.infos)) {
      
      if(counter == 1){  
        first.value <- slot(sta.infos[[counter]], this.slot)  
      }
      else {
        if(!identical(first.value, slot(sta.infos[[counter]], this.slot))) {
          all.good <- FALSE
          all.all.good <- FALSE
        }
      }

    }
    
     # if all the same set the slot to the common value
    if(all.good) slot(common.sta.info, this.slot) <- first.value
     
  }
  
  if(logical) return(all.all.good)
  else return(common.sta.info)
 
}


