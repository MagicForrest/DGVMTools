

##### RETRIEVES AN OBJECT FROM A LIST BASED ON THE 'id' SLOTS
#
#' Retrieves an object from a list based on it's \code{id} slot
#' 
#' Looks through a list of arbitary object until it finds one which has a slot called "id" whose value matches the imput argument.
#' The idea is that you can use this fucntion to pull a particular PFT from a list of PFT objects, or a a particular run from a list of Source objects etc.
#' 
#' @param id The id sought (must be string)
#' @param list The list to be scanned (must be a list)
#' 
#' @return The matching object from the list or NULL.  If NULL it will give a message and a warning, the following code will probably fail. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
#' 
byIDfromList <- function(id, list) {
  
  
  # Error checking
  if(class(id)[1] != "character") {
    stop(paste("From function IDfromList(): Argument 'id' not a string therefore not valid.  It has class = ",  class(id)[1], sep = ""))
  }
  
  if(class(list)[1] != "list") {
    stop(paste("From function IDfromList(): Argument 'list' is not a list therefore not valid.  It has class =",  class(list)[1], sep = ""))
  }
  
  for(item in list){
    
    tryCatch(
      {
        if(item@id == id) return(item)
      },
      error= function(cond){
        message(paste("Caught an expection. Function byIDfromList(id, list) found an item in the list argument with no slots. ", sep = ""))
      },
      warning=function(cond) {
      },
      finally={}
    )    
    
  }
  
  message(paste("ATTENTION! No object with id = ", id, " found in list ", deparse(substitute(list)), ", so probably your script will now fail.", sep = ""))
  
  
}
