############# REMOVE AN OBJECT FROM A SOURCE ######################################

#' Remove an object (either a \code{Field} from a \code{Source} object to reclaim memory.  
#' 
#' Processing a lot of different files and storing the full output internally (in the Source) may use too much memory. You can get around this by not storing them internally, 
#' but if you need to do this, for example because you want to average many spatial or temporal extents from one output file, this function lets you remove Field to free the space again.  
#' 
#' @param object.id The id of the object to remove  to the \code{Source},   
#' @param run The \code{Source} object to which the object argument should be added
#' @return A Source with the object removed
#' @export
#' @seealso Source, SourceInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

removeFromSource <- function(object.id, run){
  
  
  if(object.id %in% names(run@objects)) {
    
    model.objects.list <- run@objects
    model.objects.list[[object.id]] <- NULL
    run@objects <- model.objects.list
    rm(model.objects.list)
    gc()
    
  }
  
  else{
    warning(paste("Can't remove Field with id ", object.id, " from run ", run@id, " because I can't find it in the run!"))
  }
  
  
  return(run)
  
}

