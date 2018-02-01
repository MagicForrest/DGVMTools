############# ADD AN OBJECT TO A SOURCE ######################################

#' Add an object (either a \code{Field} or a  \code{SpatialComparison}) to a \code{Source} object to be use later.  
#' 
#' Stores an object in its run for later calculations, plotting, comparisons.
#' 
#' @param object Object to add to the \code{Source}.
#' @param run The \code{Source} object to which the object argument should be added
#' @return A Source object the the object argument added
#' @export
#' @seealso Source, SourceInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

addToSource <- function(object, run){
  
  # Add a BiomeComaprison or RasterComparison to the list in the benchmarks slot 
  if(is.SpatialComparison(object)) {
    
    benchmark.list <- run@benchmarks
    benchmark.list[[object@id]] <- object
    run@benchmarks <- benchmark.list
    rm(benchmark.list)
    
  }
  
  # Add a Field to the list in the objects slot 
  else if(is.Field(object)) {
    
    # Check that run ids match, if not, stop because something is really wrong
    if(run@id != object@source@id){
      stop(paste("Adding Field ", object@id, " which comes from run with id = ",  object@source@id, " to run with id = ", run@id, ". If you are doing something funky, like averaging Fields from different runs to make a a Source representing an ensemble mean, then make sure that the ids match. Otherwise you will break the internal logic of DGVMTools so aborting. Contact the package creator if this seems wrong to you." , sep = ""))
    }
    
    model.objects.list <- run@objects
    model.objects.list[[object@id]] <- object
    run@objects <- model.objects.list
    rm(model.objects.list)
    
  }
  
  else{
    
    warning(paste("Cannot add object of class", class(object), "to a Source object", sep = " "))
    
  }
  
  return(run)
  
}