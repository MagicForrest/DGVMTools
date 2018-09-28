#!/usr/bin/Rscript

#' Retrieve a Quantity from the standard list.
#'
#' Function to get an Quantity based on an ID string *and* the scope (i.e. a particular model or the 'Standard' properties) from the standard \code{dgvm.quantities} list
#' 
#' @param quant.id String holding the id of the \code{Quantity} you want
#' @param context The context in which to look up the Quantity.  Can be a Format object, Source object or a list (of Quantity objects). 
#' Leave blank to lookup from the Standard.quantities list.  
#' @param verbose Logical, if TRUE (default) give some extra information
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @return A \code{Quantity} object if it finds the right one.  Otherwise the code halts.
#' @export
lookupQuantity <- function(quant.id, context = Standard.quantities, verbose = TRUE){
  
  if(is.Format(context)) {
    specific.quantities.list <- context@quantities 
  }
  else if(is.Source(context)) {
    specific.quantities.list <- context@format@quantities 
  }
  else if(is.list(context)){
    specific.quantities.list <- context
  }
  else if(missing(context)) {
    specific.quantities.list <- Standard.quantities
  }
  else {
    stop(paste0("Can't lookup from context class ", class(context)))
  }
  
  # this code will make the function first check for a user-defined Standard.quantities list
  local.Standard.quantities <- get('Standard.quantities', envir=.GlobalEnv)
  
  # First look for the exact quant.id model combination
  for(quant in specific.quantities.list){
    if(quant.id == quant@id) return(quant)
  }
  
  # Otherwise, look for the standard quantity
  for(quant in local.Standard.quantities){
    if(quant.id == quant@id) return(quant)
  }
  
  # this code will make the function first check for a user-defined dgvm.quantities list
  local.supported.biome.schemes <- get('supported.biome.schemes', envir=.GlobalEnv)

  for(biome.scheme in local.supported.biome.schemes){
    if(as(object = biome.scheme, Class = "Quantity")@id == quant.id) return(as(object = biome.scheme, Class = "Quantity"))
  }
  
  # fail because can't find the quantity
  stop(paste0("Can't find a quantity with id = ", quant.id, " anywhere in this context, or in Standard.quantities, or the supported.biomes.schemes, so failing."))
  
}


