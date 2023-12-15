#' Checks if STAInfo matches between Fields
#'
#' This is an internal function, used when checking preprocessed Fields in getField().
#'
#' @param sta.requested The STAInfo requested (in the getField() call)
#' @param sta.found The STAInfo found the in the existing Field (the one which will have been read from disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#'
#' @return Logical
#'

#' @keywords internal

checkSTAMatches <- function(sta.requested, sta.found, verbose) {
  
  
  
  # Check that the spatial extent matches before returning
  # Note that there are two cases to check here (specifically defined extents or just the same ids)
  
  #### CHECK SPATIAL DOMAIN ####
  # If no spatial extent was specified (ie length of spatial.extent argument was 0) and the spatial.extent.id of the Field that was read in is either: 
  # "Full" or 
  # "Global" (which comes some netCDF files processed by the DGVMData package) or
  # "Unspecified" (which comes from files read by the NetCDF Format of DGVMTools)
  # then we are sure we have got the full, original spatial extent of the dataset and can use it
  full.domain.matched <- FALSE
  if(length(sta.requested@spatial.extent) == 0 && sta.found@spatial.extent.id %in% c("Full", "Global", "Unspecified")) {
    full.domain.matched <- TRUE
    if(verbose) message("* Full spatial domain matched.")
  }
  
  # else, if they both have spatial extents specified, check that the actual spatial extents are the same
  cropped.domain.matched <- FALSE
  if(length(sta.requested@spatial.extent) > 0 && length(sta.found@spatial.extent)  > 0 ) {
    if(identical(sta.requested@spatial.extent, sta.found@spatial.extent)){
      cropped.domain.matched <- TRUE
      if(verbose) message("* Cropped spatial domain matched.")
    }
  }
  
  spatial.ok <-  full.domain.matched || cropped.domain.matched
  if(!spatial.ok) {
    # Otherwise we must discard this Field and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Spatial extent was requested with id = ", sta.requested@spatial.extent.id))
    message(paste("and extent definition:"))
    print(sta.requested@spatial.extent)
    message(paste("However, spatial extent found has id = ", sta.found@spatial.extent.id))
    message(paste("and extent definition:"))
    print(sta.found@spatial.extent)
    message("I am therefore reading the entire raw data again.")
    return(FALSE)
  }
  
  
  #### CHECK YEARS ####
  
  # check first.year (only if first.year was supplied, otherwise assume ok)
  if(length(sta.requested@first.year) > 0 && !is.null(sta.requested@first.year)) {
  if(!identical(as.integer(sta.found@first.year), as.integer(sta.requested@first.year))) {
      message(paste0("Attention: First year requested (", sta.requested@first.year, ") does not match first year in preprocessed file (", sta.found@first.year, "), I am therefore reading the entire raw data again."))
      warning(paste0("First year requested (", sta.requested@first.year, ") does not match first year in preprocessed file (", sta.found@first.year, "), I am therefore reading the entire raw data again."))
      return(FALSE)
    } 
  }
  else{
    if(verbose) message(paste0(" ** Argument first.year not specified so not checked.  I found '", sta.found@first.year, "', perhaps check this is what you wanted."))
  }
  # check last.year (only if last.year was supplied, otherwise assume ok)
  if(length(sta.requested@first.year) > 0 && !is.null(sta.requested@last.year)) {
    if(!identical(as.integer(sta.found@last.year),as.integer(sta.requested@last.year))) {
      message(paste0("Attention: Last year requested (", sta.requested@last.year, ") does not match last year in preprocessed file (", sta.found@last.year, "), I am therefore reading the entire raw data again."))
      warning(paste0("Last year requested (", sta.requested@last.year, ") does not match last year in preprocessed file (", sta.found@last.year, "), I am therefore reading the entire raw data again."))
      return(FALSE)
    } 
  }
  else{
    if(verbose) message(paste0(" ** Argument last.year not specified so not checked.  I found '", sta.found@last.year, "', perhaps check this is what you wanted."))
  }
  # check year.aggregate.method (only if last.year was supplied, otherwise assume ok)
  if(!length(sta.requested@year.aggregate.method) && !is.null(sta.requested@year.aggregate.method)) {
    if(!identical(sta.found@year.aggregate.method,sta.requested@year.aggregate.method)) {
      message(paste0("Attention: Requested year aggregate method (", sta.requested@year.aggregate.method, ") does not match year aggregate method in preprocessed file (", sta.found@year.aggregate.method, "), I am therefore reading the entire raw data again."))
      warning(paste0("Requested year aggregate method (", sta.requested@year.aggregate.method, ") does not match year aggregate method in preprocessed file (", sta.found@year.aggregate.method, "), I am therefore reading the entire raw data again."))
      return(FALSE)
    } 
  }
  else{
    if(verbose) message(paste0(" ** Argument year.aggregate.method not specified so not checked.  I found '", sta.found@year.aggregate.method, "', perhaps check this is what you wanted."))
  }
  if(verbose)  message("* Year matched.")
  
  #### CHECK SUBANNUAL ####
  
  # check subannual.resolution (only if subannual.resolution was supplied, otherwise assume ok)
  if(length(sta.requested@subannual.resolution) > 0 && !is.null(sta.requested@subannual.resolution)) {
    if(!identical(sta.found@subannual.resolution, sta.requested@subannual.resolution)) {
      message(paste0("Attention: Subannual resolution requested (", sta.requested@subannual.resolution, ") does not match first year in preprocessed file (", sta.found@subannual.resolution, "), I am therefore reading the entire raw data again."))
      warning(paste0("Subannual resolution requested (", sta.requested@subannual.resolution, ") does not match first year in preprocessed file (", sta.found@subannual.resolution, "), I am therefore reading the entire raw data again."))
      return(FALSE)
    } 
  }
  else{
    if(verbose) message(paste0(" ** Argument subannual.resolution not specified so not checked.  I found '", sta.found@subannual.resolution, "', perhaps check this is what you wanted."))
  }
  # check subannual.resolution (only if subannual.resolution was supplied, otherwise assume ok)
  if(length(sta.requested@subannual.original) > 0 && !is.null(sta.requested@subannual.original)) {
    if(!identical(sta.found@subannual.original != sta.requested@subannual.original)) {
      message(paste0("Attention: Original subannual resolution requested (", sta.requested@subannual.original, ") does not match first year in preprocessed file (", sta.found@subannual.original, "), I am therefore reading the entire raw data again."))
      warning(paste0("Original subannual resolution requested (", sta.requested@subannual.original, ") does not match first year in preprocessed file (", sta.found@subannual.original, "), I am therefore reading the entire raw data again."))
      return(FALSE)
    } 
  }
  else{
    if(verbose) message(paste0(" ** Argument subannual.original not specified so not checked.  I found '", sta.found@subannual.original, "', perhaps check this is what you wanted."))
  }
  
  # check subannual.aggregate.method (only if last.subannual was supplied, otherwise assume ok)
  if(length(sta.requested@subannual.aggregate.method) > 0 && !is.null(sta.requested@subannual.aggregate.method)) {
    if(!identical(sta.found@subannual.aggregate.method, sta.requested@subannual.aggregate.method)) {
      message(paste0("Attention: Requested subannual aggregate method (", sta.requested@subannual.aggregate.method, ") does not match subannual aggregate method in preprocessed file (", sta.found@subannual.aggregate.method, "), I am therefore reading the entire raw data again."))
      warning(paste0("Requested subannual aggregate method (", sta.requested@subannual.aggregate.method, ") does not match subannual aggregate method in preprocessed file (", sta.found@subannual.aggregate.method, "), I am therefore reading the entire raw data again."))
      return(FALSE)
    } 
  }
  else{
    if(verbose) message(paste0(" ** Argument subannual.aggregate.method not specified so not checked.  I found '", sta.found@subannual.aggregate.method, "', perhaps check this is what you wanted."))
  }
  
  if(verbose)  message("* Subannual matched.")
  
  return(TRUE)  


}