#!/usr/bin/Rscript

############################## MAKE THE 'id' STRING FOR A FIELD
#
#' Make an ID string for a \code{\linkS4class{Field}}
#' 
#' Given a string for the quantity and temporal and spatial extents and averaging flags, build an appropriate (and unique) ID string
#' for use in the \code{id} slot of a \code{\linkS4class{Field}}and for filenames etc.
#' 
#' @param quant.string Character string to describe the \code{\linkS4class{Quantity}}, eg "lai" or "corrected.cmass" or "npp.diff"
#' @param first.year The first year if it has been cropped from the original duration, otherwise NULL
#' @param last.year The first year if it has been cropped from the original duration, otherwise NULL
#' @param year.aggregate.method Character, method by which this Field was temporally aggregated (if not temporally averaged leave blank of use "none")
#' @param spatial.extent.id The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param spatial.aggregate.method Character, method by which this Field was spatially aggregated (if not spatially averaged leave blank of use "none")
#' @return A character string 
#' @export
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeFieldID <- function(field,
                        source,
                        quant.string,
                        sta.info,
                        first.year,
                        last.year,
                        year.aggregate.method,
                        subannual.resolution = NULL,
                        spatial.aggregate.method = NULL,
                        subannual.aggregate.method = NULL,
                        spatial.extent.id = NULL, 
                        subannual.original = NULL){
  
  
  # separator character
  sep = "."
  
  # First, if an actual Field it provided as an argument, use it 
  if(!missing(field)  && !is.null(field)) {
    
    this.id <- makeFieldID(source = field@source,
                           quant.string = field@quant@id, 
                           sta.info = as(field, "STAInfo"))
    return(this.id)
    
  }
  
  
  ###################################################
  ##########  MAKE THE YEAR STRING
  
  # first the aggregation method
  year.string <- ""
  sep.char = ""
  if(sta.info@year.aggregate.method != "none") {
    year.string <- sta.info@year.aggregate.method
    sep.char = "_"
  }
  
  
  # then the interval
  got.first.year <- length(sta.info@first.year) > 0 
  got.last.year <- length(sta.info@last.year) > 0 
  if(got.first.year & got.last.year) {
    year.string <- paste0(year.string, sep.char, sta.info@first.year, "-", sta.info@last.year)
  }
  else if(!got.first.year & !got.last.year) {
    year.string <- paste0(year.string, sep.char, "all_years")
  }
  else {
    stop("Got to provide both first.year and last.year, or neither of them.")
  }
  
  
  
  # from source and quantity make leading part of the id
  field.id <- paste(source@id, quant.string, sep = sep)
  
 
  # spatial extent and aggregation
  if(sta.info@spatial.aggregate.method != "none"  
     || length(sta.info@spatial.extent.id) > 0
     || length(sta.info@spatial.extent) > 0) {
    field.id <- paste(field.id, "Spatial", sep = sep)
    if(sta.info@spatial.aggregate.method != "none")  field.id <- paste(field.id, sta.info@spatial.aggregate.method, sep = "_")
    if(length(sta.info@spatial.extent.id) > 0) field.id <- paste(field.id, sta.info@spatial.extent.id, sep = "_")
  }
  
  
  if(year.string != "") {
    field.id <- paste(field.id, year.string, sep = ".")
  }
  
  # subannual aggregation
  
  # First put the resolution actual resolution
  if(length(sta.info@subannual.resolution) > 0) field.id <- paste(field.id, sta.info@subannual.resolution, sep = ".")
  
  # then aggregate and original subannual resolution
   
  if(sta.info@subannual.aggregate.method != "none"  || length(sta.info@subannual.original) > 0) {
    
    field.id <- paste(field.id, sta.info@subannual.aggregate.method, sep = ".")
    
    if(length(sta.info@subannual.original) > 0 && sta.info@subannual.original != sta.info@subannual.resolution) {
      if(length(sta.info@subannual.original) > 0 ) field.id <- paste(field.id, "of", sta.info@subannual.original, sep =".")
      else field.id <- paste(field.id, "of", "Unknown", sep =".")
    }
    
  } 

  return(field.id)
  
}