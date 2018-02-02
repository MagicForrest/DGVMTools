#!/usr/bin/Rscript

############################## MAKE THE 'id' STRING FOR A FIELD
#
#' Make an ID string for a \code{Field}
#' 
#' Given a string for the quantity and temporal and spatial extents and averaging flags, build an appropriate (and unique) ID string
#' for use in the \code{id} slot of a \code{Field} and for filenames etc.
#' 
#' @param var.string Character string to describe the variable, eg "lai" or "corrected.cmass" or "npp.diff"
#' @param unit.string Character string to describe the units in which the data are measured
#' @param first.year The first year if it has been cropped from the orginal duration, otherwise NULL
#' @param last.year The first year if it has been cropped from the orginal duration, otherwise NULL
#' @param year.aggregate.method Character, method by which this Field was temporally aggregated (if not temporally averaged leave blank of use "none")
#' @param spatial.extent.id The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param spatial.aggregate.method Character, method by which this Field was spatially aggregated (if not spatially averaged leave blank of use "none")
#' @return A character string 
#' @export
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeFieldID <- function(source.info,
                        var.string,
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
  
  
  ###################################################
  ##########  MAKE THE YEAR STRING
  
  # first the aggragation method
  year.string <- ""
  sep.char = ""
  if(!missing(year.aggregate.method) & !is.null(year.aggregate.method) & year.aggregate.method != "none") {
    year.string <- year.aggregate.method
    sep.char = "_"
  }
  
  
  # then the interval
  got.first.year <- !missing(first.year) & !is.null(first.year)
  got.last.year <- !missing(last.year) & !is.null(last.year)
  if(got.first.year & got.last.year) {
    year.string <- paste0(year.string, sep.char, first.year, "-", last.year)
  }
  else if(!got.first.year & !got.last.year) {
    year.string <- paste0(year.string, sep.char, "all_years")
  }
  else {
    stop("Got to provide both first.year and last.year, or neither of them.")
  }
  
  
  
  
  
  # from source.info make the leading part of the id, note the difference between model- and data-derived fields
  field.id <- paste(source.info@format, source.info@id, sep = sep)
  
  # quantity
  field.id <- paste(field.id, var.string, sep = sep)
  
  # spatial aggregation
  if((!missing(spatial.aggregate.method) && !is.null(spatial.aggregate.method) && tolower(spatial.aggregate.method) != "none")) {
    field.id <- paste(field.id, "Spatial", sep = sep)
    if(!missing(spatial.aggregate.method) && !is.null(spatial.aggregate.method) && tolower(spatial.aggregate.method) != "none")  field.id <- paste(field.id, spatial.aggregate.method, sep = ".")
    if(!missing(spatial.extent.id) && !is.null(spatial.extent.id) && tolower(spatial.extent.id) != "Full") field.id <- paste(field.id, spatial.extent.id, sep = ".")
  }
  
  
  if(year.string != "") {
    field.id <- paste(field.id, year.string, sep = ".")
  }
  
  # subannual aggregation
  if(!missing(subannual.aggregate.method) &&  !is.null(subannual.aggregate.method) && !tolower(subannual.aggregate.method) == "none")  field.id <- paste(field.id, subannual.aggregate.method, sep = ".")
  if(!missing(subannual.original) && !is.null(subannual.original) && !tolower(subannual.original) == "none") field.id <- paste(field.id, "of", subannual.original, sep =".")
  
  return(field.id)
  
}