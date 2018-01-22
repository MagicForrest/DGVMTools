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
#' @param temporal.resolution Character string to describe the temporal resolution of this data, eg. "Annual" or "Daily"
#' @param spatial.resolution Character string to describe the spatial resolution of this data eg "HD" (for half degree) or "QD" (for quarter degree)
#' @param temporal.extent.id The temporal extent of this object if it has been cropped from the orginal duration, otherwise NULL
#' @param spatial.extent.id The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param temporal.aggregate.method Character, method by which this Field was temporally aggregated (if not temporally averaged leave blank of use "none")
#' @param spatial.aggregate.method Character, method by which this Field was spatially aggregated (if not spatially averaged leave blank of use "none")
#' @return A character string 
#' @export
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeFieldID <- function(source.info,
                        var.string,
                        spatial.resolution = NULL, 
                        temporal.resolution = NULL,
                        subannual.resolution = NULL,
                        spatial.aggregate.method = NULL,
                        temporal.aggregate.method = NULL, 
                        subannual.aggregate.method = NULL,
                        temporal.extent.id = NULL, 
                        spatial.extent.id = NULL, 
                        subannual.original = NULL){
  
  
  # separator character
  sep = "."
  
  # from source.info make the leading part of the id, note the difference between model- and data-derived fields
  field.id <- paste(source.info@format, source.info@id, sep = sep)

  # quantity
  field.id <- paste(field.id, var.string, sep = sep)
  
  # spatial aggregation
  if((!missing(spatial.aggregate.method) && !is.null(spatial.aggregate.method) && tolower(spatial.aggregate.method) != "none") || (!missing(temporal.aggregate.method) && !is.null(temporal.aggregate.method) && tolower(temporal.aggregate.method) != "none")) {
    field.id <- paste(field.id, "Spatial", sep = sep)
    if(!missing(spatial.aggregate.method) && !is.null(spatial.aggregate.method) && tolower(spatial.aggregate.method) != "none")  field.id <- paste(field.id, spatial.aggregate.method, sep = ".")
    if(!missing(spatial.extent.id) && !is.null(spatial.extent.id) && tolower(spatial.extent.id) != "Full") field.id <- paste(field.id, spatial.extent.id, sep = ".")
  }
  
  # temporal aggregation
  if((!missing(temporal.aggregate.method) && !is.null(temporal.aggregate.method) && tolower(temporal.aggregate.method) != "none") || (!missing(temporal.aggregate.method) && !is.null(temporal.aggregate.method) && tolower(temporal.aggregate.method) != "none")) {
    field.id <- paste(field.id, "temporal", sep = sep)
    if(!missing(temporal.aggregate.method) && !is.null(temporal.aggregate.method) && tolower(temporal.aggregate.method) != "none")  field.id <- paste(field.id, temporal.aggregate.method, sep = ".")
    if(!missing(temporal.extent.id) && !is.null(temporal.extent.id) && tolower(temporal.extent.id) != "Full") field.id <- paste(field.id, temporal.extent.id, sep = ".")
  }
  
  # subannual aggregation
  if(!missing(subannual.aggregate.method) &&  !is.null(subannual.aggregate.method) && !tolower(subannual.aggregate.method) == "none")  field.id <- paste(field.id, subannual.aggregate.method, sep = ".")
  if(!missing(subannual.original) && !is.null(subannual.original) && !tolower(subannual.original) == "none") field.id <- paste(field.id, "of", subannual.original, sep =".")
  
  
  return(field.id)
  
}