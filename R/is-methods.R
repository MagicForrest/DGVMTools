#!/usr/bin/Rscript


#' Check if an object is a particular type of DGVMTools object.   
#'
#' Returns TRUE if an object of the particular object type and FALSE otherwise 
#' 
#' @param input Any R object to be checked
#' @param spatial check if input is a spatial object
#' @param temporal check if input is a temporal object
#' @param site check if input is a site object
#' @return logical
#' @name is.object-methods
#' @rdname is.object-methods
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
NULL

#' @rdname is.object-methods
#' @export
is.DataObject <- function(input, spatial=FALSE, temporal=FALSE, site=FALSE) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package"))) {
    if (class.def[1] == "DataObject" && attr(class.def, "package")=="DGVMTools") {
      ## JS: check more carefully if this if structure makes sense
      if (spatial && !temporal && !site) {
        if (!input@is.site && !input@is.spatially.averaged)
          return(TRUE)
      } else if (!spatial && temporal) {
        if (!input@is.temporally.averaged)
          return(TRUE)
      } else if (site) {
        if (input@is.site)
          return(TRUE)
      } else if (!spatial && !temporal && !site) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
  return(FALSE)
}

#' @rdname is.object-methods
#' @export
is.ModelObject <- function(input, spatial=FALSE, temporal=FALSE, site=FALSE) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package"))) {
    if (class.def[1] == "ModelObject" && attr(class.def, "package")=="DGVMTools") {
      ## JS: check more carefully if this if structure makes sense
      if (spatial && !temporal && !site) {
        if (!input@is.site && !input@is.spatially.averaged)
          return(TRUE)
      } else if (!spatial && temporal) {
        if (!input@is.temporally.averaged)
          return(TRUE)
      } else if (site) {
        if (input@is.site)
          return(TRUE)
      } else if (!spatial && !temporal && !site) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
  return(FALSE)
}



#' @rdname is.object-methods
#' @export
is.ModelRun <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "ModelRun" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}


#' @rdname is.object-methods
#' @export
is.Quantity <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "Quantity" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}


#' @rdname is.object-methods
#' @export
is.SpatialComparison <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "SpatialComparison" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}
