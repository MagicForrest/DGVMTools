#!/usr/bin/Rscript


#' Check if an object is a particular type of DGVMTools object.   
#'
#' Returns TRUE if an object of the particular object type and FALSE otherwise 
#' 
#' @param input Any R object to be checked
#' @param spatial check if input is a spatial object
#' @param temporal check if input is a temporal object
#' @return logical
#' @name is.object-methods
#' @rdname is.object-methods
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
NULL

#' @rdname is.object-methods
#' @export
is.Field <- function(input, spatial=FALSE, temporal=FALSE) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package"))) {
    if (class.def[1] == "Field" && attr(class.def, "package")=="DGVMTools") {
      ## JS: check more carefully if this if structure makes sense
      if (spatial && !temporal) {
        if (input@spatial.aggregate.method == "none")
          return(TRUE)
      } else if (!spatial && temporal) {
        if (input@year.aggregate.method == "none")
          return(TRUE)
      } else if (!spatial && !temporal) {
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
is.ComparisonLayer <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "ComparisonLayer" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}



#' @rdname is.object-methods
#' @export
is.Source<- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "Source" && attr(class.def, "package")=="DGVMTools")
      return(TRUE)
  return(FALSE)
}

#' @rdname is.object-methods
#' @export
is.SourceInfo <- function(input) {
  class.def <- class(input)
  if (!is.null(attr(class.def, "package")))
    if (class.def[1] == "SourceInfo" && attr(class.def, "package")=="DGVMTools")
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
