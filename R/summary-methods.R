

## print an experimental summary of the class Field.
##
## Don't know, which were the really important information to return,
## therfore this is just a template.
## so far simply returns a list. 
## Needs to also include a generic "print" for pretty output
##

#' Summary methods
#' 
#' Print easy to read summaries of DGVMTools objects
#' 
#' @param object a DGVMTools object
#' @param ... Other arguments, not currently used
#' @return A list of strings
#' @name Summary-methods
#' @rdname Summary-methods
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}         
setGeneric("summary", function(object,...) standardGeneric("summary"))

#' @rdname Summary-methods
#' @aliases summary
setMethod("summary", signature("Field"), function(object, ...) {
  ret <- list(id=object@id, name=object@name, model=object@model)
  
  spatial=NULL
  temporal=NULL
  full=NULL
  for (n in names(object@objects)) {
    if (object@objects[[n]]@spatial.aggregate.method) {
      temporal[[n]]=list(name=n,
                         description=object@objects[[n]]@quant@name,
                         units=object@objects[[n]]@quant@units,
                         colnames=colnames(object@objects[[n]]@data))
    } else if (object@objects[[n]]@year.aggregate.method) {
      spatial[[n]]=list(name=n,
                        description=object@objects[[n]]@quant@name,
                        units=object@objects[[n]]@quant@units,
                        colnames=colnames(object@objects[[n]]@data))
    } else {
      full[[n]]=list(name=n,
                     description=object@objects[[n]]@quant@name,
                     units=object@objects[[n]]@quant@units,
                     colnames=colnames(object@objects[[n]]@data))
    }
  }
  if (!is.null(spatial)) {
    ret[['spatial']] = spatial
  }
  if (!is.null(temporal)) {
    ret[['temporal']] = temporal
  }
  if (!is.null(full)) {
    ret[['full']] = full
  }
  
  return(ret)
})
