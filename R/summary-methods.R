

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
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @exportMethod summary
setGeneric("summary", function(object,...) standardGeneric("summary"))


#' @rdname Summary-methods
#' @aliases summary
setMethod("summary", signature(object="Source"), function(object) {
  
  cat(paste0("Source:\n"))
  cat(paste0("id = ", "\"", object@id, "\"", "\n"))
  cat(paste0("name = ", "\"", object@name, "\"", "\n"))
  cat(paste0("format = ", "\"", object@format@id, "\"", "\n"))
  cat(paste0("directory = ", "\"", object@dir, "\"", "\n"))
  cat(paste0("lon-lat offset = (", object@lonlat.offset[1], ",", object@lonlat.offset[2], ")\n"))
  cat(paste0("year offset = ", object@year.offset, "\n"))
  cat(paste0("forcing data = ", "\"", object@forcing.data, "\"", "\n"))
  cat(paste0("london.centre = ", object@london.centre, "\n"))
  cat(paste0("land.use.included = ", object@land.use.included, "\n"))
  cat(paste0("institute = ", "\"", object@institute, "\"", "\n"))
  cat(paste0("contact = ", "\"", object@contact, "\"", "\n"))
  cat(paste0("PFT superset:", "\n"))
  all.PFTs <- c()
  for(PFT in object@pft.set){
    all.PFTs <- append(all.PFTs, PFT@id)
  }
  cat(paste0(all.PFTs))
  
})



#' @rdname Summary-methods
#' @aliases summary
setMethod("summary", signature("Field"), function(object, ...) {
  ret <- list(id=object@id, name=object@name, format=object@format)
  
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
