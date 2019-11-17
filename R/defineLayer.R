######## DEFINE A LAYER OBJECT 

#' Define a Layer object 
#' 
#' This function is  \linkS4class{Layer} represents a sub-component or one particular aspect of a \linkS4class{Field} object, 
#' for example one PFT or one carbon pool.
#' 
#' This function is preferred to a \code{new("Layer",...)} initialisation because it does both the initialisation 
#' and also optionally adds the Field to to a \linkS4class{Format} object with the \code{add.to} argument.
#'
#' Note that no actual data is stored in the resultant \linkS4class{Layer}, only metadata. 
#' 
#' @param id A unique character string to identify this particular data.  Recommended to be alphanumeric because it is used to construct file names and to use underscores and full stops (periods) 
#' for separating characters where need be.   For example, "Standard_v4.0" or "Saatchi2011.HD"  (can be derived from \code{name} if a \code{name} is supplied, otherwise mandatory)
#' @param format A Format object to describe the type of source.  Can be GUESS, aDGVM, aDGVM2 or DGVMData (note no quotes since these are actual R objects not strings).  (Mandatory) DEPRECATED: alternatively a character string to identify the format of the files of this data sorce. This can be anything, but should be in order for "getField()" to work corrected 

#' MF TODO UPDATE THIS DOCUMENTATION Note that that \code{id}, \code{name}, \code{format}, and \code{dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS configurations.
#' @return A Layer object including metadata defined by empty data slots
#' @export
#' @seealso Layer
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

defineLayer <- function(id,
                         name,
                         colour,
                         properties = list(type = "Undefined"),
                         add.to){
  
  # if name is missing use id
  if(missing(name)) name <- id
  
  # check that properties is a full-named list
  if(!is.list(properties) || length(names(properties)) != length(properties) || "" %in% names(properties)) stop("The 'properties' argument to defineLayer() must be a named list")

  # make Source object from the supplied meta data
  layer <- new("Layer",
              id = id,
              name = name,
              colour = colour,
              properties = properties)
  
  if(!missing(add.to)) {
    
    # quick sanity check, require that add.to is a Format 
    if(!is.Format(add.to)) stop("add.to argument to defineLayer() must be a Format object (if supplied)")
    
    # okay, so we definitely have a Format object, so we have some already defined layers
    these.defined.layers <- add.to@defined.layers
    
    # now look for a Layer with the required id, in the exisiting Format
    # and if it exists get its index to over-write it but also give a warning
    # if not, add it to the end
    this.index <- length(these.defined.layers)+1
    for(this.layer.index in 1:length(these.defined.layers)){
      if(these.defined.layers[[this.layer.index]]@id == id) {
        warning(paste("Function defineLayer() is replacing an exisiting Layer with id =", id, "in Format", deparse(substitute(add.to)), sep = " "))
        this.index <- this.layer.index
       }
    }
    these.defined.layers[[this.index]] <- layer
    
    # make an updated Format object
    this.format <- copy(add.to)
    this.format@defined.layers <- these.defined.layers
    
    # finally, add the updated Format object to the global environment
    assign(x = deparse(substitute(add.to)), value = this.format, pos = ".GlobalEnv")

  }
  
  # return the Layer
  return(layer)
  
  
}

