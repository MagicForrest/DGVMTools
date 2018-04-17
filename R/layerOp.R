#' Layer Operation
#' 
#' Perform an operation on some layers with a field to make a new layer  within the field (or change their values, or remove them). For example, adding some layers, dividing one layer by another or setting some layers to a constant value. 
#' \bold{NOTE}: this function performs 'in-place' operations which, unlike normal R functions, will actually change the arguments, not just return a value! Read what this means below. 
#' 
#' @param field The Field object on which we are operating.
#' @param operator The operator we are applying, can be:
#' \itemize{
#'  \item{"+" (or "sum" or "add")} {Add the layers, can apply to any number of layers}
#'  \item{"mean" (or "average")} {Average the layers, can apply to any number of layers}
#'  \item{"*" (or "multiply")} {Multiply the layers, can apply to any number of layers}
#'  \item{"-" (or "subtract")} {Subtract one layer from another. Requires exactly two layers to be specified, subtracts the second from the first, ie. layer1 - layer 2.}
#'  \item{"/" (or "divide")} {Divide one layer by another. Requires exactly two layers to be specified, divides the second by the first, ie. layer1 / layer 2.}
#'  \item{\emph{any numeric value}} {Sets each of the layers specified uniformly to the numeric value specified, most usefuly for 0. The new.layer argument is ignored.}
#'  \item{\emph{NULL}} {A special case of the above which removes the layers from the field}
#'  \item{\emph{Something else?}}{Contact the author!}
#' }
#' @param layers The names of the layers upon which to operate (as a vector of characters)
#' @param new.layer A single character specifying the name of the new layer, will over-write an existing layer if already present.  Will be built automatically if not specified, ignored is operator is a numeric or NULL
#'
#'  
#'
#'
#'
#'
#' @return A Field
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'  
#'    




layerOp <- function(field, operator, layers, new.layer){
  
  # First consider the numeric or NULL case to set or remove layers
  if(is.null(operator) || is.numeric(operator)){
    field@data[, (layers) := operator]
    return(field)
  }
  
  
  
  
  
}