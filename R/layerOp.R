#' Layer Operation
#' 
#' Perform an operation on some layers with a Field to make a new layer within the Field (or change their values, or remove them). For example, adding some layers, dividing one layer by another or setting some layers to a constant value. 
#' \bold{NOTE}: this function performs 'in-place' operations which, unlike normal R functions, will actually change the objects passed in as the arguments, not just return a value! Read what this means below. 
#' 
#' @param x The x object on which we are operating.
#' @param operator The operator we are applying, can be:
#' \itemize{
#'  \item{"+" (or "sum" or "add")} {Add the layers, can apply to any number of layers}
#'  \item{"mean" (or "average")} {Average the layers, can apply to any number of layers}
#'  \item{"*" (or "multiply" or "product")} {Multiply the layers, can apply to any number of layers}
#'  \item{"-" (or "subtract" or "minus")} {Subtract one layer from another. Requires exactly two layers to be specified, subtracts the second from the first, ie. layer1 - layer 2.}
#'  \item{"/" (or "divide" or "through")} {Divide one layer by another. Nore this does 'safe division' which returns zero if the denomination is zero. Requires exactly two layers to be specified, divides the second by the first, ie. layer1 / layer 2.}
#'  \item{"max.layer"} {Gets the layer with the maximum value from the input layers (is a layer of factors).  If they are all zero at a point then "None" is assigned. In the case of ties, the first layer in the layers arguement will be returned as the max.}
#'  \item{"min.layer"} {Gets the layer with the minimum value from the input layers (is a layer of factors).  If they are all zero at a point then "None" is assigned. In the case of ties, the first layer in the layers arguement will be returned as the min.}
#'  \item{\emph{any numeric value}} {Sets each of the layers specified uniformly to the numeric value specified, most usefuly for 0.  Previously not existing layers in the layers and new.layer argument will be created.}
#'  \item{\emph{NULL}} {A special case of the above which removes the layers from the Field}
#'  \item{\emph{Whatever function}} {Now we are into crazy territory!  You can provide any function (the actual function, not a string ) that operates on a vector of numerics and it might just work!  Works for sd, var, min and max, but your mileage may vary.}
#'  \item{\emph{Something else?}}{Contact the author!}
#' }
#' @param layers The names of the layers upon which to operate (as a vector of characters).  Furthermore, one can utilise a handy a handy trick whereby any layer spcecified, 
#' @param new.layer A single character specifying the name of the new layer, will over-write an existing layer if already present.  Will be built automatically if not specified.
#'
#' @return A Field (but not this is not strictly necessary since the objects are changed in place)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export  
#'    

layerOp <- function(x, operator, layers, new.layer){
  
  t1 <- Sys.time()
  
  
  ### PREAMBLE - find layers which start with a '.' and expand them to PFT ids
  final.layers <- c()
  for(layer in layers) {
    
    # expand the ".xxxx" arguments
    if(substring(layer, 1, 1) == '.') {
      
      criteria <- substring(layer, 2)
      if(tolower(criteria) == "pfts" || tolower(criteria) == "pft") expanded.layers <- listPFTs(x)
      else expanded.layers <- listPFTs(x, criteria)
      final.layers <- append(final.layers, unlist(expanded.layers))
      
    }
    # else if the layer is not present in  the input Field and the operator is *not* NULL or numeric
    # then ignore the layer with a warning
    else if(!(layer %in% layers(x)) && !(is.null(operator) || is.numeric(operator))) {
      warning(paste0("Layer ", layer, " is not is in the available layers, so this layer will be ignored.\n"))
    }
    # otherwise include the layer
    else {
      final.layers <- append(final.layers, layer)
    }
    
  }
  
  
  
  # reset the layers to the new ones, and if no layers warn and exit
  # but not if the operator is NULL or a numeric, we *expect* that layers from maybe be missing in this case
  layers <- final.layers
  if(length(layers) == 0) {
    if(!missing(new.layer)){
      warning(paste0("No input layers found when building layer ", new.layer, ", so making a 'zero layer'.  Please check that is is really what you want"))
      x@data <- x@data[, (new.layer) := 0]
    }
    else {
      warning(paste0("No valid input layers found, so returning field unchanged"))
    }
    return(x)
  }
  
  
  
  # First consider the numeric or NULL case to set or remove layers
  if(is.null(operator) || is.numeric(operator)){
    if(length(operator) > 1 ) stop(paste0("When assigning values to layers using layerOp() you need to provide a single value or NULL, you provided ", length(operator)))
    x@data <- x@data[, (layers) := operator]
    if(!missing(new.layer)) x@data <- x@data[, (new.layer) := operator]
    return(x)
  }
  
  # else if character
  else if(is.character(operator)) {
    
    # add
    if(operator == "+" || operator == "sum" || operator == "add") {
      if(missing(new.layer)) new.layer <- paste0(layers, collapse = "+")
      x@data <- x@data[, (new.layer) := rowSums(.SD), .SDcols = layers]
    }
    
    # mean
    else if(operator == "mean" || operator == "average") {
      if(missing(new.layer)) new.layer <- paste0("mean(", paste0(layers, collapse = ","), ")")
      x@data <- x@data[, (new.layer) := rowMeans(.SD), .SDcols = layers]
    }
    
    # product
    else if(operator == "*" || operator == "multiply" || operator == "product") {
      if(missing(new.layer)) new.layer <- paste0(layers, collapse = "*")
      x@data <- x@data[, (new.layer) := apply(.SD, 1, prod), .SDcols = layers]
    }
    
    # subtract
    else if(operator == "-" || operator == "subtract" || operator == "minus") {
      if(length(layers) != 2) stop(paste0("Can only apply the subtraction operator between exactly 2 layers, you have provided ", length(layers), " (", paste0(layers, collapse=","), ")"))
      if(missing(new.layer)) new.layer <- paste0(layers, collapse = "-")
      x@data <- x@data[, (new.layer) := get(layers[1]) - get(layers[2])]
    }
    
    # divide
    else if(operator == "/" || operator == "divide" || operator == "through") {
      if(length(layers) != 2) stop(paste0("Can only apply the division operator between exactly 2 layers, you have provided ", length(layers), " (", paste0(layers, collapse=","), ")"))
      if(missing(new.layer)) new.layer <- paste0(layers, collapse = "/")
      x@data <- x@data[, (new.layer) := get(layers[1]) %/0% get(layers[2])]
    }
    
    # max
    else if(operator == "max.layer") {
      if(missing(new.layer)) new.layer <- paste0("max.layer(", paste0(layers, collapse = ","), ")")
      max.layer <- function(x){ 
        if(all(!x)) return("None")
        return(names(x)[which.max(x)])
      }
      x@data <- x@data[, (new.layer) := factor(apply(.SD, 1, max.layer)), .SDcols = layers]
    }
    
    # min 
    else if(operator == "min.layer") {
      if(missing(new.layer)) new.layer <- paste0("min.layer(", paste0(layers, collapse = ","), ")")
      min.layer <- function(x){ 
        if(all(!x)) return("None")
        return(names(x)[which.min(x)])
      }
      x@data <- x@data[, (new.layer) := factor(apply(.SD, 1, min.layer)), .SDcols = layers]
    }
    
    # else
    else {
      stop(paste("Unknown operator (as character)", operator, " to layerOp().  If you want to try a function, don't quote it."))
    }
    
  }
  
  # maybe a random function works
  else if(is.function(operator)) {
    function.name <- match.call()[[3]]
    if(missing(new.layer)) new.layer <- paste0(function.name, "(", paste0(layers, collapse = ","), ")")
    x@data <- x@data[, (new.layer) := apply(.SD, 1, operator), .SDcols = layers]
  }
  
  else {
    stop("Wrong")
  }
  
  t2 <- Sys.time()
  #print(t2-t1)
  return(x)
  
}