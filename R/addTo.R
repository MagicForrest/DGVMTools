#' Add \linkS4class{Layer}/\linkS4class{Quantity} to a \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field}  
#' 
#' Adds metadata encapsulated in a \linkS4class{Layer} or \linkS4class{Quantity}
#' 
#' @param x The \linkS4class{Layer}/\linkS4class{Quantity} to be added
#' @param add.to The \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field}  
#' 
#' @return The returned oobject will be or an updated \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} 
#' object if "add.to" was specified.
#' 
#' @export
#' @seealso \linkS4class{Layer}, \linkS4class{Quantity}, \link{defineQuantity}, \link{defineLayer}
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

addTo <- function(x, add.to) {
  
  
  if(is.Layer(x)) {
    
    if(is.Format(add.to)) initial.layers <- add.to@predefined.layers
    else if(is.Source(add.to)) initial.layers <- add.to@defined.layers
    else if(is.Field(add.to)) initial.layers <- add.to@source@defined.layers
    else stop("add.to argument to defineLayer()/addTo() must be a Format, Source or Field (if supplied)")
    
    # now look for a Layer with the required id, in the exisiting Format
    # and if it exists get its index to over-write it but also give a warning
    # if not, add it to the end
    this.index <- length(initial.layers)+1
    for(this.layer.index in 1:length(initial.layers)){
      if(initial.layers[[this.layer.index]]@id == x@id) {
        warning(paste("Function defineLayer()/addTo() is replacing an exisiting Layer with id =", x@id, "in Format/Source/Field", deparse(substitute(add.to)), sep = " "))
        this.index <- this.layer.index
      }
    }
    initial.layers[[this.index]] <- x
    
    
    # make an updated Format add.to and return
    this.updated <- copy(add.to)
    if(is.Format(add.to)) {
      this.updated@predefined.layers <- initial.layers
      return(this.updated)
    }
    else if(is.Source(add.to)) {
      this.updated@defined.layers <- initial.layers
      return(this.updated)
    }
    else if(is.Field(add.to)) {
      this.updated@source@defined.layers <- initial.layers
      return(this.updated)
    }
    
  } # end if Layer
  
  else if(is.Quantity(x)) {
    
    if(is.Format(add.to)) initial.quantities <- add.to@quantities
    else if(is.Source(add.to)) initial.quantities <- add.to@format@quantities
    else if(is.Field(add.to)) initial.quantities <- add.to@source@format@quantities
    
    else stop("add.to argument to defineQuantity()/addTo() must be a Format, Source or Field (if supplied)")
    
    # now look for a Quantity with the required id, in the exisiting Format
    # and if it exists get its index to over-write it but also give a warning
    # if not, add it to the end
    this.index <- length(initial.quantities)+1
    for(this.quantity.index in 1:length(initial.quantities)){
      if(initial.quantities[[this.quantity.index]]@id == x@id) {
        warning(paste("Function defineQuantity()/addTo() is replacing an exisiting Quantity with id =", x@id, "in Format/Source", deparse(substitute(add.to)), sep = " "))
        this.index <- this.quantity.index
      }
    }
    initial.quantities[[this.index]] <- x
    
    
    # make an updated Format add.to and return
    this.updated <- copy(add.to)
    if(is.Format(add.to)) {
      this.updated@quantities <- initial.quantities
      return(this.updated)
    }
    else if(is.Source(add.to)) {
      this.updated@format@quantities <- initial.quantities
      return(this.updated)
    }
    else if(is.Field(add.to)) {
      this.updated@source@format@quantities <- initial.quantities
      this.updated@quant <- x
      this.updated@id <- makeFieldID(this.updated)
      return(this.updated)
    }
    
  } # end if Quantity
  
  else {
    stop("'x' argument to addTo can only be a Layer or a Quantity ")
  }
  
}