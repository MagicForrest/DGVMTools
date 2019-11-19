

#' @export
addTo <- function(x, add.to) {
  
  
  if(is.Layer(x)) {
    
    if(is.Format(add.to)) initial.layers <- add.to@predefined.layers
    else if(is.Source(add.to)) initial.layers <- add.to@defined.layers
    else if(is.Field(add.to)) initial.layers <- add.to@source@defined.layers
    else stop("add.to argument to defineLayer() must be a Format, Source or Field add.to (if supplied)")
    
    # now look for a Layer with the required id, in the exisiting Format
    # and if it exists get its index to over-write it but also give a warning
    # if not, add it to the end
    this.index <- length(initial.layers)+1
    for(this.layer.index in 1:length(initial.layers)){
      if(initial.layers[[this.layer.index]]@id == x@id) {
        warning(paste("Function defineLayer() is replacing an exisiting Layer with id =", x@id, "in Format", deparse(substitute(add.to)), sep = " "))
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
      this.updated@sorucedefined.layers <- initial.layers
      return(this.updated)
    }
    
  } # end if Layer
  
  else if(is.Quantity(x)) {
    
    stop("Adding Quantities not defined yet.")
    
    if(is.Format(add.to)) initial.layers <- add.to@predefined.layers
    else if(is.Source(add.to)) initial.layers <- add.to@defined.layers
    else if(is.Field(add.to)) initial.layers <- add.to@source@defined.layers
    else stop("add.to argument to defineLayer() must be a Format, Source or Field add.to (if supplied)")
    
    # now look for a Layer with the required id, in the exisiting Format
    # and if it exists get its index to over-write it but also give a warning
    # if not, add it to the end
    this.index <- length(initial.layers)+1
    for(this.layer.index in 1:length(initial.layers)){
      if(initial.layers[[this.layer.index]]@id == x@id) {
        warning(paste("Function defineLayer() is replacing an exisiting Layer with id =", x@id, "in Format", deparse(substitute(add.to)), sep = " "))
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
      this.updated@sorucedefined.layers <- initial.layers
      return(this.updated)
    }
    
  } # end if Quantity
  
  else {
    
    stop("'x' argument to addTo can only be a Layer or a Quantity ")
    
  }
  
}