


calcLayer <- function(x, layers, denom.layers, new.name, method = sum) {
  
  # if it is a Field 
  if(is.Field(x)) { 
    x@data[, eval(new.name) := rowSums(.SD), .SDcols=layers]
    return(x)
  }
  #   dt <- x@data}
  # # Else assume it is a data.table
  # else{ dt <- x }
  # 
  # 
  # dt[, eval(new.name) := rowSums(.SD), .SDcols=layers]
  # 
  # # if it is a Field 
  # if(is.Field(x)) { 
  #   x@data <- dt <- x@data
  #   return(x)
  # }
  # # Else assume it is a data.table
  # else{ return(dt)}
  # 
  
  
}