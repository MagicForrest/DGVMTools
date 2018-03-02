
###################################################################################
##### MAKE TOTALS (LIFEFORM, PHENOLOGY, ZONE, ETC...)
#'
#' Automatically combine layers of a Field (or a data.table)
#' 
#' This is very useful and important function.  It aggregates different layers 
#' of a Field according the the desired method (or a sensible default).  Please note some special features of the arguments, designed for convenience, which are described below.
#' 
#' @param input The Field for which to aggregate layers.
#' @param layers The new layers to produce
#' @param method The method to use to aggregate the layers ie. "mean" or "sum".  However, it is often most sensible to leave it unspecified and use the default (see below)
#' @param PFT.data If calling the function on a data.table, it is neccessary to specify a PFT set here.  Normally this function will be called on  Field so it is not neccessary. 
#'
#' @details
#' Whilst the \code{method} argument can be specified for maximum flexibility, the recommended usage is not specify this argument.
#' In this case, the function uses the default of the Quantity object of the Field, which should be the sensible option.  
#' For example, per-PFT variables (such as lai or cmass) should typically be summed, 
#' but monthly variables (such as soil water content) should be average to calculate the seasonal means.
#' 
#' For convenience, both \code{layers} will be expanded using \code{expandLayers}.
#' This allows all lifeforms totals in a Field to be calculated using a simple call such as
#' 
#'  \code{veg.obj <- autoLayer(veg.obj, c("lifeforms"))}
#'  
#' See documention of \code{expandLayers} for details.
#' 
#' @return A Field (or data.table) with the new layers added
#' @import data.table
#' @export
#' @seealso expandLayers getVegFractions
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
autoLayer <- function(input, layers, method = NULL, PFT.data = NULL){
  
  Woody = Total = TempTotal = NULL
  
  ### HANDLE CLASS OF INPUT OBJECT
  # Here allow the possibility to handle both Fields and data.tables directly (for internal calculations)
  
  # if it is a Field 
  if(is.Field(input)) {
    # We get a warning about a shallow copy here, suppress it
    suppressWarnings(dt <- input@data)
    PFT.data <- input@source@pft.set
    # also if no specfic method specified, pull it from the the Field
    if(is.null(method)) method <- input@quant@aggregate.method
  }
  # Else assume it is a data.table
  else{
    print("INFO - using autoLayer on a data.table")
    warning("INFO - using autoLayer on a data.table")
    
    suppressWarnings(dt <- input)
    # if a data.table has been supplied but not method, use sums, but issue a warning
    if(is.null(method)) {
      method <- "sum"
      warning("autoLayer() has been called on a data.table but the aggregation method has not been specified so assuming sum.  Is this what you wanted? ")
      message("autoLayer() has been called on a data.table but the aggregation method has not been specified so assuming sum.  Is this what you wanted? ")
    }
  }
  
  # auxiliary function to be apply'd in the case of max and min
  max.layer <- function(x){
    if(sum(x) == 0) {
      the.max <- "None"
    }
    else{
      the.max <- names(x)[which.max(x)]
    }
    return(the.max)
  }
  
  min.layer <- function(x){
    if(sum(x) == 0) {
      the.min <- "None"
    }
    else{
      the.min <- names(x)[which.min(x)]
    }
    return(the.min)
  }
  
  
  ### SET UP THE AGGREGATE METHOD 
  method <- match.arg(method, c("average", "mean", "sum", "total", "max", "min"))
  method <- switch(method,
                   mean = rowMeans,
                   average = rowMeans,
                   sum = rowSums,
                   total = rowSums,
                   max = max.layer,
                   min = min.layer)
  
  
  # GET PFTs PRESENT
  all.PFTs <- getPFTs(dt, PFT.data)
  
  ### EXPAND LAYERS
  
  # for special case of methods "max" and "min", do not expand "months" and "PFTs" because in these cases we want to provide one layer with the min/max
  # of all months/PFT, not seperate layers for each month/PFTs
  PFT.present <- FALSE
  if("PFT" %in% layers) {
    layers <- layers[-which(layers == "PFT")]
    PFT.present <- TRUE
  }
  
  # expands layer
  layers <- expandLayers(layers, dt, PFT.data)
  
  # remove PFTs from layers since a layer for a PFT is already the sum/avg/min/max for that particular PFT
  for(PFT in all.PFTs){ if(PFT@id %in% layers) layers <- layers[-which(layers == PFT@id)]}
  
  # add back in "Month" and "PFT
  if(PFT.present) layers <- append(layers, "PFT")
  
  # DEFINE STRING FOR MIN/MAX
  if(identical(method, max.layer)) method.string <- "Max"
  if(identical(method, min.layer)) method.string <- "Min"
  
  ### FOR PER-PFT FILES
  for(this.layer in layers){
    
    if(this.layer != "Month") {
      
      # build list of columns to combine for layer
      layer.cols <- c()
      for(PFT in all.PFTs){
        if(PFT@lifeform == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        if(PFT@zone == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        if(PFT@leafform == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        if(PFT@phenology == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        # Special case for Woody
        if(this.layer == "Woody" & (PFT@lifeform == "Tree" || PFT@lifeform == "Shrub")) {layer.cols <- append(layer.cols, PFT@id)}
        # Special case for Total and PFT
        if(this.layer == "Total" | this.layer == "PFT") {layer.cols <- append(layer.cols, PFT@id)}
      }
      
      # now combine the relevant columns
      
      # if not requiring the maximum or minimum
      if(!identical(method, max.layer) & !identical(method, min.layer)) {
        
        if(!is.null(layer.cols)) { suppressWarnings(dt[, eval(this.layer) := method(.SD), .SDcols = layer.cols]) }
        else { suppressWarnings(dt[, eval(this.layer) := 0])}
        
      }
      
      # else it is min/max
      else{
        
        if("Total" %in% layer.cols) layer.cols <- layer.cols[-which(layer.cols == "Total")]
        # MF TODO: make this faster/nicer?
        suppressWarnings(dt[, eval(paste0(method.string, this.layer) ) := apply(dt[,layer.cols,with=FALSE],FUN=method,MARGIN=1)])
        dt[, eval(quote(paste0(method.string, this.layer))) := as.factor(get(paste0(method.string, this.layer)))]
        
      }
      
    } # if not month
    
  } # for each layer
  
  
  if(is.Field(input)) {
    input@data <- dt
    return(input)
  }
  else {
    return(dt)
  }
  
}
