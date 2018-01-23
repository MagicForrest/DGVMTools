#!/usr/bin/Rscript


############################################################################
######################### SPATIALLY AGGREGATE ##############################
############################################################################


#' Spatially average a Field, DataObject or data.table 
#' 
#' Spatially average all gridcells of a Field, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param input.obj Field, DataObject or data.table to be averaged  
#' @param method The method with which to spatially aggregate.  Can be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field, DataObject or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateSpatial.uncompiled <- function(input.obj,
                                        method = "mean",
                                        verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  Year = Lat = Lon = area = NULL
  
  ### SET UP THE AGGREGATE METHOD 
  print(method)
  method <- match.arg(method, c("weighted.mean", "w.mean", "mean", "weighted.sum", "w.sum", "sum", "max", "min", "sd", "var"))
  
  method.function <- switch(method,
                            weighted.mean = stats::weighted.mean,
                            w.mean = stats::weighted.mean,
                            mean = mean,
                            weighted.sum = sum,
                            w.sum = sum,
                            sum = sum,
                            max = max,
                            min = min,
                            sd = stats::sd,
                            var = stats::var)
  
  if(method == "weighted.mean") method = "w.mean"
  if(method == "weighted.sum") method = "w.sum"
  
  # sort out the input object class
  if(is.Field(input.obj)) {input.dt <- input.obj@data}
  else if(is.data.table(input.obj)) {input.dt <- input.obj}
  
  
  
  # Do the aggregating
  if (method == "w.mean") {
    if (!any(colnames(input.dt)=="area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=verbose)
    }
    if(verbose) message(paste("Spatially averaging (with area weighting) ...", sep = ""))
    
    # check to see if Year is still a colmun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, method.function, w=area), by=list(Year)]
    else {
      output.dt <- input.dt[,lapply(.SD, method.function, w=area)]
    }
    output.dt[,area:=NULL]
  } 
  else if (method == "w.sum") {
    if (!any(colnames(input.dt)=="area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=verbose)
    }
    if(verbose) message(paste("Spatially summing (with area weighting) ...", sep = ""))
    
    
    
    # get all column names, remove the spatial and temporal and return
    remove <- c("Lon", "Lat", "Year", "Month", "Day", "area")
    col.names <- names(input.dt)
    col.names <- col.names[!col.names %in% remove]
    
    # check to see if Year is still a column name (it might have been averaged away)
    input.dt[, (col.names) := lapply(.SD, function(x) x * input.dt[['area']] ), .SDcols = col.names]
    
    if("Year" %in% names(input.dt)) {
      output.dt <- input.dt[, lapply(.SD, method.function), by=list(Year)]
    }
    else {
      output.dt <- input.dt[, (col.names) := lapply(.SD, method.function)]
    }
    output.dt[,area:=NULL]
    
  } 
  
  # if not weighted
  else {
    if(verbose) message(paste("Spatially aggregating with function", method," and no area-weighting ...", sep = ""))
    
    # Check the 'by' dims
    by.dims <- c()
    avail.dims <- getSTInfo(input.obj)
    for(possible.dim in list("Year","Season","Month","Day")){
      if(possible.dim %in% avail.dims) by.dims <- append(by.dims, possible.dim)
    }
    
    output.dt <- input.dt[,lapply(.SD, method.function), by=by.dims]
    
  }
  
  # remove the Lon and Lat columns 'cos they dun' make so much sense no mo'
  output.dt[,Lon:=NULL]
  output.dt[,Lat:=NULL]
  
  if(verbose) message("Averaged")
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyDGVM(output.dt)
  
  # sort out the input object class
  if(is.Field(input.obj)) {
    input.obj@data <- output.dt
    input.obj@spatial.aggregate.method <- method
    input.obj@id <- makeFieldID(input.obj@quant@id, temporal.extent.id = input.obj@temporal.extent.id, spatial.extent.id = input.obj@spatial.extent, temporal.aggregate.method = input.obj@temporal.aggregate.method, spatial.aggregate.method = input.obj@spatial.aggregate.method)
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}

######################### SPATIALLY AVERAGE ##############################
#
#' Spatially average a Field, DataObject or data.table 
#' 
#' Spatially average all gridcells of a Field, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param input.obj A Field, DataObject or data.table to be averaged 
#' @param method The method with which to spatially aggregate.  Can be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field, DataObject or data.table depending on the input object
#' @keywords internal
#' @import  compiler cmpfun
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateSpatial <- compiler::cmpfun(aggregateSpatial.uncompiled)
