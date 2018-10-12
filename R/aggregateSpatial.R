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
#' @param ... Extra arguments passed to addArea function if a weighted method is being used.
#' @return A Field, DataObject or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateSpatial.uncompiled <- function(input.obj,
                                        method = "mean",
                                        verbose = FALSE,
                                        ...){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  Year = Lat = Lon = area = NULL
  
  ### SET UP THE AGGREGATE METHOD 
  
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
  
  
  # get the 'by' colums (these are basically the time dimensions if present)
  # check to see if Year is still a colmun name (it might have been averaged away)
  # Check the 'by' dims
  by.dims <- c()
  avail.dims <- getDimInfo(input.obj)
  for(possible.dim in list("Year","Season","Month","Day")){
    if(possible.dim %in% avail.dims) by.dims <- append(by.dims, possible.dim)
  }
  
  # 
  # 
  # by.cols <- list()
  # for(time.dim in c("Year", "Month", "Day")) {
  #   if(time.dim %in% names(input.dt)) by.cols[[length(by.cols)+1]] <- time.dim
  # }
  
  # Do the aggregating
  if (method == "w.mean") {
    if (!any(colnames(input.dt)=="area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=verbose, ...)
    }
    if(verbose) message(paste("Spatially averaging (with area weighting) ...", sep = ""))
    
    
    output.dt <- input.dt[,lapply(.SD, method.function, w=area), by=by.dims]
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
    
    output.dt <- input.dt[, lapply(.SD, method.function), by=by.dims]
    output.dt[,area:=NULL]
    
  } 
  
  # if not weighted
  else {
    if(verbose) message(paste("Spatially aggregating with function ", method," and no area-weighting ...", sep = ""))
    
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
    input.obj@id <- makeFieldID(source = input.obj@source,
                                var.string = input.obj@quant@id, 
                                sta.info = as(input.obj, "STAInfo"))
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
#' @inheritParams aggregateSpatial.uncompiled
#' @return A Field, DataObject or data.table depending on the input object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateSpatial <- compiler::cmpfun(aggregateSpatial.uncompiled)
