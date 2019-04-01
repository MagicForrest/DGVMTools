#!/usr/bin/Rscript


############################################################################
######################### SPATIALLY AGGREGATE ##############################
############################################################################


#' Spatially aggregate a Field
#' 
#' Spatially aggragte all gridcells of a Field (or a data.table assuming it has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon, Lat)  
#'
#' @param input.obj Field (or data.table) to be averaged  
#' @param method The method with which to spatially aggregate.  Can be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".
#' @param verbose If TRUE give some progress update about the averaging.
#' @param ... Extra arguments passed to addArea function if a weighted method is being used.
#' @return A Field or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateSpatial.uncompiled <- function(input.obj,
                                        method = "mean",
                                        verbose = FALSE,
                                        ...){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  Year = Lat = Lon = Area = NULL
  
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

  # Do the aggregating
  if (method == "w.mean") {
    if (!any(colnames(input.dt)=="Area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=FALSE, ...)
    }
    if(verbose) message(paste("Spatially averaging (with area weighting) ...", sep = ""))
    
    output.dt <- input.dt[,lapply(.SD, method.function, w=Area), by=by.dims]
    output.dt[,Area:=NULL]
    
  } 
  else if (method == "w.sum") {
    if (!any(colnames(input.dt)=="Area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=FALSE, ...)
    }
    if(verbose) message(paste("Spatially summing (with area weighting) ...", sep = ""))
    
    
    
    # get all column names, remove the spatial and temporal and return
    remove <- c("Lon", "Lat", "Year", "Month", "Day", "Area")
    col.names <- names(input.dt)
    col.names <- col.names[!col.names %in% remove]
    
    # check to see if Year is still a column name (it might have been averaged away)
    input.dt[, (col.names) := lapply(.SD, function(x) x * input.dt[['Area']] ), .SDcols = col.names]
    
    output.dt <- input.dt[, lapply(.SD, method.function), by=by.dims]
    output.dt[,Area:=NULL]
    
  } 
  
  # if not weighted
  else {
    if(verbose) message(paste("Spatially aggregating with function ", method," and no area-weighting ...", sep = ""))
    
    output.dt <- input.dt[,lapply(.SD, method.function), by=by.dims]
    
  }
  
  # remove the Lon and Lat columns 'cos they dun' make so much sense no mo'
  output.dt[,Lon:=NULL]
  output.dt[,Lat:=NULL]
  
  if(verbose) message("Aggregated")
  
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


#' Spatially aggregate a Field
#' 
#' Spatially aggregate all gridcells of a Field (or a data.table assuming it has columns "Lon" and "Lat").  Can use a variety of aggregation methods, 
#' see argument `method`
#'
#' @inheritParams aggregateSpatial.uncompiled
#' @return A Field or data.table depending on the input object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
#' @details  For summing variables which are defined on a per \eqn{m^2} use the method = "w.sum" (or "weighted.sum) to scale from \eqn{m^-2} to gridcell
#' totals before adding (gridcell area calculated by addArea() function).  For averaging, use method = "w.mean" (or "weighted.mean") to weight by the 
#' gridcell area. 
#' 
#' @examples 
#' \donttest{
#'  
#' # Get an example Field
#' africa.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralAfrica", package = "DGVMTools")
#' africa.Source <- defineSource(name = "LPJ-GUESS", dir = africa.dir,  format = GUESS)
#' field <- getField(source = africa.Source, var = "cmass")
#' 
#' # calculate mean over gridcells and look at the resulting data table
#' spatial.mean <- aggregateSpatial(input.obj = field, method = "mean", verbose = TRUE)
#' print(spatial.mean@data)
#' 
#' # do weighted.sum and look at the resulting data table
#' spatial.wsum <- aggregateSpatial(input.obj = field, method = "w.sum", verbose = TRUE)
#' print(spatial.wsum@data) 
#' 
#' # plot the weight and unweighted sums as a time series
#' # (also modify ylab to be more meaningful)
#' p <- plotTemporal(list(spatial.wsum))
#' p <- p + ylab("Vegetation Carbon Mass (kgC))
#' print(p)
#' 
#' }
aggregateSpatial <- compiler::cmpfun(aggregateSpatial.uncompiled)
