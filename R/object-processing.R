#!/usr/bin/Rscript

###############################################################################################################################
######################### FUNCTIONS FOR SPATIAL AND TEMPORAL AVERAGING DATA.TABLES  ###########################################
###############################################################################################################################


######################### TIME AVERAGE A DATA.TABLE OR DGVMTOOLS OBJECT  ##############################
#
#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") or a data.table object
#'
#' @param input.obj data.table, ModelObject or DataObject  
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
averageTemporal.uncompiled <- function(input.obj,
                                       method = "mean",
                                       verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  
  ### SET UP THE AGGREGATE METHOD 
  method <- match.arg(method, c("mean", "sum", "max", "min", "sd", "var"))
  method.function <- switch(method,
                            mean = mean,
                            sum = sum,
                            max = max,
                            min = min,
                            sd = sd,
                            var = var)
  
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {input.dt <- input.obj@data}
  else if(is.data.table(input.obj)) {input.dt <- input.obj}
  
  
  
  # Do the averaging
  if(verbose) message("Temporally averaging ...")
  if("Lon" %in% names(input.dt))  output.dt <- input.dt[,lapply(.SD, method.function), by=list(Lon, Lat)]
  else output.dt <- input.dt[,lapply(.SD, method)]
  if(verbose) message("...done.")
  
  # remove the Year 'cos it dun' make so much sense no mo'
  output.dt[,Year:=NULL]
  
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyDGVM(output.dt)
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {
    input.obj@data <- output.dt
    input.obj@temporal.aggregate.method <- method
    input.obj@id <- makeModelObjectID(input.obj@quant@id, temporal.extent = input.obj@temporal.extent, spatial.extent = input.obj@spatial.extent, temporal.aggregate.method = input.obj@temporal.aggregate.method, spatial.aggregate.method = input.obj@spatial.aggregate.method)
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}



#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") of a data.table object.  This function does not select the years, that should be done first.
#'
#' @param input.dt data.table  
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords export
#' @import  compiler cmpfun
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
averageTemporal <- compiler::cmpfun(averageTemporal.uncompiled)


######################### SPATIALLY AVERAGE ##############################
#
#' Spatially average a ModelObject, DataObject or data.table 
#' 
#' Spatially average all gridcells of a ModelObject, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param A ModelObject, DataObject or data.table to be averaged  
#' @param mean
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
averageSpatial.uncompiled <- function(input.obj,
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
                            sd = sd,
                            var = var)
 
  if(method == "weighted.mean") method = "w.mean"
  if(method == "weighted.sum") method = "w.sum"
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {input.dt <- input.obj@data}
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
    # check to see if Year is still a clomun name (it might already have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, method.function), by=list(Year)]
    else output.dt <- input.dt[,lapply(.SD, method.function)]
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
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {
    input.obj@data <- output.dt
    input.obj@spatial.aggregate.method <- method
    input.obj@id <- makeModelObjectID(input.obj@quant@id, temporal.extent = input.obj@temporal.extent, spatial.extent = input.obj@spatial.extent, temporal.aggregate.method = input.obj@temporal.aggregate.method, spatial.aggregate.method = input.obj@spatial.aggregate.method)
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}

######################### SPATIALLY AVERAGE ##############################
#
#' Spatially average a ModelObject, DataObject or data.table 
#' 
#' Spatially average all gridcells of a ModelObject, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param input.obj A ModelObject, DataObject or data.table to be averaged 
#' @param area.weighted If TRUE area-weight the gridcells
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import  compiler cmpfun
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
averageSpatial <- compiler::cmpfun(averageSpatial.uncompiled)
