#!/usr/bin/Rscript


#############################################################################
######################### TEMPORAL AGGREGATION ##############################
#############################################################################

#
#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") or a data.table object
#'
#' @param input.obj data.table, Field or DataObject  
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field, DataObject or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateTemporal.uncompiled <- function(input.obj,
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
                            sd = stats::sd,
                            var = stats::var)
  
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.Field(input.obj)) {input.dt <- input.obj@data}
  else if(is.data.table(input.obj)) {input.dt <- input.obj}
  
  
  
  # Do the averaging
  if(verbose) message("Temporally averaging ...")
  
  # Check the 'by' dims
  by.dims <- c()
  avail.dims <- getSTInfo(input.obj)
  for(possible.dim in list("Lon","Lat","Season","Month","Day")){
    if(possible.dim %in% avail.dims) by.dims <- append(by.dims, possible.dim)
  }
  
  output.dt <- input.dt[,lapply(.SD, method.function), by=by.dims]
  if(verbose) message("...done.")
  
  # remove the Year 'cos it dun' make so much sense no mo'
  output.dt[,Year:=NULL]
  
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyDGVM(output.dt)
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.Field(input.obj)) {
    input.obj@data <- output.dt
    input.obj@temporal.aggregate.method <- method
    input.obj@id <- makeFieldID(input.obj@quant@id, temporal.extent = input.obj@temporal.extent.id, spatial.extent = input.obj@spatial.extent.id, temporal.aggregate.method = input.obj@temporal.aggregate.method, spatial.aggregate.method = input.obj@spatial.aggregate.method)
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}



#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") or a data.table object
#'
#' @param input.obj data.table, Field or DataObject  
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field, DataObject or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateTemporal <- compiler::cmpfun(aggregateTemporal.uncompiled)