#!/usr/bin/Rscript


###########################################################################
######################### YEARLY AGGREGATION ##############################
###########################################################################


#' Aggregate years
#' 
#' Aggregated all available years in a Field or a data.table object (with years denoted by column "Years"). 
#'
#' @param input.obj A Field or data.table (with a "Year" column)   
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateYears.uncompiled <- function(input.obj,
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
  if(is.Field(input.obj)) {input.dt <- input.obj@data}
  else if(is.data.table(input.obj)) {input.dt <- input.obj}
  
  
  # Do the averaging
  if(verbose) message("Aggregating years ...")
  
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
  if(is.Field(input.obj)) {
    input.obj@data <- output.dt
    input.obj@year.aggregate.method <- method
    input.obj@id <- makeFieldID(source.info = input.obj@source,
                                var.string = input.obj@quant@id, 
                                first.year= input.obj@first.year, 
                                last.year= input.obj@last.year, 
                                year.aggregate.method = input.obj@year.aggregate.method, 
                                spatial.extent.id = input.obj@spatial.extent.id, 
                                spatial.aggregate.method = input.obj@spatial.aggregate.method, 
                                subannual.aggregate.method = input.obj@subannual.aggregate.method, 
                                subannual.original = input.obj@subannual.original)
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}



#' Aggregate years
#' 
#' Aggregated all available years in a Field or a data.table object (with years denoted by column "Years"). 
#'
#' @param input.obj A Field or data.table (with a "Year" column)   
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field or data.table depending on the input object
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateYears <- compiler::cmpfun(aggregateYears.uncompiled)