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
  avail.dims <- getDimInfo(input.obj)
  if(!"Year" %in% avail.dims) stop("Aggregation of years requested, but year column not present.  Failing.")
  by.dims <- avail.dims[-which(avail.dims == "Year")]
  
  # and actually do it
  output.dt <- input.dt[, lapply(.SD, method.function), by=by.dims]
  output.dt[, Year := NULL]
  
  # try another way - THIS IS SLOWER!!
  if(FALSE) {
    
    t1 <- Sys.time()
    all.cols <- names(input.dt)
    # build the command
    command <- "list("
    for(col in all.cols) {
      if(col != "Year") command <- paste0(command, "mean", "(", col, "),")
    }
    command <- substr(command,1,nchar(command)-1) 
    command <- paste0(command, ")")
    print(command)
    
    these.cols <- all.cols[-which(all.cols == "Year")]
    
    input.dt[, Year := as.numeric(Year)]
    t15 <- Sys.time()
    print(t15 - t1)
    input.dt[, (these.cols) := eval(parse(text = command)), by=by.dims]
    print(input.dt)
    t2 <- Sys.time()
    print(t2 -t1)
    
    if(verbose) message("...done.")
    setKeyDGVM(input.dt)
    setKeyDGVM(output.dt)
    print(identical(input.dt, output.dt))
    
  }
  
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyDGVM(output.dt)
  
  # sort out the input object class
  if(is.Field(input.obj)) {
    input.obj@data <- output.dt
    input.obj@year.aggregate.method <- method
    input.obj@id <- makeFieldID(source = input.obj@source,
                                var.string = input.obj@quant@id, 
                                sta.info = as(input.obj, "STAInfo"))
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}



#' Aggregate years
#' 
#' Aggregated all available years in a Field or a data.table object (with years denoted by column "Years"). 
#'
#' @inheritParams aggregateYears.uncompiled
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field or data.table depending on the input object
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateYears <- compiler::cmpfun(aggregateYears.uncompiled)