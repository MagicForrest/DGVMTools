#!/usr/bin/Rscript


###########################################################################
######################### YEARLY AGGREGATION ##############################
###########################################################################


#' Aggregate years
#' 
#' Aggregated all available years in a Field or a data.table object (with years denoted by column "Years"). 
#'
#' @param x A Field or data.table (with a "Year" column)   
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "mode", "median", "sum", "max", "min", "sd", "var" and "cv" (= coefficient of variation: sd/mean).
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateYears.uncompiled <- function(x,
                                      method = "mean",
                                      verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  Year = Lat = Lon = NULL
  
  
  ### SET UP THE AGGREGATE METHOD 
  method <- match.arg(method, c("mean", "mode", "median", "sum", "max", "min", "sd", "var", "cv"))
  method.function <- switch(method,
                            mean = mean,
                            mode = stats_mode,
                            median = stats::median,
                            sum = sum,
                            max = max,
                            min = min,
                            sd = stats::sd,
                            var = stats::var,
                            cv = function(x) {stats::sd(x)/mean(x)})
  
  
  # sort out the input object class
  if(is.Field(x)) {input.dt <- x@data}
  else if(is.data.table(x)) {input.dt <- x}
  
  
  # Do the averaging
  if(verbose) message("Aggregating years ...")
  
  # Check the 'by' dims
  avail.dims <- getDimInfo(x)
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
  if(is.Field(x)) {
    x@data <- output.dt
    x@year.aggregate.method <- method
    if(method == "cv") x@quant@units = "fraction"
    if(method == "var") x@quant@units = paste0("(", x@quant@units, ")^2")
    x@id <- makeFieldID(source = x@source,
                                quant.string = x@quant@id, 
                                sta.info = as(x, "STAInfo"))
   
    return(x)
  }
  else if(is.data.table(x)) {return(output.dt)}
  
}



#' Aggregate years
#' 
#' Aggregated all available years in a Field or a data.table object (with years denoted by column "Years"). 
#'
#' @inheritParams aggregateYears.uncompiled
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A Field or data.table depending on the input object
#' @export
#' @import compiler
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @examples 
#' \donttest{
#'  
#' # Get an example Field
#' test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(name = "LPJ-GUESS", dir = test.dir,  format = GUESS)
#' field <- getField(source = test.Source, quant = "lai")
#' 
#' # calculate of mean of all years
#' mean.allyears <- aggregateYears(x = field, method = "mean", verbose = TRUE)
#' print(mean.allyears@data)
#' print(plotSpatial(mean.allyears))
#' 
#' #  calculate standard deviation of all years
#' sd.allyears <- aggregateYears(x = field, method = "sd", verbose = FALSE)
#' print(sd.allyears@data)
#' print(plotSpatial(sd.allyears))
#' 
#' 
#' }
aggregateYears <- compiler::cmpfun(aggregateYears.uncompiled)