#!/usr/bin/Rscript


#############################################################################
######################### SUBANNUAL AGGREGATION #############################
#############################################################################

#' Sub-annual aggregation
#' 
#' Aggregates data with sub-annual time resolution to a coarser time resolution.  For example, going from monthly to annual. 
#'
#' @param x data.table or Field 
#' @param method A character string describing the method by which to aggregate the data.  Can currently be "mean", "mode", "median", "sum", "max", "min", "sd", "var" and "cv" (= coefficient of variation sd/mean).
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param target A character string defining the subannual period to which the data should be aggregate. Can be "Month", "Season" or "Year" (also "Annual" is valid). 
#' Default is year.  
#' @param verbose If TRUE give some progress update about the averaging.
#' 
#' Input data can be a Field or data.table with appropriate columns.
#' 
#' @return A Field or data.table depending on the input object
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
aggregateSubannual.uncompiled <- function(x,
                                          method = "mean",
                                          target = "Year",
                                          verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Season = Month = Day = Lat = Lon = Weight = NULL
  
  # standardise arguments
  if(tolower(target) == "annual"  || tolower(target) == "yearly") target = "Year"
  
  
  # Function for weighting months by their number of days
  addMonthlyWeights <- function(x){
    
    days.in.month <- c()
    for(month in all.months) {
      days.in.month <- append(days.in.month, month@days)
    }
    
    return(days.in.month[x])
    
  }
  
  # Function for weighting months by their number of days
  addSeasonalWeights <- function(x){
    
    days.in.season <- c()
    for(season in all.seasons) {
      days.in.season <- append(days.in.season, season@days)
    }
    
    return(days.in.season[x])
    
  }
  
  # Function for weighting months by their number of days
  monthFromDay <- function(x){
    
    
    
    days.in.season <- c()
    for(season in all.seasons) {
      days.in.season <- append(days.in.season, season@days)
    }
    
    return(days.in.season[x])
    
  }
  
  
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
  
  ### Get the spatial-temporal dimensions present and determine the initial subannual resolution
  avail.dims <- getDimInfo(x)
  if("Month" %in% avail.dims) initial.subannual <- "Month"
  else if("Day" %in% avail.dims) initial.subannual <- "Day"
  else if("Season" %in% avail.dims) initial.subannual <- "Season"
  else initial.subannual <- "Year"
  
  ### Check the 'by' dims
  by.dims <- c()
  for(possible.dim in list("Lon","Lat","Year")){
    if(possible.dim %in% avail.dims) by.dims <- append(by.dims, possible.dim)
  }
  
  
  
  ###### DO THE AGGREGATION
  
  ### AGGREGATION TO ANNUAL
  if(tolower(target) == "annual"  || tolower(target) == "year"  || tolower(target) == "yearly"){
    
    # FROM DAILY
    if("Day" %in% avail.dims) {
      if(verbose) message("Sub-annual aggregation from daily to annual")
      output.dt <- input.dt[,lapply(.SD, method.function), by=by.dims]
      output.dt[,Day:=NULL]
    }
    
    # FROM MONTHLY
    else if("Month" %in% avail.dims) {
      
      if(verbose) message("Sub-annual aggregation from monthly to annual")
      
      # if not doing mean, simply apply the required function
      if(!identical(method.function, mean)){
        output.dt <- input.dt[,lapply(.SD, method.function), by=by.dims]
        output.dt[,Month:=NULL]
      }
      # else use the weighted mean, weighted by the days in the month
      else{
        output.dt <- copy(input.dt)[, Weight := lapply(.SD, addMonthlyWeights), .SDcols = c("Month")]
        output.dt <- output.dt[,lapply(.SD, stats::weighted.mean, w = Weight), by=by.dims]
        output.dt[,Month:=NULL]
        output.dt[,Weight:=NULL]
      }
      
    }
    
    # FROM SEASONAL
    else if("Season" %in% avail.dims) {
      
      if(verbose) message("Sub-annual aggregation from seasonal to annual")
      
      # if not doing mean, simply apply the required function
      if(!identical(method.function, mean)){
        output.dt <- copy(input.dt)[,lapply(.SD, method.function), by=by.dims]
        output.dt[,Season:=NULL]
      }
      # else use the weighted mean by days in the season
      else {
        
        #  calculate weights
        output.dt <- copy(input.dt)[, Weight := lapply(.SD, addSeasonalWeights), .SDcols = c("Season")]
        output.dt <- copy(output.dt)[,lapply(.SD, stats::weighted.mean, w = Weight), by=by.dims]
        output.dt[,Season:=NULL]
        output.dt[,Weight:=NULL]
        
      }
      
    }
    
    else if("Year" %in% avail.dims) {
      warning("Aggregation to Year requested but data already are yearly so no averaging done and returning original data!")
      return(x)
    }
    
    else{
      warning("Subannual aggregation requested but no subannual dimensions present, it looks like this has already been aggregated!\n Just returning the original data.")
      return(x)
    }
  }
  
  ### AGGREGATION TO SEASONAL
  else if(tolower(target) == "season"){
    
    # FROM DAILY
    if("Day" %in% avail.dims) {
      # TODO Daily to seasonal aggregation
      stop("Daily to seasonal aggregration not yet implemented")
    }
    
    # FROM MONTHLY
    else if("Month" %in% avail.dims) {
      
      if(verbose) message("Sub-annual aggregation from monthly to seasonal")
      
      # set the season 
      # this function is a bit dirty, but okay for now
      monthToSeason <- function(x){
        month.to.season <- c("DJF","DJF","MAM","MAM","MAM","JJA","JJA","JJA","SON","SON","SON","DJF")
        return(month.to.season[x])
      }
      output.dt <- copy(input.dt)[, Season := lapply(.SD, monthToSeason), .SDcols = c("Month")]
      by.dims <- append(by.dims, "Season")
      
      
      # if not doing mean, simply apply the required function
      if(!identical(method.function, mean)){
        
        output.dt <- output.dt[, Month:=NULL]
        output.dt <- output.dt[,lapply(.SD, method.function), by=by.dims]
        
      }
      
      # else use the weighted mean, weighted by the days in the month
      else{
        
        # calculate the weights
        output.dt <- output.dt[, Weight := lapply(.SD, addMonthlyWeights), .SDcols = c("Month")]
        output.dt <- output.dt[, Month:=NULL]
        
        # do weighted mean
        output.dt <- output.dt[,lapply(.SD, stats::weighted.mean, w = Weight), by=by.dims]
        output.dt[,Weight:=NULL]
        
      }
      
    }
    
    # IF ALREADY SEASONAL
    else if("Season" %in% avail.dims) {warning("Aggregation to Season requested but data already are already Seasonal, so no averging done and returning original data!")
      return(x)}
    
    # ELSE FAIL
    else{
      stop("Subannual aggregation to Season requested but sub-seasonal time dimension not present.  Exiting...")
    }
  }
  
  ### AGGREGATE TO MONTHLY
  else if(tolower(target) == "month"){
    
    # FROM DAILY
    if("Day" %in% avail.dims) { 
      
      # make look up vector for days -> months
      days.to.months <- c()
      for(month in all.months) {
        days.to.months <- append(days.to.months, rep(month@index, month@days))
      }
      
      # add months column 
      output.dt <- copy(input.dt)[, Month := days.to.months[Day]]
      output.dt[,Day:=NULL]
      by.dims <- append(by.dims, "Month")
      output.dt <- output.dt[,lapply(.SD, method.function), by=by.dims]

    }
    else if("Month" %in% avail.dims) {warning("Aggregation to monthly requested but data already are already monthly, so no averging done and returning original data!")
      return(x)}
    else{
      stop("Subannual aggregation to Month requested but sub-monthly time dimension not present.  Exiting...")
    }
    
  }
  
  # Else fail
  else{
    stop(paste("Unknown target for sub-annual aggregation \"", target, "\" found. Exiting..."))
  }
  
  if(verbose) message("...done.")
  
  # Set keys and return the averaged table
  setKeyDGVM(output.dt)
  
  if(target == "month") target <- "Month"
  
  # sort out the input object class
  if(is.Field(x)) {
    x@data <- output.dt
    x@subannual.aggregate.method <- method
    if(method == "cv") x@quant@units = "fraction"
    if(method == "var") x@quant@units = paste0("(", x@quant@units, ")^2")
    x@subannual.original <- initial.subannual
    x@subannual.resolution  <- target
    x@id <- makeFieldID(source = x@source,
                                quant.string = x@quant@id, 
                                sta.info = as(x, "STAInfo"))
    return(x)
  }
  else if(is.data.table(x)) {return(output.dt)}
  
}



#' Sub-annual aggregation
#' 
#' Aggregates data with sub-annual time resolution to a coarser time resolution.  For example, going from monthly to annual. 
#'
#' @inheritParams aggregateSubannual.uncompiled
#' @return A Field or data.table depending on the input object
#' 
#' @details Input data can be a Field or data.table with appropriate columns.
#' 
#' @import compiler
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
#' @examples 
#' \donttest{
#'  
#' # Get an example Field
#' test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(name = "LPJ-GUESS", dir = test.dir,  format = GUESS)
#' field <- getField(source = test.Source, quant = "mlai", year.aggregate.method = "mean")
#' 
#' # calculate of meteorological seasons (DJF, MAM, JJA, SON)
#' seasonal.mean <- aggregateSubannual(x = field, method = "mean", 
#'                                     target = "Season", verbose = TRUE)
#' print(seasonal.mean@data)
#' print(plotSpatial(seasonal.mean))
#' 
#' #  calculate annual mean
#' annual.mean <- aggregateSubannual(x = field, method = "mean", verbose = TRUE)
#' print(annual.mean@data) 
#' print(plotSpatial(annual.mean))
#' 
#' #  calculate annual standard deviation
#' annual.sd <- aggregateSubannual(x = field, method = "sd", verbose = TRUE)
#' print(annual.sd@data) 
#' print(plotSpatial(annual.sd))
#' 
#' }
aggregateSubannual <- compiler::cmpfun(aggregateSubannual.uncompiled)