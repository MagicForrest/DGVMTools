#!/usr/bin/Rscript

#' Average Fields provided as a list
#' 
#' Useful for averaging model ensembles, replicate simulations etc.  
#' Note that since you can supply the function, you don't actually need to average, you can also do, for example, \code{max}, \code{min}, \code{sd}, etc.  
#' Fails with an error message if the supplied Fields don't have the same fields
#' 
#' @param list.of.fields The Fields that you want to average, stored in a list.
#' @param run Either a Source object which provides the metadata about the newly created "run" (representing, say, the ensemble mean) or \code{NULL}, 
#' in which case the function just returns a data.table with the average values.
#' @param method The function which you want to use.  Is \code{mean} by default, but it can be anything that operates on a vector of numbers.  
#' Useful options might be \code{max},  \code{min} and \code{sd}.  
#'  
#'@details  
#' The function currently does no checks that the spatial extent, yearly period and Quantity are consistent between runs.  The resulting Field simple takes these things
#' from the first object from list.of.vegruns.  It is therefore up to the user not do anything silly and average quantites, or spatial or temporal extents that don't makes sense.
#'          
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @return Either a Field (if a Source has been supplied to provide the necessary meta-data), or a data.table (if no meta-data has been supplies in the form of a Source)
#' 
averageFields <- function(list.of.fields, run = NULL, method = mean) {
  
  
  # make lists of what is to be processed, and check that the columns are the same (if not fail!)
  list.of.dts <- list()
  for(object in list.of.fields){
    list.of.dts[[paste(object@source@id, object@id, sep = ".")]] <- object@data
  }
  for(this.dt in list.of.dts) {
    if(!identical(names(this.dt), names(list.of.dts[[1]]))) stop("You are trying to average model.objects with different layers, this won't work!")
  }
  
  
  # check if Lon, Lat and Year are present and so should be averaged over
  by.list <- c()
  if("Lat" %in% names(list.of.dts[[1]])) by.list <- append(by.list, "Lat")
  if("Lon" %in% names(list.of.dts[[1]])) by.list <- append(by.list, "Lon")
  if("Year" %in% names(list.of.dts[[1]])) by.list <- append(by.list, "Year")
  
  
  # do the averaging to make the new data.table and tidy up
  full.dt <- rbindlist(l=list.of.dts)
  setKeyDGVM(full.dt)
  output.dt <- full.dt[,lapply(.SD, method), keyby=by.list]
  rm(full.dt, list.of.dts)
  gc()
  
  
  # if no run is supplied then cannot construct a full Field, therefore just return the data.table
  if(is.null(run)) {
    return(output.dt)
  }
  
  # else, if a run was provided, make a Field and return that
  else {
    
    #  MF TODO: check that the Fields we just averaged have the same spatial and temporal properties 
    # if they don't, drop an error message
    
    
    # construct a new Field for the average data that we have just calculated based on the first on in the list
    # but put in the new run and data
    new.Field <- list.of.fields[[1]]
    new.Field@data <- output.dt
    new.Field@source <- run
    
    
    return(new.Field)
  }
  
}