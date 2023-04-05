#!/usr/bin/Rscript

#' Average Fields
#' 
#' Average Fields provided to the function as a list; useful for averaging model ensembles, replicate simulations etc.  
#' Note that since you can supply the function, you don't actually need to average, you can also do, for example, \code{max}, \code{min}, \code{sd}, etc.  
#' Fails with an error message if the supplied Fields don't have the same layers.
#' 
#' @param list.of.fields The Fields that you want to average, stored in a list.
#' @param source Either a Source object which provides the metadata about the newly created data (representing, say, the ensemble mean) or \code{NULL}/missing, 
#' in which case a Source will be created using the metadata from the input Fields.
#' @param method The function which you want to use.  Is \code{mean} by default, but it can be anything that operates on a vector of numbers.  
#' Useful options might be \code{max},  \code{min} and \code{sd}.  
#'  
#'@details  
#' The function currently does no checks that the spatial extent, yearly period and Quantity are consistent between runs.  
#' The resulting Field simple takes these things from the first object from list.of.fields.  
#' It is therefore up to the user not do anything silly and average quantites, or spatial or temporal extents 
#' that don't makes sense.
#' 
#' It is also only defined for numeric layers (no checks currently done), but could be definied for 
#' categorical ones if there was demand.
#'          
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @return A Field
##' @examples 
#' \donttest{
#'  
#' # Get two example Fields (which will contain the same data but different meta-data)
#' test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source1 <- defineSource(name = "LPJ-GUESS run 1", dir = test.dir,  format = GUESS)
#' field1 <- getField(source = test.Source1, quant = "lai", year.aggregate.method = "mean")
#' test.Source2 <- defineSource(name = "LPJ-GUESS run 2", dir = test.dir,  format = GUESS)
#' field2 <- getField(source = test.Source2, quant = "lai", year.aggregate.method = "mean")
#'
#' # Average these two fields, specifically take the mean
#' field.mean <- averageFields(list(field1, field2))
#' print(field.mean)
#' print(field.mean@source)
#' 
#' # Plot them all together (they should be the same)
#' print(plotSpatial(list(field1, field2, field.mean)))
#' 
#' # Plot just the Total layer for clarity and set out panels vertically in three rows
#' p <- plotSpatial(list(field1, field2, field.mean), layers = "Total")
#' p <- p + facet_wrap(~ Facet, nrow = 3)
#' print(p)
#' 
#' # Repeat the procedure, but this time define a nicer Source to make nicer metadata
#' 
#' test.Source.averaged <- defineSource(name = "Ensemble Mean", dir = test.dir,  format = GUESS)
#' field.mean.2 <- averageFields(list(field1, field2), source = test.Source.averaged)
#' print(field.mean.2)
#' print(plotSpatial(list(field.mean.2)))
#' 
#' # also make the standard deviation (should be zero since the runs are identical)
#' field.sd <- averageFields(list(field1, field2), method = sd)
#' print(plotSpatial(field.sd))
#' 
#' }
averageFields <- function(list.of.fields, method = mean, source = NULL) {
  
  if(!missing(method)) method.str <- as.character(as.list(match.call())$method)
  else method.str <- "mean"   

  #  MF TODO: check that the Fields we are averaging have the same spatial and temporal properties 
  # if they don't, drop an error message
  
  # make lists of what is to be processed, and check that the columns are the same (if not fail!)
  list.of.dts <- list()
  for(object in list.of.fields){
    list.of.dts[[paste(object@source@id, object@id, sep = ".")]] <- object@data
  }
  for(this.dt in list.of.dts) {
    if(!identical(names(this.dt), names(list.of.dts[[1]]))) stop("You are trying to average model.objects with different layers, this won't work!")
  }
  
  # do the averaging to make the new data.table and tidy up
  full.dt <- rbindlist(l=list.of.dts)
  setKeyDGVM(full.dt)
  by.list <- getDimInfo(list.of.dts[[1]])
  output.dt <- full.dt[,lapply(.SD, method), keyby=by.list]
  rm(full.dt, list.of.dts)
  gc()
  
  # make a new source if necessary
  if(is.null(source) | missing(source)) {
    source <- list.of.fields[[1]]@source
    source@id <- method.str
    source@name <- paste0(method.str, "(")
    for(this.field in list.of.fields) {
      source@id <- paste(source@id, this.field@source@id, sep = "_")
      source@name <- paste0(source@name, this.field@source@name, ", ")
    }
    source@name <- substr(source@name, 1, nchar(source@name)-2) 
    source@name <- paste0(source@name, ")")
  }
  
  # make and return a new Field 
  return(new("Field",
             id = makeFieldID(source = source, quant.string = list.of.fields[[1]]@quant@id, sta.info = as(list.of.fields[[1]], "STAInfo")),
             data = output.dt,
             quant = list.of.fields[[1]]@quant,
             source = source))
  
}