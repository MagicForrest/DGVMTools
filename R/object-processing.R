#!/usr/bin/Rscript

###############################################################################################################################
######################### FUNCTIONS FOR SPATIAL AND TEMPORAL AVERAGING DATA.TABLES  ###########################################
###############################################################################################################################


######################### TIME AVERAGE A DATA.TABLE  ##############################
#
#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") or a data.table object
#'
#' @param input.dt data.table  
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doTemporalAverage.uncompiled <- function(input.dt,
                                         verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # Do the averaging
  if(verbose) message("Temporally averaging ...")
  if("Lon" %in% names(input.dt))  output.dt <- input.dt[,lapply(.SD, mean), by=list(Lon, Lat)]
  else output.dt <- input.dt[,lapply(.SD, mean)]
  if(verbose) message("...done.")
  
  # remove the Year 'cos it dun' make so much sense no mo'
  output.dt[,Year:=NULL]
  
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyDGVM(output.dt)
  return(output.dt)
  
}



#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") of a data.table object.  This function does not select the years, that should be done first.
#'
#' @param input.dt data.table  
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords export
#' @import cmpfun compiler
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doTemporalAverage <- cmpfun(doTemporalAverage.uncompiled)




.selectYears <- function(input, temporal.extent){
  
  # To stop compiler NOTES
  Year = NULL
  
  # Warning if a certain year is not present
  years.present <- unique(input[["Year"]])
  for(year in temporal.extent@start:temporal.extent@end){
    if(!(year %in% years.present)) warning(paste("Year", year, "requested, but it is not in the data!", sep = " "))
  }
  
  # return the subsetted data.table
  return(subset(input, Year >= temporal.extent@start & Year <= temporal.extent@end))    
  
}


######################### SPATIALLY AVERAGE ##############################
#
#' Spatially average a ModelObject, DataObject or data.table 
#' 
#' Spatially average all gridcells of a ModelObject, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param A ModelObject, DataObject or data.table to be averaged  
#' @param area.weighted If TRUE area-weight the gridcells
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doSpatialAverage.uncompiled <- function(input.obj,
                                        verbose = FALSE,
                                        area.weighted=TRUE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {input.dt <- input.obj@data}
  else if(is.data.table(input.obj)) {input.dt <- input.obj}
  
  
  
  # Do the averaging
  if (area.weighted) {
    if (!any(colnames(input.dt)=="area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=verbose)
    }
    if(verbose) message(paste("Spatially averaging (area weighted) whole domain...", sep = ""))
    # check to see if Year is still a clomun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area), by=list(Year)]
    else {
      output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area)]
    }
    
    output.dt[,area:=NULL]
  } else {
    if(verbose) message(paste("Spatially averaging (not area weighted) whole domain...", sep = ""))
    # check to see if Year is still a clomun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, mean), by=list(Year)]
    else output.dt <- input.dt[,lapply(.SD, mean)]
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
    input.obj@is.spatially.averaged <- TRUE
    input.obj@id <- makeModelObjectID(input.obj@quant@id, temporal.extent = input.obj@temporal.extent, spatial.extent = input.obj@spatial.extent, temporally.averaged = input.obj@is.temporally.averaged, spatially.averaged = TRUE)
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
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doSpatialAverage <- cmpfun(doSpatialAverage.uncompiled)
