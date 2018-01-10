#' Select years from a Data/ModelObject
#' 
#' 
#' @param x The ModelObject or DataObject, data.table or data.frame from which the years should be selected.  Note that a data.table or data.frame 
#' should have columns "Lon" and "Lat" included.
#' @param temporal.extent The years to be extracted.  Can either be a TemporalExtent or a list of years (as a numeric vector)
#' 
#' @return A ModelObject, DataObject, data.table or data.frame depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

selectYears <- function(x, temporal.extent){
  
  # To stop compiler NOTES
  Year = NULL
  
  # check input class
  if(is.ModelObject(x) || is.DataObject(x)) input.dt <- x@data 
  else if(is.data.table(x))  input.dt <- x
  else stop("Called for an object which is not a Data/Model object or a data.table.  Exiting...")
  
  
  if(class(temporal.extent) == "TemporalExtent") is.TE <- TRUE
  else if(class(temporal.extent) == "numeric") is.TE <- FALSE
  else stop("selectYears called with invalid temporal.extent argument. Exiting... ")
  
  # Warning if a certain year is not present
  years.present <- unique(input.dt[["Year"]])
  if(is.TE) years.needed <- temporal.extent@start:temporal.extent@end
  else years.needed <- temporal.extent
  
  for(year in years.needed){
    if(!(year %in% years.present)) warning(paste("Year", year, "requested, but it is not in the data!", sep = " "))
  }
  
  # subset the data.table
  if(is.TE) output.dt <- subset(input.dt, Year >= temporal.extent@start & Year <= temporal.extent@end)
  else output.dt <- subset(input.dt, Year %in% temporal.extent)   
  
  # and return
  if(is.ModelObject(x) || is.DataObject(x)) {
    x@data <- output.dt
    return(x)
  }
  else {
    return(output.dt)
  }
  
  
}
