#' Select days from a Field
#' 
#' 
#' @param x The Field or data.table from which the days should be selected.  Note that a data.table should have a column called "Day".
#' @param days A numeric vector of the days to be selected
#' 
#' @return A Field or data.table depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

selectDays <- function(x, days){
  
  # To stop compiler NOTES
  Day = NULL
  
  # check input class
  if(is.Field(x)) input.dt <- x@data 
  else if(is.data.table(x))  input.dt <- x
  else stop("Called for an object which is not a Data/Model object or a data.table.  Exiting...")
  
  # Fail if day in not a column
  if(!"Day" %in% getDimInfo(input.dt)) stop("Day column not present!")
  
  # Warning if a certain day is not present
  days.present <- unique(input.dt[["Day"]])
  for(day in days){
    if(!(day %in% days.present)) warning(paste("Day", day, "requested, but it is not in the data!", sep = " "))
  }
  
  # subset the data.table
  output.dt <- subset(input.dt, Day %in% days)
  setKeyDGVM(output.dt)
  
  # and return
  if(is.Field(x)) {
    x@data <- output.dt
    return(x)
  }
  else {
    return(output.dt)
  }
  
  
}
