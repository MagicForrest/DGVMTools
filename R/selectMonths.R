#' Select months from a Field
#' 
#' 
#' @param x The Field or data.table from which the days should be selected.  Note that a data.table should have a column called "Month".
#' @param months A numeric vector of the months to be selected (Can also be abbreviations, "Jan", "Feb" etc... )
#' 
#' @return A Field or data.table depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

selectMonths <- function(x, months){
  
  # To stop compiler NOTES
  Month = NULL
  
  # check input class
  if(is.Field(x)) input.dt <- x@data 
  else if(is.data.table(x))  input.dt <- x
  else stop("Called for an object which is not a Data/Model object or a data.table.  Exiting...")
  
  # Fail if Month is not a column
  if(!"Month" %in% getDimInfo(input.dt)) stop("Month column not present!")
  
  # Substitute month strings for month numbers
  old.months <- months
  months <- c()
  for(month in old.months) {
    if(is.numeric(month)) {months <- append(months, month)}
    else if(is.character(month)) {
      found <- FALSE
      for(month.meta in all.months) {
        if(month.meta@abbreviation == month){
          months <- append(months, month.meta@index)
          found <- TRUE
        } 
      }
      if(!found) warning(paste0("Can't select month ", month, ".  Please use numbers between 1 and 12 or the first three letters of the month (with a capital), eg 'Jan', 'feb', etc..."))
    }
    else {
      warning(paste0("Can't select month ", month, ".  Please use numbers between 1 and 12 or the first three letters of the month (with a capital), eg 'Jan', 'feb', etc..."))
    }
  }
  
  
  # Warning if a certain day is not present
  months.present <- unique(input.dt[["Month"]])
  for(month in months){
    if(!(month %in% months.present)) warning(paste("Month", month, "requested, but it is not in the data!", sep = " "))
  }
  
  # subset the data.table
  output.dt <- subset(input.dt, Month %in% months)
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
