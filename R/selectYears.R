#' Select years from a Field
#' 
#' 
#' @param x The Field or data.table from which the years should be selected.  Note that a data.table should have a column called "Year".
#' should have columns "Lon" and "Lat" included.
#' @param first The first year to be selected (numeric)
#' @param last The last year to be selected (numeric)
#' 
#' @return A Field or data.table depending on the type of the input x.
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

selectYears <- function(x, first, last){
  
  # To stop compiler NOTES
  Year = NULL
  
  # check input class
  if(is.Field(x)) input.dt <- x@data 
  else if(is.data.table(x))  input.dt <- x
  else stop("Called for an object which is not a Field or a data.table.  Exiting...")
  
  # 
  if(first > last) stop("Error, first year must be smaller or equal to the last year!")
  
  
  # Warning if a certain year is not present
  years.present <- unique(input.dt[["Year"]])
  years.needed <- first:last

  
  for(year in years.needed){
    if(!(year %in% years.present)) warning(paste("Year", year, "requested, but it is not in the data!", sep = " "))
  }
  
  # subset the data.table
  output.dt <- subset(input.dt, Year >= first & Year <= last)
  setKeyDGVM(output.dt)
  rm(input.dt)
  gc()
  
  # and return
  if(is.Field(x)) {
    x@data <- output.dt
    x@first.year <- first
    x@last.year <- last
    x@id <- makeFieldID(source = x@source,
                        var.string = x@quant@id, 
                        sta.info = as(x, "STAInfo"))
    return(x)
  }
  else {
    return(output.dt)
  }
  
  
}
