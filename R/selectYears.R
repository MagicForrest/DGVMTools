#' Select years from a Field
#' 
#' 
#' @param x The Field or data.table from which the years should be selected.  Note that a data.table should have a column called "Year".
#' should have columns "Lon" and "Lat" included.
#' @param first The first year to be selected (numeric)
#' @param last The last year to be selected (numeric)
#' 
#' @return A Field or data.table depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

selectYears <- function(x, first, last){
  
  # To stop compiler NOTES
  Year = NULL
  
  # check input class
  if(is.Field(x)) input.dt <- x@data 
  else if(is.data.table(x))  input.dt <- x
  else stop("Called for an object which is not a Data/Model object or a data.table.  Exiting...")
  
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
  rm(input.dt)
  gc()
  
  # and return
  if(is.Field(x)) {
    x@data <- output.dt
    x@first.year <- first
    x@last.year <- last
    x@id <- makeFieldID(source.info = x@source,
                        var.string = x@quant@id, 
                        first.year= x@first.year, 
                        last.year= x@last.year, 
                        year.aggregate.method = x@year.aggregate.method, 
                        spatial.extent.id = x@spatial.extent.id, 
                        spatial.aggregate.method = x@spatial.aggregate.method, 
                        subannual.aggregate.method = x@subannual.aggregate.method, 
                        subannual.original = x@subannual.original)
    return(x)
  }
  else {
    return(output.dt)
  }
  
  
}
