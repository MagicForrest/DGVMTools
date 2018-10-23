#' Select seasons from a Field
#' 
#' 
#' @param x The Field or data.table from which the seasons should be selected.  Note that a data.table should have a column called "Season".
#' @param seasons A character vector of the seasons to be selected, ie some of "MAM", "JJA", "SOM" and "DDF" (or potentionally a custom defined season)
#' 
#' @return A Field or data.table depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

selectSeasons <- function(x, seasons){
  
  # To stop compiler NOTES
  Season = NULL
  
  # check input class
  if(is.Field(x)) input.dt <- x@data 
  else if(is.data.table(x))  input.dt <- x
  else stop("Called for an object which is not a Data/Model object or a data.table.  Exiting...")
  
  # Fail if Season is not a column
  if(!"Season" %in% getDimInfo(input.dt)) stop("Season column not present!")
  
  # Warning if a certain season is not present
  seasons.present <- unique(input.dt[["Season"]])
  for(season in seasons){
    if(!(season %in% seasons.present)) warning(paste("Season", season, "requested, but it is not in the data!", sep = " "))
  }
  
  # subset the data.table
  output.dt <- subset(input.dt, Season %in% seasons)
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
