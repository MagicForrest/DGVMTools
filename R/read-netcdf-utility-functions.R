
calculateYearCroppingIndices <- function(target_STAInfo, years_vector) {
  
  # if both target first and last year are defined
  if(length(target_STAInfo@first.year) > 0 & length(target_STAInfo@last.year) > 0) {
    
    # extract the first and last target years 
    target.first.year <- target_STAInfo@first.year
    target.last.year <-  target_STAInfo@last.year
    
    # if first and last years don't already match the beginning and end of the dataset
    if(target.first.year != years_vector[1] | target.last.year != years_vector[length(years_vector)]) {
      
      
      # if first or last year is outisde the dataset then file
      if(target.first.year < years_vector[1]) stop(paste("In DGVMData requested first.year = ", target.first.year, ", but first year in data is", years_vector[1], sep = " "))
      if(target.last.year > years_vector[length(years_vector)]) stop(paste("In DGVMData requested last.year = ", target.last.year, ", but last year in data is", years_vector[length(years_vector)], sep = " "))
      
      # find the first occurence of the target first.year and the last occurance of the target year in the year.vector
      first.index <- match(target.first.year, years_vector)
      last.index <-  match(target.last.year, rev(years_vector))
      last.index <- length(years_vector) - last.index +1
      
      # return these indices
      return(list(first = first.index, last = last.index))
      
    }
    
  }
  
  # otherwise return the full range
  return(list(first = 1, last = length(years_vector)))
  
}