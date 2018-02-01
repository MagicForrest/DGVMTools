########### HANDY PROCESSING FUNCTIONS ########### 

#' Safe division
#' 
#' Function to divide two number but return 0 if the denominator is 0
#' 
#' @param x numerator
#' @param y denominator
#' 
#' Handy little thing.
#' @export
#' 
#' 
"%/0%" <- function(x,y) ifelse(y==0,0,base::"/"(x,y))

#!/usr/bin/Rscript


######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table based in the spatial (Lon, Lat) and temporal (Year, Month, Day), present. 
#' 
#' Keys should be set on all data.table object for sorts, joins, DGVMTool-defined operators etc.  
#'  This function should be called on a data.table stored in a Field after it has been created,
#'  including if it was created by avergaing another data.table because it seems as keys are not conserved.
#'
#' @param dt The data.table for which to set the key
#' @return Returns nothing because changes the original data.table by reference (this is the data.table way)
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

setKeyDGVM <- function(dt){
  
  keys.present <- getSTInfo(dt)
  
  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  
}



#' London centre a gridcell
#' 
#' Transform a vector of longitude values to lie in the range (-180,180) instead of (0,36)
#'
#' @param lon A vector longitude value to transform 
#' @return The transformed longitude values
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
LondonCentre <- function(lon) {
  
  return(ifelse(lon > 180, lon - 360, lon))
  
}