#!/usr/bin/Rscript

########### HANDY PROCESSING FUNCTIONS ########### 

#' Safe division
#' 
#' Function to divide two numbers but return 0 if the denominator is 0
#' 
#' @param x numerator
#' @param y denominator
#' 
#' Handy little thing.
#' @keywords internal
#' 
#' 
"%/0%" <- function(x,y) ifelse(y==0,0,base::"/"(x,y))


#' Statistical mode
#' 
#' Calculate the statistical mode with is strangely missing from base R.  In a tie it returns the first of the most common values
#' encountered in the original vector.
#' 
#' @param x vector from which to find the most common value
#' 
#' @keywords internal
#' @return The mode, ie the most common value. In case of ties the return is first in the original vector
#' 
stats_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}


######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table based in the spatial (Lon, Lat) and temporal (Year, Month, Day), present. 
#' 
#' Keys should be set on all data.table object for sorts, joins, DGVMTool-defined operators etc.  
#' This function should be called on a data.table stored in a Field after it has been created,
#' including if it was created by averaging another data.table because it seems as keys are not conserved.
#'
#' @param dt The data.table for which to set the key
#' @return Returns nothing because changes the original data.table by reference (this is the data.table way)
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

setKeyDGVM <- function(dt){
  
  keys.present <- getDimInfo(dt)
  
  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  else warning("No spatial/temporal columns present in this data.table to set as keys!")
  
}



#' London centre a gridcell
#' 
#' Transform a vector of longitude values to lie in the range (-180,180) instead of (0,36)
#'
#' @param lon A vector longitude value to transform 
#' @return The transformed longitude values
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
LondonCentre <- function(lon) {
  
  return(ifelse(lon > 180, lon - 360, lon))
  
}





#' check if a given year is a leap year
#' 
#' @param year year (integer or vector)
#' @param proleptic use leap years even before 1582.
#' @param doy return days of year instead logical.
#' @return logical or integer, if doy is TRUE
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @keywords internal
#' @export
is.leapyear <- function(year, proleptic=FALSE, doy=FALSE) {
  leap <- sapply(year, function(x) {
    if (!proleptic && x < 1582) return(FALSE)
    if (((x %% 4 == 0) & (x %% 100 != 0)) | (x %% 400 == 0))
      return(TRUE)
    return(FALSE)
  })
  if (doy)
    return(ifelse(leap, 366, 365))
  return(leap)
}


#' Check if two Quantity objects are equivalent
#' 
#' Performs a simple check on the units and types of two quantities
#' @param quant1 First Quantity to compare
#' @param quant2 Second Quantity to compare
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @keywords internal

equivalentQuantities <- function(quant1, quant2) {
  if (quant1@units==quant2@units)  return(TRUE)
  else return(FALSE)
}

#' Read ASCII table
#' 
#' Reads a regular ASCII table using data.table:fread.  Regular means file must have the same number of columns on each line.
#' @param file.string Character string of the full file path.
#' @param verbose If TRUE give extra output
#' @param layers A character string (or a vector of character strings) specifying which optional columns to be read .  NULL (default) means read all.
#' @param always.read A character string (or a vector of character strings) specifying which columns should always be read (ie "Lon", "Lat", etc) 
#' Can directly read gzipped files (.gz) is they are present.
#' @return A data.table
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

readRegularASCII <- function(file.string, verbose, layers = NULL, always.read = NULL) {
  
  
  # check for file or gzipped file
  if(file.exists(file.string))  { if(verbose) message(paste("Found and opening file", file.string, sep = " ")) }
  else if(file.exists(paste(file.string, "gz", sep = "."))) file.string <- paste(file.string, "gz", sep = ".")
  else stop(paste("File (or gzipped file) not found:", file.string))
  
  # if specific layers requested then read only those layers
  if(!is.null(layers)) {
    
    # get the header of the file - this is potentially a bottleneck as it seems to decompress and the the whole file just to get
    # the head but I don't see a way around it right now
    header <- names(fread(file.string, nrows = 0, header = TRUE))
    
    # Find columns which are always to be read (ie. Lon, Lat, Year, etc for LPJ-GUESS) and store them a vector
    standard.cols.to.read <- header[which(header %in% always.read)] 
    
    # Now loop over the request layers, check they are present and add them to list of columns to read
    # if they are not give a warning but proceed
    selected.cols.to.read <- c()
    for(this.layer in layers) {
      if(this.layer %in% header) selected.cols.to.read <- append(selected.cols.to.read, this.layer)
      else {
        message(paste0("Layer '", this.layer, "' requested but not found in data."))
        warning(paste0("Layer '", this.layer, "' requested but not found in data."))
      }
    }
    
    if(length(selected.cols.to.read) == 0){
      message("Not reading any layers as none of your requested layers were in the file, is this really what you wanted to do?  Subsequent code will likely fail.")
      warning("Not reading any layers as none of your requested layers were in the file, is this really what you wanted to do?  Subsequent code will likely fail.")
    }
    
    dt <- fread(file.string, select = append(standard.cols.to.read, selected.cols.to.read))
    
  }
  #else read 'em all
  else {
    dt <- fread(file.string)
  }
  
  return(dt)
 
}
