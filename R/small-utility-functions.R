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
#' Reads a regular ASCII table using data.table:fread.  Regular means file must have the same number of
#' columns on each line.
#' @param file.string Character string of the full file path.
#' @param verbose If TRUE give extra output
#' Also checks for the file on file systems, fails if not present, and gunzips and zips the file if necessary.
#' Note also some shenangigans for data.table package versions as earlier versions have a security risk.
#' @return A data.table
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

readRegularASCII <- function(file.string, verbose, header = FALSE, awk_str = character(0)) {
  
  #### !!! Check data.table package version (see data.table NEWS file for v1.11.6 point #5)
  compare.string <- utils::compareVersion(a = as.character(utils::packageVersion("data.table")), b = "1.11.6")
  new.data.table.version <- FALSE
  if(compare.string >= 0) new.data.table.version <- TRUE
  
  ### First check is we have a normal or gzipped file and read the header
  gzipped <- FALSE
  re.zip <- FALSE
  
  # Got a non-gzipped file
  if(file.exists(file.string)){ 
    if(verbose) message(paste("Found and opening file", file.string, sep = " "))
    # if(header) this_header <- names(fread(file.string, nrows = 1, header = TRUE))
  }
  
  # Got a gzipped file
  else if(file.exists(paste(file.string, "gz", sep = "."))){
    
    if(verbose) message(paste("File", file.string, "not found, but gzipped file present so using that", sep = " "))
    gzipped <- TRUE
    file.string.gz <- paste(file.string, "gz", sep = ".")
    
    if(.Platform$OS.type == "unix") {
      # if(header) {
      #   if(new.data.table.version) this_header <- names(fread(cmd = paste("gzip -d -c " ,file.string.gz ,  sep = ""), nrows = 1, header = TRUE))
      #   else this_header <- names(fread(paste("gzip -d -c ", file.string.gz, sep = ""), nrows = 1), header = TRUE)
      # }
    }
    # if not unix actually unzip and rezip the file, does this also work on MAcs?
    else {
      re.zip <- TRUE
      R.utils::gunzip(file.string.gz)
      #if(header) this_header <- as.character(fread(file.string, nrows = 1))
    }
  }
  
  # Got neither
  else {
    stop(paste("File (or gzipped file) not found:", file.string))
  }  
  
  
  # ### output header for debugging
  # if(verbose && header) {
  #   message("File header is:")
  #   print(paste("    ", this_header))
  # }
  
  re.zip <- FALSE
  # Non gzipped case
  if(!gzipped){ 
    if(.Platform$OS.type == "unix") {
      if(new.data.table.version) dt <- fread(cmd = paste(awk_str, " < ",file.string, sep = ""))
      else dt <- fread(paste( paste(awk_str, " < ",file.string, sep = "")))
    }
    else {
      dt <- fread(file.string)
    }
  }
  # else gzipped case
  else {
    if(.Platform$OS.type == "unix") {
      print(paste("gzip -d -c " , file.string.gz, " | ", awk_str,  sep = ""))
      if(new.data.table.version) dt <- fread(cmd = paste("gzip -d -c " , file.string.gz, " | ", awk_str,  sep = ""))
      else dt <- fread(paste("gzip -d -c ",  " < ", file.string.gz, " | ", awk_str, sep = ""))
    }
    # if not unix the file have been already unzipped
    else {
      dt <- fread(file.string)
    }
  }
  
  
  # if re-zip
  if(re.zip) R.utils::gzip(file.string)
  
  # Add the header
  print(dt)
  # if(header) setnames(dt, this_header)
  print(dt)
  
  return(dt)
  
}
