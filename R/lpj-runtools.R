#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
###################################### 

############################################################################################################################
############################ FUNCTIONS TO HANDLE LPJ-GUESS FILES ###########################################################
############################################################################################################################



######################### OPEN AN LPJ-GUESS *.out FILE  #####################################################################
#' Open an LPJ-GUESS .out file
#'
#' \code{openLPJOutputFile} returns a data.table object given a string defining a vegetation quantity 
#' from the run (eg. "lai", to read the file "lai.out") and  \code{VegRun} object which defines where the run is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param run A \code{VegRun} containing the meta-data about the LPJ-GUESS run
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @export
openLPJOutputFile <- function(run,
                              variable,
                              verbose = FALSE){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = NULL
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.string = file.path(run@run.dir, paste(variable, ".out", sep=""))
  if(file.exists(file.string)){ 
    if(verbose) message(paste("Found and opening file", file.string, sep = " "))
    dt <- fread(file.string)
  }
  else if(file.exists(paste(file.string, "gz", sep = "."))){
    if(verbose) message(paste("File", file.string, "not found, but gzipped file present so using that", sep = " "))
    dt <- fread(paste("zcat < ", paste(file.string, "gz", sep = "."), sep = ""))
    
    #     # BLARP: ugly fix because of ugly bug with readr and large gzipped files.  Maybe this can be removed some day
    #     # if gzipped file is greater than 100 MB
    #     if(file.info(file.string)$size/(1024*1024)){
    #       system(paste("gunzip -c",  paste(file.string, "gz", sep = "."), ">", file.string, sep = " "))
    #       gzip = TRUE
    #     }
    #     else {
    #       file.string <- paste(file.string, "gz", sep = ".") 
    #     }
    
  }
  else {
    stop(paste("File (or gzipped file) not found:", file.string))
  }
  
  #   # OLD WAY - deprecated by new fread which can handle whitspace.  Notice also the ugly BLARP above
  #   # Use the quite fantastic readr package to read the LPJ-GUESS files which have fixed width formatting
  #   # Read the column names
  #   column.names <- unlist(read.table(file.string, nrows = 1, header = FALSE, sep ='', stringsAsFactors = FALSE))
  #   # Get the widths, and note that we also gotta broaden the widths because the fwf_empty() function reliably detects the end of a field but not the beginning
  #   widths <- fwf_empty(file.string, skip = 1, col_names = column.names)
  #   widths$begin[1] <- 0
  #   for(counter in 2:length(widths$begin)){
  #     widths$begin[counter] <- widths$end[counter-1]+1
  #   }
  #   # Read the file and put it straight into a data.table
  #   dt <- as.data.table(read_fwf(file.string, widths, skip = 1))
  #   
  ### OLD, OLD WAY!  Slower, also depends on awk, this is definitely deprecated by the 
  ### Read using fread function from data.table but with the white spaces handled correctly using an awk command
  ### at time of writing fread does not handles multiple whitespaces as separators
  ### get the number of columns which is and read the file
  #ncols <- length(names(read.table(file.string, header=TRUE, dec=".",  colClasses="numeric", comment.char="", nrows = 1)))
  #dt <- awkFread(file.string, colNums = c(1:ncols), header=T)
  
  #   
  #   # Gzip the file again if that was requested - ignore
  #   if(gzip){
  #     if(verbose) message(paste("Gzipping file ", file.string, " again", sep = ""))
  #     system(paste("gzip",  file.string, sep = " "))
  #   }
  
  
  #  Print messages
  if(verbose) {
    message("Read table. It has header:")
    print(names(dt))
    message("It has shape:")
    print(dim(dt))      
  }
  
  
  # Correct year, lons and lats
  if(verbose)message("Correcting years, lons and lats...")
  if(run@year.offset != 0) dt[,Year := Year + run@year.offset]
  if(length(run@lonlat.offset) == 2 ){
    if(run@lonlat.offset[1] != 0) dt[, Lon := Lon + run@lonlat.offset[1]]
    if(run@lonlat.offset[2] != 0) dt[, Lat := Lat + run@lonlat.offset[2]]
  }
  
  else if(length(run@lonlat.offset) == 1 ){
    if(run@lonlat.offset[1] != 0) dt[, Lon := Lon + run@lonlat.offset[1]]
    if(run@lonlat.offset[1] != 0) dt[, Lat := Lat + run@lonlat.offset[1]]
  }
  
  
  if(verbose)message("Corrected.")
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(run@london.centre){ dt[, Lon := vapply(dt[,Lon], 1, FUN = .LondonCentre)] }
  
  # set some attributes about the file - works!
  attr(dt, "shadeToleranceCombined") <- FALSE
  
  # set keys
  setkey(dt, Lon, Lat, Year)
  
  # remove any NAs
  dt <- na.omit(dt)
  
  return(dt)
  
}


######################### RETURN A DATA.TABLE CONTAINING THE FULL DATA FROM ONE LPJ-GUESS OUTPUT VARIABLE #####################################################################
#
#' Returns the data from one LPJ-GUESS output variable as a \code{data.table}.   
#'
#' \code{getVegQuantity_LPJ} returns a \code{data.table} containing the full data (not averaged spatially or temporally) data from an LPJ-GUESS
#' output variable.  Normally it will read the file from disk, but if that has already been done, and the \code{data.table} has been saved to the 
#' \code{VegRun} object, it will return that to save time.
#' 
#' @param A \code{VegRun} containing the meta-data about the LPJ-GUESS run from which the data is to be read.  Most importantly it must contain the run.dara nd the offsets.
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param store.internally A logical defining whether to attach the resulting to the \code{data.table} to the \code{VegRun} object for later use
#' @param A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @export

getVegQuantity_LPJ <- function(run, var.string, store.internally = FALSE,verbose = FALSE) {
  
   
  # USE THE FULL FILE IF ALREADY STORED IN MEMORY
  if(var.string %in% names(run@full)){
    if(verbose) message(paste(var.string, ".out is already read, so using that internal copy.", sep = ""))
    this.dt <- run@full[[var.string]]
    setkey(this.dt, Lon, Lat, Year)
  }
  
  # READ THE FULL FILE
  else {
    if(verbose) message(paste(var.string, ".out not already read, so using reading it now.", sep = ""))
    
    this.dt <- openLPJOutputFile(run, var.string, verbose = TRUE)
    
    if(verbose) {
      message("Head of full .out file (after offsets):")
      print(head(this.dt))
    }
    
    # if requested save the full data.table containing the entire ,out file to the run object
    if(store.internally) {run <<- addToVegRun(this.dt, run, id = var.string)}
    
  }
  
  return(this.dt)
  
}



######################### LIST ALL LPJ-GUESS OUTPUT VARIABLES (STORED AS *.out FILES) IN AN RUN DIRECTORY  #####################################################################
#' List all LPJ-GUESS *.out files in a run directory
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "guess.out" and "*.out" 
#' 
#' @param run.directory A path to a directory on the file system containing some .out files
#' @return A list of all the .out files present, with the ".out" removed. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

listAllLPJOutput <- function(run.directory){
  
  # First get the list of *.out files present
  files.present <- list.files(run.directory, "*.out")
  
  # Now strip the .out file extension out the get the variable name
  this.var.list <- unlist(lapply(files.present, FUN = trimLPJFilename))
  
  # get rid of stupid ones
  ignore.list <- c("*", "guess_out", "guess_err")
  for(ignore.string in ignore.list){
    if(ignore.string %in% this.var.list) {
      this.var.list <- this.var.list[-which(this.var.list == ignore.string)]
    }
  }
  
  return(this.var.list)
  
}




######################### TRIM AN LPJ-GUESS FILENAME  #####################################################################
#' Helper function to trim the ".out" from an LPJ-GUESS filename
#' 
#' Very simple, just removes the last four characters. 
#' No error checking to see if the last four characters are really ".out"!
#'
#' @param var.filename The string of the filename to be trimmed
#' @return A string less the last four characters.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @internal


# handy helper function for trimming file names to get a variable name
trimLPJFilename <- function(var.filename){
  return(substr( var.filename, 1, (nchar(var.filename) - nchar(".out"))))
}


