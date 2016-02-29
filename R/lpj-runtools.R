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

openLPJOutputFile <- function(run,
                              variable,
                              verbose = FALSE,
                              gzip = FALSE){
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.string = file.path(run@run.dir, paste(variable, ".out", sep=""))
  if(file.exists(file.string)){ 
    if(verbose) message(paste("Found and opening file", file.string, sep = " "))
  }
  else if(file.exists(paste(file.string, "gz", sep = "."))){
    if(verbose) message(paste("File", file.string, "not found, but gzipped file present so using that", sep = " "))
    
    
    # BLARP: ugly fix because of ugly bug with readr and large gzipped files.  Maybe this can be removed some day
    # if gzipped file is greater than 100 MB
    if(file.info("/home/forrest/GuessRuns/SPITFIRE/v0.7.0/FireMIP/SF1/anpp.out.gz")$size/(1024*1024)){
      system(paste("gunzip",  paste(file.string, "gz", sep = "."), sep = " "))
      gzip = TRUE
    }
    else {
      file.string <- paste(file.string, "gz", sep = ".") 
    }
  }
  
  else {
    stop(paste("File (or gzipped file) not found:", file.string))
  }
  
  # Use the quite fantastic readr package to read the LPJ-GUESS files which have fixed width formatting
  # Read the column names
  column.names <- unlist(read.table(file.string, nrows = 1, header = FALSE, sep ='', stringsAsFactors = FALSE))
  # Get the widths, and note that we also gotta broaden the widths because the fwf_empty() function reliably detects the end of a field but not the beginning
  widths <- fwf_empty(file.string, skip = 1, col_names = column.names)
  widths$begin[1] <- 0
  for(counter in 2:length(widths$begin)){
    widths$begin[counter] <- widths$end[counter-1]+1
  }
  # Read the file and put it straight into a data.table
  dt <- as.data.table(read_fwf(file.string, widths, skip = 1))
  
  ### OLD WAY!  Slower, also depends on awk, this is definitely deprecated by the 
  ### Read using fread function from data.table but with the white spaces handled correctly using an awk command
  ### at time of writing fread does not handles multiple whitespaces as separators
  ### get the number of columns which is and read the file
  #ncols <- length(names(read.table(file.string, header=TRUE, dec=".",  colClasses="numeric", comment.char="", nrows = 1)))
  #dt <- awkFread(file.string, colNums = c(1:ncols), header=T)
  
  
  # Gzip the file again if that was requested or if the 
  if(gzip){
    if(verbose) message(paste("Gzipping file ", file.string, " again", sep = ""))
    system(paste("gzip",  file.string, sep = " "))
  }
  
  
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



######################### CONVERT AN TEMPORALLY AVERAGED DATA.TABLE TO A SPATIALPIXELSDATAFRAME OBJECT #####################################





############################################################################################################################
######################### MISC FUNCTION FOR HANDLING FILENAMES, VARIABLE LISTS ETC #####################################
############################################################################################################################


listAllOutputFiles <- function(run.directory){
  
  # First get the list of *.out files present
  files.present <- list.files(run.directory, "*.out")
  
  # Now strip the .out file extension out the get the variable name
  this.var.list <- unlist(lapply(files.present, FUN = .trim.var.filename))
  
  # Sometimes there is a file called "*.out", remove it so code doesn't fall over
  if("*" %in% this.var.list) {
    this.var.list <- this.var.list[-which(this.var.list == "*")]
  }
  
  # Sometimes there is a file called "*guess_out.out", remove it so code doesn't fall over
  if("guess_out" %in% this.var.list) {
    this.var.list <- this.var.list[-which(this.var.list == "guess_out")]
  }
  
  # Sometimes there is a file called "*guess_err.out", remove it so code doesn't fall over
  if("guess_err" %in% this.var.list) {
    this.var.list <- this.var.list[-which(this.var.list == "guess_err")]
  }
  
  
  return(this.var.list)
  
}



# handy helper function for trimming file names to get a variable name
.trim.var.filename <- function(var.filename){
  return(substr( var.filename, 1, (nchar(var.filename) - nchar(".out"))))
}


