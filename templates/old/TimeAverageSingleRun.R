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

source("~/Tools/RVCTools/v2.0/lpj-tools.R")

#########################################################################
################# USER PARAMETER STUFF!! ################################
#########################################################################

### location of the run you want to average
run.dir <- "/data/forrest/GuessRuns/Dummy/"

### save the time averaged output as a file
save = TRUE

### path to save the file, leave as "" if you want to save the the run directory
save.dir = ""

### offset (to go from GUESS years to calendar years)
### eg for a standard CRU run this is 1401 (= calendar_year_of_first_output - first_model_output year = 1901 - 500)
calendar.offset = 1401

### first calendar year 
first.year <- 1961

### last calendar year
last.year <- 1990

### list variables to process 
### set to all in the run directory using 
variables <- "all"
### or specify as an R list using the following 
variables <- list("firert", "cmass", "lai", "bioclim")

### verbose - give lots of output
verbose <- TRUE





#########################################################################
################# THE PROCESSING CODE ###################################
#########################################################################

# handy function for trimming file names
trim.var.filename <- function(var.filename){
  return(substr( var.filename, 1, (nchar(var.filename) - nchar(".out"))))
}



# if to process all files
if(variables == "all"){

    # First get the list of *.out files present
  files.present <- list.files(run.dir, "*.out")

  # Now strip the .out file extension out the get the variable name
  variables <- unlist(lapply(files.present, FUN = trim.var.filename))

  
}
                  
 
print(paste("Okay, time averaging the following variables between", first.year, "and", last.year, "appling an offset of", calendar.offset, sep = " "), quote = FALSE)


for(variable in variables) {
  
  doTimeAverage(run.dir, variable, first.year, last.year, calendar.offset, save, save.dir, verbose)
  
  gc(verbose = verbose)

}


stop()