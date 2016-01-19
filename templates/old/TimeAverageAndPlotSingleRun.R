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
### SINGLE RUN PROCESSING SCRIPT
###
### This is a template script to process a single run using the RVCTools v2.0
### It can:
###         1) Time average the run
###         2) Plot the summary variables
###         3) Compare a couple of benchmarks



#########################################################################
################# DEFINE THE RUN INFO HERE!! ############################
#########################################################################

### location of the run you want to process
run.dir <- "/data/forrest/GuessRuns/Dummy/"

### string identifier for the run
### keep it simple with no spaces or special characters since this can be used for file names
run.id <- "v2.4"

### A full text description of the run, could be used for plot titles etc
run.description <- "GUESS v2.4 with N limitation ON"

### String to indicate the climate data used to drive the run
### Again, keep it simple 'cos it can be used for filesnames
run.driving.data <- "CRU"

### A string to signify what outline should be drawn over the plots if you want one 
### Possible values: "modern" - modern coastlines
###                  "hires" - high resolution coastlines and country boundaries
###                  "tortonian" - Tortonian (9.43 Ma) coastlines 
run.map.overlay <- "modern"

### Boolean flag ensure the longitudes are translates to a London-centered (and not Pacific-centered) map
run.london.centre <- TRUE

### Longitude and latitude offset, useful for gridboxes defined by say the South-West corner 
### Like the standard CRU version for example.
run.lonlat.offset <- c(0.25, 0.25)

### calendar offset (to go from GUESS years to calendar years)
### eg for a standard CRU run this is 1401 (= calendar_year_of_first_output - first_model_output_year = 1901 - 500)
run.year.offset = 1401
                       




#########################################################################
################# DEFINE THE PROCESSING PARAMETERS HERE!! ###############
#########################################################################


### save the time averaged output as a file
save = TRUE

### path to save the file, leave as "" if you want to save the file to the run directory
save.dir = ""

### plot the time averaged output
plot = TRUE

### path to save the plots, leave as "" if you want to save the plots to the run directory
plot.dir = ""

### first calendar year for time averaging
first.year <- 1961

### last calendar year for time averaging
last.year <- 1990

### list variables to process 
### set to all in the run directory using 
#variables <- "all"
### or specify as an R list using the following 
variables <- list("lai")


### verbose - give lots of output
verbose <- TRUE

### 





#########################################################################
################# THE PROCESSING CODE ###################################
#########################################################################

### For timing
start.time <- Sys.time()

### if required to process all files, get a list of all the .out files present
if(tolower(variables) == "all"){

  # First get the list of *.out files present
  files.present <- list.files(run.dir, "*.out")

  # Now strip the .out file extension out the get the variable name
  variables <- unlist(lapply(files.present, FUN = trim.var.filename))
    
  # Sometimes there is a file called "*.out", remove it so code doesn't fall over
  variables <- variables[-which(variables == "*")]
  
}


### create a VegRun object to store the data and fill it with the metadata written above
this.run <- openVegRun(run.dir = run.dir,
                        id = run.id,
                        description = run.description,
                        driving.data = run.driving.data,
                        map.overlay = run.map.overlay,
                        london.centre = run.london.centre,
                        lonlat.offset = run.lonlat.offset,
                        year.offset = run.year.offset
                        )
 


print(paste("Okay, time averaging the following variables between", first.year, "and", last.year, "appling an offset of", run.year.offset, sep = " "), quote = FALSE)
print(unlist(variables))

for(variable in variables) {
  
  # open the .out file read it as a data frame
  this.Full <- openLPJOutputFile(run.dir, variable, verbose)
  
  # time average it, save it to disk if requested and finally return the time averaged data frame as a VegVarTA object
  this.VegVarTA <- makeVegVarTA(input.df = this.Full, 
                                run = this.run, 
                                variable = variable, 
                                id = variable, 
                                first.year = first.year, 
                                last.year = last.year, 
                                save = save, 
                                save.dir = save.dir, 
                                plot = plot,
                                plot.dir = plot.dir,
                                verbose = verbose)
  
 
  
  # also could make a time series at this point
  # PLACEHOLDER
  
  # remove the dataframe of the full file to free up memory
  rm(this.Full)
  gc()
  
  # add this variable to the run
  this.run <- addVegVar(this.run, this.VegVarTA)
  
  gc(verbose = verbose)

}

# For timing
end.time <- Sys.time()
time.elapsed <- end.time-start.time
print(time.elapsed)

stop()