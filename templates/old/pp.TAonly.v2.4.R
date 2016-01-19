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

# use the RVCTools v2.3
source("/home/matthew/Science/Tools/RVCTools/v2.4/rvc-tools.R")


###################################################################################
############################# USER SETTINGS #######################################
###################################################################################

# Periods to average
periods = list(pnv = c(start = 1976,end = 2005), 
               sat = c(start = 2000, end = 2010))

# variables to analyse
### set to all in the run directory using 
var.list <- "all"

### or specify as an R list using the following 
#var.list <- list("lai", "mgpp", "firert", "fireseason", "fpc", "cmass", "mnpp")

# Force reaveraging - for example if you have re-downloaded a corrected run
force.reaveraging = TRUE

# give more detail output
verbose <- TRUE



###################################################################################
############################# RUNS TO PROCESS #####################################
###################################################################################

### create a list of VegRun objects that we want to analyse
vegrun.list <- list()

PGF <- openVegRun(run.dir = "/media/matthew/home/DataTemp/GuessRuns/PGF/Standard/",
                  id = "PGF",
                  description= "Standard run, PGF forcing",
                  driving.data = "PGF",
                  map.overlay = "lowres",
                  london.centre = TRUE,
                  lonlat.offset = c(0,0),
                  year.offset = 0
)
vegrun.list[[PGF@id]] <- PGF

CRU <- openVegRun(run.dir = "/media/matthew/home/DataTemp/GuessRuns/CRU_TS_3.00/",
                  id = "CRU",
                  description= "Standard run, CRU forcing",
                  driving.data = "CRU_TS_3.00",
                  map.overlay = "lowres",
                  london.centre = TRUE,
                  lonlat.offset = c(0.25,0.25),
                  year.offset = 1401
)
vegrun.list[[CRU@id]] <- CRU



###################################################################################
###################### AUTOMATIC PREAMBLE AND CHECKS ###############################
###################################################################################


# timing
t1 <- Sys.time()

# Before processing check all directories exist
for(run in vegrun.list){
  if(!file_test("-d", run@run.dir)) stop(paste("No directory:", run@run.dir, sep = " "))
}
message("All run directories present and correct.")

# store the original working to return there at the end
original.workdir <- getwd() 



###################################################################################
############## THE MAIN LOOP FOR ALL RUNS TO BE PROCESSES #########################
###################################################################################


### for each run
for(run in vegrun.list){
  if(verbose) message(paste(" ------ Processing run", run@id, "------", sep = " "))
  
  
  # go to the run directory
  setwd(run@run.dir)
  
  ### if required to process all files, get a list of all the .out files present
  if(tolower(var.list) == "all"){ this.var.list <- listAllOutputFiles(run@run.dir)   }
  else{this.var.list <- var.list}  
  
  
  ### for each variable
  for(var in this.var.list){
    if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
    
    # this stores the full .out file but only if we need to do it
    # we store it so we don't need to read it more than once
    this.full <- NULL
        
    
    #####################################################
    ########### TIME AVERAGING ##########################
    #####################################################
        
    ########## Time average over all time periods, write files and make plots
    for(period in periods){
      
      # look for the correct time averaged files and if there read it in
      TA.filename <- paste(run@run.dir, "/", var, ".TA.", period[["start"]], "-", period[["end"]], ".Rtable", sep ="")
      if(file.exists(paste(TA.filename)) & !force.reaveraging){
        if(verbose) message(paste("File",  TA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))
        this.TA.dt <- openTAFile(run, var, period)
      } 
      
      #if not there, open the full .out file read it as a data table and time average it
      else {
        if(verbose) message(paste("File",  TA.filename, "not found in directory",  run@run.dir, "so reading and averaging.", sep = " "))
        # if already opened read the full .out file as a data table
        if (is.null(this.full)) this.full <- openLPJOutputFile(run@run.dir, var, verbose = TRUE)
        # do time averaging and save the results to disk
        this.TA.dt <- doTA(this.full, run, period, save = TRUE, save.dir = run@run.dir, verbose = verbose)
      }
      
       
      # clean up temporal averages to save memory
      rm(this.TA.dt)
      gc()
       
    } # for each period
      
    # clean up to save memory   
    rm(this.full)
    gc()
      
    message("Done.")
    
  } # for each variable 
  
  
  # return to original working directory
  setwd(original.workdir)
  
  
} # For each run

# terminate with a happy message
message("Finished processing all runs gracefully :-)")


# timing
t2 <- Sys.time()
print(t2-t1)