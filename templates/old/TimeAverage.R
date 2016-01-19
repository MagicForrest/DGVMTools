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

# use the RVCTools v2.2
source("/home/forrest/Tools/RVCTools/v2.3/rvc-tools.R")


###################################################################################
############################# USER SETTINGS #######################################
###################################################################################

# Years to average and offset
first.year <- 1961
last.year <- 1990
year.offset <- 0


# variables to analyse
### set to all in the run directory using 
#var.list <- "all"
### or specify as an R list using the following 
var.list <- list("firert")

verbose <- TRUE



###################################################################################
############################# RUNS TO PROCESS #######################################
###################################################################################

### create a list of VegRun objects that we want to analyse
vegrun.list <- list("/data/forrest/GuessRuns/DailyAllocation/Standard/OFF/")


###################################################################################
###################### AUTOMATIC PREAMBLE AND CHECKS ###############################
###################################################################################


# timing
t1 <- Sys.time()

# Before processing check all directories exist
for(run.dir in vegrun.list){
  if(!file_test("-d", run.dir)) stop(paste("No directory:", run.dir, sep = " "))
}
message("All run directories present and correct.")

# store the original working to return there at the end
original.workdir <- getwd() 










###################################################################################
############## THE MAIN LOOP FOR ALL RUNS TO BE PROCESSES #########################
###################################################################################


### for each run
for(run.dir in vegrun.list){
  
  
  ### if required to process all files, get a list of all the .out files present
  
  if(tolower(var.list) == "all"){
    
    # First get the list of *.out files present
    files.present <- list.files(run.dir, "*.out")
    
    # Now strip the .out file extension out the get the variable name
    this.var.list <- unlist(lapply(files.present, FUN = trim.var.filename))
   
    # Sometimes there is a file called "*.out", remove it so code doesn't fall over
    if("*" %in% this.var.list) {
      this.var.list <- this.var.list[-which(temp.var.list == "*")]
    }
  }
  else{
    this.var.list <- var.list
  }
  
  
  # for each variable
  for(var in this.var.list){
    
    # print something helpful and reassuring
    if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
       
    # go to the run directory
    setwd(run.dir)
    
    # open the .out file read it as a data.table
    this.full <- openLPJOutputFile(run.dir, var, year.offset, verbose)
    
    # time average
    this.TA <- doTimeAverage.cmpd(this.full, first.year, last.year,  verbose)
    
    # space average
    this.SA <- doSpaceAverage(this.full, verbose)
    
    print(this.SA)
        
    ##### Save the nicely averaged table if selected 
    if(verbose) message("Saving as a table...")
       write.table(this.TA, file = paste(run.dir, "/", var, ".avg.", first.year, "-", last.year, ".Rtable", sep =""), quote = FALSE, row.names = FALSE)
    if(verbose) message("Saved table.")
    
    # clean up
    rm(this.full)
    rm(this.TA)
    rm(this.SA)
    gc()
       

    
  }
  
  # return to original working directory
  setwd(original.workdir)
  
  
}



# timing
t2 <- Sys.time()
print(t2-t1)



# terminate with a happy message
message("Processing finished gracefully :-)")

