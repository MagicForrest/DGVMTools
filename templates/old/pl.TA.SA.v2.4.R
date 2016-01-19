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

# use the RVCTools v2.4
source("~/Tools/RVCTools/v2.4/rvc-tools.R")


###################################################################################
############################# USER SETTINGS #######################################
###################################################################################

# Periods to average
periods = list(pnv = c(start = 1999,end = 2100), 
               sat = c(start = 2099, end = 2199))

# Force reaveraging - for example if you have re-downloaded a corrected run
force.reaveraging = FALSE

# variables to analyse
### set to all in the run directory using 
var.list <- "all"
### or specify as an R list using the following 
#var.list <- list("lai", "mgpp", "firert", "fireseason", "fpc", "cmass", "mnpp")
var.list <- list("lai", "fpc")

# for which per-PFT variables to make vegetation summaries
#veg.summaries <- "all"
veg.summaries <- c("lai", "fpc")


# give more detail output
verbose <- TRUE

# specific analyses
doGlobalBiomes <- TRUE



###################################################################################
############################# RUNS TO PROCESS #####################################
###################################################################################

### create a list of VegRun objects that we want to analyse
vegrun.list <- list()

T21 <- openVegRun(run.dir = "/data/forrest/GuessRuns/Coupling/T21/First",
                  id = "T21",
                  description= "First online EMAC-GUESS run",
                  driving.data = "EMAC_Online",
                  map.overlay = "lowres",
                  london.centre = TRUE,
                  lonlat.offset = c(0.0,0.0),
                  year.offset = 0
)
vegrun.list[[T21@id]] <- T21



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

# if doGlobalBiomes 
if(doGlobalBiomes){
  
  # we will need to read the LAI data fure sure
  if(!"lai" %in% var.list){ var.list <- append(var.list, "lai") }
  
}

###################################################################################
############## THE MAIN LOOP FOR ALL RUNS TO BE PROCESSES #########################
###################################################################################


### for each run
for(run in vegrun.list){
  if(verbose) message(paste(" ------ Processing run", run@id, "------", sep = " "))  
  
  # go to the run directory
  setwd(run@run.dir)
  
  ### if required to process all files, get a list of all the .out files present
  this.var.list <- NULL
  if(tolower(var.list) == "all"){ this.var.list <- listAllOutputFiles(run@run.dir)   }
  else{this.var.list <- var.list}  
  
  
  ### for each variable
  for(var in this.var.list){
    if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
    
    # this stores the full .out file but only if we need to read it
    # we store it so we don't need to read it more than once
    this.full <- NULL
    
    
    ####################################################################################################
    ################### TEMPORAL AVERAGING AND SPATIAL ANALYSIS/PLOTTING ###############################
    ########## Time average over all time periods, write files and make plots ##########################
    ####################################################################################################
     
    for(period in periods){
      
      ##### GET TIME-AVERAGED DATA #####
      
      # look for the correct time averaged files and if there read it in
      TA.filename <- paste(run@run.dir, "/", var, ".TA.", period[["start"]], "-", period[["end"]], ".Rtable", sep ="")
      if(file.exists(paste(TA.filename)) & !force.reaveraging){
        if(verbose) {message(paste("File",  TA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))}
        this.TA.dt <- fread(TA.filename, header = TRUE, stringsAsFactors=FALSE)
      } 
      #if not there, open the full .out file read it as a data table, time average it and save to disk
      else {
        if(verbose) message(paste("File",  TA.filename, "not found in directory",  run@run.dir, "so reading and averaging.", sep = " "))
        if (is.null(this.full)) this.full <- openLPJOutputFile(run, var, verbose = TRUE)
        this.TA.dt <- doTimeAverage.cmpd(this.full, period, verbose)
        if(verbose) message("Saving as a table...")
        write.table(this.TA.dt, file = TA.filename, quote = FALSE, row.names = FALSE)
      }
      
      
      ##### PLOT TIME AVERAGED DATA #####
      
      # get the VegQuant corresponding to this variable to provide some metadata
      this.Quant <- getVegQuantity(var)
      
      
      ### FOR PER-PFT VARIABLES
      if(this.Quant@type == "PFT"){  
        
        # plot the per PFT summary and individual PFT plots
        plotPFTMaps(this.TA.dt, this.Quant, period = period, doSummary = TRUE, doIndividual = TRUE, useLongnames = TRUE)
        
        # make life-form summaries and plots
        if(veg.summaries == "all" || var %in% veg.summaries && FALSE){
          message(paste("Making lifeform/phenology/etc summaries for ", var, sep = ""))
          addAllVegTotals(this.TA.dt, global.PFTs)
          stop("UNDER DEVELOPEMENT")
        }
      }
        
        
      ### SPECIFIC ANALYSES
      if(doGlobalBiomes & var == "lai"){
        
          addGlobalBiomes(this.TA.dt, global.PFTs)
          plotGlobalBiomeMap(this.TA.dt, period = period)
           
      }
      
      
       
      
      
      # clean up temporal averages to save memory
      rm(this.TA.dt)
      gc()
      
    } # for each period
    
    
    ####################################################################################################
    ################### SPATIAL AVERAGING AND TEMPORAL ANALYSIS/PLOTTING ###############################
    ####################################################################################################
    
    ##### GET SPATIALLY-AVERAGED DATA #####
    
    # look for the correct time averaged files and if there read it in
    SA.filename <- paste(run@run.dir, "/", var, ".SA.Rtable", sep ="")
    if(file.exists(paste(SA.filename)) & !force.reaveraging){
      if(verbose) {message(paste("File",  SA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))}
      this.SA.dt <- fread(SA.filename, header = TRUE, stringsAsFactors=FALSE)
    } 
    #if not there, open the full .out file read it as a data.table, time average it and save it to disk
    else {
      if(verbose) message(paste("File",  SA.filename, "not found in directory",  run@run.dir, "so reading and averaging.", sep = " "))
      if (is.null(this.full)) this.full <- openLPJOutputFile(run, var, verbose = TRUE)
      this.SA.dt <- doSpaceAverage.cmpd(this.full, verbose)
      if(verbose) message("Saving as a table...")
      write.table(this.SA.dt, file = SA.filename, quote = FALSE, row.names = FALSE)
    }
    
    # clean up to save memory   
    rm(this.SA.dt)
    rm(this.full)
    gc
    
    message("Done.")
    
  } # for each variable 
  
  
  # return to original working directory
  setwd(original.workdir)
  
  
} # For each run


# terminate with a happy message
message("Processing finished gracefully :-)")
t2 <- Sys.time()
print(t2-t1)


