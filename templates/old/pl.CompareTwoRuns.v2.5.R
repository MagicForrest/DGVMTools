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

# use the RVCTools v2.5
source("~/Tools/RVCTools/v2.5/rvc-tools.R")
source("~/Tools/RVCTools/v2.5/projects/rvc-turkey.R")


###################################################################################
############################# USER SETTINGS #######################################
###################################################################################

# Periods to average
#periods = list(pnv = c(start = 1951,end = 1975),
#               sat = c(start = 2000,end = 2010))

#periods = list(sat = c(start = 2000,end = 2010))
periods = list(pnv = c(start = 1951,end = 1975))

# Force reaveraging - for example if you have re-downloaded a corrected run
force.reaveraging = FALSE

# variables to analyse
var.list <- c("anpp")

layout.objs <- NULL

# now map the list of layout objects to overlay with sp.layout, this will be used for coastlines/country borders
#layout.objs <- list()
#layout.objs[["coastlines"]] <- makeOverlay("hires")

verbose = TRUE



###################################################################################
############################# RUNS TO PROCESS #####################################
###################################################################################

### create a list of VegRun objects that we want to analyse
vegrun.list <- list()


############ MAIN RUNS v2.1 ##############


# PNV
vegrun.list[["PNV-Standard-2014-10-13-20Patches"]] <- new("VegRun",
                                                          id = "PNV-Standard-2014-10-13",
                                                          run.dir = "/data/forrest/GuessRuns/Turkey/v2.1/PNV/Standard-2014-10-13-20patches",
                                                          description= "Potential Natural Vegetation",
                                                          driving.data = "TurkeyRegional",
                                                          map.overlay = "lowres"
)


# Bug fixed
# vegrun.list[["InterceptionFix"]] <- new("VegRun",
#                                 id = "InterceptionFix",
#                                 run.dir = "/data/forrest/GuessRuns/Turkey/v2.1/PNV/InterceptionFix",
#                                 description= "Potential Natural Vegetation",
#                                 driving.data = "TurkeyRegional",
#                                 map.overlay = "lowres"
# )

vegrun.list[["BothFix"]] <- new("VegRun",
                                id = "BothFix",
                                run.dir = "/data/forrest/GuessRuns/Turkey/v2.1/PNV/BothFix",
                                description= "Potential Natural Vegetation",
                                driving.data = "TurkeyRegional",
                                map.overlay = "lowres"
)

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


### for each variable
for(var in var.list){
  if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
  
  print(var)
  
  # get the VegQuant corresponding to this variable to provide some metadata
  this.Quant <- getVegQuantity(var)  
  
  for(period.name in names(periods)){
    period <- periods[[period.name]]
    
    list.of.runs <- list()
    
    
    ### for each run
    for(run in vegrun.list){
      if(verbose) message(paste(" ------ Processing run", run@id, "------", sep = " "))  
      
      # go to the run directory
      setwd(run@run.dir)
      
      
      
      # this stores the full .out file but only if we need to read it
      # we store it so we don't need to read it more than once
      this.full <- NULL
      
      
      ####################################################################################################
      ################### TEMPORAL AVERAGING AND SPATIAL ANALYSIS/PLOTTING ###############################
      ########## Time average over all time periods, write files and make plots ##########################
      ####################################################################################################
      
      ##### GET TIME-AVERAGED DATA #####
      
      # look for the correct time averaged files and if there read it in
      TA.filename <- paste(run@run.dir, "/", var, ".TA.", period[["start"]], "-", period[["end"]], ".Rtable", sep ="")
      if(file.exists(paste(TA.filename)) & !force.reaveraging){
        if(verbose) {message(paste("File",  TA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))}
        this.TA.dt <- fread(TA.filename, header = TRUE, stringsAsFactors=FALSE)
        this.TA.dt <- na.omit(this.TA.dt)
      } 
      #if not there, open the full .out file read it as a data table, time average it and save to disk
      else {
        if(verbose) message(paste("File",  TA.filename, "not found in directory",  run@run.dir, "so reading and averaging.", sep = " "))
        if (is.null(this.full)) this.full <- openLPJOutputFile(run, var, verbose = TRUE)
        this.TA.dt <- doTimeAverage.cmpd(this.full, period, verbose)
        if(verbose) message("Saving as a table...")
        write.table(this.TA.dt, file = TA.filename, quote = FALSE, row.names = FALSE)
      }
      
      # save the variable for comparison between runs
      list.of.runs[[run@id]] <- promoteToRaster(this.TA.dt) 
      
      
      
    } # for each vegrun
  } # for each period 
  
  # make a raster of difference
  if(verbose) message("Calculating differences")
  difference <- list.of.runs[["BothFix"]] - list.of.runs[["PNV-Standard-2014-10-13"]]
  difference <- aggregate(difference, 4)
  
  # plot difference for each PFT
  if(verbose) message("Plotting difference maps")
  plotLPJMaps(data = difference,
              quant = lpj.quantities[["anpp.diff"]], 
              period = period, 
              doSummary = TRUE, 
              doIndividual = TRUE, 
              run = NULL, 
              plot.dir = "/home/forrest/Projects/Turkey/Plots/AET_and_Interception_BugFix", 
              file.name = NULL, 
              summary.file.name = paste(lpj.quantities[["anpp.diff"]]@id, "Summary.aggregated.png", sep ="."),
              summary.title = "Bug fix aNPP effect: Fixed - Bugged" ,
              special.string = "Fixed-Bugged.aggregated",
              layout.objs = NULL, 
              plot.labels =  NULL,
              useLongnames = FALSE,
              maxpixels = 20000000)
  
  stop()
  
  
} # for each variable we are comparing


# terminate with a happy message
message("Processing finished gracefully :-)")
t2 <- Sys.time()
print(t2-t1)


