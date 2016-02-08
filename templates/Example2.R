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

################################################################################################################
#############  MF 2016-02-08 : MAJOR TODOS
#############   1. Benchmarking.   Compare many runs to a benchmark simultaneously and add GFED4 including times series
#############      to include in the standard RVCTools distribution.
#############   2. Time series.  Producing is simple enough, but need to take plotting code from Joerg.
#############   3. Compare one run to a 'baseline' run.
#############   4. Sort out the warnings.
################################################################################################################



################################################################################################################
####### PREAMBLE: Load the packages, define the number of threads to use and start the timer
#######


# Load RVCTools package (clear the environment and unload the package first in case we are actively developing the package)
rm(list = ls())
if("package:RVCTools" %in% search()) detach(name = "package:RVCTools", unload = TRUE)
library(RVCTools)

# Load extra packages to allow multi-threading, and also set the number of threads 
library(foreach)
library(doParallel)
registerDoParallel(1) # number of threads - leave at one for single threaded

# Start the timer
t1 <- Sys.time()



################################################################################################################
####### SETTINGS: Here define the 'analysis' - meaning which variables and datsets to plot, 
#######           the directory and filenames, etc... 
#######


### Analysis label (for today's date use "analysis.label <- Sys.Date()")
analysis.label <- "Example2"                    

### Plot directory for run comparison plots
plot.dir <- "/home/forrest/Temp/Example2"   

### Time spans and spatials extents over which to average t
periods <- list(PNV = new("TimeSpan", name = "Reference", start = 1961, end = 1990)) 
extents <- standard.continental.extents

### Universal resolution for all runs in this analysis (set to NULL in the unlikely event that not all runs use the same resolution) 
universal.resolution <- "HD"

### Force re-averaging - in case the raw data have been updated, or we required new a new time period
forceReAveraging <- FALSE

### Make individual plots for each PFT/month/Carbon pool etc. (can also be turned on for individual variables use the detailed.var.list below)
doIndividual <- FALSE

### Which variables to analyse and in what detail (use "all" to choose all *.out files in the run directory)
var.list <- c("lai") # simple summary analysis
detailed.var.list <- NULL #c("mwcont_upper", "lai") # for which variable to plot in more detail (make individual plots, lifeform plots for PFTs, seasonal plots for monthly variable etc)
fraction.pft.var.list <- NULL #c("lai") # for which PFT variables to plot fractions
do.dominant.pft.var.list <- NULL # c("lai") # for which PFT variables to calculate dominant PFT for


### prepare a list of benchmarking datasets
benchmarking.datasets.list <- prepareBenchmarkingDatasets(list(#"Smith2014",
                                                               "Saatchi2011"))

### Verbose for extra output
verbose <- TRUE




################################################################################################################
####### RUNS TO PROCESS: Here define the list of runs to process and analyse
#######

vegrun.list <- list()

vegrun.list[["LPJ-GUESS-SPITFIRE-Run1"]] <- defineVegRun(run.dir = "/data/forrest/GuessRuns/SPITFIRE/v0.7.0/CRUNCEP_OFM_PB-0.2_CB-0.05_HIP-1.0_NoShrubs_res.mult.5",
                                                         id = "LPJ-GUESS-SPITFIRE-Run1",
                                                         description= "LPJ-GUESS-SPITFIRE Example Run 1",
                                                         pft.set = global.PFTs,
                                                         model = "LPJ-GUESS-SPITFIRE",
                                                         driving.data = "CRUNCEP",
                                                         map.overlay = "",
                                                         london.centre = TRUE,
                                                         lonlat.offset = c(0.25,0.25),
                                                         landuseSimulated = TRUE,
                                                         year.offset = 0,
                                                         line.col = "darkviolet",
                                                         line.width = 1,
                                                         line.type = 1
)

vegrun.list[["LPJ-GUESS-SPITFIRE-Run2"]] <- defineVegRun(run.dir = "/data/forrest/GuessRuns/SPITFIRE/v0.7.0/CRUNCEP_NFM_PB-1_CB-0_HIP-3.0_NoShrubs_res.mult.5",
                                                         id = "LPJ-GUESS-SPITFIRE-Run2",
                                                         description= "LPJ-GUESS-SPITFIRE Example Run 2",
                                                         pft.set = global.PFTs,
                                                         model = "LPJ-GUESS-SPITFIRE",
                                                         driving.data = "CRUNCEP",
                                                         map.overlay = "",
                                                         london.centre = TRUE,
                                                         lonlat.offset = c(0.25,0.25),
                                                         landuseSimulated = TRUE,
                                                         year.offset = 0,
                                                         line.col = "darkgreen",
                                                         line.width = 1,
                                                         line.type = 1
)



###################################################################################
###### THE MAIN LOOP FOR ALL RUNS FO
###### 

### for each run
for(run in vegrun.list){
  if(verbose) message(paste(" +++++++ Processing run", run@id, "+++++++", sep = " "))  
  
  
  ### if required to process all files, get a list of all the .out files present
  if(length(var.list) == 1 && tolower(var.list) == "all"){var.list <- listAllOutputFiles(run@run.dir)}
  if(length(detailed.var.list) == 1 &&  tolower(detailed.var.list) == "all"){detail.var.list <- listAllOutputFiles(run@run.dir)}
  
  
  ###  FOR EACH VARIABLE
  foreach(var.num = 1:length(var.list),   .verbose = FALSE) %dopar% {
    
    var = var.list[var.num]	 
    
    if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
    
    # look-up quantity, 
    if(var == "mfirefrac" & run@model != "LPJ-GUESS-SPITFIRE") {var <- "firert"}
    this.VegQuantity <- lookupVegQuantity(var)
    
    
   
    ### FOR EACH TIME PERIOD
    for(period in periods){
      
      # open the output file and average it over the required period, resulting in a "VegSpatial" object
      this.VegSpatial <- getVegSpatial(run, period, this.VegQuantity, forceReAveraging = forceReAveraging)
      
      
      ### STANDARD SUMMARY PLOTS 
      plotVegMaps(this.VegSpatial, doIndividual = doIndividual | var %in% detailed.var.list)
      
      ### DETAILED OUTPUT IF REQUESTED
      if(var %in% detailed.var.list){
        
        if(this.VegQuantity@type == "PFT"){
          
          # Calculate the lifeform totals, the temperate total and the evergeen total
          this.VegSpatial <- addVegTotals(this.VegSpatial, target = c("Lifeforms", "Zones", "Phenologies", "Leafforms"))
          
          # plot the per lifeform summary and individual lifeform plots
          plotVegMaps(this.VegSpatial, which.layers = c("Lifeforms"), special.string = "Lifeforms")
          plotVegMaps(this.VegSpatial, which.layers = c("Zones"),  special.string = "ClimateZones")
          plotVegMaps(this.VegSpatial, which.layers = c("Phenologies"), special.string = "Phenologies")
          plotVegMaps(this.VegSpatial, which.layers = c("Leafforms"),  special.string = "Leafforms")
          
          
          
        } 
        else if(this.VegQuantity@type == "monthly"){
          
          this.VegSpatial <- addSeasonal(this.VegSpatial)
          
          # plot the per lifeform summary and individual lifeform plots
          plotVegMaps(this.VegSpatial, which.layers = "Annual")
          plotVegMaps(this.VegSpatial, which.layers = c("DJF", "MAM", "JJA", "SON"), special.string = "Seasonal")
          
        }
        
      }
      
      ### FRACTIONAL PLOTS IF REQUESTED
      if(var %in% fraction.pft.var.list){
        
        if(this.VegQuantity@type == "PFT"){
          
          # Add fractions and plot
          this.VegSpatial <- addVegFractions(this.VegSpatial, targets =  c("PFTs", "Lifeforms", "Zones", "Phenologies", "Leafforms"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE)
          plotVegMaps(this.VegSpatial, which.layers = c("PFTs"), special = "fraction", special.string = "PFT")
          plotVegMaps(this.VegSpatial, which.layers = c("Lifeforms"), special = "fraction", special.string = "Lifeforms")
          plotVegMaps(this.VegSpatial, which.layers = c("Zones"), special = "fraction",  special.string = "ClimateZones")
          plotVegMaps(this.VegSpatial, which.layers = c("Phenologies"), special = "fraction", special.string = "Phenologies")
          plotVegMaps(this.VegSpatial, which.layers = c("Leafforms"), special = "fraction",  special.string = "Leafforms")
          
        }
      }
      
      
      ### DOMINANT PFT IF REQUESTED
      if(var %in% do.dominant.pft.var.list){ 
        
        if(verbose) message("Doing Dominant PFT Classification")
        
        # combine shade tolerance classes and get dominant PFTs
        this.VegSpatial <- combineShadeTolerance(this.VegSpatial)
        this.VegSpatial <- addDominantPFT(this.VegSpatial, do.all = TRUE, do.tree = TRUE, do.woody = TRUE)
        
        # plot dominant PFT
        plotDominantPFTMap(this.VegSpatial, "Dominant")
        plotDominantPFTMap(this.VegSpatial, "DominantTree")
        plotDominantPFTMap(this.VegSpatial, "DominantWoody")
          
      }
      
      # clean up temporal averages to save memory
      rm(this.VegSpatial)
      gc()
      
    } # for each period
    
  } # for each variable 
  
  
  ####################################################################################################
  ###### BENCHMARKING AGAINST SPECIFIC DATASETS 
  ######
  
  # now do each benchmark for the run
  for(benchmarking.dataset in benchmarking.datasets.list){
    
    ### Do the biome classifications and comparisons
    if(benchmarking.dataset@id %in% names(supported.biome.schemes)) {
      
      # get biome scheme and give message
      scheme <- supported.biome.schemes[[benchmarking.dataset@id]]
      if(verbose) message(paste("Doing ", scheme@name,  " Biome Classification"))
      
      # do the comparison, save it for later comparison plots and tidy up
      biome.comparison <- compareBiomes(run, lookupVegQuantity("lai"), periods[["PNV"]], scheme, plot = TRUE)
      run <- addToVegRun(biome.comparison, run)
      rm(scheme, biome.comparison)
      
      
    }
    
    ### do Saatchi2011 if requested
    if(benchmarking.dataset@id == "Saatchi2011"){
      
      # average across the Saatchi years, the calculate the Tree total
      Saatchi.VegSpatial <- getVegSpatial(run, period = benchmarking.dataset@time.span, benchmarking.dataset@veg.quant, forceReAveraging = FALSE)
      Saatchi.VegSpatial <- addVegTotals(Saatchi.VegSpatial, "Tree")
      
      # compare to data
      # Do the generic analysis   
      cmass.local <- lookupVegQuantity("cmass")
      cmass.local@cuts <- seq(0,35,1)
      saatchi.comparison <- compareRunToSpatialDataset(dataset = benchmarking.dataset, 
                                                       vegvar = Saatchi.VegSpatial,
                                                       layer = "Tree",
                                                       quant = cmass.local)
      
      # save the comparison for plotting later
      run <- addToVegRun(saatchi.comparison, run)
      rm(Saatchi.VegSpatial, cmass.local, saatchi.comparison)
      
    } # if doing Saatchi2011 benchmark
    
    
  } # for each benchmark requested
 
  
} # For each run



####################################################################################
################### CODE FOR COMPARISON OF RUNS ####################################
####################################################################################

if(FALSE){
  
  if(verbose) print("Doing comparisons")
  
  ###  Plot comparison of all runs against Saatchi
  if(doSaatchi2011){
    
    compareManyRunsToData(list.of.results = Saatchi.results, 
                          list.of.runs = vegrun.list,
                          dataset = Saatchi.dataset, 
                          label = analysis.label,
                          diff.cuts = seq(-35,35,1),
                          maxpixels = 40000000,
                          plot.dir = plot.dir,
                          showR2 = TRUE,
                          layout.objs = layout.objs,
                          spatial.extent = extent(-180,180,-90,90))
    
  }
  
  
  
  ###  Plot all global biomes and comapre to PNV from Hickler et al. 2006
  if(doGlobalBiomes){
    
    
    labels <- list()
    for(run in vegrun.list){
      labels <- append(labels, run@description)
    }
    
    
    plotBiomeMap(Biome.stack,
                 "all",
                 addData = PNV.biomes, 
                 biome.strings = biome.scheme@strings, 
                 biome.cols= biome.scheme@cols, 
                 file.name = paste("GlobalBiomes", analysis.label, sep = "."),
                 run.title = "Global Biomes",
                 plot.dir = plot.dir,
                 plot.labels = labels,
                 maxpixels = 4000000,
                 plot.height =1000,
                 plot.width = 1800,
                 layout.objs = layout.objs,
                 Cairo.type = c("png","ps"))
    
  }
  
  
  
  
  
}


# terminate with a happy message
message("Processing finished gracefully :-)")

t2 <- Sys.time()
print(t2-t1)


