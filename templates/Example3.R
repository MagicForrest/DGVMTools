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
#############   1. Time series.  Producing is simple enough, but need to take plotting code from Joerg.
################################################################################################################



################################################################################################################
####### PREAMBLE: Load the packages, define the number of threads to use and start the timer
#######           You shouldn't need to change anything here  


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
analysis.label <- "Example3"                    

### Plot directory for run comparison plots (create it if it doesn't exist)
plot.dir <- "/home/forrest/RVCToolsExamples/Example3/Plots"
dir.create(plot.dir, showWarnings = FALSE) 

### Time spans and spatials extents over which to average t
periods <- list(PNV = new("TemporalExtent", id = "Reference", name = "Reference", start = 1961, end = 1990)) 
extents <- standard.continental.extents

### Universal resolution for all runs in this analysis (set to NULL in the unlikely event that not all runs use the same resolution) 
universal.resolution <- "HD"

### OPTIMISING SWITCHES
reread.file <- FALSE # Force re-reading the .out file - in case the raw data have been updated
write <- TRUE # Whether to write the VegObjects to disk for faster reading next time (set this to TRUE when you are sure the data has been properly processed)
single.run.plots <- FALSE # Whether to re-make the 'run summary plots' in a given directory
doRunComparisons <- TRUE

### Which variables to analyse and in what detail (use "all" to choose all *.out files in the run directory)
#var.list <- c("lai", "mwcont_upper", "mwcont_lower", "firert") # simple summary analysis
var.list <- c("lai") # simple summary analysis
detailed.var.list <- NULL #c("anpp") # for which variable to plot in more detail (make individual plots, lifeform plots for PFTs, seasonal plots for monthly variable etc)
fraction.pft.var.list <- NULL #c("lai") # for which PFT variables to plot fractions
do.dominant.pft.var.list <- c("lai") # for which PFT variables to calculate dominant PFT for



### prepare a list of benchmarking datasets
benchmarking.datasets.list <- list(

  # H&P biome map (Smith2014 scheme)
  "Smith2014" = new("SpatialDataset",
                    id = "Smith2014",
                    name = paste("H&P PNV Biomes classified by scheme", "Smith2014", sep = " "),
                    temporal.extent = new("TemporalExtent", id = "PNVPeriod", name = "PNV Period", start = 1961, end = 1990) ,
                    data = readHandPBiomes(resolution = universal.resolution, classification = "Smith2014"),
                    veg.quant = lookupVegQuantity("lai"),
                    units = ""),
  
  # Satchi 2011 above ground biomass
  "Saatchi2011" = getSaatchi2011(resolution = universal.resolution)
  
)


#benchmarking.datasets.list <- prepareBenchmarkingDatasets(list())

### Verbose for extra output
verbose <- TRUE

### Plot type: 




### Choose a smaller plot extent

#Yunnan
plot.extent <- NULL

################################################################################################################
####### RUNS TO PROCESS: Here define the list of runs to process and analyse
#######

vegrun.list <- list()

### ORIGINAL TRUNK VERSION RUNS

vegrun.list[["Original"]] <- defineVegRun(run.dir = "/home/forrest/GuessRuns/SPITFIRE/OriginalTrunk/CRUNCEP_daily_nc/",
                                          id = "Original",
                                          description= "Original Phenology",
                                          pft.set = global.PFTs,
                                          model = "LPJ-GUESS-SPITFIRE",
                                          driving.data = "CRUNCEP",
                                          map.overlay = "world",
                                          london.centre = TRUE,
                                          lonlat.offset = c(0,0),
                                          landuseSimulated = FALSE,
                                          year.offset = 0,
                                          line.col = "darkviolet",
                                          line.width = 1,
                                          line.type = 1
)


vegrun.list[["SubAnnual"]] <- defineVegRun(run.dir = "/home/forrest/GuessRuns/SPITFIRE/Snapshots/2016-04-28-fixed-CtoN/Daily-subannualturnover",
                                           id = "SubAnnual",
                                           description= "Updated Phenology and Turnover",
                                           pft.set = global.PFTs,
                                           model = "LPJ-GUESS-SPITFIRE",
                                           driving.data = "CRUNCEP",
                                           map.overlay = "world",
                                           london.centre = TRUE,
                                           lonlat.offset = c(0,0),
                                           landuseSimulated = FALSE,
                                           year.offset = 0,
                                           line.col = "darkviolet",
                                           line.width = 1,
                                           line.type = 1
) 


base.run <- "Original"


###################################################################################
###### THE MAIN LOOP FOR ALL RUNS
###### 

### for each run - here use parallelisation to processes both/many runs at once
#vegrun.list <- foreach(run = iter(vegrun.list), .verbose = FALSE) %dopar% {
for(run in vegrun.list){ 
  
  if(verbose) message(paste(" +++++++ Processing run", run@id, "+++++++", sep = " "))  
  
  
  ### if required to process all files, get a list of all the .out files present
  if(length(var.list) == 1 && tolower(var.list) == "all"){var.list <- listAllLPJOutput(run@run.dir)}
  if(length(detailed.var.list) == 1 &&  tolower(detailed.var.list) == "all"){detail.var.list <- listAllLPJOutput(run@run.dir)}
  
  
  ###  FOR EACH VARIABLE - here might be an appropriate place to use parallelisation after for standard post-processing
  # This will be fine for writing out simple one-variable plots and pre-averaged files, but care must be taken when saving VegObjs
  # inside the loop for outside the loop because "side-effects" don't generally work with foreach
  #
  #foreach(var.num = 1:length(var.list),   .verbose = FALSE) %dopar% {
  #var = var.list[var.num]	 
  
  for(var in var.list) {
    
    if(verbose) message(paste(" ------- Processing", var, "-------", sep = " "))
    
    # look-up quantity, 
    if(var == "mfirefrac" & run@model != "LPJ-GUESS-SPITFIRE") {var <- "firert"}
    this.VegQuantity <- lookupVegQuantity(var)
    
    
    ### FOR EACH TIME PERIOD
    for(period in periods){
      
      # open the output file and average it over the required period, resulting in a "VegSpatial" object
      this.VegSpatial <- getVegSpatial(run, this.VegQuantity, period, reread.file = reread.file, write = write)
      
      ### STANDARD SUMMARY PLOTS 
      if(this.VegQuantity@id == "firert") {
        if(single.run.plots) plotVegMaps(this.VegSpatial, doIndividual = var %in% detailed.var.list, special = "firert", plot.extent = plot.extent)
      }
      else{
        if(single.run.plots) plotVegMaps(this.VegSpatial, doIndividual = var %in% detailed.var.list, plot.extent = plot.extent)
      }
      
      ### DETAILED OUTPUT IF REQUESTED
      if(var %in% detailed.var.list){
        
        if(this.VegQuantity@type == "PFT"){
          
          # Calculate the lifeform totals, the temperate total and the evergeen total
          this.VegSpatial <- aggregateLayers(this.VegSpatial, target = c("Lifeforms", "Zones", "Phenologies", "Leafforms"))
          
          # plot the per lifeform summary and individual lifeform plots
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Lifeforms"), special.string = "Lifeforms", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Zones"),  special.string = "ClimateZones", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Phenologies"), special.string = "Phenologies", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Leafforms"),  special.string = "Leafforms", plot.extent = plot.extent)
          
          
          
        } 
        else if(this.VegQuantity@type == "monthly"){
          
          this.VegSpatial <- aggregateLayers(this.VegSpatial, c("seasons", "Annual"))
          
          # plot the per lifeform summary and individual lifeform plots
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = "Annual", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("DJF", "MAM", "JJA", "SON"), special.string = "Seasonal", plot.extent = plot.extent)
          
        }
        
      }
      
      ### FRACTIONAL PLOTS IF REQUESTED
      if(var %in% fraction.pft.var.list){
        
        if(this.VegQuantity@type == "PFT"){
          
          # Add fractions and plot
          this.VegSpatial <- divideLayers(this.VegSpatial, targets =  c("PFTs", "Lifeforms", "Zones", "Phenologies", "Leafforms"), denominators = list("Total"), plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("PFTs"), special = "fraction", special.string = "PFT", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Lifeforms"), special = "fraction", special.string = "Lifeforms", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Zones"), special = "fraction",  special.string = "ClimateZones", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Phenologies"), special = "fraction", special.string = "Phenologies", plot.extent = plot.extent)
          if(single.run.plots) plotVegMaps(this.VegSpatial, targets = c("Leafforms"), special = "fraction",  special.string = "Leafforms", plot.extent = plot.extent)
          
        }
      }
      
      
      ### DOMINANT PFT IF REQUESTED
      if(var %in% do.dominant.pft.var.list){ 
        
        if(verbose) message("Doing Dominant PFT Classification")
        
        # combine shade tolerance classes and get dominant PFTs
        this.VegSpatial <- combineShadeTolerance(this.VegSpatial)
        this.VegSpatial <- addDominantPFT(this.VegSpatial, do.all = TRUE, do.tree = TRUE, do.woody = TRUE)
        
        # plot dominant PFT
        plotVegMaps(this.VegSpatial, special = "Dominant")
        plotVegMaps(this.VegSpatial, target = "DominantTree", special = "Dominant")
      
      }
      
      ### SAVE VEGOBJ FOR COMPARISON LATER - note that we have to explicitly access the list element, not the local one inside the loop 
      vegrun.list[[run@id]] <- addToVegRun(this.VegSpatial, IDFromList(run@id,vegrun.list))
      
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
      biome.comparison <- compareBiomes(run, 
                                        lookupVegQuantity("lai"), 
                                        periods[["PNV"]], 
                                        scheme, 
                                        plot = TRUE,
                                        plot.extent = plot.extent)
      vegrun.list[[run@id]] <- addToVegRun(biome.comparison, IDFromList(run@id,vegrun.list))
      
      #run <- addToVegRun(biome.comparison, run)
      rm(scheme, biome.comparison)
      
    }
    
    ### do Saatchi2011 if requested
    if(benchmarking.dataset@id == "Saatchi2011"){
      
      # average across the Saatchi years, the calculate the Tree total
      Saatchi.VegSpatial <- getVegSpatial(run, period = benchmarking.dataset@temporal.extent, benchmarking.dataset@veg.quant, reread.file = FALSE)
      Saatchi.VegSpatial <- aggregateLayers(Saatchi.VegSpatial, "Tree")
      
      # compare to data
      # Do the generic analysis   
      cmass.local <- lookupVegQuantity("cmass")
      cmass.local@cuts <- seq(0,35,1)
      saatchi.comparison <- compareRunToSpatialDataset(dataset = benchmarking.dataset, 
                                                       vegobject = Saatchi.VegSpatial,
                                                       layer = "Tree",
                                                       quant = cmass.local,
                                                       plot.extent = plot.extent)
      
      # save the comparison for plotting later - note that you cannot use just 'run' here, otherwise it won't be saved outside the loop
      vegrun.list[[run@id]] <- addToVegRun(saatchi.comparison, IDFromList(run@id,vegrun.list))
      rm(Saatchi.VegSpatial, cmass.local, saatchi.comparison)
      
    } # if doing Saatchi2011 benchmark
    
    
  } # for each benchmark requested
  
  #return(run) # from arrempt at foreach
  
} # For each run


####################################################################################
################### CODE FOR COMPARISON OF RUNS TO DATASETS ########################
####################################################################################

if(verbose) message(paste("Comparing all runs to benchmarks simultaneously, saving plots to ", plot.dir, sep = ""))

# now do each benchmark for the run
for(benchmarking.dataset in benchmarking.datasets.list){
  
  layout.objs <- vegrun.list[[1]]@map.overlay

  ### if benchmark is Saatchi2011 
  if(benchmarking.dataset@id == "Saatchi2011"){
    
    summariseRasterComparisons(runs = vegrun.list,
                               dataset = benchmarking.dataset, 
                               label = analysis.label,
                               diff.cuts = seq(-35,35,1),
                               plot.dir = plot.dir,
                               plot.extent = plot.extent,
                               layout.objs = layout.objs)
    
    
  }
  
  ### If benchmark is a biome scheme
  if(benchmarking.dataset@id %in% names(supported.biome.schemes)) {
    
    compareManyRunsToBiomes(runs = vegrun.list, 
                            biome.dataset = benchmarking.dataset, 
                            analysis.label = analysis.label,
                            plot.dir = plot.dir,
                            Cairo.height = 1200,
                            Cairo.width = 1400,
                            plot.extent = plot.extent,
                            Cairo.type = c("png","ps"),
                            plot.data = TRUE,
                            layout.objs = layout.objs)
    
  }
 
  stop() 
}

####################################################################################
################### CODE FOR COMPARISON OF RUNS TO EACH OTHER ######################
####################################################################################

if(doRunComparisons) {
  
  message("Doing inter-run comparisons")
  if(!exists("base.run")) base.run <- NULL 
  
  layout.objs <- vegrun.list[[1]]@map.overlay
  standard.args <- list(layout.objs = layout.objs, tag = analysis.label, base.run.id = base.run, plot.dir = plot.dir, plot.extent = plot.extent)
  
  if(verbose) message(paste(" ------- Comparing", var, "-------", sep = " "))
  
  # look-up quantity, 
  if(var == "mfirefrac" & run@model != "LPJ-GUESS-SPITFIRE") {var <- "firert"}
  this.VegQuantity <- lookupVegQuantity(var)
  
  
  ### FOR EACH TIME PERIOD
  for(period in periods){
    
    for(var in var.list){
      
      
      if(tolower(this.VegQuantity@type) == "pft"){
        comparisons <- list("PFT")
        if(var %in% detailed.var.list)  comparisons <- append(comparisons, list(c("phenology"), c("Tree", "Grass")))
      } 
      
      
      for(comparison in comparisons) {
        
        do.call(compareVegSpatialObject,
                append(list(vegrun.list, 
                            makeVegObjectID(var, temporal.extent = period, temporally.averaged = TRUE), 
                            comparison, 
                            doIndividual = TRUE,
                            diff.cuts = seq(-0.5,0.5,0.025)), 
                       standard.args))
      }
      
      
      # 
      # compareVegSpatialObject(vegrun.list, diff.cuts = seq(-0.51,0.51,0.02), "mwcont_lower.TA.1961-1990", c("seasons"), base.run.id = base.run, plot.dir = plot.dir, plot.extent = plot.extent, doIndividual = TRUE, layout.objs = layout.objs, special.string = analysis.label)
      # compareVegSpatialObject(vegrun.list, diff.cuts = seq(-0.21,0.21,0.02), "mwcont_upper.TA.1961-1990", c("seasons"), base.run.id = base.run, plot.dir = plot.dir, plot.extent = plot.extent, doIndividual = TRUE, layout.objs = layout.objs, special.string = analysis.label)
      # compareVegSpatialObject(vegrun.list, diff.cuts = seq(-0.51,0.51,0.02), "mwcont_upper.TA.1961-1990", c("Annual"), base.run.id = base.run, plot.dir = plot.dir, plot.extent = plot.extent, doIndividual = TRUE, layout.objs = layout.objs, special.string = analysis.label)
      # 
      # 
      # 
      # compareVegSpatialObject(vegrun.list, "firert.TA.1961-1990", c("FireRT"), base.run.id = base.run, plot.dir = plot.dir, doSummary = TRUE, special = "firert", plot.extent = plot.extent, layout.objs = layout.objs)
      # 
    }
    
  }
  
}





# terminate with a happy message
message("Processing finished gracefully :-)")

t2 <- Sys.time()
print(t2-t1)


