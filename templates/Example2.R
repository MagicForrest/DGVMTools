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
#############  MF 2016-02-02 : MAJOR TODOS
#############   1. Benchmarking.  Big question.  How self-contained to make the benchmarks?  Which (and how)
#############      to include in the standard RVCTools distribution.
#############   2. Time series.  Producing is simple enough, but need to take plotting code from Joerg.
#############   3. Save VegSpatial objects, VegTS objects and full data.tables to the VegRun object?
################################################################################################################



################################################################################################################
####### PREAMBLE: Load the packages, start the timer and define the number of threads to use
#######


# Load RVCTools package (clear the environment and unload the package first in case we are actively developing the package)
rm(list = ls())
if("package:RVCTools" %in% search()) detach(name = "package:RVCTools", unload = TRUE)
library(RVCTools)
library(foreach)
library(doParallel)

# start the timer
t1 <- Sys.time()

# number of threads - leave at one for single threaded
registerDoParallel(1)




################################################################################################################
####### SETTINGS: Here define the 'analysis' - meaning which variables and datsets to plot, 
#######           the directory and filenames, etc... 
#######


### Analysis plot label and directory for analysis plots
analysis.label <- "Example2"                    # set to NULL or "" [empty string] to use today's date
plot.dir <- "/home/forrest/Temp/Example2"       # here the run comaprison will go

### Time spans and spatials extents over which to average t
periods <- list(PNV = new("TimeSpan", name = "Reference", start = 1961, end = 1990)) 
extents <- standard.continental.extents

### Universal resolution for all runs in this analysis (set to NULL in the unlikely event that not all runs use the same resolution) 
universal.resolution <- "HD"

### Biome classification scheme
biome.scheme <- Smith2014.scheme

### Force re-averaging - in case the raw data have been updated, or we required new a new time period
forceReAveraging <- FALSE

### Make individual plots for each PFT/month/Carbon pool etc. (can also be turned on for individual variables use the detailed.var.list below)
doIndividual <- FALSE

### Which variables to analyse and in what detail (use "all" to choose all *.out files in the run directory)
var.list <- c("mwcont_upper", "lai") # simple summary analysis
detailed.var.list <- c("mwcont_upper", "lai") # for which variable to plot in more detail (make individual plots, lifeform plots for PFTs, seasonal plots for monthly variable etc)
fraction.pft.var.list <- c("lai") # for which PFT variables to plot fractions
do.dominant.pft.var.list <- c("lai") # for which PFT variables to calculate dominant PFT for

### Which benchmarks to do
benchmark.list <- list("Smith2014",
                       "Saatchi2011")


### Spatial analyses
doGlobalBiomes <- FALSE
doSaatchi2011 <- FALSE
doBaccini2012 <- FALSE
doAvitabile2015 <- FALSE
doMODISTreecover <-FALSE
doGFED4 <- FALSE
doFineFuel <- FALSE

### Time series analyses
doGFED4AnnualTS <- FALSE
doGFED4MonthlyTS <- FALSE
doGFED4SeasonalCycle <- FALSE

### Do Temporal Averaging
doTemporalAveraging <- TRUE

### Do Spatial Averaging
doSpatialAveraging <- FALSE

### Do comparisons
doComparisons <- FALSE

### Plots to make for specific analysis
doHistoPlots <- TRUE
doScatterPlots <- TRUE

### Verbose
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
                                                         correct.for.landuse = FALSE,
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
                                                         correct.for.landuse = FALSE,
                                                         year.offset = 0,
                                                         line.col = "darkgreen",
                                                         line.width = 1,
                                                         line.type = 1
)




####################################################################################
###### PREPARATION: read datasets, set up temporary variables etc...
######

# store the original working to return there at the end
original.workdir <- getwd() 

# set analysis label to todays date if nothing is specified
if(is.null(analysis.label) | analysis.label == ""){
  analysis.label <- Sys.Date()
}

# prepare benchmarking datasets
benchmarking.datasets.list <- prepareBenchmarkingDatasets(benchmark.list)


##### MAKE SURE WE PROCESS THE VARIABLES NEEDED FOR SPECIFIC BENCHMARKS

# if doGlobalBiomes we will need to read the LAI data for sure
if(doGlobalBiomes | doDominantPFT){ 
  if(!"lai" %in% var.list & var.list[1] != "all") var.list <- append(var.list, "lai") 
  if(doGlobalBiomes){
    Biome.stack <- stack()
    if(!is.null(universal.resolution)){
      PNV.biomes <- readPNVBiomes(resolution = universal.resolution, classification = biome.scheme@id)
    }
  }
}

# if doing Saatchi or Baccini we will need to read the C mass data for sure
if(doSaatchi2011 | doBaccini2012 | doAvitabile2015){ 
  
  # make sure we process cmass and read the Baccini and Saatchi data if needed
  if(!"cmass" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "cmass") }
  if(doBaccini2012) {
    Baccini.dataset <- getBaccini2012(universal.resolution)  
    Baccini.results <- list()
  }
  if(doSaatchi2011){
    Saatchi.dataset <- getSaatchi2011(universal.resolution)  
    Saatchi.results <- list()
  }
  if(doAvitabile2015){
    Avitabile.dataset <- getAvitabile2015(universal.resolution)  
    Avitabile.results <- list()
  }
}

# if doGFED4 we will need firert for sure
if(doGFED4){
  
  GFED4.dataset <- getGFED4Annual(universal.resolution)
  
  GFED4.results <- list()
  if(!"mfirefrac" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "mfirefrac") }
}

# if doMODISTreecover we will need fpc for sure
if(doMODISTreecover){
  if(!"vegcover" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "vegcover") }
  MODIS.treecover.dataset <- getMODISTreecover(universal.resolution)
  Treecover.results <- list()
  
}

# if doFineFuel
if(doFineFuel){
  
  if(!"litter_leaf" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "litter_leaf") }
  if(!"litter_repr" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "litter_repr") }
  if(!"litter_wood" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "litter_wood") }
  Fine.Fuel.stack <- stack()
  
}

GFED.t1 <- Sys.time()

if(doGFED4AnnualTS){
  GFED4.TS.datasets <- getGFED4TS(extents = extents)
  GFED4.TS.results <- data.frame()
  for(extent in extents){
    GFED4.TS.results <- rbind(GFED4.TS.results,
                              data.frame(Year = GFED4.TS.datasets[[1]]@time.span@start:GFED4.TS.datasets[[1]]@time.span@end,
                                         BurntArea_ha =  GFED4.TS.datasets[[extent@id]]@data,
                                         BurntArea_ha_scaled =  GFED4.TS.datasets[[extent@id]]@data,
                                         Extent = extent@name,
                                         Source = "GFED4"))
    
  }
  if(!"mfirefrac" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "mfirefrac") }
  
} 

if(doGFED4SeasonalCycle){
  
  GFED4.SeasonalCycle.datasets <- getGFED4TS(extents = extents, temporal.resolution = "seasonal.cycle")
  
  GFED4.SeasonalCycle.results <- data.frame()
  for(extent in extents){
    GFED4.SeasonalCycle.results <- rbind(GFED4.SeasonalCycle.results,
                                         data.frame(Month = 1:12,
                                                    BurntArea_ha =  GFED4.SeasonalCycle.datasets[[extent@id]]@data,
                                                    Extent = extent@name,
                                                    Source = "GFED4"))
    
  }
  
  if(!"mfirefrac" %in% var.list & var.list[1] != "all"){ var.list <- append(var.list, "mfirefrac") }
  
} 

GFED.t2 <- Sys.time()
print(GFED.t2-GFED.t1)




###################################################################################
############## THE MAIN LOOP FOR ALL RUNS TO BE PROCESSES #########################
###################################################################################


### for each run
for(run in vegrun.list){
  if(verbose) message(paste(" +++++++ Processing run", run@id, "+++++++", sep = " "))  
  
  # go to the run directory
  setwd(run@run.dir)
  
  ### if required to process all files, get a list of all the .out files present
  if(length(var.list) > 0 && tolower(var.list) == "all"){var.list <- listAllOutputFiles(run@run.dir)}
  if(length(detailed.var.list) > 0 &&  tolower(detailed.var.list) == "all"){detail.var.list <- listAllOutputFiles(run@run.dir)}
  
  ###  FOR PARALLEL
  foreach(var.num = 1:length(var.list),   .verbose = TRUE) %dopar% {
    var = var.list[var.num]	 
      
    if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
    
    # look-up quantity, 
    if(var == "mfirefrac" | run@model != "LPJ-GUESS-SPITFIRE") {var <- "firert"}
    this.VegQuantity <- lookupVegQuantity(var)
    
    ### READ FULL DATA FILE IF REQUESTED ###
    
    # MF: Disable for now, want this to happen more automagically
    
    #     if(doTemporalAveraging | doSpatialAveraging){
    #       
    #       # this stores the full .out file but only if we need to read it we store it so we don't need to read it more than once
    #       if(forceReAveraging) { 
    #         if(forceReAveraging & verbose) message(paste("Reading raw data from ", var, ".out because forceReAveraging is set to TRUE", sep = ""))
    #         if(!exists("this.full") & verbose) message(paste("Reading raw data from ", var, ".out", sep = ""))
    #         this.full <- openLPJOutputFile(run, var, verbose = TRUE)
    #       }
    #       else {
    #         this.full <- NULL
    #       }
    #       
    #     }
    
    ####################################################################################################
    ################### TEMPORAL AVERAGING AND SPATIAL ANALYSIS/PLOTTING ###############################
    ########## Time average over all time periods, write files and make plots ##########################
    ####################################################################################################
    
    
    if(doTemporalAveraging){
      
      
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
        if(var %in% pft.fraction.var.list){
          
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
          
          ### Here write the table, maybe can be used in another script or plotting program
          #write.table(this.TA.dt[, list(Lon, Lat, Dominant, DominantTree, DominantWoody)], file = paste0("Dominants.", period@start, "-", period@end, ".Rtable"), row.names= FALSE, quote= FALSE)
                    
        }
        
        
        
        ### SPECIFIC ANALYSES FOR EVERY TIME PERIOD (FOR EXAMPLE GLOBAL BIOMES OR DOMINANT PFT)
        
        # Global Biomes
        if(doGlobalBiomes & var == "lai"){
          
          if(verbose) message("Doing Global Biome Classification")
          
          
          #         # Add GDD5 for classifying tundra
          #         if(forceReAveraging & !exist(this.full.bioclim)) { 
          #           message(paste("Reading raw data from ", "bioclim", ".out because forceReAveraging is set to TRUE", sep = ""))
          #           this.full.bioclim <- openLPJOutputFile(run, "bioclim", verbose = TRUE)
          #         }
          #         
          #         this.TA.bioclim.dt <- getTADT(run, period, "bioclim", this.full = this.full.bioclim, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
          #         
          #         this.TA.dt <- this.TA.dt[this.TA.bioclim.dt]
          #         
          
          this.TA.dt[,GDD5 := 500]
          
          addBiomes(this.TA.dt, global.PFTs, biome.scheme)
          plotBiomeMap(this.TA.dt, 
                       which.layers = biome.scheme@id,
                       biome.strings = biome.scheme@strings, 
                       biome.cols= biome.scheme@cols, 
                       run = run, 
                       period = period, 
                       addData = PNV.biomes, 
                       run.title = run@id, 
                       maxpixels = 100000,
                       Cairo.type = c("png","ps"), 
          )
          
          ### Here write the table, maybe can be used in another script or plotting program
          write.table(this.TA.dt[, list(Lon, Lat, biome.scheme@id)], file = paste(biome.scheme@id, "Biome", paste(period@start, period@end, sep = "-"), "Rtable", sep = "."), row.names= FALSE, quote= FALSE)
          
          biome.raster <- promoteToRaster(this.TA.dt, biome.scheme@id, run@tolerance)
          biome.raster <- extend(biome.raster, extent(-180,180,-90,90))
          names(biome.raster) <- run@id
          Biome.stack <- addLayer(Biome.stack, biome.raster)
          rm(biome.raster)
          
        }
        
               
        # Here save the litters raster for calculating total fine litter (fuel)
        if(doFineFuel & var == "litter_leaf") Leaf.Litter.raster <- promoteToRaster(this.TA.dt, "Total")     
        if(doFineFuel & var == "litter_repr") Repr.Litter.raster <- promoteToRaster(this.TA.dt, "Total")  
        if(doFineFuel & var == "litter_wood") Wood.Litter.raster <- promoteToRaster(this.TA.dt, "Total")     
        
        # clean up temporal averages to save memory
        rm(this.VegSpatial)
        gc()
        
      } # for each period
      
      
      ###########################################################################################
      ########################## SPECIFIC ANALYSES/BENCHMARKS ###################################
      ###########################################################################################
      
      
      ##### SAATCHI BIOMASS BENCHMARK
      if(var == "cmass" & doSaatchi2011){
        
        # average the model data
        this.TA.dt <- getTADT(run, period = Saatchi.dataset@time.span,  var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
        addLifeformTotals(this.TA.dt, global.PFTs)
        model.raster <- promoteToRaster(this.TA.dt, "Tree", run@tolerance)
        
        # Land use correction 
        if(run@correct.for.landuse){
          full.correction.data <- readNaturalLandCoverGC2005("HD")
          correction.raster <- crop(full.correction.data, model.raster)
          model.raster <- model.raster * correction.raster
        }
        
        # Do the generic analysis	 
        cmass.local <- lookupVegQuantity("cmass")
        cmass.local@cuts <- seq(0,35,1)
        
        Saatchi.results[[run@id]] <- compareRunToSpatialDataset(Saatchi.dataset, 
                                                                model.raster,
                                                                run,
                                                                diff.breaks = seq(-60, 60, 1), 
                                                                plot.breaks = seq(-35,35,1),  
                                                                histo.plot.range = c(-20,30), 
                                                                doScatterPlots=doScatterPlots, 
                                                                doHistoPlots=doHistoPlots, 
                                                                quant = cmass.local,
                                                                map.layout.objs = layout.objs
        )				
        
        rm(this.TA.dt, model.raster, full.correction.data, correction.raster )
        
      }
      
      ##### Baccini BIOMASS BENCHMARK
      if(var == "cmass" & doBaccini2012){
        
        # average the model data
        this.TA.dt <- getTADT(run, Baccini.dataset@time.span,  var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
        addLifeformTotals(this.TA.dt, global.PFTs)
        model.raster <- promoteToRaster(this.TA.dt, "Tree", run@tolerance)
        
        # Land use correction 
        if(run@correct.for.landuse){
          full.correction.data <- readNaturalLandCoverGC2009("HD")
          correction.raster <- extend(crop(full.correction.data, model.raster), model.raster)
          model.raster <- model.raster * correction.raster
        }
        
        # Do the generic analysis   
        cmass.local <- lookupVegQuantity("cmass")
        cmass.local@cuts <- seq(0,35,1)
        
        Baccini.results[[run@id]] <- compareRunToSpatialDataset(Baccini.dataset, 
                                                                model.raster,
                                                                run,
                                                                diff.breaks = seq(-60, 60, 1), 
                                                                plot.breaks = seq(-35,35,1),  
                                                                histo.plot.range = c(-20,30), 
                                                                doScatterPlots=doScatterPlots, 
                                                                doHistoPlots=doHistoPlots, 
                                                                quant = cmass.local,
                                                                map.layout.objs = layout.objs
        )				
        
        rm(this.TA.dt, model.raster, full.correction.data, correction.raster )
        
      }
      
      ##### AVITABILE BIOMASS BENCHMARK
      if(var == "cmass" & doAvitabile2015){
        
        # average the model data
        this.TA.dt <- getTADT(run, Avitabile.dataset@time.span,  var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
        addLifeformTotals(this.TA.dt, global.PFTs)
        model.raster <- promoteToRaster(this.TA.dt, "Tree", run@tolerance)
        
        # Land use correction 
        if(run@correct.for.landuse){
          full.correction.data <- readNaturalLandCoverGC2005("HD")
          correction.raster <- extend(crop(full.correction.data, model.raster), model.raster)
          model.raster <- model.raster * correction.raster
        }
        
        # Do the generic analysis   
        cmass.local <- lookupVegQuantity("cmass")
        cmass.local@cuts <- seq(0,35,1)
        
        Avitabile.results[[run@id]] <- compareRunToSpatialDataset(Avitabile.dataset, 
                                                                  model.raster,
                                                                  run,
                                                                  diff.breaks = seq(-60, 60, 1), 
                                                                  plot.breaks = seq(-35,35,1),  
                                                                  histo.plot.range = c(-20,30), 
                                                                  doScatterPlots=doScatterPlots, 
                                                                  doHistoPlots=doHistoPlots, 
                                                                  quant = cmass.local,
                                                                  map.layout.objs = layout.objs
        )  			
        
        rm(this.TA.dt, model.raster, full.correction.data, correction.raster )
        
      }
      
      ##### MODIS TREECOVER BENCHMARK
      if(var == "vegcover" & doMODISTreecover){
        
        # average the model data
        this.TA.dt <- getTADT(run, period = MODIS.treecover.dataset@time.span,  var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
        model.raster <- promoteToRaster(this.TA.dt, "Tree", run@tolerance)
        
        # Land use correction 
        if(run@correct.for.landuse){
          full.correction.data <- readNaturalLandCoverGC2005("HD")
          correction.raster <- extend(crop(full.correction.data, model.raster), model.raster)
          model.raster <- model.raster * correction.raster
        }
        
        # Do the generic analysis   
        Treecover.results[[run@id]] <- compareRunToSpatialDataset(MODIS.treecover.dataset, 
                                                                  model.raster,
                                                                  run,
                                                                  diff.breaks = seq(-2, 2, 0.05),
                                                                  plot.breaks = seq(-1,1,0.02),  
                                                                  histo.plot.range = c(-1, 1.5), 
                                                                  doScatterPlots=doScatterPlots, 
                                                                  doHistoPlots=doHistoPlots, 
                                                                  quant = lookupVegQuantity("vegcover"),
                                                                  map.layout.objs = layout.objs
        )  			
        
        rm(this.TA.dt, model.raster, full.correction.data, correction.raster )
        
      }
      
      
      ##### GFED4 BURNT AREA BENCHMARK
      if(doGFED4 & (var == "mfirefrac" || var == "firert")){
        
        file.string = file.path(run@run.dir, paste(var, ".out", sep=""))
        if(var == "mfirefrac"){ 
          this.TA.dt <- getTADT(run, period = GFED4.dataset@time.span,  var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
          addSeasonal(this.TA.dt, method = "total")
          model.raster <- promoteToRaster(this.TA.dt, layers = "Annual", run@tolerance) 
          #model.raster[model.raster > 2] <- 2   
        }
        
        else if(var == "firert") {
          this.TA.dt <- getTADT(run, period = GFED4.dataset@time.span,  var = "firert", this.full = NULL, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
          model.raster <- 1 / promoteToRaster(this.TA.dt, tolerance = run@tolerance)
          model.raster[model.raster > 999] <- 999      
        }
        
        # Do the generic analysis   
        GFED4.results[[run@id]] <- compareRunToSpatialDataset(GFED4.dataset, 
                                                              model.raster,
                                                              run,
                                                              diff.breaks = seq(-10, 10, 0.02),
                                                              plot.breaks = seq(-1,1,0.02),  
                                                              histo.plot.range = c(-1, 1), 
                                                              doScatterPlots=doScatterPlots, 
                                                              doHistoPlots=doHistoPlots, 
                                                              quant = lookupVegQuantity("burntarea"),
                                                              map.layout.objs = layout.objs
        )    	
        
        model.firert <- 1/model.raster
        model.firert[model.firert > 999] <- 999      
        data.firert <- GFED4.dataset@data
        data.firert[data.firert < 0.001] <- 0.001
        data.firert <- mask(extend(crop(1/data.firert, model.raster), model.raster), model.raster)
        
        combined.firert <- stack(model.firert,data.firert)
        
        plotFireRT(combined.firert, 
                   run=run, 
                   main.title = "Fire Return Time",
                   plot.labels = c(run@description, "GFED4 (1996-2012)"),
                   layout.objs = layout.objs)
        
        rm(this.VegSpatial, model.raster, model.firert, data.firert, combined.firert)	
        
      } # if doFGED4 
      
      
      rm(this.VegSpatial)
      
    } # ifDoTemporalAveraging
    
    
    
    
    
    ####################################################################################################
    ################### SPATIAL AVERAGING AND TEMPORAL ANALYSIS/PLOTTING ###############################
    ####################################################################################################
    
    if(doSpatialAveraging){ 
      
      # For each spatial domain
      for(extent in extents){
        
        print(extent@name)
        
        ##### GET SPATIALLY-AVERAGED DATA #####
        # get the time averaged data.table, either by reading it from disk or by averaging the raw *.out file
        this.SA.dt <- getSADT(run, var, spatial.extent = extent, this.full = this.full, write = FALSE, forceReAveraging = forceReAveraging, verbose = verbose)
        
        # GFED4 time series benchmarks
        if((doGFED4AnnualTS | doGFED4SeasonalCycle) & (var == "mfirefrac" || var == "firert")){
          
          # read the full file if necessary, select required extent and years
          if(is.null(this.full)) this.full <- openLPJOutputFile(run, var, verbose = TRUE)
          this.cropped <- cropLPJ(this.full, extent)
          this.years <- selectYears(this.cropped, GFED4.TS.datasets[[1]]@time.span)
          
          if(doGFED4AnnualTS) {
            
            addSeasonal(this.cropped, seasons = c("Annual"), method = "sum", verbose = TRUE)
            
            # promote to raster and add to stack so we can handle gridcell areas
            annual.stack <- stack()
            for(year in GFED4.TS.datasets[[1]]@time.span@start:GFED4.TS.datasets[[1]]@time.span@end){
              annual.stack <- addLayer(annual.stack, promoteToRaster(this.cropped[Year == year,], "Annual"))
            }
            
            # calculate burnt area from burnt fraction, then sum, then covert to xts
            annual.stack <- annual.stack * area(annual.stack) * kmsq_to_ha
            annual.timeseries <- cellStats(annual.stack, sum)
            
            scaling.factor <- sum(subset(GFED4.TS.results, Source=="GFED4" & Extent==extent@name)$BurntArea_ha) / sum(annual.timeseries)
            
            GFED4.TS.results <- rbind(GFED4.TS.results,
                                      data.frame(Year = GFED4.TS.datasets[[1]]@time.span@start:GFED4.TS.datasets[[1]]@time.span@end,
                                                 BurntArea_ha = annual.timeseries,
                                                 BurntArea_ha_scaled = annual.timeseries * scaling.factor,
                                                 Extent = extent@name,
                                                 Source = run@description))
            
          } # if doGFED4AnnualTS
          
          if(doGFED4SeasonalCycle) {
            
            this.annual.average <- doTimeAverage(this.years, period = GFED4.TS.datasets[[1]]@time.span, verbose = FALSE)
            
            # promote to raster, calculate area and burnt area from burnt fraction, then sum, then covert to xts
            monthly.stack <- promoteToRaster(this.annual.average)
            monthly.stack  <- monthly.stack  * area(monthly.stack ) * kmsq_to_ha
            seasonal.cycle.timeseries <- cellStats(monthly.stack, sum)
            
            GFED4.SeasonalCycle.results <- rbind(GFED4.SeasonalCycle.results,
                                                 data.frame(Month = 1:12,
                                                            BurntArea_ha = seasonal.cycle.timeseries,
                                                            Extent = extent@name,
                                                            Source = run@description))
            
            
            
            
          } # if doGFED4SeasonalCyle
          
        } # if do GFED4 time series analyses
        
        rm(this.cropped)
        gc()
        
      } # for each Spatial Extent
      
      
      # clean up to save memory   
      rm(this.SA.dt)
      
      gc()
      
    } # if do SpatialAveraging
    
    rm(this.full)
    gc()
    message("Done.")
    
    
  } # for each variable 
  
  
  # now do each benchmark
  for(benchmarking.dataset in benchmarking.datasets.list){
    
    if(benchmarking.dataset@id %in% names(supported.biome.schemes)) {
     
      scheme <- supported.biome.schemes[[benchmarking.dataset@id]]
      
      if(verbose) message("Doing Global Biome Classification")
      
      
      #         # Add GDD5 for classifying tundra
      #         if(forceReAveraging & !exist(this.full.bioclim)) { 
      #           message(paste("Reading raw data from ", "bioclim", ".out because forceReAveraging is set to TRUE", sep = ""))
      #           this.full.bioclim <- openLPJOutputFile(run, "bioclim", verbose = TRUE)
      #         }
      #         
      #         this.TA.bioclim.dt <- getTADT(run, period, "bioclim", this.full = this.full.bioclim, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
      #         
      #         this.TA.dt <- this.TA.dt[this.TA.bioclim.dt]
      #         
      
      this.TA.dt[,GDD5 := 500]
      
      addBiomes(this.TA.dt, global.PFTs, biome.scheme)
      plotBiomeMap(this.TA.dt, 
                   which.layers = biome.scheme@id,
                   biome.strings = biome.scheme@strings, 
                   biome.cols= biome.scheme@cols, 
                   run = run, 
                   period = period, 
                   addData = PNV.biomes, 
                   run.title = run@id, 
                   maxpixels = 100000,
                   Cairo.type = c("png","ps"), 
      )
      
      ### Here write the table, maybe can be used in another script or plotting program
      write.table(this.TA.dt[, list(Lon, Lat, biome.scheme@id)], file = paste(biome.scheme@id, "Biome", paste(period@start, period@end, sep = "-"), "Rtable", sep = "."), row.names= FALSE, quote= FALSE)
      
      biome.raster <- promoteToRaster(this.TA.dt, biome.scheme@id, run@tolerance)
      biome.raster <- extend(biome.raster, extent(-180,180,-90,90))
      names(biome.raster) <- run@id
      Biome.stack <- addLayer(Biome.stack, biome.raster)
      rm(biome.raster)
      
      
      
      
      
    }
    
   
    
    
    
  }
    
  
  
  
  # Calculated fine fuel after retrived all litter types...
  if(doFineFuel){
    Fine.Fuel.raster <- Leaf.Litter.raster + Repr.Litter.raster + 0.045 * Wood.Litter.raster
    names(Fine.Fuel.raster) <- run@id
    Fine.Fuel.stack <- addLayer(Fine.Fuel.stack, Fine.Fuel.raster)
    plotVegMaps(Fine.Fuel.raster, 
                run = run,
                quant = "fine_fuel", 
                period = period, 
                doSummary = TRUE, 
                doIndividual = FALSE, 
                useLongnames = FALSE, 
                maxpixels = 100000)
    rm(Fine.Fuel.raster)
  }
  
  # return to original working directory
  setwd(original.workdir)
  
  
} # For each run



####################################################################################
################### CODE FOR COMPARISON OF RUNS ####################################
####################################################################################

if(doComparisons){
  
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
  
  
  ###  Plot comparison of all runs against Baccini
  if(doBaccini2012){
    
    
    compareManyRunsToData(list.of.results = Baccini.results, 
                          list.of.runs = vegrun.list,
                          dataset = Baccini.dataset, 
                          label = analysis.label,
                          diff.cuts = seq(-35,35,1),
                          maxpixels = 40000000,
                          plot.dir = plot.dir,
                          showR2 = TRUE,
                          layout.objs = layout.objs,
                          spatial.extent = extent(-180,180,-90,90))
    
  }
  
  ###  Plot comparison of all runs against Avitabile
  if(doAvitabile2015){
    
    
    compareManyRunsToData(list.of.results = Avitabile.results, 
                          list.of.runs = vegrun.list,
                          dataset = Avitabile.dataset, 
                          label = analysis.label,
                          diff.cuts = seq(-35,35,1),
                          maxpixels = 40000000,
                          plot.dir = plot.dir,
                          showR2 = TRUE,
                          layout.objs = layout.objs,
                          spatial.extent = extent(-180,180,-90,90))
    
  }
  
  
  ###  Plot comparison of all runs against Treecover
  if(doMODISTreecover){
    
    compareManyRunsToData(list.of.results = Treecover.results, 
                          list.of.runs = vegrun.list,
                          dataset = MODIS.treecover.dataset, 
                          label = analysis.label,
                          diff.cuts = NULL,
                          maxpixels = 40000000,
                          plot.dir = plot.dir,
                          showR2 = TRUE,
                          layout.objs = layout.objs,
                          spatial.extent = extent(-180,180,-90,90)
    )
    
    
    
  }
  
  ###  Plot comparison of all runs against GFED4
  if(doGFED4){
    
    compareManyRunsToData(list.of.results = GFED4.results, 
                          list.of.runs = vegrun.list,
                          dataset = GFED4.dataset, 
                          label = analysis.label,
                          diff.cuts = NULL,
                          maxpixels = 40000000,
                          plot.dir = plot.dir,
                          showR2 = TRUE,
                          layout.objs = layout.objs,
                          spatial.extent = extent(-180,180,-90,90))
    
    
  }
  
  
  
  ###  Plot all global biomes and comapre to PNV from Hickler et al. 2006
  if(doGlobalBiomes){
    
    
    #if(!exists("Saatchi.dataset")) Saatchi.dataset <- readSaatchi2011()  
    
    #Biome.stack <- crop(Biome.stack, Saatchi.dataset@data)
    
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
    
    #     plotBiomeMap(this.TA.dt, 
    #                  biome.strings = biome.scheme@strings, 
    #                  biome.cols= biome.scheme@cols, 
    #                  run = run, 
    #                  period = period, 
    #                  addData = PNV.biomes, 
    #                  run.title = run@id, 
    #                  maxpixels = 100000,
    #                  Cairo.type = c("png","ps"), 
    #     )
    #     
  }
  
  if(doGFED4AnnualTS){
    
    line.cols <- c("black")
    line.widths <- c(1.5)
    line.types <- c(1)
    
    for(run in vegrun.list){
      
      line.cols <- append(line.cols, run@line.col)
      line.types <- append(line.types, run@line.type)
      line.widths <- append(line.widths, run@line.width)
      
    }
    
    for(extent in extents){
      
      Cairo(file = file.path(plot.dir, paste("BA.TS", analysis.label, extent@id, "png", sep = ".")), width = 1200, height = 500)
      temp.plot <- ggplot(GFED4.TS.results[GFED4.TS.results$Extent == extent@name,],aes(x=Year,y=BurntArea_ha,colour=Source,group=Source)) + geom_line(aes(linetype=Source, color=Source, size=Source))
      temp.plot <- temp.plot + ggtitle(paste(extent@name))
      temp.plot <- temp.plot + scale_linetype_manual(values=line.types)
      temp.plot <- temp.plot + scale_color_manual(values=line.cols) 
      temp.plot <- temp.plot + scale_size_manual(values=line.widths) 
      temp.plot <- temp.plot + theme(legend.title=element_blank(), 
                                     legend.text = element_text(size = 18, 
                                                                hjust = 3, 
                                                                vjust = 3, 
                                                                face = 'bold'), 
                                     legend.key.size = unit(2, "cm"), 
                                     plot.title = element_text(size = rel(2)),
                                     axis.text=element_text(size=14),
                                     axis.title=element_text(size=18,face="bold"))
      print(temp.plot)
      dev.off()
      
      Cairo(file = file.path(plot.dir, paste("BA.TS_scaled", analysis.label, extent@id, "png", sep = ".")), width = 1200, height = 500) 
      temp.plot <- ggplot(GFED4.TS.results[GFED4.TS.results$Extent == extent@name,],aes(x=Year,y=BurntArea_ha_scaled,colour=Source,group=Source)) + geom_line(aes(linetype=Source, color=Source, size=Source))
      temp.plot <- temp.plot + ggtitle(paste(extent@name, "(models scaled to data)"))
      temp.plot <- temp.plot + scale_linetype_manual(values=line.types)
      temp.plot <- temp.plot+ scale_color_manual(values=line.cols) 
      temp.plot <- temp.plot+ scale_size_manual(values=line.widths) 
      temp.plot <- temp.plot + theme(legend.title=element_blank(), 
                                     legend.text = element_text(size = 18, 
                                                                hjust = 3, 
                                                                vjust = 3, 
                                                                face = 'bold'), 
                                     legend.key.size = unit(2, "cm"), 
                                     plot.title = element_text(size = rel(2)),
                                     axis.text=element_text(size=14),
                                     axis.title=element_text(size=18,face="bold"))
      print(temp.plot)
      dev.off()
    }
    
    if(doGFED4SeasonalCycle) {
      
      line.cols <- c("black")
      line.widths <- c(1.5)
      line.types <- c(1)
      
      
      for(run in vegrun.list){
        
        line.cols <- append(line.cols, run@line.col)
        line.types <- append(line.types, run@line.type)
        line.widths <- append(line.widths, run@line.width)
        
      }
      
      for(extent in extents){
        
        print("Plotting seasonal cycle")
        
        Cairo(file = file.path(plot.dir, paste("BA.SeasonalCycle", analysis.label, extent@id, "png", sep = ".")), width = 1200, height = 500)
        temp.plot <- ggplot(GFED4.SeasonalCycle.results[GFED4.SeasonalCycle.results$Extent == extent@name,],aes(x=Month,y=BurntArea_ha,colour=Source,group=Source)) + geom_line(aes(linetype=Source, color=Source, size=Source))
        temp.plot <- temp.plot + ggtitle(paste(extent@name))
        temp.plot <- temp.plot + scale_linetype_manual(values=line.types)
        temp.plot <- temp.plot + scale_color_manual(values=line.cols) 
        temp.plot <- temp.plot + scale_size_manual(values=line.widths) 
        temp.plot <- temp.plot + theme(legend.title=element_blank(), 
                                       legend.text = element_text(size = 18, 
                                                                  hjust = 3, 
                                                                  vjust = 3, 
                                                                  face = 'bold'), 
                                       legend.key.size = unit(2, "cm"), 
                                       plot.title = element_text(size = rel(2)),
                                       axis.text=element_text(size=14),
                                       axis.title=element_text(size=18,face="bold"))
        temp.plot <- temp.plot + scale_x_continuous(breaks=1:12)
        print(temp.plot)
        dev.off()
        
      }
      
      
    }
    
    
    
    
    
    
    
  }
  
  
  
  
}

rm(Treecover.results, GFED4.results, Biome.stack)
if(doBaccini2012) rm(Baccini.dataset, Baccini.results)
if(doSaatchi2011) rm(Saatchi.dataset, Saatchi.results)


# terminate with a happy message
message("Processing finished gracefully :-)")


t2 <- Sys.time()
print(t2-t1)


