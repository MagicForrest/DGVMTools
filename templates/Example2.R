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
#############  MF 2016-01-26 : THIS NEEDS A LOT OF WORK TO REGAIN FUNCTIONALITY  ###############################
################################################################################################################



# use the RVCTools v2.9
source("~/Tools/RVCTools/v2.9/rvc-tools.R")

registerDoParallel(2)


###################################################################################
############################# SETTINGS #######################################
###################################################################################

### Label for analysis (set to NULL or "" [empty string] to use today's date)
analysis.label <- "Example2"

### Time spans to average
periods = list(PNV = new("TimeSpan", name = "", start = 1961, end = 1990)) 

### Spatial Extents - to average over for the time series 
extents = list(Global = new("SpatialExtent", id = "Global", name = "Global", extent = extent(-180, 180, -90, 90)),
               Africa = new("SpatialExtent", id = "Africa", name = "Africa", extent =  extent(-20, 55, -30, 36)),
               Europe = new("SpatialExtent", id = "Europe", name = "Europe", extent =  extent(-30, 40, 36, 70)),
               Asia = new("SpatialExtent", id = "Asia", name = "Asia", extent =  extent(40, 180, -10, 80)),
               NorthAmerica = new("SpatialExtent", id = "NorthAmerica", name = "North America", extent =  extent(-170, -70, 25, 75)),
               SouthAmerica = new("SpatialExtent", id = "SouthAmerica", name = "South America", extent = extent(-180, -50, -60, 25)),
               Australia = new("SpatialExtent", id = "Australia", name = "Australia", extent = extent(110, 160, -45 ,10)),
               Mediterranean = new("SpatialExtent", id = "Med", name = "Mediterranean", extent = extent(10, 40, 28 ,48)),
               CentralAsia = new("SpatialExtent", id = "CentralAsia", name = "Central Asia", extent = extent(25, 140, 40, 55)),
               SouthEastAsia = new("SpatialExtent", id = "SouthEastAsia", name = "South East Asia", extent = extent(90, 140, 10, 40)),
               CentralNorthAmerica = new("SpatialExtent", id = "CentralNorthAmerica", name = "Central North America", extent = extent(-110, -85, 30, 50)),
               Boreal = new("SpatialExtent", id = "Boreal", name = "Boreal", extent = extent(-180, 180, 60, 90)),
               NHAfrica = new("SpatialExtent", id = "NHAfrica", name = "Northern Hemisphere Africa", extent = extent(-20, 50, 0, 25)),
               SHAfrica = new("SpatialExtent", id = "SHAfrica", name = "Southern Hemisphere Africa", extent = extent(5, 50, -30, 0))
               
)

### Directory to save combi plots
plot.dir <- "/home/forrest/Temp/Example2"

### Universal resolution for all runs in this analysis (set to NULL in the unlikely event that not all runs use the same resolution) 
universal.resolution <- "HD"

### Biome classification scheme
biome.scheme <- Smith2014.scheme


### Force re-averaging - in case the raw data have been updated, or we required new a new time period
forceReAveraging <- FALSE

### Make individual plots for each PFT/month/Carbon pool etc.
### (can also be turned on for individual variables use the detailed.var.list below)
doIndividual <- FALSE

### Variables to analyse
### set to all in the run directory using 
var.list <- list("mwcont_upper")
### or specify as an R list using the following 
#var.list <- list("mwcont_upper")                                                                                                                         

### Which variable to plot in more detail (make individual plots, lifeform plots for PFTs, seasonal plots for monthly variable etc)
### Syntax as above
#detailed.var.list <- "all"
detailed.var.list <- list("mwcont_upper")

### Which variable to plot fractions
fraction.var.list <- list()

### Spatial analyses
doGlobalBiomes <- TRUE
doDominantPFT <- TRUE
doSaatchi2011 <- FALSE
doBaccini2012 <- FALSE
doAvitabile2015 <- TRUE
doMODISTreecover <-TRUE
doGFED4 <- TRUE
doFineFuel <- FALSE

### Time series analyses
doGFED4AnnualTS <- FALSE
doGFED4MonthlyTS <- FALSE
doGFED4SeasonalCycle <- FALSE

### Do Temporal Averaging
doTemporalAveraging <- TRUE

### Do Spatial Averaging
doSpatialAveraging <- TRUE

### Do comparisons
doComparisons <- TRUE

### Plots to make for specific analysis
doHistoPlots <- TRUE
doScatterPlots <- TRUE

### Verbose
verbose <- TRUE



###################################################################################
############################# RUNS TO PROCESS #####################################
###################################################################################

### create a list of VegRun objects that we want to analyse
vegrun.list <- list()

vegrun.list[["LPJ-GUESS-SPITFIRE-Run1"]] <- new("VegRun",
                                                run.dir = "/data/forrest/GuessRuns/SPITFIRE/v0.7.0/CRUNCEP_OFM_PB-0.2_CB-0.05_HIP-1.0_NoShrubs_res.mult.5",
                                                id = "LPJ-GUESS-SPITFIRE-Run1",
                                                description= "LPJ-GUESS-SPITFIRE Example Run 1",
                                                driving.data = "CRUNCEP",
                                                map.overlay = "lowres",
                                                london.centre = TRUE,
                                                lonlat.offset = c(0.25,0.25),
                                                correct.for.landuse = FALSE,
                                                year.offset = 0,
                                                line.col = "darkviolet",
                                                line.width = 1,
                                                line.type = 1
                                                
)



vegrun.list[["LPJ-GUESS-SPITFIRE-Run2"]] <- new("VegRun",
                                                run.dir = "/data/forrest/GuessRuns/SPITFIRE/v0.7.0/CRUNCEP_NFM_PB-1_CB-0_HIP-3.0_NoShrubs_res.mult.5",
                                                id = "LPJ-GUESS-SPITFIRE-Run2",
                                                description= "LPJ-GUESS-SPITFIRE Example Run 2",
                                                driving.data = "CRUNCEP",
                                                map.overlay = "lowres",
                                                london.centre = TRUE,
                                                lonlat.offset = c(0.25,0.25),
                                                correct.for.landuse = FALSE,
                                                year.offset = 0,
                                                line.col = "darkgreen",
                                                line.width = 1,
                                                line.type = 1
                                                
)


####################################################################################
###################### AUTOMATIC PREAMBLE AND CHECKS ###############################
####################################################################################


# timing
t1 <- Sys.time()

# Before processing check all directories exist
for(run in vegrun.list){
  if(!file_test("-d", run@run.dir)) stop(paste("No directory:", run@run.dir, sep = " "))
}
message("All run directories present and correct.")

# store the original working to return there at the end
original.workdir <- getwd() 

# set analysis label to todays date if nothing is specified
if(is.null(analysis.label) | analysis.label == ""){
  analysis.label <- Sys.Date()
}


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

# empty layout object
layout.objs <- list(costlines = makeOverlay("lowres"))
mp = 1E20



###################################################################################
############## THE MAIN LOOP FOR ALL RUNS TO BE PROCESSES #########################
###################################################################################


### for each run
for(run in vegrun.list){
  if(verbose) message(paste(" +++++++ Processing run", run@id, "+++++++", sep = " "))  
  
  # go to the run directory
  setwd(run@run.dir)
  
  ### if required to process all files, get a list of all the .out files present
  if(tolower(var.list) == "all"){var.list <- listAllOutputFiles(run@run.dir)}
  if(length(detailed.var.list) > 0 &&  tolower(detailed.var.list) == "all"){detail.var.list <- listAllOutputFiles(run@run.dir)}
  
  ###  FOR PARALLEL
  #foreach(var.num = 1:length(var.list),
  #        .verbose = TRUE) %dopar% {
  
  #         var = var.list[var.num]	 
  
  
  
  
  ### FOR SINGLE PROCESS
  for(var in var.list){
    
    if(verbose) message(paste(" ------ Processing", var, "------", sep = " "))
    
    
    ### READ FULL DATA FILE IF REQUESTED ###
    if(doTemporalAveraging | doSpatialAveraging){
      
      # this stores the full .out file but only if we need to read it we store it so we don't need to read it more than once
      if(forceReAveraging) { 
        if(forceReAveraging & verbose) message(paste("Reading raw data from ", var, ".out because forceReAveraging is set to TRUE", sep = ""))
        if(!exists("this.full") & verbose) message(paste("Reading raw data from ", var, ".out", sep = ""))
        this.full <- openLPJOutputFile(run, var, verbose = TRUE)
      }
      else {
        this.full <- NULL
      }
      
    }
    
    ####################################################################################################
    ################### TEMPORAL AVERAGING AND SPATIAL ANALYSIS/PLOTTING ###############################
    ########## Time average over all time periods, write files and make plots ##########################
    ####################################################################################################
    
    
    if(doTemporalAveraging){
      
      
      for(period in periods){
        
        
        #### TIME AVERAGE AND SAVE #####
        
        # get the time averaged data.table, either by reading it from disk or by averaging the raw *.out file
        if(var != "mfirefrac"){
          this.TA.dt <- getTADT(run, period, var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
        }
        else {
          file.string = file.path(run@run.dir, paste(var, ".out", sep=""))
          if(file.exists(file.string) | file.exists(paste(file.string, "gz", sep = "."))){ 
            this.TA.dt <- getTADT(run, period, var, this.full = this.full, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
          }
          else {
            this.TA.dt <- getTADT(run, period,  var = "firert", this.full = NULL, write = TRUE, forceReAveraging = forceReAveraging, verbose = verbose)
            var = "firert"
          }
        }     
        
        
        ##### PLOT TIME AVERAGED DATA #####
        
        # get the VegQuant corresponding to this variable to provide some metadata
        this.Quant <- getVegQuantity(var)
        
        # plot the per PFT summary and individual PFT plots
        plotLPJMaps(this.TA.dt, 
                    run = run,
                    quant = this.Quant, 
                    period = period, 
                    doSummary = TRUE, 
                    doIndividual = doIndividual | var %in% detailed.var.list, 
                    useLongnames = FALSE, 
                    maxpixels = 100000)
        
        
        
        ### If detailed output requested  
        if(var %in% detailed.var.list){
          
          if(this.Quant@type == "PFT"){
            
            addLifeformTotals(this.TA.dt,global.PFTs)
            
            # plot the per lifeform summary and individual lifeform plots
            plotLPJMaps(this.TA.dt,
                        which.layers = c("Tree", "Grass", "Shrub", "Total"),
                        run = run,
                        quant = this.Quant, 
                        period = period, 
                        doSummary = TRUE, 
                        doIndividual = TRUE,
                        special.string = "Lifeform",
                        useLongnames = FALSE, 
                        maxpixels = 100000)
            
            if(var %in% fraction.var.list){
              
              # Add PFT and lifeform fraction
              addVegFractions(this.TA.dt, global.PFTs, targets =  c("pfts", "lifeforms"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE)
              
              # Get list of fractions to plot
              fraction.list <- names(this.TA.dt)[grep("Fraction",names(this.TA.dt) )]
              
              # Make a temporary "fraction" variable for plotting
              fraction.var <- getVegQuantity("fraction")
              fraction.var@id <- paste(var, fraction.var@id, sep = ".")
              fraction.var@short.string <- paste(var, fraction.var@short.string, sep = ".")
              fraction.var@full.string <- paste(var, fraction.var@full.string, sep = " ")
              
              plotLPJMaps(this.TA.dt,
                          which.layers =fraction.list,
                          run = run,
                          quant = fraction.var, 
                          period = period, 
                          doSummary = TRUE, 
                          doIndividual = TRUE,
                          special.string = "Fraction",
                          useLongnames = FALSE, 
                          maxpixels = 100000)
            }
            
            
          } 
          else if(this.Quant@type == "monthly"){
            
            addSeasonal(this.TA.dt, method = "total")
            
            # plot the per lifeform summary and individual lifeform plots
            plotLPJMaps(this.TA.dt,
                        which.layers = c("DJF", "MAM", "JJA", "SON", "Annual"),
                        run = run,
                        quant = this.Quant, 
                        period = period, 
                        doSummary = TRUE, 
                        doIndividual = TRUE,
                        special.string = "Seasonal",
                        useLongnames = FALSE, 
                        maxpixels = 100000)
          }
          
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
        
        # Dominant PFT
        if(doDominantPFT & var == "lai"){
          if(verbose) message("Doing Dominant PFT Classification")
          
          combineShadeTolerance(this.TA.dt,global.PFTs)
          addDominantPFT(this.TA.dt, global.PFTs, do.all = TRUE, do.tree = TRUE, do.woody = TRUE)
          
          plotDominantPFTMap(this.TA.dt,
                             which.dominant = "Dominant",
                             run = run, 
                             period = period)
          
          plotDominantPFTMap(this.TA.dt,
                             which.dominant = "DominantTree",
                             run = run, 
                             period = period)
          
          plotDominantPFTMap(this.TA.dt,
                             which.dominant = "DominantWoody",
                             run = run,
                             period = period)
          
          ### Here write the table, maybe can be used in another script or plotting program
          write.table(this.TA.dt[, list(Lon, Lat, Dominant, DominantTree, DominantWoody)], file = paste0("Dominants.", period@start, "-", period@end, ".Rtable"), row.names= FALSE, quote= FALSE)
          
        }
        
        
        # Here save the litters raster for calculating total fine litter (fuel)
        if(doFineFuel & var == "litter_leaf") Leaf.Litter.raster <- promoteToRaster(this.TA.dt, "Total")     
        if(doFineFuel & var == "litter_repr") Repr.Litter.raster <- promoteToRaster(this.TA.dt, "Total")  
        if(doFineFuel & var == "litter_wood") Wood.Litter.raster <- promoteToRaster(this.TA.dt, "Total")     
        
        # clean up temporal averages to save memory
        rm(this.TA.dt)
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
        cmass.local <- getVegQuantity("cmass")
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
        cmass.local <- getVegQuantity("cmass")
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
        cmass.local <- getVegQuantity("cmass")
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
                                                                  quant = getVegQuantity("vegcover"),
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
                                                              quant = getVegQuantity("burntarea"),
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
        
        rm(this.TA.dt, model.raster, model.firert, data.firert, combined.firert)	
        
      }
      
    }
    
    
    
    
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
  
  
  # Calculated fine fuel after retrived all litter types...
  if(doFineFuel){
    Fine.Fuel.raster <- Leaf.Litter.raster + Repr.Litter.raster + 0.045 * Wood.Litter.raster
    names(Fine.Fuel.raster) <- run@id
    Fine.Fuel.stack <- addLayer(Fine.Fuel.stack, Fine.Fuel.raster)
    plotLPJMaps(Fine.Fuel.raster, 
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
    
    
    
    
    
    stop()
    
  }
  
  
  
  
}

rm(Treecover.results, GFED4.results, Biome.stack)
if(doBaccini2012) rm(Baccini.dataset, Baccini.results)
if(doSaatchi2011) rm(Saatchi.dataset, Saatchi.results)


# terminate with a happy message
message("Processing finished gracefully :-)")


t2 <- Sys.time()
print(t2-t1)


