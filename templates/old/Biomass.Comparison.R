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

#####################################################################################
### BIOMASS COMPARISON PROCESSING SCRIPT
###
### This is a template script to process a single run using the RVCTools v2.0
### It can:
###         1) Time average the run
###         2) Plot the summary variables
###         3) Compare a couple of benchmarks
###         4) Compares one run to another
###
#####################################################################################


#####################################################################################
############################## VARIOUS PREAMBLE #####################################
#####################################################################################

# start time for benchmarking
start.time <- Sys.time()

# read in the library
source("~/Tools/RVCTools/v2.1/rvc-tools.R")



#####################################################################################
########################### READ THE LPJ-GUESS RUNS #################################
#####################################################################################


### create of list to store all the runs we want to process
list.of.runs <- list()

### create a VegRun object to store one run
run.id <- "B1_WC_default_Saatchi_091213"
list.of.runs[[run.id]]<- openVegRun(run.dir = "/data/forrest/GuessRuns/SPITFIRE/v0.4/Allan-2013-12-05/B1_WC_default_Saatchi_091213",
                       id = run.id,
                       description = "Feldpausch/Dantas allometry",
                       driving.data = "PGF",
                       map.overlay = "countries",
                       london.centre = TRUE,
                       lonlat.offset = c(0.25,0.25),
                       year.offset = 0
)

### create a VegRun object to store another run
run.id <- "B2_WC_default_Saatchi_091213"
list.of.runs[[run.id]] <- openVegRun(run.dir = "/data/forrest/GuessRuns/SPITFIRE/v0.4/Allan-2013-12-05/B2_WC_default_Saatchi_091213",
                                   id = run.id,
                                   description = "Feldpausch/Moncrieff allometry",
                                   driving.data = "PGF",
                                   map.overlay = "countries",
                                   london.centre = TRUE,
                                   lonlat.offset = c(0.25,0.25),
                                   year.offset = 0
)

### and so on for more runs...


# which variables to process? cmass!
variable = "cmass"

# do we want to splurge out lots of output?
verbose = TRUE


for(run in list.of.runs){
  
  ### open the .out file read it as a data frame
  this.Full <- openLPJOutputFile(run@run.dir, variable)
  
  ### create a VegVarTA object (means Vegatation Variable, Time Averaged) and does the following
  ### * time averages the full output file
  ### * save the time-averaged data to disk (if requested)
  ### * plots a summary of the time-averaged data (if requested)
  ### * return the time averaged data frame as a VegVarTA object
  this.VegVarTA <- makeVegVarTA(input.df = this.Full, 
                                run = run, 
                                variable = variable, 
                                id = variable, 
                                first.year = 1998, 
                                last.year = 2007, 
                                save = TRUE, 
                                save.dir = run@run.dir, 
                                plot = TRUE,
                                plot.dir = run@run.dir,
                                verbose = verbose)
  
  ### remove the dataframe of the full file to free up memory
  rm(this.Full)
  gc()
  
  ### get the PFTs present
  PFTs <- getAllPFTs(list(this.VegVarTA), global.PFTs, verbose)
  
  ### calculate total tree biomass
  this.VegVarTA@data@data$TreeTotal <- apply(this.VegVarTA@data@data, 1, FUN = getLifeformTotal, PFTs, "Tree")
      
  ### add this variable to the run
  list.of.runs[[run@id]] <- addVegVar(list.of.runs[[run@id]], this.VegVarTA)
    
}



#####################################################################################
######### READ THE BENCHMARKING DATA AND CROP IT ALL TO THE SAME EXTENTS ############
#####################################################################################

### Open the Saatchi data and store it in a raster object
saatchi.biomass.raster <- readSaatchi2011()

### Open the Baccini data and store it in a raster object
baccini.biomass.raster <- readBaccini2012()

### Open the Globcover2009 natural landcover fraction for correcting the model output
gc2009.natural.raster <- readNaturalLandCoverGC2009()

###  Define the Saatchi extent, this is what we want to plots over and is the area where Saatchi et al. report their data
saatchi.extent <- extent(-116.25, 153.25, -44.25, 39.75)

### put runs on same geographical extent
saatchi.biomass.raster <- crop(saatchi.biomass.raster, saatchi.extent)
gc2009.natural.raster <- crop(gc2009.natural.raster, saatchi.extent)


### Baccini must be expanded and cropped because it is on a smaller extent
baccini.biomass.raster <- extend(baccini.biomass.raster, saatchi.extent)
baccini.biomass.raster <- crop(baccini.biomass.raster, saatchi.extent)






#####################################################################################
########## MAKE A LIST OF FINAL WOODY BIOMASS RASTERS TO COMPARE TO DATA ############
########## ALSO: * correct the model using landcover data                ############
##########       * crop the model output to match the Saatchi grid       ############
#####################################################################################

### make list of total woody biomass with and without land cover correction
list.of.tree.biomass <- list()
list.of.evergreen.biomass <- list()
list.of.raingreen.biomass <- list()


for(run in list.of.runs){
  
  ### retrieve the cmass VegVarTA
  cmass.VegVarTA <- run@vars[[variable]]
  
  ### make full SpatialGridDataFrame
  cmass.SpatialGridDataFrame  = as(cmass.VegVarTA@data, "SpatialGridDataFrame") # to full grid

  ### rasterise and then select the total tree biomass only
  cmass.raster <- brick(cmass.SpatialGridDataFrame)
  cmass.raster.total <- subset(cmass.raster, "TreeTotal")
  cmass.raster.raingreen <- subset(cmass.raster, "TrBR")
  cmass.raster.evergreen <- subset(cmass.raster, "TrBE") + subset(cmass.raster, "TrIBE")
  
  ### match extent to Saatchi extent
  cmass.raster.total <- crop(cmass.raster.total, saatchi.extent)
  cmass.raster.raingreen <- crop(cmass.raster.raingreen, saatchi.extent)
  cmass.raster.evergreen <- crop(cmass.raster.evergreen, saatchi.extent)
    
  ### save into the list of biomasses that we defined above
  list.of.tree.biomass[[run@id]] <- cmass.raster.total * gc2009.natural.raster# put land cover raster here  
  list.of.raingreen.biomass[[run@id]] <- cmass.raster.raingreen * gc2009.natural.raster # put land cover raster here  
  list.of.evergreen.biomass[[run@id]] <- cmass.raster.evergreen * gc2009.natural.raster # put land cover raster here  
  
  

}




#####################################################################################
############################### PLOTTING TIME!! #####################################
#####################################################################################

### make the coastlines object to plot
coastlines <- makeCoastlines("countries")
layout.list <- list(coastlines)


############################### SAATCHI ########################################


### Loop over runs and plots the Saatchi maps
pdf("Saatchi.Maps.pdf")
for(run in list.of.runs){
  
  ### get modelled biomass, make the difference and plot
  diff.raster <- list.of.tree.biomass[[run@id]] - saatchi.biomass.raster
  #minmax <- max(maxValue(diff.raster, abs(minValue(diff.raster))))
  title <- paste("Total biomass: ", run@id, " - Saatchi (kgC/m^2)", sep = "")
  print(spplot(diff.raster, scales = list(draw = TRUE), col.regions=lpj.quantities[["cmass.diff"]]@colours, at=seq(-21,21,2), sp.layout = layout.list, main = title))
  
}
dev.off()


### Loop over runs and do the scatter and R^2 vs Saatchi
pdf("Saatchi.Scatter.pdf")
for(run in list.of.runs){
  
  ### get modelled biomass
  modelled.biomass.raster <-list.of.tree.biomass[[run@id]]
  
  plot(modelled.biomass.raster, saatchi.biomass.raster, ylim = c(0,40), xlim = c(0,40), ylab = "Saatchi Biomass (kgC/m^2)", xlab = paste("LPJ-GUESS Biomass (kgC/m^2): ", run@description, sep = ""), cex.lab = 1.3, cex.axis = 1.3, main = paste("Saatchi vs ", run@description, sep = ""), cex.main = 1.6)
  abline(a= 0,b = 1, col = "red")
  modelled <- as.vector(modelled.biomass.raster)
  observed <- as.vector(saatchi.biomass.raster)
  
  #calculate RMSE, R^2 and put on plot
  diff <- (observed - modelled)^2
  MSE <- sum(diff, na.rm = TRUE)
  nObs <- sum(!is.nan(diff))
  MSE <- MSE/nObs
  RMSE <- (MSE)^(0.5)
  R.squ <- 1 - (MSE/var(observed, na.rm = T))
  legend('top', c(paste("RMSE:", round(RMSE, 2), "kgC/m^2", sep = " "), paste("R^2:", round(R.squ, 2), sep = " ")), text.col = c("blue", "green"), cex = 2.5, bty = "n") 

 
}
dev.off()



############################## BACCINI ##############################################


### Loop over runs and plots the Baccini maps
pdf("Baccini.Maps.pdf")
for(run in list.of.runs){
  
  ### get modelled biomass, calulate the difference and plot
  diff.raster <- list.of.tree.biomass[[run@id]] - baccini.biomass.raster
  title <- paste("Total biomass: ", run@id, " - Baccini (kgC/m^2)", sep = "")
  print(spplot(diff.raster, scales = list(draw = TRUE), col.regions=lpj.quantities[["cmass.diff"]]@colours, at=seq(-21,21,2), sp.layout = layout.list, main = title))
  
}
dev.off()


### Loop over runs and do the scatter and R^2 vs Baccini
pdf("Baccini.Scatter.pdf")
for(run in list.of.runs){
  
  ### get modelled biomass
  modelled.biomass.raster <-list.of.tree.biomass[[run@id]]
  
  plot(modelled.biomass.raster, baccini.biomass.raster, ylim = c(0,40), xlim = c(0,40), ylab = "Baccini Biomass (kgC/m^2)", xlab = paste("LPJ-GUESS Biomass (kgC/m^2): ", run@description, sep = ""), cex.lab = 1.3, cex.axis = 1.3, main = paste("Baccini vs ", run@description, sep = ""), cex.main = 1.6)
  abline(a= 0,b = 1, col = "red")
  modelled <- as.vector(modelled.biomass.raster)
  observed <- as.vector(baccini.biomass.raster)
  
  #calculate RMSE, R^2 and put on plot
  diff <- (observed - modelled)^2
  MSE <- sum(diff, na.rm = TRUE)
  nObs <- sum(!is.nan(diff))
  MSE <- MSE/nObs
  RMSE <- (MSE)^(0.5)
  R.squ <- 1 - (MSE/var(observed, na.rm = T))
  legend('top', c(paste("RMSE:", round(RMSE, 2), "kgC/m^2", sep = " "), paste("R^2:", round(R.squ, 2), sep = " ")), text.col = c("blue", "green"), cex = 2.5, bty = "n") 
  
}

dev.off()

######################### COMPARE RUNS ##############################################

run.diff <- list.of.tree.biomass[["B1_WC_default_Saatchi_091213"]] - list.of.tree.biomass[["B2_WC_default_Saatchi_091213"]]



pdf("Run.Diff.Maps.pdf")
  title <- "B1 - B2"
  print(spplot(run.diff, main = title, scales = list(draw = TRUE), col.regions=lpj.quantities[["cmass.diff"]]@colours, at=seq(-21,21,2), sp.layout = layout.list))
dev.off()

run.diff.raingreen <- list.of.raingreen.biomass[["B1_WC_default_Saatchi_091213"]] - list.of.raingreen.biomass[["B2_WC_default_Saatchi_091213"]]

pdf("Run.Diff.Raingreen.Maps.pdf")
title <- "B1 - B2 raingreen"
print(spplot(run.diff.raingreen, main = title, scales = list(draw = TRUE), col.regions=lpj.quantities[["cmass.diff"]]@colours, sp.layout = layout.list, at=seq(-21,21,2)))
dev.off()







# For timing
end.time <- Sys.time()
time.elapsed <- end.time-start.time
print(time.elapsed)

stop()