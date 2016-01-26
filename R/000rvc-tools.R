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

###
### Matt Forrest 2016-01-26
### Need a new way to handle this stuff with file paths etc




###############################################################################
### SPECIFY SYSTEM DEPENDENT PATHS - CHANGE THESE FOR YOUR SYSTEM
################################################################################


####### SPECIFY LOCATION OF AUXILIARY DATA HERE 
auxiliary.data.dir <- "/home/forrest/AuxiliaryData" # NOTE: must be the full path or OGR gets confused :-(


###############################################################################
### BUILD COMPOUND PATHS TO SHAPEFILES, GRIDLISTS, BENCHMARKING DATA ETC.
###############################################################################


####### DIRECTORIES FOR ALL BENCHMARKING DATA, SHAPEFILES, GRIDLISTS
benchmarking.data.dir <- file.path(auxiliary.data.dir, "BenchmarkingData")
shapefile.dir <- file.path(auxiliary.data.dir, "Shapefiles") # NOTE: must be the full path or OGR gets confused :-(
gridlist.dir <- file.path(auxiliary.data.dir, "Gridlists/")


######
#gfed4.annual <-  file.path(benchmarking.data.dir, "GFED4/GFED4.0_HD_Annual_BA.nc")
#saatchi2011.original.data.path <- ("/senckenberg.de/DATEN_PBE/PB-E/PBE-ALLG/Datasets/Biomass/2011_Saatchi/www-radar.jpl.nasa.gov/projects/carbon/datasets")



