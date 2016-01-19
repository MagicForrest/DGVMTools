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

######################################


###########################################################################
###
### RCVTool - lpj-tools package
###
### This is a 'meta-package' bringing together a collection 
### of plotting tools and classes for either standard output
### from LPJ-GUESS or from files already averaged over the target period
###
### Author: Matthew Forrest 
### Date: 04/09/2013
### Version: v2.5 - still under development
###
### * Takes a flexible number of text files ('runs') of LAI/FPC/Biomass etc
### and plots the variable by PFT, total tree, grass and shrub LAI and dominant PFT by LAI.
### * When getting dominant PFT it combines shade-tolerant and shade-intolerant
### PFTs.
### * Can deal with different runs having different PFTs.
### * Has a reasonably nice colour scheme for standard PFTs.
### * Heavily utilises the 'sp' package for much of the heavy lifting and classes
###
###########################################################################


####### INCLUDE SOME NECESSARY LIBRARIES #####################
# these should all be available from CRAN with no problems
library(ncdf4)
library(sp)
library(raster)
library(rgdal)
library(lattice)
library(fields) # for tim.colors
library(data.table)
library(xts)
library(foreach)
library(doParallel)
library(rgeos)  # only used for cropping Spatial Lines at the moment, can easily be hacked out if required
library(compiler)
library(ggplot2)


###############################################################################
### SPECIFY SYSTEM DEPENDENT PATHS - CHANGE THESE FOR YOUR SYSTEM
### Normally only base.dir and auxiliary.data.dir are essential, the others are built on this 
################################################################################

####### SPECIFY BASE DIRECTORY OF PACKAGE HERE 
base.dir <- "~/Tools/RVCTools/v2.9"

####### SPECIFY LOCATION OF AUXILIARY DATA HERE 
auxiliary.data.dir <- "/home/forrest/AuxiliaryData" # NOTE: must be the full path or OGR gets confused :-(


###############################################################################
### BUILD COMPOUND PATHS TO SHAPEFILES, GRIDLISTS, BENCHMARKING DATA ETC.
### Normally no need to do anything here
###############################################################################


####### DIRECTORIES FOR ALL BENCHMARKING DATA, SHAPEFILES, GRIDLISTS
benchmarking.data.dir <- file.path(auxiliary.data.dir, "BenchmarkingData")
shapefile.dir <- file.path(auxiliary.data.dir, "Shapefiles") # NOTE: must be the full path or OGR gets confused :-(
gridlist.dir <- file.path(auxiliary.data.dir, "Gridlists/")


######
gfed4.annual <-  file.path(benchmarking.data.dir, "GFED4/GFED4.0_HD_Annual_BA.nc")
saatchi2011.original.data.path <- ("/senckenberg.de/DATEN_PBE/PB-E/PBE-ALLG/Datasets/Biomass/2011_Saatchi/www-radar.jpl.nasa.gov/projects/carbon/datasets")




###############################################################################
### LOAD ALL FUNCTIONS IN THE LIBRARY 
### Normally no need to do anything here
###############################################################################


####### SOURCE ALL THE FILES TO LOAD ALL THE FUNCTIONS AND OBJECTS
source(file.path(base.dir, "rvc-classes.R"))
source(file.path(base.dir, "rvc-misc.R"))
source(file.path(base.dir, "rvc-plotting.R"))


####### SOURCE ALL THE FILES TO LOAD ALL THE FUNCTIONS FOR HANDLING LPJ RUNS
source(file.path(base.dir, "lpj-runtools.R"))
source(file.path(base.dir, "lpj-processingtools.R"))
source(file.path(base.dir, "lpj-plotting.R"))
source(file.path(base.dir, "lpj-quantities.R"))
source(file.path(base.dir, "lpj-globalPFTs.R"))
source(file.path(base.dir, "lpj-euroPFTs.R"))


####### SOURCE ALL THE FILES TO LOAD ALL THE FUNCTIONS FOR HANDLING CLIMATE DATA
source(file.path(base.dir, "clim-tools.R"))


####### SOURCE BENCHMARKS ###################################
source(file.path(base.dir, "datasets", "Saatchi2011.R"))  
source(file.path(base.dir, "datasets", "Baccini2012.R"))   
source(file.path(base.dir, "datasets", "Avitabile2015.R"))      
source(file.path(base.dir, "datasets", "Globcover.R"))   
source(file.path(base.dir, "datasets", "PNVBiomes.R"))
source(file.path(base.dir, "datasets", "GFED4.R"))      
source(file.path(base.dir, "datasets", "MODISTreecover.R"))      



message("+++ Loaded RVCTools +++")
