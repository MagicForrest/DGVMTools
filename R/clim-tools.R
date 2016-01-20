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
### Version: v2.0 - still under development
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



####### SOURCE ALL THE FILES TO LOAD ALL THE FUNCTIONS AND OBJECTS



        



