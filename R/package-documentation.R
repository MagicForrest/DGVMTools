#' @title  An overview of the DGVMTools package
#' 
#' @name DGVMTools-package
#' @docType package
#' 
#' @description This package is designed for reading, processing and plotting output from Dynamic Vegetation Models (DGVMs), land surface models from climate models and other 
#' spatial representions of the terrestrial biosphere or land surface.  There are many such models and each have their own output format.  
#' This package gives a framework for reading the different outputs from these models and putting them into a common internal representation.  Once this is done, 
#' it provides many tools for analysing the model results and comparing them to data (and each other).  These include common tasks such as:
#' 
#' \itemize{
#'  \item Subsetting and averaging certain time periods or spatial extents,
#'  \item Calculating total values for certain seasons, growth forms (for example all trees), leaf phenologies (for example all evergreen vegetation), climate zones (for example all tropical vegetation), and others,
#'  \item Making maps and time series plots of the model output,
#'  \item Reading in observed data (satellite data, flux tower data etc.) to which model results can be compared,
#'  \item Calculating statistical metrics to quantify the agreement between models and data, and producing plots (scatter plots, histograms of residuals) to visually compare models and data,
#'  \item Making classifications (such as biomes, vegetation types, pyromes, etc) from the model output and plotting them,
#'  \item Making difference maps to evaluate the effects of a model change or spatially comparing models to data,
#'  \item Storing averaged model data on disk for fast re-reading,
#'  \item Saving model data in standard netCDF format.
#' }
#' 
#' 
#' 
#' @details There is obviously some overlap in functionality with the terra, sf and stars R packages.  Indeed this package builds uses some functions from terra and sf, and exports its native Fields objects
#' to terra::SpatRast objects.  The advantages of DGVMTools over these other packages for analysing DGVM output are two-fold. 
#' Firstly, it is tailored for a typical DGVM analysis workflow.  Concepts like 'data sources' and 'plant functional types' 
#' are explicit objects with their own meta-data (if you don't know what these concepts are then this package probably isn't for you).  
#' Once these objects are correctly defined (which is not difficult),  analysis is very convenient.  
#' Many common tasks are already coded efficiently into functions, and because of the meta-data attached to the objects,
#' These functions can do a lot of 'sensible and standard'stuff without too much direction from the user.
#' Secondly, the data is stored internally as a data.table (as opposed to a data.frame).  
#' The advantage of this is that data.tables are very, very much faster than data.frames for many operations (check out the data.table package 
#' documentation and webpage for more info).  This is obviously a great advantage when working with very large spatial-temporal datasets. 
#' It should be noted that this advantage was very important compared to the now outdated raster package.  The replacement for raster (terra) is faster so this 
#' advantage is not as large.
#' 
#' 
#' Furthermore, the objects defined in DGVMTools can be very easily  converted into terra objects or data.frames, and so can fit directly back
#' into any existing R code, packages or other machinery.  Thus the transition to using DGVMTools is very smooth.  DGVMTools can be used to read the 
#' data and perform standard operations, but then data can be converted to a raster or data.frame for more specific or idiosyncratic R scripts or 
#' functions. However, DGVMTools is intended to be a reasonably complete analysis environment in itself.  It should be possible to go from model output
#'all the way to results and publication quality plots using only DGVMTools and some base R functionality for other tasks.  
#' 

NULL