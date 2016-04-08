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

######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table in order Lon, Lat, Year (if present)
#' 
#' Keys should be set on all data.table object for sorts, joins, RVCTool-defined operators etc.  
#'  This function should be called on a data.table stored in a VegObject after it has been created,
#'  including if it was created by avergaing another data.table because it seems as keys are not conserved.
#'
#' @param dt The data.table for which to set the key
#' @return Returns nothing because changes the original data.table by reference (this is the data.table way)
#' @import data.table
setKeyRVC <- function(dt){
  
  keys.present <- c()
  
  if("Lon" %in% names(dt)) keys.present <- append(keys.present, "Lon")
  if("Lat" %in% names(dt)) keys.present <- append(keys.present, "Lat")
  if("Year" %in% names(dt)) keys.present <- append(keys.present, "Year")
  
  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  
}


######## DEFINE A VEGRUN OBJECT 

#' Define a VegRun object, setting up all the required metadata (only). 
#' 
#' This function is preferred to a simple "new("VegRunInfo")" and "new("VegRunInfo") initialisation because it does both intialisations in one step and also performs some extra check and preamble such as prepared the map overlays
#'
#' Note that actual data from the run is not stored, only metadata. The data, stored as VegObjects and added to a VegRun object, are added built with later with other commands and added to \code{VegRun} command using \code{addToVegRun}
#'
#' @param ...  The parameters are the slots of a VegRunInfo object. Note that that \code{id} and \code{run.dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' compulsory.  Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS confgurations.
#' @return A VegRun object, with metadata defined by empty data slots.s
#' @export
#' @seealso VegRun, VegRunInfo 

defineVegRun <- function(...){
  
  # make a VegRunInfo object from the supplied meta data
  info <- new("VegRunInfo", ...)
  
  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(info@pft.set) == 0) info@pft.set <- NULL
  if(length(info@tolerance) == 0)  info@tolerance <- 0.0000001
  if(length(info@description) == 0)  info@description <- "No description specified"
  if(length(info@map.overlay) == 0 | info@map.overlay[1] == "")  info@map.overlay <- NULL
  if(length(info@lonlat.offset) == 0)  info@tlonlat.offset <- c(0,0)
  if(length(info@year.offset) == 0)  info@year.offset <- 0
  if(length(info@tolerance) == 0)  info@tolerance <- 0.00001
  if(length(info@driving.data) == 0)  info@driving.data <- "No forcing data set"
  if(length(info@london.centre) == 0)  info@london.centre <- TRUE
  if(length(info@fill.col) == 0)  info@fill.col <- "transparent"
  if(length(info@line.col) == 0)  info@line.col <- "green"
  if(length(info@line.width) == 0)  info@line.width <- 1
  if(length(info@line.type) == 0)  info@line.type <- 1
  if(length(info@landuseSimulated) == 0)  info@landuseSimulated <- FALSE
  
  # lookup map over from maps and mapdata package
  if(!is.null(info@map.overlay)) {
    
    # if we get a single characters string, assume it is a map and just a simple map from it
    if(length(info@map.overlay) == 1 & class(info@map.overlay)[1] == "character") {
       info@map.overlay <- makeOverlay(info@map.overlay)
    }
    # else if is a list 
    else if(class(info@map.overlay)[1] == "list"){
      
      # check if it is actually a list of lists 
      list.of.lists <- FALSE
      for(test in info@map.overlay) {
        if(class(test[1]) == "list") list.of.lists <- TRUE
      }
      
      # if it is a simple list, then assumer it describes a single overlay and build a single overlay
      if(!list.of.lists){
        info@map.overlay <- do.call(makeOverlay, info@map.overlay)
      }
      # else assume each item is an overlay and try to make them all
      else {
        temp <- list()
        counter <- 0
        for(overlay in info@map.overlay){
          counter <- counter +1
          if(class(overlay[1]) == "character") temp[[counter]] <- makeOverlay(overlay)
          else if(class(overlay[1]) == "list") temp[[counter]] <- do.call(makeOverlay, overlay)
          else warning("Something funny specified in the overlays (ie not a character or a list), ignoring it.  You might want to check this.")
        }
        info@map.overlay <- temp
      }
      
      
    }
    
  } 
  
  # return a VegRun object with empty data fields but meta data filled  
  return(new("VegRun",
             info))
  
  
}


############# ADD AN OBJECT TO A VEGRUN ######################################

#' Add an object (either a \code{VegObject}, \code{BiomeComparison} or \code{RasterComparison}) to a \code{VegRun} object to be use later.  
#' 
#' Stores an object in its run for later calculations, plotting, comparisons.
#' 
#' @param object Object to add to the \code{VegRun},   The parameters are the slots of a VegRunInfo object. Note that that \code{id} and \code{run.dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' compulsory.  Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS confgurations.
#' @param run The \code{VegRun} object to which the object argument shold be added
#' @return A VegRun object the the object argument added
#' @export
#' @seealso VegRun, VegRunInfo 
addToVegRun <- function(object, run){
  
  # Add a BiomeComaprison or RasterComparison to the list in the benchmarks slot 
  if(is.BiomeComparison(object) | is.RasterComparison(object)) {
    
    benchmark.list <- run@benchmarks
    benchmark.list[[object@id]] <- object
    run@benchmarks <- benchmark.list
    rm(benchmark.list)
    
  }
  
  # Add a VegObject to the list in the objects slot 
  else if(is.VegObject(object)) {
    
    # Check that run ids match, if not, stop becuase something is really wrong
    if(run@id != object@run@id){
      stop(paste("Adding VegObject ", object@id, " which comes from run with id = ",  object@run@id, " to run with id = ", run@id, ". I can think of no reason to do this, and doing so will break the internal logic of RVCTools so aborting. Contact the package creator if this seems wrng to you" , sep = ""))
    }
    
    veg.objects.list <- run@objects
    veg.objects.list[[object@id]] <- object
    run@objects <- veg.objects.list
    rm(veg.objects.list)
    
  }
  
  else{
    
    warning(paste("Cannot add object of class", class(object), "to a VegRun object", sep = " "))
    
  }
  
  return(run)
  
}


################################# GET TEMPORALLY-AVERAGED DATA #########################################

#' Get a temporally-averaged \code{VegObject}
#' 
#' Given a \code{VegRun} object, a \code{VegQuant} object and a \code{TemporalExtent} object, return an appropriate temporally-averaged \code{VegObject} oject for that run, quantity and time period over the whole simulation domain.
#' 
#' @param run The \code{VegRun} object for which the temporally-averaged \code{VegObject} should be built (eg. "lai")
#' @param var The quantity (either a \code{VegQuant} or a string containing its \code{id}) 
#' @param period The time period (as a \code{TemporalExtent} over which the data is to be averaged)
#' @param ... additional arguments passed to \code{getVegObject}, for example \code{rereadfile}, and \code{store.internally}.
#' @return A temporally averaged\code{VegObject}  but with no-spatial averaging or cropping (ie. include complete original simulation domain). In other words, a map!
#' @export
#' @seealso \code{getVegObject}, \code{getVegTemporal}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'  
getVegSpatial <- function(run,
                          var,
                          period,
                          ...){
  
  
  return(
    getVegObject(run, 
                 var, 
                 spatial.extent = NULL, 
                 spatially.average = FALSE,
                 temporal.extent = period, 
                 temporally.average = TRUE, 
                 ...)
  )
  
  
}


################################# GET SPATIALLY-AVERAGED DATA #########################################


#' Get a spatially-averaged \code{VegObject}
#' 
#' Given a \code{VegRun} object, a \code{VegQuant} object and a \code{SpatialExtent} object, return an appropriate spatially-averaged \code{VegObject} oject for that run, quantity and time period over the whole simulation domain.
#' 
#' @param run The \code{VegRun} object for which the spatially-averaged \code{VegObject} should be built (eg. "lai")
#' @param var The quantity (either a \code{VegQuant} or a string containing its \code{id}) 
#' @param spatial.extent The spatial extent (as a \code{SpatialExtent} object over which the data is to be averaged)
#' @param ... additional arguments passed to \code{getVegObject}, for example \code{rereadfile}, \code{area.weighted} and \code{store.internally}.
#' @return A spatially averaged\code{VegObject} but with no temporal averaging or cropping (ie. include complete original simulation duration). In other words, a time-series!
#' @export
#' @seealso \code{getVegObject}, \code{getVegspatial}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

getVegTemporal <- function(run,
                           var,
                           spatial.extent = NULL,
                           ...){
  
  return(
    getVegObject(run, 
                 var, 
                 spatial.extent = spatial.extent, 
                 spatially.average = TRUE,
                 temporal.extent = NULL, 
                 temporally.average = FALSE, 
                 ...)
  )
  
  
}



################################# GET VEGOBJECT - Does a lot! #########################################
#
#' Get a \code{VegObject}, optionally for spatial/temporal averaging/cropping 
#' 
#' Given a \code{VegRun} object a \code{VegQuant} object, return an appropriate spatially-averaged \code{VegObject} oject for that run and quantity. Arguments can also be provided for averaging over different spatial or temporal extents (very useful) or optionall just cropping to those extents
#' 
#' Note that because there are two types of averaging available, the resulting \code{VegRun} obhect can either be full spatial-temporal dataset, a spatial-only dataset (map), a temporal only datasey a time-series) or an average across both space and time, i.e. a single number.
#' Also not that the data is stored internal as a data.table object, but this is mostly not important to the user.
#'   
#' @param run The \code{VegRun} object for which the spatially-averaged \code{VegObject} should be built (eg. "lai")
#' @param var The quantity (either a \code{VegQuant} or a string containing its \code{id}) 
#' @param temporal.extent The temporal extent (as a \code{TemporalExtent} object over which the data is to be averaged)
#' @param temporally.average Whether or not to temporally average (logical)
#' @param spatial.extent The spatial extent (as a \code{SpatialExtent} object over which the data is to be averaged)
#' @param spatially.average Whether or not to spatially average (logical)
#' @param reread.file If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param area.weighted If TRUE, perform weighting by gridcell area when doing spatial averaging
#' @param write If TRUE, write the data of the \code{VegObject} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{VegObject} in the \code{VegRun} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{VegObjects} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM run, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A spatially averaged\code{VegObject} but with no temporal averaging or cropping (ie. include complete original simulation duration). In other words, a time-series!
#' @export
#' @import data.table raster
#' @seealso \code{getVegObject}, \code{getVegspatial}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getVegObject <- function(run, 
                         var, 
                         temporal.extent = NULL, 
                         temporally.average = FALSE, 
                         spatial.extent = NULL, 
                         spatially.average = FALSE,
                         area.weighted=TRUE,
                         write = FALSE, 
                         reread.file = TRUE, 
                         verbose = TRUE, 
                         store.internally = FALSE,
                         store.full = FALSE,
                         adgvm.scheme = 1){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  
  ### CONVERT STRING TO VEGQUANT
  if(class(var) == "character") {
    quant <- lookupVegQuantity(var)
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  ### MAKE UNIQUE IDENTIFIER OF THIS VEGOBJECT VARIABLE - this describes completely whether we want the files spatially or temporally averaged and reduced in extent
  TA.str = SA.str = "."
  if(spatially.average) SA.str <- ".SA."
  if(temporally.average) TA.str <- ".TA."
  if(is.null(spatial.extent) & !is.null(temporal.extent)) vegobject.id <- paste(var.string, TA.str, paste(temporal.extent@start, temporal.extent@end, sep = "-"), sep ="")
  else if(!is.null(spatial.extent) & is.null(temporal.extent)) vegobject.id <- paste(var.string, SA.str, spatial.extent@id, sep ="")
  else if(!is.null(spatial.extent) & !is.null(temporal.extent)) vegobject.id <- paste(var.string, SA.str, spatial.extent@id, TA.str,  paste(temporal.extent@start, temporal.extent@end, sep = "-"), sep ="")
  else  vegobject.id <- var.string
  file.name <- file.path(run@run.dir, paste(vegobject.id, "RVCData", sep = "."))
  
  
  
  ### CASE 1 - USE THE EXACT VEGOBJECT IF IT HAS ALREADY BEEN COMPUTED AND SAVED IN THE VEGRUN IN MEMORY
  if(vegobject.id %in% names(run@objects)){
    
    # if it is present it in memory then can be returned directly
    if(verbose) message(paste("Exact VegObject (with id = ", vegobject.id, ") already found in memory for this VegRun, so using that.", sep = ""))
    return(run@objects[[vegobject.id]])
    
  }
  
  
  
  ### CASE 2 - USE THE PREAVERAGED/CROPPED VEGOBJECT FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !reread.file){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  run@run.dir, "(and reread.file not selected) so reading it from disk and using that.",  sep = " "))}
    vegobject <- readRDS(file.name)
    
    # Update the run object, that might have (legitimately) changed compared to the id that was used when this veg object was created
    # for example ti might be assigned a new id, or new map overlays or whatever
    vegobject@run <- run
    
    
    # Check that the spatial extent matches before returning
    # Note that there are various cases to check here (the full spatial extent and specifically defined extents)
    
    if(is.null(spatial.extent) & vegobject@spatial.extent@id == "FullDomain"
       | identical(spatial.extent, vegobject@spatial.extent)){
      if(store.internally) {run <<- addToVegRun(vegobject, run)}
      return(vegobject)
      
    }  
    
    # Otherwise we must discard this VegObject and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of SpatialExtent",  spatial.extent@id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(vegobject)
    gc()
    
  }
  
  
  
  ############################################################################################################  
  #### NOTE: We don't pass this point if the correct pre-averaged data is available (on disk or in memory) ###
  #### as we will have already returned in CASE 1 or CASE 2 above.                                         ###
  ############################################################################################################
  
  
  
  ### CASE 3 - IF THE WHOLE FILE HAS BEEN READ AND STORED IN MEMORY AS A VEGOBJECT, THEN TAKE THAT AND EXTRACT THE DATA.TABLE AND THEN AVERAGE IT BELOW
  if(var.string %in% names(run@objects)){
    
    if(verbose) message(paste(var.string, " is already read, so using that internal copy.", sep = ""))
    this.dt <- run@objects[[var.string]]@data
    setKeyRVC(this.dt)
    
  }
  
  
  
  ### CASE 4 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  else {
    
    ### !!! CALL MODEL SPECIFIC FUNTIONS HERE !!!
    
    if(run@model == "LPJ-GUESS" | run@model == "LPJ-GUESS-SPITFIRE") {
      
      if(verbose) message(paste("File ", var.string, ".out not already read, so reading it now.", sep = ""))
      
      this.dt <- openLPJOutputFile(run, var.string, verbose = TRUE)
      
    } # END IF LPJ-GUESS or LPJ-GUESS-SPITFIRE
    
    
    else if(run@model == "aDGVM") {
      
      if(adgvm.scheme == 1) this.dt <- data.table(getVegQuantity_aDGVM_Scheme1(run, temporal.extent, quant))
      if(adgvm.scheme == 2) this.dt <- data.table(getVegQuantity_aDGVM_Scheme2(run, temporal.extent, quant))
      setKeyRVC(this.dt)
      
    } # END IF aDGVM
    
    ### !!! END CALL MODEL SPECIFIC FUNCTIONS !!!
    
    
    ### STORE THE FULL MODEL OUTPUT AS AN UNAVERAGED VEGOBJECT IF REQUESTED
    # Note we gotta do this now before the cropping and averaging below
    if(store.full){
      
      # Unique Lons, Lats and Years to build the extent objects
      sorted.unique.lats = sorted.unique.lons = sorted.unique.years = NULL
      if("Lat" %in% names(this.dt)) { sorted.unique.lats <- sort(unique(this.dt[,Lat]))}
      if("Lon" %in% names(this.dt)) { sorted.unique.lons <- sort(unique(this.dt[,Lon]))}
      if("Year" %in% names(this.dt)) { sorted.unique.years <- sort(unique(this.dt[,Year]))}
      
      
      # build the spatial extent depending on if it is a single site or not
      # if it is a site
      if(length(sorted.unique.lons) == 1 & length(sorted.unique.lats) == 1){
        extent.temp <- c(sorted.unique.lons[1], sorted.unique.lats[1])
        is.site <- TRUE
        
      }
      else{
        extent.temp =  extent(sorted.unique.lons[1] - ((sorted.unique.lons[2] - sorted.unique.lons[1])/2), 
                              sorted.unique.lons[length(sorted.unique.lons)] + ((sorted.unique.lons[length(sorted.unique.lons)] - sorted.unique.lons[length(sorted.unique.lons)-1])/2),
                              sorted.unique.lats[1] - ((sorted.unique.lats[2] - sorted.unique.lats[1])/2), 
                              sorted.unique.lats[length(sorted.unique.lats)] + ((sorted.unique.lats[length(sorted.unique.lats)] - sorted.unique.lats[length(sorted.unique.lats)-1])/2))
        is.site <- FALSE
      }
      
      # Build the full object
      vegobject.full <- new("VegObject",
                            id = var.string,
                            data = this.dt,
                            quant = quant,
                            spatial.extent = new("SpatialExtent",
                                                 id = "FullDomain",
                                                 name = "Full simulation extent",
                                                 extent =  extent.temp),
                            temporal.extent = new("TemporalExtent",
                                                  id = "FullTS",
                                                  name = "Full simulation duration",
                                                  start = sorted.unique.years[1],
                                                  end = sorted.unique.years[length(sorted.unique.years)]),
                            is.site = is.site,
                            is.spatially.averaged = FALSE,
                            is.temporally.averaged = FALSE,
                            run = as(run, "VegRunInfo"))
      run <<- addToVegRun(vegobject.full, run)
      
    } # end if(store.full)
    
  } # end option 4
  
  
  ### CROP THE SPATIAL AND TEMPORAL EXTENTS IF REQUESTED
  if(!is.null(spatial.extent))  this.dt <- cropRVC(this.dt, spatial.extent)      
  if(!is.null(temporal.extent))  this.dt <- .selectYears(this.dt, temporal.extent)     
  
  
  ### GET THE LONS, LATS AND YEAR AFTER CROPPING BUT BEFORE THEY ARE AVERAGED AWAY
  is.site <- FALSE
  sorted.unique.lats = sorted.unique.lons = sorted.unique.years = NULL
  if("Lat" %in% names(this.dt)) { sorted.unique.lats <- sort(unique(this.dt[,Lat]))  }
  if("Lon" %in% names(this.dt)) { sorted.unique.lons <- sort(unique(this.dt[,Lon]))}
  if("Year" %in% names(this.dt)) { sorted.unique.years <- sort(unique(this.dt[,Year]))}
  if(length(sorted.unique.lons) == 1 & length(sorted.unique.lats) == 1) is.site <- TRUE
  
  
  ###  DO SPATIAL AVERAGE - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
  if(spatially.average){
    this.dt <- doSpatialAverage(this.dt, verbose, area.weighted)
    if(verbose) {
      message("Head of spatially averaged data.table:")
      print(head(this.dt))
    }
  }
  
  
  ###  DO TIME AVERAGE
  if(temporally.average){
    this.dt <- doTemporalAverage(this.dt, verbose)
    if(verbose) {
      message("Head of time averaged data.table:")
      print(head(this.dt))
    }
  }
  
  
  ### IF NO EXTENTS SPECIFIED, GET THE EXTENTS FOR THE FINAL VEGOBJECT TO RETURN
  
  # TEMPORAL
  if(is.null(temporal.extent)) {
    temporal.extent<- new("TemporalExtent",
                          id = "FullTS",
                          name = "Full simulation duration",
                          start = sorted.unique.years[1],
                          end = sorted.unique.years[length(sorted.unique.years)]
    )
    if(verbose) message(paste("No temporal extent specified, setting temporal extent to whole simulation duration (",  temporal.extent@start, "-", temporal.extent@end, ")", sep = ""))
  }
  
  # SPATIAL
  if(is.null(spatial.extent)) {
    
    # If it is
    if(is.site){
      spatial.extent <- new("SpatialExtent",
                            id = "Site",
                            name = "Single site simulation",
                            extent =  c(sorted.unique.lons[1], sorted.unique.lats[1]))
      
      if(verbose) message(paste("No spatial extent specified, but noting that this is a single site with coordinates = (",  sorted.unique.lons[1], ",", sorted.unique.lats[1], ")", sep = ""))
      
    }
    else  {
      spatial.extent <- new("SpatialExtent",
                            id = "FullDomain",
                            name = "Full simulation extent",
                            extent =  extent(sorted.unique.lons[1] - ((sorted.unique.lons[2] - sorted.unique.lons[1])/2), 
                                             sorted.unique.lons[length(sorted.unique.lons)] + ((sorted.unique.lons[length(sorted.unique.lons)] - sorted.unique.lons[length(sorted.unique.lons)-1])/2),
                                             sorted.unique.lats[1] - ((sorted.unique.lats[2] - sorted.unique.lats[1])/2), 
                                             sorted.unique.lats[length(sorted.unique.lats)] + ((sorted.unique.lats[length(sorted.unique.lats)] - sorted.unique.lats[length(sorted.unique.lats)-1])/2)))
      
      if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  spatial.extent@extent@xmin, ",", spatial.extent@extent@xmax, "), Lat = (" ,  spatial.extent@extent@ymin, ",", spatial.extent@extent@ymax, ").", sep = ""))
      
    }
    
    
  }
  
  ### BUILD THE FINAL VEGOBJECT, STORE IT IF REQUESTED AND RETURN IT
  vegobject <- new("VegObject",
                   id = vegobject.id,
                   data = this.dt,
                   quant = quant,
                   spatial.extent = spatial.extent,
                   temporal.extent = temporal.extent,
                   is.site = is.site,
                   is.spatially.averaged = spatially.average,
                   is.temporally.averaged = temporally.average,
                   run = as(run, "VegRunInfo"))
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN RVCData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .RVCData object...")}
    saveRDS(vegobject, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  ### ADD TO THE VEGRUN OBJECT IF REQUESTED
  if(store.internally) {
    run <<- addToVegRun(vegobject, run)
  }
  
  return(vegobject)
  
}


################################# PROMOTE TO RASTER
#
#' Prepares data as a raster object for plotting.
#' 
#' Converts a data.table, VegObject or an Spatial*-object to Raster object, also subsetting the requested layers.  
#' It can also handle a RasterLayber/Stack/Brick, in which case the only processing done is the subsetting since the data is already in Raster form.
#' This is generally called in the \code{plotVegMaps} function (or potentially before any use of the raster::spplot and raster::plot functions),
#' but can be useful in and of itself.
#'   
#' @param data data.table, VegObject or Spatial*-object to be converted into a raster. Also takes a Raster*-object, in which case he 
#' @param layers The columns to be selected included in the final Raster* object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats,  when converting gridded table to a raster, in the case the the gridded data is not commatters for uneven
#' @param grid.topology A character string defining the grid topology when going from a table to raster, used in a call to SpatialPixels 
#' @return A RasterLayer (or RasterBrick)
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
promoteToRaster <- function(data, layers = "all", tolerance = 0.0000001, grid.topology = NULL){
  
  ###  Get class of the objetc we are dealing with
  this.class = class(data)[1]
  
  ###  Define the layers we are pulling out
  # for VegObject - note could define a methods "names" do cover this exception
  if(is.VegObject(data) & (is.null(layers) | layers[1] == "all")) {layers <- names(data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers[1] == "all") {layers = names(data)}
  
  
  ###  If wSpatialPixelsDataFrame we can plot it
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in veg-runtools.R")
    data.raster <- raster(data, layers)
  }
  ### If data.table or VegObject (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | is.VegObject(data)){
    if(this.class == "data.table") data.spdf <- makeSPDFfromDT(data, layers, tolerance, grid.topology = NULL)
    if(is.VegObject(data)) data.spdf <- makeSPDFfromDT(data@data, layers, tolerance, grid.topology = NULL)
    if(length(layers) == 1){
      data.raster <- raster(data.spdf)
      rm(data.spdf)
    }
    else {
      data.all.raster <- brick(data.spdf)
      data.raster <- subset(data.all.raster, layers) 
      rm(data.spdf, data.all.raster)   
    }
  } 
  ###  If a single raster layer then we are done
  else if(this.class == "RasterLayer"){
    data.raster <- data    
  }
  ### If a stack or a brick, then subset 
  else if(this.class == "RasterBrick" | this.class == "RasterStack"){
    data.raster <- subset(data, layers)
    names(data.raster) <- layers
  }
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to promote object of type", class(data), "to Raster, which I don't know how to do.", sep = " "))
  }
  
  gc()
  return(data.raster)
  
}

################################# SANITIZE NAMES FOR RASTER LAYESR
#
#' Make names appropriate for raster layers 
#' 
#' This "sanaitises" names of an object for use as Raster*-object laey names.  This just involves replacing "-" with "." in the names of the object or the character strings.
#'       
#' @param input A data.table, VegObject, Spatial*-object or vector of characters string to have the names (or itself in the case of a character string) 
#' sanitised to be suitable for use as Raster*object layer names. Also takes a Raster*-object, but this is return directly.
#' @return An object of the same type as was put in.
#' @export
#' @import data.table sp raster
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
sanitiseNamesForRaster <- function(input){
  
  ###  Get class of the object we are dealing with
  this.class = class(input)[1]
  
  
  ###  If SpatialPixelsDataFrame 
  if(this.class == "SpatialPixelsDataFrame"){ 
    names(input) <- sub("-", ".", names(input))
    return(input)
  }
  ### If data.table
  else if(this.class == "data.table"){ 
    input.copy <- copy(input)
    setnames(input.copy, sub("-", ".", names(input.copy)))
    return(input.copy)
  }
  ### If VegObject (which contains a data.table) 
  else if(is.VegObject(input)){
    dt <- copy(input@data)
    setnames(dt, sub("-", ".", names(dt)))
    input@data <- dt
    rm(dt)
    gc()
    return(input)
  }
  ### If "character"
  else if(this.class == "character"){
    input <- sub("-", ".", input)
    return(input)
  }
  ###  If raster object already we are done
  else if(this.class == "RasterLayer" | this.class == "RasterBrick" | this.class == "RasterStack"){
    # Already a raster thing, donothing to do here
    return(input)
  }
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to sanitise names object of type", class(input), ", which I don't know how to do.", sep = ""))
  }
  
  
  
}


##########################################################################################################
########### HELPER FUNCTIONS


################################# MAKE SPATIALPIXELSDATAFRAME FROM DATA.FRAME OR DATA.TABLE
#
#' Make SpatialPixelDataFrame from a data.table
#' 
#' Converts a data.table (or data.frame) to a SpatialPixelsDataFrame, using the columns "Lon and "Lat" to provide the spatial information.  
#' Mostly is called by \code{promoteToRaster}, but can be useful in and of itself.
#'
#' @param data data.table or data.frame, with columsn "Lon" and "Lat" which specify the spatial data 
#' @param layers The columns to be selected included in the final SpatialPixelsDataFrame object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats
#' @param grid.topology A character string defining the grid topology for the SpatialPixelsDataFrame object
#' @return A SpatialPixelDataFrame
#' @export
#' @import data.table sp
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
makeSPDFfromDT <- function(data, layers = "all",  tolerance = 0.0000001, grid.topology = NULL) {
  
  # to stop complaints at build time
  Lon = Lat = NULL
  
  # sort the layers
  if(is.null(layers) | layers[1] == "all") {layers = names(data)}
  
  # remove things we don't want to plot like "Lon" and "Lat"
  remove.list <- c("Lon", "Lat", "Year")
  for(remove in remove.list){
    if(remove %in% layers) {layers <- layers[-which(layers == remove)]}     
  }
  
  # convert to SPDF
  #sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]))
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance, grid = grid.topology)
  )
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    data.spdf <- SpatialPixelsDataFrame(sp.pixels, data[,layers,with=FALSE], tolerance = tolerance)
  )
  # clean up
  rm(sp.points, sp.pixels)
  
  return(data.spdf)  
  
}

###############################################################################################################################
######################### FUNCTIONS FOR SPATIAL AND TEMPORAL AVERAGING DATA.TABLES  ###########################################
###############################################################################################################################





######################### TIME AVERAGE A DATA.TABLE  ##############################
#
#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") or a data.table object
#'
#' @param input.dt data.table  
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doTemporalAverage.uncompiled <- function(input.dt,
                                         verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # Do the averaging
  if(verbose) message("Temporally averaging ...")
  if("Lon" %in% names(input.dt))  output.dt <- input.dt[,lapply(.SD, mean), by=list(Lon, Lat)]
  else output.dt <- input.dt[,lapply(.SD, mean)]
  if(verbose) message("...done.")
  
  # remove the Year 'cos it dun' make so much sense no mo'
  output.dt[,Year:=NULL]
  
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyRVC(output.dt)
  return(output.dt)
  
}



#' Time average a data.table
#' 
#' Time average all availables years (denoted by column "Years") of a data.table object.  This function does not select the years, that should be done first.
#'
#' @param input.dt data.table  
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords export
#' @import cmpfun compiler
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doTemporalAverage <- cmpfun(doTemporalAverage.uncompiled)




.selectYears <- function(input, temporal.extent){
  
  # To stop compiler NOTES
  Year = NULL
  
  # return the subsetted data.table
  return(subset(input, Year >= temporal.extent@start & Year <= temporal.extent@end))    
  
}


######################### SPATIALLY AVERAGE A DATA.TABLE  ##############################
#
#' Spatially average a data.table
#' 
#' Spatially average all gridcells (identified by "Lat" and "Lon" columns) of a data.table object. 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param input.dt data.table  
#' @param area.weighted If TRUE area-weight the gridcells
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doSpatialAverage.uncompiled <- function(input.dt,
                                        verbose = FALSE,
                                        area.weighted=TRUE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # Do the averaging
  if (area.weighted) {
    if (!any(colnames(input.dt)=="area"))
      if (verbose)
        message("Add column area.")
    input.dt <- addArea(input.dt, verbose=verbose)
    if(verbose) message(paste("Spatially averaging (area weighted) whole domain...", sep = ""))
    # check to see if Year is still a clomun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area), by=list(Year)]
    else output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area)]
    
    output.dt[,area:=NULL]
  } else {
    if(verbose) message(paste("Spatially averaging (not area weighted) whole domain...", sep = ""))
    # check to see if Year is still a clomun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, mean), by=list(Year)]
    else output.dt <- input.dt[,lapply(.SD, mean)]
  }
  
  # remove the Lon and Lat columns 'cos they dun' make so much sense no mo'
  output.dt[,Lon:=NULL]
  output.dt[,Lat:=NULL]
  
  if(verbose) message("Averaged")
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  setKeyRVC(output.dt)
  return(output.dt)
  
}

#' Spatially average a data.table
#' 
#' Spatially average all gridcells (identified by "Lat" and "Lon" columns) of a data.table object. 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param input.dt data.table  
#' @param area.weighted If TRUE area-weight the gridcells
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doSpatialAverage <- cmpfun(doSpatialAverage.uncompiled)


#' London centre a gridcell
#' 
#' Transform a longitude value to lie in the range (-180,180) instead of (0,36)
#'
#' @param lon A longitude value to transform data.table  
#' @return The transformed longitude value
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
LondonCentre <- function(lon) {
  
  if(lon <= 180) return(lon)
  else return(lon - 360)
  
}



