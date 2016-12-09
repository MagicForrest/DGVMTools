#!/usr/bin/Rscript


######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table in order Lon, Lat, Year (if present)
#' 
#' Keys should be set on all data.table object for sorts, joins, DGVMTool-defined operators etc.  
#'  This function should be called on a data.table stored in a ModelObject after it has been created,
#'  including if it was created by avergaing another data.table because it seems as keys are not conserved.
#'
#' @param dt The data.table for which to set the key
#' @return Returns nothing because changes the original data.table by reference (this is the data.table way)
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

setKeyDGVM <- function(dt){
  
  keys.present <- c()
  
  if("Lon" %in% names(dt)) keys.present <- append(keys.present, "Lon")
  if("Lat" %in% names(dt)) keys.present <- append(keys.present, "Lat")
  if("Year" %in% names(dt)) keys.present <- append(keys.present, "Year")
  
  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  
}


######## DEFINE A MODELRUN OBJECT 

#' Define a ModelRun object, setting up all the required metadata (only). 
#' 
#' This function is preferred to a \code{new("ModelRunInfo",...)} initialisation followed by  \code{new("ModelRun",...)} initialisation because it does both intialisations in one step and also performs some extra checks and initialisations of defaults.
#'
#' Note that initialliy, no actual data from the run is stored, only metadata. The data, stored as ModelObjects and added to a ModelRun object, are added built with later with other commands and added to \code{ModelRun} command using \code{addToModelRun}
#'
#' @param ...  The parameters are the slots of a ModelRunInfo object. Note that that \code{id} and \code{run.dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' compulsory.  Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS confgurations.
#' @return A ModelRun object, with metadata defined by empty data slots.s
#' @export
#' @seealso ModelRun, ModelRunInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

defineModelRun <- function(...){
  
  # make a ModelRunInfo object from the supplied meta data
  info <- new("ModelRunInfo", ...)
  
  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(info@pft.set) == 0) info@pft.set <- NULL
  if(length(info@description) == 0)  info@description <- "No description specified"
  if(length(info@lonlat.offset) == 0)  info@lonlat.offset <- c(0,0)
  if(length(info@year.offset) == 0)  info@year.offset <- 0
  if(length(info@tolerance) == 0)  info@tolerance <- 0.001
  if(length(info@driving.data) == 0)  info@driving.data <- "No forcing data set"
  if(length(info@london.centre) == 0)  info@london.centre <- TRUE
  if(length(info@fill.col) == 0)  info@fill.col <- "transparent"
  if(length(info@line.col) == 0)  info@line.col <- "green"
  if(length(info@line.width) == 0)  info@line.width <- 1
  if(length(info@line.type) == 0)  info@line.type <- 1
  if(length(info@landuseSimulated) == 0)  info@landuseSimulated <- FALSE
  
  
  # return a ModelRun object with empty data fields but meta data filled  
  return(new("ModelRun",
             info))
  
  
}


############# ADD AN OBJECT TO A MODELRUN ######################################

#' Add an object (either a \code{ModelObject} or a  \code{SpatialComparison}) to a \code{ModelRun} object to be use later.  
#' 
#' Stores an object in its run for later calculations, plotting, comparisons.
#' 
#' @param object Object to add to the \code{ModelRun}.
#' @param run The \code{ModelRun} object to which the object argument should be added
#' @return A ModelRun object the the object argument added
#' @export
#' @seealso ModelRun, ModelRunInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

addToModelRun <- function(object, run){
  
  # Add a BiomeComaprison or RasterComparison to the list in the benchmarks slot 
  if(is.SpatialComparison(object)) {
    
    benchmark.list <- run@benchmarks
    benchmark.list[[object@id]] <- object
    run@benchmarks <- benchmark.list
    rm(benchmark.list)
    
  }
  
  # Add a ModelObject to the list in the objects slot 
  else if(is.ModelObject(object)) {
    
    # Check that run ids match, if not, stop becuase something is really wrong
    if(run@id != object@run@id){
      stop(paste("Adding ModelObject ", object@id, " which comes from run with id = ",  object@run@id, " to run with id = ", run@id, ". If you are doing something funky, like averaging ModelObjects from different runs to make a a ModelRun representing an ensemble mean, then make sure that the ids match. Otherwise you will break the internal logic of DGVMTools so aborting. Contact the package creator if this seems wrong to you." , sep = ""))
    }
    
    model.objects.list <- run@objects
    model.objects.list[[object@id]] <- object
    run@objects <- model.objects.list
    rm(model.objects.list)
    
  }
  
  else{
    
    warning(paste("Cannot add object of class", class(object), "to a ModelRun object", sep = " "))
    
  }
  
  return(run)
  
}


############# REMOVE AN OBJECT FROM A MODELRUN ######################################

#' Remove an object (either a \code{ModelObject} from a \code{ModelRun} object to reclaim memory.  
#' 
#' Processing a lot of different files and storing the full output internally (in the ModelRun) may use too much memory. You can get around this by not storing them internally, 
#' but if you need to do this, for example because you want to average many spatial or temporal extents from one output file, this function lets you remove ModelObject to free the space again.  
#' 
#' @param object.id The id of the object to remove  to the \code{ModelRun},   
#' @param run The \code{ModelRun} object to which the object argument should be added
#' @return A ModelRun with the object removed
#' @export
#' @seealso ModelRun, ModelRunInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

removeFromModelRun <- function(object.id, run){
  
  
  if(object.id %in% names(run@objects)) {
    
    model.objects.list <- run@objects
    model.objects.list[[object.id]] <- NULL
    run@objects <- model.objects.list
    rm(model.objects.list)
    gc()
    
  }
  
  else{
    warning(paste("Can't remove ModelObject with id ", object.id, " from run ", run@id, " because I can't find it in the run!"))
  }
  
  
  return(run)
  
}



############################## MAKE THE 'id' STRING FOR A VEGOBJECT
#
#' Make an ID string for a \code{ModelObject}
#' 
#' Given a string for the quantity and temporal and spatial extents and averaging flags, build an appropriate (and unique) ID string
#' for use in the \code{id} slot of a \code{ModelObject} and for filenames etc.
#' 
#' @param var.string Character string to describe the variable, eg "lai" or "corrected.cmass" or "npp.diff"
#' @param temporal.extent The temporal extent of this object if it has been cropped from the orginal duration, otherwise NULL
#' @param spatial.extent The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param temporally.averaged Logical, should be TRUE if temporal averaging has been done
#' @param spatially.averaged Logical, should be TRUE if spatial averaging has been done
#' @return A character string 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeModelObjectID <- function(var.string, temporal.extent = NULL, spatial.extent = NULL, temporally.averaged = FALSE, spatially.averaged = FALSE){
  
  
  model.object.id <- var.string
  if(spatially.averaged)  model.object.id <- paste(model.object.id, "SA", sep = ".")
  if(!is.null(spatial.extent)) model.object.id <- paste(model.object.id, spatial.extent@id, sep = ".")
  if(temporally.averaged)  model.object.id <- paste(model.object.id, "TA", sep = ".")
  if(!is.null(temporal.extent)) model.object.id <- paste(model.object.id, paste(temporal.extent@start, temporal.extent@end, sep = "-"), sep =".")
  
  return(model.object.id)
  
}

################################# GET TEMPORALLY-AVERAGED DATA #########################################

#' Get a temporally-averaged \code{ModelObject}
#' 
#' Given a \code{ModelRun} object, a \code{Quantity} object and a \code{TemporalExtent} object, return an appropriate temporally-averaged \code{ModelObject} oject for that run, quantity and time period over the whole simulation domain.
#' 
#' @param run The \code{ModelRun} object for which the temporally-averaged \code{ModelObject} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param period The time period (as a \code{TemporalExtent} over which the data is to be averaged)
#' @param ... additional arguments passed to \code{getModelObject}, for example \code{read.full}, and \code{store.internally}.
#' @return A temporally averaged\code{ModelObject}  but with no-spatial averaging or cropping (ie. include complete original simulation domain). In other words, a map!
#' @export
#' @seealso \code{getModelObject}, \code{getVegTemporal}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'  
getVegSpatial <- function(run,
                          var,
                          period,
                          ...){
  
  return(
    getModelObject(run, 
                   var, 
                   spatial.extent = NULL, 
                   spatially.average = FALSE,
                   temporal.extent = period, 
                   temporally.average = TRUE, 
                   ...)
  )
  
  
}


################################# GET SPATIALLY-AVERAGED DATA #########################################


#' Get a spatially-averaged \code{ModelObject}
#' 
#' Given a \code{ModelRun} object, a \code{Quantity} object and a \code{SpatialExtent} object, return an appropriate spatially-averaged \code{ModelObject} oject for that run, quantity and time period over the whole simulation domain.
#' 
#' @param run The \code{ModelRun} object for which the spatially-averaged \code{ModelObject} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param spatial.extent The spatial extent (as a \code{SpatialExtent} object over which the data is to be averaged)
#' @param ... additional arguments passed to \code{getModelObject}, for example \code{read.full}, \code{area.weighted} and \code{store.internally}.
#' @return A spatially averaged\code{ModelObject} but with no temporal averaging or cropping (ie. include complete original simulation duration). In other words, a time-series!
#' @export
#' @seealso \code{getModelObject}, \code{getVegspatial}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

getVegTemporal <- function(run,
                           var,
                           spatial.extent = NULL,
                           ...){
  
  return(
    getModelObject(run, 
                   var, 
                   spatial.extent = spatial.extent, 
                   spatially.average = TRUE,
                   temporal.extent = NULL, 
                   temporally.average = FALSE, 
                   ...)
  )
  
  
}



################################# GET MODELOBJECT - Does a lot! #########################################
#
#' Get a \code{ModelObject}, optionally with spatial/temporal averaging/cropping 
#' 
#' Given a \code{ModelRun} object a \code{Quantity} object, return an appropriate spatially-averaged \code{ModelObject} oject for that run and quantity. Arguments can also be provided for averaging over different spatial or temporal extents (very useful) or optionall just cropping to those extents
#' 
#' Note that because there are two types of averaging available, the resulting \code{ModelRun} object can either be full spatial-temporal dataset, a spatial-only dataset (map), a temporal only datasey a time-series) or an average across both space and time, i.e. a single number.
#' Also not that the data is stored internal as a data.table object, but this is mostly not important to the user.
#'   
#' @param run The \code{ModelRun} object for which the spatially-averaged \code{ModelObject} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param temporal.extent The temporal extent (as a \code{TemporalExtent} object over which the data is to be averaged)
#' @param temporally.average Whether or not to temporally average (logical)
#' @param spatial.extent The spatial extent (as a \code{SpatialExtent} object over which the data is to be averaged)
#' @param spatially.average Whether or not to spatially average (logical)
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param area.weighted If TRUE, perform weighting by gridcell area when doing spatial averaging
#' @param write If TRUE, write the data of the \code{ModelObject} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{ModelObject} in the \code{ModelRun} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{ModelObjects} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM run, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A spatially averaged\code{ModelObject} but with no temporal averaging or cropping (ie. include complete original simulation duration). In other words, a time-series!
#' @export
#' @import data.table raster
#' @seealso \code{getModelObject}, \code{getVegspatial}
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getModelObject <- function(run, 
                           var, 
                           temporal.extent = NULL, 
                           temporally.average = FALSE, 
                           spatial.extent = NULL, 
                           spatially.average = FALSE,
                           area.weighted=TRUE,
                           write = FALSE, 
                           read.full = TRUE, 
                           verbose = TRUE, 
                           store.internally = FALSE,
                           store.full = FALSE,
                           adgvm.scheme = 1){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  
  ### CONVERT STRING TO VEGQUANT
  if(class(var) == "character") {
    quant <- lookupQuantity(var, run@model)
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  ### MAKE UNIQUE IDENTIFIER OF THIS VEGOBJECT VARIABLE AND FILENAME - this describes completely whether we want the files spatially or temporally averaged and reduced in extent
  model.object.id <- makeModelObjectID(var.string, temporal.extent, spatial.extent, temporally.average, spatially.average)
  file.name <- file.path(run@run.dir, paste(model.object.id, "DGVMData", sep = "."))
  if(verbose) message(paste("Seeking ModelObject with id = ", model.object.id, sep = ""))
  
  
  
  ### CASE 1 - USE THE EXACT VEGOBJECT IF IT HAS ALREADY BEEN COMPUTED AND SAVED IN THE MODELRUN IN MEMORY
  if(model.object.id %in% names(run@objects)){
    
    # if it is present it in memory then can be returned directly
    if(verbose) message(paste("Exact ModelObject (with id = ", model.object.id, ") already found in memory for this ModelRun, so using that.", sep = ""))
    return(run@objects[[model.object.id]])
    
  }
  
  
  
  ### CASE 2 - USE THE PREAVERAGED/CROPPED VEGOBJECT FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  run@run.dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    model.object <- readRDS(file.name)
    
    # Update the run object, that might have (legitimately) changed compared to the id that was used when this model.object was created
    # for example it might be assigned a new id.
    model.object@run <- run
    
    
    # Check that the spatial extent matches before returning
    # Note that there are various cases to check here (the full spatial extent and specifically defined extents)
    
    if(is.null(spatial.extent) & model.object@spatial.extent@id == "FullDomain"
       | identical(spatial.extent, model.object@spatial.extent)){
      if(store.internally) {run <<- addToModelRun(model.object, run)}
      return(model.object)
      
    }  
    
    # Otherwise we must discard this ModelObject and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of SpatialExtent",  spatial.extent@id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(model.object)
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
    setKeyDGVM(this.dt)
    
  }
  
  
  
  ### CASE 4 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  else {
    
    if(verbose) message(paste("File ", var.string, ".out not already read, so reading it now.", sep = ""))
    
    ### !!! CALL MODEL SPECIFIC FUNTIONS HERE !!!
    
    # If model is LPJ-GUESS(-SPITFIRE) and the required Quantity is defined for LPJ-GUESS(-SPITFIRE)
    if(run@model == "LPJ-GUESS" | run@model == "LPJ-GUESS-SPITFIRE" ) {
      
      
      if("LPJ-GUESS" %in% quant@model  | "LPJ-GUESS-SPITFIRE" %in% quant@model) {
        this.dt <- openLPJOutputFile(run, var.string, verbose = verbose)
      }
      else if(quant@model == "Standard") {
        this.dt <- getStandardQuantity_LPJ(run, quant, verbose = verbose)
      }
      
    } # END IF LPJ-GUESS or LPJ-GUESS-SPITFIRE
    
    
    # If model is aDGVM and the required Quantity is defined for aDGVM
    else if(run@model == "aDGVM") {
      
      if(verbose) message(paste("File ", var.string, ".out not already read, so reading it now.", sep = ""))
      
      if("aDGVM" %in% quant@model) {
        if(adgvm.scheme == 1) this.dt <- data.table(getQuantity_aDGVM_Scheme1(run, temporal.extent, quant))
        if(adgvm.scheme == 2) this.dt <- data.table(getQuantity_aDGVM_Scheme2(run, temporal.extent, quant))
      }
      else if(quant@model == "Standard") {
        stop("Standard quantities nor currently defined for aDGVM")
      }
      
    } # END IF aDGVM
    
    
    # If model is from FireMIP
    else if(run@model == "LPJ-GUESS-SPITFIRE-FireMIP"  ||
            run@model == "LPJ-GUESS-BLAZE-FireMIP"     ||
            run@model == "LPJ-GUESS-GlobFIRM-FireMIP"  ||
            run@model == "CLM-FireMIP"                 ||
            run@model == "CTEM-FireMIP"                ||
            run@model == "Inferno-FireMIP"             ||
            run@model == "JSBACH-FireMIP"              ||
            run@model == "ORCHIDEE-FireMIP"               ) {
      
      
      if(quant@model == "FireMIP") {
        this.dt <- openFireMIPOutputFile(run, var.string, quantity = quant, temporal.extent = temporal.extent, verbose = verbose)
      }
      else {
        
        stop(paste0("Cannot open quantity", quant, "for FireMIP output, only special 'FireMIP' quantities are defined"))
        
      }
      
      
    }
    
    
    
    
    
    
    else {
      
      stop(paste0("The Quantity ", quant@id, " is defined for models ", paste(quant@models, sep=", ", collapse="") , ", which doesn't include the model that you requested getting output for (", run@model, ").  Please check this."))
      
      
    }
    
    setKeyDGVM(this.dt)
    
    
    
    ### !!! END CALL MODEL SPECIFIC FUNCTIONS !!!
    
    
    ### STORE THE FULL MODEL OUTPUT AS AN UNAVERAGED VEGOBJECT IF REQUESTED
    # Note we gotta do this now before the cropping and averaging below
    if(store.full){
      
      # Build the full object
      year.range <- range(this.dt[,Year])
      model.object.full <- new("ModelObject",
                               id = var.string,
                               data = this.dt,
                               quant = quant,
                               spatial.extent = new("SpatialExtent",
                                                    id = "FullDomain",
                                                    name = "Full simulation extent",
                                                    extent =  extent(this.dt)),
                               temporal.extent = new("TemporalExtent",
                                                     id = "FullTS",
                                                     name = "Full simulation duration",
                                                     start = year.range[1],
                                                     end = year.range[length(year.range)]),
                               is.site = FALSE,
                               is.spatially.averaged = FALSE,
                               is.temporally.averaged = FALSE,
                               run = as(run, "ModelRunInfo"))
      
      # name and store
      #names(model.object.full) <- var.string
      run <<- addToModelRun(model.object.full, run)
      
    } # end if(store.full)
    
  } # end option 4
  
  
  ### CROP THE SPATIAL AND TEMPORAL EXTENTS IF REQUESTED
  if(!is.null(spatial.extent))  this.dt <- cropDGVM(this.dt, spatial.extent)      
  if(!is.null(temporal.extent))  this.dt <- .selectYears(this.dt, temporal.extent)     
  
  
  ### GET THE SPATAIAL AND TEMPORAL EXTENTS BEFORE THEY MAY BE AVERAGED AWAY
  is.site <- FALSE
  temp.extent <- extent(this.dt)
  ordered.years = NULL
  if("Year" %in% names(this.dt)) { 
    ordered.years <- sort(unique(this.dt[,Year]))
  }
  
  
  
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
                          start = ordered.years[1],
                          end = ordered.years[length(ordered.years)]
    )
    if(verbose) message(paste("No temporal extent specified, setting temporal extent to whole simulation duration (",  temporal.extent@start, "-", temporal.extent@end, ")", sep = ""))
  }
  
  # SPATIAL
  if(is.null(spatial.extent)) {
    
    spatial.extent <- new("SpatialExtent",
                          id = "FullDomain",
                          name = "Full simulation extent",
                          extent =  temp.extent)
    
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  spatial.extent@extent@xmin, ",", spatial.extent@extent@xmax, "), Lat = (" ,  spatial.extent@extent@ymin, ",", spatial.extent@extent@ymax, ").", sep = ""))
    
  }
  
  
  
  ### BUILD THE FINAL VEGOBJECT, STORE IT IF REQUESTED AND RETURN IT
  model.object <- new("ModelObject",
                      id = model.object.id,
                      data = this.dt,
                      quant = quant,
                      spatial.extent = spatial.extent,
                      temporal.extent = temporal.extent,
                      is.site = is.site,
                      is.spatially.averaged = spatially.average,
                      is.temporally.averaged = temporally.average,
                      run = as(run, "ModelRunInfo"))
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMData object...")}
    saveRDS(model.object, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  ### ADD TO THE MODELRUN OBJECT IF REQUESTED
  if(store.internally) {
    run <<- addToModelRun(model.object, run)
  }
  
  return(model.object)
  
}


################################# PROMOTE TO RASTER
#
#' Prepares data as a raster object for plotting.
#' 
#' Converts a data.table, ModelObject or an Spatial*-object to Raster object, also subsetting the requested layers.  
#' It can also handle a RasterLayber/Stack/Brick, in which case the only processing done is the subsetting since the data is already in Raster form.
#' This is generally called in the \code{plotSpatial} function (or potentially before any use of the raster::spplot and raster::plot functions),
#' but can be useful in and of itself.
#'   
#' @param input.data data.table, ModelObject or Spatial*-object to be converted into a raster. Also takes a Raster*-object, in which case he 
#' @param layers The columns to be selected included in the final Raster* object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats,  when converting gridded table to a raster, in the case the the gridded data is not commatters for uneven
#' @param grid.topology A character string defining the grid topology when going from a table to raster, used in a call to SpatialPixels 
#' @return A RasterLayer (or RasterBrick)
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
promoteToRaster <- function(input.data, layers = "all", tolerance = 0.0000001, grid.topology = NULL){
 
  ###  Get class of the object we are dealing with
  this.class = class(input.data)[1]

  ###  Define the layers we are pulling out
  # for ModelObject - note could define a methods "names" do cover this exception
  if((is.ModelObject(input.data) || is.DataObject(input.data)) & (is.null(layers) | layers[1] == "all")) {layers <- names(input.data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers[1] == "all") {layers = names(input.data)}
  
  
  ###  If SpatialPixelsDataFrame rasterise it directly
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in veg-runtools.R")
    data.raster <- brick(input.data, layers)
  }
  ### If data.table or ModelObject (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | is.ModelObject(input.data) |  is.DataObject(input.data)){
    
    # first make a SpatialPointsDataFrame
    
    if(this.class == "data.table") {
      data.spdf <- makeSPDFfromDT(input.data, layers, tolerance, grid.topology = grid.topology)
    }
    
    if(is.ModelObject(input.data) | is.DataObject(input.data)) data.spdf <- makeSPDFfromDT(input.data@data, layers, tolerance, grid.topology = grid.topology)
    
    # now convert to raster
    if(length(layers) == 1){
      data.raster <- raster(data.spdf)
      rm(data.spdf)
    }
    else {
      data.raster <- brick(data.spdf)
      rm(data.spdf)   
    }
    
  } 
  ###  If a single raster layer then we are done
  else if(this.class == "RasterLayer"){
    data.raster <- input.data    
  }
  ### If a stack or a brick, then subset 
  else if(this.class == "RasterBrick" | this.class == "RasterStack"){
    data.raster <- subset(input.data, layers)
    names(data.raster) <- layers
  }
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to promote object of type", class(input.data), "to Raster, which I don't know how to do.", sep = " "))
  }
  
  gc()
  return(data.raster)
  
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
#' @param input.data data.table or data.frame, with columsn "Lon" and "Lat" which specify the spatial data 
#' @param layers The columns to be selected included in the final SpatialPixelsDataFrame object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats
#' @param grid.topology A GridTopology defining the grid topology for the SpatialPixelsDataFrame object
#' @return A SpatialPixelDataFrame
#' @export
#' @import data.table sp
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
makeSPDFfromDT <- function(input.data, layers = "all",  tolerance = 0.0000001, grid.topology = NULL) {
  
  # to stop complaints at build time
  Lon = Lat = NULL
  
  # sort the layers
  if(is.null(layers) | layers[1] == "all") {layers = names(input.data)}
  
  # remove things we don't want to plot like "Lon" and "Lat"
  remove.list <- c("Lon", "Lat", "Year")
  for(remove in remove.list){
    if(remove %in% layers) {layers <- layers[-which(layers == remove)]}     
  }
  
  # convert to SPDF
  #sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.points <- SpatialPoints(data.frame(input.data[,list(Lon, Lat)]))
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance, grid = grid.topology)
  )
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    data.spdf <- SpatialPixelsDataFrame(sp.pixels, input.data[,layers,with=FALSE], tolerance = tolerance)
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
  setKeyDGVM(output.dt)
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
  
  # Warning if a certain year is not present
  years.present <- unique(input[["Year"]])
  for(year in temporal.extent@start:temporal.extent@end){
    if(!(year %in% years.present)) warning(paste("Year", year, "requested, but it is not in the data!", sep = " "))
  }
  
  # return the subsetted data.table
  return(subset(input, Year >= temporal.extent@start & Year <= temporal.extent@end))    
  
}


######################### SPATIALLY AVERAGE ##############################
#
#' Spatially average a ModelObject, DataObject or data.table 
#' 
#' Spatially average all gridcells of a ModelObject, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param A ModelObject, DataObject or data.table to be averaged  
#' @param area.weighted If TRUE area-weight the gridcells
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doSpatialAverage.uncompiled <- function(input.obj,
                                        verbose = FALSE,
                                        area.weighted=TRUE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {input.dt <- input.obj@data}
  else if(is.data.table(input.obj)) {input.dt <- input.obj}
  
  
  
  # Do the averaging
  if (area.weighted) {
    if (!any(colnames(input.dt)=="area")) {
      if (verbose) message("Add column area.")
      input.dt <- addArea(input.dt, verbose=verbose)
    }
    if(verbose) message(paste("Spatially averaging (area weighted) whole domain...", sep = ""))
    # check to see if Year is still a clomun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area), by=list(Year)]
    else {
      output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area)]
    }
    
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
  setKeyDGVM(output.dt)
  
  # sort out the input object class
  if(is.DataObject(input.obj) | is.ModelObject(input.obj)) {
    input.obj@data <- output.dt
    input.obj@is.spatially.averaged <- TRUE
    input.obj@id <- makeModelObjectID(input.obj@quant@id, temporal.extent = input.obj@temporal.extent, spatial.extent = input.obj@spatial.extent, temporally.averaged = input.obj@is.temporally.averaged, spatially.averaged = TRUE)
    return(input.obj)
  }
  else if(is.data.table(input.obj)) {return(output.dt)}
  
}

######################### SPATIALLY AVERAGE ##############################
#
#' Spatially average a ModelObject, DataObject or data.table 
#' 
#' Spatially average all gridcells of a ModelObject, DataObject or data.table (assuming the data.table has columns "Lon" and "Lat"). 
#' Can use area-weighting to take into account the difference different areas of gridcells (even though they are constant in Lon,Lat)  
#'
#'
#' @param input.obj A ModelObject, DataObject or data.table to be averaged 
#' @param area.weighted If TRUE area-weight the gridcells
#' @param verbose If TRUE give some progress update about the averaging.
#' @return A data.table
#' @keywords internal
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
doSpatialAverage <- cmpfun(doSpatialAverage.uncompiled)


#' London centre a gridcell
#' 
#' Transform a vector of longitude values to lie in the range (-180,180) instead of (0,36)
#'
#' @param lon A vector longitude value to transform 
#' @return The transformed longitude values
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
LondonCentre <- function(lon) {
  
  return(ifelse(lon > 180, lon - 360, lon))
  
}

#' Get the spatial extent from a data.table
#' 
#' Returns either a raster::extent (for a grid of points) or a numerical vector (for a single site) from a data.table 
#' assuming column name "Lon" and "Lat" are present.
#' 
#' @param dt A data.table
#' 
#' @export
#' @importFrom raster extent
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @return Either a raster::extent (for a grid of points) or a numerical vector (for a single site)
getExtentFromDT <- function(dt){
  
  Lon = Lat = NULL
  
  # Get an ordered list of lons and lats
  if("Lat" %in% names(dt)) { ordered.lats <- sort(unique(dt[,Lat]))}
  if("Lon" %in% names(dt)) { ordered.lons <- sort(unique(dt[,Lon]))}
  
  
  # Now build the spatial extent depending on if it is a single site or not
  # if it is a site
  if(length(ordered.lons) == 1 & length(ordered.lats) == 1){
    extent.temp <- c(ordered.lons[1], ordered.lats[1])
  }
  # else it is a 'proper' extent
  else{
    
    extent.temp =  extent(ordered.lons[1] - ((ordered.lons[2] - ordered.lons[1])/2), 
                          ordered.lons[length(ordered.lons)] + ((ordered.lons[length(ordered.lons)] - ordered.lons[length(ordered.lons)-1])/2),
                          ordered.lats[1] - ((ordered.lats[2] - ordered.lats[1])/2), 
                          ordered.lats[length(ordered.lats)] + ((ordered.lats[length(ordered.lats)] - ordered.lats[length(ordered.lats)-1])/2))
  }
  
  return(extent.temp)
  
}


#' Average ModelObjects provided as a list
#' 
#' Useful for averaging model ensembles, replicate simulations etc.  
#' Note that since you can supply the function, you don't actually need to average, you can also do, for example, \code{max}, \code{min}, \code{sd}, etc.  
#' Fails with an error message if the supplied ModelObjects don't have the same fields
#' 
#' @param list.of.model.objects The ModelObjects that you want to average, stored in a list.
#' @param run Either a ModelRun object which provides the metadata about the newly created "run" (representing, say, the ensemble mean) or \code{NULL}, 
#' in which case the function just returns a data.table with the average values.
#' @param method The function which you want to use.  Is \code{mean} by default, but it can be anything that operates on a vector of numbers.  
#' Useful options might be \code{max},  \code{min} and \code{sd}.  
#'  
#'@details  
#' The function currently does no checks that the spatial extent, temporal extent and Quantity are consistent between runs.  The resulting ModelObject simple takes these things
#' from the first object from list.of.vegruns.  It is therefore up to the user not do anything silly and average quantites, or spatial or temporal extents that don't makes sense.
#'          
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @return Either a ModelObject (if a ModelRun has been supplied to provide the necessary meta-data), or a data.table (if no meta-data has been supplies in the form of a ModelRun)
#' 
averageModelObjects <- function(list.of.model.objects, run = NULL, method = mean) {
  
  
  # make lists of what is to be processed, and check that the columns are the same (if not fail!)
  list.of.dts <- list()
  for(object in list.of.model.objects){
    list.of.dts[[paste(object@run@id, object@id, sep = ".")]] <- object@data
  }
  for(this.dt in list.of.dts) {
    if(!identical(names(this.dt), names(list.of.dts[[1]]))) stop("You are trying to average model.objects with different layers, this won't work!")
  }
  
  
  # check if Lon, Lat and Year are present and so should be averaged over
  by.list <- c()
  if("Lat" %in% names(list.of.dts[[1]])) by.list <- append(by.list, "Lat")
  if("Lon" %in% names(list.of.dts[[1]])) by.list <- append(by.list, "Lon")
  if("Year" %in% names(list.of.dts[[1]])) by.list <- append(by.list, "Year")
  
  
  # do the averaging to make the new data.table and tidy up
  full.dt <- rbindlist(l=list.of.dts)
  setKeyDGVM(full.dt)
  output.dt <- full.dt[,lapply(.SD, method), keyby=by.list]
  rm(full.dt, list.of.dts)
  gc()
  
  
  # if no run is supplied then cannot construct a full ModelObject, therefore just return the data.table
  if(is.null(run)) {
    return(output.dt)
  }
  
  # else, if a run was provided, make a ModelObject and return that
  else {
    
    #  MF TODO: check that the ModelObjects we just averaged have the same spatial and temporal properties 
    # if they don't, drop an error message
    
    
    # construct a new ModelObject for the average data that we have just calculated based on the first on in the list
    # but put in the new run and data
    new.ModelObject <- list.of.model.objects[[1]]
    new.ModelObject@data <- output.dt
    new.ModelObject@run <- as(run, "ModelRunInfo")
    
    
    return(new.ModelObject)
  }
  
}