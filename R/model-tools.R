#!/usr/bin/Rscript


######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table based in the spatial (Lon, Lat) and temporal (Year, Month, Day), present. 
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
  
  keys.present <- getSTInfo(dt)

  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  
}


######## DEFINE A MODELRUN OBJECT 

#' Define a ModelRun object, setting up all the required metadata (only). 
#' 
#' This function is preferred to a \code{new("ModelRunInfo",...)} initialisation followed by  \code{new("ModelRun",...)} initialisation because it does both intialisations in one step and also performs some extra checks and initialisations of defaults.
#'
#' Note that initialliy, no actual data from the run is stored, only metadata. The data, stored as ModelObjects and added to a ModelRun object, are added built with later with other commands and added to \code{ModelRun} command using \code{addToModelRun}
#'
#' @param id A unique character string to identify this particular model un.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @param model A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE" or "aDGVM". (Mandatory)
#' @param pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @param name A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @param run.dir The location of this run on the file system (Mandatory)
#' @param lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @param year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @param tolerance The tolerance arguement when converting uneven spaced grids to regular rasters for plotting
#' @param london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @param fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @param line.col  A string to define an R colour used when plotting this runs as a line graph
#' @param line.width A numeric to define the width of a line representing this model run
#' @param line.type A numeric to define the line style representing this model run
#' @param driving.data A character string identifying the climate or other data used to produce this model run
#' @param landuseSimulated If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @param contact Name and email address of responsible person (default to OS username).
#' @param institute Name of the institute (default "none").
#' 
#' The parameters are the slots of a ModelRunInfo object. Note that that \code{id}, \code{model}, \code{pft.set} and \code{run.dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS configurations.
#' @return A ModelRun object, with metadata defined by empty data slots
#' @export
#' @seealso ModelRun, ModelRunInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

defineModelRun <- function(id,
                           name,
                           run.dir,
                           model,
                           pft.set,
                           lonlat.offset = c(0,0),
                           year.offset = 0,
                           tolerance = 0.001,
                           london.centre = TRUE,
                           fill.col = "transparent",
                           line.col = "green",
                           line.width = 1,
                           line.type = 1,
                           driving.data = "Not specified",
                           landuseSimulated = FALSE,
                           contact = "Not specified",
                           institute = "Not specified"){
  
  # make a ModelRunInfo object from the supplied meta data
  info <- new("ModelRunInfo",
              id = id,
              model = model,
              pft.set = pft.set,
              name = name,
              run.dir = run.dir,                              
              driving.data = driving.data,
              lonlat.offset = lonlat.offset,
              year.offset = year.offset,
              tolerance = tolerance,
              london.centre = london.centre,
              fill.col = fill.col, # not commonly needed, only for more complex run comparisons
              line.col = line.col, # # not commonly needed, only for more complex run comparisons
              line.width = line.width, # not commonly needed, only for more complex run comparisons
              line.type = line.type, #numeric", # not commonly needed, only for more complex run comparisons
              landuseSimulated = landuseSimulated,
              contact = contact,
              institute = institute)
  
  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(info@pft.set) == 0) info@pft.set <- NULL
  if(length(info@name) == 0)  info@name <- info@id
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
  if(length(info@contact) == 0)  info@contact <- Sys.getenv("USER")
  if(length(info@institute) == 0)  info@institute <- "none"
  
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
    
    # Check that run ids match, if not, stop because something is really wrong
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



############################## MAKE THE 'id' STRING FOR A MODELOBJECT
#
#' Make an ID string for a \code{ModelObject}
#' 
#' Given a string for the quantity and temporal and spatial extents and averaging flags, build an appropriate (and unique) ID string
#' for use in the \code{id} slot of a \code{ModelObject} and for filenames etc.
#' 
#' @param var.string Character string to describe the variable, eg "lai" or "corrected.cmass" or "npp.diff"
#' @param temporal.extent The temporal extent of this object if it has been cropped from the orginal duration, otherwise NULL
#' @param spatial.extent The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param temporal.aggregate.method Character, method by which this ModelObject was temporally aggregated (if not temporally averaged leave blank of use "none")
#' @param spatial.aggregate.method Character, method by which this ModelObject was spatially aggregated (if not spatially averaged leave blank of use "none")
#' @return A character string 
#' @export
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeModelObjectID <- function(var.string, temporal.extent = NULL, spatial.extent = NULL, temporal.aggregate.method = "none", spatial.aggregate.method = "none"){
  
  
  model.object.id <- var.string
  if(tolower(spatial.aggregate.method) != "none")  model.object.id <- paste(model.object.id, "spatial", spatial.aggregate.method, sep = ".")
  if(!is.null(spatial.extent)) model.object.id <- paste(model.object.id, spatial.extent, sep = ".")
  if(tolower(temporal.aggregate.method) != "none")  model.object.id <- paste(model.object.id, "temporal", temporal.aggregate.method, sep = ".")
  if(!is.null(temporal.extent)) model.object.id <- paste(model.object.id, temporal.extent, sep =".")
  
  return(model.object.id)
  
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
#' @param temporal.extent.id A character string to give an identifier for the temporal period this ModelObject covers.
#' @param temporal.aggregate.method A character string describing the method by which to temporally aggregate the data.  Leave blank or use "none" to apply no temporal aggregation. Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param spatial.extent The spatial extent (as a \code{SpatialExtent} object over which the data is to be averaged)
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelObject covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank or use "none" to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param area.weighted If TRUE, perform weighting by gridcell area when doing spatial averaging
#' @param write If TRUE, write the data of the \code{ModelObject} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{ModelObject} in the \code{ModelRun} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{ModelObjects} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM run, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A \code{ModelObject}. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getModelObject <- function(run, 
                           var, 
                           temporal.extent = NULL, 
                           temporal.extent.id = "Full",
                           temporal.aggregate.method = "none", 
                           spatial.extent = NULL, 
                           spatial.extent.id = "Full", 
                           spatial.aggregate.method = "none",
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
  
  ### MAKE UNIQUE IDENTIFIER OF THIS VEGOBJECT VARIABLE AND FILENAME - this describes completely whether we want the files spatially or temporally aggregated and reduced in extent
  model.object.id <- makeModelObjectID(var.string, temporal.extent.id, spatial.extent.id, temporal.aggregate.method, spatial.aggregate.method)
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
      if(is.null(spatial.extent) & model.object@spatial.extent.id == "Full"
       | identical(spatial.extent, model.object@spatial.extent)){
      if(store.internally) {run <<- addToModelRun(model.object, run)}
      return(model.object)
    }  

    # Otherwise we must discard this ModelObject and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of SpatialExtent",  spatial.extent.id, "didn't match.  So file on disk ignored and the original data is being re-read"))
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

      if("aDGVM" %in% quant@model | "Standard" == quant@model) {
        if(adgvm.scheme == 1) this.dt <- data.table(getQuantity_aDGVM_Scheme1(run, temporal.extent, quant))
        if(adgvm.scheme == 2) this.dt <- data.table(getQuantity_aDGVM_Scheme2(run, temporal.extent, quant))
      }
      #else if(quant@model == "Standard") {
      #  stop("Standard quantities nor currently defined for aDGVM")
      #}
      else {
        stop(paste("Quantity", var.string, "doesn't seem to be defined for aDGVM"))
      }
      
    } # END IF aDGVM
    
    
    # If model is from FireMIP
    else if(run@model == "LPJ-GUESS-SPITFIRE-FireMIP"  ||
            run@model == "LPJ-GUESS-SIMFIRE-BLAZE-FireMIP"     ||
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
      
      stop(paste0("The Quantity ", quant@id, " is defined for models ", paste(quant@model, sep=", ", collapse="") , ", which doesn't include the model that you requested getting output for (", run@model, ").  Please check this."))
      
      
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
                                                    extent(this.dt)),
                               spatial.extent.id = "Full",
                               temporal.extent = new("TemporalExtent",
                                                     id = "FullTS",
                                                     name = "Full simulation duration",
                                                     start = year.range[1],
                                                     end = year.range[length(year.range)]),
                               temporal.extent.id = "Full",
                               spatial.aggregate.method = "none",
                               temporal.aggregate.method = "none",
                               run = as(run, "ModelRunInfo"))
      
      # name and store
      #names(model.object.full) <- var.string
      run <<- addToModelRun(model.object.full, run)
      
    } # end if(store.full)
    
  } # end option 4
  
  
  ### CROP THE SPATIAL AND TEMPORAL EXTENTS IF REQUESTED, AND CHECK THAT WE HAVE A VALID DATA.TABLE
  if(!is.null(spatial.extent))  {
    
    # if the provided spatial yields a valid extent, use the crop function
    possible.error <- try ( extent(spatial.extent), silent=TRUE )
    if (class(possible.error) != "try-error") {
      this.dt <- crop(this.dt, spatial.extent)  
      this.spatial.extent <- extent(spatial.extent)
      if(missing(spatial.extent.id)) spatial.extent.id <- "CroppedToExtent"
    }
    
    # else check if some gridcells to be selected with getGridcells
    else if(is.data.frame(spatial.extent) || is.data.table(spatial.extent) || is.numeric(spatial.extent) || is.list(spatial.extent)){
      this.dt <- selectGridcells(this.dt, spatial.extent)
      this.spatial.extent <- spatial.extent
      if(missing(spatial.extent.id)) spatial.extent.id <- "SubsetOfGridcells"
    }
    
    # else fail with error message
    else {
      stop(paste("Trying to select a spatial extent using an object of class", class(spatial.extent)[1], "which isn't really working for me right now.  If this a Spatial* class, contact the authors and they might implement it"))
    }

  }
  else {
    
    this.spatial.extent <- extent(this.dt)
    
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  this.spatial.extent@xmin, ",", this.spatial.extent@xmax, "), Lat = (" ,  this.spatial.extent@ymin, ",", this.spatial.extent@ymax, ").", sep = ""))

  }
  
  if(!is.null(temporal.extent))  this.dt <- selectYears(this.dt, temporal.extent)   
  
  if(length(this.dt) == 0) stop("getModelObject() has produced an empty data.table, so subsequent code will undoubtedly fail.  Please check your input data and the temporal.exent and spatial.extent that you have requested.")
  
  
  
  ### GET THE SPATAIAL AND TEMPORAL EXTENTS BEFORE THEY MAY BE AVERAGED AWAY
  ordered.years = NULL
  if("Year" %in% names(this.dt)) { 
    ordered.years <- sort(unique(this.dt[,Year]))
  }
  
  
  
  ###  DO SPATIAL AGGREGATION - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
  if(tolower(spatial.aggregate.method) != "none"){
    this.dt <- aggregateSpatial(this.dt, method = spatial.aggregate.method, verbose = verbose)
    if(verbose) {
      message("Head of spatially aggregated data.table:")
      print(utils::head(this.dt))
    }
  }
  
  
  ###  DO TIME AGGREGATATION
  if(tolower(temporal.aggregate.method) != "none"){
    this.dt <- aggregateTemporal(this.dt, method = temporal.aggregate.method, verbose = verbose)
    if(verbose) {
      message("Head of time aggregated data.table:")
      print(utils::head(this.dt))
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
 
 
  
  ### BUILD THE FINAL VEGOBJECT, STORE IT IF REQUESTED AND RETURN IT
  model.object <- new("ModelObject",
                      id = model.object.id,
                      data = this.dt,
                      quant = quant,
                      spatial.extent = this.spatial.extent,
                      temporal.extent = temporal.extent,
                      spatial.extent.id = spatial.extent.id,
                      temporal.extent.id = temporal.extent.id,
                      spatial.aggregate.method = spatial.aggregate.method,
                      temporal.aggregate.method = temporal.aggregate.method,
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
  if((is.ModelObject(input.data) || is.DataObject(input.data) || is.ComparisonLayer(input.data) ) & (is.null(layers) | layers[1] == "all")) {layers <- names(input.data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers[1] == "all") {
    layers = names(input.data)
    
    # remove things we don't want to plot like "Lon" and "Lat"
    remove.list <- c("Lon", "Lat", "Year")
    for(remove in remove.list){
      if(remove %in% layers) {layers <- layers[-which(layers == remove)]}     
    }
    
  }
  
  
  ###  If SpatialPixelsDataFrame rasterise it directly
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in veg-runtools.R")
    data.raster <- raster::brick(input.data, layers)
  }
  ### If data.table or ModelObject (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | is.ModelObject(input.data) |  is.DataObject(input.data) | is.ComparisonLayer(input.data)) {
    # first make a SpatialPointsDataFrame
    if(this.class == "data.table") {
      data.spdf <- makeSPDFfromDT(input.data, layers, tolerance, grid.topology = grid.topology)
    }
    
    if(is.ModelObject(input.data) | is.DataObject(input.data) | is.ComparisonLayer(input.data)) {
      data.spdf <- makeSPDFfromDT(input.data@data, layers, tolerance, grid.topology = grid.topology)
    }
    
    
    # now convert to raster
    if(length(layers) == 1){
      data.raster <- raster::raster(data.spdf)
      rm(data.spdf)
    }
    else {
      data.raster <- raster::brick(data.spdf)
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
  
  # alternate all columns
  if(FALSE) {
    all.cols <- unlist(list("Lon", "Lat", unlist(layers)))
    data.spdf <- data.frame(input.data[,all.cols,with=FALSE])
    coordinates(data.spdf) <- ~Lon+Lat
    gridded(data.spdf) <- TRUE
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