#!/usr/bin/Rscript

################################# GET MODEL FIELD - Does a lot! #########################################
#
#' Get a \code{ModelField}, optionally with spatial/temporal averaging/cropping 
#' 
#' Given a \code{Source} object a \code{Quantity} object, return an appropriate spatially-averaged \code{ModelField} oject for that run and quantity. Arguments can also be provided for averaging over different spatial or temporal extents (very useful) or optionall just cropping to those extents
#' 
#' Note that because there are two types of averaging available, the resulting \code{Source} object can either be full spatial-temporal dataset, a spatial-only dataset (map), a temporal only datasey a time-series) or an average across both space and time, i.e. a single number.
#' Also not that the data is stored internal as a data.table object, but this is mostly not important to the user.
#'   
#' @param run The \code{Source} object for which the spatially-averaged \code{ModelField} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param temporal.extent The temporal extent (as a \code{TemporalExtent} object over which the data is to be averaged)
#' @param temporal.extent.id A character string to give an identifier for the temporal period this ModelField covers.
#' @param temporal.aggregate.method A character string describing the method by which to temporally aggregate the data.  Leave blank or use "none" to apply no temporal aggregation. Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param spatial.extent The spatial extent (as a \code{SpatialExtent} object over which the data is to be averaged)
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelField covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank or use "none" to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param area.weighted If TRUE, perform weighting by gridcell area when doing spatial averaging
#' @param write If TRUE, write the data of the \code{ModelField} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{ModelField} in the \code{Source} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{ModelFields} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM run, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A \code{ModelField}. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getModelField <- function(run, 
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
  model.field.id <- makeFieldID(var.string, temporal.extent.id, spatial.extent.id, temporal.aggregate.method, spatial.aggregate.method)
  file.name <- file.path(run@dir, paste(model.field.id, "DGVMData", sep = "."))
  if(verbose) message(paste("Seeking ModelField with id = ", model.field.id, sep = ""))
  
  
  
  ### CASE 1 - USE THE EXACT VEGOBJECT IF IT HAS ALREADY BEEN COMPUTED AND SAVED IN THE MODELRUN IN MEMORY
  if(model.field.id %in% names(run@objects)){
    
    # if it is present it in memory then can be returned directly
    if(verbose) message(paste("Exact ModelField (with id = ", model.field.id, ") already found in memory for this.Field, so using that.", sep = ""))
    return(run@objects[[model.field.id]])
    
  }
  
  
  
  ### CASE 2 - USE THE PREAVERAGED/CROPPED VEGOBJECT FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  run@dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    model.field <- readRDS(file.name)
    
    # Update the run object, that might have (legitimately) changed compared to the id that was used when this model.field was created
    # for example it might be assigned a new id.
    model.field@source <- run
    
    
    # Check that the spatial extent matches before returning
    # Note that there are various cases to check here (the full spatial extent and specifically defined extents)
    if(is.null(spatial.extent) & model.field@spatial.extent.id == "Full"
       | identical(spatial.extent, model.field@spatial.extent)){
      if(store.internally) {run <<- addToSource(model.field, run)}
      return(model.field)
    }  
    
    # Otherwise we must discard this ModelField and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of SpatialExtent",  spatial.extent.id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(model.field)
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
      model.field.full <- new("Field",
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
                               source = as(run, "SourceInfo"))
      
      # name and store
      #names(model.field.full) <- var.string
      run <<- addToSource(model.field.full, run)
      
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
  
  if(length(this.dt) == 0) stop("getModelField() has produced an empty data.table, so subsequent code will undoubtedly fail.  Please check your input data and the temporal.exent and spatial.extent that you have requested.")
  
  
  
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
  model.field <- new("Field",
                      id = model.field.id,
                      data = this.dt,
                      quant = quant,
                      spatial.extent = this.spatial.extent,
                      temporal.extent = temporal.extent,
                      spatial.extent.id = spatial.extent.id,
                      temporal.extent.id = temporal.extent.id,
                      spatial.aggregate.method = spatial.aggregate.method,
                      temporal.aggregate.method = temporal.aggregate.method,
                      source = as(run, "SourceInfo"))
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMData object...")}
    saveRDS(model.field, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  ### ADD TO THE MODELRUN OBJECT IF REQUESTED
  if(store.internally) {
    run <<- addToSource(model.field, run)
  }
  
  return(model.field)
  
}
