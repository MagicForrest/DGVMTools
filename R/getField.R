#!/usr/bin/Rscript

################################# GET FIELD - Does a lot! #########################################
#
#' Get a \code{Field}, optionally with spatial/yearly averaging/cropping 
#' 
#' Given a \code{Source} object a \code{Quantity} object, return an appropriate spatially-averaged \code{ModelField} oject for that source and quantity. Arguments can also be provided for averaging over different spatial or year extents (very useful) or optionall just cropping to those extents
#' 
#' Note that because there are two types of averaging available, the resulting \code{Source} object can either be full spatial-temporal dataset, a spatial-only dataset (map), a temporal only datasey a time-series) or an average across both space and time, i.e. a single number.
#' Also not that the data is stored internal as a data.table object, but this is mostly not important to the user.
#'   
#' @param source The \code{Source} object for which the spatially-averaged \code{ModelField} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param year.aggregate.method A character string describing the method by which to annual aggregate the data.  Leave blank or use "none" to apply no annual aggregation. Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param spatial.extent An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelField covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank or use "none" to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param write If TRUE, write the data of the \code{ModelField} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{ModelField} in the \code{Source} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{ModelFields} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM source, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A \code{ModelField}. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getField <- function(source, 
                     var, 
                     first.year = NULL,
                     last.year = NULL,
                     year.aggregate.method = "none", 
                     spatial.extent = NULL, 
                     spatial.extent.id = "Full", 
                     spatial.aggregate.method = "none",
                     write = FALSE, 
                     read.full = TRUE, 
                     verbose = FALSE, 
                     store.internally = FALSE,
                     store.full = FALSE,
                     adgvm.scheme = 1){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  ### CHECK ARGUEMENTS
  
  
  ### CONVERT STRING TO QUANTITY
  if(class(var) == "character") {
    quant <- lookupQuantity(var, source@format)
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  ### MAKE UNIQUE IDENTIFIER OF THIS FIELD VARIABLE AND FILENAME - this describes completely whether we want the files spatially, yearly or subanually aggregated and reduced in extent
  model.field.id <- makeFieldID(source.info = as(source, "SourceInfo"), 
                                var.string = var.string, 
                                first.year = first.year,
                                last.year = last.year,
                                year.aggregate.method = year.aggregate.method, 
                                spatial.extent.id = spatial.extent.id, 
                                spatial.aggregate.method = spatial.aggregate.method)
  file.name <- file.path(source@dir, paste(model.field.id, "DGVMField", sep = "."))
  if(verbose) message(paste("Seeking ModelField with id = ", model.field.id, sep = ""))
  
  
  
  ### CASE 1 - USE THE EXACT VEGOBJECT IF IT HAS ALREADY BEEN COMPUTED AND SAVED IN THE SOURCE IN MEMORY
  if(model.field.id %in% names(source@objects)){
    
    # if it is present it in memory then can be returned directly
    if(verbose) message(paste("Exact ModelField (with id = ", model.field.id, ") already found in memory for this.Field, so using that.", sep = ""))
    return(source@objects[[model.field.id]])
    
  }
  
  
  
  ### CASE 2 - USE THE PREAVERAGED/CROPPED VEGOBJECT FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  source@dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    model.field <- readRDS(file.name)
    
    # Update the source object, that might have (legitimately) changed compared to the id that was used when this model.field was created
    # for example it might be assigned a new id.
    model.field@source <- source
    
    
    # Check that the spatial extent matches before returning
    # Note that there are two cases to check here (specifically defined extents or just the same ids)
    if(identical(spatial.extent, model.field@spatial.extent) ||
       spatial.extent.id == model.field@spatial.extent.id){
      if(store.internally) {source <<- addToSource(model.field, source)}
      return(model.field)
    }  
    
    # Otherwise we must discard this Field and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of the spatial extent",  spatial.extent.id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(model.field)
    gc()
    
  }
  
  
  
  ############################################################################################################  
  #### NOTE: We don't pass this point if the correct pre-averaged data is available (on disk or in memory) ###
  #### as we will have already returned in CASE 1 or CASE 2 above.                                         ###
  ############################################################################################################
  
  
  
  ### CASE 3 - IF THE WHOLE FILE HAS BEEN READ AND STORED IN MEMORY AS A VEGOBJECT, THEN TAKE THAT AND EXTRACT THE DATA.TABLE AND THEN AVERAGE IT BELOW
  temp.var.string <- paste(source@format, source@id, var.string, "all_years", sep = ".")
  if(var.string %in% names(source@objects) || temp.var.string %in% names(source@objects)){
    
    if(verbose) message(paste(var.string, " is already read, so using that internal copy.", sep = ""))
    if(var.string %in% names(source@objects)) { this.dt <- source@objects[[var.string]]@data }
    else if(temp.var.string %in% names(source@objects)) { this.dt <- source@objects[[temp.var.string]]@data }
    setKeyDGVM(this.dt)
    
  }
  
  
  
  ### CASE 4 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  else {
    
    if(verbose) message(paste("File ", var.string, ".out not already read, so reading it now.", sep = ""))
    
    ### !!! CALL FORMAT SPECIFIC FUNTIONS HERE !!!
    
    # If model is LPJ-GUESS(-SPITFIRE) and the required Quantity is defined for LPJ-GUESS(-SPITFIRE)
    if(source@format == "LPJ-GUESS" | source@format == "LPJ-GUESS-SPITFIRE" ) {
      
      
      # First check if quantity is for FireMIP, if so call a special function with the extra processing required
      if("FireMIP" %in% quant@model) {
        this.dt <- openLPJOutputFile_FireMIP(source, var.string, first.year = first.year, last.year = last.year, verbose = verbose)
      }
      else if("LPJ-GUESS" %in% quant@model | "LPJ-GUESS-SPITFIRE" %in% quant@model) {
        this.dt <- openLPJOutputFile(source, var.string, first.year = first.year, last.year = last.year, verbose = verbose)
      }
      else if("Standard" %in% quant@model) {
        this.dt <- getStandardQuantity_LPJ(source, quant, first.year = first.year, last.year = last.year, verbose = verbose)
      }
      
      
      # Meta-data for use later on
      if("Year" %in% getSTInfo(this.dt)) {
        year.range <- range(this.dt[,Year])
        first.year.present <- year.range[1]
        last.year.present <- year.range[2]
      }
      year.aggregate.method.present = "none"
      
    } # END IF LPJ-GUESS or LPJ-GUESS-SPITFIRE
    
    
    else if(source@format == "DGVMData") {
      
      data.list <- openDGVMDataFile(source, quant, verbose = verbose)
      this.dt <- data.list$dt
      if("first.year" %in% names(data.list)) first.year.present <-  data.list$first.year
      if("last.year" %in% names(data.list)) last.year.present <-  data.list$last.year
      if("year.aggregate.method" %in% names(data.list)) year.aggregate.method.present <-  data.list$year.aggregate.method
      if("spatial.extent" %in% names(data.list)) spatial.extent.present <- data.list$spatial.extent
      if("spatial.extent.id" %in% names(data.list)) spatial.extent.id.present <- data.list$spatial.extent.id
      if("spatial.aggregate.method" %in% names(data.list)) spatial.aggregate.method.present <- data.list$spatial.aggregate.method
      
    }
    
    
    # If model is aDGVM and the required Quantity is defined for aDGVM
    else if(source@format == "aDGVM") {
      
      if("aDGVM" %in% quant@format | "Standard" == quant@format) {
        if(adgvm.scheme == 1) this.dt <- data.table(getQuantity_aDGVM_Scheme1(source, first.year = first.year, last.year= last.year, quant))
        if(adgvm.scheme == 2) this.dt <- data.table(getQuantity_aDGVM_Scheme2(source, first.year = first.year, last.year= last.year, quant))
      }
      #else if(quant@format == "Standard") {
      #  stop("Standard quantities nor currently defined for aDGVM")
      #}
      else {
        stop(paste("Quantity", var.string, "doesn't seem to be defined for aDGVM"))
      }
      
    } # END IF aDGVM
    
    
    # If model is from FireMIP
    else if(source@format == "LPJ-GUESS-SPITFIRE-FireMIP"  ||
            source@format == "LPJ-GUESS-SPITFIRE-OLD-FireMIP"  ||
            source@format == "LPJ-GUESS-SIMFIRE-BLAZE-FireMIP"     ||
            source@format == "LPJ-GUESS-GlobFIRM-FireMIP"  ||
            source@format == "CLM-FireMIP"                 ||
            source@format == "CTEM-FireMIP"                ||
            source@format == "Inferno-FireMIP"             ||
            source@format == "JSBACH-FireMIP"              ||
            source@format == "ORCHIDEE-FireMIP"               ) {
      
      
      if(quant@model == "FireMIP") {
        
        
        
        this.dt <- openFireMIPOutputFile(source, 
                                         var.string, 
                                         quantity = quant, 
                                         first.year = first.year, 
                                         last.year= last.year, 
                                         verbose = verbose)
      }
      else {
        
        stop(paste0("Cannot open quantity", quant, "for FireMIP output, only special 'FireMIP' quantities are defined"))
        
      }
      
      
    }
    
    
    
    else {
      
      stop(paste0("The Quantity ", quant@id, " is defined for models ", paste(quant@format, sep=", ", collapse="") , ", which doesn't include the model that you requested getting output for (", source@format, ").  Please check this."))
      
      
    }
    
    setKeyDGVM(this.dt)
    
    
    
    ### !!! END CALL MODEL SPECIFIC FUNCTIONS !!!
    
    
    ### STORE THE FULL MODEL OUTPUT AS AN UNAVERAGED VEGOBJECT IF REQUESTED
    # Note we gotta do this now before the cropping and averaging below
    if(store.full){
      
      print("Storing full")
      
      
      # if spatial extent and spatial.extent.id not already determined use then set them
      if(!exists("spatial.extent.present")) spatial.extent.present <- extent(this.dt)
      if(!exists("spatial.extent.id.present")) spatial.extent.id.present <- "Full"
      if(!exists("year.aggregate.method.present")) year.aggregate.method.present <- "none"
      
      model.field.full <- new("Field",
                              id = var.string,
                              data = this.dt,
                              quant = quant,
                              spatial.extent = spatial.extent.present,
                              spatial.extent.id = spatial.extent.id.present,
                              first.year = first.year.present,
                              last.year  = last.year.present,
                              year.aggregate.method = year.aggregate.method.present,
                              spatial.aggregate.method = "none",
                              source = as(source, "SourceInfo"))
      
      # name and store
      #names(model.field.full) <- var.string
      source <<- addToSource(model.field.full, source)
      print(source)
      
    } # end if(store.full)
    
  } # end option 4
  
  ### CROP THE SPATIAL EXTENT IF REQUESTED, AND 
  if(!is.null(spatial.extent))  {
    
    # if the provided spatial yields a valid extent, use the crop function
    possible.error <- try ( extent(spatial.extent), silent=TRUE )
    if (class(possible.error) != "try-error") {
      this.dt <- crop(this.dt, spatial.extent)  
      spatial.extent.present <- extent(spatial.extent)
      if(missing(spatial.extent.id)) spatial.extent.id <- "CroppedToExtent"
    }
    
    # else check if some gridcells to be selected with getGridcells
    else if(is.data.frame(spatial.extent) || is.data.table(spatial.extent) || is.numeric(spatial.extent) || is.list(spatial.extent)){
      this.dt <- selectGridcells(this.dt, spatial.extent)
      spatial.extent.present <- spatial.extent
      if(missing(spatial.extent.id)) spatial.extent.id <- "SubsetOfGridcells"
    }
    
    # else fail with error message
    else {
      stop(paste("Trying to select a spatial extent using an object of class", class(spatial.extent)[1], "which isn't really working for me right now.  If this a Spatial* class, contact the authors and they might implement it"))
    }
    
  }
  else {
    
    if(!exists("spatial.extent.present")) spatial.extent.present <- extent(this.dt)
    
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  spatial.extent.present@xmin, ",", spatial.extent.present@xmax, "), Lat = (" ,  spatial.extent.present@ymin, ",", spatial.extent.present@ymax, ").", sep = ""))
    
  }
  
  ### SELECT THE YEARS IF REQUESTED
  got.first.year <- !missing(first.year) & !is.null(first.year)
  got.last.year <- !missing(last.year) & !is.null(last.year)
  if(got.first.year & got.last.year) {
    if(verbose) message(paste("Selecting years from", first.year, "to", last.year, sep = " "))
    this.dt <- selectYears(this.dt, first = first.year, last = last.year) 
  }
  else if(!got.first.year & !got.last.year) {
    if(verbose) message("No year selection being applied")
  }
  else {
    stop("Got to provide both first.year and last.year, or neither of them.")
  }
  
  
  
  ### CHECK THAT WE HAVE A VALID DATA.TABLE
  if(nrow(this.dt) == 0) stop("getField() has produced an empty data.table, so subsequent code will undoubtedly fail.  Please check your input data and the years and spatial.extent that you have requested.")
  
  
  
  ### GET THE YEARS BEFORE THEY MAY BE AVERAGED AWAY
  ordered.years = NULL
  if("Year" %in% names(this.dt)) { 
    ordered.years <- sort(unique(this.dt[,Year]))
    first.year.present <- min(ordered.years)
    last.year.present <- max(ordered.years)
  }
  
  ###  DO SPATIAL AGGREGATION - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
  if(tolower(spatial.aggregate.method) != "none"){
    this.dt <- aggregateSpatial(this.dt, method = spatial.aggregate.method, verbose = verbose)
    if(verbose) {
      message("Head of spatially aggregated data.table:")
      print(utils::head(this.dt))
    }
  }
  
  
  ###  DO YEAR AGGREGATATION
  if(tolower(year.aggregate.method) != "none"){
    this.dt <- aggregateYears(this.dt, method = year.aggregate.method, verbose = verbose)
    if(verbose) {
      message("Head of year aggregated data.table:")
      print(utils::head(this.dt))
    }
  }
  year.aggregate.method.present <- year.aggregate.method
  
  ### IF NO EXTENTS SPECIFIED, GET THE EXTENTS FOR THE FINAL VEGOBJECT TO RETURN
  
  
  
  ### BUILD THE FINAL Field, STORE IT IF REQUESTED AND RETURN IT
  model.field <- new("Field",
                     id = model.field.id,
                     data = this.dt,
                     quant = quant,
                     first.year = first.year.present, 
                     last.year = last.year.present, 
                     year.aggregate.method = year.aggregate.method.present,
                     spatial.extent = spatial.extent.present,
                     spatial.extent.id = spatial.extent.id,
                     spatial.aggregate.method = spatial.aggregate.method,
                     subannual.aggregate.method = "none",
                     subannual.original = "none",
                     source = as(source, "SourceInfo"))
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMField object...")}
    saveRDS(model.field, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  ### ADD TO THE MODELsource OBJECT IF REQUESTED
  if(store.internally) {
    source <<- addToSource(model.field, source)
  }
  
  return(model.field)
  
}
