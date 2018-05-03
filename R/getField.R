#!/usr/bin/Rscript

#
#' Get a \code{Field} from a \code{Source}
#' 
#' Given a \code{Source} object and a \code{Quantity} object, return an appropriate spatially-averaged \code{Field} object. optionally including spatial,
#' yearly and/or subannual cropping and aggregating.
#' 
#' Note that because there are two types of averaging available, the resulting \code{Source} object can either be full spatial-temporal dataset, a spatial-only dataset (map), a temporal only datasey a time-series) or an average across both space and time, i.e. a single number.
#' Also not that the data is stored internal as a data.table object, but this is mostly not important to the user.
#'   
#' @param source The \code{Source} object for which the spatially-averaged \code{ModelField} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param year.aggregate.method A character string describing the method by which to annual aggregate the data.  Leave blank to apply no annual aggregation. Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param spatial.extent An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelField covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param write If TRUE, write the data of the \code{Field} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{ModelField} in the \code{Source} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{ModelFields} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM source, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A \code{Field}. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getField <- function(source, 
                     var, 
                     first.year,
                     last.year,
                     year.aggregate.method, 
                     spatial.extent, 
                     spatial.extent.id, 
                     spatial.aggregate.method,
                     subannual.resolution,
                     subannual.original,
                     subannual.aggregate.method,
                     sta.info,
                     write = FALSE, 
                     read.full = TRUE, 
                     verbose = FALSE, 
                     ...){
  
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
  
  ### TIDY THE RELEVANT STA ARGUMENTS INTO THE TARGET STA OBJECT
  if(missing(sta.info)) {
    sta.info <- new("STAInfo")
    if(!missing(first.year)) sta.info@first.year = first.year
    if(!missing(last.year)) sta.info@last.year = last.year
    if(!missing(year.aggregate.method)) sta.info@year.aggregate.method = year.aggregate.method
    if(!missing(spatial.extent)) sta.info@spatial.extent = spatial.extent
    if(!missing(spatial.extent.id)) sta.info@spatial.extent.id = spatial.extent.id
    if(!missing(spatial.aggregate.method)) sta.info@spatial.aggregate.method = spatial.aggregate.method
    if(!missing(subannual.resolution)) sta.info@subannual.resolution = subannual.resolution
    if(!missing(subannual.original)) sta.info@subannual.original = subannual.original
    if(!missing(subannual.aggregate.method)) sta.info@subannual.aggregate.method = subannual.aggregate.method
  }
  else {
    if(!missing(first.year)) warning("Since 'sta.info' argument has been specified, the 'first.year' argument will be ignored")
    if(!missing(last.year)) warning("Since 'sta.info' argument has been specified, the 'last.year' argument will be ignored")
    if(!missing(year.aggregate.method)) warning("Since 'sta.info' has been argument specified, the 'year.aggregate.method' argument will be ignored")
    if(!missing(spatial.extent)) warning("Since 'sta.info' argument has been specified, the 'spatial.extent' argument will be ignored")
    if(!missing(spatial.extent.id)) warning("Since 'sta.info' argument has been specified, the 'spatial.extent.id' argument will be ignored")
    if(!missing(spatial.aggregate.method)) warning("Since 'sta.info' has been argument specified, the 'spatial.aggregate.method' argument will be ignored")
    if(!missing(subannual.original)) warning("Since 'sta.info' argument has been specified, the 'subannual.original' argument will be ignored")
    if(!missing(subannual.resolution)) warning("Since 'sta.info' argument has been specified, the 'subannual.resolution' argument will be ignored")
    if(!missing(subannual.aggregate.method)) warning("Since 'sta.info' argument has been specified, the 'subannual.aggregate.method' argument will be ignored")
  }
  
  ### MAKE UNIQUE IDENTIFIER OF THIS FIELD VARIABLE AND FILENAME - this describes completely whether we want the files spatially, yearly or subanually aggregated and reduced in extent
  target.field.id <- makeFieldID(source.info = as(source, "SourceInfo"), 
                                 var.string = var.string, 
                                 first.year = sta.info@first.year,
                                 last.year = sta.info@last.year,
                                 year.aggregate.method = sta.info@year.aggregate.method, 
                                 spatial.extent.id = sta.info@spatial.extent.id, 
                                 spatial.aggregate.method = sta.info@spatial.aggregate.method,
                                 subannual.resolution = sta.info@subannual.resolution,
                                 subannual.original = sta.info@subannual.original,
                                 subannual.aggregate.method = sta.info@subannual.aggregate.method
  )
  print(target.field.id)
  
  file.name <- file.path(source@dir, paste(target.field.id, "DGVMField", sep = "."))
  if(verbose) message(paste("Seeking ModelField with id = ", target.field.id, sep = ""))
  
  
  
  
  ### CASE 1 - USE THE PREAVERAGED/CROPPED VEGOBJECT FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  source@dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    model.field <- readRDS(file.name)
    
    # Update the source object, that might have (legitimately) changed compared to the id that was used when this model.field was created
    # for example it might be assigned a new id.
    model.field@source <- source
    
    
    # Check that the spatial extent matches before returning
    # Note that there are two cases to check here (specifically defined extents or just the same ids)
    if(identical(sta.info@spatial.extent, model.field@spatial.extent) ||
       sta.info@spatial.extent.id == model.field@spatial.extent.id){
      return(model.field)
    }  
    
    # Otherwise we must discard this Field and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of the spatial extent",  sta.info@spatial.extent.id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(model.field)
    gc()
    
  }
  
  
  #############################################################################################  
  #### NOTE: We don't pass this point if the correct pre-averaged data is available on disk ###
  #### as we will have already returned in CASE 1  above.                                   ###
  #############################################################################################
  
  
  
  
  ### CASE 2 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  else {
    
    if(verbose) message(paste("File for ", var.string, "not already read (or 'read.full' argument set to TRUE), so reading full data file now.", sep = ""))
    
    data.list <- source@format@getField(source, quant, sta.info, verbose, ...)
    this.dt <- data.list[["dt"]]
    setKeyDGVM(this.dt)
    actual.sta.info <- data.list[[2]]
 
    
    # 
    # else {
    #   
    #   stop(paste0("The Quantity ", quant@id, " is defined for models ", paste(quant@format, sep=", ", collapse="") , ", which doesn't include the model that you requested getting output for (", source@format, ").  Please check this."))
    #       #   
    # }
    # 
    ### !!! END CALL MODEL SPECIFIC FUNCTIONS !!!
    
    
  } # end option 4
  
  ### CROP THE SPATIAL EXTENT IF REQUESTED, AND 
  if(!is.null(sta.info@spatial.extent))  {
    
    # if the provided spatial yields a valid extent, use the crop function
    possible.error <- try ( extent(sta.info@spatial.extent), silent=TRUE )
    if (class(possible.error) != "try-error") {
      this.dt <- crop(this.dt, sta.info@spatial.extent)  
      actual.sta.info@spatial.extent <- extent(sta.info@spatial.extent)
      if(length(sta.info@spatial.extent) == 0 ) actual.sta.info@spatial.extent.id <- "CroppedToExtent"
      else actual.sta.info@spatial.extent.id <- sta.info@spatial.extent.id
      
    }
    
    # else check if some gridcells to be selected with getGridcells
    else if(is.data.frame(sta.info@spatial.extent) || is.data.table(sta.info@spatial.extent) || is.numeric(sta.info@spatial.extent) || is.list(sta.info@spatial.extent)){
      this.dt <- selectGridcells(this.dt, sta.info@spatial.extent)
      actual.sta.info@spatial.extent <- sta.info@spatial.extent
      if(length(sta.info@spatial.extent) == 0 ) actual.sta.info@spatial.extent.id <- "SubsetOfGridcells"
      else actual.sta.info@spatial.extent.id <- sta.info@spatial.extent.id
    }
    
    # else fail with error message
    else {
      stop(paste("Trying to select a spatial extent using an object of class", class(sta.info@spatial.extent)[1], "which isn't really working for me right now.  If this a Spatial* class, contact the authors and they might implement it"))
    }
    
  }
  else {
    
    #if(!exists("spatial.extent.present")) spatial.extent.present <- extent(this.dt)
    
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  spatial.extent.present@xmin, ",", spatial.extent.present@xmax, "), Lat = (" ,  spatial.extent.present@ymin, ",", spatial.extent.present@ymax, ").", sep = ""))
    
  }
  
  
  ### SELECT THE YEARS IF REQUESTED
  if("Year" %in% getDimInfo(this.dt)) {
    
    if(length(sta.info@first.year) == 1) {
      if(sta.info@first.year != actual.sta.info@first.year) {
        first.year <- sta.info@first.year
        crop.first <- TRUE
      }
      else {
        first.year <- actual.sta.info@first.year
        crop.first <- FALSE
      }
    }
    
    if(length(sta.info@last.year) == 1) {
      if(sta.info@last.year != actual.sta.info@last.year) {
        last.year <- sta.info@last.year
        crop.last <- TRUE
      }
      else {
        last.year <- actual.sta.info@last.year
        crop.last <- FALSE
      }
    }
    
    if(crop.first || crop.last) {
      
      if(verbose) message(paste("Selecting years from", first.year, "to", last.year, sep = " "))
      this.dt <- selectYears(this.dt, first = first.year, last = last.year) 
      
      # update meta-data
      years.present <- sort(unique(this.dt[["Year"]]))
      actual.sta.info@first.year <- min(years.present)
      actual.sta.info@last.year <- max(years.present)
      
    }
    else {
      if(verbose) message("No year selection being applied")
    }
    
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
  if(sta.info@spatial.aggregate.method != actual.sta.info@spatial.aggregate.method &&
     length(sta.info@spatial.aggregate.method) > 0 ){
    
    this.dt <- aggregateSpatial(this.dt, method = sta.info@spatial.aggregate.method, verbose = verbose)
    
    # update meta-data and report  
    actual.sta.info@spatial.aggregate.method <- sta.info@spatial.aggregate.method
    if(verbose) {
      message("Head of spatially aggregated data.table:")
      print(utils::head(this.dt))
    }
  }
  
  
  ###  DO YEAR AGGREGATATION
  if(length(sta.info@year.aggregate.method) > 0){
    if("Year" %in% getDimInfo(this.dt, "names")){
      
      this.dt <- aggregateYears(this.dt, method = sta.info@year.aggregate.method, verbose = verbose)
      
      # update meta-data and report  
      actual.sta.info@year.aggregate.method <- sta.info@year.aggregate.method
      if(verbose) {
        message("Head of year aggregated data.table:")
        print(utils::head(this.dt))
      }
    }
  }
  
  
  ###  DO SUBANNUAL AGGREGATION
  if(length(sta.info@subannual.resolution) > 0 ){
    
    # get the original subannual resolution
    these.dims <- getDimInfo(this.dt, info = "names")
    if("Day" %in% these.dims) actual.sta.info@subannual.original <- "Daily"
    if("Month" %in% these.dims) actual.sta.info@subannual.original <- "Monthly"
    else actual.sta.info@subannual.original <- "Annual"
    
    if(sta.info@subannual.resolution != actual.sta.info@subannual.resolution){
      
      if(length(sta.info@subannual.aggregate.method) == 0) {
        stop(paste0("Please provide a subannual.aggregate.method if you want to aggraget the data from ", actual.sta.info@subannual.original, " to ", sta.info@subannual.resolution))
      }
      
      this.dt <- aggregateSubannual(this.dt, method = sta.info@subannual.aggregate.method, target = sta.info@subannual.resolution, verbose = verbose)
      
      # update meta-data and report  
      actual.sta.info@subannual.aggregate.method <- sta.info@subannual.aggregate.method
      actual.sta.info@subannual.resolution <- sta.info@subannual.resolution
      if(verbose) {
        message("Head of year aggregated data.table:")
        print(utils::head(this.dt))
      }
      
    }
  }
  
  
  
  
  ### IF NO EXTENTS SPECIFIED, GET THE EXTENTS FOR THE FINAL VEGOBJECT TO RETURN
  
  
  
  ### BUILD THE FINAL Field, STORE IT IF REQUESTED AND RETURN IT
  model.field <- new("Field",
                     id = target.field.id,
                     data = this.dt,
                     quant = quant,
                     actual.sta.info,
                     # first.year = first.year.present, 
                     # last.year = last.year.present, 
                     # year.aggregate.method = year.aggregate.method.present,
                     # spatial.extent = actual.sta.info@spatial.extent,
                     # spatial.extent.id = actual.sta.info@spatial.extent,
                     # spatial.aggregate.method = spatial.aggregate.method,
                     # subannual.aggregate.method = "none",
                     # subannual.original = "none",
                     source = as(source, "SourceInfo"))
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMField object...")}
    saveRDS(model.field, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  return(model.field)
  
}
