#!/usr/bin/Rscript

#
#' Get a \code{Field} from a \code{Source}
#' 
#' Given a \code{Source} object and a \code{Quantity} object, return an appropriate spatially/temporal/annually-aggregated \code{Field} object. optionally including
#' spatial, temporal and annual cropping.
#' 
#' Note that because there are three types of aggregating available, the resulting \code{Field} object can a wide select of spatio-temporal dimensions.
#' To check what dimensions you have you can use \code{\link{getDimInfo}}  
#' 
#' @param source The \code{Source} object for which the spatially-averaged \code{ModelField} should be built (eg. "lai")
#' @param scheme The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param sta.info Optionally an STAInfo object defining the exact spatial-temporal-annual domain over which the data should be retrieved.  
#' Can also be a Field object from which the STA info will de derived.
#' If specified the following 9 arguments are ignored (with a warning)
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param year.aggregate.method A character string describing the method by which to annual aggregate the data.  Leave blank to apply no annual aggregation. Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' See \code{\link{aggregateYears}} 
#' @param spatial.extent An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelField covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' See \code{\link{aggregateSpatial}} 
#' @param subannual.resolution A character string specifying the subannual resolution that you want to the data on.  Can be "Year", "Month" or "Day".
#' @param subannual.aggregate.method A character string specifying the method by which to aggragte the data subannually,  can be "mean", "sum", "max", "min", "sd" or "var".
#' See \code{\link{aggregateSubannual}} 
#' @param subannual.original A character string specifying the subannual you want the data to be on before applying the subannual.aggregate.method. 
#' Can be "Year", "Month" or "Day".  Currently ignored.
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param read.full.components If TRUE ignore any pre-averaged file for the *component* variables on disk, if FALSE use thjm if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param write If TRUE, write the data of the \code{Field} to disk as text file.
#' @param write.components If TRUE, write the data of the *component* \code{Field}s to disk as text file.
#' @param ...  Other arguments that are passed to the getField function for the specific Format.  Currently this is only 'adgvm.scheme' (for the aDGVM Format) which can be 1 or 2.
#' 
#' @return A \code{Field}. 
#' @seealso \code{\link{aggregateSubannual}}, \code{\link{aggregateSpatial}}, \code{\link{aggregateYears}}, \code{\link{getDimInfo}}   
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getBiomes <- function(source, 
                      scheme, 
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
                      write.components = FALSE,
                      read.full = TRUE, 
                      read.full.components = TRUE, 
                      verbose = FALSE, 
                      ...){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  ### CHECK ARGUEMENTS
  
  
  ### CONVERT SCHEME ID STRING TO A BIOME SCHEME
  if(class(scheme) == "character") {
    scheme.string <- scheme
    scheme <- byIDfromList(scheme, supported.biome.schemes)
  }
  else {
    scheme <- scheme
    scheme.string <- scheme@id
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
    if(is.Field(sta.info)) sta.info <- as(sta.info, "STAInfo")
    if(!missing(first.year)) warning("Since 'sta.info' argument has been specified, the 'first.year' argument will be ignored")
    if(!missing(last.year)) warning("Since 'sta.info' argument has been specified, the 'last.year' argument will be ignored")
    if(!missing(year.aggregate.method)) warning("Since 'sta.info' argument has been specified, the 'year.aggregate.method' argument will be ignored")
    if(!missing(spatial.extent)) warning("Since 'sta.info' argument has been specified, the 'spatial.extent' argument will be ignored")
    if(!missing(spatial.extent.id)) warning("Since 'sta.info' argument has been specified, the 'spatial.extent.id' argument will be ignored")
    if(!missing(spatial.aggregate.method)) warning("Since 'sta.info' has been argument specified, the 'spatial.aggregate.method' argument will be ignored")
    if(!missing(subannual.original)) warning("Since 'sta.info' argument has been specified, the 'subannual.original' argument will be ignored")
    if(!missing(subannual.resolution)) warning("Since 'sta.info' argument has been specified, the 'subannual.resolution' argument will be ignored")
    if(!missing(subannual.aggregate.method)) warning("Since 'sta.info' argument has been specified, the 'subannual.aggregate.method' argument will be ignored")
  }
  
  
  ### CATCH AWKWARD CASE
  ### Fail when a spatial.extent is specified but no spatial.extent.id
  if(!missing(spatial.extent) && missing(spatial.extent.id)){
    stop("Please specify a spatial.extent.id when specifying a spatial.extent (just a simple character string).  This is to maintain metadata integrity.")
  }
  
  
  ### MAKE UNIQUE IDENTIFIER OF THIS FIELD VARIABLE AND FILENAME - this describes completely whether we want the files spatially, yearly or subanually aggregated and reduced in extent
  target.field.id <- makeFieldID(source = source, 
                                 var.string = scheme.string, 
                                 sta.info = sta.info)
  
  file.name <- file.path(source@dir, paste(target.field.id, "DGVMField", sep = "."))
  if(verbose) message(paste("Seeking ModelField with id = ", target.field.id, sep = ""))
  
  
  
  
  ### CASE 1 - USE THE PREAVERAGED/CROPPED FIELD FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  source@dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    model.field <- readRDS(file.name)
    
    # Update the source object, that might have (legitimately) changed compared to the id that was used when this model.field was created
    # for example it might be assigned a new id.
    model.field@source <- source
    
    
    # Check that the spatial extent matches before returning
    # Note that there are two cases to check here (specifically defined extents or just the same ids)
    
    
    full.domain.matched <- FALSE
    if(length(sta.info@spatial.extent) == 0 && model.field@spatial.extent.id == "Full") {
      full.domain.matched <- TRUE
      if(verbose) message("Full domain matched.")
    }
    
    cropped.domain.matched <- FALSE
    if(length(sta.info@spatial.extent) > 0 && length(model.field@spatial.extent)  > 0 ) {
      if(identical(sta.info@spatial.extent, model.field@spatial.extent)){
        cropped.domain.matched <- TRUE
        if(verbose) message("Cropped domain matched.")
      }
    }
    
    if(full.domain.matched || cropped.domain.matched){
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
  if(verbose) message(paste("Field ", target.field.id, " not already saved (or 'read.full' argument set to TRUE), so reading full data file to create the field now.", sep = ""))
  
  
  # lookup the quantities required
  all.quantities <- list()
  for(this.layer in scheme@layers.needed){
    if(!this.layer$quantity %in% all.quantities) all.quantities <- append(all.quantities, this.layer$quantity)
  }
  
  # get the fields and layers required
  all.fields <- list()
  for(this.quantity in all.quantities){
    
    # get the raw field
    this.field <- getField(source = source, var = this.quantity, sta.info = sta.info, read.full = read.full.components, write = write.components, ...)
    
    # calculate the layers required
    for(this.layer in scheme@layers.needed){
      
      
      these.arguments <- list(x=this.field, 
                              operator = this.layer$operator, 
                              layers = this.layer$layers)
      if("new.layer" %in% names(this.layer)){
        these.arguments[["new.layer"]] = this.layer$new.layer
      }
      
      
      if(this.layer$quantity == this.quantity) this.field <- do.call(layerOp, args = these.arguments)
    } 
    
    renameLayers(this.field, names(this.field), paste(this.quantity, names(this.field), sep = "_"))
    
    # save to the list
    all.fields[[this.quantity]] <- this.field
    
  }
  
  dt <- all.fields[[1]]@data
  
  if(length(all.fields) > 1){
    for(counter in 1:length(all.fields)){
      dt <- dt[all.fields[[counter]]@data]
    }
  }
  
  
  print("Starting classification")
  suppressWarnings(dt[, scheme@id := as.factor(apply(dt[,,with=FALSE],FUN=scheme@rules,MARGIN=1))])
  this.dt <- dt[,append(getDimInfo(dt), scheme@id), with = FALSE]
  setKeyDGVM(this.dt)
  actual.sta.info <- as(object = all.fields[[1]], Class = "STAInfo")
  
  # tidy up
  rm(dt)
  rm(all.fields)
  gc()
  
  
  
  ### MF: I the rest is carried over from getField(), so I think that only the return statements is actually needed (because the STA info 
  ### should have been sorted out by the previous getField() calls in this function)
  
  
  ### CROP THE SPATIAL EXTENT IF REQUESTED
  if(!is.null(sta.info@spatial.extent))  {
    
    # if the provided spatial yields a valid extent, use the crop function
    possible.error <- try ( extent(sta.info@spatial.extent), silent=TRUE )
    if (class(possible.error) != "try-error") {
      this.dt <- crop(this.dt, sta.info@spatial.extent, sta.info@spatial.extent.id)  
      actual.sta.info@spatial.extent <- extent(sta.info@spatial.extent)
      actual.sta.info@spatial.extent.id <- sta.info@spatial.extent.id
      
    }
    
    # else check if some gridcells to be selected with getGridcells
    else if(is.data.frame(sta.info@spatial.extent) || is.data.table(sta.info@spatial.extent) || is.numeric(sta.info@spatial.extent) || is.list(sta.info@spatial.extent)){
      this.dt <- selectGridcells(this.dt, sta.info@spatial.extent)
      actual.sta.info@spatial.extent <- sta.info@spatial.extent
      actual.sta.info@spatial.extent.id <- sta.info@spatial.extent.id
    }
    
    # else fail with error message
    else {
      stop(paste("Trying to select a spatial extent using an object of class", class(sta.info@spatial.extent)[1], "which isn't really working for me right now.  If this a Spatial* class, contact the authors and they might implement it"))
    }
    
  }
  else {
    
    actual.sta.info@spatial.extent.id <- "Full"
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  actual.sta.info@spatial.extent@xmin, ",", actual.sta.info@spatial.extent@xmax, "), Lat = (" ,  actual.sta.info@spatial.extent@ymin, ",", actual.sta.info@spatial.extent@ymax, ").", sep = ""))
    
  }
  
  
  ### SELECT THE YEARS IF REQUESTED
  if("Year" %in% getDimInfo(this.dt)) {
    
    crop.first <- FALSE
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
    
    crop.last <- FALSE
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
  
  
  ###  DO SPATIAL AGGREGATION - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
  if(sta.info@spatial.aggregate.method != "none") {
    
    if(sta.info@spatial.aggregate.method != actual.sta.info@spatial.aggregate.method){
      
      this.dt <- aggregateSpatial(this.dt, method = sta.info@spatial.aggregate.method, verbose = verbose)
      
      # update meta-data and report  
      actual.sta.info@spatial.aggregate.method <- sta.info@spatial.aggregate.method
      if(verbose) {
        message("Head of spatially aggregated data.table:")
        print(utils::head(this.dt))
      }
    }
  }
  
  
  ###  DO YEAR AGGREGATATION
  if(sta.info@year.aggregate.method != "none"){
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
    if("Day" %in% these.dims) actual.sta.info@subannual.original <- "Day"
    if("Month" %in% these.dims) actual.sta.info@subannual.original <- "Month"
    else actual.sta.info@subannual.original <- "Year"
    
    if(sta.info@subannual.resolution != actual.sta.info@subannual.resolution){
      
      if(sta.info@subannual.aggregate.method == "none") {
        stop(paste0("Please provide a subannual.aggregate.method if you want to aggregate the data from ", actual.sta.info@subannual.original, " to ", sta.info@subannual.resolution))
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
  
  
  
  ### BUILD THE FINAL Field, STORE IT IF REQUESTED AND RETURN IT
  model.field <- new("Field",
                     id = target.field.id,
                     data = this.dt,
                     quant = as(object = scheme, Class = "Quantity"),
                     actual.sta.info,
                     source = source)
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMField object...")}
    saveRDS(model.field, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  return(model.field)
  
}
