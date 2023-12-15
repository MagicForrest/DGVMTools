#!/usr/bin/Rscript

#
#' Get a \code{\linkS4class{Field}} from a \code{\linkS4class{Source}}
#' 
#' Given a \code{\linkS4class{Source}} object and a \code{\linkS4class{Quantity}} object, return an appropriate spatially/temporal/annually-aggregated \code{\linkS4class{Field}} object, optionally including
#' spatial, temporal and annual cropping.
#' 
#' Note that because there are three types of aggregating available, the resulting \code{\linkS4class{Field}} object can have a wide select of spatio-temporal dimensions.
#' To check what dimensions you have you can use \code{\link{getDimInfo}}  
#' 
#' @param source The \code{\linkS4class{Source}} object for which the \code{\linkS4class{Field}} should be built, typically a model run or a dataset.
#' @param quant The \\code{\linkS4class{Quantity}} to be read - either a \code{\linkS4class{Quantity}} object or a string containing its \code{id}.  If it is a character string it will be checked
#' against the predefined \code{Quantities} in the \code{\linkS4class{Source}} object and failing that a simple dummy \code{\linkS4class{Quantity}} will be be made.  For the \code{NetCDF Format}
#' most \code{\linkS4class{Quantity}} metadata will be taken from the NetCDF file where possible (thus overriding this option).
#' @param layers A list (or vector of character) of character strings to specify which Layers should be read from the file.  
#' If missing or NULL then all Layers are read.
#' -NOTE- Using this arguments is not recommended for reading gzipped output with the \code{GUESS} Format since it involves gunzipping the file twice, 
#' which likely makes it less time efficient than simply reading all the layers and then dropping the ones you don't want.   
#' @param file.name Character string specifying the file name (not the full path, just the file name, or the file name relative to source@@dir) where the data is stored.
#' For the \code{GUESS}, \code{aDGVM} and \code{aDGVM2} Formats this is optional under normal circumstances this is optional since the file names are normally 
#' standardised (although they have been renamed). However for the \code{NetCDF} Format this is pretty much always essential because random netCDF files don't tend to have 
#' standardised file names in the same way that model output does.  Leave missing or set to \code{NULL} to use the standard file name for the particular Format.
#' @param sta.info Optionally an \code{\linkS4class{STAInfo}} object defining the exact spatial-temporal-annual domain over which the data should be retrieved.  
#' Can also be a Field object from which the STA info will de derived.
#' If specified the following 9 arguments are ignored (with a warning)
#' @param first.year The first year (as a numeric) of the data to be returned (if not specified or NULL start from the beginning of the data set)
#' @param last.year The last year (as a numeric) of the data to be returned (if not specified or NULL take the data to the end of the data set)
#' @param year.aggregate.method A character string describing the method by which to annual aggregate the data.  Leave blank to apply no annual aggregation. Can currently be "mean", "sum", "max", "min", "sd", "var and "cv" (= coefficient of variation: sd/mean).
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' See \code{\link{aggregateYears}} 
#' @param spatial.extent An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this Field covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank to apply no spatial aggregation. Can currently be "weighted.mean"/"w.mean", "mean", 
#' "weighted.sum"/"w.sum", "sum", "max", "min", "sd", "var and "cv" (= coefficient of variation: sd/mean).  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' See \code{\link{aggregateSpatial}} 
#' @param subannual.resolution A character string specifying the subannual resolution that you want.  Can be "Year", "Month" or "Day".
#' @param subannual.aggregate.method A character string specifying the method by which to aggregate the data subannually,  can be "mean", "sum", "max", "min", "sd", "var and "cv" (= coefficient of variation: sd/mean)
#' See \code{\link{aggregateSubannual}} 
#' @param subannual.original A character string specifying the subannual you want the data to be on before applying the subannual.aggregate.method. 
#' Can be "Year", "Month" or "Day".  Currently ignored.
#' @param quick.read.file A character string.  If set, the function will look for a file of this name (plus the extension ".RData") in the run directory.  
#' If if finds one then it reads that.  If it doesn't find the appropriate file, then it reads the raw data, performs whatever cropping and aggregating is necessary,
#' and then saves the data in a file of that name (in the run directory) for use next time.  (Note: the function also checks that the specified layers, cropping and aggregating 
#' found in the file match that which was requested by the arguments here).
#' @param quick.read.autodelete If TRUE then the file specified by the above "quick.read.file" argument will be deleted, thus ensuring that the raw data will be read afresh 
#' (and saved again).  Ignored if valid "quick.read.file" argument not supplied.
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param ...  Other arguments that are passed to the getField function for the specific Format or additional arguements for selecting space/time/years.  
#' For all Formats, the followings arguments apply:
#' \itemize{
#'  \item{\code{cover.fraction}}  When selecting gridcells based on a SpatialPolygonsDataFrame (ie from a shapefile) as the \code{spatial.extent} argument, this optional arguement determines 
#'  how much of the gridcell needs to be in the the polygon for it to be selected. Should be between 0 and 1.
#' }
#' For the aDGVM(1) Format, the following arguments apply:
#' \itemize{
#'  \item{\code{adgvm.file.type}}  This character string argument specifies from which file to read the data.
#'  This can be one of "Yearly", "Sys", "Fire", "Soil" or "Size".  The default is "Yearly", which is sensible because the yearly file is always written. 
#'  \item{\code{adgvm.fire}}  This numeric argument (taking values 0 or 1) specifies to take a run with fire on (1) or off (0). Default is 1.
#'  \item{\code{adgvm.climate}}   This numeric argument (taking values 0 or 1) specifies to take a run with constant (0) or transient (1) climate.  Default is 0.
#'  \item{\code{adgvm.header}} If your aDGVM run has been has extra columns added to the output tables, use this argument
#'  to specify the column names.  For the default column names see the source file \code{aDGVM-Format.R}.
#' }
#' For the aDGVM2 Format, the following arguments apply:
#' \itemize{
#'  \item{\code{adgvm2.scheme}}  This numeric argument defines the aDGVM PFT scheme which can be 1 or 2.
#'  \item{\code{adgvm2.daily}}  A logical, set to true to read daily data (only for \code{adgvm2.scheme=1} and if daily data are provided in pop file)
#' }
#' For the NetCDF Format, the following arguments apply:
#'  \itemize{
#'  \item{\code{nc.verbose}} A logical, set to true to give progress/debug information from the ncdf4 package functions.  This can be a lot, 
#' so it is handy to control that separately.
#'  \item{\code{calendar}} Character string, sometimes the calendar string on the time axis can be incorrect or missing.  Here you can manually provide it.
#' Note: A common error in paleo files is "standard" instead of "proleptic_gregorian". Specifically, if you have dates with years before 1582 
#' (the start of the Gregorian calendar) and it includes leap years the calendar needs to be set to "proleptic_gregorian".
#' }
#'  
#' @return A \code{\linkS4class{Field}}. 
#' @seealso \code{\link{aggregateSubannual}}, \code{\link{aggregateSpatial}}, \code{\link{aggregateYears}}, \code{\link{getDimInfo}}   
#' @include classes.R
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getField <- function(source, 
                     quant, 
                     layers = NULL,
                     file.name = NULL,
                     first.year = NULL,
                     last.year = NULL,
                     year.aggregate.method = NULL, 
                     spatial.extent = NULL, 
                     spatial.extent.id = NULL, 
                     spatial.aggregate.method = NULL,
                     subannual.resolution = NULL,
                     subannual.original = NULL,
                     subannual.aggregate.method = NULL,
                     sta.info = NULL,
                     quick.read.file = NULL, 
                     quick.read.autodelete = FALSE, 
                     verbose = FALSE, 
                     ...){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  ### CHECK ARGUMENTS
  if(!(missing(first.year) | is.null(first.year)) & !(missing(last.year) | is.null(last.year)) ) {
    if(first.year > last.year) stop("first.year cannot be greater than last.year!")
  }
  if(!missing(layers) && !is.character(layers) && !is.null(layers)) stop("The 'layers' argument must be a character string or a list of character strings.")
  
  
  ## QUICK READ ARGUMENTS AND AUTODELETE
  # check validity of quick.read.file argument
  valid.quick.read.file <- !is.null(quick.read.file) && is.character(quick.read.file)  && length(quick.read.file) > 0 && !identical(quick.read.file, "")
  if(!valid.quick.read.file && !is.null(quick.read.file)) {
    warning("Invalid 'quick.read.file' file argument specified, quick reading (or saving of file) will not be done,")
  }
  # if vali file then make the preprocessed file name and do autodelete (if specified)
  if(valid.quick.read.file) {
    preprocessed.file.name <- file.path(source@dir, paste(quick.read.file, "RData", sep = "."))
    if(quick.read.autodelete && file.exists(preprocessed.file.name))  {
      if(verbose) message(paste("Auto-deleting file"), preprocessed.file.name)
      file.remove(preprocessed.file.name)
    }
  }
  
  ### CONVERT STRING TO QUANTITY
  if(is.character(quant)) {
    
    quant.id <- quant
    
    # if no quantity it found, make a dummy quantity based on the input var
    quant <- tryCatch(
      {
        lookupQuantity(quant.id, source@format)
      },
      error= function(cond){
        new("Quantity",
            id = quant.id,
            name = quant.id,
            units = "undefined unit",
            colours = viridis::viridis,
            format = c(source@format@id),
            standard_name = quant.id)
      },
      warning=function(cond) {
      },
      finally={}
    )    
    
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
  
  
  ### CASE 1 - CHECK FOR PREPROCESSED FIELD FROM DISK IF AVAILABLE AND OPTION ISN'T DISABLED
  if(valid.quick.read.file) {
    
    if(verbose) message(paste0("Seeking file on disk for quick reading at location: ", preprocessed.file.name))
    
    # check for the file on disk and if found proceed
    if(file.exists(preprocessed.file.name)) { 
      
      # get the object from disk
      if(verbose) {message(paste("File",  preprocessed.file.name, "found in",  source@dir, "so reading it from disk and checking it.",  sep = " "))}    
      # read the model data with a 
      
      successful.read <- FALSE
      successful.read <- tryCatch(
        {
          preprocessed.field <- readRDS(preprocessed.file.name)
          TRUE
        },
        error= function(cond){
          print(cond)
          warning(paste0("Reading of file ", preprocessed.file.name, " failed, so re-reading the raw data."))
          FALSE
        },
        warning=function(cond) {
          print(cond)
          warning(paste0("Reading of file ", preprocessed.file.name, " gave a warning, so for peace of mind I am re-reading the raw data."))
          FALSE
        },
        finally={}
      )    
      
      if(successful.read) {
        
        # Update the source object, that might have (legitimately) changed compared to the id that was used when this preprocessed.field was created
        # for example it might be assigned a new name.
        preprocessed.field@source <- source
        
        ###  Run check function, and if passed return the preprocessed Field 
        if(verbose) message("*** Checking dimensions of pre-processed file ***")
        sta.info.matches <- checkSTAMatches(sta.info, as(preprocessed.field, "STAInfo"), verbose)
        if(verbose) {
          if(sta.info.matches) message("*** Preprocessed file matches in terms of spatial and temporal extent. ***")
          else message("*** Preprocessed file does NOT match in terms of spatial and temporal extent. ***")
        }
        
        # check layers (if specified)
        layers.match <- TRUE
        if(!missing(layers) && !is.null(layers)) {
          layers.match <- identical(sort(layers), sort(layers(preprocessed.field)))
          if(verbose) {
            if(layers.match) message("*** Preprocessed file matches in terms of layers present. ***")
            else {
              message("*** Preprocessed file does NOT match in terms of layers present. ***")
              message(paste0("   -> Layers requested: ", paste0(sort(layers), collapse = ",")))
              message(paste0("   -> Layers found: ", paste0(sort(layers(preprocessed.field)), collapse = ",")))
            }
          }
        }
        else {
          if(verbose) message(paste0(" ** Argument layers not specified so not checked.  I found '", paste(sort(layers(preprocessed.field)), collapse = ","), "', perhaps check these are the layers you wanted."))
          warning(paste0(" ** Argument layers not specified so not checked.  I found '", paste0(sort(layers(preprocessed.field)), collapse = ","), "', perhaps check these are the layers you wanted."))
        }
        
        
        # if all good return the pre-processed file
        if(layers.match && sta.info.matches) return(preprocessed.field)
        # else clean up and proceed to read the raw data
        else rm(preprocessed.field)
        
      } # if successful read
    } # if file processed file found on disk
  } # if got a valid quick.read.file
  
  
  #############################################################################################  
  #### NOTE: We don't pass this point if the correct pre-averaged data is available on disk ###
  #### as we will have already returned in CASE 1  above.                                   ###
  #############################################################################################
  
  
  ### CASE 2 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  if(verbose) {
    if(is.null(quick.read.file)) message(paste("The 'quick.read.file' argument was set to not set, so reading raw data to create the Field now.", sep = ""))
    else if(!is.null(quick.read.file) && !file.exists(paste(preprocessed.file.name))) message(paste("Field ", quick.read.file, " not present, so reading full data file to create the Field now.", sep = ""))
    else {
      message(paste("*** Reading of preprocessed file failed ***"))
      message(paste("So file on disk ignored and the original data is being re-read and new preprocessed file will be written."))
    }
  }
  
  this.Field <- source@format@getField(source, quant, layers, sta.info, file.name, verbose, ...)
  actual.sta.info <- as(this.Field, "STAInfo")
  
  
  
  ### CROP THE SPATIAL EXTENT IF REQUESTED
  if(!is.null(sta.info@spatial.extent) && sta.info@spatial.extent.id != actual.sta.info@spatial.extent.id)  {
    
    # if the provided spatial yields a valid extent, use the crop function
    possible.error <- try ( extent(sta.info@spatial.extent), silent=TRUE )
    # note that data.tables *do* return a valid extent, but we don't want to crop with that here (hence the second condition)
    if (!inherits(possible.error, "try-error") && !is.data.table(spatial.extent)) {
      this.Field <- crop(x = this.Field, y = sta.info@spatial.extent, spatial.extent.id = sta.info@spatial.extent.id)  
    }
    
    # else check if some gridcells to be selected with getGridcells
    else if(is.data.frame(sta.info@spatial.extent) || is.data.table(sta.info@spatial.extent) || is.numeric(sta.info@spatial.extent) || class(sta.info@spatial.extent)[1] == "SpatialPolygonsDataFrame"){
      this.Field <- selectGridcells(x = this.Field, gridcells = sta.info@spatial.extent, spatial.extent.id = sta.info@spatial.extent.id, ...)
    }
    
    # else fail with error message
    else {
      stop(paste("Trying to select a spatial extent using an object of class", class(sta.info@spatial.extent)[1], "which isn't really working for me right now.  If this a Spatial* class, contact the authors and they might implement it"))
    }
    
  }
  else if(is.null(sta.info@spatial.extent)) {
    
    if(verbose) message(paste("No spatial extent specified, using full spatial extent of simulation: Lon = (",  actual.sta.info@spatial.extent@xmin, ",", actual.sta.info@spatial.extent@xmax, "), Lat = (" ,  actual.sta.info@spatial.extent@ymin, ",", actual.sta.info@spatial.extent@ymax, ").", sep = ""))
    
  }
  
  
  ### SELECT THE YEARS IF REQUESTED
  if("Year" %in% getDimInfo(this.Field)) {
    
    call.selectYear <- FALSE
    
    # check if cropping needs to be done based on first year
    if(length(sta.info@first.year) == 1 && sta.info@first.year != actual.sta.info@first.year) {
        call.selectYear <- TRUE
        first.year <- sta.info@first.year
    }
    else first.year <- actual.sta.info@first.year
    
    # check if cropping needs to be done based on last year
    if(length(sta.info@last.year) == 1 && sta.info@last.year != actual.sta.info@last.year) {
      call.selectYear <- TRUE
      last.year <- sta.info@last.year
    }
    else last.year <- actual.sta.info@last.year
  
    # 
    if(call.selectYear) {
      if(verbose) message(paste("Selecting years from", first.year, "to", last.year, sep = " "))
      this.Field <- selectYears(this.Field, first = first.year, last = last.year) 
    }

  }
  
  ### CHECK THAT WE HAVE A VALID DATA.TABLE
  if(nrow(this.Field@data) == 0) stop("getField() has produced an empty data.table, so subsequent code will undoubtedly fail.  Please check your input data and the years and spatial.extent that you have requested.")
  
  ###  DO SPATIAL AGGREGATION - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
  if(sta.info@spatial.aggregate.method != "none") {
    
    if(sta.info@spatial.aggregate.method != actual.sta.info@spatial.aggregate.method){
      
      if(verbose) message(paste("Spatially aggregating by method" , sta.info@spatial.aggregate.method, sep = " "))
      
      this.Field <- aggregateSpatial(this.Field, method = sta.info@spatial.aggregate.method, verbose = verbose, ...)
      
      if(verbose) {
        message("Head of spatially aggregated data.table:")
        print(utils::head(this.Field@data))
      }
    }
  }
  
  ###  DO YEAR AGGREGATATION
  if(sta.info@year.aggregate.method != "none"){
    if("Year" %in% getDimInfo(this.Field, "names")){
      
      this.Field <- aggregateYears(this.Field, method = sta.info@year.aggregate.method, verbose = verbose)
      
      if(verbose) {
        message("Head of year aggregated data:")
        print(utils::head(this.Field@data))
      }
    }
  }
  
  ###  DO SUBANNUAL AGGREGATION
  if(length(sta.info@subannual.resolution) > 0 ){
    
    # get the original subannual resolution
    these.dims <- getDimInfo(this.Field, info = "names")
    if("Day" %in% these.dims) actual.sta.info@subannual.original <- "Day"
    if("Month" %in% these.dims) actual.sta.info@subannual.original <- "Month"
    else actual.sta.info@subannual.original <- "Year"
    
    if(sta.info@subannual.resolution != actual.sta.info@subannual.resolution){
      
      if(sta.info@subannual.aggregate.method == "none") {
        stop(paste0("Please provide a subannual.aggregate.method if you want to aggregate the data from ", actual.sta.info@subannual.original, " to ", sta.info@subannual.resolution))
      }
      
      this.Field <- aggregateSubannual(this.Field, method = sta.info@subannual.aggregate.method, target = sta.info@subannual.resolution, verbose = verbose)
      
      if(verbose) {
        message("Head of year aggregated data:")
        print(utils::head(this.Field@data))
      }
      
    }
  }
  
  
  ### WRITE THE FIELD TO DISK AS AN RData OBJECT IF REQUESTED
  if(valid.quick.read.file) {
    if(verbose) {message("Saving as a .RData object...")}
    saveRDS(this.Field, file = preprocessed.file.name)
    if(verbose) {message("...done.")}
  }
  
  # clean up and return
  gc()
  return(this.Field)
  
}
