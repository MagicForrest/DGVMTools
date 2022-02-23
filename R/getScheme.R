#!/usr/bin/Rscript

#
#' Get a Classification \code{Field} from a \code{Source}
#' 
#' Given a \code{Source} object and a \code{Scheme} object, return an appropriate spatially/temporal/annually-aggregated \code{Field} object with categorical values u based on the 
#' classification contained in the \code{Scheme}, optionally including spatial, temporal and annual cropping.
#' 
#' Note that because there are three types of aggregating available, the resulting \code{Field} object can a wide select of spatio-temporal dimensions.
#' To check what dimensions you have you can use \code{\link{getDimInfo}}  
#' 
#' @param source The \code{Source} object for which the Classification \code{Field} should be built. A list of \code{Source} objects can also be provided, in which case 
#' the required \code{Field} object derived from each \code{Source} in the list (for example LAI or fractional cover) are first averaged, and then do the biome 
#' classification. In the case of a list of \code{Source} objects, the \code{averaged.source} arguments (see below) must also be supplied so that the averaged 
#' data can be written to/read from disk.
#' @param scheme The classification scheme to be used as \code{Scheme} object, or the id of a \code{Scheme} defined in the package, as a character string.
#' @param sta.info Optionally an STAInfo object defining the exact spatial-temporal-annual domain over which the data should be retrieved.  
#' Can also be a Field object from which the STA info will de derived.
#' If specified the following 9 arguments are ignored (with a warning).
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param year.aggregate.method A character string describing the method by which to annual aggregate the data.  Leave blank to apply no annual aggregation. Can currently be "mean", "sum", "max", "min", "mode", "median", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!  Note that this aggregation is done before the Scheme is calculated.
#' See \code{\link{aggregateYears}} 
#' @param spatial.extent An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelField covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "mode", "median", weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' Note that this aggregation is done before the Scheme is calculated.
#' See \code{\link{aggregateSpatial}} 
#' @param subannual.resolution A character string specifying the subannual resolution that you want to the data on.  Can be "Year", "Month" or "Day".
#' @param subannual.aggregate.method A character string specifying the method by which to aggragte the data subannually,  can be "mean", "sum", "max", "min", "mode", "median", "sd" or "var".
#' Note that this aggregation is done before the Scheme is calculated.
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
#' @param averaged.source If a list of \code{Source} objects have been supplied to be averaged before the classification, you must supply another \code{Source} object to store the averaged results.
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
#'  to specify the column names.  For the default column names see the source file \code{aDGVM1-Format}.
#' }
#' For the aDGVM2 Format, the following arguments apply:
#' \itemize{
#'  \item{\code{adgvm2.scheme}}  This numeric argument defines the aDGVM PFT scheme which can be 1 or 2.
#'  \item{\code{adgvm2.daily}}  A logical, set to true to read daily data (only for \code{adgvm2.scheme=1} and if daily data are provided in pop file)
#' }
#' @return A \code{Field}. 
#' @seealso \code{\link{aggregateSubannual}}, \code{\link{aggregateSpatial}}, \code{\link{aggregateYears}}, \code{\link{getDimInfo}},  \code{\link{getField}}    
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' 
#' @examples 
#' \donttest{
#'  
#' # In this example we derive and plot the Smith et al 2014 and the 
#' # Forrest et al 2015 biome classifications    
#'  
#' # First define a Source
#' test.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(name = "LPJ-GUESS run", dir = test.dir,  format = GUESS)
#' 
#' # Smith et al. 2014
#' Smith2014.biomes <- getScheme(source = test.Source, scheme = Smith2014BiomeScheme, 
#'                               year.aggregate.method = "mean")
#' print(plotSpatial(Smith2014.biomes))
#' 
#' # Forrest et al. 2014
#' Forrest2015.biomes <- getScheme(source = test.Source, scheme = Forrest2015BiomeScheme, 
#'                                 year.aggregate.method = "mean")
#' print(plotSpatial(Forrest2015.biomes))
#' 
#' }
getScheme <- function(source, 
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
                      quick.read.file = NULL, 
                      quick.read.autodelete = FALSE,  
                      averaged.source, 
                      verbose = FALSE, 
                      ...){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  ### CHECK ARGUEMENTS
  if(!missing(first.year) & !missing(last.year) ) {
    if(first.year > last.year) stop("first.year cannot be greater than last.year!")
  }

  ## QUICK READ ARGUMENTS AND AUTODELETE
  # check validity of quick.read.file argument
  valid.quick.read.file <- !is.null(quick.read.file) && is.character(quick.read.file)  && length(quick.read.file) > 0 && !identical(quick.read.file, "")
  if(!valid.quick.read.file && !is.null(quick.read.file)) {
    warning("Invalid 'quick.read.file' file argument specified, quick reading (or saving of file) will not be done,")
  }
  # if valid file then make the preprocessed file name and do autodelete (if specified)
  if(valid.quick.read.file) {
    preprocessed.file.name <- file.path(source@dir, paste(quick.read.file, "RData", sep = "."))
    if(quick.read.autodelete && file.exists(reprocessed.file.name))  {
      if(verbose) message(paste("Auto-deleting file"), preprocessed.file.name)
      file.remove(preprocessed.file.name)
    }
  }
  
  ### IF A LIST OF SOURCES PROVIDED, CHECK FOR THE averaged.source SOURCE AND SET THE final.source APPROPRIATELY
  if(is.list(source)) {
    if(missing(averaged.source)) stop("If you want to average over some Sources in getScheme(), then you need to supply the 'averaged.source' argument for saving the results")
    else final.source <- averaged.source
  }
  else {
    final.source <- source
  }
  
  ### CONVERT SCHEME ID STRING TO A CLASSIFICATION SCHEME
  if(is.character(scheme)) {
    scheme.string <- scheme
    scheme <- byIDfromList(scheme, supported.classification.schemes)
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
        if(sta.info.matches) {
          if(verbose) message("*** Preprocessed file matches, so using that. ***")
          return(preprocessed.field)
        }
        # else clean up and proceed to read the raw data
        else rm(preprocessed.field)
        
      } # if successful read
    } # if file processed file found on disk
  } # if got a valid quick.read.file
  
  
  ##############################################################################################  
  #### NOTE: We don't pass this point if the correct pre-processed data is available on disk ###
  #### as we will have already returned in CASE 1  above.                                    ###
  ##############################################################################################
  
  
  ### CASE 2 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  if(verbose) {
    if(is.null(quick.read.file)) message(paste("The 'quick.read.file' argument was set to not set, so reading raw data to create the Field now.", sep = ""))
    else if(!is.null(quick.read.file) && !file.exists(paste(preprocessed.file.name))) message(paste("Field ", quick.read.file, " not already saved, so reading full data file to create the Field now.", sep = ""))
    else {
      message(paste("*** Reading of preprocessed file failed ***"))
      message(paste("So file on disk ignored and the original data is being re-read and new preprocessed file will be written."))
    }
  }
  
  # lookup the quantities required
  all.quantities <- list()
  for(this.layer in scheme@layers.needed){
    if(!this.layer$quantity %in% all.quantities) all.quantities <- append(all.quantities, this.layer$quantity)
  }
  
  # get the fields and layers required
  all.fields <- list()
  for(this.quantity in all.quantities){
    
    # if source is a single Source the easy life
    if(is.Source(source)) {
      this.field <- getField(source = source, quant = this.quantity, layers = NULL, sta.info = sta.info, ...)
    }
    # else get a Field for each Source average
    else{
      
      one.field.per.source <- list()
      for(this.source in source){
        one.field.per.source[[length(one.field.per.source) +1]] <- getField(source = this.source, quant = this.quantity, sta.info = sta.info, ...)
      }
      
      this.field <- averageFields(one.field.per.source, source = averaged.source)
      
    }
    
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
    
    # rename and store
    renameLayers(this.field, names(this.field), paste(this.quantity, names(this.field), sep = "_"))
    all.fields[[length(all.fields) + 1 ]] <- this.field
    
  }
  
  # roll together all the data and extract the STAInfo (from the first one)
  final.stainfo <- as(Class = "STAInfo", object = all.fields[[1]])
  
  dt <- all.fields[[1]]@data
  if(length(all.fields) > 1){
    for(counter in 2:length(all.fields)){
      dt <- dt[all.fields[[counter]]@data]
    }
  }
  
  # remove the individual fields
  rm(all.fields)
  
  if(verbose) message("Starting classification")
  suppressWarnings(dt[, scheme@id := factor(apply(dt[,,with=FALSE],FUN=scheme@rules,MARGIN=1), levels = scheme@units)])
  
  # remove all layers which are not the scheme layers and set key
  all.layers <- layers(dt)
  all.non.scheme.layers <- all.layers[which(all.layers != scheme@id)]
  dt[, (all.non.scheme.layers) := NULL]
  setKeyDGVM(dt)
  
  ### BUILD THE FINAL Field
  scheme.field <- new("Field",
                      id = makeFieldID(source = final.source, quant.string = scheme@id, sta.info = final.stainfo),
                      data = dt,
                      quant = as(object = scheme, Class = "Quantity"),
                      source = final.source,
                      final.stainfo)
  
  
  ### WRITE THE FIELD TO DISK AS AN .RData OBJECT IF REQUESTED
  if(valid.quick.read.file) {
    if(verbose) {message("Saving as a .RData object...")}
    saveRDS(scheme.field, file = preprocessed.file.name)
    if(verbose) {message("...done.")}
  }
  
  ### TIDY UP AND RETURN
  rm(dt)
  gc()
  
  return(scheme.field)
  
}
