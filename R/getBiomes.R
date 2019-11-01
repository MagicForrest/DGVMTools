#!/usr/bin/Rscript

#
#' Get a biome \code{Field} from a \code{Source}
#' 
#' Given a \code{Source} object and a \code{BiomeScheme} object, return an appropriate spatially/temporal/annually-aggregated biome \code{Field} object using the 
#' classification contained in the \code{BiomeScheme} optionally including spatial, temporal and annual cropping.
#' 
#' Note that because there are three types of aggregating available, the resulting \code{Field} object can a wide select of spatio-temporal dimensions.
#' To check what dimensions you have you can use \code{\link{getDimInfo}}  
#' 
#' @param source The \code{Source} object for which the biome \code{Field} should be built. A list of \code{Source} objects can also be provided, in which case 
#' the required \code{Field} object derived from each \code{Source} in the list (for example LAI or fractional cover) are first averaged, and then do the biome 
#' classification. In the case of a list of \code{Source} objects, the \code{averaged.source} arguments (see below) must also be supplied so that the averaged 
#' data can be written to/read from disk.
#' @param scheme The biome scheme to be used as \code{BiomeScheme} object, or the id of a \code{BiomeScheme} defined in the package, as a character string.
#' @param sta.info Optionally an STAInfo object defining the exact spatial-temporal-annual domain over which the data should be retrieved.  
#' Can also be a Field object from which the STA info will de derived.
#' If specified the following 9 arguments are ignored (with a warning).
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
#' @param read.full.components If TRUE ignore any pre-averaged file for the *component* variables on disk, if FALSE use them if they are there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param write If TRUE, write the data of the \code{Field} to disk as text file.
#' @param write.components If TRUE, write the data of the *component* \code{Field}s to disk as text file.
#' @param averaged.source If a list of \code{Source} objects have been supplied to be averaged before the classification, you must supply another \code{Source} object to store the averaged results.
#' @param ...  Other arguments that are passed to the getField function for the specific Format or for selecting space/time/years.  Currently this can be
#' \itemize{
#'  \item{adgvm.scheme}  For the aDGVM Format, defines the aDGVM PFT scheme which can be 1 or 2.
#'  \item{cover.fraction}  Optional when selecting gridcells based on a SpatialPolygonsDataFrame (ie from a shapefile) as the \code{spatial.extent} argument, should be between 0 and 1.
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
#' Smith2014.biomes <- getBiomes(source = test.Source, scheme = Smith2014BiomeScheme, 
#'                               year.aggregate.method = "mean")
#' print(plotSpatial(Smith2014.biomes))
#' 
#' # Forrest et al. 2014
#' Forrest2015.biomes <- getBiomes(source = test.Source, scheme = Forrest2015BiomeScheme, 
#'                                 year.aggregate.method = "mean")
#' print(plotSpatial(Forrest2015.biomes))
#' 
#' }
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
                      averaged.source, 
                      verbose = FALSE, 
                      ...){
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  ### CHECK ARGUEMENTS
  
  
  ### IF A LIST OF SOURCES PROVIDED, CHECK FOR THE averaged.source SOURCE AND SET THE final.source APPROPRIATELY
  if(is.list(source)) {
    if(missing(averaged.source)) stop("If you want to average over some Sources in getBiomes(), then you need to supply the 'averaged.source' argument for saving the results")
    else final.source <- averaged.source
  }
  else {
    final.source <- source
  }
  
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
  target.field.id <- makeFieldID(source = final.source, 
                                 var.string = scheme.string, 
                                 sta.info = sta.info)
  
  file.name <- file.path(final.source@dir, paste(target.field.id, "DGVMField", sep = "."))
  if(verbose) message(paste("Seeking ModelField with id = ", target.field.id, sep = ""))
  
  
  
  
  ### CASE 1 - USE THE PREAVERAGED/CROPPED FIELD FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name)) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name, "found in",  final.source@dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    biome.field <- readRDS(file.name)
    
    # Update the source object, that might have (legitimately) changed compared to the id that was used when this biome.field was created
    # for example it might be assigned a new id.
    biome.field@source <- final.source
    
    
    # Check that the spatial extent matches before returning
    # Note that there are two cases to check here (specifically defined extents or just the same ids)
    
    
    full.domain.matched <- FALSE
    if(length(sta.info@spatial.extent) == 0 && biome.field@spatial.extent.id == "Full") {
      full.domain.matched <- TRUE
      if(verbose) message("Full domain matched.")
    }
    
    cropped.domain.matched <- FALSE
    if(length(sta.info@spatial.extent) > 0 && length(biome.field@spatial.extent)  > 0 ) {
      if(identical(sta.info@spatial.extent, biome.field@spatial.extent)){
        cropped.domain.matched <- TRUE
        if(verbose) message("Cropped domain matched.")
      }
    }
    
    if(full.domain.matched || cropped.domain.matched){
      return(biome.field)
    }  
    
    # Otherwise we must discard this Field and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of the spatial extent",  sta.info@spatial.extent.id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(biome.field)
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
    
    # if source is a single Source the easy life
    if(is.Source(source)) {
      this.field <- getField(source = source, var = this.quantity, sta.info = sta.info, read.full = read.full.components, write = write.components, ...)
    }
    # else get a Field for each Source average
    else{
      
      one.field.per.source <- list()
      for(this.source in source){
        one.field.per.source[[length(one.field.per.source) +1]] <- getField(source = this.source, var = this.quantity, sta.info = sta.info, read.full = read.full.components, write = write.components, ...)
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
  
  if(verbose) print("Starting classification")
  suppressWarnings(dt[, scheme@id := as.factor(apply(dt[,,with=FALSE],FUN=scheme@rules,MARGIN=1))])
  
  # remove all layers which are not the biome layers and set key
  all.layers <- layers(dt)
  all.non.biome.layers <- all.layers[which(all.layers != scheme@id)]
  dt[, (all.non.biome.layers) := NULL]
  setKeyDGVM(dt)
  
  ### BUILD THE FINAL Field
  biome.field <- new("Field",
                     id = makeFieldID(source = final.source, var.string = scheme@id, sta.info = final.stainfo),
                     data = dt,
                     quant = as(object = scheme, Class = "Quantity"),
                     source = final.source,
                     final.stainfo)
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMField object...")}
    saveRDS(biome.field, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  ### TIDY UP AND RETURN
  rm(dt)
  gc()
  
  return(biome.field)
  
}
