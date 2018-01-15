#!/usr/bin/Rscript


######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table based in the spatial (Lon, Lat) and temporal (Year, Month, Day), present. 
#' 
#' Keys should be set on all data.table object for sorts, joins, DGVMTool-defined operators etc.  
#'  This function should be called on a data.table stored in a Field after it has been created,
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



############# ADD AN OBJECT TO A MODELRUN ######################################

#' Add an object (either a \code{Field} or a  \code{SpatialComparison}) to a \code{Source} object to be use later.  
#' 
#' Stores an object in its run for later calculations, plotting, comparisons.
#' 
#' @param object Object to add to the \code{Source}.
#' @param run The \code{Source} object to which the object argument should be added
#' @return A Source object the the object argument added
#' @export
#' @seealso Source, SourceInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

addToSource <- function(object, run){
  
  # Add a BiomeComaprison or RasterComparison to the list in the benchmarks slot 
  if(is.SpatialComparison(object)) {
    
    benchmark.list <- run@benchmarks
    benchmark.list[[object@id]] <- object
    run@benchmarks <- benchmark.list
    rm(benchmark.list)
    
  }
  
  # Add a Field to the list in the objects slot 
  else if(is.Field(object)) {
    
    # Check that run ids match, if not, stop because something is really wrong
    if(run@id != object@source@id){
      stop(paste("Adding Field ", object@id, " which comes from run with id = ",  object@source@id, " to run with id = ", run@id, ". If you are doing something funky, like averaging Fields from different runs to make a a Source representing an ensemble mean, then make sure that the ids match. Otherwise you will break the internal logic of DGVMTools so aborting. Contact the package creator if this seems wrong to you." , sep = ""))
    }
    
    model.objects.list <- run@objects
    model.objects.list[[object@id]] <- object
    run@objects <- model.objects.list
    rm(model.objects.list)
    
  }
  
  else{
    
    warning(paste("Cannot add object of class", class(object), "to a Source object", sep = " "))
    
  }
  
  return(run)
  
}


############# REMOVE AN OBJECT FROM A MODELRUN ######################################

#' Remove an object (either a \code{Field} from a \code{Source} object to reclaim memory.  
#' 
#' Processing a lot of different files and storing the full output internally (in the Source) may use too much memory. You can get around this by not storing them internally, 
#' but if you need to do this, for example because you want to average many spatial or temporal extents from one output file, this function lets you remove Field to free the space again.  
#' 
#' @param object.id The id of the object to remove  to the \code{Source},   
#' @param run The \code{Source} object to which the object argument should be added
#' @return A Source with the object removed
#' @export
#' @seealso Source, SourceInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

removeFromSource <- function(object.id, run){
  
  
  if(object.id %in% names(run@objects)) {
    
    model.objects.list <- run@objects
    model.objects.list[[object.id]] <- NULL
    run@objects <- model.objects.list
    rm(model.objects.list)
    gc()
    
  }
  
  else{
    warning(paste("Can't remove Field with id ", object.id, " from run ", run@id, " because I can't find it in the run!"))
  }
  
  
  return(run)
  
}



############################## MAKE THE 'id' STRING FOR A MODELOBJECT
#
#' Make an ID string for a \code{Field}
#' 
#' Given a string for the quantity and temporal and spatial extents and averaging flags, build an appropriate (and unique) ID string
#' for use in the \code{id} slot of a \code{Field} and for filenames etc.
#' 
#' @param var.string Character string to describe the variable, eg "lai" or "corrected.cmass" or "npp.diff"
#' @param temporal.extent The temporal extent of this object if it has been cropped from the orginal duration, otherwise NULL
#' @param spatial.extent The spatial extent of this object if it has been cropped from the orginal simulation extent, otherwise NULL
#' @param temporal.aggregate.method Character, method by which this.Field was temporally aggregated (if not temporally averaged leave blank of use "none")
#' @param spatial.aggregate.method Character, method by which this.Field was spatially aggregated (if not spatially averaged leave blank of use "none")
#' @return A character string 
#' @export
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 


makeFieldID <- function(var.string, temporal.extent = NULL, spatial.extent = NULL, temporal.aggregate.method = "none", spatial.aggregate.method = "none"){
  
  
  model.object.id <- var.string
  if(tolower(spatial.aggregate.method) != "none")  model.object.id <- paste(model.object.id, "spatial", spatial.aggregate.method, sep = ".")
  if(!is.null(spatial.extent)) model.object.id <- paste(model.object.id, spatial.extent, sep = ".")
  if(tolower(temporal.aggregate.method) != "none")  model.object.id <- paste(model.object.id, "temporal", temporal.aggregate.method, sep = ".")
  if(!is.null(temporal.extent)) model.object.id <- paste(model.object.id, temporal.extent, sep =".")
  
  return(model.object.id)
  
}



################################# PROMOTE TO RASTER
#
#' Prepares data as a raster object for plotting.
#' 
#' Converts a data.table, Field or an Spatial*-object to Raster object, also subsetting the requested layers.  
#' It can also handle a RasterLayber/Stack/Brick, in which case the only processing done is the subsetting since the data is already in Raster form.
#' This is generally called in the \code{plotSpatial} function (or potentially before any use of the raster::spplot and raster::plot functions),
#' but can be useful in and of itself.
#'   
#' @param input.data data.table, Field or Spatial*-object to be converted into a raster. Also takes a Raster*-object, in which case he 
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
  # for Field - note could define a methods "names" do cover this exception
  if((is.Field(input.data) || is.ComparisonLayer(input.data) ) & (is.null(layers) | layers[1] == "all")) {layers <- names(input.data@data)} 
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
  ### If data.table or Field (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | is.Field(input.data) | is.ComparisonLayer(input.data)) {
    # first make a SpatialPointsDataFrame
    if(this.class == "data.table") {
      data.spdf <- makeSPDFfromDT(input.data, layers, tolerance, grid.topology = grid.topology)
    }
    
    if(is.Field(input.data) | is.ComparisonLayer(input.data)) {
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


#' Average Fields provided as a list
#' 
#' Useful for averaging model ensembles, replicate simulations etc.  
#' Note that since you can supply the function, you don't actually need to average, you can also do, for example, \code{max}, \code{min}, \code{sd}, etc.  
#' Fails with an error message if the supplied Fields don't have the same fields
#' 
#' @param list.of.model.objects The Fields that you want to average, stored in a list.
#' @param run Either a Source object which provides the metadata about the newly created "run" (representing, say, the ensemble mean) or \code{NULL}, 
#' in which case the function just returns a data.table with the average values.
#' @param method The function which you want to use.  Is \code{mean} by default, but it can be anything that operates on a vector of numbers.  
#' Useful options might be \code{max},  \code{min} and \code{sd}.  
#'  
#'@details  
#' The function currently does no checks that the spatial extent, temporal extent and Quantity are consistent between runs.  The resulting Field simple takes these things
#' from the first object from list.of.vegruns.  It is therefore up to the user not do anything silly and average quantites, or spatial or temporal extents that don't makes sense.
#'          
#' @export
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @return Either a Field (if a Source has been supplied to provide the necessary meta-data), or a data.table (if no meta-data has been supplies in the form of a Source)
#' 
averageFields <- function(list.of.model.objects, run = NULL, method = mean) {
  
  
  # make lists of what is to be processed, and check that the columns are the same (if not fail!)
  list.of.dts <- list()
  for(object in list.of.model.objects){
    list.of.dts[[paste(object@source@id, object@id, sep = ".")]] <- object@data
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
  
  
  # if no run is supplied then cannot construct a full Field, therefore just return the data.table
  if(is.null(run)) {
    return(output.dt)
  }
  
  # else, if a run was provided, make a Field and return that
  else {
    
    #  MF TODO: check that the Fields we just averaged have the same spatial and temporal properties 
    # if they don't, drop an error message
    
    
    # construct a new Field for the average data that we have just calculated based on the first on in the list
    # but put in the new run and data
    new.Field <- list.of.model.objects[[1]]
    new.Field@data <- output.dt
    new.Field@source <- as(run, "SourceInfo")
    
    
    return(new.Field)
  }
  
}