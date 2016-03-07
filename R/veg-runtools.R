#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
###################################### 

############################################################################################################################
############################ FUNCTIONS TO BUILD VEG* OBJECTS ###############################################################
############################################################################################################################



defineVegRun <- function(...){
  
  # make a VegRunInfo object from the supplied meta data
  info <- new("VegRunInfo", ...)
  
  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(info@pft.set) == 0) info@pft.set <- NULL
  if(length(info@tolerance) == 0)  info@tolerance <- 0.0000001
  if(length(info@description) == 0)  info@description <- "No description specified"
  if(length(info@map.overlay) == 0 | info@map.overlay == "")  info@map.overlay <- NULL
  if(length(info@lonlat.offset) == 0)  info@tlonlat.offset <- c(0,0)
  if(length(info@year.offset) == 0)  info@year.offset <- 0
  if(length(info@tolerance) == 0)  info@tolerance <- 0.00001
  if(length(info@driving.data) == 0)  info@driving.data <- "No forcing data set"
  if(length(info@london.centre) == 0)  info@london.centre <- TRUE
  if(length(info@fill.col) == 0)  info@fill.col <- "transparent"
  if(length(info@line.col) == 0)  info@line.col <- "green"
  if(length(info@line.width) == 0)  info@line.width <- 1
  if(length(info@line.type) == 0)  info@line.type <- 1
  if(length(info@landuseSimulated) == 0)  info@landuseSimulated <- FALSE
  
  # lookup map over from maps and mapdata package
  if(!is.null(info@map.overlay)) {
    class(info@map.overlay)
    info@map.overlay <- makeOverlay(info@map.overlay)
  } 
  
  # return a VegRun object with empty data fields but meta data filled  
  return(new("VegRun",
             info))
  
  
}


addToVegRun <- function(object, run, id = NULL){
  
  object.class <- class(object)[1]
  
  if(object.class == "data.table") {
    
    if(is.null(id)) stop("When adding a full data.table to a VegRun you *must* specific an id")
    run@full[[id]] <- object
    
  }
  
  else if(object.class == "BiomeComparison" | object.class == "RasterComparison") {
    
    if(is.null(id)) id <- object@id
    benchmark.list <- run@benchmarks
    benchmark.list[[id]] <- object
    run@benchmarks <- benchmark.list
    rm(benchmark.list)
    
  }
  
  
  
  else if(is.VegObject(object)) {
    
    if(is.null(id)) id <- object@id
    spatial.list <- run@spatial
    spatial.list[[id]] <- object
    run@spatial <- spatial.list
    rm(spatial.list)
    
  }
  
  else{
    
    warning(paste("Cannot add object of class", object.class, "to a VegRun object", sep = " "))
    
  }
  
  
  return(run)
  
}


################################# GET TIME-AVERAGED DATA #########################################

getVegSpatial <- function(run,
                          var,
                          period,
                          reread.file = TRUE,
                          ...){
  
  
  return(
    getVegObject(run, 
                 var, 
                 spatial.extent = NULL, 
                 spatially.average = FALSE,
                 temporal.extent = period, 
                 temporally.average = TRUE, 
                 reread.file = reread.file, 
                 ...)
  )
  
  
}


################################# GET SPACE-AVERAGED DATA #########################################


getVegTemporal <- function(run,
                           var,
                           spatial.extent = NULL,
                           reread.file = TRUE,
                           ...){
  
  return(
    getVegObject(run, 
                 var, 
                 spatial.extent = spatial.extent, 
                 spatially.average = TRUE,
                 temporal.extent = NULL, 
                 temporally.average = FALSE, 
                 reread.file = reread.file, 
                 ...)
  )
  
  
}



################################# GET VEGOBJECT - Does a lot! #########################################

getVegObject <- function(run, 
                         var, 
                         temporal.extent = NULL, 
                         temporally.average = FALSE, 
                         spatial.extent = NULL, 
                         spatially.average = FALSE,
                         area.weighted=TRUE,
                         write = FALSE, 
                         reread.file = TRUE, 
                         verbose = TRUE, 
                         store.internally = FALSE,
                         adgvm.scheme = 1){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = NULL  
  
  
  ### CONVERT STRING TO VEGQUANT
  if(class(var) == "character") {
    quant <- lookupVegQuantity(var)
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  ### MAKE file.name VARIABLE - this describes completely whether we want the files spatially or temporally averaged and reduced in extent
  TA.str = SA.str = "."
  if(spatially.average) SA.str <- ".SA."
  if(temporally.average) TA.str <- ".TA."
  if(is.null(spatial.extent) & !is.null(temporal.extent)) file.name <- file.path(run@run.dir, paste(var.string, TA.str, paste(temporal.extent@start, temporal.extent@end, sep = "-"), ".Rtable", sep =""))
  else if(!is.null(spatial.extent) & is.null(temporal.extent)) file.name <- file.path(run@run.dir, paste(var.string, SA.str, spatial.extent@id, ".Rtable", sep =""))
  else if(!is.null(spatial.extent) & !is.null(temporal.extent)) file.name <- file.path(run@run.dir, paste(var.string, SA.str, spatial.extent@id, TA.str,  paste(temporal.extent@start, temporal.extent@end, sep = "-"), ".Rtable", sep =""))
  else  file.name <- file.path(run@run.dir, paste(var.string, "Rtable", sep ="."))
  
  
  ### USE THE PREAVERAGED/CROPPED FILE IF AVAILABLE (and we are not forcing a re-read and we have not already read the full file)
  if(file.exists(paste(file.name)) & !reread.file & !var.string %in% names(run@full)){
    if(verbose) {message(paste("File",  file.name, "found in",  run@run.dir, "(and reread.file not selected) so reading it from disk and using that.",  sep = " "))}
    this.dt <- fread(file.name)
    .setKeyRVC(this.dt)
    if(spatially.average) {
      message(paste("getVegObject: Note that we are reading pre-averaged file", file.name, "which has been spatially averaged over the extent", spatial.extent@id, "which might not correspond to the exact extent specified here.  If you changed the extent recently (or don't know the extent used) you might should consider setting reread.file = TRUE for a small increase in run time but you can be certain you are averaging over the right area", sep = " "))
      warning(paste("getVegObject: Note that we are reading pre-averaged file", file.name, "which has been spatially averaged over the extent", spatial.extent@id, "which might not correspond to the exact extent specified here.  If you changed the extent recently (or don't know the extent used) you might should consider setting reread.file = TRUE for a small increase in run time but you can be certain you are averaging over the right area", sep = " "))
    }
  }

  ### IF PRE-AVERAGED/CROPPED FILE NOT AVAILABLE THEN CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT
  ### AND DO THE CROPPING/AVERAGING 
  else {
    
    ### !!! CALL MODEL SPECIFIC FUNTIONS HERE !!!
    if(run@model == "LPJ-GUESS" | run@model == "LPJ-GUESS-SPITFIRE") {
      
      this.dt <- getVegQuantity_LPJ(run, var.string, store.internally, verbose)
      
    } # END IF LPJ-GUESS or LPJ-GUESS-SPITFIRE
    
    else if(run@model == "aDGVM") {
      
      if(adgvm.scheme == 1) this.dt <- data.table(getVegQuantity_aDGVM_Scheme1(run, temporal.extent, quant))
      if(adgvm.scheme == 2) this.dt <- data.table(getVegQuantity_aDGVM_Scheme2(run, temporal.extent, quant))
      
      setkey(this.dt, Lon, Lat, Year)
      
    } # END IF aDGVM
    
    
    
    ### CROP THE SPATIAL AND TEMPORAL EXTENTS IF REQUESTED
    if(!is.null(spatial.extent))  this.dt <- cropRVC(this.dt, spatial.extent)      
    if(!is.null(temporal.extent))  this.dt <- .selectYears(this.dt, temporal.extent)     
    
    
    ### GET THE LONS, LATS AND YEAR BEFORE THEY ARE AVERAGED AWAY
    sorted.unique.years <- sort(unique(this.dt[,Year]))
    sorted.unique.lats <- sort(unique(this.dt[,Lat]))
    sorted.unique.lons <- sort(unique(this.dt[,Lon]))
    
    
    ###  DO SPATIAL AVERAGE - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
    if(spatially.average){
      this.dt <- .doSpatialAverage(this.dt, verbose, area.weighted)
      if(verbose) {
        message("Head of spatially averaged data.table:")
        print(head(this.dt))
      }
    }
    
    
    ###  DO TIME AVERAGE
    if(temporally.average){
      this.dt <- .doTemporalAverage(this.dt, verbose)
      if(verbose) {
        message("Head of time averaged data.table:")
        print(head(this.dt))
      }
    }
    
    ### WRITE THE TABLE IF REQUESTED
    if(write) {
      if(verbose) {message("Saving as a table...")}
      write.table(this.dt, file = file.name, quote = FALSE, row.names = FALSE)
    }
    
    
  } # IF READING RAW DATA
  
  
  ### IF NO EXTENTS SPECIFIED, GET THE EXTENTS FOR THE RETURN OBJECT
  
  # But first build the VegObject id and ignore 
  
  # TEMPORAL
  if(is.null(temporal.extent)) {
    temporal.extent<- new("TemporalExtent",
                          id = "FullTS",
                          name = "Full simulation duration",
                          start = sorted.unique.years[1],
                          end = sorted.unique.years[length(sorted.unique.years)]
    )
    if(verbose) message(paste("No temporal extent specified, setting temporal extent to whole file (",  temporal.extent@start, "-", temporal.extent@end, ")", sep = ""))
  }
  
  # SPATIAL
  if(is.null(spatial.extent)) {
    spatial.extent <- new("SpatialExtent",
                          id = "FullDomain",
                          name = "Full simulation extent",
                          extent = extent(sorted.unique.lons[1] - ((sorted.unique.lons[2] - sorted.unique.lons[1])/2), 
                                          sorted.unique.lons[length(sorted.unique.lons)] + ((sorted.unique.lons[length(sorted.unique.lons)] - sorted.unique.lons[length(sorted.unique.lons)-1])/2),
                                          sorted.unique.lats[1] - ((sorted.unique.lats[2] - sorted.unique.lats[1])/2), 
                                          sorted.unique.lats[length(sorted.unique.lats)] + ((sorted.unique.lats[length(sorted.unique.lats)] - sorted.unique.lats[length(sorted.unique.lats)-1])/2)
                          )
    )
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  spatial.extent@extent@xmin, ",", spatial.extent@extent@xmax, "), Lat = (" ,  spatial.extent@extent@ymin, ",", spatial.extent@extent@ymax, ").", sep = ""))
  }
  
  
  return(new("VegObject",
             id = paste(var.string, temporal.extent@id, spatial.extent@id, sep = "_"),
             data = this.dt,
             quant = quant,
             spatial.extent = spatial.extent,
             temporal.extent = temporal.extent,
             is.site = FALSE,
             is.spatially.averaged = spatially.average,
             is.temporally.averaged = temporally.average,
             run = as(run, "VegRunInfo")))
  
}




promoteToRaster <- function(data, layers = "all", tolerance = 0.0000001, grid.topology = NULL){
  
  ###  Get class of the objetc we are dealing with
  this.class = class(data)[1]
  
  ###  Define the layers we are pulling out
  # for VegObject - note could define a methods "names" do cover this exception
  if(is.VegObject(data) & (is.null(layers) | layers[1] == "all")) {layers <- names(data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers[1] == "all") {layers = names(data)}
  
  
  ###  If wSpatialPixelsDataFrame we can plot it
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in veg-runtools.R")
    data.raster <- raster(data, layers)
  }
  ### If data.table or VegObject (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | is.VegObject(data)){
    if(this.class == "data.table") data.spdf <- .makeSPDFfromDT(data, layers, tolerance, grid.topology = NULL)
    if(is.VegObject(data)) data.spdf <- .makeSPDFfromDT(data@data, layers, tolerance, grid.topology = NULL)
    if(length(layers) == 1){
      data.raster <- raster(data.spdf)
      rm(data.spdf)
    }
    else {
      data.all.raster <- brick(data.spdf)
      data.raster <- subset(data.all.raster, layers) 
      rm(data.spdf, data.all.raster)   
    }
  } 
  ###  If a single raster layer then we are done
  else if(this.class == "RasterLayer"){
    data.raster <- data    
  }
  ### If a stack or a brick, then subset 
  else if(this.class == "RasterBrick" | this.class == "RasterStack"){
    data.raster <- subset(data, layers)
    names(data.raster) <- layers
  }
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to promote object of type", class(data), "to Raster, which I don't know how to do.", sep = " "))
  }
  
  gc()
  return(data.raster)
  
}


sanitiseNamesForRaster <- function(input){
  
  ###  Get class of the object we are dealing with
  this.class = class(input)[1]
  
  
  ###  If SpatialPixelsDataFrame 
  if(this.class == "SpatialPixelsDataFrame"){ 
    names(input) <- sub("-", ".", names(input))
    return(input)
  }
  ### If data.table
  else if(this.class == "data.table"){ 
    input.copy <- copy(input)
    setnames(input.copy, sub("-", ".", names(input.copy)))
    return(input.copy)
  }
  ### If VegObject (which contains a data.table) 
  else if(is.VegObject(input)){
    dt <- copy(input@data)
    setnames(dt, sub("-", ".", names(dt)))
    input@data <- dt
    rm(dt)
    gc()
    return(input)
  }
  ### If "character"
  else if(this.class == "character"){
    input <- sub("-", ".", input)
    return(input)
  }
  ###  If raster object already we are done
  else if(this.class == "RasterLayer" | this.class == "RasterBrick" | this.class == "RasterStack"){
    # Already a raster thing, donothing to do here
    return(input)
  }
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to sanitise names object of type", class(input), ", which I don't know how to do.", sep = ""))
  }
  
  
  
}


##########################################################################################################
########### HELPER FUNCTIONS


# Helper function to make a SpatialPixelsDateFrame from a data.table object
# It is assumed that data.table has columns Lon and Lat to provide the spatial information
.makeSPDFfromDT <- function(data, layers = "all",  tolerance = 0.0000001, grid.topology = NULL) {
  
  # to stop complaints at build time
  Lon <- NULL
  Lat <- NULL
  
  # sort the layers
  if(is.null(layers) | layers[1] == "all") {layers = names(data)}
  
  # remove things we don't want to plot like "Lon" and "Lat"
  remove.list <- c("Lon", "Lat", "Year")
  for(remove in remove.list){
    if(remove %in% layers) {layers <- layers[-which(layers == remove)]}     
  }
  
  # convert to SPDF
  #sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]))
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance, grid = grid.topology)
  )
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    data.spdf <- SpatialPixelsDataFrame(sp.pixels, data[,layers,with=FALSE], tolerance = tolerance)
  )
  # clean up
  rm(sp.points, sp.pixels)
  
  return(data.spdf)  
  
}

###############################################################################################################################
######################### FUNCTIONS FOR SPATIAL AND TEMPORAL AVERAGING DATA.TABLES  ###########################################
###############################################################################################################################





######################### TIME AVERAGE AN LPJ-GUESS FULL OUTPUT FILE COMING IN AS A data.table  ##############################

.doTemporalAverage.uncompiled <- function(input.dt,
                                          verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # Do the averaging
  if(verbose) message("Temporally averaging ...")
  if("Lon" %in% names(input.dt))  output.dt <- input.dt[,lapply(.SD, mean), by=list(Lat, Lon)]
  else output.dt <- input.dt[,lapply(.SD, mean)]
  if(verbose) message("...done.")
  
  # remove the Year 'cos it dun' make so much sense no mo'
  output.dt[,Year:=NULL]
  
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Set keys and return the averaged table
  .setKeyRVC(output.dt)
  return(output.dt)
  
}

.doTemporalAverage <- cmpfun(.doTemporalAverage.uncompiled)




.selectYears <- function(input.dt, temporal.extent){
  
  # To stop compiler NOTES
  Year = NULL
  
  # return the subsetted data.table
  return(subset(input.dt, Year >= temporal.extent@start & Year <= temporal.extent@end))    
  
}


######################### SPACE AVERAGE AN LPJ-GUESS FULL OUTPUT FILE COMING IN AS A data.table  ##############################

.doSpatialAverage.uncompiled <- function(input.dt,
                                         verbose = FALSE,
                                         area.weighted=TRUE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # Do the averaging
  if (area.weighted) {
    if (!any(colnames(input.dt)=="area"))
      if (verbose)
        message("Add column area.")
    input.dt <- addArea(input.dt, verbose=verbose)
    if(verbose) message(paste("Spatially averaging (area weighted) whole domain...", sep = ""))
    # check to see if Year is still a clomun name (it might have been averaged away)
    if("Year" %in% names(input.dt)) output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area), by=list(Year)]
    else output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area)]
    
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
  .setKeyRVC(output.dt)
  return(output.dt)
  
}

.doSpatialAverage <- cmpfun(.doSpatialAverage.uncompiled)



.LondonCentre <- function(lon) {
  
  if(lon <= 180) return(lon)
  else return(lon - 360)
  
}


.setKeyRVC <- function(dt){
  
  keys.present <- c()
  
  if("Lat" %in% names(dt)) keys.present <- append(keys.present, "Lat")
  if("Lon" %in% names(dt)) keys.present <- append(keys.present, "Lon")
  if("Year" %in% names(dt)) keys.present <- append(keys.present, "Year")
  
  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  
}

