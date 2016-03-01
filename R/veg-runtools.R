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
  
  else if(object.class == "VegTemporal") {
    
    if(is.null(id)) id <- object@id
    temporal.list <- run@temporal
    temporal.list[[id]] <- object
    run@temporal <- temporal.list
    rm(temporal.list)
    
  }
  
  else if(object.class == "VegSpatial") {
    
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

getVegSpatial <- function(run, period, var, this.full = NULL, write = TRUE, forceReAveraging = TRUE, verbose = TRUE, adgvm.scheme = 1){
  
  if(class(var) == "character") {
    quant <- lookupVegQuantity(var)
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  if(run@model == "LPJ-GUESS" | run@model == "LPJ-GUESS-SPITFIRE") {
    
    # make file name
    TA.filename <- paste(run@run.dir, "/", var.string, ".TA.", period@start, "-", period@end, ".Rtable", sep ="")
    
    # if file is present and we are not forcing re-averaging, read in the pre-averaged file
    if(file.exists(paste(TA.filename)) & !forceReAveraging){
      if(verbose) {message(paste("File",  TA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))}
      this.TA.dt <- fread(TA.filename, header = TRUE, stringsAsFactors=FALSE)
    } 
    
    # otherwise, open the full .out file read it as a data table, time average it and save to disk
    else {
      if (is.null(this.full)) {
        if(verbose){
          if(forceReAveraging & file.exists(paste(TA.filename))) message(paste("File",  TA.filename, "exists but forecReAveraging selected so reading ther full output again", sep = " "))
          else message(paste("File ",  TA.filename, " not found in directory ",  run@run.dir, " and ", var.string, ".out is not already read, reading .out file.", sep = ""))
        }
          
        this.full <- openLPJOutputFile(run, var.string, verbose = TRUE)
        if(verbose) {
          message("Head of full .out file (after offsets):")
          print(head(this.full))
        }
      }
      else{
        if(verbose) message(paste("File ",  TA.filename, " not found in directory ",  run@run.dir, " but ", var.string, ".out is already read, so using that.", sep = ""))
      }
      
      # do temporal averaging
      this.TA.dt <- .doTimeAverage.cmpd(this.full, period, verbose)
      if(verbose) {
        message("Head of time averaged data.table:")
        print(head(this.TA.dt))
      }
      
      
      if(write) {
        if(verbose) {message("Saving as a table...")}
        write.table(this.TA.dt, file = TA.filename, quote = FALSE, row.names = FALSE)
      }
    }
  }
  else if(run@model == "aDGVM") {
    
    if(adgvm.scheme == 1) this.dt <- data.table(getVegQuantity_aDGVM_Scheme1(run, period, quant))
    if(adgvm.scheme == 2) this.dt <- data.table(getVegQuantity_aDGVM_Scheme2(run, period, quant))
    
    print(this.dt)
    
    this.TA.dt <- .doTimeAverage.cmpd(this.dt, period, verbose)
    
  }
  
  setkey(this.TA.dt, Lon, Lat)
  
  return(new("VegSpatial",
             id = paste(var.string, period@id, sep = "_"),
             data = this.TA.dt,
             temporal.extent = period,
             quant = quant,
             run = as(run, "VegRunInfo")))
  
  
}


################################# GET SPACE-AVERAGED DATA #########################################


getVegTemporal <- function(run, var, spatial.extent = NULL, this.full = NULL, write = TRUE, forceReAveraging = TRUE, verbose = TRUE, area.weighted=TRUE, adgvm.scheme = 1){
  
  if(class(var) == "character") {
    quant <- lookupVegQuantity(var)
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  # look for the correct time averaged files and if there read it in
  if(is.null(spatial.extent)){
    SA.filename <- paste(run@run.dir, "/", var.string, ".SA.Rtable", sep ="")
  } 
  else {
    SA.filename <- paste(run@run.dir, "/", var.string, ".", spatial.extent@id, ".SA.Rtable", sep ="")
  }
  
  # if file is present and we are not forcing re-averaging, read in the pre-averaged file
  if(file.exists(paste(SA.filename)) & !forceReAveraging){
    if(verbose) {message(paste("File",  SA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))}
    this.SA.dt <- fread(SA.filename, header = TRUE, stringsAsFactors=FALSE)
    # In this case the 
    if(is.null(spatial.extent)) {
      # In this case we cannot determine original extent, so put in dummy values
      spatial.extent <- new("SpatialExtent", id = "original", name = "Full original domain", extent = extent(NaN, NaN, NaN, NaN))
    }
  } 
  
  # otherwise, open the full .out file read it as a data table, time average it and save to disk
  else {
    if (is.null(this.full)) {
      if(verbose) message(paste("File ",  SA.filename, " not found in directory ",  run@run.dir, " and ", var.string, ".out is not already read, reading .out file.", sep = ""))
      this.full <- openLPJOutputFile(run, var.string, verbose = TRUE)
    }
    else{
      if(verbose) message(paste("File ",  SA.filename, " not found in directory ",  run@run.dir, " but ", var.string, ".out is already read, so using that.", sep = ""))
    }
    # If required, crop spatial extent before spatially averaging
    if(!is.null(spatial.extent)) {
      this.full <- cropRVC(this.full, spatial.extent)
    }
    this.SA.dt <- .doSpaceAverage.cmpd(this.full, verbose, area.weighted)
    if(write) {
      if(verbose) {message("Saving as a table...")}
      write.table(this.SA.dt, file = SA.filename, quote = FALSE, row.names = FALSE)
    }
  }
  setkey(this.SA.dt, Year)
  
  # In the special case that a special extent was not specified and we read the whole file, determine the spatial extent
  if(is.null(spatial.extent) & !is.null(this.full)) {
    Lons <- sort(unique(this.full[,Lon]))
    Lats <- sort(unique(this.full[,Lat]))
    lon.min <- Lons[1] - (Lons[2] - Lons[1])/2
    lon.max <- Lons[length(Lons)] + (Lons[length(Lons)] -Lons[length(Lons)-1])/2
    lat.min <- Lats[1] - (Lats[2] - Lats[1])/2
    lat.max <- Lats[length(Lats)] + (Lats[length(Lats)] -Lats[length(Lats)-1])/2
    spatial.extent <- new("SpatialExtent", id = "Full", name = "Full Domain", extent = extent(lon.min,lon.max,lat.min,lat.max))
  }
  
  rm(this.full)
  gc()
  
  return(new("VegTemporal",
             id = paste(var.string, spatial.extent@id, sep = "."),
             data = this.SA.dt,
             spatial.extent = spatial.extent,
             quant = quant,
             run = as(run, "VegRunInfo")))
}




promoteToRaster <- function(data, layers = "all", tolerance = 0.0000001, grid.topology = NULL){
  
  ###  Get class of the objetc we are dealing with
  this.class = class(data)[1]
  
  ###  Define the layers we are pulling out
  # for VegSpatial - note could define a methods "names" do cover this exception
  if(this.class == "VegSpatial" & (is.null(layers) | layers[1] == "all")) {layers <- names(data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers[1] == "all") {layers = names(data)}
  
  
  ###  If wSpatialPixelsDataFrame we can plot it
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in veg-runtools.R")
    data.raster <- raster(data, layers)
  }
  ### If data.table or VegSpatial (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | this.class == "VegSpatial"){
    if(this.class == "data.table") data.spdf <- .makeSPDFfromDT(data, layers, tolerance, grid.topology = NULL)
    if(this.class == "VegSpatial") data.spdf <- .makeSPDFfromDT(data@data, layers, tolerance, grid.topology = NULL)
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
    stop(paste("Trying to promote object of type", class(data), "to Raster, which I don't know how to do.", sep = ""))
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
  ### If VegSpatial (which contains a data.table) 
  else if(this.class == "VegSpatial"){
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

doTimeAverage <- function(input.dt,
                          period = new("Period", name = "Default", start = 1961, end = 1990),
                          verbose = FALSE){
  
  # Messy solution to stop "notes" about undeclared global variables stemming from data.table syntax 
  # Possible can solve this by replace the subset function
  Year = Lat = Lon = NULL
  
  # Do the averaging
  if(verbose) message(paste("Averaging from ", period@start, " to ", period@end, "...", sep = ""))
  
  # do the temporal averaging between first.year and last.year 
  output.dt <- subset(input.dt, Year >= period@start & Year <= period@end)[,lapply(.SD, mean), by=list(Lat, Lon)]
  
  # remove the Year 'cos it dun' make so much sense no mo'
  output.dt[,Year:=NULL]
  
  
  # MF: alternative way with nice syntax but doesn't produce binary identical results
  # don't know why, it is frustrating
  #     t1 <- Sys.time()
  #     output.dt.2 <- input.dt[Year >= period@start & Year <= period@end, lapply(.SD,mean), by=list(Lat, Lon)]
  #     output.dt.2[,Year:=NULL]
  #     t2 <- Sys.time()
  #     print(t2-t1)
  #     print("check")
  #     print(identical(output.dt, output.dt.2))
  #   
  if(verbose) message("Averaged")
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Return the averaged table
  return(output.dt)
  
}

.doTimeAverage.cmpd <- cmpfun(doTimeAverage)


selectYears <- function(input.dt, temporal.extent){
  
  output.dt <- subset(input.dt, Year >= temporal.extent@start & Year <= temporal.extent@end)
  
  return(output.dt)    
  
}


######################### SPACE AVERAGE AN LPJ-GUESS FULL OUTPUT FILE COMING IN AS A data.table  ##############################

doSpaceAverage <- function(input.dt,
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
    output.dt <- input.dt[,lapply(.SD, weighted.mean, w=area), by=list(Year)]
    output.dt[,area:=NULL]
  } else {
    if(verbose) message(paste("Spatially averaging (not area weighted) whole domain...", sep = ""))
    output.dt <- input.dt[,lapply(.SD, mean), by=list(Year)]
  }
  
  # remove the Lon and Lat columns 'cos they dun' make so much sense no mo'
  output.dt[,Lon:=NULL]
  output.dt[,Lat:=NULL]
  
  if(verbose) message("Averaged")
  
  # Delete the full dataset to free up memory - necessary??
  rm(input.dt)
  gc()
  
  # Return the averaged table
  return(output.dt)
  
}

.doSpaceAverage.cmpd <- cmpfun(doSpaceAverage)




.LondonCentre <- function(lon) {
  
  if(lon <= 180) return(lon)
  else return(lon - 360)
  
}

