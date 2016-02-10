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
######################### FUNCTIONS TO HANDLE FILES AND DATATABLES #########################################################
############################################################################################################################



######################### OPEN AN LPJ-GUESS *.out FILE  #####################################################################

openLPJOutputFile <- function(run,
                              variable,
                              verbose = FALSE,
                              gzip = FALSE){
  
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.string = file.path(run@run.dir, paste(variable, ".out", sep=""))
  if(file.exists(file.string)){ 
    if(verbose) message(paste("Found and opening file", file.string, sep = " "))
    was.gzipped <- FALSE
  }
  else if(file.exists(paste(file.string, "gz", sep = "."))){
    if(verbose) message(paste("File ", file.string, ", but gzipped file present so using that", sep = ""))
    system(paste("gunzip",  paste(file.string, "gz", sep = "."), sep = " "))
    was.gzipped <- TRUE
  }
  else {
    stop(paste("File (or gzipped file) not found:", file.string))
  }
  
  # Use the quite fantastic readr package to read the LPJ-GUESS files which have fixed width formatting
  # Read the column names
  column.names <- unlist(read.table(file.string, nrows = 1, header = FALSE, sep ='', stringsAsFactors = FALSE))
  # Get the widths, and note that we also gotta broaden the widths because the fwf_empty() function reliably detects the end of a field but not the beginning
  widths <- fwf_empty(file.string, skip = 1, col_names = column.names)
  widths$begin[1] <- 0
  for(counter in 2:length(widths$begin)){
    widths$begin[counter] <- widths$end[counter-1]+1
  }
  # Read the file and put it straight into a data.table
  dt <- as.data.table(read_fwf(file.string, widths, skip = 1))
  
  ### OLD WAY!  Slower, also depends on awk, this is definitely deprecated by the 
  ### Read using fread function from data.table but with the white spaces handled correctly using an awk command
  ### at time of writing fread does not handles multiple whitespaces as separators
  ### get the number of columns which is and read the file
  #ncols <- length(names(read.table(file.string, header=TRUE, dec=".",  colClasses="numeric", comment.char="", nrows = 1)))
  #dt <- awkFread(file.string, colNums = c(1:ncols), header=T)
  
  
  # Gzip the file again if that was requested or if the 
  if(gzip | was.gzipped){
    if(verbose) message(paste("Gzipped file ", file.string, " again", sep = ""))
    system(paste("gzip",  file.string, sep = " "))
  }
  
  
  #  Print messages
  if(verbose) {
    message("Read table. It has header:")
    print(names(dt))
    message("It has shape:")
    print(dim(dt))      
  }
  
  
  # Correct year, lons and lats
  if(verbose)message("Correcting years, lons and lats...")
  if(run@year.offset != 0) dt[,Year := Year + run@year.offset]
  if(length(run@lonlat.offset) == 2 ){
    if(run@lonlat.offset[1] != 0) dt[, Lon := Lon + run@lonlat.offset[1]]
    if(run@lonlat.offset[2] != 0) dt[, Lat := Lat + run@lonlat.offset[2]]
  }
  
  else if(length(run@lonlat.offset) == 1 ){
    if(run@lonlat.offset[1] != 0) dt[, Lon := Lon + run@lonlat.offset[1]]
    if(run@lonlat.offset[1] != 0) dt[, Lat := Lat + run@lonlat.offset[1]]
  }
  
  
  if(verbose)message("Corrected.")
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(run@london.centre){ dt[, Lon := vapply(dt[,Lon], 1, FUN = .LondonCentre)] }
  
  # set some attributes about the file - works!
  attr(dt, "shadeToleranceCombined") <- FALSE
  
  # set keys
  setkey(dt, Lon, Lat, Year)
  
  # remove any NAs
  dt <- na.omit(dt)
  
  return(dt)
  
}



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
  if(length(info@tolerance) == 0)  info@tolerance <- 0.0000001
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
  
  # return a VegRun object with empty data fieldsbut meta data filled  
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
        if(verbose) message(paste("File ",  TA.filename, " not found in directory ",  run@run.dir, " and ", var.string, ".out is not already read, reading .out file.", sep = ""))
        this.full <- openLPJOutputFile(run, var.string, verbose = TRUE)
      }
      else{
        if(verbose) message(paste("File ",  TA.filename, " not found in directory ",  run@run.dir, " but ", var.string, ".out is already read, so using that.", sep = ""))
      }
      
      # do temporal averaging
      this.TA.dt <- doTimeAverage.cmpd(this.full, period, verbose)
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
             id = paste(var.string, period@id),
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
    SA.filename <- paste(run@run.dir, "/", var, ".SA.Rtable", sep ="")
  } 
  else {
    SA.filename <- paste(run@run.dir, "/", var, ".", extent@id, ".SA.Rtable", sep ="")
  }
  
  # if file is present and we are not forcing re-averaging, read in the pre-averaged file
  if(file.exists(paste(SA.filename)) & !forceReAveraging){
    if(verbose) {message(paste("File",  SA.filename, "found in",  run@run.dir, "so using that.",  sep = " "))}
    this.SA.dt <- fread(SA.filename, header = TRUE, stringsAsFactors=FALSE)
  } 
  
  # otherwise, open the full .out file read it as a data table, time average it and save to disk
  else {
    if (is.null(this.full)) {
      if(verbose) message(paste("File ",  SA.filename, " not found in directory ",  run@run.dir, " and ", var, ".out is not already read, reading .out file.", sep = ""))
      this.full <- openLPJOutputFile(run, var, verbose = TRUE)
    }
    else{
      if(verbose) message(paste("File ",  SA.filename, " not found in directory ",  run@run.dir, " but ", var, ".out is already read, so using that.", sep = ""))
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
  
  return(new("VegTemporal",
             id = paste(var.string, period@id),
             data = this.SA.dt,
             spatial.extent = spatial.extent,
             quant = quant,
             run = as(run, "VegRunInfo")))
}


######################### CONVERT AN TEMPORALLY AVERAGED DATA.TABLE TO A SPATIALPIXELSDATAFRAME OBJECT #####################################


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


promoteToRaster <- function(data, layers = "all", tolerance = 0.0000001, grid.topology = NULL){
  
  ###  Get class of the objetc we are dealing with
  this.class = class(data)[1]
  
  ###  Define the layers we are pulling out
  # for VegSpatial - note could define a methods "names" do cover this exception
  if(this.class == "VegSpatial" & (is.null(layers) | layers == "all")) {layers <- names(data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers == "all") {layers = names(data)}
  
  
  ###  If wSpatialPixelsDataFrame we can plot it
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in lpj-runtools.R")
    data.raster <- raster(data, layers)
  }
  ### If data.table or VegSpatial (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | this.class == "VegSpatial"){
    
    if(this.class == "data.table") data.spdf <- makeSPDFfromDT(data, layers, tolerance, grid.topology = NULL)
    if(this.class == "VegSpatial") data.spdf <- makeSPDFfromDT(data@data, layers, tolerance, grid.topology = NULL)
    
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
  # remove the Year 'cos it dun' make so much sense
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
  
  # remove the Lon and Lat columns 'cos they dun' make so much sense
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






############################################################################################################################
######################### MISC FUNCTION FOR HANDLING FILENAMES, VARIABLE LISTS ETC #####################################
############################################################################################################################


listAllOutputFiles <- function(run.directory){
  
  # First get the list of *.out files present
  files.present <- list.files(run.directory, "*.out")
  
  # Now strip the .out file extension out the get the variable name
  this.var.list <- unlist(lapply(files.present, FUN = .trim.var.filename))
  
  # Sometimes there is a file called "*.out", remove it so code doesn't fall over
  if("*" %in% this.var.list) {
    this.var.list <- this.var.list[-which(this.var.list == "*")]
  }
  
  # Sometimes there is a file called "*guess_out.out", remove it so code doesn't fall over
  if("guess_out" %in% this.var.list) {
    this.var.list <- this.var.list[-which(this.var.list == "guess_out")]
  }
  
  # Sometimes there is a file called "*guess_err.out", remove it so code doesn't fall over
  if("guess_err" %in% this.var.list) {
    this.var.list <- this.var.list[-which(this.var.list == "guess_err")]
  }
  
  
  return(this.var.list)
  
}



# handy function for trimming file names to get a variable name
.trim.var.filename <- function(var.filename){
  return(substr( var.filename, 1, (nchar(var.filename) - nchar(".out"))))
}



.LondonCentre <- function(lon) {
  
  if(lon <= 180) return(lon)
  else return(lon - 360)
  
}


writeNetCDF <- function(raster.in, var.name, var.units, time.resolution = "annual", time.units.string = NULL, long.name = "", filename = "test.nc", ordering = "standard", missing.value = -999999){
  
  # MAKE LONGITUDE AND LATITUDE DIMENSION
  lon.list <- seq(from = xmin(raster.in) + xres(raster.in)/2 , to = xmax(raster.in) - xres(raster.in)/2, by = xres(raster.in))
  lon.dim <- ncdim_def("Lon", "degrees", lon.list, unlim=FALSE, create_dimvar=TRUE)
  lat.list <- seq(from = ymin(raster.in) + yres(raster.in)/2 , to = ymax(raster.in) - yres(raster.in)/2, by = yres(raster.in))
  lat.dim <- ncdim_def("Lat", "degrees", lat.list, unlim=FALSE, create_dimvar=TRUE)
  
  # MAKE TIME DIMENSION
  # monthly - format as "days since YYYY-MM-DD HH:MM:SS"
  if(tolower(time.resolution) == "monthly" || tolower(time.resolution) == "month"){ 
    midpoints <- c(16,44,75,105,136,166,197,228,258,289,319,350)
    ncycles <- ceiling(nlayers(raster.in)/12)
    long.midpoints <- c()
    for(cycle in 0:(ncycles-1)){
      long.midpoints <- append(long.midpoints,midpoints  + (365*cycle))
    }
    time.list <- long.midpoints[1:nlayers(raster.in)]
    time.dim <- ncdim_def("Time", time.units.string, time.list, unlim=FALSE, create_dimvar=TRUE)
  }
  # annual - format as "days since YYYY-MM-DD HH:MM:SS"
  else if(tolower(time.resolution) == "annual" || tolower(time.resolution) == "yearly"){
    
    time.list <- seq(from = 0, to = nlayers(raster.in)-1, by = 1)*365
    time.dim <- ncdim_def("Time", time.units.string, time.list, unlim=FALSE, create_dimvar=TRUE)
    
  }
  
  # PERMUTE THE DATA AND SET UP THE VARIABLE DIMENSION
  
  # convert raster to an array
  array.out <- drop(as.array(raster.in))
  
  # For time series
  if(length(dim(array.out)) == 3) {
    
    # reverse latitudes, necessary, but I don't know why
    array.out <- array.out[dim(array.out)[1]:1,,]
    
    # order dimensions as requested and create empty netCDF file
    if(tolower(ordering) == "standard" || tolower(ordering) == "std") {
      array.out <- aperm(array.out, c(2,1,3))  
      var.out <- ncvar_def(var.name, var.units, list(lon.dim, lat.dim,  time.dim), longname = long.name, missing.value)  # standard
    }
    else if(tolower(ordering) == "lpj" || tolower(ordering) == "lpj-guess"){
      array.out <- aperm(array.out, c(3,2,1))
      var.out <- ncvar_def(var.name, var.units, list(time.dim, lon.dim, lat.dim ), missing.value)  # order for fast access in LPJ-GUESS
    }
    else {
      stop(paste("writeNetCDF: Unknown ordering requested: ", ordering))
    }
    
  }
  # For a single map (no-time series)
  else if(length(dim(array.out)) == 2){
    
    # reverse latitudes, necessary, but I don't know why
    array.out <- array.out[dim(array.out)[1]:1,]
    
    # here lon and lat are standard because it is the time axis that cause somplication in LPJ guess
    array.out <- aperm(array.out, c(2,1))  
    var.out <- ncvar_def(var.name, var.units, list(lon.dim, lat.dim), longname = long.name, missing.value)  # standard
    
    
  }
  
  
  
  # CREATE FILE AND ADD THE VARIABLE
  outfile <- nc_create(filename, var.out, verbose=FALSE)
  ncvar_put(outfile, var.name,  array.out, start=NA, count=NA, verbose=FALSE)
  print(paste("Saving variable", var.name, "to file",  filename, sep =" " ), quote=FALSE)
  
  # ADD ATTRIBUTES
  ncatt_put(outfile, "Lon" , "units", "degrees_east")
  ncatt_put(outfile, "Lon" , "axis", "X")
  ncatt_put(outfile, "Lon" , "standard_name", "longitude")
  ncatt_put(outfile, "Lon" , "long_name", "longitude")
  
  ncatt_put(outfile, "Lat" , "units", "degrees_north")
  ncatt_put(outfile, "Lat" , "axis", "Y")
  ncatt_put(outfile, "Lat" , "standard_name", "latitude")
  ncatt_put(outfile, "Lat" , "long_name", "latitude")
  
  if(length(dim(array.out)) == 3){
    ncatt_put(outfile, "Time" , "units", time.units.string)
    ncatt_put(outfile, "Time" , "axis", "T")
    ncatt_put(outfile, "Time" , "standard_name", "time")
    ncatt_put(outfile, "Time" , "long_name", "Time")
    ncatt_put(outfile, "Time" , "calendar", "365_day")
  }
  
  ncatt_put(outfile, 0, "Conventions", "CF-1.6")
  
  
  # CLOSE
  nc_close(outfile)
  
}

