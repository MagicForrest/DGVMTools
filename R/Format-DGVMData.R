#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE DGVMData FILES ###########################################################
############################################################################################################################


#' Get a Field for DGVMData
#' 
#' 
#' An internal function that reads data from an LPJ-GUESS run.  It actually call one of three other functions depending on the type of quantity specified.   
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param quant A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be returned
#' @param last.year The last year (as a numeric) of the data to be returned
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tabel containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_DGVMData <- function(source,
                           quant,
                           target.STAInfo,
                           verbose) {
  
  
    this.dt <- openDGVMDataFile(source, quant, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, verbose = verbose)

  return(this.dt)
  
}



######################### OPEN A DGVMData *.nc FILE  #####################################################################
#' Open a DGVMData *.n file
#'
#' \code{penDGVMDataFile} returns a data.table object given a string defining a vegetation quantity 
#' from the source (eg. "lai", to read the file "lai.out") and  \code{Source} object which defines where the source is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param source A \code{Source} containing the meta-data about the LPJ-GUESS source
#' @param quant A string the define what output file from the LPJ-GUESS source to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be returned
#' @param last.year The last year (as a numeric) of the data to be returned
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @keywords internal
openDGVMDataFile <- function(source,
                             quant,
                             first.year,
                             last.year,
                             verbose = FALSE){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Time = NULL
  
  getGlobalAttribute <- function(attr.name, global.attributes) {
    dgvm.attr.name <- paste("DGVMData", attr.name, sep = "_")
    if(dgvm.attr.name %in% names(global.attributes)) {
      return(global.attributes[[dgvm.attr.name]]) 
    }
    else {
      warning(paste0("No ", attr.name,  " attribute (", dgvm.attr.name, " ) found in file ", file.name.nc, ".  This is not DGVMData compliant.  Right now setting attribute to NULL, but this might cause problems later"))
      return(NULL)
    }
  }
  
  
  # The list of things to return
  data.list <-list()
  
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.name.nc <- file.path(source@dir, paste(source@id, quant@id, "nc", sep = "."))
  if(file.exists(file.name.nc)){ 
    if(verbose) message(paste("Found and opening file", file.name.nc, sep = " "))
  }
  else if(file.exists(paste(file.name.nc, "gz", sep = "."))){
    #dt <- fread(paste("zcat < ", paste(file.name.nc, "gz", sep = "."), sep = ""))
    stop("Gunzipping not yet supported for DGVMData.")
  }
  else {
    file.name.nc.old <- file.name.nc
    file.name.nc <- file.path(source@dir, paste(quant@id, "nc", sep = "."))
    if(file.exists(file.name.nc)){ 
      if(verbose) message(paste("Found and opening file", file.name.nc, sep = " "))
    }
    else if(file.exists(paste(file.name.nc, "gz", sep = "."))){
      #dt <- fread(paste("zcat < ", paste(file.name.nc, "gz", sep = "."), sep = ""))
      stop("Gunzipping not yet supported for DGVMData.")
    }
    else {
      stop(paste("File (or gzipped file) not found:", file.name.nc.old, "or", file.name.nc))
    }
  }
  
  
  # Open file and get global attributes
  message(paste0("Opening file ", file.name.nc))     
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE ))
  global.attributes <- ncatt_get(this.nc, 0, attname=NA, verbose=FALSE)
  
  # Check out dimensions
  dims.present <- names(this.nc$dim)
  message(paste("File has dimensions", paste(dims.present, collapse = ","), sep = " "))
  
  
  all.lats <- numeric(0)
  all.lons <- numeric(0)
  all.times <- numeric(0)
  for(this.dimension in dims.present) {
    
    # pick up Lat/lat
    if(this.dimension == "lat") { all.lats <- this.nc$dim$lat$vals  }
    else if(this.dimension == "Lat") { all.lats <- this.nc$dim$Lat$vals }
    
    # pick up Lon/lon
    else if(this.dimension == "lon") { all.lons <- this.nc$dim$lon$vals }
    else if(this.dimension == "Lon") { all.lons <- this.nc$dim$Lon$vals }
    
    # pick up Time/time
    else if(this.dimension == "time") { all.time <- this.nc$dim$time$vals }
    else if(this.dimension == "Time") { all.times <- this.nc$dim$Time$vals }
    
    # Catch the rest
    else {
      stop(paste("Unknown dimension found", this.dimension, " which I don't know what to do with."))
    }
    
  }
  
  # check the number of dimensions and pull time if present
  start <- rep(1, length(dims.present))
  count <- rep(-1, length(dims.present))
  
  dimension.names <- list()
  if(length(all.lons) > 0) dimension.names[["Lon"]] <- all.lons
  if(length(all.lats) > 0) dimension.names[["Lat"]] <- all.lats
  if(length(all.times) > 0) dimension.names[["Time"]] <- all.times
  
  # look up year-related attributes
  data.list[["first.year"]]  <- getGlobalAttribute("first.year", global.attributes)
  data.list[["last.year"]]  <- getGlobalAttribute("last.year", global.attributes)
  data.list[["year.aggregate.method"]]  <- getGlobalAttribute("year.aggregate.method", global.attributes)
  first.year <- data.list[["first.year"]] 
  last.year <- data.list[["last.year"]] 
  
  ###  HACK!! - for some reason this ncdf4 package is reading time value dimensions as NA
  ###  These values seem to be perfectly valid according to ncview and ncdump, so I don't know what is going on.
  ###  In the mean time, if any NA's detected, make up a time axis (this will be over-written below anyways, they just need to be no NAs)
  if(length(all.times) > 0){
    time.NAs.present  <- FALSE
    for(check.thing in all.times){
      if(is.na(check.thing)) time.NAs.present <- TRUE
    }
    if(time.NAs.present) {
      all.times <- first.year:last.year
      dimension.names[["Time"]] <- all.times
    }
  }
  
  
  dt.list <- list()
  
  for(this.var in this.nc$var) {
    
    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = start, count = count, verbose = verbose)
    dimnames(this.slice) <- dimension.names
    
    # prepare data.table from the slice (array)
    this.slice.dt <- as.data.table(melt(this.slice))
    this.slice.dt <- stats::na.omit(this.slice.dt)
    setnames(this.slice.dt, "value", this.var$name)
    
    
    ###  HANDLE TIME AXIS 
    if(length(all.times) > 0) {
      
      # some tricks to determine exactly what the 'Time' axis represent 
      
      # don't need to be so fancy here!
      # time.axis.string <- this.nc$dim$Time$units
      
      
      # actually, lets do some shortcuts
      length.time.axis <- length(all.times)
      
      # lets see if we have annual data
      if(length.time.axis == (last.year - first.year + 1)) {
        if(verbose) message("Got annual data.")
        
        
        # subsitute using first.year and last.year   
        this.slice.dt[, Year := plyr::mapvalues(this.slice.dt[["Time"]], from = all.times, to = first.year:last.year)]
        this.slice.dt[, Time := NULL]
      }
      # else check for monthly
      if(length.time.axis == (last.year - first.year + 1) *12) {
        if(verbose) message("Got monthly data.")
        
        all.years <- first.year:last.year
        
        # sort data.table by Time axis
        this.slice.dt[order(Time)] 
        
        # make Year vector and add it do the data.table
        temp.nentries.per.year <- nrow(this.slice.dt)/length(all.years)
        year.vector <- c()
        for(year in all.years) {
          year.vector <- append(year.vector, rep.int(year, temp.nentries.per.year))
        }
        this.slice.dt[, Year := year.vector]
        
        # Make Month vector
        month.vector <- c()
        for(year in all.years) {
          for(month in 1:12) {
            month.vector <-append(month.vector, rep.int(month, temp.nentries.per.year/12))
          }
        }
        this.slice.dt[, Month := month.vector]
        
        
        # Remove the Time columns
        this.slice.dt[, Time := NULL]
        
        # make new colum order so that the quant is last
        all.names <- names(this.slice.dt)
        all.names <- all.names[-which(all.names == this.var$name)]
        all.names <- append(all.names, this.var$name)
        setcolorder(this.slice.dt, all.names)
        
        
      }
      
      
    }
    
    setKeyDGVM(this.slice.dt)
    
    
    if(quant@type == "categorical") {
      this.slice.dt[,this.var$name := factor(this.slice.dt[[this.var$name]], labels = quant@units)]
    }
  
    # now join this to all.dt
    dt.list[[length(dt.list)+1]] <- this.slice.dt
    
   
    
  }
  
  
  # join all together and set key
  dt <- dt.list[[1]]
  

  if(length(dt.list) > 1) {
    for(this.dt in dt.list[2:length(dt.list)]) {
     dt <- merge(x = dt, y = this.dt)
    }
  }
  
  if(verbose) message("Setting key")
  dt <- setKeyDGVM(dt)
  
  # STInfo
  st.info <- getDimInfo(dt)
  
  
  ### DETERMINE SPATIAL EXTENT 
  
  # simple ones
  data.list[["spatial.extent.id"]]  <- getGlobalAttribute("spatial.extent.id", global.attributes)
  data.list[["spatial.aggregate.method"]]  <- getGlobalAttribute("spatial.aggregate.method", global.attributes)
  
  # first attempt to use the attributes from the NetCDF file
  xmin  <- getGlobalAttribute("xmin", global.attributes)
  xmax  <- getGlobalAttribute("xmax", global.attributes)
  ymin  <- getGlobalAttribute("ymin", global.attributes)
  ymax  <- getGlobalAttribute("ymax", global.attributes)
  # else attempt to lookm of from Lon and Lat columns
  if(!is.null(xmin) & !is.null(xmax) & !is.null(ymin) & !is.null(ymax)){
    data.list[["spatial.extent"]] <- raster::extent(xmin, xmax, ymin,ymax)
    if(verbose) message("Setting spatial extent from DGVMData attributes")
  }
  else if("Lon" %in% st.info && "Lat" %in% st.info) {
    data.list[["spatial.extent"]] <- extent(dt)
    if(verbose) message("Setting spatial extent from Lon and Lat dimensions")
  }
  else{
    if(verbose) message(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    warning(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    data.list[["spatial.extent"]] <- extent(c(0,1,0,1))
  }
  
  
  
  # Correct year, lons and lats
  if(verbose) message("Correcting years, lons and lats with offsets...")
  if(source@year.offset != 0 && "Year" %in% st.info) dt[,Year := Year + source@year.offset]
  if(length(source@lonlat.offset) == 2 ){
    if(source@lonlat.offset[1] != 0) dt[, Lon := Lon + source@lonlat.offset[1]]
    if(source@lonlat.offset[2] != 0) dt[, Lat := Lat + source@lonlat.offset[2]]
  }
  
  else if(length(source@lonlat.offset) == 1 ){
    if(source@lonlat.offset[1] != 0) dt[, Lon := Lon + source@lonlat.offset[1]]
    if(source@lonlat.offset[1] != 0) dt[, Lat := Lat + source@lonlat.offset[1]]
  }
  
  if(verbose) {
    message("Offsets applied. Head of full .out file (after offsets):")
    print(utils::head(dt))
  }
  
  # if london.centre is requested, shift to -180 to +180
  if(length(all.lons) > 0) {
    if(source@london.centre  && max(all.lons) >= 180){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  }
  
  
  # set some attributes about the file - works!
  attr(dt, "shadeToleranceCombined") <- FALSE
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs, complete list and reutn
  dt <- stats::na.omit(dt)
  data.list[["dt"]] <- dt
  return(data.list)
  
}





######################### LIST ALL LPJ-GUESS OUTPUT VARIABLES (STORED AS *.out FILES) IN AN source DIRECTORY  #####################################################################
#' List all LPJ-GUESS *.out files in a source directory
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "guess.out" and "*.out" 
#' 
#' @param source A path to a directory on the file system containing some .out files
#' @return A list of all the .out files present, with the ".out" removed. 
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


determineQuantities_DGVMData <- function(source){
  
  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")
  
  quantities.present <- list()
  for(file in files.present) {
    
    # check if file contains paste(".", source@id, ".nc")
    # and if so, get the part before it
    this.quantity <- "lsls" # TODO
    quantities.present <- append(quantities.present, this.quantity)
    
    
  }
  
  return(quantities.present)
  
}


#' Detemine PFTs present in an DGVMData source 
#' 
#' @param x  A Source objects describing a DGVMData source
#' @param variables Some variable to look for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.  Currently ignored.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_DGVMData <- function(x, variables) {
  
  warning("Datasets don't normally have PFTs (or they are not likely to be defined consistently) so I am not looking for them.  Instead I am returning the source@format@pft.set argument directly (in case you defined some yourself, which would be the way to go in this case)")
  return(x@format@pft.set)
  
}


########################################################
########### DGVMData QUANTITIES ########################
########################################################

#' @title dummy text
#' 
#' @description
#' 
#' @details DGVMData Output Quantities
#' 
#' @format A list of \code{Quantity} objects that store meta-data for standard output variabla for supported models
#' @rdname Quantity-class
#' @keywords datasets
#' 
#' 
DGVMData.quantities <- list(
  
  
  new("Quantity",
      id = "fraction",
      type = "unknown",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      model = c("DGVMData"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      type = "",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      model = c("DGVMData"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      type = "PFT",
      units = "kg m-2",
      colours = reversed.viridis,
      model = c("DGVMData"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      type = "PFT",
      units = "1",
      colours = viridis::viridis,
      model = c("DGVMData"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      type = "monthly",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      model = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      type = "PFT",
      units = "kgC m-2 year-1",
      colours = fields::tim.colors,
      model = c("DGVMData"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      type = "PFT",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      model = c("DGVMData")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      type = "-",
      units = "m",
      colours = reversed.magma,
      model = c("DGVMData"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      type = "annual",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      model = c("DGVMData"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      type = "annual",
      units = "fraction",
      colours = veg.palette,
      model = c("DGVMData"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      type = "annual",
      units = "GtC/year",
      colours = veg.palette,
      model = c("DGVMData"))
  
)


####################################################
########### DGVMDATA FORMAT ########################
####################################################

#' @title dummy text
#' 
#' @description dummy description 
#' 
#' @details DGVMData Format 
#' 
#' @format A list of \code{Quantity} objects that store meta-data for standard output variabla for supported models
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' 
#' 
DGVMData <- new("Format",
             
             # UNIQUE ID
             id = "DGVMData",
             
             # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
             determinePFTs = determinePFTs_DGVMData,
             
             # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
             determineQuantities = determineQuantities_DGVMData,
             
             # FUNCTION TO READ A FIELD 
             getField = getField_DGVMData,
             
             # DEFAULT GLOBAL PFTS  
             default.pfts = list(),
             
             # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
             quantities = DGVMData.quantities
             
)

