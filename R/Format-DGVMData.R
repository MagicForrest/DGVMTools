#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE DGVMData FILES ###########################################################
############################################################################################################################


#' Get a Field for DGVMData
#' 
#' 
#' An internal function that reads data from an DGVMData .nc file.  
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param quant A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param target.sta.info An STAInfo object defining the spatial-temporal-annual extent over which we want the data
#' @param last.year The last year (as a numeric) of the data to be returned
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tablle containing the data, and secondly the STAInfo for the data that we have
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_DGVMData <- function(source,
                              quant,
                              target.STAInfo,
                              verbose = FALSE) {
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to read DGVMData files.")
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Time = NULL
  
  # get fist and last year for use later on
  first.year = target.STAInfo@first.year
  last.year = target.STAInfo@last.year
  
  # function read attributes and warn if they are not present.
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
  
  
  # STAInfo object describing the data
  sta.info = new("STAInfo")
  
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.name.nc <- file.path(source@dir, paste(quant@id, "nc", sep = "."))
  file.name.nc.gz <- paste(file.name.nc, "gz", sep = ".")
  zipped <- FALSE
  if(file.exists(file.name.nc)){ 
    if(verbose) message(paste("Found and opening file", file.name.nc, sep = " "))
  }
  else if(file.exists(file.name.nc.gz)){
    zipped <- TRUE
    R.utils::gunzip(file.name.nc.gz)
    if(verbose) message(paste("Found, gunzipping and opening file", file.name.nc.gz, sep = " "))
  }
  
  # Open file and get global attributes
  if(verbose) message(paste0("Opening file ", file.name.nc))     
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE ))
  global.attributes <- ncdf4::ncatt_get(this.nc, 0, attname=NA, verbose=FALSE)
  
  # Check out dimensions
  dims.present <- names(this.nc$dim)
  if(verbose) message(paste("File has dimensions", paste(dims.present, collapse = ","), sep = " "))
  
  
  all.lats <- numeric(0)
  all.lons <- numeric(0)
  all.times <- numeric(0)
  for(this.dimension in dims.present) {
    
    # pick up Lat
    if(this.dimension == "lat") { all.lats <- this.nc$dim$lat$vals  }
    else if(this.dimension == "Lat") { all.lats <- this.nc$dim$Lat$vals }
    else if(this.dimension == "latitude") { all.lats <- this.nc$dim$latitude$vals }
    else if(this.dimension == "Latitude") { all.lats <- this.nc$dim$Latitude$vals }
    
    # pick up Lon
    else if(this.dimension == "lon") { all.lons <- this.nc$dim$lon$vals }
    else if(this.dimension == "Lon") { all.lons <- this.nc$dim$Lon$vals }
    else if(this.dimension == "longitude") { all.lons <- this.nc$dim$longitude$vals }
    else if(this.dimension == "Longitude") { all.lons <- this.nc$dim$Longitude$vals }
    
    # pick up Time/time
    else if(this.dimension == "time") { all.times <- this.nc$dim$time$vals }
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
  first.year  <- getGlobalAttribute("first.year", global.attributes)
  last.year  <- getGlobalAttribute("last.year", global.attributes)
  year.aggregate.method <- getGlobalAttribute("year.aggregate.method", global.attributes)
  if(!is.null(first.year)) sta.info@first.year <- first.year
  if(!is.null(last.year)) sta.info@last.year <- last.year
  if(!is.null(year.aggregate.method)) sta.info@year.aggregate.method <- year.aggregate.method
  
  
  ###  HACK!! - for some reason this ncdf4 package is reading time value dimensions as NA
  ###  These values seem to be perfectly valid according to ncview and ncdump, so I don't know what is going on.
  ###  In the mean time, if any NA's detected, make up a time axis (this will be over-written below anyways, they just need to be no NAs)
  if(length(all.times) > 0){
    time.NAs.present  <- FALSE
    for(check.thing in all.times){
      if(is.na(check.thing)) time.NAs.present <- TRUE
    }
    if(time.NAs.present) {
      # quick check if yearly data
      if(length(first.year:last.year) == length(all.times)) {
        dimension.names[["Time"]] <- first.year:last.year
        all.times <- first.year:last.year
      }
      # else assume monthly data
      else {
        dimension.names[["Time"]] <- 1:length(all.times)
        all.times <- 1:length(all.times)
      }
    }
  }
  
  
  dt.list <- list()
  
  for(this.var in this.nc$var) {
    
    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = start, count = count, verbose = verbose)
    dimnames(this.slice) <- dimension.names
    
    # prepare data.table from the slice (array)
    this.slice.dt <- as.data.table(melt(this.slice))
    rm(this.slice)
    gc()
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
        
        sta.info@subannual.resolution <- "Year"
        sta.info@subannual.original <- "Year"
      }
      # else check for monthly
      if(length.time.axis == (last.year - first.year + 1) *12) {
        if(verbose) message("Got monthly data.")
        
        all.years <- first.year:last.year
        
        ### MF: This might be slow, consider optimising
        
        # sort data.table by Time axis
        this.slice.dt[order(Time)] 
        
        # make Year vector and add it do the data.table
        temp.nentries.per.year <- nrow(this.slice.dt)/length(all.years)
        year.vector <- c()
        
        for(year in all.years) {
          year.vector <- append(year.vector, rep.int(year, temp.nentries.per.year))
        }
        this.slice.dt[, Year := year.vector]
        rm(year.vector)
        
        # Make Month vector
        month.vector <- c()
        for(year in all.years) {
          for(month in 1:12) {
            month.vector <-append(month.vector, rep.int(month, temp.nentries.per.year/12))
          }
        }
        this.slice.dt[, Month := month.vector]
        rm(month.vector)
        
        # Remove the Time columns
        this.slice.dt[, Time := NULL]
        
        # make new colum order so that the quant is last
        all.names <- names(this.slice.dt)
        all.names <- all.names[-which(all.names == this.var$name)]
        all.names <- append(all.names, this.var$name)
        setcolorder(this.slice.dt, all.names)
        
        sta.info@subannual.resolution <- "Month"
        sta.info@subannual.original <- "Month"
      }
      
      
    }
    else {
      sta.info@subannual.resolution <- "Year"
      sta.info@subannual.original <- "Year"
    }
    
    setKeyDGVM(this.slice.dt)
    
    
    if(length(quant@units) > 1) {
      
      # # remove categories which aren't present
      categories.present <- unique(this.slice.dt[[this.var$name]])
      all.categories <- quant@units
      
      this.slice.dt[,this.var$name := factor(this.slice.dt[[this.var$name]], labels = all.categories[sort(categories.present)])]
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
  
  rm(dt.list)
  gc()
  
  if(verbose) message("Setting key")
  dt <- setKeyDGVM(dt)
  
  # STInfo
  st.info <- getDimInfo(dt)
  
  
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
  
  
  ### DETERMINE SPATIAL EXTENT 
  
  # simple ones
  spatial.extent.id  <- getGlobalAttribute("spatial.extent.id", global.attributes)
  spatial.aggregate.method  <- getGlobalAttribute("spatial.aggregate.method", global.attributes)
  if(!is.null(spatial.extent.id)) sta.info@spatial.extent.id <- spatial.extent.id
  if(!is.null(spatial.aggregate.method)) sta.info@spatial.aggregate.method <- spatial.aggregate.method
  
  # first attempt to use the attributes from the NetCDF file
  xmin  <- getGlobalAttribute("xmin", global.attributes)
  xmax  <- getGlobalAttribute("xmax", global.attributes)
  ymin  <- getGlobalAttribute("ymin", global.attributes)
  ymax  <- getGlobalAttribute("ymax", global.attributes)
  # else attempt to lookm of from Lon and Lat columns
  if(!is.null(xmin) & !is.null(xmax) & !is.null(ymin) & !is.null(ymax)){
    sta.info@spatial.extent <- raster::extent(xmin, xmax, ymin,ymax)
    if(verbose) message("Setting spatial extent from DGVMData attributes")
  }
  else if("Lon" %in% st.info && "Lat" %in% st.info) {
    sta.info@spatial.extent <- extent(dt)
    if(verbose) message("Setting spatial extent from Lon and Lat dimensions")
  }
  else{
    if(verbose) message(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    warning(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    sta.info@spatial.extent <- numeric(0)
  }
  
  
  
  # if london.centre is requested, shift to -180 to +180
  if(length(all.lons) > 0) {
    if(source@london.centre  && max(all.lons) >= 180){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  }
  
  
  # set some attributes about the file - works!
  attr(dt, "shadeToleranceCombined") <- FALSE
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs, complete list and return
  dt <- stats::na.omit(dt)
  
  # close the netcdf file and if necessary zip again
  ncdf4::nc_close(this.nc)
  if(zipped) {
    R.utils::gzip(file.name.nc)
  }
  
  return(list(dt = dt, 
              sta.info = sta.info))
  
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


availableQuantities_DGVMData <- function(source){
  
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


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' @include colour-palettes.R
#' 
#' 
DGVMData.quantities <- list(
  
  
  new("Quantity",
      id = "fraction",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      format = c("DGVMData"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("DGVMData"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("DGVMData"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      units = "1",
      colours = viridis::viridis,
      format = c("DGVMData"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      units = "kgC m-2 year-1",
      colours = fields::tim.colors,
      format = c("DGVMData"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      format = c("DGVMData")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("DGVMData"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("DGVMData"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = veg.palette,
      format = c("DGVMData"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = veg.palette,
      format = c("DGVMData"))
  
)


####################################################
########### DGVMDATA FORMAT ########################
####################################################


#' @description \code{DGVMData} - a format defined here to unify the multitude of different datasets into a common format.  It is essentially CF-compliant netCDF with a couple of extra attributes defined. 
#' Can be produced using the companion DGVMData package. 
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @include colour-palettes.R
#' @export
DGVMData <- new("Format",
                
                # UNIQUE ID
                id = "DGVMData",
                
                # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
                determinePFTs = determinePFTs_DGVMData,
                
                # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                availableQuantities = availableQuantities_DGVMData,
                
                # FUNCTION TO READ A FIELD 
                getField = getField_DGVMData,
                
                # DEFAULT GLOBAL PFTS  
                default.pfts = list(),
                
                # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
                quantities = DGVMData.quantities
                
)

