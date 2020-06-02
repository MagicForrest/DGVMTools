#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE LPJ-GUESS FILES ###########################################################
############################################################################################################################

#' Get a Field for LPJ-GUESS
#' 
#' An internal function that reads data from an LPJ-GUESS run.  It actually call one of three other functions depending on the type of quantity specified.   
#' 
#' @param source A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param quant A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param target.STAInfo The spatial-temporal target domain
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tabel containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_GUESS <- function(source,
                           quant,
                           target.STAInfo,
                           file.name,
                           verbose,
                           ...) {
  
  # First check if quantity is for FireMIP, if so call a special function with the extra processing required
  if("FireMIP" %in% quant@format) {
    return(openLPJOutputFile_FireMIP(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  else if("GUESS" %in% quant@format | "LPJ-GUESS-SPITFIRE" %in% quant@format) {
    return(openLPJOutputFile(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  else if("Standard" %in% quant@format) {
    return(getStandardQuantity_LPJ(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  else{
    stop("Unrecognised Format of Quantity in 'quant' argument to getField_GUESS()")
  }
  
  
  
}


######################### OPEN AN LPJ-GUESS *.out FILE  #####################################################################
#' Open an LPJ-GUESS .out file
#'
#' \code{openLPJOutputFile} returns a data.table object given a string defining a vegetation quantity 
#' from the run (eg. "lai", to read the file "lai.out") and  \code{Source} object which defines where the run is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param quant A Quant to define what output file from the LPJ-GUESS run to open, 
#' can also be a simple string defining the LPJ-GUESS output file if the \code{return.data.table} argument is TRUE
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param data.table.only A logical, if TRUE return a data.table and not a Field
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @keywords internal
openLPJOutputFile <- function(run,
                              quant,
                              target.sta,
                              file.name = file.name,
                              verbose = FALSE,
                              data.table.only = FALSE,
                              ...){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Annual = Year = Month = Day = NULL
  
  # extract from the target.sta
  first.year = target.sta@first.year
  last.year = target.sta@last.year
  
  if(!data.table.only && class(quant)[1] != "Quantity") stop("Please supply a formal Quantity object as the quant argument since you are not requesting at data.table")
  
  if(class(quant)[1] == "Quantity") variable <- quant@id
  else variable <- quant
  
  
  # Make the filename and read the file using the handy utility function
  if(is.null(file.name)) file.string <- file.path(run@dir, paste(variable, ".out", sep=""))
  else file.string <- file.path(run@dir, file.name)
  dt <- readRegularASCII(file.string, verbose)
  
  #  Print messages
  if(verbose) {
    message("Read table. It has header:")
    print(names(dt))
    message("It has shape:")
    print(dim(dt))      
  }
  
  # Correct year
  if(run@year.offset != 0) {
    dt[,Year := Year + run@year.offset]
    if(verbose) message("Correcting year with offset.")
  }
  
  # Select year
  if(length(first.year) == 0) first.year <- NULL
  if(length(last.year) == 0) last.year <- NULL
  if(!missing(first.year) & !missing(last.year) & !is.null(first.year) & !is.null(last.year)) {
    dt <- selectYears(dt, first.year, last.year)
  }
  
  # also correct days to be 1-365 instead of 0-364, if necessary
  if("Day" %in% names(dt)) {
    if(0 %in% unique(dt[["Day"]])) dt[, Day := Day+1]
  }
  
  # Correct lon and lats
  if(length(run@lonlat.offset) == 2 ){
    if(verbose) message("Correcting lons and lats with offset.")
    if(run@lonlat.offset[1] != 0) dt[, Lon := Lon + run@lonlat.offset[1]]
    if(run@lonlat.offset[2] != 0) dt[, Lat := Lat + run@lonlat.offset[2]]
  }
  else if(length(run@lonlat.offset) == 1 ){
    if(verbose) message("Correcting lons and lats with offset.")
    if(run@lonlat.offset[1] != 0) dt[, Lon := Lon + run@lonlat.offset[1]]
    if(run@lonlat.offset[1] != 0) dt[, Lat := Lat + run@lonlat.offset[1]]
  }
  
  if(verbose) {
    message("Offsets applied. Head of full .out file (after offsets):")
    print(utils::head(dt))
  }
  
  # if london.centre is requested, make sure all longitudes greater than 180 are shifted to negative
  if(run@london.centre){
    if(max(dt[["Lon"]]) > 180) {
      dt[, Lon := LondonCentre(Lon)]
    }
  }
  
  
  # if spatial extent specified, crop to it
  new.extent <- NULL
  full.extent <- extent(dt)
  if(!is.null(target.sta@spatial.extent)) {
    
    spatial.extent.class <- class(target.sta@spatial.extent)[1]
    
    if(spatial.extent.class == "SpatialPolygonsDataFrame" || spatial.extent.class == "numeric" || is.data.frame(target.sta@spatial.extent) || is.data.table(target.sta@spatial.extent)) {
      dt <- selectGridcells(x = dt, gridcells = target.sta@spatial.extent, spatial.extent.id = target.sta@spatial.extent.id, ...)
      new.extent <- target.sta@spatial.extent
      # if new.extent is a data.frame, convery it to a data.table for consistency
      #if(is.data.frame(new.extent) & !is.data.table(new.extent)) new.extent <- as.data.table(new.extent)
    }
    
    else {
      dt <- crop(x = dt, y = target.sta@spatial.extent, spatial.extent.id = target.sta@spatial.extent.id)
      new.extent <- extent(target.sta@spatial.extent)
    } 
    
  }
  
  gc()
  
  
  # if year cropping selected, do that here, before aggregating
  all.years <- sort(unique(dt[["Year"]]))
  
  crop.first <- FALSE
  if(length(target.sta@first.year) == 1) {
    if(target.sta@first.year != min(all.years)) {
      first.year <- target.sta@first.year
      crop.first <- TRUE
    }
    else {
      first.year <- min(all.years)
      crop.first <- FALSE
    }
  }
  
  crop.last <- FALSE
  if(length(target.sta@last.year) == 1) {
    if(target.sta@last.year != max(all.years)) {
      last.year <- target.sta@last.year
      crop.last <- TRUE
    }
    else {
      last.year <- target.sta@last.year
      crop.last <- FALSE
    }
  }
  
  if(crop.first || crop.last) {
    
    if(verbose) message(paste("Selecting years from", first.year, "to", last.year, sep = " "))
    dt <- selectYears(dt, first = first.year, last = last.year) 
    all.years <- sort(unique(dt[["Year"]]))
  }
  else {
    if(verbose) message("No year selection being applied")
  }
  
  
  
  # if yearly aggregating requested, so it before melting (so save on memory)
  # first store all the years before averaging them away

  this.year.aggregate.method <- "none"
  if(target.sta@year.aggregate.method != "none") {
    
    dt <- aggregateYears(input.obj = dt, method = target.sta@year.aggregate.method, verbose = verbose)
    this.year.aggregate.method <- target.sta@year.aggregate.method
    
  }
  
  gc()
  
  
  
  # If data is has monthly or daily columns, melt to long/tidy data where "Month" becomes a column
  
  # first get of all the columns which are not spatial-temporal info
  all.cols <- names(dt)
  st.cols <- getDimInfo(dt)
  nonst.cols <- all.cols[!all.cols %in% st.cols]
  
  # if monthly then melt
  standard.monthly.ljp.col.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  if(identical(nonst.cols, standard.monthly.ljp.col.names)){
    
    # replace column names with 1,2,3.. etc before melting, and then melt
    setnames(dt, old = standard.monthly.ljp.col.names, new = paste(1:12))
    dt <- melt(dt, id.vars = st.cols, measure.vars = paste(1:12), variable.name = "Month", value.name = variable)
    dt[, Month := as.integer(Month)]
    
  }
  
  
  # if daily then melt
  # TODO - implement daily melting, follow above for implementation
  
  
  
  
  # set some attributes about the data - works!
  # currently screws up unit tests and isn't used.  Consider using if updating metadata system.
  # setattr(dt, "shadeToleranceCombined", FALSE)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  # set the keys (very important!)
  setKeyDGVM(dt)
  
  # Build as STAInfo object describing the data
  dimensions <- getDimInfo(dt)
  subannual <- "Year"
  if("Month" %in% dimensions) subannual <- "Month"
  else if("Day" %in% dimensions) subannual <- "Day"
  
  
  sta.info = new("STAInfo",
                 first.year = min(all.years),
                 last.year = max(all.years),
                 year.aggregate.method = this.year.aggregate.method,
                 subannual.resolution = subannual,
                 subannual.original = subannual)
  
  # if cropping has been done, set the new spatial.extent and spatial.extent.id
  if(!is.null(new.extent))  {
    sta.info@spatial.extent = new.extent
    sta.info@spatial.extent.id <- target.sta@spatial.extent.id
  }
  # otherwise set to the (potentially what the extent was before spatial aggregating)
  else {
    sta.info@spatial.extent = full.extent
    sta.info@spatial.extent.id <- "Full"
  }
  
  gc()
  
  if(data.table.only) return(dt)
  else {
    
    # make the ID and then make and return Field
    field.id <- makeFieldID(source = run, var.string = variable, sta.info = sta.info)
    
    return(
      
      new("Field",
          id = field.id,
          data = dt,
          quant = quant,
          source = run,
          sta.info 
      )
      
    )
    
  }
  
  
}


######################### OPEN AN LPJ-GUESS *.out FILE  #####################################################################
#' Open an LPJ-GUESS .out file
#'
#' \code{openLPJOutputFile} returns a data.table object given a string defining a vegetation quantity 
#' from the run (eg. "lai", to read the file "lai.out") and  \code{Source} object which defines where the run is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param quant A Quantity to define what output file from the LPJ-GUESS run to open.
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @import data.table
openLPJOutputFile_FireMIP <- function(run,
                                      quant,
                                      target.sta,
                                      file.name = file.name,
                                      verbose = FALSE,
                                      soil_water_capacities = "none",
                                      ...){
  
  Lon = Lat = Seconds = Month = Total = mwcont_lower = mwcont_upper = maet= mevap = mintercep = mrso = mrsos = Capacity = Code = NULL
  target.cols = SoilfC = SoilsC = NULL
  
  
  variable = quant@id
  
  # seconds in month
  seconds.in.month <- c()
  for(month in all.months) {
    seconds.in.month <- append(seconds.in.month, month@days * 24 * 60 * 60)
  }
  
  
  ################################################################################################
  ############# PER PFT VARIABLES
  
  if(variable == "lai") {
    dt <- openLPJOutputFile(run, "lai", data.table.only = TRUE, target.sta = target.sta,  file.name = file.name, verbose = verbose)
  }
  if(variable == "landCoverFrac") {
    dt <- openLPJOutputFile(run, "fpc", data.table.only = TRUE, target.sta = target.sta,  file.name = file.name, verbose = verbose)
  }
  if(variable == "theightpft") {
    dt <- openLPJOutputFile(run, "speciesheights", data.table.only = TRUE, target.sta = target.sta,  file.name = file.name, verbose = verbose)
  }
  
  
  ################################################################################################
  ############ MONTHLY VARIABLES (NOT PER PFT)
  
  # These will often require some sort of unit conversions
  monthly.to.second <- FALSE
  CO2.to.C <- FALSE
  CO.to.C <- FALSE
  monthly.to.percent <- FALSE
  monthly <- FALSE
  
  ## Here look for variables that require per month to per second
  if(variable == "gpp") {
    guess.var <- "mgpp"
    monthly.to.second <- TRUE
  }
  if(variable == "npp") {
    guess.var <- "mnpp"
    monthly.to.second <- TRUE
  }
  if(variable == "nbp") {
    guess.var <- "mnbp"
    monthly.to.second <- TRUE
  }
  if(variable == "ra") {
    guess.var <- "mra"
    monthly.to.second <- TRUE
  }
  if(variable == "rh") {
    guess.var <- "mrh"
    monthly.to.second <- TRUE
  }
  if(variable == "mrro") {
    guess.var <- "mrunoff"
    monthly.to.second <- TRUE
  }
  if(variable == "tran") {
    guess.var <- "maet"
    monthly.to.second <- TRUE
  }
  if(variable == "evspsblveg") {
    guess.var <- "mintercep"
    monthly.to.second <- TRUE
  }
  if(variable == "evspsblsoi") {
    guess.var <- "mevap"
    monthly.to.second <- TRUE
  }
  
  ## Here look for variables that require conversion to percent
  if(variable == "BA") {
    guess.var <- "monthly_burned_area"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuelLiveGrass") {
    guess.var <- "mlivegrass_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel1hr") {
    guess.var <- "m1hr_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel10hr") {
    guess.var <- "m10hr_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel100hr") {
    guess.var <- "m100hr_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel1000hr") {
    guess.var <- "m1000hr_cc"
    monthly.to.percent <- TRUE
  }
  
  ## Here we have simple monthly variables (no conversion)
  if(variable == "cFuelLiveGrass") {
    guess.var <- "mlivegrass_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel1hr") {
    guess.var <- "m1hr_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel10hr") {
    guess.var <- "m10hr_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel100hr") {
    guess.var <- "m100hr_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel1000hr") {
    guess.var <- "m1000hr_fuel"
    monthly <- TRUE
  }
  if(variable == "mFuelDead") {
    guess.var <- "mdlm_deadfuel"
    monthly <- TRUE
  }
  if(variable == "mFuelLiveGrass") {
    guess.var <- "mdlm_livegrass"
    monthly <- TRUE
  } 
  if(variable == "nrfire") {
    guess.var <- "real_num_fires"
    monthly <- TRUE
  }
  if(variable == "cMortality") {
    guess.var <- "monthly_nind_killed"
    monthly <- TRUE
  }
  if(variable == "intensFire") {
    guess.var <- "real_intensity"
    monthly <- TRUE
  }
  if(variable == "durat") {
    guess.var <- "real_duration"
    monthly <- TRUE
  }
  if(variable == "RoS") {
    guess.var <- "mRoS"
    monthly <- TRUE
  }
  
  ## Finally we have a couple of monthly to second variables which also required molar conversion to C
  if(variable == "fFire") {
    guess.var <- "m_co2flux_fire"
    monthly.to.second <- TRUE
    CO2.to.C <- TRUE
  }
  if(variable == "coFire") {
    guess.var <- "m_coflux_fire"
    monthly.to.second <- TRUE
    CO.to.C <- TRUE
  }
  
  
  # Now calculate these bad boys
  if(monthly.to.second || monthly.to.percent || monthly){
    
    dt <- openLPJOutputFile(run, guess.var, target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setnames(dt, guess.var, variable)
    if(monthly.to.second){
      #suppressWarnings(dt[, Seconds := seconds.in.month[Month]])
      dt[, Seconds := seconds.in.month[Month]]
      dt[, (variable) := get(variable)/Seconds]
      dt[, Seconds := NULL]
    }
    if(monthly.to.percent){
      dt[, (variable) := get(variable) * 100]
    }
    if(CO2.to.C){
      dt[, (variable ):= get(variable) * 12 / 44]
    }
    if(CO.to.C){
      dt[, (variable) := get(variable) * 12 / 28]
    }
    
  }
  
  ### Special monthly variables
  if(variable == "meanFire") {
    guess.var <- "real_fire_size"
    dt <- openLPJOutputFile(run, guess.var, target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setnames(dt, guess.var, variable)
    dt[, (variable) := get(variable) * 10000]
  }
  
  if(variable == "mrso") {
    
    
    # standard stuf for LPJ-GUESS
    wcap <- c(0.110, 0.150, 0.120, 0.130, 0.115, 0.135, 0.127, 0.300, 0.100)
    thickness_upper_layer_mm <- 500
    thickness_lower_layer_mm <- 1000
    
    dt_cap <- fread(soil_water_capacities)
    
    setnames(dt_cap, c("Lon", "Lat", "Code"))
    dt_cap[, Lat := Lat + 0.25]
    dt_cap[, Lon := Lon + 0.25]
    dt_cap <- subset(dt_cap, Code>0)
    setkey(dt_cap, Lon, Lat)
    dt_cap[, Capacity := wcap[Code]]
    dt_cap[, Code := NULL]
    
    dt_upper <- openLPJOutputFile(run, "mwcont_upper", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt_upper)
    
    dt_lower <- openLPJOutputFile(run, "mwcont_lower", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt_lower)
    dt <- dt_upper[dt_lower]
    
    dt <- dt[dt_cap]
    dt <- stats::na.omit(dt)
    dt[, mrso := (mwcont_lower * thickness_lower_layer_mm * Capacity) + (mwcont_upper * thickness_upper_layer_mm * Capacity)]
    dt[, mwcont_lower := NULL]
    dt[, mwcont_upper := NULL]
    dt[, Capacity := NULL]
    
    rm(dt_upper,dt_lower)
    gc()
    
  }
  if(variable == "mrsos") {
    
    
    # standard stuf for LPJ-GUESS
    wcap <- c(0.110, 0.150, 0.120, 0.130, 0.115, 0.135, 0.127, 0.300, 0.100)
    thickness_upper_layer_mm <- 500
    
    dt_cap <- fread(soil_water_capacities)
    
    setnames(dt_cap, c("Lon", "Lat", "Code"))
    dt_cap[, Lat := Lat + 0.25]
    dt_cap[, Lon := Lon + 0.25]
    dt_cap <- subset(dt_cap, Code>0)
    setkey(dt_cap, Lon, Lat)
    dt_cap[, Capacity := wcap[Code]]
    dt_cap[, Code := NULL]
    
    dt <- openLPJOutputFile(run, "mwcont_upper", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt)
    
    dt <- dt[dt_cap]
    dt <- stats::na.omit(dt)
    dt[, mrsos := mwcont_upper * thickness_upper_layer_mm * Capacity]
    
    dt[, mwcont_upper := NULL]
    dt[, Capacity := NULL]
    
    rm(dt_cap)
    gc()
    
  }
  
  if(variable == "evapotrans") {
    
    # firstly combine transpiration and evaporation
    dt_trans <- openLPJOutputFile(run, "maet", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    dt_evap <- openLPJOutputFile(run, "mevap", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt_trans)
    setKeyDGVM(dt_evap)
    dt_trans <- dt_evap[dt_trans]
    rm(dt_evap)
    gc()
    
    # now add interception
    dt_intercep <- openLPJOutputFile(run, "mintercep", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt_intercep)
    dt_trans <- dt_trans[dt_intercep]
    rm(dt_intercep)
    
    
    # shade.tolerance, convert and clean up
    dt_trans[, (variable) := maet + mevap + mintercep]
    dt_trans[, maet := NULL]
    dt_trans[, mevap := NULL]
    dt_trans[, mintercep := NULL]
    dt_trans[, Seconds := seconds.in.month[Month]]
    dt_trans[, (variable) := get(variable)/Seconds]
    dt_trans[, Seconds := NULL]
    dt <- dt_trans
  }
  
  ### ANNUAL C POOLS FROM cpool.out FILE
  
  if(variable == "cVeg") {
    dt <- openLPJOutputFile(run, "cpool", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "VegC")
    dt <- dt[,target.cols,with=FALSE]
    setnames(dt, "VegC", "cVeg")
  }
  if(variable == "cLitter") {
    dt <- openLPJOutputFile(run, "cpool", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "LittC")
    dt <- dt[,target.cols,with=FALSE]
    setnames(dt, "LittC", "cLitter")
  }
  if(variable == "cSoil") {
    dt <- openLPJOutputFile(run, "cpool", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(unlist(getDimInfo(dt)), c("SoilfC", "SoilsC"))
    dt <- dt[,target.cols,with=FALSE]
    dt[, "cSoil" := SoilfC + SoilsC]
    dt[, SoilfC := NULL]
    dt[, SoilsC := NULL]
  }
  
  ### LAND USE FLUX AND STORE FROM luflux.out FILE
  
  if(variable == "cProduct") {
    dt <- openLPJOutputFile(run, "luflux", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "Products_Pool")
    dt <- dt[,target.cols, with = FALSE]
    setnames(dt, "Products_Pool", "cProduct")
  }
  
  if(variable == "fLuc") {
    
    dt <- openLPJOutputFile(run, "luflux", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "Deforest_Flux")
    dt <- dt[,target.cols, with = FALSE]
    setnames(dt, "Deforest_Flux", "fLuc")
  }
  
  # Build as STAInfo object describing the data
  all.years <- sort(unique(dt[["Year"]]))
  dimensions <- getDimInfo(dt)
  subannual <- "Year"
  if("Month" %in% dimensions) subannual <- "Month"
  else if("Day" %in% dimensions) subannual <- "Day"
  
  
  sta.info = new("STAInfo",
                 first.year = min(all.years),
                 last.year = max(all.years),
                 subannual.resolution = subannual,
                 subannual.original = subannual,
                 spatial.extent = extent(dt))
  
  
  # make the ID and then make and return Field
  field.id <- makeFieldID(source = run, var.string = variable, sta.info = sta.info)
  
  
  return(
    
    new("Field",
        id = field.id,
        data = dt,
        quant = quant,
        source = run,
        sta.info 
    )
    
  )
  
  
}



#' Returns the data from one LPJ-GUESS output variable as a \code{data.table}.   
#'
#' 
#' This funtion can retrieve a 'Standard' vegetation quantity (returned as a data.table) with standard definition and units
#' to compare to other models and to data.  This must be implemented for each and every Standard quantity 
#' for each and every model to to ensure completeness.
#' 
#' 
#' output variable.  Normally it will read the file from disk, but if that has already been done, and the \code{data.table} has been saved to the 
#' \code{Source} object, it will return that to save time.
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run from which the data is to be read.  Most importantly it must contain the run.dara nd the offsets.
#' @param quant A Quantity to define what output file from the LPJ-GUESS run to open
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @keywords internal

getStandardQuantity_LPJ <- function(run, 
                                    quant, 
                                    target.sta,
                                    file.name = file.name,
                                    verbose = FALSE) {
  
  Total = Year = FireRT = NULL
  
  # columns not to be modified for unit conversions etc.
  unmod.cols <- c("Lon", "Lat", "Year", "Month", "Day")
  
  # Check that this really is a standard Quantity and therefore should have behaviour defined here
  if(!"Standard" %in% quant@format) {
    stop((paste("getStandardQuantity_LPJ called for a non-Standard Quantity (", quant@id, ")", sep = "")))
  }
  
  
  
  #### Here is the code to define each and every Standard Quantity for LPJ-GUESS output
  
  # vegcover_std
  if(quant@id == "vegcover_std") {
    
    # vegcover.out provides the right quantity here (note this is not standard LPJ-GUESS)
    this.Field <- openLPJOutputFile(run, lookupQuantity("vegcover", GUESS), target.sta, file.name = file.name, verbose = verbose)
    
    # But we need to scale it to %
    if(verbose) message("Multiplying fractional areal vegetation cover by 100 to get percentage areal cover")
    this.dt <- this.Field@data
    mod.cols <- names(this.dt)
    mod.cols <- mod.cols[!mod.cols %in% unmod.cols]
    this.dt[, (mod.cols) := lapply(.SD, function(x) x * 100 ), .SDcols = mod.cols]
    if("Grass" %in% names(this.dt)) { setnames(this.dt, old = "Grass", new = "NonTree") }
    this.Field@data <- this.dt
    
    return(this.Field)
    
  }
  
  # vegC_std 
  else if(quant@id == "vegC_std") {
    
    # cmass provides the right quantity here - so done
    this.Field <- openLPJOutputFile(run, lookupQuantity("cmass", GUESS), target.sta, file.name = file.name, verbose = verbose)
    
  }
  
  # LAI_std 
  else if(quant@id == "LAI_std") {
    
    # lai provides the right quantity here - so done
    this.Field <- openLPJOutputFile(run, lookupQuantity("lai", GUESS), target.sta, file.name = file.name, verbose = verbose)
    
  }
  
  # FPAR_std 
  else if(quant@id == "FPAR_std") {
    
    # lai provides the right quantity here - so done
    this.Field <- openLPJOutputFile(run, lookupQuantity("fpc", GUESS), target.sta, file.name = file.name, verbose = verbose)
    all.layers <- layers(this.Field)
    drop.layers <- all.layers [! all.layers %in% c("Total")]
    this.Field@data[, (drop.layers) := NULL]
    this.Field@data[, Total := pmin(Total, 1) * 100 * 0.83]
    
  }
  
  # aGPP_std 
  else if(quant@id == "aGPP_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    if(file.exists(file.path(run@dir, "agpp.out")) || file.exists(file.path(run@dir, "agpp.out.gz"))){
      this.Field <- openLPJOutputFile(run, lookupQuantity("agpp", GUESS), target.sta, file.name = file.name, verbose = verbose)
    }
    else {
      this.Field <- openLPJOutputFile(run, lookupQuantity("mgpp", GUESS), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field, method = "sum", target = "Year")
    }
    
  }
  
  
  # aNPP_std 
  else if(quant@id == "aNPP_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    
    if(file.exists(file.path(run@dir, "anpp.out")) || file.exists(file.path(run@dir, "anpp.out.gz"))){
      this.Field <- openLPJOutputFile(run, lookupQuantity("anpp", GUESS), target.sta, file.name = file.name, verbose = verbose)
    }
    else{
      this.Field <-  openLPJOutputFile(run, lookupQuantity("mnpp", GUESS), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field , method = "sum", target = "Year")
    }
    
  }
  
  # mNPP_std 
  else if(quant@id == "aNEE_std") {
    
    this.Field <- openLPJOutputFile(run, lookupQuantity("cflux", GUESS), target.sta, file.name = file.name, verbose = verbose)
    
    # take NEE and  ditch the rest
    all.layers <- layers(this.Field)
    drop.layers <- all.layers [! all.layers %in% c("NEE")]
    this.Field@data[, (drop.layers) := NULL]
    
  }
  
  # canopyheight_std 
  else if(quant@id == "canopyheight_std") {
    
    # The canopyheight output fromth e benchmarkoutput output module is designed to be exactly this quantity
    this.Field <- openLPJOutputFile(run, lookupQuantity("canopyheight", GUESS), target.sta, file.name = file.name, verbose = verbose)
    renameLayers(this.Field, "CanHght", "CanopyHeight")
    
  }
  
  # burntfraction_std 
  else if(quant@id == "burntfraction_std") {
    
    # if annual_burned_area is present the open it and use it
    if("annual_burned_area" %in% availableQuantities_GUESS(run, names=TRUE)){
      this.Field <- openLPJOutputFile(run, lookupQuantity("annual_burned_area", GUESS), target.sta, file.name = file.name, verbose = verbose)
      renameLayers(this.Field, quant@id)
    }
    
    # if monthly_burned_area is present the open it and use it
    else if("monthly_burned_area" %in% availableQuantities_GUESS(run, names=TRUE)){
      this.Field <- openLPJOutputFile(run, lookupQuantity("monthly_burned_area", GUESS), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field, method = "sum")
      renameLayers(this.Field, quant@id)
      
    }
    
    
    # if mfirefrac is present the open it and use it
    else if("mfirefrac" %in% availableQuantities_GUESS(run, names=TRUE)){
      this.Field <- openLPJOutputFile(run, lookupQuantity("mfirefrac", GUESS), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field, method = "sum")
      renameLayers(this.Field, "mfirefrac", quant@id)
      
    }
    
    # otherwise open firert to get GlobFIRM fire return interval and invert it
    else {
      this.Field <- openLPJOutputFile(run, lookupQuantity("firert", GUESS), target.sta, file.name = file.name, verbose = verbose)
      this.Field@data[, "burntfraction_std" :=  1 / FireRT]
      this.Field@data[, FireRT :=  NULL]
    }
    
  }
  
  # mfire_size_std 
  else if(quant@id == "mfire_size_std") {
    
    this.Field <- openLPJOutputFile(run, lookupQuantity("real_fire_size", GUESS), target.sta, file.name = file.name, verbose = verbose)
    renameLayers(this.Field, "real_fire_size", quant@id)
    
  }  
  
  # else stop
  else {
    
    stop(paste("Unfortunately standard quantity", quant@id, "does not seem to be available in LPJ-GUESS"))
    
  }
  
  # Update the Field with the 'standard' Quantity (also Field id) and return
  this.Field@quant <- quant
  this.Field@id <- makeFieldID(source = this.Field@source, var.string = quant@id, sta.info = as(object = this.Field, Class = "STAInfo"))
  return(this.Field)
  
}



######################### LIST ALL LPJ-GUESS OUTPUT VARIABLES (STORED AS *.out FILES) IN AN RUN DIRECTORY  #####################################################################
#' List all LPJ-GUESS *.out files in a run directory
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "guess.out" and "*.out" 
#' 
#' @param source A GUESS source object
#' @param names Logical, if TRUE return the namse of the quantities, if FLASE return the quanties themseleves
#' @return A list of all the .out files present, with the ".out" removed. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

availableQuantities_GUESS <- function(source, names = TRUE, verbose = FALSE){
  
  directory <- source@dir
  
  # First get the list of *.out files present
  files.present <- list.files(directory, ".out$")
  files.present <- append(files.present, list.files(directory, ".out.gz$"))
  
  # Now strip the .out file extension out the get the variable name
  this.var.list <- unlist(lapply(files.present, FUN = LPJQuantFromFilename))
  
  
  # check that they correspond to actual quantities, if not through a warning and chuck them out
  good.list <- list()
  ignore.list <- c("*", "guess_out", "guess_err")
  
  for(this.file in files.present){
    
    variable<- LPJQuantFromFilename(this.file)
    
    if(!variable %in% ignore.list) {
      
      result = tryCatch({
        dummy.quant <- suppressWarnings(lookupQuantity(variable, source@format))
      },  warning = function(w) {
        #warning(w)
      }, error = function(e) {
        #warning(e)
      }, finally = {
      })
      
      if(is.Quantity(result))  {
        if(names) good.list <- append(good.list, variable)
        else good.list <- append(good.list, result)
      }
      else {
        if(verbose) warning("Although I have found file with an appropriate extension that looks like an LPJ-GUESS output variable (", this.file, "), I have no Quantity object corrsponding to \"", variable, "\".  I am therefore ignoring it.  \n However, not to worry! If you want this file included, you can easily add a new Quantity to the dgvm.quantities list (just in your analysis script, doesn't need to be in the package).")
      }
      
    }
    
  }
  
  return(unlist(good.list))
  
}


######################### TRIM AN LPJ-GUESS FILENAME  #####################################################################
#' Helper function to raster::trim the ".out" or the ".out.gz" from an LPJ-GUESS filename to get the variable in question
#' 
#' Returns NULL if the last characters are not ".out" or ".out.gz
#'
#' @param var.filename The string of the filename to be raster::trimmed
#' @return A string less the last fou.
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


# handy helper function for raster::trimming file names to get a variable name
LPJQuantFromFilename <- function(var.filename){
  
  for(ending in c(".out", ".out.gz")) {
    if(substr(var.filename, nchar(var.filename) - nchar(ending) + 1,  nchar(var.filename)) == ending) return(substr(var.filename, 1, nchar(var.filename) - nchar(ending)))
  }
  
  return(NULL)
  
}


#####################################################################
########### LPJ-GUESS(-SPITFIRE) GLOBAL LAYERS ########################
#####################################################################


#' @format An S4 class object with the slots as defined below.
#' @rdname Layer-class
#' @keywords datasets
GUESS.Layers <- list(
  
  # BOREAL TREES
  new("Layer",
      id = "BNE",
      name = "Boreal Needleleaved Evergreen Tree",
      colour = "darkblue",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "BINE",
      name = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
      colour = "dodgerblue3",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "BNE",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "BNS",
      name = "Boreal Needleleaved Summergreen Tree",
      colour = "cadetblue2",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Summergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "IBS",
      name = "Shade-intolerant B/leaved Summergreen Tree",
      colour = "chartreuse",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  
  # TEMPERATE TREES
  new("Layer",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      colour = "darkgreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "TeNE",
      name = "Temperate Needleleaved Evergreen Tree",
      colour = "lightseagreen",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "TeBS",
      name = "Temperate Broadleaved Summergreen Tree",
      colour = "darkolivegreen3",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Temperate",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  
  
  # TROPICAL TREES
  new("Layer",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      colour = "orchid4",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "TrIBE",
      name = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
      colour = "orchid",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Evergreen",
                        climate.zone = "Tropical", 
                        shade.tolerance = "TrBE",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "TrBR",
      name = "Tropical Broadleaved Raingreen Tree",
      colour = "palevioletred",
      properties = list(type = "PFT",
                        growth.form = "Tree",
                        leaf.form = "Broadleaved",
                        phenology = "Raingreen",
                        climate.zone = "Tropical",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  
  
  # GRASSES
  new("Layer",
      id = "C3G",
      name = "Boreal/Temperate Grass",
      colour = "lightgoldenrod1",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "C4G",
      name = "Tropical Grass",
      colour = "sienna1",
      properties = list(type = "PFT",
                        name = "Tropical Grass",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ), 
  
  # SHRUBS
  new("Layer",
      id = "BLSE",
      name = "Boreal Evergreen Low Shrub",
      colour = "plum",
      properties = list(type = "PFT",
                        name = "Boreal Evergreen Low Shrub",
                        growth.form = "Shrub",
                        leaf.form = "Needleleaved",
                        phenology = "Evergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "BLSS",
      name = "Boreal Summergreen Low Shrub",
      colour = "mistyrose3",
      properties = list(type = "PFT",
                        name = "Boreal Summergreen Low Shrub",
                        growth.form = "Shrub",
                        leaf.form = "Broadleaved",
                        phenology = "Summergreen",
                        climate.zone = "Boreal",
                        shade.tolerance = "None",
                        land.cover = "Natural")
  ),
  
  # PASTURE PFTS
  new("Layer",
      id = "C3G_pas",
      name = "Boreal/Temperate Pasture Grass",
      colour = "lightgoldenrod4",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Pasture")
  ),
  new("Layer",
      id = "C4G_pas",
      name = "Tropical Pasture Grass",
      colour = "sienna3",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Pasture")
  ),
  
  # CROP AND INTERCROP GRASS PFTS
  new("Layer",
      id = "CC3G_ic",
      name = "Boreal/Temperate Intercrop Grass",
      colour = "palegreen2",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland")
  ),
  new("Layer",
      id = "CC4G_ic",
      name = "Tropical Intercrop Grass",
      colour = "palegreen4",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "GrassPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland")
  ),
  new("Layer",
      id = "CC3ann",
      name = "C3 Annual Crop",
      colour = "red",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = FALSE)
  ),
  new("Layer",
      id = "CC3per",
      name = "C3 Perennial Crop",
      colour = "red",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = FALSE)
  ),
  new("Layer",
      id = "CC3nfx",
      name = "C3 Nitrogen Fixing Crop",
      colour = "red",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = FALSE)
  ),
  new("Layer",
      id = "CC4ann",
      name = "C4 Annual Crop",
      colour = "red",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = FALSE)
  ),
  new("Layer",
      id = "CC4per",
      name = "C4 Perennial Crop",
      colour = "red",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = FALSE)
  ),
  new("Layer",
      id = "CC3anni",
      name = "Irrigated C3 Annual Crop",
      colour = "slategray",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = TRUE)
  ),
  new("Layer",
      id = "CC3peri",
      name = "Irrigated C3 Perennial Crop",
      colour = "slategray",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = TRUE)
  ),
  new("Layer",
      id = "CC3nfxi",
      name = "Irrigated C3 Nitrogen Fixing Crop",
      colour = "slategray",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = TRUE)
  ),
  new("Layer",
      id = "CC4anni",
      name = "Irrigiated C4 Annual Crop",
      colour = "slategray",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = TRUE)
  ),
  new("Layer",
      id = "CC4peri",
      name = "Irrigated C4 Perennial Crop",
      colour = "slategray",
      properties = list(type = "PFT",
                        growth.form = "Grass",
                        leaf.form = "Broadleaved",
                        phenology = "CropPhenology",
                        climate.zone = "NA",
                        shade.tolerance = "None",
                        land.cover = "Cropland",
                        irrigated = TRUE)
  ),
  
  # PFT LANDCOVER AGGREGATES
  new("Layer",
      id = "Crop_sum",
      name = "Crop Sum",
      colour = "Red",
      properties = list(type = "Sum",
                        land.cover = "Cropland")
  ),
  new("Layer",
      id = "Pasture_sum",
      name = "Pasture Sum",
      colour = "springgreen3",
      properties = list(type = "Sum",
                        land.cover = "Pasture")
  ),
  new("Layer",
      id = "Natural_sum",
      name = "Natural Sum",
      colour = "saddlebrown",
      properties = list(type = "Sum",
                        land.cover = "Natural")
  ),
  new("Layer",
      id = "Barren_sum",
      name = "Barren Sum",
      colour = "gray75",
      properties = list(type = "Sum",
                        land.cover = "Barren")
  ),
  new("Layer",
     id = "Total",
     name = "Total",
     colour = "black",
     properties = list(type = "Sum",
                       land.cover = "All")
  )
  
  
)

#####################################################################
########### LPJ-GUESS(-SPITFIRE) QUANTITIES ########################
#####################################################################


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' @include colour-palettes.R
#' 
#' 
GUESS.quantities <- list(
  new("Quantity",
      id = "lai",
      name = "LAI",
      units = "m^2/m^2",
      colours = reversed.viridis,
      format = c("GUESS"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mlai",
      name = "Monthly LAI",
      units = "m^2/m^2",
      colours = viridis::viridis,
      format = c("GUESS"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "fpc",
      name = "Foliar Projective Cover",
      units = "m^2/m^2",
      colours = veg.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mfpc",
      name = "Monthly Foliar Projective Cover",
      units = "m^2/m^2",
      colours = veg.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "vegcover",
      name = "Vegetation Cover",
      units = "%",
      colours = veg.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "agpp",
      name = "Annual GPP",
      units = "kgC/m2/year",
      colours = viridis::inferno,
      format = c("GUESS")),
  
  new("Quantity",
      id = "cmass",
      name = "Vegetation Carbon Mass",
      units = "kgC/m^2",
      colours = viridis::viridis,
      format = c("GUESS")),
  
  new("Quantity",
      id = "clitter",
      name = "Litter Carbon Mass",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "agb",
      name = "Above Ground Biomass",
      units = "tonnes/hectare",
      colours = viridis::viridis,
      format = c("GUESS")),
  
  new("Quantity",
      id = "cpool",
      name = "Carbon Pools",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "litter_wood",
      name = "Wood Litter",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "litter_leaf",
      name = "Leaf Litter",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "litter_repr",
      name = "Reproductive Litter",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "fine_fuel",
      name = "Fine Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mnpp",
      name = "NPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mgpp",
      name = "GPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mnee",
      name = "Monthly NEE",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mrh",
      name = "Monthly Heterotrophic Respiration",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mra",
      name = "Monthly Autotrophic Respiration",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "anpp",
      name = "Annual NPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "cflux",
      name = "Carbon Flux",
      units = "kgC/m^2/y",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "dens",
      name = "PFT Density",
      units = "indiv/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "speciesheights",
      name = "PFT Average Heights",
      units = "m",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "height",
      name = "PFT Average Heights",
      units = "m",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "canopyheight",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("GUESS"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "doc",
      name = "Dissolved Organic Carbon (?)",
      units = "kgC/m^2/year",
      colours = reversed.magma,
      format = c("GUESS"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "maet",
      name = "Monthly Actual Evapotranspiration",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mpet",
      name = "Monthly Potential Evapotranspiration",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mevap",
      name = "Monthly Evaporation",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mrunoff",
      name = "Monthly Runoff",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mintercep",
      name = "Monthly Interception",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mwcont_upper",
      name = "Monthly Upper Soil Layer",
      units = "fraction",
      colours = reversed.tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mwcont_lower",
      name = "Monthly Lower Soil Layer",
      units = "fraction",
      colours = reversed.tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "msnowpack",
      name = "Monthly Snow Pack",
      units = "mm H20",
      colours = reversed.tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "aaet",
      name = "Annual Actual Evapotranspiration",
      units = "mm/year",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "aiso",
      name = "Annual Isoprene Emissions",
      units = "kg/year",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  
  new("Quantity",
      id = "miso",
      name = "Monthly Isoprene Emissions",
      units = "kg/month",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "amon",
      name = "Annual Monoterpene Emissions",
      units = "kg/year",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mmon",
      name = "Monthly Monoterpene Emissions",
      units = "kg/year",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "firert",
      name = "Fire Return Interval",
      units = "years",
      colours = fire.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "monthly_burned_area",
      name = "Monthly Burned Area Fraction",
      units = "fraction",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "annual_burned_area",
      name = "annual Burned Area Fraction",
      units = "fraction",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "fireseason",
      name = "Fire Season Length",
      units = "days",
      colours = fire.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "firesl",
      name = "Fire Season Length",
      units = "days",
      colours = fire.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "burntarea", 
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = fire.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "burntfraction",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("GUESS"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "tot_runoff",
      name = "Runoff",
      units = "mm/year",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "vmaxnlim",
      name = "Nitrogen Limitation to Vmax",
      units = "fraction",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "bioclim",
      name = "Bioclimatic Limit Variables",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "gdd5",
      name = "Growing Degree Days (5deg C base)",
      units = "degree days",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  
  new("Quantity",
      id = "aleafshed",
      name = "Number times leafs shed per year",
      units = "",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "cton_leaf",
      name = "C:N leaf",
      units = "ckgC/kgN",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "nmass",
      name = "Vegetation Nitrogen Mass",
      units = "kgN/m^2",
      colours = viridis::viridis,
      format = c("GUESS")),
  
  new("Quantity",
      id = "ngases",
      name = "Annual Nitrogren Gases Emissions",
      units = "kg/ha/year",
      colours = viridis::viridis,
      format = c("GUESS")),
  
  new("Quantity",
      id = "npool",
      name = "Nitrogen",
      units = "kgN/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "nuptake",
      name = "Nitrogen Uptake",
      units = "kgN/ha",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "nsources",
      name = "Nitrogen Source",
      units = "gN/ha",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  
  new("Quantity",
      id = "nflux",
      name = "Nitrogen Flux",
      units = "kgN/ha",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "nlitter",
      name = "Litter Nitrogen Mass",
      units = "kgN/ha",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mprec",
      name = "Monthly Precipitation",
      units = "mm",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mtemp",
      name = "Mean Monthly Temperature",
      units = "deg C",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mwmass_stem",
      name = "Water Stored in Stem",
      units = "kgH20/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "mwmass_leaf",
      name = "Water Stored in Leaves",
      units = "kgH20/m^2",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  new("Quantity",
      id = "bioclim_mtemps",
      name = "Bioclimactic Temperatures",
      units = "deg C",
      colours = fields::tim.colors,
      format = c("GUESS")),
  
  
  #############################################################################################
  ############################# LPJ-GUESS-SPITFIRE QUANTITIES #################################
  #############################################################################################
  
  new("Quantity",
      id = "mfirefrac",
      name = "Monthly Burned Area Fraction",
      units = "",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mtau_l",
      name = "Monthly Residence Time",
      units = "mins",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "pyro_flux",
      name = "Pyrogenic Emmisions",
      units = "kg species/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfireintens",
      name = "Monthly Fire Intensity",
      units = "kW/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mlivegrass_fuel",
      name = "Mean Monthly Live Grass Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m1hr_fuel",
      name = "Mean Monthly 1hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m10hr_fuel",
      name = "Mean Monthly 10hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m100hr_fuel",
      name = "Mean Monthly 100hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m1000hr_fuel",
      name = "Mean Monthly 1000hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mlivegrass_cc",
      name = "Mean Monthly Live Grass Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m1hr_cc",
      name = "Mean Monthly 1hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m10hr_cc",
      name = "Mean Monthly 10hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id ="m100hr_cc",
      name = "Mean Monthly 100hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "m1000hr_cc",
      name = "Mean Monthly 1000hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "afdays",
      name = "Fire Days per Year",
      units = "days",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mavenest",
      name = "Average Monthly Nesterov",
      units = "",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mdlm_livegrass",
      name = "Monthly Litter Moisture of Live Grass",
      units = "",
      colours = reversed.tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mdlm_deadfuel",
      name = "Monthly Litter Moisture of Dead Fuel",
      units = "",
      colours = reversed.tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "meff_wind",
      name = "Monthly Effective Windspeed",
      units = "m/min",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mnfdi",
      name = "Monthly Nesterov Fire Danger Index",
      units = "m/min",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  
  new("Quantity",
      id = "mFBD",
      name = "Monthly Fuel Bulk Density",
      units = "m^3/m^3",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mSAV",
      name = "Monthly Surface Area to Volume Ratio",
      units = "m^2/m^3",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mMoE",
      name = "Monthly Moisture of Extinction",
      units = "",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mmcont",
      name = "Monthly Fuel Moisture Content",
      units = "",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfire_durat",
      name = "Monthly Fire Duration",
      units = "mins",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mscorch_height",
      name = "Monthly Scorch Height",
      units = "m",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mRoS",
      name = "Monthly Rate of Spread",
      units = "m/min",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfire_size",
      name = "Monthly Fire Size",
      units = "ha",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "real_fire_size",
      name = "Fire Size",
      units = "ha",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mhuman_ign",
      name = "Monthly average of human ignition rate",
      units = "ign/day",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mlightning_ign",
      name = "Monthly average of lightning ignition rate",
      units = "ign/day",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_duration",
      name = "Monthly Fire Duration (Fire Days Only)",
      units = "mins",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_scorch_height",
      name = "Monthly Scorch Height (Fire Days Only)",
      units = "m",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_intensity",
      name = "Monthly Fire Intensity (Fire Days Only)",
      units = "kW/m",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_nesterov",
      name = "Monthly Nesterov (Fire Days Only)",
      units = "",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_residence_time",
      name = "Monthly Residence Time (Fire Days Only)",
      units = "min",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "allocation_fails",
      name = "Monthly Allocation Fails",
      units = "days",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "allocation_iters",
      name = "Monthly Iteration To Allocation Fire",
      units = "days",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfuel",
      name = "Monthly Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfinefuel",
      name = "Monthly Fine Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mleaffuel",
      name = "Monthly Leaf Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfiredays",
      name = "Monthly sum of daily fire probabilites",
      units = "days",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE")),
  
  new("Quantity",
      id = "mfiredaysfine",
      name = "Monthly sum of daily fire probabilites (based on fine fuel threshold)",
      units = "days",
      colours = fields::tim.colors,
      format = c("LPJ-GUESS-SPITFIRE"))
)

################################################################
########### LPJ-GUESS(-SPITFIRE) FORMAT ########################
################################################################

#' @description \code{GUESS} - a Format for reading standard LPJ-GUESS(-SPITFIRE) model output
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @include colour-palettes.R
#' @export
#' 
GUESS <- new("Format",
             
             # UNIQUE ID
             id = "GUESS",
             
             # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
             availableQuantities = availableQuantities_GUESS,
             
             # FUNCTION TO READ A FIELD 
             getField = getField_GUESS,
             
             # DEFAULT GLOBAL LAYERS 
             predefined.layers = GUESS.Layers,
             
             # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
             quantities = GUESS.quantities
             
)

