#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE LPJ-GUESS FILES ###########################################################
############################################################################################################################

#' Get a Field for LPJ-GUESS
#' 
#' An internal function that reads data from an LPJ-GUESS run.  It actually call one of three other functions depending on the type of quantity specified.   
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tabel containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_GUESS <- function(source,
                           quant,
                           target.STAInfo,
                           verbose,
                           ...) {
  
  
  # First check if quantity is for FireMIP, if so call a special function with the extra processing required
  if("FireMIP" %in% quant@format) {
    data.list <- openLPJOutputFile_FireMIP(source, quant@id, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, verbose = verbose, ...)
  }
  else if("GUESS" %in% quant@format | "LPJ-GUESS-SPITFIRE" %in% quant@format) {
    data.list <- openLPJOutputFile(source, quant@id, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, verbose = verbose)
  }
  else if("Standard" %in% quant@format) {
    data.list <- getStandardQuantity_LPJ(source, quant, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, verbose = verbose)
  }
  
  
  return(data.list)
  
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
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @keywords internal
openLPJOutputFile <- function(run,
                              variable,
                              first.year,
                              last.year,
                              verbose = FALSE,
                              data.table.only = FALSE){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Annual = Year = Month = Day = NULL
  
  #### !!! Check data.table package version (see data.table NEWS file for v1.11.6 point #5)
  compare.string <- utils::compareVersion(a = as.character(utils::packageVersion("data.table")), b = "1.11.6")
  new.data.table.version <- FALSE
  if(compare.string >= 0) new.data.table.version <- TRUE
  
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.string = file.path(run@dir, paste(variable, ".out", sep=""))
  re.zip <- FALSE
  if(file.exists(file.string)){ 
    if(verbose) message(paste("Found and opening file", file.string, sep = " "))
    dt <- fread(file.string)
  }
  else if(file.exists(paste(file.string, "gz", sep = "."))){
    if(verbose) message(paste("File", file.string, "not found, but gzipped file present so using that", sep = " "))
    if(.Platform$OS.type == "unix") {
      if(new.data.table.version) dt <- fread(cmd = paste("gzip -d -c < ", paste(file.string, "gz", sep = "."), sep = ""))
      else dt <- fread(paste("gzip -d -c < ", paste(file.string, "gz", sep = "."), sep = ""))
    }
    else {
      re.zip <- TRUE
      R.utils::gunzip(paste(file.string, "gz", sep = "."))
      dt <- fread(file.string)
    }
  }
  else {
    stop(paste("File (or gzipped file) not found:", file.string))
  }
  
  
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
  
  # also correct days to be 1-365 instead of 0-364, if necessary
  if("Day" %in% names(dt)) {
    if(0 %in% unique(dt[["Day"]])) dt[, Day := Day+1]
  }
  
  if(verbose) {
    message("Offsets applied. Head of full .out file (after offsets):")
    print(utils::head(dt))
  }
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(run@london.centre){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  
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
    dt <- dt[, Month := as.numeric(Month)]
    
  }
  
  # if daily then melt
  # TODO - implement daily melting, follow above for implementation
  
  
  # set some attributes about the data - works!
  setattr(dt, "shadeToleranceCombined", FALSE)
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  # if re-zip
  if(re.zip) R.utils::gzip(file.string)
  
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
  
  if(data.table.only) return(dt)
  else return(list(dt = dt,
                   sta.info = sta.info))
  
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
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @import data.table
openLPJOutputFile_FireMIP <- function(run,
                                      variable,
                                      first.year,
                                      last.year,
                                      verbose = FALSE,
                                      soil_water_capacities = "none"){
  
  Lon = Lat = Seconds = Month = Total = mwcont_lower = mwcont_upper = maet= mevap = mintercep = mrso = mrsos = Capacity = Code = NULL
  target.cols = SoilfC = SoilsC = NULL
  
  
  # seconds in month
  seconds.in.month <- c()
  for(month in all.months) {
    seconds.in.month <- append(seconds.in.month, month@days * 24 * 60 * 60)
  }
  
  
  ################################################################################################
  ############# PER PFT VARIABLES
  
  if(variable == "lai") {
    return(openLPJOutputFile(run, "lai", first.year, last.year,  verbose))
  }
  if(variable == "landCoverFrac") {
    return(openLPJOutputFile(run, "fpc", first.year, last.year,  verbose))
  }
  if(variable == "theightpft") {
    return(openLPJOutputFile(run, "speciesheights", first.year, last.year,  verbose))
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
    guess.var <- "mfirefrac"
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
    
    dt <- openLPJOutputFile(run, guess.var, first.year, last.year,  verbose, data.table.only = TRUE)
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
    dt <- openLPJOutputFile(run, guess.var, first.year, last.year,  verbose, data.table.only = TRUE)
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
    
    dt_upper <- openLPJOutputFile(run, "mwcont_upper", first.year, last.year,  verbose, data.table.only = TRUE)
    setKeyDGVM(dt_upper)
    print(dt_upper)
    
    dt_lower <- openLPJOutputFile(run, "mwcont_lower", first.year, last.year,  verbose, data.table.only = TRUE)
    setKeyDGVM(dt_lower)
    dt <- dt_upper[dt_lower]
    print(dt_lower)
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
    
    dt <- openLPJOutputFile(run, "mwcont_upper", first.year, last.year,  verbose, data.table.only = TRUE)
    setKeyDGVM(dt)
    
    print(dt)
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
    dt_trans <- openLPJOutputFile(run, "maet", first.year, last.year,  verbose, data.table.only = TRUE)
    dt_evap <- openLPJOutputFile(run, "mevap", first.year, last.year,  verbose, data.table.only = TRUE)
    setKeyDGVM(dt_trans)
    setKeyDGVM(dt_evap)
    dt_trans <- dt_evap[dt_trans]
    rm(dt_evap)
    gc()
    
    # now add interception
    dt_intercep <- openLPJOutputFile(run, "mintercep", first.year, last.year,  verbose, data.table.only = TRUE)
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
    dt <- openLPJOutputFile(run, "cpool", first.year, last.year,  verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "VegC")
    dt <- dt[,target.cols,with=FALSE]
    setnames(dt, "VegC", "cVeg")
  }
  if(variable == "cLitter") {
    dt <- openLPJOutputFile(run, "cpool", first.year, last.year,  verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "LittC")
    dt <- dt[,target.cols,with=FALSE]
    setnames(dt, "LittC", "cLitter")
  }
  if(variable == "cSoil") {
    dt <- openLPJOutputFile(run, "cpool", first.year, last.year,  verbose, data.table.only = TRUE)
    target.cols <- append(unlist(getDimInfo(dt)), c("SoilfC", "SoilsC"))
    dt <- dt[,target.cols,with=FALSE]
    dt[, "cSoil" := SoilfC + SoilsC]
    dt[, SoilfC := NULL]
    dt[, SoilsC := NULL]
  }
  
  ### LAND USE FLUX AND STORE FROM luflux.out FILE
  
  if(variable == "cProduct") {
    dt <- openLPJOutputFile(run, "luflux", first.year, last.year,  verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "Products_Pool")
    dt <- dt[,target.cols, with = FALSE]
    setnames(dt, "Products_Pool", "cProduct")
  }
  
  if(variable == "fLuc") {
    
    dt <- openLPJOutputFile(run, "luflux", first.year, last.year,  verbose, data.table.only = TRUE)
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
  
  return(list(dt = dt,
              sta.info = sta.info))
  
  
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
#' @param quant A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @keywords internal
#' @export

getStandardQuantity_LPJ <- function(run, 
                                    quant, 
                                    first.year,
                                    last.year,
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
    data.list <- openLPJOutputFile(run, "vegcover", first.year, last.year, verbose = TRUE)
    
    # But we need to scale it to %
    if(verbose) message("Multiplying fractional areal vegetation cover by 100 to get percentage areal cover")
    mod.cols <- names(data.list[["dt"]])
    mod.cols <- mod.cols[!mod.cols %in% unmod.cols]
    data.list[["dt"]][, (mod.cols) := lapply(.SD, function(x) x * 100 ), .SDcols = mod.cols]
    if("Grass" %in% names(data.list[["dt"]])) { setnames(data.list[["dt"]], old = "Grass", new = "NonTree") }
    return(data.list)
    
  }
  
  # vegC_std 
  else if(quant@id == "vegC_std") {
    
    # cmass provides the right quantity here - so done
    data.list <- openLPJOutputFile(run, "cmass", first.year, last.year, verbose = TRUE)
    return(data.list)
    
  }
  
  # LAI_std 
  else if(quant@id == "LAI_std") {
    
    # lai provides the right quantity here - so done
    data.list <- openLPJOutputFile(run, "lai", first.year, last.year, verbose = TRUE)
    return(data.list)
    
    
  }
  
  # FPAR_std 
  else if(quant@id == "FPAR_std") {
    
    # lai provides the right quantity here - so done
    data.list <- openLPJOutputFile(run, "fpc", first.year, last.year, verbose = TRUE)
    
    data.list[["dt"]] <- data.list[["dt"]][, c("Lon", "Lat", "Year", "Total")]
    data.list[["dt"]][, Total := pmin(Total, 1) * 100 * 0.83]
    return(data.list)
    
  }
  
  # mGPP_std 
  else if(quant@id == "aGPP_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    if(file.exists(file.path(run@dir, "agpp.out")) || file.exists(file.path(run@dir, "agpp.out.gz"))){
      data.list <-  openLPJOutputFile(run, "agpp", first.year, last.year, verbose = TRUE)
      data.list[["dt"]] <- data.list[["dt"]][, c("Lon", "Lat", "Year","Total"), with = FALSE]
    }
    else {
      data.list <- openLPJOutputFile(run, "mgpp", first.year, last.year, verbose = TRUE)
      data.list[["dt"]] <- aggregateSubannual(data.list[["dt"]], method = "sum", target = "Year")
    }
    return(data.list)
    
  }
  
  
  # mNPP_std 
  else if(quant@id == "aNPP_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    
    if(file.exists(file.path(run@dir, "anpp.out") || file.path(run@dir, "anpp.out.gz"))){
      data.list <- openLPJOutputFile(run, "anpp", first.year, last.year, verbose = TRUE)
      data.list[["dt"]] <-  data.list[["dt"]][, c("Lon", "Lat", "Year","Total"), with = FALSE]
    }
    else{
      data.list <- openLPJOutputFile(run, "mnpp", first.year, last.year, verbose = TRUE)
      data.list[["dt"]]  <- aggregateSubannual(data.list[["dt"]] , method = "sum", target = "Year")
    }
    return(data.list)
    
  }
  
  # mNPP_std 
  else if(quant@id == "aNEE_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    data.list <- openLPJOutputFile(run, "cflux", first.year, last.year, verbose = TRUE)
    
    # take NEE and  ditch the rest
    data.list[["dt"]] <- data.list[["dt"]][, c("Lon", "Lat", "Year","NEE"), with = FALSE]
    
    return(data.list)
    
  }
  
  # canopyheight_std 
  else if(quant@id == "canopyheight_std") {
    
    # The canopyheight output fromth e benchmarkoutput output module is designed to be exactly this quantity
    data.list <- openLPJOutputFile(run, "canopyheight", first.year, last.year, verbose = TRUE)
    setnames(data.list[["dt"]], "CanHght", "CanopyHeight")
    
    return(data.list)
    
  }
  
  # burntfraction_std 
  else if(quant@id == "burntfraction_std") {
    
    # if mfirefrac is present the open it and use it
    if("mfirefrac" %in% availableQuantities_GUESS(run, names=TRUE)){
      data.list <- openLPJOutputFile(run, "mfirefrac", first.year, last.year, verbose = TRUE)
      data.list[["dt"]] <- aggregateSubannual(data.list[["dt"]], method = "sum")
      
    }
    
    # otherwise open firert to get GlobFIRM fire return interval and invert it
    else {
      data.list <- openLPJOutputFile(run, "firert", first.year, last.year, verbose = TRUE)
      data.list[["dt"]][, "burntfraction_std" :=  1 / FireRT]
      data.list[["dt"]][, FireRT :=  NULL]
    }
    
    return(data.list)
    
  }
  
  # else stop
  else {
    
    stop(paste("Unfortunately standard quantity", quant@id, "does not seem to be available in LPJ-GUESS"))
    
  }
  
  
  
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

availableQuantities_GUESS <- function(source, names = TRUE){
  
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
        warning("Although I have found file with an appropriate extension that looks like an LPJ-GUESS output variable (", this.file, "), I have no Quantity object corrsponding to \"", variable, "\".  I am therefore ignoring it.  \n However, not to worry! If you want this file included, you can easily add a new Quantity to the dgvm.quantities list (just in your analysis script, doesn't need to be in the package).")
      }
      
    }
    
  }
  
  return(unlist(good.list))
  
}



#' Detemine PFTs present in an LPJ-GUESS run
#' 
#' @param x  A Source objects describing an LPJ-GUESS(-SPITFIRE) run
#' @param variables Some variable to loom for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_GUESS <- function(x, variables) {
  
  # first get a list of all avaiable variables
  available.vars <- suppressWarnings(availableQuantities_GUESS(x))
  
  # check for the presence the following variables (in order)
  possible.vars <- c("lai", "cmass", "dens", "fpc")
  
  for(this.var in possible.vars) {
    
    if(this.var %in% available.vars) {
      
      file.string = file.path(x@dir, paste(this.var, ".out", sep=""))
      
      if(file.exists(file.string)){ 
        header <- utils::read.table(file.string, header = TRUE, nrow = 1)
      }
      else if(file.exists(paste(file.string, "gz", sep = "."))){
        header <- utils::read.table(gzfile(paste(file.string, "gz", sep = ".")), header = TRUE, nrow = 1)
      }
      
      PFTs.present <- list()
      for(colname in names(header)){
        for(PFT in x@pft.set){
          if(PFT@id == colname) {
            PFTs.present <- append(PFTs.present, PFT)
          }
        }
      }
      
      return(PFTs.present)
      
    }
    
  }
  
  warning(paste("Hmmm, not been able to identify the PFTs in LPJ-GUESS(-SPITFIRE) run", x@name, "because I can't find an appropriate per-PFT file in the run directory. Returning the super-set list.", sep = " ") )
  return(x@pft.set)
  
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
########### LPJ-GUESS(-SPITFIRE) GLOBAL PFTS ########################
#####################################################################


#' @format An S4 class object with the slots as defined below.
#' @rdname PFT-class
#' @keywords datasets
GUESS.PFTs <- list(
  
  # BOREAL TREES
  
  # BNE
  new("PFT",
      id = "BNE",
      name = "Boreal Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Boreal",
      colour = "darkblue",
      shade.tolerance = "None"
  ),
  
  # BINE
  new("PFT",
      id = "BINE",
      name = "Boreal Shade-Intolerant Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Boreal",
      colour = "dodgerblue3",
      shade.tolerance = "BNE"
  ),
  
  # BNS
  BNS = new("PFT",
            id = "BNS",
            name = "Boreal Needleleaved Summergreen Tree",
            growth.form = "Tree",
            leaf.form = "Needleleaved",
            phenology = "Summergreen",
            climate.zone = "Boreal",
            colour = "cadetblue2",
            shade.tolerance = "None"
  ),
  
  # IBS
  new("PFT",
      id = "IBS",
      name = "Shade-intolerant B/leaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      climate.zone = "Temperate",
      colour = "chartreuse",
      shade.tolerance = "None"
  ),
  
  # TEMPERATE TREES
  
  # TeBE
  new("PFT",
      id = "TeBE",
      name = "Temperate Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Temperate",
      colour = "darkgreen",
      shade.tolerance = "None"
  ),
  
  # TeNE
  new("PFT",
      id = "TeNE",
      name = "Temperate Needleleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Needleleaved",
      phenology = "Evergreen",
      climate.zone = "Temperate",
      colour = "lightseagreen",
      shade.tolerance = "None"
  ),
  
  # TeBS
  new("PFT",
      id = "TeBS",
      name = "Temperate Broadleaved Summergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Summergreen",
      colour = "darkolivegreen3",
      climate.zone = "Temperate",
      shade.tolerance = "None"
  ),
  
  
  # TROPICAL TREES
  
  # TrBE
  new("PFT",
      id = "TrBE",
      name = "Tropical Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Tropical",
      colour = "orchid4",
      shade.tolerance = "None"
  ),
  
  
  # TrIBE
  new("PFT",
      id = "TrIBE",
      name = "Tropical Shade-intolerant Broadleaved Evergreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Evergreen",
      climate.zone = "Tropical", 
      colour = "orchid",
      shade.tolerance = "TrBE"
  ),
  
  # TrBR 
  new("PFT",
      id = "TrBR",
      name = "Tropical Broadleaved Raingreen Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Raingreen",
      climate.zone = "Tropical",
      colour = "palevioletred",
      shade.tolerance = "None"
  ),
  
  
  # GRASSES
  
  # C3G 
  new("PFT",
      id = "C3G",
      name = "Boreal/Temperate Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "lightgoldenrod1",
      shade.tolerance = "None"
  ),
  
  # C4G
  new("PFT",
      id = "C4G",
      name = "Tropical Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "GrassPhenology",
      climate.zone = "NA",
      colour = "sienna2",
      shade.tolerance = "None"
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
      units = "m^2/m^2",
      colours = veg.palette,
      format = c("GUESS")),
  
  new("Quantity",
      id = "agpp",
      name = "Annual GPP",
      units = "kgC/m2/year",
      colours = fields::tim.colors,
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
      name = "Carbon",
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
             
             # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
             determinePFTs = determinePFTs_GUESS,
             
             # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
             availableQuantities = availableQuantities_GUESS,
             
             # FUNCTION TO READ A FIELD 
             getField = getField_GUESS,
             
             # DEFAULT GLOBAL PFTS  
             default.pfts = GUESS.PFTs,
             
             # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
             quantities = GUESS.quantities
             
)

