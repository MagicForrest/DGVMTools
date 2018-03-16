#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE LPJ-GUESS FILES ###########################################################
############################################################################################################################


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
#' @export
openLPJOutputFile <- function(run,
                              variable,
                              first.year,
                              last.year,
                              verbose = FALSE){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = NULL
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.string = file.path(run@dir, paste(variable, ".out", sep=""))
  if(file.exists(file.string)){ 
    if(verbose) message(paste("Found and opening file", file.string, sep = " "))
    dt <- fread(file.string)
  }
  else if(file.exists(paste(file.string, "gz", sep = "."))){
    if(verbose) message(paste("File", file.string, "not found, but gzipped file present so using that", sep = " "))
    dt <- fread(paste("zcat < ", paste(file.string, "gz", sep = "."), sep = ""))
    
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
  
  if(verbose) {
    message("Offsets applied. Head of full .out file (after offsets):")
    print(utils::head(dt))
  }
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(run@london.centre){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  
  # If data is has monthly or daily columns, melt to long/tidy data where "Month" becomes a column
  
  # first get of all the columns which are not spatial-temporal info
  all.cols <- names(dt)
  st.cols <- getSTInfo(dt)
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
  
  
  # set some attributes about the file - works!
  attr(dt, "shadeToleranceCombined") <- FALSE
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  return(dt)
  
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
#' @export
openLPJOutputFile_FireMIP <- function(run,
                                      variable,
                                      first.year,
                                      last.year,
                                      verbose = FALSE){
  
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
  if(variable == "fFirePFT") {
    guess.var <- "cflux_fire"
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
  if(variable == "intensFire") {
    guess.var <- "real_intensity"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuelLiveGrass") {
    guess.var <- "mlivegrass_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel1hr") {
    guess.var <- "m1hr_c"
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

  # Here we have simple monthly variables (no conversion)
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
  
    # Now calculate these bad boys
  if(monthly.to.second || monthly.to.percent || monthly){
    
    dt <- openLPJOutputFile(run, guess.var, first.year, last.year,  verbose)
    setnames(dt, guess.var, "Total")
    if(monthly.to.second){
      dt[, Seconds := seconds.in.month[Month]]
      dt[, Total := Total/Seconds]
      dt[, Seconds := NULL]
    }
    if(monthly.to.percent){
      dt[, Total := Total * 100]
    }
    return(dt)
    
  }
  
  ### Special monthly variables
  if(variable == "meanFire") {
    dt <- openLPJOutputFile(run, "real_fire_size", first.year, last.year,  verbose)
    setnames(dt, guess.var, "Total")
    dt[, Total := Total * 10000]
    return(dt)
  }
  
  if(variable == "mrso") {
    dt_upper <- openLPJOutputFile(run, "mwcont_upper", first.year, last.year,  verbose)
    setKeyDGVM(dt_upper)
    dt_lower <- openLPJOutputFile(run, "mwcont_lower", first.year, last.year,  verbose)
    setKeyDGVM(dt_lower)
    dt <- dt_upper[dt_lower]
    dt[, Total := mwcont_lower + mwcont_upper]
    dt[, mwcont_lower := NULL]
    dt[, mwcont_upper := NULL]
    rm(dt_upper,dt_lower)
    gc()
    return(dt)
  }
  
  if(variable == "evapotrans") {
    
    # firsty combine transpiration and evaporation
    dt_trans <- openLPJOutputFile(run, "maet", first.year, last.year,  verbose)
    dt_evap <- openLPJOutputFile(run, "mevap", first.year, last.year,  verbose)
    setKeyDGVM(dt_trans)
    setKeyDGVM(dt_evap)
    dt_evap <- dt_evap[dt_trans]
    rm(dt_trans)
    gc()
    
    # now add interception
    dt_intercep <- openLPJOutputFile(run, "mintercep", first.year, last.year,  verbose)
    setKeyDGVM(dt_intercep)
    dt_trans <- dt_trans[dt_intercep]
    rm(dt_intercep)
    
    
    # combine, convert and clean up
    dt_trans[,Total := maet + mevap + mintercep]
    dt_trans[,maet := NULL]
    dt_trans[,mevap := NULL]
    dt_trans[,mintercep := NULL]
    dt_trans[, Seconds := seconds.in.month[Month]]
    dt_trans[, Total := Total/Seconds]
    dt_trans[, Seconds := NULL]
    return(dt_trans)
  }
  
  
}



#' Returns the data from one LPJ-GUESS output variable as a \code{data.table}.   
#'
#' 
#' This fucntion can retrieve a 'Standard' vegetation quantity (returned as a data.table) with standard definition and units
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
  
  Total = Annual = FireRT = NULL
  
  # columns not to be modified for unit conversions etc.
  unmod.cols <- c("Lon", "Lat", "Year", "Day")
  
  # Check that this really is a standard Quantity and therefore should have behaviour defined here
  if(!"Standard" %in% quant@model) {
    stop()(paste("getStandardQuantity_LPJ called for a non-standard Quantity (", quant@id, ")", sep = ""))
  }
  
  
  
  #### Here is the code to define each and every Standard Quantity for LPJ-GUESS output
  
  # vegcover_std
  if(quant@id == "vegcover_std") {
    
    # vegcover.out provides the right quantity here (note this is not standard LPJ-GUESS)
    this.dt <- openLPJOutputFile(run, "vegcover", first.year, last.year, verbose = TRUE)
    
    # But we need to scale it to %
    if(verbose) message("Multiplying fractional areal vegetation cover by 100 to get percentage areal cover")
    mod.cols <- names(this.dt)
    mod.cols <- mod.cols[!mod.cols %in% unmod.cols]
    this.dt <- this.dt[, (mod.cols) := lapply(.SD, function(x) x * 100 ), .SDcols = mod.cols]
    if("Grass" %in% names(this.dt)) { setnames(this.dt, old = "Grass", new = "NonTree") }
    return(this.dt)
    
  }
  
  # vegC_std 
  else if(quant@id == "vegC_std") {
    
    # cmass provides the right quantity here - so done
    this.dt <- openLPJOutputFile(run, "cmass", first.year, last.year, verbose = TRUE)
    
    return(this.dt)
    
  }
  
  # LAI_std 
  else if(quant@id == "LAI_std") {
    
    # lai provides the right quantity here - so done
    this.dt <- openLPJOutputFile(run, "lai", first.year, last.year, verbose = TRUE)
    
    return(this.dt)
    
  }
  
  # LAI_std 
  else if(quant@id == "FPAR_std") {
    
    # lai provides the right quantity here - so done
    temp.dt <- openLPJOutputFile(run, "fpc", first.year, last.year, verbose = TRUE)
    this.dt <- temp.dt[, c("Lon", "Lat", "Year", "Total")]
    this.dt[, Total := pmin(Total, 1) * 100 * 0.83]
    return(this.dt)
    
  }
  
  # mGPP_std 
  else if(quant@id == "aGPP_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    if(file.exists(file.path(run@dir, "agpp.out")) || file.exists(file.path(run@dir, "agpp.out.gz"))){
      this.dt <- openLPJOutputFile(run, "agpp", first.year, last.year, verbose = TRUE)
      this.dt <- this.dt[, c("Lon", "Lat", "Year","Total"), with = FALSE]
    }
    else {
      this.dt <- openLPJOutputFile(run, "mgpp", first.year, last.year, verbose = TRUE)
      this.dt <- autoLayer(this.dt, "Annual") 
      this.dt <- this.dt[, c("Lon", "Lat", "Year","Annual"), with = FALSE]
      this.dt <- this.dt[, Total := Annual]
      this.dt <- this.dt[, Annual := NULL]
    }
    return(this.dt)
    
  }
  
  
  # mNPP_std 
  else if(quant@id == "aNPP_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    
    if(file.exists(file.path(run@dir, "anpp.out") || file.path(run@dir, "anpp.out.gz"))){
      this.dt <- openLPJOutputFile(run, "anpp", first.year, last.year, verbose = TRUE)
      this.dt <- this.dt[, c("Lon", "Lat", "Year","Total"), with = FALSE]
    }
    else{
      this.dt <- openLPJOutputFile(run, "mnpp", first.year, last.year, verbose = TRUE)
      this.dt <- autoLayer(this.dt, "Annual")
    }
    
    
    return(this.dt)
    
  }
  
  # mNPP_std 
  else if(quant@id == "aNEE_std") {
    
    # in older version of LPJ-GUESS, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    this.dt <- openLPJOutputFile(run, "cflux", first.year, last.year, verbose = TRUE)
    
    # make the annual total and remove ditch the rest
    this.dt <- this.dt[, c("Lon", "Lat", "Year","NEE"), with = FALSE]
    
    
    print(this.dt)
    
    return(this.dt)
    
  }
  
  # canopyheight_std 
  else if(quant@id == "canopyheight_std") {
    
    # The canopyheight output fromth e benchmarkoutput output module is designed to be exactly this quantity
    this.dt <- openLPJOutputFile(run, "canopyheight", first.year, last.year, verbose = TRUE)
    setnames(this.dt, "CanHght", "CanopyHeight")
    
    return(this.dt)
    
  }
  
  # burntfraction_std 
  else if(quant@id == "burntfraction_std") {
    
    # if mfirefrac is present the open it and use it
    if("mfirefrac" %in% listAllLPJOutput(run@dir)){
      this.dt <- openLPJOutputFile(run, "mfirefrac", first.year, last.year, verbose = TRUE)
      this.dt <- aggregateSubannual(this.dt, method = "sum")
      
    }
    
    # otherwise open firert to get GlobFIRM fire return interval and invert it
    else {
      this.dt <- openLPJOutputFile(run, "firert", first.year, last.year, verbose = TRUE)
      this.dt[, Annual :=  1 / FireRT]
      this.dt[, FireRT :=  NULL]
    }
    
    return(this.dt)
    
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
#' @param directory A path to a directory on the file system containing some .out files
#' @return A list of all the .out files present, with the ".out" removed. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

listAllLPJOutput <- function(directory){
  
  # First get the list of *.out files present
  files.present <- list.files(directory, "*.out")
  files.present <- append(files.present, list.files(directory, "*.out.gz"))
  
  
  # Now strip the .out file extension out the get the variable name
  this.var.list <- unlist(lapply(files.present, FUN = LPJQuantFromFilename))
  
  # get rid of stupid ones
  ignore.list <- c("*", "guess_out", "guess_err")
  for(ignore.string in ignore.list){
    if(ignore.string %in% this.var.list) {
      this.var.list <- this.var.list[-which(this.var.list == ignore.string)]
    }
  }
  
  return(this.var.list)
  
}



#' Detemine PFTs present in an LPJ-GUESS run
#' 
#' @param x  A Source objects describing an LPJ-GUESS(-SPITFIRE) run
#' @param variables Some variable to loom for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

listPFTs_LPJ <- function(x, variables) {
  
  # first get a list of all avaiable variables
  available.vars <- listAllLPJOutput(x@dir)
  
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



