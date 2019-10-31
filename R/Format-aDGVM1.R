#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE aDGVM1 FILES ###########################################################
############################################################################################################################

#' Get a Field for aDGVM1
#' 
#' An internal function that reads data from an aDGVM1 run.  It actually call one of three other functions depending on the type of quantity specified.   
#' 
#' @param source A \code{Source} containing the meta-data about the aDGVM1 run
#' @param quant A string the define what output file from the aDGVM1 run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param target.STAInfo The spatial-temporal target domain
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tabel containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_aDGVM1 <- function(source,
                            quant,
                            target.STAInfo,
                            file.name,
                            verbose,
                            ...) {
  
  # First check if quantity is for FireMIP, if so call a special function with the extra processing required
  if("FireMIP" %in% quant@format) {
    return(openaDGVM1OutputFile_FireMIP(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  else if("aDGVM1" %in% quant@format | "aDGVM1-SPITFIRE" %in% quant@format) {
    return(getYearlyField_aDGVM1(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  else if("Standard" %in% quant@format) {
    return(getStandardQuantity_aDGVM1(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  else{
    stop("Unrecognised Format of Quantity in 'quant' argument to getField_aDGVM1()")
  }
  
  
  
}


######################### OPEN AN aDGVM1 *.out FILE  #####################################################################
#' Open an aDGVM1 .out file
#'
#' \code{openaDGVM1OutputFile} returns a data.table object given a string defining a vegetation quantity 
#' from the run (eg. "lai", to read the file "lai.out") and  \code{Source} object which defines where the run is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param run A \code{Source} containing the meta-data about the aDGVM1 run
#' @param quant A Quant to define what output file from the aDGVM1 run to open, 
#' can also be a simple string defining the aDGVM1 output file if the \code{return.data.table} argument is TRUE
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
getYearlyField_aDGVM1 <- function(run,
                                  quant,
                                  target.sta,
                                  file.name = file.name,
                                  verbose = FALSE,
                                  data.table.only = FALSE,
                                  adgvm.fire = 1,
                                  adgvm.climate = 0,
                                  ...){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Annual = Year = Month = Day = NULL
  
  # extract from the target.sta
  first.year = target.sta@first.year
  last.year = target.sta@last.year
  
  # determine the years that are actually present
  if(is.null(file.name)) {
    all.files <- list.files(path = run@dir, paste("YearlyData", "*", adgvm.climate, adgvm.fire, "dat", sep="."))
  }
  else {
    all.files <- file.name
  }
  print(all.files)
  
  if(!data.table.only && class(quant)[1] != "Quantity") stop("Please supply a formal Quantity object as the quant argument since you are not requesting at data.table")
  
  if(class(quant)[1] == "Quantity") variable <- quant@id
  else variable <- quant
  
  #### !!! Check data.table package version (see data.table NEWS file for v1.11.6 point #5)
  compare.string <- utils::compareVersion(a = as.character(utils::packageVersion("data.table")), b = "1.11.6")
  new.data.table.version <- FALSE
  if(compare.string >= 0) new.data.table.version <- TRUE
  
  
  # loop for each year
  all.dts <- list()
  for(this.file in all.files){
    
    # Make the filename and check for the file, gunzip if necessary, fail if not present
    file.string <- file.path(run@dir, this.file)
    re.zip <- FALSE
    if(file.exists(file.string)){ 
      if(verbose) message(paste("Found and opening file", file.string, sep = " "))
      all.dts[[length(all.dts)+1]] <- fread(file.string)
    }
    else if(file.exists(paste(file.string, "gz", sep = "."))){
      if(verbose) message(paste("File", file.string, "not found, but gzipped file present so using that", sep = " "))
      if(.Platform$OS.type == "unix") {
        if(new.data.table.version) all.dts[[length(all.dts)+1]] <- fread(cmd = paste("gzip -d -c < ", paste(file.string, "gz", sep = "."), sep = ""))
        else all.dts[[length(all.dts)+1]] <- fread(paste("gzip -d -c < ", paste(file.string, "gz", sep = "."), sep = ""))
      }
      else {
        re.zip <- TRUE
        R.utils::gunzip(paste(file.string, "gz", sep = "."))
        all.dts[[length(all.dts)+1]] <- fread(file.string)
      }
    }
    else {
      stop(paste("File (or gzipped file) not found:", file.string))
    }
    
  }
  
  dt <- rbindlist(all.dts)
  setnames(dt, c("Lon", "Lat","Year","Clim","Fire","Seed","Rain","EvapoTot","EvapoGrass","EvapoSoil","C4G_LeafBiomass","C4G_RootBiomass","C3G_LeafBiomass","C3G_RootBiomass","DeadGrass_LeafBiomass","SavTr_Cancov","ForTr_Cancov","Tree_LeafBiomass","Tree_StemBiomass","Tree_RootBiomass","Grass_Ratio","Tree_MeanHeight","Tree_MaxHeight","Tree_Popsize","SavTr_Popsize","ForTr_Popsize","Tree_BasalArea","NPP","NEE","SoilCarbon","PhenologyCount","FireNumber","MeanFireIntensity","SoilN","SoilC","TmpMean","CO2ppm","dp1","dp2","dp3","dp4","dp5","dp6","dp7","dp8","dp9","dp10","dp11","dp12","dp13","dp14","dp15","dp16","dp17","dp18","dp19","dp20","dp21","dp22","dp23","dp24","dp25","dp26","dp27","dp28","dp29","Tree_ActiveDays","Grass_ActiveDays","ETref","A0C3","A0C4"))
  rm(all.dts)
  gc()
  
  
  if(variable == "Cancov") {
    
    dt <- dt[, c("Lon", "Lat", "Year", "ForTr_Cancov", "SavTr_Cancov")]
    setnames(dt, c("ForTr_Cancov", "SavTr_Cancov"), c("ForTr", "SavTr"))
    print(dt)
    
  }
  
  if(variable == "LeafBiomass") {
    
    dt <- dt[, c("Lon", "Lat", "Year", "C4G_LeafBiomass", "C3G_LeafBiomass","Tree_LeafBiomass")]
    setnames(dt, c("C4G_LeafBiomass", "C3G_LeafBiomass","Tree_LeafBiomass"), c("C4G", "C3G", "Tree"))
    print(dt)
    
  }
  
  if(variable == "RootBiomass") {
    
    dt <- dt[, c("Lon", "Lat", "Year", "C4G_RootBiomass", "C3G_RootBiomass","Tree_RootBiomass")]
    setnames(dt, c("C4G_RootBiomass", "C3G_RootBiomass","Tree_RootBiomass"), c("C4G", "C3G", "Tree"))
    print(dt)
    
  }
  
  if(variable == "StemBiomass") {
    
    dt <- dt[, c("Lon", "Lat", "Year","Tree_StemBiomass")]
    setnames(dt, c("Tree_StemBiomass"), c("Tree"))
    print(dt)
    
  }
  
  if(variable == "DeadGrassBiomass") {
    
    dt <- dt[, c("Lon", "Lat", "Year","DeadGrass_LeafBiomass")]
    setnames(dt, c("DeadGrass_LeafBiomass"), c("Grass"))
    print(dt)
    
  }
  
  if(variable == "PopSize") {
    
    dt <- dt[, c("Lon", "Lat", "Year", "ForTr_Popsize","SavTr_Popsize")]
    setnames(dt, c("ForTr_Popsize","SavTr_Popsize"), c("ForTr", "SavTr"))
    print(dt)
    
  }
  
  if(variable == "ET") {
    
    dt <- dt[, c("Lon", "Lat", "Year", "EvapoTot","EvapoGrass","EvapoSoil")]
    dt[, EvapoTot := EvapoTot / Year]
    dt[, EvapoGrass := EvapoGrass / Year]
    dt[, EvapoSoil := EvapoSoil / Year]
    dt[, EvapoTree := EvapoTot - (EvapoGrass + EvapoSoil)]
    setnames(dt, c("EvapoTot","EvapoGrass","EvapoSoil","EvapoTree"), c("Total","Grass","Soil","Tree"))
    print(dt)
    
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
  if(!is.null(target.sta@spatial.extent)) {
    
    spatial.extent.class <- class(target.sta@spatial.extent)[1]
    
    if(spatial.extent.class == "SpatialPolygonsDataFrame" || spatial.extent.class == "numeric" || is.data.frame(target.sta@spatial.extent) || is.data.table(target.sta@spatial.extent)) {
      dt <- selectGridcells(x = dt, gridcells = target.sta@spatial.extent, spatial.extent.id = target.sta@spatial.extent.id, ...)
      new.extent <- target.sta@spatial.extent
      # if new.extent is a data.frame, convery it to a data.table for consistency
      if(is.data.frame(new.extent) & !is.data.table(new.extent)) new.extent <- as.data.table(new.extent)
    }
    
    else {
      dt <- crop(x = dt, y = target.sta@spatial.extent, spatial.extent.id = target.sta@spatial.extent.id)
      new.extent <- extent(dt)
    } 
    
    
    
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
                 subannual.original = subannual)
  
  # if cropping has been done, set the new spatial.extent and spatial.extent.id
  if(!is.null(new.extent))  {
    sta.info@spatial.extent = new.extent
    sta.info@spatial.extent.id <- target.sta@spatial.extent.id
  }
  # otherwise set
  else {
    sta.info@spatial.extent = extent(dt)
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


######################### OPEN AN aDGVM1 *.out FILE  #####################################################################
#' Open an aDGVM1 .out file
#'
#' \code{openaDGVM1OutputFile} returns a data.table object given a string defining a vegetation quantity 
#' from the run (eg. "lai", to read the file "lai.out") and  \code{Source} object which defines where the run is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param run A \code{Source} containing the meta-data about the aDGVM1 run
#' @param quant A Quantity to define what output file from the aDGVM1 run to open.
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @import data.table
openaDGVM1OutputFile_FireMIP <- function(run,
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
    dt <- openaDGVM1OutputFile(run, "lai", data.table.only = TRUE, target.sta = target.sta,  file.name = file.name, verbose = verbose)
  }
  if(variable == "landCoverFrac") {
    dt <- openaDGVM1OutputFile(run, "fpc", data.table.only = TRUE, target.sta = target.sta,  file.name = file.name, verbose = verbose)
  }
  if(variable == "theightpft") {
    dt <- openaDGVM1OutputFile(run, "speciesheights", data.table.only = TRUE, target.sta = target.sta,  file.name = file.name, verbose = verbose)
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
    aDGVM1.var <- "mgpp"
    monthly.to.second <- TRUE
  }
  if(variable == "npp") {
    aDGVM1.var <- "mnpp"
    monthly.to.second <- TRUE
  }
  if(variable == "nbp") {
    aDGVM1.var <- "mnbp"
    monthly.to.second <- TRUE
  }
  if(variable == "ra") {
    aDGVM1.var <- "mra"
    monthly.to.second <- TRUE
  }
  if(variable == "rh") {
    aDGVM1.var <- "mrh"
    monthly.to.second <- TRUE
  }
  if(variable == "mrro") {
    aDGVM1.var <- "mrunoff"
    monthly.to.second <- TRUE
  }
  if(variable == "tran") {
    aDGVM1.var <- "maet"
    monthly.to.second <- TRUE
  }
  if(variable == "evspsblveg") {
    aDGVM1.var <- "mintercep"
    monthly.to.second <- TRUE
  }
  if(variable == "evspsblsoi") {
    aDGVM1.var <- "mevap"
    monthly.to.second <- TRUE
  }
  
  ## Here look for variables that require conversion to percent
  if(variable == "BA") {
    aDGVM1.var <- "mfirefrac"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuelLiveGrass") {
    aDGVM1.var <- "mlivegrass_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel1hr") {
    aDGVM1.var <- "m1hr_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel10hr") {
    aDGVM1.var <- "m10hr_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel100hr") {
    aDGVM1.var <- "m100hr_cc"
    monthly.to.percent <- TRUE
  }
  if(variable == "ccFuel1000hr") {
    aDGVM1.var <- "m1000hr_cc"
    monthly.to.percent <- TRUE
  }
  
  ## Here we have simple monthly variables (no conversion)
  if(variable == "cFuelLiveGrass") {
    aDGVM1.var <- "mlivegrass_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel1hr") {
    aDGVM1.var <- "m1hr_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel10hr") {
    aDGVM1.var <- "m10hr_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel100hr") {
    aDGVM1.var <- "m100hr_fuel"
    monthly <- TRUE
  }
  if(variable == "cFuel1000hr") {
    aDGVM1.var <- "m1000hr_fuel"
    monthly <- TRUE
  }
  if(variable == "mFuelDead") {
    aDGVM1.var <- "mdlm_deadfuel"
    monthly <- TRUE
  }
  if(variable == "mFuelLiveGrass") {
    aDGVM1.var <- "mdlm_livegrass"
    monthly <- TRUE
  } 
  if(variable == "nrfire") {
    aDGVM1.var <- "real_num_fires"
    monthly <- TRUE
  }
  if(variable == "cMortality") {
    aDGVM1.var <- "monthly_nind_killed"
    monthly <- TRUE
  }
  if(variable == "intensFire") {
    aDGVM1.var <- "real_intensity"
    monthly <- TRUE
  }
  if(variable == "durat") {
    aDGVM1.var <- "real_duration"
    monthly <- TRUE
  }
  if(variable == "RoS") {
    aDGVM1.var <- "mRoS"
    monthly <- TRUE
  }
  
  ## Finally we have a couple of monthly to second variables which also required molar conversion to C
  if(variable == "fFire") {
    aDGVM1.var <- "m_co2flux_fire"
    monthly.to.second <- TRUE
    CO2.to.C <- TRUE
  }
  if(variable == "coFire") {
    aDGVM1.var <- "m_coflux_fire"
    monthly.to.second <- TRUE
    CO.to.C <- TRUE
  }
  
  
  # Now calculate these bad boys
  if(monthly.to.second || monthly.to.percent || monthly){
    
    dt <- openaDGVM1OutputFile(run, aDGVM1.var, target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setnames(dt, aDGVM1.var, variable)
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
    aDGVM1.var <- "real_fire_size"
    dt <- openaDGVM1OutputFile(run, aDGVM1.var, target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setnames(dt, aDGVM1.var, variable)
    dt[, (variable) := get(variable) * 10000]
  }
  
  if(variable == "mrso") {
    
    
    # standard stuf for aDGVM1
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
    
    dt_upper <- openaDGVM1OutputFile(run, "mwcont_upper", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt_upper)
    
    dt_lower <- openaDGVM1OutputFile(run, "mwcont_lower", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
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
    
    
    # standard stuf for aDGVM1
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
    
    dt <- openaDGVM1OutputFile(run, "mwcont_upper", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
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
    dt_trans <- openaDGVM1OutputFile(run, "maet", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    dt_evap <- openaDGVM1OutputFile(run, "mevap", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
    setKeyDGVM(dt_trans)
    setKeyDGVM(dt_evap)
    dt_trans <- dt_evap[dt_trans]
    rm(dt_evap)
    gc()
    
    # now add interception
    dt_intercep <- openaDGVM1OutputFile(run, "mintercep", target.sta,  file.name = file.name, verbose = verbose, data.table.only = TRUE)
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
    dt <- openaDGVM1OutputFile(run, "cpool", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "VegC")
    dt <- dt[,target.cols,with=FALSE]
    setnames(dt, "VegC", "cVeg")
  }
  if(variable == "cLitter") {
    dt <- openaDGVM1OutputFile(run, "cpool", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "LittC")
    dt <- dt[,target.cols,with=FALSE]
    setnames(dt, "LittC", "cLitter")
  }
  if(variable == "cSoil") {
    dt <- openaDGVM1OutputFile(run, "cpool", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(unlist(getDimInfo(dt)), c("SoilfC", "SoilsC"))
    dt <- dt[,target.cols,with=FALSE]
    dt[, "cSoil" := SoilfC + SoilsC]
    dt[, SoilfC := NULL]
    dt[, SoilsC := NULL]
  }
  
  ### LAND USE FLUX AND STORE FROM luflux.out FILE
  
  if(variable == "cProduct") {
    dt <- openaDGVM1OutputFile(run, "luflux", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
    target.cols <- append(getDimInfo(dt), "Products_Pool")
    dt <- dt[,target.cols, with = FALSE]
    setnames(dt, "Products_Pool", "cProduct")
  }
  
  if(variable == "fLuc") {
    
    dt <- openaDGVM1OutputFile(run, "luflux", target.sta, file.name = file.name, verbose = verbose, data.table.only = TRUE)
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



#' Returns the data from one aDGVM1 output variable as a \code{data.table}.   
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
#' @param run A \code{Source} containing the meta-data about the aDGVM1 run from which the data is to be read.  Most importantly it must contain the run.dara nd the offsets.
#' @param quant A Quantity to define what output file from the aDGVM1 run to open
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param file.name Character string holding the name of the file.  This can be left blank, in which case the file name is just taken to be 
#' "<quant@id>.out" (also "<quant@id>.out.gz")
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @keywords internal
#' @export

getStandardQuantity_aDGVM1 <- function(run, 
                                    quant, 
                                    target.sta,
                                    file.name = file.name,
                                    verbose = FALSE) {
  
  Total = Year = FireRT = NULL
  
  # columns not to be modified for unit conversions etc.
  unmod.cols <- c("Lon", "Lat", "Year", "Month", "Day")
  
  # Check that this really is a standard Quantity and therefore should have behaviour defined here
  if(!"Standard" %in% quant@format) {
    stop((paste("getStandardQuantity_aDGVM1 called for a non-Standard Quantity (", quant@id, ")", sep = "")))
  }
  
  
  
  #### Here is the code to define each and every Standard Quantity for aDGVM1 output
  
  # vegcover_std
  if(quant@id == "vegcover_std") {
    
    # vegcover.out provides the right quantity here (note this is not standard aDGVM1)
    this.Field <- openaDGVM1OutputFile(run, lookupQuantity("vegcover", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    
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
    this.Field <- openaDGVM1OutputFile(run, lookupQuantity("cmass", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    
  }
  
  # LAI_std 
  else if(quant@id == "LAI_std") {
    
    # lai provides the right quantity here - so done
    this.Field <- openaDGVM1OutputFile(run, lookupQuantity("lai", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    
  }
  
  # FPAR_std 
  else if(quant@id == "FPAR_std") {
    
    # lai provides the right quantity here - so done
    this.Field <- openaDGVM1OutputFile(run, lookupQuantity("fpc", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    all.layers <- layers(this.Field)
    drop.layers <- all.layers [! all.layers %in% c("Total")]
    this.Field@data[, (drop.layers) := NULL]
    this.Field@data[, Total := pmin(Total, 1) * 100 * 0.83]
    
  }
  
  # aGPP_std 
  else if(quant@id == "aGPP_std") {
    
    # in older version of aDGVM1, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    if(file.exists(file.path(run@dir, "agpp.out")) || file.exists(file.path(run@dir, "agpp.out.gz"))){
      this.Field <- openaDGVM1OutputFile(run, lookupQuantity("agpp", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    }
    else {
      this.Field <- openaDGVM1OutputFile(run, lookupQuantity("mgpp", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field, method = "sum", target = "Year")
    }
    
  }
  
  
  # aNPP_std 
  else if(quant@id == "aNPP_std") {
    
    # in older version of aDGVM1, the mgpp file must be aggregated to annual
    # newer versions have the agpp output variable which has the per PFT version
    
    if(file.exists(file.path(run@dir, "anpp.out")) || file.exists(file.path(run@dir, "anpp.out.gz"))){
      this.Field <- openaDGVM1OutputFile(run, lookupQuantity("anpp", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    }
    else{
      this.Field <-  openaDGVM1OutputFile(run, lookupQuantity("mnpp", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field , method = "sum", target = "Year")
    }
    
  }
  
  # mNPP_std 
  else if(quant@id == "aNEE_std") {
    
    this.Field <- openaDGVM1OutputFile(run, lookupQuantity("cflux", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    
    # take NEE and  ditch the rest
    all.layers <- layers(this.Field)
    drop.layers <- all.layers [! all.layers %in% c("NEE")]
    this.Field@data[, (drop.layers) := NULL]
    
  }
  
  # canopyheight_std 
  else if(quant@id == "canopyheight_std") {
    
    # The canopyheight output fromth e benchmarkoutput output module is designed to be exactly this quantity
    this.Field <- openaDGVM1OutputFile(run, lookupQuantity("canopyheight", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
    renameLayers(this.Field, "CanHght", "CanopyHeight")
    
  }
  
  # burntfraction_std 
  else if(quant@id == "burntfraction_std") {
    
    # if mfirefrac is present the open it and use it
    if("mfirefrac" %in% availableQuantities_aDGVM1(run, names=TRUE)){
      this.Field <- openaDGVM1OutputFile(run, lookupQuantity("mfirefrac", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
      this.Field <- aggregateSubannual(this.Field, method = "sum")
      renameLayers(this.Field, "mfirefrac", quant@id)
      
    }
    
    # otherwise open firert to get GlobFIRM fire return interval and invert it
    else {
      this.Field <- openaDGVM1OutputFile(run, lookupQuantity("firert", aDGVM1), target.sta, file.name = file.name, verbose = verbose)
      this.Field@data[, "burntfraction_std" :=  1 / FireRT]
      this.Field@data[, FireRT :=  NULL]
    }
    
  }
  
  # else stop
  else {
    
    stop(paste("Unfortunately standard quantity", quant@id, "does not seem to be available in aDGVM1"))
    
  }
  
  # Update the Field with the 'standard' Quantity and return
  this.Field@quant <- quant
  return(this.Field)
  
}



######################### LIST ALL aDGVM1 OUTPUT VARIABLES (STORED AS *.out FILES) IN AN RUN DIRECTORY  #####################################################################
#' List all aDGVM1 *.out files in a run directory
#'
#' Simply lists all aDGVM1 output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "aDGVM1.out" and "*.out" 
#' 
#' @param source A aDGVM1 source object
#' @param names Logical, if TRUE return the namse of the quantities, if FLASE return the quanties themseleves
#' @return A list of all the .out files present, with the ".out" removed. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

availableQuantities_aDGVM1 <- function(source, names = TRUE, verbose = FALSE){
  
  directory <- source@dir
  
  # First get the list of *.out files present
  files.present <- list.files(directory, ".out$")
  files.present <- append(files.present, list.files(directory, ".out.gz$"))
  
  # Now strip the .out file extension out the get the variable name
  this.var.list <- unlist(lapply(files.present, FUN = aDGVM1QuantFromFilename))
  
  
  # check that they correspond to actual quantities, if not through a warning and chuck them out
  good.list <- list()
  ignore.list <- c("*", "aDGVM1_out", "aDGVM1_err")
  
  for(this.file in files.present){
    
    variable<- aDGVM1QuantFromFilename(this.file)
    
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
        if(verbose) warning("Although I have found file with an appropriate extension that looks like an aDGVM1 output variable (", this.file, "), I have no Quantity object corrsponding to \"", variable, "\".  I am therefore ignoring it.  \n However, not to worry! If you want this file included, you can easily add a new Quantity to the dgvm.quantities list (just in your analysis script, doesn't need to be in the package).")
      }
      
    }
    
  }
  
  return(unlist(good.list))
  
}



#' Detemine PFTs present in an aDGVM1 run
#' 
#' @param x  A Source objects describing an aDGVM1(-SPITFIRE) run
#' @param variables Some variable to loom for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_aDGVM1 <- function(x, variables) {
  
  # first get a list of all avaiable variables
  available.vars <- suppressWarnings(availableQuantities_aDGVM1(x))
  
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
  
  warning(paste("Hmmm, not been able to identify the PFTs in aDGVM1(-SPITFIRE) run", x@name, "because I can't find an appropriate per-PFT file in the run directory. Returning the super-set list.", sep = " ") )
  return(x@pft.set)
  
}


######################### TRIM AN aDGVM1 FILENAME  #####################################################################
#' Helper function to raster::trim the ".out" or the ".out.gz" from an aDGVM1 filename to get the variable in question
#' 
#' Returns NULL if the last characters are not ".out" or ".out.gz
#'
#' @param var.filename The string of the filename to be raster::trimmed
#' @return A string less the last fou.
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


# handy helper function for raster::trimming file names to get a variable name
aDGVM1QuantFromFilename <- function(var.filename){
  
  for(ending in c(".out", ".out.gz")) {
    if(substr(var.filename, nchar(var.filename) - nchar(ending) + 1,  nchar(var.filename)) == ending) return(substr(var.filename, 1, nchar(var.filename) - nchar(ending)))
  }
  
  return(NULL)
  
}


#####################################################################
########### aDGVM1(-SPITFIRE) GLOBAL PFTS ########################
#####################################################################


#' @format An S4 class object with the slots as defined below.
#' @rdname PFT-class
#' @keywords datasets
aDGVM1.PFTs <- list(
  
  
  # Forest Tree
  new("PFT",
      id = "ForTr",
      name = "Forest Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Adaptive",
      climate.zone = "Tropical",
      colour = "darkgreen",
      shade.tolerance = "None"
  ),
  
  # Savanna Tree
  new("PFT",
      id = "SavTr",
      name = "Savanna Tree",
      growth.form = "Tree",
      leaf.form = "Broadleaved",
      phenology = "Adaptive",
      climate.zone = "Boreal",
      colour = "lightgreen",
      shade.tolerance = "None"
  ),
  
  # GRASSES
  
  # C3G 
  new("PFT",
      id = "C3G",
      name = "C3 Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "Adaptive",
      climate.zone = "Tropical",
      colour = "lightgoldenrod1",
      shade.tolerance = "None"
  ),
  
  # C4G
  new("PFT",
      id = "C4G",
      name = "C4 Grass",
      growth.form = "Grass",
      leaf.form = "Broadleaved",
      phenology = "Adaptive",
      climate.zone = "Tropical",
      colour = "sienna2",
      shade.tolerance = "None"
  )
  
)

#####################################################################
########### aDGVM1(-SPITFIRE) QUANTITIES ########################
#####################################################################


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' @include colour-palettes.R
#' 
#' 
aDGVM1.quantities <- list(
  
  
  new("Quantity",
      id = "Cancov",
      name = "Canopy Cover",
      units = "%",
      colours = reversed.viridis,
      format = c("aDGVM1"),
      cf.name = "land_area_fraction"),
  
  new("Quantity",
      id = "LeafBiomass",
      name = "Leaf biomass",
      units = "tonnes/hectare",
      colours = reversed.viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "RootBiomass",
      name = "Root biomass",
      units = "tonnes/hectare",
      colours = reversed.viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "StemBiomass",
      name = "Stem biomass",
      units = "tonnes/hectare",
      colours = reversed.viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "DeadGrassBiomass",
      name = "Dead grass biomass",
      units = "tonnes/hectare",
      colours = reversed.viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "PopSize",
      name = "Tree population size",
      units = "number of individuals",
      colours = reversed.viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "ET",
      name = "Evapotranspiration",
      units = "mm/year",
      colours = reversed.viridis,
      format = c("aDGVM1"),
      cf.name = "water_evapotranspiration_flux"),
  
  new("Quantity",
      id = "mfpc",
      name = "Monthly Foliar Projective Cover",
      units = "m^2/m^2",
      colours = veg.palette,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "vegcover",
      name = "Vegetation Cover",
      units = "%",
      colours = veg.palette,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "agpp",
      name = "Annual GPP",
      units = "kgC/m2/year",
      colours = viridis::inferno,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "cmass",
      name = "Vegetation Carbon Mass",
      units = "kgC/m^2",
      colours = viridis::viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "clitter",
      name = "Litter Carbon Mass",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "agb",
      name = "Above Ground Biomass",
      units = "tonnes/hectare",
      colours = viridis::viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "cpool",
      name = "Carbon Pools",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "litter_wood",
      name = "Wood Litter",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "litter_leaf",
      name = "Leaf Litter",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "litter_repr",
      name = "Reproductive Litter",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "fine_fuel",
      name = "Fine Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mnpp",
      name = "NPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mgpp",
      name = "GPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mnee",
      name = "Monthly NEE",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mrh",
      name = "Monthly Heterotrophic Respiration",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mra",
      name = "Monthly Autotrophic Respiration",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "anpp",
      name = "Annual NPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "cflux",
      name = "Carbon Flux",
      units = "kgC/m^2/y",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "dens",
      name = "PFT Density",
      units = "indiv/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "speciesheights",
      name = "PFT Average Heights",
      units = "m",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "height",
      name = "PFT Average Heights",
      units = "m",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "canopyheight",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("aDGVM1"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "doc",
      name = "Dissolved Organic Carbon (?)",
      units = "kgC/m^2/year",
      colours = reversed.magma,
      format = c("aDGVM1"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "maet",
      name = "Monthly Actual Evapotranspiration",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mpet",
      name = "Monthly Potential Evapotranspiration",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mevap",
      name = "Monthly Evaporation",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mrunoff",
      name = "Monthly Runoff",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mintercep",
      name = "Monthly Interception",
      units = "mm/month",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mwcont_upper",
      name = "Monthly Upper Soil Layer",
      units = "fraction",
      colours = reversed.tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mwcont_lower",
      name = "Monthly Lower Soil Layer",
      units = "fraction",
      colours = reversed.tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "msnowpack",
      name = "Monthly Snow Pack",
      units = "mm H20",
      colours = reversed.tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "aaet",
      name = "Annual Actual Evapotranspiration",
      units = "mm/year",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "aiso",
      name = "Annual Isoprene Emissions",
      units = "kg/year",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  
  new("Quantity",
      id = "miso",
      name = "Monthly Isoprene Emissions",
      units = "kg/month",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "amon",
      name = "Annual Monoterpene Emissions",
      units = "kg/year",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mmon",
      name = "Monthly Monoterpene Emissions",
      units = "kg/year",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "firert",
      name = "Fire Return Interval",
      units = "years",
      colours = fire.palette,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "fireseason",
      name = "Fire Season Length",
      units = "days",
      colours = fire.palette,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "firesl",
      name = "Fire Season Length",
      units = "days",
      colours = fire.palette,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "burntarea", 
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = fire.palette,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "burntfraction",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("aDGVM1"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "tot_runoff",
      name = "Runoff",
      units = "mm/year",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "vmaxnlim",
      name = "Nitrogen Limitation to Vmax",
      units = "fraction",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "bioclim",
      name = "Bioclimatic Limit Variables",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "gdd5",
      name = "Growing Degree Days (5deg C base)",
      units = "degree days",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  
  new("Quantity",
      id = "aleafshed",
      name = "Number times leafs shed per year",
      units = "",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "cton_leaf",
      name = "C:N leaf",
      units = "ckgC/kgN",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "nmass",
      name = "Vegetation Nitrogen Mass",
      units = "kgN/m^2",
      colours = viridis::viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "ngases",
      name = "Annual Nitrogren Gases Emissions",
      units = "kg/ha/year",
      colours = viridis::viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "npool",
      name = "Nitrogen",
      units = "kgN/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "nuptake",
      name = "Nitrogen Uptake",
      units = "kgN/ha",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "nsources",
      name = "Nitrogen Source",
      units = "gN/ha",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  
  new("Quantity",
      id = "nflux",
      name = "Nitrogen Flux",
      units = "kgN/ha",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "nlitter",
      name = "Litter Nitrogen Mass",
      units = "kgN/ha",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mprec",
      name = "Monthly Precipitation",
      units = "mm",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mtemp",
      name = "Mean Monthly Temperature",
      units = "deg C",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mwmass_stem",
      name = "Water Stored in Stem",
      units = "kgH20/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "mwmass_leaf",
      name = "Water Stored in Leaves",
      units = "kgH20/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "bioclim_mtemps",
      name = "Bioclimactic Temperatures",
      units = "deg C",
      colours = fields::tim.colors,
      format = c("aDGVM1")),
  
  
  #############################################################################################
  ############################# aDGVM1-SPITFIRE QUANTITIES #################################
  #############################################################################################
  
  new("Quantity",
      id = "mfirefrac",
      name = "Monthly Burned Area Fraction",
      units = "",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mtau_l",
      name = "Monthly Residence Time",
      units = "mins",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "pyro_flux",
      name = "Pyrogenic Emmisions",
      units = "kg species/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfireintens",
      name = "Monthly Fire Intensity",
      units = "kW/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mlivegrass_fuel",
      name = "Mean Monthly Live Grass Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m1hr_fuel",
      name = "Mean Monthly 1hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m10hr_fuel",
      name = "Mean Monthly 10hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m100hr_fuel",
      name = "Mean Monthly 100hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m1000hr_fuel",
      name = "Mean Monthly 1000hr Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mlivegrass_cc",
      name = "Mean Monthly Live Grass Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m1hr_cc",
      name = "Mean Monthly 1hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m10hr_cc",
      name = "Mean Monthly 10hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id ="m100hr_cc",
      name = "Mean Monthly 100hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "m1000hr_cc",
      name = "Mean Monthly 1000hr Combustion Completeness",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "afdays",
      name = "Fire Days per Year",
      units = "days",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mavenest",
      name = "Average Monthly Nesterov",
      units = "",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mdlm_livegrass",
      name = "Monthly Litter Moisture of Live Grass",
      units = "",
      colours = reversed.tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mdlm_deadfuel",
      name = "Monthly Litter Moisture of Dead Fuel",
      units = "",
      colours = reversed.tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "meff_wind",
      name = "Monthly Effective Windspeed",
      units = "m/min",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mnfdi",
      name = "Monthly Nesterov Fire Danger Index",
      units = "m/min",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  
  new("Quantity",
      id = "mFBD",
      name = "Monthly Fuel Bulk Density",
      units = "m^3/m^3",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mSAV",
      name = "Monthly Surface Area to Volume Ratio",
      units = "m^2/m^3",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mMoE",
      name = "Monthly Moisture of Extinction",
      units = "",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mmcont",
      name = "Monthly Fuel Moisture Content",
      units = "",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfire_durat",
      name = "Monthly Fire Duration",
      units = "mins",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mscorch_height",
      name = "Monthly Scorch Height",
      units = "m",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mRoS",
      name = "Monthly Rate of Spread",
      units = "m/min",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfire_size",
      name = "Monthly Fire Size",
      units = "ha",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mhuman_ign",
      name = "Monthly average of human ignition rate",
      units = "ign/day",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mlightning_ign",
      name = "Monthly average of lightning ignition rate",
      units = "ign/day",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_duration",
      name = "Monthly Fire Duration (Fire Days Only)",
      units = "mins",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_scorch_height",
      name = "Monthly Scorch Height (Fire Days Only)",
      units = "m",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_intensity",
      name = "Monthly Fire Intensity (Fire Days Only)",
      units = "kW/m",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_nesterov",
      name = "Monthly Nesterov (Fire Days Only)",
      units = "",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfireday_residence_time",
      name = "Monthly Residence Time (Fire Days Only)",
      units = "min",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "allocation_fails",
      name = "Monthly Allocation Fails",
      units = "days",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "allocation_iters",
      name = "Monthly Iteration To Allocation Fire",
      units = "days",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfuel",
      name = "Monthly Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfinefuel",
      name = "Monthly Fine Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mleaffuel",
      name = "Monthly Leaf Fuel",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfiredays",
      name = "Monthly sum of daily fire probabilites",
      units = "days",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE")),
  
  new("Quantity",
      id = "mfiredaysfine",
      name = "Monthly sum of daily fire probabilites (based on fine fuel threshold)",
      units = "days",
      colours = fields::tim.colors,
      format = c("aDGVM1-SPITFIRE"))
)

################################################################
########### aDGVM1(-SPITFIRE) FORMAT ########################
################################################################

#' @description \code{aDGVM1} - a Format for reading standard aDGVM1(-SPITFIRE) model output
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @include colour-palettes.R
#' @export
#' 
aDGVM1 <- new("Format",
              
              # UNIQUE ID
              id = "aDGVM1",
              
              # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
              determinePFTs = determinePFTs_aDGVM1,
              
              # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
              availableQuantities = availableQuantities_aDGVM1,
              
              # FUNCTION TO READ A FIELD 
              getField = getField_aDGVM1,
              
              # DEFAULT GLOBAL PFTS  
              default.pfts = aDGVM1.PFTs,
              
              # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM aDGVM1 RUNS  
              quantities = aDGVM1.quantities
              
)

