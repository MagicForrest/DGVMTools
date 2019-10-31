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
  
  # cheeky hard-coded variable lists
  yearly.aDGVM1.quantities <- list("Cancov", "LeafBiomass", "RootBiomass", "StemBiomass", "DeadGrassBiomass", "PopSize")
  daily.aDGVM1.quantities <- list("GPP", "SoilC", "FireNum", "Sizes")
  
  
  # aDGVM1 yearly quantities
  if("aDGVM1" %in% quant@format && quant@id %in% yearly.aDGVM1.quantities) {
    return(getYearlyField_aDGVM1(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  # aDGVM1 daily quantities
  else if("aDGVM1" %in% quant@format && quant@id %in% daily.aDGVM1.quantities) {
    return(getDailyField_aDGVM1(source, quant, target.sta = target.STAInfo, file.name = file.name, verbose = verbose, ...))
  }
  # Standard quantities 
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
#' \code{getYearlyField_aDGVM1} returns a data.table object given a string defining a vegetation quantity 
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
    all.dts[[length(all.dts)+1]] <- readRegularASCII(file.string, verbose)
    
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
  
  
  
  # set some attributes about the data - works!
  # currently screws up unit tests and isn't used.  Consider using if updating metadata system.
  # setattr(dt, "shadeToleranceCombined", FALSE)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  # set the keys (very important!)
  setKeyDGVM(dt)
  
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
#' \code{getYearlyField_aDGVM1} returns a data.table object given a string defining a vegetation quantity 
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
getDailyField_aDGVM1 <- function(run,
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
  
  # determine which file to open bases on the Quantity that we want
  Sys.quantities <- c("GPP")
  Fire.quantities <- c("FireNum") # Glenn, this is just an example
  Soil.quantities <- c("SoilC") # Glenn, this is just an example
  Size.quantities <- c("Sizes") # Glenn, this is just an example
  
  if(quant@id %in% Sys.quantities) file.substring <- "Sys"
  else if(quant@id %in% Fire.quantities) file.substring <- "Fire"
  else if(quant@id %in% Soil.quantities) file.substring <- "Soil"
  else if(quant@id %in% Size.quantities) file.substring <- "Size"
  else stop(paste0("Quantity ", quant@id, " not defined as being in file SysData_, FireData_, SizeData_ or SoilData_ in function getDailyField_aDGVM1()"))
  
  # in the case of Soil Data read one SysData file to get the Days and Years
  if(file.substring == "Soil") {
    all.files <- list.files(path = run@dir)
    all.files <- all.files[grepl(paste0("SysData_.*_", adgvm.fire, ".*", adgvm.climate, ".dat"), all.files)]
    file.string <- file.path(run@dir, all.files[1])
    temp.dt <- readRegularASCII(file.string, verbose)
    YearDay.dt <- temp.dt[,c(1,2)]
    setnames(YearDay.dt, c("Year", "Day"))
    rm(temp.dt)
  }
  
  
  # get the list of files to be read
  if(is.null(file.name)) {
    all.files <- list.files(path = run@dir)
    all.files <- all.files[grepl(paste0(file.substring , "Data_.*_", adgvm.fire, ".*", adgvm.climate, ".dat"), all.files)]
  }
  else {
    all.files <- file.name
  }
  
  print(all.files)
  
  if(!data.table.only && class(quant)[1] != "Quantity") stop("Please supply a formal Quantity object as the quant argument since you are not requesting at data.table")
  
  if(class(quant)[1] == "Quantity") variable <- quant@id
  else variable <- quant
  
  # loop for each year
  all.dts <- list()
  for(this.file in all.files){
    
    # Make the filename and call the function to read the file, and handle the results
    file.string <- file.path(run@dir, this.file)
    dt <- readRegularASCII(file.string, verbose)
    
    # if file not empty (to catch empty fire files)
    if(nrow(dt) != 0) {
      
      # also extract Lon and Lat and add to the file
      str.components <- unlist(strsplit(this.file, split = "_"))
      dt[, Lon := as.numeric(str.components[2])]
      dt[, Lat := as.numeric(str.components[3])]
      if(file.substring == "Soil") {
        dt[, c("Year", "Day") := YearDay.dt]
      }
      # Size are output annually, so hardcode Years 
      if(file.substring == "Size") {
        dt[, "Year" := 1:nrow(dt)]
      }
      
      all.dts[[length(all.dts)+1]] <- dt
      
    }
    
    rm(dt)
    
  }
  
 
  dt <- rbindlist(all.dts)
  rm(all.dts)
  gc()
  print(dt)
  
  # set names depending on file type
  if(file.substring == "Sys") {
    Sys.names <- c("Year","Day","Grass_LeafBiomassLive","Grass_RootBiomassLive","Grass_LeafBiomassDeadStanding","Grass_LeafBiomassDeadLying","Grass_RootBiomassDead","Grass_GPP","Grass_RMA","Grass_RGR","SavTree_Cancov","ForTree_Cancov","Tree_LeafBiomassLive","Tree_StemBiomassLive","Tree_RootBiomassLive","Tree_LeafBiomassDeadStanding","Tree_LeafBiomassDeadLying","Tree_StemBiomassDeadStanding","Tree_StemBiomassDeadLying","Tree_RootBiomassDead","Tree_GPP","Tree_RMA","Tree_RGR","Tree_LAI","Tree_Popsize","Tree_MeanHeight","Grass_Ratio","Tmp_Mean","Tree_TallNum","Tree_BA","CO2ppm","Rain_day","EvapoTot","SoilCarbonRelease","Combustion","MeanRain")
    original.length <- length(names(dt)) - 2 # subtract 2 because we have added Lon and Lat
    setnames(dt, names(dt)[1:original.length], Sys.names[1:original.length])
  }
  else if(file.substring == "Soil") {
    Soil.names <- c("Soil_FineWoody","Soil_CoarseWoody","Soil_Extractives","Soil_Cellulose","Soil_Lignin","Soil_Humus1","Soil_Humus2","Soil_NonWoodyLitterInput","Soil_FineWoodyLitterInput","Soil_CoarseWoodyLitterInput","Soil_CO2Extractives","Soil_CO2Cellulose","Soil_CO2Lignin","Soil_CO2Humus1","Soil_CO2Humus2")
    setnames(dt, names(dt)[1:length(Soil.names)], Soil.names)
  }
  else if(file.substring == "Fire") {
    Fire.names <- c("FireNum","Year","Day","DeadFuel","LiveFuel","DeadFuelMoisture","LiveFuelMoisture","TotalFuel","MeanFuelMoisture","FireIntensity","Patchiness","Scorch","CombustFine","CombustCoarse","CombustHeavy","CombustHelper","Grass_LeafLiveCombustion","Grass_LeafDeadStandingCombustion","Grass_LeafDeadLyingCombustion","Tree_LeafLiveCombustion","Tree_LeafDeadLyingCombustion","Tree_LeafDeadLyingCombustion","Tree_StemLiveCombustion","Tree_StemDeadStandingCoarseCombustion","Tree_StemDeadLyingCoarseCombustion","Tree_StemDeadStandingHeavyCombustion","Tree_StemDeadLyingHeavyCombustion","Tree_StemDeadStandingFineCombustion","Tree_StemDeadLyingFineCombustion","Grass_LeafN20","Tree_LeafN20","Tree_StemCoarseN20","Tree_StemHeavyN20","Tree_StemFineN20","Grass_LeafCH4","Tree_LeafCH4","Tree_StemCoarseCH4","Tree_StemHeavyCH4","Tree_StemFineCH4","CO2ppm")
    original.length <- length(names(dt)) - 2 # subtract 2 because we have added Lon and Lat 
    setnames(dt, names(dt)[1:original.length], Fire.names[1:original.length])
  }
  else if(file.substring == "Size") {
    Size.names <- c("50","100","150","200","250","300","350","400","450","500","550","600","650","700","750","800","850","900","950","1000","1050","1100","1150","1200","1250","1300","1350","1400","1450","1500","1550","1600","1650","1700","1750","1800","1850","1900","1950","2000","2050","2100","2150","2200","2250","2300","2350","2400","2450","2500","2550","2600","2650","2700","2750","2800","2850","2900","2950","3000","3050","3100","3150","3200","3250","3300","3350","3400","3450","3500")
    original.length <- length(names(dt)) - 3 # subtract 3 because we have added Lon, Lat and Year
    setnames(dt, names(dt)[1:original.length], Size.names[1:original.length])
  }
  
  
  
  print(dt) 
  
  
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
  
  
  
  # set some attributes about the data - works!
  # currently screws up unit tests and isn't used.  Consider using if updating metadata system.
  # setattr(dt, "shadeToleranceCombined", FALSE)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  # set the keys (very important!)
  setKeyDGVM(dt)
  
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
  
  # directory <- source@dir
  # 
  # # First get the list of *.out files present
  # files.present <- list.files(directory, ".out$")
  # files.present <- append(files.present, list.files(directory, ".out.gz$"))
  # 
  # # Now strip the .out file extension out the get the variable name
  # this.var.list <- unlist(lapply(files.present, FUN = aDGVM1QuantFromFilename))
  # 
  # 
  # # check that they correspond to actual quantities, if not through a warning and chuck them out
  # good.list <- list()
  # ignore.list <- c("*", "aDGVM1_out", "aDGVM1_err")
  # 
  # for(this.file in files.present){
  #   
  #   variable <- aDGVM1QuantFromFilename(this.file)
  #   
  #   if(!variable %in% ignore.list) {
  #     
  #     result = tryCatch({
  #       dummy.quant <- suppressWarnings(lookupQuantity(variable, source@format))
  #     },  warning = function(w) {
  #       #warning(w)
  #     }, error = function(e) {
  #       #warning(e)
  #     }, finally = {
  #     })
  #     
  #     if(is.Quantity(result))  {
  #       if(names) good.list <- append(good.list, variable)
  #       else good.list <- append(good.list, result)
  #     }
  #     else {
  #       if(verbose) warning("Although I have found file with an appropriate extension that looks like an aDGVM1 output variable (", this.file, "), I have no Quantity object corrsponding to \"", variable, "\".  I am therefore ignoring it.  \n However, not to worry! If you want this file included, you can easily add a new Quantity to the dgvm.quantities list (just in your analysis script, doesn't need to be in the package).")
  #     }
  #     
  #   }
  #   
  # }
  # 
  # return(unlist(good.list))
  
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
  
  
  #### DUMMY - or maybe useful...
  new("Quantity",
      id = "SoilC",
      name = "Soil Carbon",
      units = "kg whatever",
      colours = reversed.viridis,
      format = c("aDGVM1")),
  
  new("Quantity",
      id = "FireNum",
      name = "Number of fires",
      units = "m^2",
      colours = reversed.viridis,
      format = c("aDGVM1"))
  
)

##################################################
########### aDGVM1 FORMAT ########################
##################################################

#' @description \code{aDGVM1} - a Format for reading standard aDGVM1 model output
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

