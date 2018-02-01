#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE DGVMData FILES ###########################################################
############################################################################################################################


######################### OPEN A DGVMData *.nc FILE  #####################################################################
#' Open a DGVMData *.n file
#'
#' \code{penDGVMDataFile} returns a data.table object given a string defining a vegetation quantity 
#' from the source (eg. "lai", to read the file "lai.out") and  \code{Source} object which defines where the source is on disk and the offsets to apply
#'
#' Note that the files can be gzipped on UNIX systems, but this might fail on windows systems.
#' 
#' @param source A \code{Source} containing the meta-data about the LPJ-GUESS source
#' @param variable A string the define what output file from the LPJ-GUESS source to open, for example "anpp" opens and read the "anpp.out" file 
#' @param verbose A logical, set to true to give progress/debug information
#' @return a data.table (with the correct tear offset and lon-lat offsets applied)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import data.table
#' @export
openDGVMDataFile <- function(source,
                             variable,
                             verbose = FALSE){
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Time = NULL
  
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.name.nc <- file.path(source@dir, paste(source@id, variable@id, "nc", sep = "."))
  if(file.exists(file.name.nc)){ 
    if(verbose) message(paste("Found and opening file", file.name.nc, sep = " "))
    
  }
  else if(file.exists(paste(file.name.nc, "gz", sep = "."))){
    #dt <- fread(paste("zcat < ", paste(file.name.nc, "gz", sep = "."), sep = ""))
    stop("Gunzipping not yet supported for DGVMData.")
  }
  else {
    stop(paste("File (or gzipped file) not found:", file.name.nc))
  }
  
  
  # Open file and get Lon and Lat dimensions, and get values for later 
  message(paste0("Opening file ", file.name.nc))     
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE ))
  this.lat <- ncdf4::ncvar_get(this.nc,"lat",verbose=verbose)
  this.lon <- ncdf4::ncvar_get(this.nc,"lon",verbose=verbose)
  all.lats <- this.nc$dim$lat$vals
  all.lons <- this.nc$dim$lon$vals
  
  # check the number of dimensions and pull time if present
  
  if(this.nc$ndims == 2){
    if(verbose) message("Got two dimensions, assuming lon and lat")  
    start <- c(1,1)
    count <- c(-1,-1)
    dimension.names <- list(this.lon, this.lat)
  }
  if(this.nc$ndims == 3) {
    this.time <- ncdf4::ncvar_get(this.nc,"Time",verbose=verbose)
    start <- c(1,1,1)
    count <- c(-1,-1,-1)
    dimension.names <- list(this.lon, this.lat, this.time)
  }
  
  
  # look up attributes for meta-data for this layer/variable
  first.year <- ncatt_get(this.nc, 0, attname="DGVMData_first.year", verbose=FALSE)$value
  last.year <- ncatt_get(this.nc, 0, attname="DGVMData_last.year", verbose=FALSE)$value
  year.aggregation.method <- ncatt_get(this.nc, 0, attname="DGVMData_year.aggregation.method", verbose=FALSE)$value
  
  
  
  all.vars <- this.nc$var
  
  dt.list <- list()
  
  for(this.var in all.vars) {
    
    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = start, count = count, verbose = verbose)
    dimnames(this.slice) <- dimension.names
    
    # melt to a data.table, via data.frame
    this.slice.dt <- as.data.table(melt(this.slice))
    
    # remove NAs
    this.slice.dt <- stats::na.omit(this.slice.dt)
    
    # 
    # # quantity
    # quant.str <- ncatt_get(this.nc, this.var, attname="DGVMTools_quant", verbose=FALSE)$value
    # 
    # # set the name to something equivalent in the model
    # layer.name <- ncatt_get(this.nc, this.var, attname="DGVMTools_layer.name", verbose=FALSE)$value
    
    #################################
    ######### Here do checks ########
    #################################
    
    
    # also set the names, key and if categorical, set to factors in the variabl@units slot
    if(this.nc$ndims == 2){
      setnames(this.slice.dt, c("Lon", "Lat", this.var$name))
    }
    if(this.nc$ndims == 3){
      setnames(this.slice.dt, c("Lon", "Lat", "Time", this.var$name))
      
      # some tricks to determine exactly what the 'Time' axis represent 
      time.axis.string <- this.nc$dim$Time$units
      
      
      # actually, lets do some shortcuts
      length.time.axis <- this.nc$dim$Time$len
      
      # lets see if we have annual data
      if(length.time.axis == (last.year - first.year + 1)) {
        if(verbose) message("Got annual data.")
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
        
        # make new colum order so that the variable is last
        all.names <- names(this.slice.dt)
        all.names <- all.names[-which(all.names == this.var$name)]
        all.names <- append(all.names, this.var$name)
        setcolorder(this.slice.dt, all.names)
        print(this.slice.dt)

        
      }
      
      
    }
    
    setKeyDGVM(this.slice.dt)
    
    
    if(variable@type == "categorical") {
      this.slice.dt[,this.var$name := factor(this.slice.dt[[this.var$name]], labels = variable@units)]
    }
    
    # now join this to all.dt
    dt.list[[length(dt.list)+1]] <- this.slice.dt
    
  }
  
  
  # join all together
  dt <- dt.list[[1]]
  if(length(dt.list) > 1) {
    for(this.dt in dt.list[[2:length(dt.list)]]) {
      print(this.dt)
      merge(dt, this.dt)
      print(dt)
    }
  }
  dt <- setKeyDGVM(dt)
  
  
  # STInfo
  st.info <- getSTInfo(dt)
  
  
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
  if(source@london.centre  && max(all.lons) >= 180){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  
  
  # set some attributes about the file - works!
  attr(dt, "shadeToleranceCombined") <- FALSE
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs
  dt <- stats::na.omit(dt)
  
  return(
    
    list(dt = dt,
         first.year = first.year,
         last.year = last.year,
         year.aggregation.method = year.aggregation.method
    )
    
  )
  
}





######################### LIST ALL LPJ-GUESS OUTPUT VARIABLES (STORED AS *.out FILES) IN AN source DIRECTORY  #####################################################################
#' List all LPJ-GUESS *.out files in a source directory
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "guess.out" and "*.out" 
#' 
#' @param source A path to a directory on the file system containing some .out files
#' @return A list of all the .out files present, with the ".out" removed. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


listAvailableQuantities_DGVMData <- function(source){
  
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


