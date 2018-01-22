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
  Lon = Lat = Year = Month = NULL
  
  
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
  
  
  message(paste0("Opening file ", file.name.nc))     
  
  
  this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  this.lat <- ncdf4::ncvar_get(this.nc,"lat",verbose=verbose)
  this.lon <- ncdf4::ncvar_get(this.nc,"lon",verbose=verbose)
  
  # look up attributes for meta-data for this layer/variable
  first.year <- ncatt_get(this.nc, 0, attname="DGVMData_first.year", verbose=FALSE)$value
  last.year <- ncatt_get(this.nc, 0, attname="DGVMData_last.year", verbose=FALSE)$value
  year.aggregation.method <- ncatt_get(this.nc, 0, attname="DGVMData_year.aggregation.method", verbose=FALSE)$value
  
  
  
  all.vars <- this.nc$var
  
  dt.list <- list()
  
  for(this.var in all.vars) {

    # Get the actual data and set the dimension names    
    this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = c(1,1), count = c(-1,-1), verbose = verbose)
    dimnames(this.slice) <- list(this.lon, this.lat)
    
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
    
    
    # also set the names and key 
    setnames(this.slice.dt, c("Lon", "Lat", this.var$name))
    setKeyDGVM(this.slice.dt)
    
    # now join this to all.dt
    dt.list[[length(dt.list)+1]] <- this.slice.dt
    
  }
  
  
  # join all together
  dt <- dt.list[[1]]
  if(length(dt.list) > 1) {
    for(this.dt in dt.list[[2:length(dt.list)]]) {
      print(this.dt)
      data.table::merge.data.table(dt, this.dt)
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
  
  # if london.centre is requested, make sure all negative longitudes are shifted to positive
  if(source@london.centre){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  
  
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


