############################################################################################################################
############################ FUNCTIONS TO HANDLE FireMIP FILES ###########################################################
############################################################################################################################


#' Get a Field for FireMIP
#' 
#' An internal function that reads data from an FireMIP run.  
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be returned
#' @param last.year The last year (as a numeric) of the data to be returned
#' @param verbose A logical, set to true to give progress/debug information
#' 
#' @return A list containing firstly the data.tabel containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_FireMIP <- function(source,
                              quant,
                              target.STAInfo,
                              verbose) {
  
  
  
  this.dt <- openFireMIPOutputFile(source, quant@id, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, verbose = verbose)
  
  
  return(list(this.dt, NULL))
  
}


#' Open a FireMIP output file
#' 
#' Opens a .nc file from the FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.  
#' Returns a data.table, because it is intended to be called by getField(), but of course the data.table could be used directly if you wish
#' 
#' 
#' @param run A Source object to define the run we want to open.
#' @param quantity A Quantity object to define which variable we want to look up
#' @param first.year The first year we want to read (numeric)
#' @param last.year The last year we want to read (numeric)
#' @param spatial.extent The spatial extent we want to read (as defined by as raster::extent or an object that can be cast to a raster::extent)
#' @param verbose Logical, if TRUE spew forth a lot of info.
#' 
#' @keywords internal
#' 
#'  
#' @return A data.table
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 

openFireMIPOutputFile <- function(run, quantity, first.year = NULL, last.year = NULL, spatial.extent = NULL, verbose = TRUE) {
  
  Year = Lon = Total = NULL
  
  # get the name of the model
  model.string <- gsub("-FireMIP", "", run@format)

  # ANNUAL, PER-VEGTYPE
  # Any annual, per PFT variable should be manageable here, eg landCoverFrac
  if(quantity@id == "landCoverFrac" ||
     quantity@id == "lai"           ||
     quantity@id == "theightpft") {
    
    # make the string (note special cases)
    file.string <- file.path(run@dir, paste0(run@id, "_", quantity@id, ".nc"))
    if(model.string == "Inferno" && quantity@id == "landCoverFrac") { file.string <- file.path(run@dir, paste0(run@id, "_", "LandCoverFrac", ".nc")) }
    
    
    # open the netCDF file (not ORCHIDEE! - those are single files, need to open them one by one)
    if(model.string != "ORCHIDEE") this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
    
    
    # get the dimensions (depends on model type) and the ordering type
    africa.centre <- FALSE
    remove.total <- FALSE
    requires.averaging <- FALSE
    if(model.string == "LPJ-GUESS-SPITFIRE-OLD") {
      if(quantity@id != "theightpft") this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
      else this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1700:2013
      ordering <- c(1,2,3)
      if(quantity@id != "theightpft") remove.total <- TRUE
    }
    else if(model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-SIMFIRE-BLAZE") {
      this.pfts <- c("C3G_pas", "C4G_pas", "BNE", "BINE", "BNS", "TeBS", "IBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "TeSW", "TeSWirr", "TeWW", "TeWWirr", "TeCo", "TeCoirr")
      this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
      if(quantity@id == "landCoverFrac") this.time <- 1700:2013
      if(quantity@id == "lai") this.time <- 1950:2013
      ordering <-  c(1,2,3)
    }
    else if(model.string == "CLM") {
      this.pfts <- c("Bare", "TeNE", "BNE", "BNS", "TrBE", "TeBE", "TrBR", "TeBS", "BBS", "BE_Shb", "TeBS_Shb", "BBS_Shb", "C3G_arc", "C3G", "C4G", "Crop1", "Crop2")
      this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
      if(quantity@id == "landCoverFrac") this.time <- 1700:2013
      if(quantity@id == "lai" || quantity@id == "theightpft") this.time <- 1950:2013
      ordering <- c(1,3,2)
      africa.centre <- TRUE
    }
    else if(model.string == "CTEM") {
      if(quantity@id == "landCoverFrac") this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS", "Bare")
      if(quantity@id == "lai") this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS", "Bare")
      this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
      this.time <- 1860:2015
      ordering <- c(3,1,2)
      africa.centre <- TRUE
      requires.averaging <- TRUE
    }
    else if(model.string == "Inferno") {
      if(quantity@id == "landCoverFrac") this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb", "Urban", "Water", "Bare", "Ice")
      if(quantity@id == "lai" || quantity@id == "theightpft") this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1701:2013
      ordering <- c(3,1,2)
      africa.centre <- TRUE
      requires.averaging <- TRUE
    }
    else if(model.string == "JSBACH") {
      this.pfts <- c("TrE", "TrD", "ExtE", "ExtD", "Rg_Shb", "De_Shb", "C3G", "C4G", "C3G_pas", "C4G_pas", "Crop")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      if(quantity@id == "landCoverFrac") this.time <- 1700:2013
      if(quantity@id == "lai" || quantity@id == "theightpft") this.time <- 1950:2013
      ordering <- c(3,1,2)
      africa.centre <- TRUE
    }
    else if(model.string == "ORCHIDEE") {
      this.pfts <- c("Bare", "TrBE", "TrBR", "TeNE", "TeBE", "TeBS", "BNE", "BBS", "BNS", "C3G", "C4G", "C3_agr", "C4_agr" )
      this.time <- 1700:2013
      ordering <- c(3,1,2)
      africa.centre <- TRUE
    }
    
    # choose range of years (if specifed, else take the whole range)
    if(!is.null(first.year) &  !is.null(last.year)){
      
      # match the range to the time axis
        index.range <- match(c(first.year, last.year), this.time)
      
      # check it is sensible
      if(class(index.range) != "integer" || length(index.range) != 2 || index.range[1] > index.range[2]){
        stop(paste("Invalid index.range in openFireMIPOutputFile(), = ", index.range))
      }
      
      # get start and end
      start.index <- index.range[1]
      n.years <- index.range[2] - index.range[1] + 1
      
    }
    # if no range specified, then get the lot
    else {
      index.range <- this.time[1]:this.time[length(this.time)]
      start.index = 1
      n.years = length(this.time)
      first.year <- this.time[1]
      last.year <- this.time[length(this.time)]
    }
    
    # What we do now depend on how we want the output to be returned
    
    # Assuming long data.table, ie.
    # Lon Lat Year PFT1 PFT1 PFT3 ... PFTN
    # ... ... ...  ...  ...  ...  ... ...
    
    # get each year and make it into a data.table
    t1 <- Sys.time()
    full.dt <- data.table()
    for(counter in 0:(n.years-1)) {
      
      # get one year - depends on the structure of each model
      if(model.string == "LPJ-GUESS-SPITFIRE-OLD" || model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-SIMFIRE-BLAZE") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, counter+start.index), count = c(-1,-1,-1, 1))
      }
      else if(model.string == "CLM") {
        this.slice <- ncvar_get(this.nc, start = c(counter+start.index,1,1,1), count = c(1,-1,-1, -1))
      }
      else if(model.string == "CTEM") {
        #this.slice <- ncvar_get(this.nc, start = c(1,1,1, ((counter+start.index) * 12) - 11) , count = c(-1,-1,-1, 12))
        this.slice <- ncvar_get(this.nc, start = c(1,1,1,counter+start.index) , count = c(-1,-1,-1,1))
      }
      else if(model.string == "Inferno") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, ((counter+start.index) * 12)) , count = c(-1,-1,-1, 12))
      }
      else if(model.string == "JSBACH") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, counter+start.index) , count = c(-1,-1,-1, 1))
      }
      else if(model.string == "ORCHIDEE") {
        file.string <- file.path(run@dir, quantity@id, paste0(quantity@id, "_", counter+first.year, ".nc"))
        this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
        this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
        this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
        this.slice <- ncvar_get(this.nc, start = c(1,1,1,1) , count = c(-1,-1,-1,1))
        if(quantity@id == "landCoverFrac") this.slice <- this.slice / 100
      }
      
      # average and permute dimensions (of necessary) and rename dimesions
      if(requires.averaging) this.slice <- apply(this.slice, c(1,2,3), mean)
      if(!identical(ordering, c(1,2,3))) this.slice <- aperm(this.slice, ordering)
      dimnames(this.slice) <- list(this.pfts, this.lon, this.lat)
      
      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))
      
      # chuck out the NAs (to save space)
      this.slice.dt <- stats::na.omit(this.slice.dt)
      
      # re-label the columns
      setnames(this.slice.dt, c("PFT", "Lon", "Lat", "value"))
      
      # dcast back to a column for every PFT
      this.slice.dt <- dcast(this.slice.dt, Lon + Lat  ~ PFT, value.var = "value")
      
      # add a column for "Year"
      this.slice.dt[, Year := this.time[counter+start.index]]
      
      # reorder columns so that "Year" follows after "Lon" and "Lat"
      new.order <- c("Lon", "Lat", names(this.slice.dt)[length(this.slice.dt)], names(this.slice.dt)[3:(length(this.slice.dt)-1)])
      setcolorder(this.slice.dt, new.order)
      
      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)
      
    }
    t2 <- Sys.time()
    print(t2-t1)
    
  } # END ANNUAL PER-PFT CASE
  
  
  # MONTHLY, ALL VEGTYPES COMBINED
  # Any annual, per PFT variable should be manageable here, eg gpp
  if(quantity@id == "gpp" ||
     quantity@id == "npp" ||
     quantity@id == "nbp") {
    
    # make the string (note special cases)
    file.string <- file.path(run@dir, paste0(run@id, "_", quantity@id, ".nc"))
    
    
    # open the netCDF file (not ORCHIDEE! - those are single files, need to open them one by one)
    if(model.string != "ORCHIDEE") this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
    
    
    # get the dimensions (depends on model type) and the ordering type
    africa.centre <- FALSE
    remove.total <- FALSE
    requires.averaging <- FALSE
    if(model.string == "LPJ-GUESS-SPITFIRE-OLD") {
      if(quantity@id != "theightpft") this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
      else this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1700:2013
      ordering <- c(1,2,3)
      if(quantity@id != "theightpft") remove.total <- TRUE
    }
    else if(model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-SIMFIRE-BLAZE") {
      this.pfts <- c("C3G_pas", "C4G_pas", "BNE", "BINE", "BNS", "TeBS", "IBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "TeSW", "TeSWirr", "TeWW", "TeWWirr", "TeCo", "TeCoirr")
      this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
      if(quantity@id == "landCoverFrac") this.time <- 1700:2013
      if(quantity@id == "lai") this.time <- 1950:2013
      ordering <-  c(1,2,3)
    }
    else if(model.string == "CLM") {
      this.pfts <- c("Bare", "TeNE", "BNE", "BNS", "TrBE", "TeBE", "TrBR", "TeBS", "BBS", "BE_Shb", "TeBS_Shb", "BBS_Shb", "C3G_arc", "C3G", "C4G", "Crop1", "Crop2")
      this.lat <- ncvar_get(this.nc,"lat",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"lon",verbose=verbose)
      if(quantity@id == "landCoverFrac") this.time <- 1700:2013
      if(quantity@id == "lai" || quantity@id == "theightpft") this.time <- 1950:2013
      ordering <- c(1,3,2)
      africa.centre <- TRUE
    }
    else if(model.string == "CTEM") {
      if(quantity@id == "landCoverFrac") this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS")
      if(quantity@id == "lai") this.pfts <- c("NDL-EVG", "NDL-DCD", "BDL-EVG", "BDL-DCD-COLD", "BDL-DCD-DRY", "C3-CROP", "C4-CROP", "C3-GRASS", "C4-GRASS", "Bare")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1860:2011
      ordering <- c(3,1,2)
      africa.centre <- TRUE
      requires.averaging <- TRUE
    }
    else if(model.string == "Inferno") {
      if(quantity@id == "landCoverFrac") this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb", "Urban", "Water", "Bare", "Ice")
      if(quantity@id == "lai" || quantity@id == "theightpft") this.pfts <- c("TrBE","TeBE", "BD", "NE", "ND", "C3G", "C4G", "Ev_Shb", "De_Shb")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1701:2013
      ordering <- c(3,1,2)
      africa.centre <- TRUE
      requires.averaging <- TRUE
    }
    else if(model.string == "JSBACH") {
      this.pfts <- c("TrE", "TrD", "ExtE", "ExtD", "Rg_Shb", "De_Shb", "C3G", "C4G", "C3G_pas", "C4G_pas", "Crop")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      if(quantity@id == "landCoverFrac") this.time <- 1700:2013
      if(quantity@id == "lai" || quantity@id == "theightpft") this.time <- 1950:2013
      ordering <- c(3,1,2)
      africa.centre <- TRUE
    }
    else if(model.string == "ORCHIDEE") {
      this.pfts <- c("Bare", "TrBE", "TrBR", "TeNE", "TeBE", "TeBS", "BNE", "BBS", "BNS", "C3G", "C4G", "C3_agr", "C4_agr" )
      this.time <- 1700:2013
      ordering <- c(3,1,2)
      africa.centre <- TRUE
    }
    
    # choose range of year (if specifed, else take the whole range)
    if(!is.null(first.year) &  !is.null(last.year)){
      
      # match the range to the time axis
      index.range <- match(c(first.year, last.year), this.time)
      
      # check it is sensible
      if(class(index.range) != "integer" || length(index.range) != 2 || index.range[1] > index.range[2]){
        stop(paste("Invalid index.range in openFireMIPOutputFile(), = ", index.range))
      }
      
      # get start and end
      start.index <- index.range[1]
      n.years <- index.range[2] - index.range[1] + 1
      
    }
    # if no range specified, then get the lot
    else {
      index.range <- this.time[1]:this.time[length(this.time)]
      start.index = 1
      n.years = length(this.time)
      first.year <- this.time[1]
      last.year <- this.time[length(this.time)]
    }
    
    # What we do now depend on how we want the output to be returned
    
    # Assuming long data.table, ie.
    # Lon Lat Year PFT1 PFT1 PFT3 ... PFTN
    # ... ... ...  ...  ...  ...  ... ...
    
    # get each year and make it into a data.table
    t1 <- Sys.time()
    full.dt <- data.table()
    for(counter in 0:(n.years-1)) {
      
      # get one year - depends on the structure of each model
      if(model.string == "LPJ-GUESS-SPITFIRE-OLD" || model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-SIMFIRE-BLAZE") {
        this.slice <- ncvar_get(this.nc, start = c(1,1, ((counter+start.index) * 12) - 11) , count = c(-1,-1, 12))
        print(((counter+start.index) * 12) - 11)
        print(dim(this.slice))
      }
      else if(model.string == "CLM") {
        this.slice <- ncvar_get(this.nc, start = c(counter+start.index,1,1,1), count = c(1,-1,-1, -1))
      }
      else if(model.string == "CTEM") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, ((counter+start.index) * 12) - 11) , count = c(-1,-1,-1, 12))
      }
      else if(model.string == "Inferno") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, ((counter+start.index) * 12)) , count = c(-1,-1,-1, 12))
      }
      else if(model.string == "JSBACH") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, counter+start.index) , count = c(-1,-1,-1, 1))
      }
      else if(model.string == "ORCHIDEE") {
        file.string <- file.path(run@dir, quantity@id, paste0(quantity@id, "_", counter+first.year, ".nc"))
        this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
        this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
        this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
        this.slice <- ncvar_get(this.nc, start = c(1,1,1,1) , count = c(-1,-1,-1,-1))
        if(quantity@id == "landCoverFrac") this.slice <- this.slice / 100
      }
      
      # average and permute dimensions (of necessary) and rename dimesions
      if(requires.averaging) this.slice <- apply(this.slice, c(1,2,3), mean)
      if(!identical(ordering, c(1,2,3))) this.slice <- aperm(this.slice, ordering)
      dimnames(this.slice) <- list(this.lon, this.lat, c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
      
      # if necessary multiply data by a constant
      this.slice <- (1/0.00001157407407) * this.slice
      
      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))

      # chuck out the NAs (to save space)
      this.slice.dt <- stats::na.omit(this.slice.dt)
      
      # re-label the columns
      setnames(this.slice.dt, c("Lon", "Lat", "Month", "value"))
  
      # dcast back to a column for every PFT
      this.slice.dt <- dcast(this.slice.dt, Lon + Lat  ~ Month, value.var = "value")
      
      # add a column for "Year"
      this.slice.dt[, Year := this.time[counter+start.index]]
      
      # reorder columns so that "Year" follows after "Lon" and "Lat"
      new.order <- c("Lon", "Lat", names(this.slice.dt)[length(this.slice.dt)], names(this.slice.dt)[3:(length(this.slice.dt)-1)])
      setcolorder(this.slice.dt, new.order)
      
      # add it on to the full data.table
      full.dt <- rbind(full.dt, this.slice.dt)
      
    }
    t2 <- Sys.time()
    print(t2-t1)
    
  }
  
  
  # Tidy stuff
  if(africa.centre) full.dt[,Lon := LondonCentre(Lon)]
  if(remove.total) full.dt[, Total := NULL]
  full.dt <- stats::na.omit(full.dt)
  
  return(full.dt)

}


#' Detemine PFTs present in an FireMIP run source 
#' 
#' @param x  A Source objects describing a FireMIP source
#' @param variables Some variable to look for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.  Currently ignored.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_FireMIP <- function(x, variables) {
  
  warning("determinePFTs_FireMIP not currently implmented.")
  return(x@format@pft.set)
  
}



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


determineQuantities_FireMIP <- function(source){
  
  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")
  
  quantities.present <- list()
  for(file in files.present) {
    
      # simple code to deparse the filename and find the variables
    
  }
  
  return(quantities.present)
  
}

########################################################
########### FireMIP Coarse PFTS ########################
########################################################

#' @format An S4 class object with the slots as defined below.
#' @rdname PFT-class
#' @keywords datasets
FireMIP.PFTs <- list(
  
  # BOREAL TREES
  
  # NE
  new("PFT",
           id = "NE",
           name = "Needleleaved Evergreen Tree",
           growth.form = "Tree",
           leaf.form = "Needleleaved",
           phenology = "Evergreen",
           climate.zone = "NA",
           colour = "darkblue",
           shade.tolerance = "no"
  ),
  
  # NS
  new("PFT",
           id = "NS",
           name = "Needleleaved Summergreen Tree",
           growth.form = "Tree",
           leaf.form = "Needleleaved",
           phenology = "Summergreen",
           climate.zone = "NA",
           colour = "cornflowerblue",
           shade.tolerance = "no"
  ),
  
  # BS
  new("PFT",
           id = "BS",
           name = "Broadleaved Summergreen Tree",
           growth.form = "Tree",
           leaf.form = "Broadleaved",
           phenology = "Summergreen",
           climate.zone = "NA",
           colour = "cyan",
           shade.tolerance = "no"
  ),
  
  # BE
  new("PFT",
           id = "BE",
           name = "Broadleaved Evergreen Tree",
           growth.form = "Tree",
           leaf.form = "Broadleaved",
           phenology = "Evergreen",
           climate.zone = "NA",
           colour = "darkgreen",
           shade.tolerance = "no"
  ),
  
  # BR
  new("PFT",
           id = "BR",
           name = "Broadleaved Raingreen Tree",
           growth.form = "Tree",
           leaf.form = "Broadleaved",
           phenology = "Raingreen",
           climate.zone = "NA",
           colour = "maroon",
           shade.tolerance = "no"
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
            shade.tolerance = "no"
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
            shade.tolerance = "no"
  ),
  
  # Shb
  new("PFT",
            id = "Shb",
            name = "Shrub",
            growth.form = "Shrub",
            leaf.form = "NA",
            phenology = "NA",
            climate.zone = "NA",
            colour = "darkred",
            shade.tolerance = "no"
  ),
 
  # Crops
  new("PFT",
            id = "Crops",
            name = "Agricultural",
            growth.form = "Agricultural",
            leaf.form = "NA",
            phenology = "NA",
            climate.zone = "NA",
            colour = "black",
            shade.tolerance = "no"
  )
  
)



######################################################################
########## FIREMIP QUANTITIES  ######################################
######################################################################  


#' @format The \code{Quantity} class is an S4 class with the slots defined below.
#' @rdname Quantity-class
#' @keywords datasets
#' 
FireMIP.quantities <- list(

#### BURNT AREA AND EMISSIONS

new("Quantity",
    id = "fFirePFT",
    name = "C emitted from fire (per PFT)",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "C emitted from fire (per PFT)"),

new("Quantity",
    id = "fFire",
    name = "C emitted from fire",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "CO2 emitted from fire"),

new("Quantity",
    id = "coFire",
    name = "CO emitted from fire",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "CO emitted from fire"),

new("Quantity",
    id = "burntArea",
    name = "Burnt Area Fraction (per PFT)",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Burnt Area Fraction (per PFT)"),

new("Quantity",
    id = "BA",
    name = "Burnt Area Fraction (monthly)",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Burnt Area Fraction (monthly)"),

new("Quantity",
    id = "fFirepft",
    name = "C emitted from fire (per PFT)",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "C emitted from fire (per PFT)"),



### FUEL LOADS

new("Quantity",
    id = "cFuelLiveGrass",
    name = "Carbon in live grass fuel",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in live grass fuel"),

new("Quantity",
    id = "cFuel1hr",
    name = "Carbon in 1hr fuel",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in 1hr fuel"),

new("Quantity",
    id = "cFuel10hr",
    name = "Carbon in 10hr fuel",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in 10hr fuel"),

new("Quantity",
    id = "cFuel100hr",
    name = "Carbon in 100hr fuel",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in 100hr fuel"),

new("Quantity",
    id = "cFuel1000hr",
    name = "Carbon in 1000hr fuel",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in 1000hr fuel"),


### COMBUSION COMPLETENESS

new("Quantity",
    id = "ccFuelLiveGrass",
    name = "Combusion Completeness in live grass fuel",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Combusion Completeness live grass fuel"),

new("Quantity",
    id = "ccFuel1hr",
    name = "Combustion Completeness in 1hr fuel",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Combustion Completenessin 1hr fuel"),

new("Quantity",
    id = "ccFuel10hr",
    name = "Combustion Completenessin 10hr fuel",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Combustion Completeness in 10hr fuel"),

new("Quantity",
    id = "ccFuel100hr",
    name = "Combustion Completenessin 100hr fuel",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Combustion Completeness in 100hr fuel"),

new("Quantity",
    id = "ccFuel1000hr",
    name = "Combustion Completenessin 1000hr fuel",
    units = "%",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Combustion Completeness in 1000hr fuel"),


### FUEL MOISTURE

new("Quantity",
    id = "mFuelDead",
    name = "Fuel moisture of dead fuel",
    units = "",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Fuel moisture of dead fuel"),

new("Quantity",
    id = "mFuelLiveGrass",
    name = "Fuel moisture of live grass fuel",
    units = "",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Fuel moisture of live grass fuel"),


### FIRE PROPERTIES AND MORTALITY

new("Quantity",
    id = "intensFire",
    name = "Fireline intensity",
    units = "kW m-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Fireline intensity"),

new("Quantity",
    id = "nrfire",
    name = "Number of fires",
    units = "nr m-2 month-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Number of fires"),

new("Quantity",
    id = "meanFire",
    name = "Mean fire size",
    units = "m2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Mean fire size"),

new("Quantity",
    id = "cMortality",
    name = "Number of individuals killed",
    units = "indiv m-2 month-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Number of individuals killed"),

new("Quantity",
    id = "RoS",
    name = "Mean rate of spread",
    units = "m/s",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Mean rate of spread"),

new("Quantity",
    id = "durat",
    name = "Mean fire duration",
    units = "min",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Mean fire duration"),

### HYDROLOGICAL VARIABLES

new("Quantity",
    id = "mrro",
    name = "Total Runoff",
    units = "kg m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Total Runoff"),

new("Quantity",
    id = "evapotrans",
    name = "Total Evapo-Transpiration",
    units = "kg m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Total Evapo-Transpiration"),

new("Quantity",
    id = "mrso",
    name = "Total Soil Moisture Content",
    units = "kg m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Total Soil Moisture Content"),

new("Quantity",
    id = "mrsos",
    name = "Surface Layer (50cm) Soil Moisture Content",
    units = "kg m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Surface Layer (50cm) Soil Moisture Content"),

new("Quantity",
    id = "evspsblveg",
    name = "Evaporation from Canopy",
    units = "kg m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Evaporation from Canopy"),

new("Quantity",
    id = "evspsblsoi",
    name = "Evaporation from Soil",
    units = "kg m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Evaporation from Soil"),

new("Quantity",
    id = "tran",
    name = "Transpiration",
    units = "kg m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Transpiration"),



### FLUXES

new("Quantity",
    id = "gpp",
    name = "Gross Primary Production",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Gross Primary Production"),

new("Quantity",
    id = "npp",
    name = "Net Primary Production",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Net Primary Production"),

new("Quantity",
    id = "nbp",
    name = "Net Biospheric Production",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Net Biospheric Production"),

new("Quantity",
    id = "ra",
    name = "Autotrophic (Plant) Respiration",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Autotrophic (Plant) Respiration"),

new("Quantity",
    id = "rh",
    name = "Heterotrophic Respiration",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Heterotrophic Respiration"),

new("Quantity",
    id = "gpppft",
    name = "Vegtype level GPP",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Vegtype level GPP"),

new("Quantity",
    id = "npppft",
    name = "Vegtype level NPP",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Vegtype level NPP"),

new("Quantity",
    id = "fLuc",
    name = "CO2 Flux to Atmosphere from Land Use Change",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "CO2 Flux to Atmosphere from Land Use Change"),


# POOLS

new("Quantity",
    id = "cVegpft",
    name = "Vegtype level Carbon in Vegetation",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Vegtype level Carbon in Vegetation"),

new("Quantity",
    id = "cVeg",
    name = "Carbon in Vegetation",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in Vegetation"),

new("Quantity",
    id = "cLitter",
    name = "Carbon in Above-ground Litter Pool",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in Above-ground Litter Pool"),

new("Quantity",
    id = "cSoil",
    name = "Carbon in Soil (including below-ground litter)",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in Soil (including below-ground litter)"),


###  STRUCTURE

new("Quantity",
    id = "landCoverFrac",
    name = "Fractional Land Cover of PFT",
    units = "",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Fractional Land Cover of PFT"),

new("Quantity",
    id = "lai",
    name = "Leaf Area Index",
    units = "",
    colours = reversed.viridis,
    format = "FireMIP",
    cf.name = "Leaf Area Index"),

new("Quantity",
    id = "theightpft",
    name = "Vegtype level tree heights",
    units = "m",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Vegtype level tree heights"),

### LAND USE

new("Quantity",
    id = "cProduct",
    name = "Carbon in Products of Land Use Change",
    units = "kg C m-2",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "Carbon in Products of Land Use Change"),

new("Quantity",
    id = "fLuc",
    name = "CO2 Flux to Atmosphere from Land Use Change",
    units = "kg C m-2 s-1",
    colours = reversed.viridis,
    format = c("FireMIP", "LPJ-GUESS-SPITFIRE"),
    cf.name = "CO2 Flux to Atmosphere from Land Use Change")

)


####################################################
########### FireMIP FORMAT ########################
####################################################

#' @description \code{FireMIP} - a Format for reading 'standard' FireMIP model output
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' 
#' 
FireMIP<- new("Format",
                
                # UNIQUE ID
                id = "FireMIP",
                
                # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
                determinePFTs = determinePFTs_FireMIP,
                
                # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                determineQuantities = determineQuantities_FireMIP,
                
                # FUNCTION TO READ A FIELD 
                getField = getField_FireMIP,
                
                # DEFAULT GLOBAL PFTS  
                default.pfts = FireMIP.PFTs,
                
                # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
                quantities = FireMIP.quantities
                
)
