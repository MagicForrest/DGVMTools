#' Open a FireMIP output file
#' 
#' Opens a .nc file from the FireMIP output and sorts out the meta-data and dimensions and all that messy stuff.  
#' Returns a data.table, because it is intended to be called by getModelObject(), but of course the data.table could be used directly if you wish
#' 
#' 
#' @param run A ModelRun object to define the run we want to open.
#' @param quantity A Quantity object to define which variable we want to look up
#' @param temporal.extent A TemporalExtent object to define which years we want to read
#' @param spatial.extent A SpatialExtent object to define the spatial extent we want to read
#' @param verbose Logical, if TRUE spew forth a lot of info.
#' 
#' @return A data.table
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 

openFireMIPOutputFile <- function(run, quantity, temporal.extent = NULL, spatial.extent = NULL, verbose = TRUE) {
  
  Year = Lon = Total = NULL
  
  # get the name of the model
  model.string <- gsub("-FireMIP", "", run@model)

  # ANNUAL, PER-VEGTYPE
  # Any annual, per PFT variable should be manageable here, eg landCoverFrac
  if(quantity@id == "landCoverFrac" ||
     quantity@id == "lai"           ||
     quantity@id == "theightpft") {
    
    # make the string (note special cases)
    file.string <- file.path(run@run.dir, paste0(run@id, "_", quantity@id, ".nc"))
    if(model.string == "Inferno" && quantity@id == "landCoverFrac") { file.string <- file.path(run@run.dir, paste0(run@id, "_", "LandCoverFrac", ".nc")) }
    
    
    # open the netCDF file (not ORCHIDEE! - those are single files, need to open them one by one)
    if(model.string != "ORCHIDEE") this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
    
    
    # get the dimensions (depends on model type) and the ordering type
    africa.centre <- FALSE
    remove.total <- FALSE
    requires.averaging <- FALSE
    if(model.string == "LPJ-GUESS-SPITFIRE") {
      if(quantity@id != "theightpft") this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
      else this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1700:2013
      ordering <- c(1,2,3)
      if(quantity@id != "theightpft") remove.total <- TRUE
    }
    else if(model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-BLAZE") {
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
    
    # choose temporal extent (if specifed, else take the whole range)
    if(!is.null(temporal.extent)){
      
      # match the range to the time axis
      first.year <- temporal.extent@start
      last.year <- temporal.extent@end
      index.range <- match(c(temporal.extent@start, temporal.extent@end), this.time)
      
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
      if(model.string == "LPJ-GUESS-SPITFIRE" || model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-BLAZE") {
        this.slice <- ncvar_get(this.nc, start = c(1,1,1, counter+start.index), count = c(-1,-1,-1, 1))
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
        file.string <- file.path(run@run.dir, quantity@id, paste0(quantity@id, "_", counter+first.year, ".nc"))
        this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
        this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
        this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
        this.slice <- ncvar_get(this.nc, start = c(1,1,1,1) , count = c(-1,-1,-1,-1))
        if(quantity@id == "landCoverFrac") this.slice <- this.slice / 100
      }
      
      # average and permute dimensions (of necessary) and rename dimesions
      if(requires.averaging) this.slice <- apply(this.slice, c(1,2,3), mean)
      if(!identical(ordering, c(1,2,3))) this.slice <- aperm(this.slice, ordering)
      dimnames(this.slice) <- list(this.pfts, this.lon, this.lat)
      
      # melt to a data.table, via data.frame
      this.slice.dt <- as.data.table(melt(this.slice))
      
      # chuck out the NAs (to save space)
      this.slice.dt <- na.omit(this.slice.dt)
      
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
  # Any annual, per PFT variable should be manageable here, eg landCoverFrac
  if(quantity@id == "gpp" ||
     quantity@id == "npp" ||
     quantity@id == "nbp") {
    
    # make the string (note special cases)
    file.string <- file.path(run@run.dir, paste0(run@id, "_", quantity@id, ".nc"))
    
    
    # open the netCDF file (not ORCHIDEE! - those are single files, need to open them one by one)
    if(model.string != "ORCHIDEE") this.nc <- nc_open(file.string, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
    
    
    # get the dimensions (depends on model type) and the ordering type
    africa.centre <- FALSE
    remove.total <- FALSE
    requires.averaging <- FALSE
    if(model.string == "LPJ-GUESS-SPITFIRE") {
      if(quantity@id != "theightpft") this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "Total")
      else this.pfts <- c("BNE", "BINE", "BNS", "BIBS", "TeNE", "TeBS", "TeIBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G")
      this.lat <- ncvar_get(this.nc,"latitude",verbose=verbose)
      this.lon <- ncvar_get(this.nc,"longitude",verbose=verbose)
      this.time <- 1700:2013
      ordering <- c(1,2,3)
      if(quantity@id != "theightpft") remove.total <- TRUE
    }
    else if(model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-BLAZE") {
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
    
    # choose temporal extent (if specifed, else take the whole range)
    if(!is.null(temporal.extent)){
      
      # match the range to the time axis
      first.year <- temporal.extent@start
      last.year <- temporal.extent@end
      index.range <- match(c(temporal.extent@start, temporal.extent@end), this.time)
      
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
      if(model.string == "LPJ-GUESS-SPITFIRE" || model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-BLAZE") {
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
        file.string <- file.path(run@run.dir, quantity@id, paste0(quantity@id, "_", counter+first.year, ".nc"))
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
      this.slice.dt <- na.omit(this.slice.dt)
      
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
  full.dt <- na.omit(full.dt)
  
  return(full.dt)

}

#' Aggregate to FireMIP PFTs
#' 
#' Aggregates the PFTs in a FireMIP ModelObject to a standard set of PFTs
#' 
#' @param input.data A ModelObject from a FireMIP model
#' @param remove.agriculture Logical, if TRUE, remove the cropland PFT and scale up the rest to compensate
#' 
#' @return A ModelObject with the standard FireMIP PFTs
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 


toFireMIPPFTs <- function(input.data, remove.agriculture = FALSE) {
  
  Lat = NS = BS = BR = NULL
  
  # get the name of the model
  model.string <- gsub("-FireMIP", "", input.data@run@model)
  print(paste0("Standardising to FireMIP PFTs: ", model.string))
  
  # retrieve the data.table
  dt <- input.data@data
  
  ### FIRST DEFINE THE FIREMIP PFTS
  FireMIP.PFTs <- c("Evergreen Broadleafed Tree" = "BE",
                    "Summergreen Broadleafed Tree" = "BS",
                    "Raingreen Broadleafed" = "BR",
                    "Evergreen Needleleafed" = "NE",
                    "Summergreen Needleafed" = "NS",
                    "C3 Grass" = "C3G",
                    "C4 Grass" ="C4G",
                    "Shrub" = "Shb",
                    "Croplands" = "Crops")
  
  
  ### NOW DEFINE THE MAPPING FOR EACH MODEL
  
  # LPJ-GUESS-SPITFIRE
  if(model.string == "LPJ-GUESS-SPITFIRE") {
    classify.df <-  data.frame(original = c("BNE"), FireMIP = c("NE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("BINE", "NE"))
    classify.df <- rbind(classify.df, c("BNS", "NS"))
    classify.df <- rbind(classify.df, c("BIBS", "BS"))
    classify.df <- rbind(classify.df, c("TeNE", "NE"))
    classify.df <- rbind(classify.df, c("TeBS", "BS"))
    classify.df <- rbind(classify.df, c("TeIBS", "BS"))
    classify.df <- rbind(classify.df, c("TeBE", "BE"))
    classify.df <- rbind(classify.df, c("TrBE", "BE"))
    classify.df <- rbind(classify.df, c("TrIBE", "BE"))
    classify.df <- rbind(classify.df, c("TrBR", "BR"))
    classify.df <- rbind(classify.df, c("C3G", "C3G"))
    classify.df <- rbind(classify.df, c("C4G", "C4G"))
    
  }
  
  # LPJ-GUESS-SPITFIRE
  if(model.string == "LPJ-GUESS-GlobFIRM" || model.string == "LPJ-GUESS-BLAZE" ) {
    
    # normal PFTs similar (but not identical to) LPJ-GUESS-SPITFIRE
    classify.df <-  data.frame(original = c("BNE"), FireMIP = c("NE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("BINE", "NE"))
    classify.df <- rbind(classify.df, c("BNS", "NS"))
    classify.df <- rbind(classify.df, c("TeBS", "BS"))
    classify.df <- rbind(classify.df, c("IBS", "BS"))
    classify.df <- rbind(classify.df, c("TeBE", "BE"))
    classify.df <- rbind(classify.df, c("TrBE", "BE"))
    classify.df <- rbind(classify.df, c("TrIBE", "BE"))
    classify.df <- rbind(classify.df, c("TrBR", "BR"))
    classify.df <- rbind(classify.df, c("C3G", "C3G"))
    classify.df <- rbind(classify.df, c("C4G", "C4G"))
    
    # agricultural
    classify.df <- rbind(classify.df, c("C3G_pas", "C3G"))
    classify.df <- rbind(classify.df, c("C4G_pas", "C4G"))
    classify.df <- rbind(classify.df, c("TeSW", "Crops"))
    classify.df <- rbind(classify.df, c("TeSWirr", "Crops"))
    classify.df <- rbind(classify.df, c("TeWW", "Crops"))
    classify.df <- rbind(classify.df, c("TeWWirr", "Crops"))
    classify.df <- rbind(classify.df, c("TeCo", "Crops"))
    classify.df <- rbind(classify.df, c("TeCoirr", "Crops"))
    
  }
  
  # CLM
  if(model.string == "CLM") {
    
    # normal PFTs similar (but not identical to) LPJ-GUESS-SPITFIRE
    classify.df <-  data.frame(original = c("BNE"), FireMIP = c("NE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("TeNE", "NE"))
    classify.df <- rbind(classify.df, c("BNS", "NS"))
    classify.df <- rbind(classify.df, c("TrBE", "BE"))
    classify.df <- rbind(classify.df, c("TeBE", "BE"))
    classify.df <- rbind(classify.df, c("TrBR", "BR"))
    classify.df <- rbind(classify.df, c("TeBS", "BS"))
    classify.df <- rbind(classify.df, c("BBS", "BS"))
    classify.df <- rbind(classify.df, c("C3G", "C3G"))
    classify.df <- rbind(classify.df, c("C3G_arc", "C3G"))
    classify.df <- rbind(classify.df, c("C4G", "C4G"))
    
    # shrubs
    classify.df <- rbind(classify.df, c("BE_Shb", "Shb"))
    classify.df <- rbind(classify.df, c("TeBS_Shb", "Shb"))
    classify.df <- rbind(classify.df, c("BBS_Shb", "Shb"))
    
    
    # agricultural
    classify.df <- rbind(classify.df, c("Crop1", "Crops"))
    classify.df <- rbind(classify.df, c("Crop2", "Crops"))
    
  }
  
  
  # CTEM
  if(model.string == "CTEM") {
    
    
    # natural PFTs
    classify.df <-  data.frame(original = c("NDL-EVG"), FireMIP = c("NE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("NDL-DCD", "NS"))
    classify.df <- rbind(classify.df, c("BDL-EVG", "BE"))
    classify.df <- rbind(classify.df, c("BDL-DCD-DRY", "BR"))
    classify.df <- rbind(classify.df, c("BDL-DCD-COLD", "BS"))
    classify.df <- rbind(classify.df, c("C3-GRASS", "C3G"))
    classify.df <- rbind(classify.df, c("C4-GRASS", "C4G"))
    
    # agriculture
    classify.df <- rbind(classify.df, c("C3-CROP", "Crops"))
    classify.df <- rbind(classify.df, c("C4-CROP", "Crops"))
    
  }
  
  
  # Inferno
  if(model.string == "Inferno") {
    
    # natural PFTs
    classify.df <-  data.frame(original = c("NE"), FireMIP = c("NE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("ND", "NS"))
    classify.df <- rbind(classify.df, c("TrBE", "BE"))
    classify.df <- rbind(classify.df, c("TeBE", "BE"))
    classify.df <- rbind(classify.df, c("BD", "BS"))
    classify.df <- rbind(classify.df, c("C3G", "C3G"))
    classify.df <- rbind(classify.df, c("C4G", "C4G"))
    
    # shrubs
    classify.df <- rbind(classify.df, c("Ev_Shb", "Shb"))
    classify.df <- rbind(classify.df, c("De_Shb", "Shb"))
    
  }
  
  # JSBACH
  if(model.string == "JSBACH") {
    
    # natural PFTs
    classify.df <-  data.frame(original = c("TrE"), FireMIP = c("BE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("TrD", "BR"))
    classify.df <- rbind(classify.df, c("ExtE", "NE"))
    classify.df <- rbind(classify.df, c("ExtD", "BS"))
    classify.df <- rbind(classify.df, c("C3G", "C3G"))
    classify.df <- rbind(classify.df, c("C4G", "C4G"))
    
    # shrubs
    classify.df <- rbind(classify.df, c("Rg_Shb", "Shb"))
    classify.df <- rbind(classify.df, c("De_Shb", "Shb"))
    
    # agriculture
    classify.df <- rbind(classify.df, c("C3G_pas", "C3G"))
    classify.df <- rbind(classify.df, c("C4G_pas", "C4G"))
    classify.df <- rbind(classify.df, c("Crop", "Crops"))
    
    
  }
  
  if(model.string == "ORCHIDEE") {
    
    # normal PFTs similar (but not identical to) LPJ-GUESS-SPITFIRE
    classify.df <-  data.frame(original = c("BNE"), FireMIP = c("NE"), stringsAsFactors = FALSE) 
    classify.df <- rbind(classify.df, c("TeNE", "NE"))
    classify.df <- rbind(classify.df, c("BNS", "NS"))
    classify.df <- rbind(classify.df, c("TrBE", "BE"))
    classify.df <- rbind(classify.df, c("TeBE", "BE"))
    classify.df <- rbind(classify.df, c("TrBR", "BR"))
    classify.df <- rbind(classify.df, c("TeBS", "BS"))
    classify.df <- rbind(classify.df, c("BBS", "BS"))
    classify.df <- rbind(classify.df, c("C3G", "C3G"))
    classify.df <- rbind(classify.df, c("C4G", "C4G"))
    
    # agricultural
    classify.df <- rbind(classify.df, c("C3_agr", "Crops"))
    classify.df <- rbind(classify.df, c("C4_agr", "Crops"))
    
  }
  
  
  
  ### DO THE CLASSIFICATION
  
  # For each FireMIP PFT sum the contributing model PFTs
  for(FireMIP.PFT in FireMIP.PFTs) {
    
    # get a list the corresponding model PFTs
    model.PFTs <- classify.df$original[which(classify.df$FireMIP == FireMIP.PFT)]
    
    # sum them 
    if(length(model.PFTs) > 0)  dt <- dt[, paste("FireMIP", FireMIP.PFT, sep = ".") := rowSums(.SD), .SDcols = model.PFTs, with = FALSE]
    else  dt <- dt[,  paste("FireMIP", FireMIP.PFT, sep = ".") := 0]
    
    # remove the PFTs we just summed
    if(length(model.PFTs) > 0)  dt <- dt[, model.PFTs := NULL, with = FALSE]
    
  }
  
  # Remove "Bare" etc.  if necessary
  if("Bare" %in% names(dt)) dt <- dt[, "Bare" := NULL, with = FALSE]
  if("Ice" %in% names(dt)) dt <- dt[, "Ice" := NULL, with = FALSE]
  if("Water" %in% names(dt)) dt <- dt[, "Water" := NULL, with = FALSE]
  if("Urban" %in% names(dt)) dt <- dt[, "Urban" := NULL, with = FALSE]
  
  
  ### SET THE NAMES
  setnames(dt, gsub("FireMIP.", "",  names(dt)))
  
  ### SPECIAL FIX FOR JSBACH FOR NEEDLE-LEAVED SUMMERGREEN
  if(model.string == "JSBACH") {
   dt <- dt[Lat > 60, NS := BS]
   dt <- dt[Lat > 60, BS := 0]
  }
  
  ### SPECIAL FIX FOR INFERNO FOR BROADLEAVED RAINGREEN
  if(model.string == "Inferno") {
    dt <- dt[abs(Lat) < 23, BR := BS]
    dt <- dt[abs(Lat) < 23, BS := 0]
  }
  
  
  
  
  ### RESCALE TO ACCOUNT FOR AGRICULTURE
  if(remove.agriculture) {
    rescale <- function(x) {
      x <- x * (1/(1-x$Crops))
    }

    dt <- dt[, FireMIP.PFTs := rescale(.SD), .SDcols = FireMIP.PFTs, with = FALSE]
    
    # important, remove any NAs that my have been introduced by scaling
    dt <- na.omit(dt)
    
    # remove agriculture column 
    if("Crops" %in% names(dt)) dt <- dt[, "Crops" := 0, with = FALSE]
    
  }
  
  ### AND RETURN
  input.data@data <- dt
  rm(dt)
  return(input.data)
  
  
}


########################################################
########### FireMIP Coarse PFTS ########################
########################################################

#' LPJ-GUESS(-SPITFIRE) Global PFTs
#' 
#' The list contain the standard LPJ-GUESS global PFTs and few more.  It includes some metadata 
#' and a preferred plot colour.
#' 
#' @format A list of \code{PFT} objects that store meta-data for commonly used PFTs from LPJ-GUESS (and LPJ-GUESS-SPITFIRE)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @seealso \code{PFT}-class
FireMIP.PFTs <- list(
  
  # BOREAL TREES
  
  NE = new("PFT",
           id = "NE",
           name = "Needleleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Evergreen",
           zone = "NA",
           colour = "darkblue",
           combine = "no"
  ),
  
  NS = new("PFT",
           id = "NS",
           name = "Needleleaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Needleleaved",
           phenology = "Summergreen",
           zone = "NA",
           colour = "cornflowerblue",
           combine = "no"
  ),
  
  BS = new("PFT",
           id = "BS",
           name = "Broadleaved Summergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Summergreen",
           zone = "NA",
           colour = "cyan",
           combine = "no"
  ),
  
  BE = new("PFT",
           id = "BE",
           name = "Broadleaved Evergreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Evergreen",
           zone = "NA",
           colour = "darkgreen",
           combine = "no"
  ),
  
  
  BR = new("PFT",
           id = "BR",
           name = "Broadleaved Raingreen Tree",
           lifeform = "Tree",
           leafform = "Broadleaved",
           phenology = "Raingreen",
           zone = "NA",
           colour = "maroon",
           combine = "no"
  ),
  
  # GRASSES
  
  C3G = new("PFT",
            id = "C3G",
            name = "Boreal/Temperate Grass",
            lifeform = "Grass",
            leafform = "Broadleaved",
            phenology = "GrassPhenology",
            zone = "NA",
            colour = "lightgoldenrod1",
            combine = "no"
  ),
  
  C4G = new("PFT",
            id = "C4G",
            name = "Tropical Grass",
            lifeform = "Grass",
            leafform = "Broadleaved",
            phenology = "GrassPhenology",
            zone = "NA",
            colour = "sienna2",
            combine = "no"
  ),
  
  # SHRUBS
  Shb = new("PFT",
            id = "Shb",
            name = "Shrub",
            lifeform = "Shrub",
            leafform = "NA",
            phenology = "NA",
            zone = "NA",
            colour = "darkred",
            combine = "no"
  ),
  
  # Agrcultural
  Crops = new("PFT",
            id = "Crops",
            name = "Agricultural",
            lifeform = "Agricultural",
            leafform = "NA",
            phenology = "NA",
            zone = "NA",
            colour = "black",
            combine = "no"
  )
  
)

