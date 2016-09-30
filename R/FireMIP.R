


openFireMIPOutputFile <- function(run, var.string, quantity, temporal.extent = NULL, spatial.extent = NULL, verbose = TRUE) {
  
  # get the name of the model
  model.string <- gsub("-FireMIP", "", run@model)
  print(model.string)
  
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
      this.pfts <- c("C3G_pas", "C4G_pas", "BNE", "BINE", "BNS", "TeBS", "IBS", "TeBE", "TrBE", "TrIBE", "TrBR", "C3G", "C4G", "TeSW", "TeSWirr", "TeWW", "TeWWirr", "TeCo", "TeCOirr")
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
      this.pfts <- c("TrE", "TrD", "ExtE", "ExtD", "Rg_Shb", "De_Sh", "C3G", "C4G", "C3G_pas", "C4G_pas", "Crop")
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
    
    # 
    if(africa.centre) full.dt[,Lon := LondonCentre(Lon)]
    if(remove.total) full.dt[, Total := NULL]
    
   
    return(full.dt)

  }
  
  
  
  
}