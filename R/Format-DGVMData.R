#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE DGVMData FILES ###########################################################
############################################################################################################################


#' Get a Field for DGVMData
#' 
#' 
#' An internal function that reads data from an DGVMData .nc file.  
#' 
#' @param source A \code{Source} containing the meta-data about the DGVMData source
#' @param quant A Quantity object to specify what quantity should be opened. 
#' @param target.sta.info An STAInfo object defining the spatial-temporal-annual extent over which we want the data
#' @param last.year The last year (as a numeric) of the data to be returned
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tablle containing the data, and secondly the STAInfo for the data that we have
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_DGVMData <- function(source,
                              quant,
                              target.STAInfo,
                              verbose = FALSE) {
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to read DGVMData files.")
  
  
  # To avoid annoying NOTES when R CMD check-ing
  Lon = Lat = Year = Month = Time = NULL
  
  # get first and last year for use later on
  first.year = target.STAInfo@first.year
  last.year = target.STAInfo@last.year
  
  # function read attributes and warn if they are not present.
  getGlobalAttribute <- function(attr.name, global.attributes) {
    dgvm.attr.name <- paste("DGVMData", attr.name, sep = "_")
    if(dgvm.attr.name %in% names(global.attributes)) {
      return(global.attributes[[dgvm.attr.name]]) 
    }
    else {
      warning(paste0("No ", attr.name,  " attribute (", dgvm.attr.name, " ) found in file ", file.name.nc, ".  This is not DGVMData compliant.  Right now setting attribute to NULL, but this might cause problems later"))
      return(NULL)
    }
  }
  
  
  # STAInfo object describing the data
  sta.info = new("STAInfo")
  
  
  # Make the filename and check for the file, gunzip if necessary, fail if not present
  file.name.nc <- file.path(source@dir, paste(quant@id, "nc", sep = "."))
  file.name.nc.gz <- paste(file.name.nc, "gz", sep = ".")
  zipped <- FALSE
  if(file.exists(file.name.nc)){ 
    if(verbose) message(paste("Found and opening file", file.name.nc, sep = " "))
  }
  else if(file.exists(file.name.nc.gz)){
    zipped <- TRUE
    R.utils::gunzip(file.name.nc.gz)
    if(verbose) message(paste("Found, gunzipping and opening file", file.name.nc.gz, sep = " "))
  }
  
  # Open file and get global attributes
  if(verbose) message(paste0("Opening file ", file.name.nc))     
  if(verbose) this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
  else this.nc <- invisible(ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE ))
  global.attributes <- ncdf4::ncatt_get(this.nc, 0, attname=NA, verbose=FALSE)
  
  # Check out dimensions and ignore bnds dimension if present
  dims.present <- names(this.nc$dim)
  if(verbose) message(paste("File has dimensions", paste(dims.present, collapse = ","), sep = " "))
  dims.present <- dims.present[which(dims.present != "bnds")]
  
  all.lats <- numeric(0)
  all.lons <- numeric(0)
  all.times <- numeric(0)
  lat.string <- ""
  lon.string <- ""
  time.string <- ""
  for(this.dimension in dims.present) {
    
    matched <- FALSE
    
    # pick up Lat
    for(possible.dim.name in c("lat", "Lat", "latitude", "Latitude")) {
      if(this.dimension == possible.dim.name) {
        lat.string <- possible.dim.name
        all.lats <- ncdf4::ncvar_get(this.nc, lat.string) 
        matched <- TRUE
      }
    }
    
    # pick up Lon
    for(possible.dim.name in c("lon", "Lon", "longitude", "Longitude")) {
      if(this.dimension == possible.dim.name) {
        lon.string <- possible.dim.name
        all.lons <-  ncdf4::ncvar_get(this.nc, lon.string) 
        matched <- TRUE
      }
    }
    
    # pick up Time
    for(possible.dim.name in c("time", "Time")) {
      if(this.dimension == possible.dim.name) {
        time.string <- possible.dim.name
        all.times <- ncdf4::ncvar_get(this.nc, time.string)
        matched <- TRUE
      }
    }                   
    
    # Catch the rest
    if(!matched) {
      stop(paste("Unknown dimension found", this.dimension, "which I don't know what to do with."))
    }
    
  }
  
  
  # old south order, new northern horizon
  
  # first look up year-related attributes
  first.year.present  <- getGlobalAttribute("first.year", global.attributes)
  last.year.present  <- getGlobalAttribute("last.year", global.attributes)
  year.aggregate.method <- getGlobalAttribute("year.aggregate.method", global.attributes)
  
  
  # next let's determine some details about the time axis
  if(length(all.times) > 0) { 
    time.units.attr <- ncdf4::ncatt_get(this.nc, time.string, "units")
    print(all.times)
    
    time.units.string.list <- strsplit(time.units.attr$value, " ") # Extract string components of the time unit
    tmp.unit <- unlist(time.units.string.list[1]) # Isolate the unit .i.e. seconds, hours, days etc.
    timestep.unit <- tmp.unit[which(tmp.unit %in% c("seconds","hours","days"))[1]] # Check unit
    if(timestep.unit != "days") stop('Currently DGVMData only accepts data with a units of time axis = "days"')
    print(timestep.unit)
    start.date <- as.POSIXct(gsub(paste(timestep.unit,'since '),'',time.units.attr$value),format="%Y-%m-%d %H:%M:%S") ## Extract the start / origin date
    start.date.year <- as.numeric(format(start.date,"%Y"))
    start.date.month <- as.numeric(format(start.date,"%m"))
    start.date.day <- as.numeric(format(start.date,"%d"))
    
    # look at the time steps
    mean.timestep.differences <- mean(diff(all.times))
    # if the mean is exactly unity then the data is daily 
    if(mean.timestep.differences == 1 ) time.res <- "daily"
    # if the mean is between 28 and 31 (inclusive) assume monthly   
    else if(mean.timestep.differences >= 28 & mean.timestep.differences <= 31 ) time.res <- "monthly"
    # if the mean is between 359 and 367 (inclusive) assume yearly  
    else if(mean.timestep.differences >= 360 & mean.timestep.differences <= 367 ) time.res <- "yearly"       
    else stop("Data doesn't appear to be daily, monthly, or yearly.  Other options are not currently supported by the DGVMData Format, but could potentially be.  So if you need this please contact the author.")
    
    # calculate the time labels and the start and count values for the time axis
    
    # default values if no time selection is required
    time.start <- 1
    time.count <- -1
    
    if(time.res == "yearly") {
      
      # convert the netcdf time increments into years
      all.years <- start.date.year:(start.date.year+length(all.times)-1)
      
      # check for cropping at this stage
      if(length(target.STAInfo@first.year) > 0 & length(target.STAInfo@last.year) > 0) {
        target.first.year <- target.STAInfo@first.year
        target.last.year <-  target.STAInfo@last.year
        if(target.first.year != all.years[1] | target.first.year != all.years[length(all.years)]) {
          
          if(target.first.year < all.years[1]) stop(paste("In DGVMData requested first.year = ", target.first.year, ", but first year in data is", all.years[1], sep = " "))
          if(target.last.year > all.years[length(all.years)]) stop(paste("In DGVMData requested last.year = ", target.last.year, ", but last year in data is", all.years[length(all.years)], sep = " "))
          
          time.start <- target.first.year - all.years[1] + 1
          time.count <- target.last.year - target.last.year
          
        }
      }
      all.times <- all.years
    }
    
    else if(time.res == "monthly") {
      
      # convert the netcdf time increments into years and months
      
      # first make a vector of the years and months
      nyears.to.cover <- ceiling(length(all.times) / 12)
      years.to.cover <- 0:(nyears.to.cover-1) + start.date.year
      years.vector <- rep(years.to.cover, each = 12)
      months.vector <- rep(c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), times = nyears.to.cover)
      years.vector <- years.vector[start.date.month:length(all.times)]
      months.vector <- months.vector[start.date.month:length(all.times)]
      
      # check for cropping at this stage
      if(length(target.STAInfo@first.year) > 0 & length(target.STAInfo@last.year) > 0) {
        target.first.year <- target.STAInfo@first.year
        target.last.year <-  target.STAInfo@last.year
        if(target.first.year != years.vector[1] | target.first.year != years.vector[length(years.vector)]) {
          
          if(target.first.year < years.vector[1]) stop(paste("In DGVMData requested first.year = ", target.first.year, ", but first year in data is", years.vector[1], sep = " "))
          if(target.last.year > years.vector[length(years.vector)]) stop(paste("In DGVMData requested last.year = ", target.last.year, ", but last year in data is", years.vector[length(years.vector)], sep = " "))
          
          
          # find the first occurence of the target first.year and the last occurance of the target year in the year.vector
          first.index <- match(target.first.year, years.vector)
          last.index <-  match(target.last.year, rev(years.vector))
          last.index <- length(years.vector) - last.index +1
          
          # make the indices
          time.start <- first.index
          time.count <- last.index - first.index + 1
          
          # crop the vectors to match and make labels
          years.vector <- years.vector[first.index:last.index]
          months.vector <- months.vector[first.index:last.index]
          
        }
      }
      
      # make a numeric code of Year.Month
      all.codes <- paste(years.vector, months.vector, sep = ".")
      print(all.codes)
      all.times <- all.codes
      
    }
    
  }
  
  
  if(!is.null(first.year.present)) sta.info@first.year <- first.year
  if(!is.null(last.year)) sta.info@last.year <- last.year
  if(!is.null(year.aggregate.method)) sta.info@year.aggregate.method <- year.aggregate.method
  
  
  
  dt.list <- list()
  
  
  # set start and count appropriately
  start <- numeric()
  count <- numeric()
  for(this.dim in dims.present){
    
    if(this.dim == lat.string) {
      # Here potentially have code allowing cropping in space giving different start and count values
      # but right now we just take the whole spatial extent
      start <- append(start, 1)
      count <- append(count, -1)
    }
    
    else if(this.dim == lon.string) {
      # Here potentially have code allowing cropping in space giving different start and count values
      # but right now we just take the whole spatial extent
      start <- append(start, 1)
      count <- append(count, -1)
    }
    
    if(this.dim == time.string) {
      print(all.times)
      print(first.year)
      print(last.year)
      
      start <- append(start, time.start)
      count <- append(count, time.count)
    }
    
  }
  
  print(start)
  print(count)
  
  
  # set up the dimension list for naming the dimensions
  dimension.names <- list()
  if(length(all.lons) > 0) dimension.names[["Lon"]] <- all.lons
  if(length(all.lats) > 0) dimension.names[["Lat"]] <- all.lats
  if(length(all.times) > 0) dimension.names[["Time"]] <- all.times
  
  for(this.var in this.nc$var) {
    
    # ignore the "time_bnds" variable that CDO creates with time aggregation
    if(this.var$name != "time_bnds") {
      
      print(this.var$name)
      
      # Get the actual data and set the dimension names    
      this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = start, count = count, verbose = verbose, collapse_degen=FALSE)
      dimnames(this.slice) <- dimension.names
      
      # prepare data.table from the slice (array)
      this.slice.dt <- as.data.table(melt(this.slice))
      rm(this.slice)
      gc()
      this.slice.dt <- stats::na.omit(this.slice.dt)
      setnames(this.slice.dt, "value", this.var$name)
      
      
      print(this.slice.dt)
      
      # ###  HANDLE TIME AXIS 
      if(length(all.times) > 0) {
        
        if(time.res == "yearly") {
          this.slice.dt[, Year := as.numeric(Time)]
          this.slice.dt[, Time := NULL]
          sta.info@subannual.resolution <- "Year"
          sta.info@subannual.original <- "Year"
        }
        else if(time.res == "monthly") {
          toYearandMonth <- function(x) {
            return(list("Year" = as.integer(trunc(x)), "Month" = as.integer(((x%%1)*100)+1)))
          }
          this.slice.dt[, c("Year", "Month") := toYearandMonth(as.numeric(Time))]
          this.slice.dt[, Time := NULL]
          sta.info@subannual.resolution <- "Month"
          sta.info@subannual.original <- "Month"
        }
        #   
        #   # some tricks to determine exactly what the 'Time' axis represent 
        #   
        #   # don't need to be so fancy here!
        #   # time.axis.string <- this.nc$dim$Time$units
        #   
        #   
        #   # actually, lets do some shortcuts
        #   length.time.axis <- length(all.times)
        #   
        #   # lets see if we have annual data
        #   if(length.time.axis == (last.year - first.year + 1)) {
        #     if(verbose) message("Got annual data.")
        #     
        #     
        #     # subsitute using first.year and last.year   
        #     this.slice.dt[, Year := plyr::mapvalues(this.slice.dt[["Time"]], from = all.times, to = first.year:last.year)]
        #     this.slice.dt[, Time := NULL]
        #     
        #     sta.info@subannual.resolution <- "Year"
        #     sta.info@subannual.original <- "Year"
        #   }
        #   # else check for monthly
        #   if(length.time.axis == (last.year - first.year + 1) *12) {
        #     if(verbose) message("Got monthly data.")
        #     
        #     all.years <- first.year:last.year
        #     
        #     ### MF: This might be slow, consider optimising
        #     
        #     # sort data.table by Time axis
        #     this.slice.dt[order(Time)] 
        #     
        #     # make Year vector and add it do the data.table
        #     temp.nentries.per.year <- nrow(this.slice.dt)/length(all.years)
        #     year.vector <- c()
        #     
        #     for(year in all.years) {
        #       year.vector <- append(year.vector, rep.int(year, temp.nentries.per.year))
        #     }
        #     this.slice.dt[, Year := year.vector]
        #     rm(year.vector)
        #     
        #     # Make Month vector
        #     month.vector <- c()
        #     for(year in all.years) {
        #       for(month in 1:12) {
        #         month.vector <-append(month.vector, rep.int(month, temp.nentries.per.year/12))
        #       }
        #     }
        #     this.slice.dt[, Month := as.integer(month.vector)]
        #     rm(month.vector)
        #     
        #     # Remove the Time columns
        #     this.slice.dt[, Time := NULL]
        #     
        #   
        #     sta.info@subannual.resolution <- "Month"
        #     sta.info@subannual.original <- "Month"
        #   }
        #   
        #   
        # }
        # else {
        #   sta.info@subannual.resolution <- "Year"
        #   sta.info@subannual.original <- "Year"
        
        
        # make new colum order so that the quant is last
        all.names <- names(this.slice.dt)
        all.names <- all.names[-which(all.names == this.var$name)]
        all.names <- append(all.names, this.var$name)
        setcolorder(this.slice.dt, all.names)
        
      }
      
      
      
      setKeyDGVM(this.slice.dt)
      print(this.slice.dt)
      print(str(this.slice.dt))
      
      
      if(length(quant@units) > 1) {
        
        # # remove categories which aren't present
        categories.present <- unique(this.slice.dt[[this.var$name]])
        all.categories <- quant@units
        
        this.slice.dt[,this.var$name := factor(this.slice.dt[[this.var$name]], labels = all.categories[sort(categories.present)])]
      }
      
      # now join this to all.dt
      dt.list[[length(dt.list)+1]] <- this.slice.dt
      
      
    } # end if statement for ignoring "time_bnds" variable
    
  }
  
  
  # join all together and set key
  dt <- dt.list[[1]]
  
  
  if(length(dt.list) > 1) {
    for(this.dt in dt.list[2:length(dt.list)]) {
      dt <- merge(x = dt, y = this.dt)
    }
  }
  
  rm(dt.list)
  gc()
  
  if(verbose) message("Setting key")
  dt <- setKeyDGVM(dt)
  
  # STInfo
  st.info <- getDimInfo(dt)
  
  
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
  
  
  ### DETERMINE SPATIAL EXTENT 
  
  # simple ones
  spatial.extent.id  <- getGlobalAttribute("spatial.extent.id", global.attributes)
  if(!is.null(spatial.extent.id)) { sta.info@spatial.extent.id <- spatial.extent.id  }
  else { sta.info@spatial.extent.id <- "Full"  }
  spatial.aggregate.method  <- getGlobalAttribute("spatial.aggregate.method", global.attributes)
  if(!is.null(spatial.aggregate.method)) sta.info@spatial.aggregate.method <- spatial.aggregate.method
  else { sta.info@spatial.aggregate.method <- "none"  }
  
  # first attempt to use the attributes from the NetCDF file
  xmin  <- getGlobalAttribute("xmin", global.attributes)
  xmax  <- getGlobalAttribute("xmax", global.attributes)
  ymin  <- getGlobalAttribute("ymin", global.attributes)
  ymax  <- getGlobalAttribute("ymax", global.attributes)
  # else attempt to lookm of from Lon and Lat columns
  if(!is.null(xmin) & !is.null(xmax) & !is.null(ymin) & !is.null(ymax)){
    sta.info@spatial.extent <- raster::extent(xmin, xmax, ymin,ymax)
    if(verbose) message("Setting spatial extent from DGVMData attributes")
  }
  else if("Lon" %in% st.info && "Lat" %in% st.info) {
    sta.info@spatial.extent <- extent(dt)
    if(verbose) message("Setting spatial extent from Lon and Lat dimensions")
  }
  else{
    if(verbose) message(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    warning(paste("No spatial extent determinable from DGVMData", file.name.nc,"file. A filler extent will be set."))
    sta.info@spatial.extent <- numeric(0)
  }
  
  
  
  # if london.centre is requested, shift to -180 to +180
  if(length(all.lons) > 0) {
    if(source@london.centre  && max(all.lons) >= 180){ dt[, Lon := vapply(dt[,Lon], 1, FUN = LondonCentre)] }
  }
  
  # set keys
  setKeyDGVM(dt)
  
  # remove any NAs, complete list and return
  dt <- stats::na.omit(dt)
  
  # close the netcdf file and if necessary zip again
  ncdf4::nc_close(this.nc)
  if(zipped) {
    R.utils::gzip(file.name.nc)
  }
  
  # make the ID and then make and return Field
  field.id <- makeFieldID(source = source, var.string = quant@id, sta.info = sta.info)
  
  return(
    
    new("Field",
        id = field.id,
        data = dt,
        quant = quant,
        source = source,
        sta.info 
    )
    
  )
  
}





######################### LIST ALL LPJ-GUESS OUTPUT VARIABLES (STORED AS *.out FILES) IN AN source DIRECTORY  #####################################################################
#' List all LPJ-GUESS *.out files in a source directory
#'
#' Simply lists all LPJ-GUESS output variables (stored as .out files) available in a directory. 
#' Also ignores some common red herrings like "guess.out" and "*.out" 
#' 
#' @param source A \code{Source} containing the meta-data about the DGVMData source
#' @param names A boolean, if TRUE return a character vector of names of available quantities, if FALSE return a list of the actual Quantities.
#' @return A list of all the .out files present, with the ".out" removed. 
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


availableQuantities_DGVMData <- function(source, names){
  
  # First get the list of *.out files present
  files.present <- list.files(source@dir, "*.nc")
  
  if(!names) quantities.present <- list()
  else quantities.present <- c()
  for(file in files.present) {
    
    # check if file contains paste(".", source@id, ".nc")
    # and if so, get the part before it
    this.quantity <- "lsls" # TODO
    quantities.present <- append(quantities.present, this.quantity)
    
    
  }
  
  return(quantities.present)
  
}


#' Detemine PFTs present in an DGVMData source 
#' 
#' @param x  A Source objects describing a DGVMData source
#' @param variables Some variable to look for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.  Currently ignored.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_DGVMData <- function(x, variables) {
  
  warning("Datasets don't normally have PFTs (or they are not likely to be defined consistently) so I am not looking for them.  Instead I am returning the source@format@pft.set argument directly (in case you defined some yourself, which would be the way to go in this case)")
  return(x@format@pft.set)
  
}


########################################################
########### DGVMData QUANTITIES ########################
########################################################


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' @include colour-palettes.R
#' 
#' 
DGVMData.quantities <- list(
  
  
  new("Quantity",
      id = "fraction",
      name = "Fraction",
      units = "",
      colours = grDevices::colorRampPalette(c("grey85", "black")), 
      format = c("DGVMData"),
      cf.name = "area_fraction"), 
  
  new("Quantity",
      id = "vegcover_std",
      name = "Area Fraction",
      units = "%",
      colours = veg.palette, 
      format = c("DGVMData"),
      cf.name = "area_fraction_percent"), 
  
  new("Quantity",
      id = "vegC_std",
      name = "Vegetation Carbon Mass",
      units = "kg m-2",
      colours = reversed.viridis,
      format = c("DGVMData"),
      cf.name = "vegetation_carbon_content"),
  
  new("Quantity",
      id = "LAI_std",
      name = "LAI",
      units = "1",
      colours = viridis::viridis,
      format = c("DGVMData"),
      cf.name = "leaf_area_index"),
  
  new("Quantity",
      id = "mGPP_std",
      name = "Monthly GPP",
      units = "kgC/m^2",
      colours = fields::tim.colors,
      format = c("Standard")),
  
  new("Quantity",
      id = "aGPP_std",
      name = "Annual GPP",
      units = "kgC m-2 year-1",
      colours = fields::tim.colors,
      format = c("DGVMData"),
      cf.name = "annual_gross_primary_productivity_of_biomass_expressed_as_carbon"),
  
  new("Quantity",
      id = "aNPP_std",
      name = "Annual NPP",
      units = "kgC/m^2/year",
      colours = fields::tim.colors,
      format = c("DGVMData")),
  
  new("Quantity",
      id = "canopyheight_std",
      name = "Canopy Height",
      units = "m",
      colours = reversed.magma,
      format = c("DGVMData"),
      cf.name = "canopy_height"),
  
  new("Quantity",
      id = "burntfraction_std",
      name = "Annual Fraction Burned",
      units = "fraction of gridcell",
      colours = reversed.fire.palette,
      format = c("DGVMData"),
      cf.name = "burned_area_fraction"),
  
  new("Quantity",
      id = "FPAR_std",
      name = "Fraction absorbed of Photosynthetically Active Radiation",
      units = "fraction",
      colours = veg.palette,
      format = c("DGVMData"),
      cf.name = "fraction_absorbed_of_photosynthetically_active_radiation"),
  
  new("Quantity",
      id = "aNEE_std",
      name = "Annual land sink (NEE)",
      units = "GtC/year",
      colours = veg.palette,
      format = c("DGVMData"))
  
)


####################################################
########### DGVMDATA FORMAT ########################
####################################################


#' @description \code{DGVMData} - a format defined here to unify the multitude of different datasets into a common format.  It is essentially CF-compliant netCDF with a couple of extra attributes defined. 
#' Can be produced using the companion DGVMData package. 
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @include colour-palettes.R
#' @export
DGVMData <- new("Format",
                
                # UNIQUE ID
                id = "DGVMData",
                
                # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
                determinePFTs = determinePFTs_DGVMData,
                
                # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
                availableQuantities = availableQuantities_DGVMData,
                
                # FUNCTION TO READ A FIELD 
                getField = getField_DGVMData,
                
                # DEFAULT GLOBAL PFTS  
                default.pfts = list(),
                
                # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
                quantities = DGVMData.quantities
                
)

