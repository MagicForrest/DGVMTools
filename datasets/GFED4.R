require(rgdal)
require(raster)
require(Cairo)

GFED4.scale.factor <- 0.01


readGFED4MonthlyData <- function(first.year = 1996, last.year = 2012, plot.data = FALSE, units = "fraction", return.period = "annual",  resolution = "QD", filename = NULL){
  
  
  BA.allyears <- brick()
  
  for(year in first.year:last.year){
    print(paste("GFED4: Reading Year", year, sep = " "))
    
    # because we only want the annual totals, use the monthly data
    for(month in 1:12){
      
      if(month < 10) {month.str <- paste0("0", month)}
      else{month.str <- paste0(month)}
      
      # FOR ANNUAL SUMS
      if(tolower(return.period) == "annual" | tolower(return.period) == "yearly") {
        if(month == 1) {
          BA <- raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Monthly/GFED4.0_MQ_", year, month.str, "_BA.hdf:0", sep = ""))      
        } 
        else {
          BA <- BA + raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Monthly/GFED4.0_MQ_", year, month.str, "_BA.hdf:0", sep = ""))      
        }
      }
      
      # TO KEEP MONTHLY VALUES
      else if(tolower(return.period) == "monthly" ) {
        if(month == 1) {
          BA <- stack(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Monthly/GFED4.0_MQ_", year, month.str, "_BA.hdf:0", sep = ""))      
        } 
        else {
          BA <- addLayer(BA, raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Monthly/GFED4.0_MQ_", year, month.str, "_BA.hdf:0", sep = "")))      
        }
      }
      
      # ELSE FAIL
      else {
        stop("Unknown time period to return GFED4")        
      }
      
    }
    
    # ADD TO THE STACK
    BA.allyears <- addLayer(BA.allyears, BA)
    
    rm(BA)
    gc()
    
  }
  
  
  # FILL IN THE METADATA
  xmin(BA.allyears) <- -180
  xmax(BA.allyears) <- 180
  ymin(BA.allyears) <- -90
  ymax(BA.allyears) <- 90
  projection(BA.allyears) <- "+proj=longlat +datum=WGS84"
  
  
  # AGREGATE IF NEED LOWER RESOLUTION
  if(resolution == "HD"){
    BA.allyears <- aggregate(BA.allyears, fact = 2, fun = sum)
  }
  else {
    if(resolution != "QD") stop(paste("Unknown resolution attempted - ", resolution, sep = ""))
  }
  
  
  # APPLY SCALE FACTOR
  BA.allyears <- BA.allyears * GFED4.scale.factor
  
  
  # CONVERT TO FRACTION IF REQUESTED
  extra.string = "_ha"
  if(units == "fraction"){
    # divide by gridcell areas (Note conversion km^2 to hectares)
    BA.allyears <- BA.allyears / ((area(BA.allyears)) * 100)
    extra.string = "Frac"
  }
  else if(units == "km_sq"){
    BA.allyears <- BA.allyears / 100
    extra.string = "_km_sq"     
  }
  else if(!(units == "ha")){
      warning(paste0("readGFED4MonthlyData: Unknown units ", units, ", returning ha"))  
  }
  
  # SET NAMES
  if(tolower(return.period) == "annual" | tolower(return.period) == "yearly") {
    names(BA.allyears) <- paste("BA", extra.string, "_", first.year:last.year, sep = "")
  }
  else if(tolower(return.period) == "monthly" ){
    names.vector <- vector()
    for(year in first.year:last.year){
      names.vector <- append(names.vector, paste("BA", extra.string, "_", year, "_", 1:12, sep = ""))  
    }
    names(BA.allyears) <- names.vector
  }
  
  if(!is.null(filename)){
    
    writeRaster(BA.allyears, filename, overwrite = TRUE)
    
  }
  
  
  return(BA.allyears)
  
  
}


readGFED4DailyData <- function(first.year = 2001, last.year = 2012, plot.data = FALSE, as.fraction = TRUE, return.period = "annual",  resolution = "QD", filename = NULL){
  
  
  BA.allyears <- brick()
  
  for(year in first.year:last.year){
    print(paste("Reading Year", year, sep = " "))
    
    # MAKE STRINGS FOR ACCESING FILES AND TAKE CARE OF LEAP YEAR
    if(year %% 4 == 0){
      day.strings <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:366))
      cum.days.in.month= c(31,60,91,121,152,182,213,244,274,305,335,366)
    } 
    else {
      day.strings <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:365))
      cum.days.in.month= c(31,59,90,120,151,181,212,243,273,304,334,365)
    }
    
    
    # FOR ANNUAL SUMS
    
    if(tolower(return.period) == "annual" | tolower(return.period) == "yearly") {
      for(day.str in day.strings){
        print(day.str)
        if(day.str == "001"){
          BA <- raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Daily/", year, "/GFED4.0_DQ_", year, day.str, "_BA.hdf:0", sep = ""))
        }
        else{
          BA <- BA + raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Daily/", year, "/GFED4.0_DQ_", year, day.str, "_BA.hdf:0", sep = ""))
        }
      }
    }
    
    
    # FOR DAILY READ INTO A BIG STACK
    else if(tolower(return.period) == "daily") {
      BA <- stack()
      for(day.str in day.strings){
        #print(day.str)
        BA <- addLayer(BA, raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Daily/", year, "/GFED4.0_DQ_", year, day.str, "_BA.hdf:0", sep = "")))
        
      }
    }
    
    
    # FOR MONTHLY SUMS
    else if(tolower(return.period) == "monthly") {
      BA <- stack()
      month.counter <- 1
      current.raster <- NULL
      for(day in 1:cum.days.in.month[length(cum.days.in.month)]){
        
        # if there is no raster read it in
        if(is.null(current.raster)){
          current.raster <- raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Daily/", year, "/GFED4.0_DQ_", year, day.strings[day], "_BA.hdf:0", sep = ""))
        }
        # else add it to the rsater for the month
        else {
          current.raster <- current.raster + raster(paste("HDF4_SDS:UNKNOWN:/data/forrest/Fire/GFED4/Daily/", year, "/GFED4.0_DQ_", year, day.strings[day], "_BA.hdf:0", sep = ""))
        }
        
        # if this was the last day of the month
        if(day == cum.days.in.month[month.counter]){
          # add the layer and name it
          BA <- addLayer(BA, current.raster)
          names(BA)[length(names(BA))] <- paste(year, months[month.counter], sep ="_")
          
          # remove the current raster ready for the next one and increment the month counter
          rm(current.raster)
          gc()
          current.raster <- NULL
          month.counter <- month.counter+1
        }
        
      }
    }
    
    # ELSE FAIL
    else {
      stop("Unknown time period to return GFED4")        
    }
    
    
    # ADD TO THE STACK
    BA.allyears <- addLayer(BA.allyears, BA)
    
    rm(BA)
    gc()
    
  }
  
  
  # FILL IN THE METADATA
  xmin(BA.allyears) <- -180
  xmax(BA.allyears) <- 180
  ymin(BA.allyears) <- -90
  ymax(BA.allyears) <- 90
  projection(BA.allyears) <- "+proj=longlat +datum=WGS84"
  
  
  # AGREGATE IF NEED LOWER RESOLUTION
  if(resolution == "HD"){
    BA.allyears <- aggregate(BA.allyears, fact = 2, fun = sum)
  }
  else {
    if(resolution != "QD") stop(paste("Unknown resolution attempted - ", resolution, sep = ""))
  }
  
  
  # APPLY SCALE FACTOR
  BA.allyears <- BA.allyears * GFED4.scale.factor
  
  
  # CONVERT TO FRACTION IF REQUESTED
  extra.string = ""
  if(as.fraction){
    # divide by gridcell areas (Note conversion km^2 to hectares)
    BA.allyears <- BA.allyears / ((area(BA.allyears)) * 100)
    extra.string = "Frac"
    
  } 
  
  
  # SET NAMES
  if(tolower(return.period) == "annual" | tolower(return.period) == "yearly") {
    names(BA.allyears) <- paste("BA", extra.string, "_", first.year:last.year, sep = "")
  }
  else if(tolower(return.period) == "monthly" ){
    names.vector <- vector()
    for(year in first.year:last.year){
      names.vector <- append(names.vector, paste("BA", extra.string, "_", year, "_", 1:12, sep = ""))  
    }
    names(BA.allyears) <- names.vector
  }
  else if(tolower(return.period) == "daily" ){
    names.vector <- vector()
    # MAKE STRINGS FOR ACCESING FILES AND TAKE CARE OF LEAP YEAR
    if(year %% 4 == 0){
      day.strings <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:366))
    } 
    else {
      day.strings <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:365))
    }
    for(year in first.year:last.year){
      names.vector <- append(names.vector, paste("BA", extra.string, "_", year, "_", day.strings, sep = ""))  
    }
    names(BA.allyears) <- names.vector
  }
  
  
  # WRITE FILE IF REQUESTED
  if(!is.null(filename)){
    writeRaster(BA.allyears, filename, overwrite = TRUE)
  }
  
  
  return(BA.allyears)
  
  
}


getGFED4Annual <- function(resolution = "HD", first.year = 1996, last.year = 2012, average = TRUE){
  
  
  if(resolution == "HD"){
    GFED4.data <- brick(file.path(benchmarking.data.dir, "GFED4/GFED4.Annual.1996-2012.HD.nc"))
    GFED4.data <- subset(GFED4.data, seq(first.year-1995,last.year-1995))
  }
  if(resolution == "T63"){
    GFED4.data <- rotate(brick(file.path(benchmarking.data.dir, "GFED4/GFED4.Annual.1996-2012.T63.nc")))
    GFED4.data <- subset(GFED4.data, seq(first.year-1995,last.year-1995))
  }
  
  if(average) GFED4.data <- calc(GFED4.data, mean)
  
  GFED4.dataset <- new("SpatialDataset",
                       id = "GFED4",
                       name = "GFED4 Burnt Area",
                       abbreviation = "GFED4",   
                       time.span = new("TimeSpan", name = "GFED4 Period", start = first.year, end = last.year),
                       data = GFED4.data,
                       veg.quant = getVegQuantity("burntarea"),
                       units = "fraction of gridcell"
  )
  
  
  rm(GFED4.data)
  
  return(GFED4.dataset)
  
  
  
}



benchmarkVsGFED4<-function(model.raster, model.name="LPJ-GUESS", run = run, doScatterPlots = TRUE, doHistoPlots = TRUE){
  
  dataset.id <- "GFED4"
  diff.breaks <- seq(-1, 1, 0.02)
  plot.breaks <- seq(-1, 1, 0.02)
  histo.plot.range <- c(-1,1)
  
  GFED4.period = new("TimeSpan", name = "GFED4", start = 2001, end = 208)                                                                                                                 
  original.data <-  getGFED4AnnualFast(first.year = 2001, last.year = 2008)
  original.data <- calc(original.data, mean)
  
  # crop all rasters to the same size
  common.extent <- intersect(model.raster, original.data)
  model.raster <- crop(model.raster, common.extent)
  data.raster <- crop(original.data, common.extent)
  
  
  # Do the generic analysis	 
  comparison.results <- compareTwoRasters(stack(model.raster, data.raster),  dataset.id, model.name, diff.breaks = diff.breaks, plot.breaks = plot.breaks,  histo.plot.range = histo.plot.range,  doScatterPlots=doScatterPlots, doHistoPlots=doHistoPlots, run = run, period = GFED4.period, quant = getVegQuantity("burntarea"))
  
  rm(original.data, common.extent, model.raster, data.raster)
  
  return(comparison.results)
  
}




getGFED4TS <- function(first.year = 1996, last.year = 2012, extents = new("SpatialExtent"), temporal.resolution = "annual", units = "ha"){
  
  list.of.TS.datasets <- list()
  unlist.after <- FALSE
  
  # if a single extent is requested, make a single into an extent for iterating over
  if(class(extents) != "list"){
    extents <- list(extents)
  }
    
  
  ### Annual 
  if(temporal.resolution == "annual"){
    # MF Quick and Dirty - take the annual file benchmarling data file and crop it, don't bother to read the original data
    # Actually this will be sound, it just doesn't help for monthly or daily time series
    GFED4.data <- brick(file.path(benchmarking.data.dir, "GFED4/GFED4.Annual.1996-2012.HD.nc"))
    
  
    # for each extent requested
    for(extent in extents){
      
      # crop
      GFED4.data.cropped <- cropLPJ(GFED4.data, extent)
      
      
      # calculate burnt area from burnt fraction, then sum, then covert to xts
      if(units == "ha"){
        GFED4.data.cropped <- GFED4.data.cropped * area(GFED4.data.cropped) * kmsq_to_ha
      }
      else if(units == "kmsq"){
        GFED4.data.cropped <- GFED4.data.cropped * area(GFED4.data.cropped)
      }
      
      # average across domain and make   
      GFED4.data.cropped.timeseries <- cellStats(GFED4.data.cropped, sum)
      GFED4.data.cropped.timeseries.xts <- xts(GFED4.data.cropped.timeseries , order.by = seq(as.Date(paste(first.year, "07-01", sep = "-")), as.Date(paste(last.year, "07-01", sep = "-")), by="years"))
      
      list.of.TS.datasets[[extent@id]] <- new("TemporalDataset",
                                              id = "GFED4",
                                              name = "GFED4 Burnt Area",
                                              abbreviation = "GFED4",   
                                              time.span = new("TimeSpan", name = "GFED4 Period", start = first.year, end = last.year),
                                              data = GFED4.data.cropped.timeseries.xts,
                                              veg.quant = getVegQuantity("burntarea"),
                                              extent = extent,
                                              units = units
      )
    }
  }
  
  ### Seasonal cycle 
  else if(temporal.resolution == "seasonal.cycle"){
    
    # read all the GFED4 monthly data
    GFED4.data <- readGFED4MonthlyData(first.year = first.year, last.year = last.year, plot.data = FALSE, units = "ha", return.period = "monthly",  resolution = "HD", filename = NULL)
      
    # for each extent requested
    for(extent in extents){
       
      # crop
      GFED4.data.cropped <- cropLPJ(GFED4.data, extent)
           
      # average years to get monthly seasoal cycle
      GFED4.seasonal.stack <- stack()
      for(month in months){
        GFED4.seasonal.stack <- addLayer(GFED4.seasonal.stack, calc(subset(GFED4.data.cropped, grep(paste0("_", month@index, "$"),names(GFED4.data.cropped))), fun = mean))
      }
             
      GFED4.seasonal.timeseries <- cellStats(GFED4.seasonal.stack, sum)
      
      list.of.TS.datasets[[extent@id]] <- new("TemporalDataset",
                                              id = "GFED4.SeasonalCycle",
                                              name = "GFED4 Burnt Area Seasonal Cyle",
                                              abbreviation = "GFED4",   
                                              time.span = new("TimeSpan", name = "GFED4 Period", start = first.year, end = last.year),
                                              data = GFED4.seasonal.timeseries,
                                              veg.quant = getVegQuantity("burntarea"),
                                              extent = extent,
                                              units = units
      )
    }
  }
  
  if(unlist.after) return(unlist(list.of.TS.datasets))
  else return(list.of.TS.datasets)
  
  
}







benchmarkVsGFED4<-function(model.raster, model.name="LPJ-GUESS", run = run, doScatterPlots = TRUE, doHistoPlots = TRUE){
  
  dataset.id <- "GFED4"
  diff.breaks <- seq(-1, 1, 0.02)
  plot.breaks <- seq(-1, 1, 0.02)
  histo.plot.range <- c(-1,1)
  
  GFED4.period = new("TimeSpan", name = "GFED4", start = 2001, end = 208)                                                                                                                 
  original.data <-  getGFED4AnnualFast(first.year = 2001, last.year = 2008)
  original.data <- calc(original.data, mean)
  
  # crop all rasters to the same size
  common.extent <- intersect(model.raster, original.data)
  model.raster <- crop(model.raster, common.extent)
  data.raster <- crop(original.data, common.extent)
  
  
  # Do the generic analysis   
  comparison.results <- compareTwoRasters(stack(model.raster, data.raster),  dataset.id, model.name, diff.breaks = diff.breaks, plot.breaks = plot.breaks,  histo.plot.range = histo.plot.range,  doScatterPlots=doScatterPlots, doHistoPlots=doHistoPlots, run = run, period = GFED4.period, quant = getVegQuantity("burntarea"))
  
  rm(original.data, common.extent, model.raster, data.raster)
  
  return(comparison.results)
  
}


# writeNetCDF(raster.in = all.GFED4, 
#             var.name = "burntFraction", 
#             var.units = "(fraction of gridcell burnt)", 
#             time.resolution = "annual", 
#             time.units.string = "year", 
#             long.name = "GFED4 burnt area fraction", 
#             filename = "/home/forrest/AuxiliaryData/BenchmarkingData/GFED4/GFED4.Annual.1996-2012.HD", 
#             ordering = "standard",
#             missing.value = -999999)


