
#' Read the original GFED4 files
#' 
#' Read the original GFED4 data files, and optionally aggregates/averages and crops the data in time and space.
#' 
#' @param location A character string specifying the the location of the original data files
#' @param list.of.spatial.extents A list of objects of type SpatialExtent, the funtion will return a dataset for each one.  
#' This is intended so that one can retrieve time series for particular regions, for example the GFED regions as used in the GFED publications. 
#' If not specified the function just returns the global data.
#' @param temporal.extent A TemporalExtent object defining the time period for the data, the default are the currently available full monthly GFED4 
#' years (1996-2013)
#' @param temporally.average Logical, whether or not to temporal average the data to return yearly, monthly or daily averages (depending on the \code{temporal.resolution} 
#' argument)
#' @param spatially.aggregate Logical, whether or not to sun the burned area across the spatial domain
#' @param spatial.resolution Character string defing the desired spatial resolution, can be "QD" (0.25 degree, the original resolution) or "HD" (0.5 degree)
#' @param temporal.resolution Character string defing the desired temporal resolution, can be "Annual", "Monthly" or "Daily"
#' @param output.files A vector of character strings for the output files.  MOVE TO A DIFFERENT FUNCTION
#' @param maxmemory Integer to specify the maximum memory to use raster processing.  If you have a lot of memory, consider increasing this so that calculations 
#' are al done in memory for better performance.
#' @param units A string specifying the units in which to return the data: can be "fraction" (fractional area burnt, weighted by gridcell area but no by sea-land mask), 
#' "ha" (hectares) or "kmsq" (square kilometers) 
#' @param archive  Logical, whether or not to write the data to disk. MOVE TO A DIFFERENT FUNCTION.
#' 
#' @return A raster (if spatial aggregating not requested) or a data.table (if spatially aggregating requested)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export


readGFED4 <-function(location = "/data/forrest/Fire/GFED4/",  
                     list.of.spatial.extents = NULL, 
                     temporal.extent = new("TemporalExtent",
                                           id = "GFED4",
                                           name = "Full GFED4 period",
                                           start = 1996,
                                           end = 2013), 
                     temporally.average = TRUE, 
                     spatially.aggregate = FALSE, 
                     spatial.resolution = "QD", 
                     temporal.resolution = "Annual", 
                     output.files = NULL, 
                     maxmemory = 1e+08, 
                     units = "fraction", 
                     archive = FALSE) {
  
  # metadata for this dataset
  GFED4.scale.factor <- 0.01
  datset.id.string <- "GFED4BA"
  rasterOptions(maxmemory = maxmemory)
  kmsq_to_ha <- 100
  ha_to_kmsq <- 0.01
  
  
  ######### PART ZERO: DEAL WITH THE GLOBAL CASE, SINGLETON DOMAIN CASE AND MAKE THE DATASET IDS
  
  # If no spatial extent is specified use a global extent
  if(is.null(list.of.spatial.extents)){
    list.of.spatial.extents <- new("SpatialExtent", id = "Global", name = "Global", extent = c(-180,180,-90,90))
  }
  
  
  singleton <- FALSE
  if(class(list.of.spatial.extents) == "SpatialExtent") {
    
    list.of.spatial.extents <- list(list.of.spatial.extents)
    singleton <- TRUE
  }
  
  # list of dataset ids.
  list.of.dataset.ids <- list()
  
  for(extent in list.of.spatial.extents){
    
    list.of.dataset.ids[[extent@id]] <- makeDatasetID(datset.id.string, 
                                                      units = units, 
                                                      temporal.resolution = temporal.resolution, 
                                                      spatial.resolution = spatial.resolution, 
                                                      temporal.extent = temporal.extent, 
                                                      spatial.extent = extent, 
                                                      temporally.averaged = temporally.average, 
                                                      spatially.averaged = spatially.aggregate)
    
   }
  
  
  ######### PART ONE: READ THE DATA
  
  # the final raster
  GFED4.total <- brick()
  
  for(year in temporal.extent@start:temporal.extent@end){
    print(year)
    # read each month into a temporary raster for the year
    BA.thisyear <- brick()
    
    # READ MONTHLY DATA (if need annual or month data then read the monthly files for speed)
    if(tolower(temporal.resolution) == "annual" || tolower(temporal.resolution) == "monthly") {
      
      for(month in months){
        print(month@id)
        BA.thisyear <- addLayer(BA.thisyear, raster(paste("HDF4_SDS:UNKNOWN:", location, "original/GFED4.0_MQ_", year, month@padded.index, "_BA.hdf:0", sep = "")))
        names(BA.thisyear) <- append(names(BA.thisyear)[1:length(names(BA.thisyear))-1], paste(datset.id.string, year, month@padded.index, sep = "_"))
        
      }
      
      # if annual make the sum for the year and name the layer
      if(tolower(temporal.resolution) == "annual") {
        
        BA.thisyear <- calc(BA.thisyear, sum)
        names(BA.thisyear) <- paste(datset.id.string, year, sep = "_")
        
      }
      
    }
    
    # READ DAILY DATA (if we need daily temporal resolution)
    else if(tolower(temporal.resolution) == "daily") {
      
      # MAKE STRINGS FOR ACCESING FILES AND TAKE CARE OF LEAP YEAR
      if(year %% 4 == 0){
        day.strings <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:366))
        cum.days.in.month= c(31,60,91,121,152,182,213,244,274,305,335,366)
      } 
      else {
        day.strings <- c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:365))
        cum.days.in.month= c(31,59,90,120,151,181,212,243,273,304,334,365)
      }
      
      
      for(day.str in day.strings){
        print(day.str)
        BA.thisyear <- addLayer(BA.thisyear, raster(paste("HDF4_SDS:UNKNOWN:", location, "original/", year, "/GFED4.0_DQ_", year, day.str, "_BA.hdf:0", sep = "")))
      }
      
    }
    
    # ELSE UNKNOWN TEMPORAL RESOLUTION (so fail, nicely)
    else {
      
      stop(paste("In readGFED4():: Unknown temporal resolution ", temporal.resolution, ", options are \"annual\", \"monthly\" or \"daily\".", sep = ""))
      
    }
    
    # conbine with other years in the big stack
    # if it is the first year then it defines the stack, can also set up the names
    if(year == temporal.extent@start)  {
      GFED4.total <- BA.thisyear
    }
    else {
      
      if(temporally.average) {
        GFED4.total <- GFED4.total + BA.thisyear
      }
      else {
        GFED4.total <- stack(GFED4.total, BA.thisyear)
      }
      
    }
    
    # clean up
    rm(BA.thisyear)
    gc()
    
  }  # for  each year
  
  # IF TEMPORALLY AVERAGING DIVIDE BY THE NUMBER OF YEARS
  if(temporally.average){
    GFED4.total <- GFED4.total / (temporal.extent@end-temporal.extent@start+1)
    # also set names
    if(tolower(temporal.resolution == "annual")) names(GFED4.total) <- paste(datset.id.string, temporal.resolution, paste("TA", temporal.extent@start, temporal.extent@end, sep = "."), sep = "_")
    if(tolower(temporal.resolution == "monthly")) names(GFED4.total) <- paste(datset.id.string, temporal.resolution, paste("TA", temporal.extent@start, temporal.extent@end, sep = "."), c(paste("0", 1:9, sep = ""), "10", "11", "12"), sep = "_")
    if(tolower(temporal.resolution == "daily")) names(GFED4.total) <- paste(datset.id.string, temporal.resolution, paste("TA", temporal.extent@start, temporal.extent@end, sep = "."), c(paste0("00", 1:9), paste0("0", 10:99), paste0(100:366)), sep = "_")
    
  }
  
  ########### PART TWO: SET THE METADATA, UNITS, ETC 
  
  # SET EXTENT AND PROJECT
  xmin(GFED4.total) <- -180
  xmax(GFED4.total) <- 180
  ymin(GFED4.total) <- -90
  ymax(GFED4.total) <- 90
  projection(GFED4.total) <- "+proj=longlat +datum=WGS84"
  
  store.names <- names(GFED4.total)
  
  # AGGREGATE IF REQUESTED AT COARSER RESOLUTION (but ignore if spatial averaging requested)
  if(!spatially.aggregate){
    if(spatial.resolution == "HD"){
      GFED4.total <- aggregate(GFED4.total, fact = 2, fun = sum)
    }
    else {
      if(spatial.resolution != "QD") stop(paste("Unknown resolution attempted - ", resolution, sep = ""))
    }
  }
  
  
  # APPLY SCALE FACTOR AND TRANSFER UNITS (if necessary)
  # note (according to the GFED4_readme.pdf) the units are hectares but with a scale factor of 0.01,
  if(units == "ha"){
    GFED4.total <- GFED4.total * GFED4.scale.factor
  }
  else if(units == "kmsq" | units == "km"){
    GFED4.total <- GFED4.total * (GFED4.scale.factor * ha_to_kmsq)
  }
  else if(units == "fraction"){
    GFED4.total <- GFED4.total / area(GFED4.total)  * (GFED4.scale.factor * ha_to_kmsq) 
  }
  else{
    stop(paste("In readGFED4():: Unknown units:", units, "specified. Valid options are \"ha\", \"kmsq\" or \"fraction\".", sep = ""))
  }
  
  # CALCULATE THE MIN/MAX
  GFED4.total <- setMinMax(GFED4.total)
  
  # SET THE NAMES AGAIN
  names(GFED4.total) <- store.names 
  
  
  ########## PART THREE: CROP TO THE REQUIRED EXTENTS AND SPATIALLY AVERAGE
  
  # list to return
  list.of.datasets <- list()
  
  for(extent in list.of.spatial.extents){
    
    # crop
    GFED4.data.cropped <- cropDGVM(GFED4.total, extent)
    
    print(names(GFED4.data.cropped))
    
    # IF SPATIALLY AGGREGATE RETURN A LIST OF TEMPORAL DATASET OBJECTS
    if(spatially.aggregate){
      
      # an absolute unit, just sum them up, nae borra
      store.names <- names(GFED4.data.cropped)
      if(units == "ha" || units == "kmsq") { 
        GFED4.data.cropped.timeseries <- cellStats(GFED4.data.cropped, sum)
      }
      # if fraction, weight by the gridcell areas
      else{
        GFED4.data.cropped.timeseries <- cellStats(GFED4.data.cropped * area(GFED4.data.cropped), sum)/ cellStats(area(GFED4.data.cropped), sum)
      }
      names(GFED4.data.cropped.timeseries) <- store.names 
      
      #GFED4.data.cropped.timeseries.xts <- xts(GFED4.data.cropped.timeseries , order.by = seq(as.Date(paste(temporal.extent@start, "07-01", sep = "-")), as.Date(paste(temporal.extent@end, "07-01", sep = "-")), by="years"))
      
      list.of.datasets[[extent@id]] <- new("TemporalDataset",
                                           id = "GFED4",
                                           name = "GFED4 Burnt Area",
                                           temporal.extent = temporal.extent,
                                           data = GFED4.data.cropped.timeseries,
                                           veg.quant = byIDfromList("burntarea", lpj.quantities),
                                           extent = extent,
                                           units = units)  
      
      
    } # if not spatially aggregating
    else {
      
      list.of.datasets[[extent@id]] <- new("SpatialDataset",
                                           id = "GFED4",
                                           name = "GFED4 Burnt Area",
                                           temporal.extent = temporal.extent,
                                           data = GFED4.data.cropped,
                                           veg.quant = byIDfromList("burntarea", lpj.quantities),
                                           units = units)  
      
      
      if(archive){
        
        writeNetCDF(raster.in = GFED4.data.cropped, 
                    var.name = "burntArea", 
                    var.units = units, 
                    time.resolution = temporal.resolution, 
                    time.units.string = "year", 
                    long.name = "GFED4 burnt area", 
                    filename = paste(location, "processed", paste(list.of.dataset.ids[[extent@id]], "nc", sep = "."), sep = "/"),
                    ordering = "standard",
                    missing.value = -999999)
        
        
      }
      
      
      
    } # if not spatially aggregating
    
    
  } # for each extent
  
  
  # archive on disk for using later
  
  if(singleton) {
    return(list.of.datasets[[1]])
  }
  else return(list.of.datasets)
  
  
}


