#!/usr/bin/Rscript

#' Process Baccini 2012 data
#' 
#' Reads the Saatchi biomass data at 1km and aggregates it to some standard resolutions.
#' 
#' @param input.dir The directory directory on disk where the original data input data is stored
#' @param output.dir The directory on disk where the aggragted data is to be stored (defaults to input.dir)
#' @param method Method by which to interpolate to the non-regular resolutions.  This should be a cdo "remapxxx" operator.  Default is "remapcon"
#' @param plot Logical, if TRUE make a .pdf book of all the datasets produced by the function for easy reference.

processBaccini <- function(input.dir, output.dir = input.dir, method = "remapcon", plot = TRUE){
  
  print("Processing Baccini 2012 Vegetation Carbon")
  
  Lon = Lat = NULL
  
  ######## DATA AND METADATA PREPARATION - this will be somewhat dataset specific
  
  # the basic info about this here ting
  id = "Baccini"
  name = "Baccini et al. 2012 Vegetation Carbon"
  quantity.id <- "vegC_std"
  quantity.units <- "kg m-2"
  layer.name <- "Tree"
  standard.name <- "vegetation_carbon_content"
  first.year <- 2006
  last.year <-  2008
  
  # reda Baccini and London-centre the data
  Baccini.data <- read.table(file.path(input.dir, "Baccini_220912.txt"), header = TRUE, stringsAsFactors=FALSE)
  Baccini.data$Lon <- vapply(Baccini.data$Lon, 1, FUN = LondonCentre)    
  
  # add coordinates, grid it, promote to SPDF and finally convert to a raster
  coordinates(Baccini.data) = ~Lon+Lat
  gridded(Baccini.data) = TRUE
  Baccini.data = as(Baccini.data, "SpatialGridDataFrame") # to full grid
  Baccini.raster <- brick(Baccini.data)
  
  # subset to get the mean layer only
  original.data <- raster::subset(Baccini.raster, "MEAN")
  
  # convert from AGB to total
  original.data <- calc(original.data, AGBtoTotalCarbon)
  
  # divide by 10 to go from tC/Ha to kgC/m^2
  original.data <-original.data/10
  
   
  names(original.data) <- layer.name
  
  # extent the original to an even number of degrees 

  super.extent <- raster::extent(c(xmin = -112, xmax = 158, ymin = -24, ymax = 24))
  extended.data <- raster::extend(original.data, super.extent)
  
  # 0.5 degree raster for interpolating of Gaussina grids
  data.intermediate.for.gaussian <- original.data

  
  print("Read the original data")
  
  # define the regular grids
  grids <- list( 
    
    #"QD" = list(res.code = "QD", type = "regular", agg.number = 30, round.dig = 3, res = 0.25),
    "HD" = list(res.code = "HD", type = "regular", agg.number = 1, round.dig = 2, res = 0.5),
    "1D" = list(res.code = "1D", type = "regular", agg.number = 2, round.dig = 1, res = 1),
    "2D" = list(res.code = "2D", type = "regular", agg.number = 4, round.dig = 0, res = 2),
    "T21" = list(res.code = "T21", type = "gaussian", res.str = "n16"),
    "T31" = list(res.code = "T31", type = "gaussian", res.str = "n24"),
    "T42" = list(res.code = "T42", type = "gaussian", res.str = "n32"),
    "T63" = list(res.code = "T63", type = "gaussian", res.str = "n48"),
    "T85" = list(res.code = "T85", type = "gaussian", res.str = "n64")
    
  )
  
  
  
  #########  STANDARD PREAMBLE
  
  # timer
  t1 <- Sys.time()
  
  # define a pdf and put the first plots in
  if(plot) {
    grDevices::pdf(file = file.path(output.dir, paste(id, "pdf", sep = ".")))
    plot(original.data, main = "Original Data")
    plot(extended.data, main = "Extended Data")
  }
  
  
  
  ######## PROCESS REGULAR LON-LAT GRIDS
  
  for(grid in grids) {
    
    # make strings and if necessary the directory for storing the data
    this.run.id <- paste(id, grid$res.code, sep = "_")
    this.output.dir <- file.path(output.dir, this.run.id)
    this.file.nc <- file.path(this.output.dir, paste(this.run.id, quantity.id, "nc", sep = "."))
    if (!file.exists(this.output.dir)){
      dir.create(this.output.dir)
    }
    
    if(grid$type == "regular") {
      
      # aggregate to the required resolution (and 'shoogle' the longitudes and latitude so that they line up with a standard grid)
      if(grid$res.code != "HD") aggregated.raster <- raster::aggregate(extended.data, grid$agg.number)
      else aggregated.raster <- extended.data
      print(aggregated.raster)
      
      if(plot) plot(aggregated.raster, main = grid$res.code)
      
      aggregated.dt <- as.data.table(raster::as.data.frame(aggregated.raster, xy = TRUE))
      setnames(aggregated.dt, c("Lon", "Lat", layer.name))
      
      
      # save as a netCDF in 'DGVMData' format
      
      # MAKE LONGITUDE AND LATITUDE DIMENSION
      lon.list <- seq(from = min(aggregated.dt[["Lon"]]) , to = max(aggregated.dt[["Lon"]]),  by = grid$res)
      lon.dim <- ncdim_def("lon", "degrees", lon.list, unlim=FALSE, create_dimvar=TRUE)
      lat.list <- seq(from = min(aggregated.dt[["Lat"]]) , to = max(aggregated.dt[["Lat"]]),  by = grid$res) 
      lat.dim <- ncdim_def("lat", "degrees", lat.list, unlim=FALSE, create_dimvar=TRUE)
      
      # MAKE AN ARRAY OF THE DATA
      output.array <- drop(as.array(aggregated.raster))
      output.array <- aperm(output.array, c(2,1))
      output.array <- output.array[,dim(output.array)[2]:1]
      
      # MAKE THE ncvar OBJECT 
      var.out <- ncvar_def(name = layer.name, 
                           units = quantity.units, 
                           dim = list(lon.dim, lat.dim), 
                           missval = -999999,
                           longname = standard.name)
      
     
      # CREATE FILE AND ADD THE VARIABLE
      outfile <- nc_create(this.file.nc, var.out, verbose=FALSE)
      ncvar_put(outfile, var.out,  output.array, start=NA, count=NA, verbose=FALSE)
      print(paste("Saving variable", quantity.id, "to file",  this.file.nc, sep =" " ), quote=FALSE)
      
       
      addStandardSpatialAttributes <- function(nc.file) { 
        
        # ADD ATTRIBUTES
        ncatt_put(nc.file, "lon" , "units", "degrees_east")
        ncatt_put(nc.file, "lon" , "axis", "X")
        ncatt_put(nc.file, "lon" , "standard_name", "longitude")
        ncatt_put(nc.file, "lon" , "long_name", "longitude")
        
        ncatt_put(nc.file, "lat" , "units", "degrees_north")
        ncatt_put(nc.file, "lat" , "axis", "Y")
        ncatt_put(nc.file, "lat" , "standard_name", "latitude")
        ncatt_put(nc.file, "lat" , "long_name", "latitude")
        
        return(nc.file)
        
      }
      
      
      outfile <- addStandardSpatialAttributes(outfile)
      
       
      # 
      # if(length(dim(output.array)) == 3){
      #   ncatt_put(outfile, "Time" , "units", time.units.string)
      #   ncatt_put(outfile, "Time" , "axis", "T")
      #   ncatt_put(outfile, "Time" , "standard_name", "time")
      #   ncatt_put(outfile, "Time" , "long_name", "Time")
      #   ncatt_put(outfile, "Time" , "calendar", "365_day")
      # }
      
    }  # end if grid$type == "regular"

    else if(grid$type == "gaussian") {

      temp.raster <- cdo(fun = method,
                         ifile = data.intermediate.for.gaussian,
                         ofile = this.file.nc,
                         grid = grid$res.str,
                         return.raster = TRUE,
                         verbose = TRUE,
                         remove.temps = TRUE)

      if(plot) plot(temp.raster, main = grid$res.code)
      
      outfile<- nc_open(this.file.nc, write=TRUE)
      addStandardSpatialAttributes(outfile)

      ncatt_put(outfile, layer.name, "units", quantity.units)
      ncatt_put(outfile, layer.name, "long_name", standard.name)
      ncatt_put(outfile, layer.name, "standard_name", standard.name)

    } # end if grid$type == "gaussian"
    
  
    
    ### FOR BOTH TYPES OF GRIDS SET GLOBAL ATTRIBUTES AND CLOSE
    ncatt_put(outfile, 0, "Conventions", "CF-1.6")
    ncatt_put(outfile, 0, "DGVMData_quant", quantity.id)
    ncatt_put(outfile, 0, "DGVMData_name", name)
    ncatt_put(outfile, 0, "DGVMData_id", id)
    ncatt_put(outfile, 0, "DGVMData_spatial.extent", "Original")
    ncatt_put(outfile, 0, "DGVMData_first.year", first.year)
    ncatt_put(outfile, 0, "DGVMData_last.year", last.year)
    ncatt_put(outfile, 0, "DGVMData_year.aggregation.method", "mean")
    ncatt_put(outfile, 0, "DGVMData_quantity", quantity.id)
    nc_close(outfile)
    
  
    
  }
  
  
  ######## FINISH AND CLEAN UP 
  rm(original.data, extended.data)
  
  if(plot) grDevices::dev.off()
  
  t2 <- Sys.time()
  print(t2-t1)
  
  
}