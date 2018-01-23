#!/usr/bin/Rscript

#' Process Simard 2011 data
#' 
#' Reads the Simard biomass data at 1km and aggregates it to some standard resolutions.
#' 
#' @param input.dir The directory directory on disk where the original data input data is stored
#' @param output.dir The directory on disk where the aggragted data is to be stored (defaults to input.dir)
#' @param method Method by which to interpolate to the non-regular resolutions.  This should be a cdo "remapxxx" operator.  Default is "remapcon"
#' @param plot Logical, if TRUE make a .pdf book of all the datasets produced by the function for easy reference.

processSimard <- function(input.dir, output.dir = input.dir, method = "remapcon", plot = TRUE){
  
  print("Processing Simard 2011 Canopy Height")
  
  Lon = Lat = NULL
  
  ######## DATA AND METADATA PREPARATION - this will be somewhat dataset specific
  
  # the basic info about this here ting
  id = "Simard2011"
  name = "Simard et al. 2011 Canopy Height"
  quantity.id <- "canopyheight_std"
  quantity.units <- "m"
  layer.name <- "CanopyHeight"
  standard.name <- "canopy_height"
  first.year <- 2003
  last.year <-  2009
  
  
  # read the original data and extent it to an even number of degrees 
  original.data.west <- raster::raster(file.path(input.dir, "Simard2011.OriginalResolution.WesternHemisphere.nc"))
  original.data.east <- raster::raster(file.path(input.dir, "Simard2011.OriginalResolution.EasternHemisphere.nc"))
  original.data <- raster::merge(original.data.east, original.data.west)
  rm(original.data.east, original.data.west)
  
  
  # aggregate to an intermediate 10km resolution (still smaller than all the target resolutions here) for the Gaussian
  data.intermediate.for.gaussian <- raster::aggregate(original.data, fact=10, fun = mean, expand = TRUE, na.rm = TRUE)
  names(data.intermediate.for.gaussian) <- layer.name
  
  print("Read the original data")
  
  # define the regular grids
  grids <- list( 
    
    "QD" = list(res.code = "QD", type = "regular", agg.number = 30, round.dig = 3, res = 0.25),
    "HD" = list(res.code = "HD", type = "regular", agg.number = 60, round.dig = 2, res = 0.5),
    "1D" = list(res.code = "1D", type = "regular", agg.number = 120, round.dig = 1, res = 1),
    "2D" = list(res.code = "2D", type = "regular", agg.number = 240, round.dig = 0, res = 2),
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
      aggregated.raster <- raster::aggregate(original.data, grid$agg.number)
      if(plot) plot(aggregated.raster, main = grid$res.code)
      
      aggregated.dt <- as.data.table(raster::as.data.frame(aggregated.raster, xy = TRUE))
      setnames(aggregated.dt, c("Lon", "Lat", layer.name))
      if(grid$res.code != "QD") {
        aggregated.dt[, Lon := round(Lon,grid$round.dig)]
        aggregated.dt[, Lat := round(Lat,grid$round.dig)]
      }
      else {
        aggregated.dt[, Lon := round(Lon*80,0)/80]
        aggregated.dt[, Lat := round(Lat*80,0)/80]
      }
      
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
                         remove.temps = FALSE)

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
  rm(original.data)
  
  if(plot) grDevices::dev.off()
  
  t2 <- Sys.time()
  print(t2-t1)
  
  
}