#!/usr/bin/Rscript

processSaatchi <- function(dir, method = "remapcon", plot = TRUE){
  
  
  ######## DATA AND METADATA PREPARATION - this will be somewhat dataset specific
  
  # look up the SourceInfo and the Quantity 
  source.info <- byIDfromList("Saatchi2011", supported.datasets)
  quantity <- lookupQuantity("vegC_std")
  quantity@units <- "kg m-2"
  layer <- "Tree"
  layer.name <- paste(layer, quantity@id, sep = ".")
  standard.name <- "vegetation_carbon_content"
  first.year <- 1995
  last.year <-  2005
  temporal.name <-  "Saatchi Period"
  temporal.id <-  "SaatchiPeriod"
  
  
    # read the original data and extent it to an even number of degrees 
  original.data <- raster(file.path(source.info@dir, "Saatchi2011.OriginalResolution.nc"))/10
  super.extent <- extent(c(xmin = -114, xmax = 156, ymin = -58, ymax = 40))
  extended.data <- extend(original.data, super.extent)
  
  # aggregate to an intermediate 10km resolution (still smaller than all the target resolutions here) for the Gaussian
  data.intermediate.for.gaussian <- aggregate(original.data, fact=10, fun = mean, expand = TRUE, na.rm = TRUE)
  names(data.intermediate.for.gaussian) <- layer.name
  
  # define the regular grids
  regular.grids <- list( 
    
    "QD" = list(res.code = "QD", agg.number = 30, round.dig = 3, res = 0.25),
    "HD" = list(res.code = "HD", agg.number = 60, round.dig = 2, res = 0.5),
    "1D" = list(res.code = "1D", agg.number = 120, round.dig = 1, res = 1),
    "2D" = list(res.code = "2D", agg.number = 240, round.dig = 0, res = 2)
    
  )
  
  # define the gaussian grids
  gaussian.grids <- list( 
    
    "T21" = list(res.code = "T21", res.str = "n16"),
    "T31" = list(res.code = "T31", res.str = "n24"),
    "T42" = list(res.code = "T42", res.str = "n32"),
    "T63" = list(res.code = "T63", res.str = "n48"),
    "T85" = list(res.code = "T85", res.str = "n64")
    
  )
  
  
  
  #########  STANDARD PREAMBLE
  
  # timer
  t1 <- Sys.time()
  
  # define a pdf and put the first plots in
  if(plot) {
    pdf(file = file.path(source.info@dir, paste(source.info@id, "pdf", sep = ".")))
    plot(original.data, main = "Original Data")
    plot(extended.data, main = "Extended Data")
  }
  
  ######## PROCESS REGULAR LON-LAT GRIDS
  
  for(grid in regular.grids) {
    
    # aggregate to the required resolution (and 'shoogle' the longitudes and latitude so that they line up with a standard grid)
    aggregated.raster <- aggregate(extended.data, grid$agg.number)
    if(plot) plot(aggregated.raster, main = grid$res.code)
    
    aggregated.dt <- as.data.table(raster::as.data.frame(aggregated.raster, xy = TRUE))
    setnames(aggregated.dt, c("Lon", "Lat", layer))
    if(grid$res.code != "QD") {
      aggregated.dt[, Lon := round(Lon,grid$round.dig)]
      aggregated.dt[, Lat := round(Lat,grid$round.dig)]
    }
    else {
      aggregated.dt[, Lon := round(Lon*80,0)/80]
      aggregated.dt[, Lat := round(Lat*80,0)/80]
    }
    
    
    # make the Field ID for saving this data as a Field
    field.id <-  makeFieldID(source.info = source.info,
                             var.string = quantity@id, 
                             spatial.resolution = grid$res.code, 
                             temporal.resolution = NULL, 
                             subannual.resolution = NULL, 
                             spatial.aggregate.method = NULL, 
                             temporal.aggregate.method = NULL, 
                             subannual.aggregate.method = NULL, 
                             temporal.extent.id = NUL, 
                             spatial.extent.id = NULL, 
                             subannual.original = NULL)  
    
    
    # make and save the Field object
    temp.field <- new("Field",
                      id = field.id,
                      data = aggregated.dt,
                      quant = quantity,
                      spatial.extent = NULL,
                      spatial.extent.id = "Full",
                      temporal.extent = NULL,
                      temporal.extent.id = "Full",
                      spatial.aggregate.method = "none",
                      temporal.aggregate.method = "none",
                      subannual.aggregate.method = "none",
                      subannual.original = "none",
                      source = source.info)
    
    saveRDS(temp.field, file = file.path(source.info@dir, paste0(field.id, ".DGVMData")))
    
    
    # also save as a netCDF (for some sort of extra flexibility I suppose)
    
    
    
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
    out.variable.name <-layer.name
    var.out <- ncvar_def(name = out.variable.name, 
                         units = quantity@units, 
                         dim = list(lon.dim, lat.dim), 
                         missval = -999999,
                         longname = standard.name)
    
    # CREATE FILE AND ADD THE VARIABLE
    outfile <- nc_create(file.path(source.info@dir, paste0(field.id, ".nc")), var.out, verbose=FALSE)
    ncvar_put(outfile, out.variable.name,  output.array, start=NA, count=NA, verbose=FALSE)
    print(paste("Saving variable", quantity@id, "to file",  file.path(source.info@dir, paste0(field.id, ".nc")), sep =" " ), quote=FALSE)
    
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
    
    # SET SPECIFIC LAYER/VARIABLE ATTRIBUTES
    ncatt_put(outfile, var.out, "DGVMTools_layer.name", layer)
    ncatt_put(outfile, var.out, "DGVMTools_quant",quantity@id)


    # SET GLOBAL ATTRIBUTES
    ncatt_put(outfile, 0, "Conventions", "CF-1.6")
    ncatt_put(outfile, 0, "DGVMTools_name", source.info@name)
    ncatt_put(outfile, 0, "DGVMTools_id", source.info@id)
    ncatt_put(outfile, 0, "DGVMTools_spatial.extent", "Full")
    ncatt_put(outfile, 0, "DGVMTools_temporal.extent.start", first.year)
    ncatt_put(outfile, 0, "DGVMTools_temporal.extent.end", last.year)
    ncatt_put(outfile, 0, "DGVMTools_temporal.extent.name", temporal.name)
    ncatt_put(outfile, 0, "DGVMTools_temporal.extent.id", temporal.id)

    
    
    # CLOSE
    nc_close(outfile)
   
    
  }
  
  
  ######## PROCESS GAUSSIAN GRIDS
  
  # loop for each resolution
  for(grid in gaussian.grids) {
    
    
    # make the Field ID for saving this data as a Field
    field.id <-  makeFieldID(source.info = source.info,
                             var.string = quantity@id, 
                             spatial.resolution = grid$res.code, 
                             temporal.resolution = NULL, 
                             subannual.resolution = NULL, 
                             spatial.aggregate.method = "mean", 
                             temporal.aggregate.method = NULL, 
                             subannual.aggregate.method = NULL, 
                             temporal.extent.id = NULL, 
                             spatial.extent.id = NULL, 
                             subannual.original = NULL)  
    
    temp.raster <- cdo(fun = method, 
                       ifile =  data.intermediate.for.gaussian,
                       ofile = file.path(source.info@dir, paste0(field.id, ".nc")),
                       grid = grid$res.str, 
                       return.raster = TRUE, 
                       verbose = TRUE, 
                       remove.temps = FALSE)
    
    if(plot) plot(temp.raster, main = grid$res.code)
    
    temp.nc <- nc_open( file.path(source.info@dir, paste0(field.id, ".nc")), write=TRUE)
    addStandardSpatialAttributes(temp.nc)
    ncatt_put(temp.nc, layer.name, "units", quantity@units)
    ncatt_put(temp.nc, layer.name, "long_name", standard.name)
    ncatt_put(temp.nc, layer.name, "standard_name", standard.name)
    nc_close(temp.nc)
    
  }
  
  ######## FINISH AND CLEAN UP 
  rm(original.data, extended.data)
  
  if(plot) dev.off()
    
  t2 <- Sys.time()
  print(t2-t1)
  
  
}