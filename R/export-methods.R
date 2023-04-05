#' Coerce from Fields and Comparisons
#' 
#' Functions to coerce Fields and Comparisons into other objects (data.frame, data.table, Raster* objects)
#' 
#' Note that for coercing to a Raster* object (RasterLayer or RasterBrick) the function is called "as.Raster" (capital "R") to avoid conflict with  
#' another function in the raster package called "as.raster"
#'
#'
#' @param x A Field or a Comparison object
#' @param keep.rownames	If ... is a matrix or data.frame, TRUE will retain the rownames of that object in a column named rn.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column names (to syntactic names: see make.names) is optional. 
#' Note that all of R's base package as.data.frame() methods use optional only for column names treatment, 
#' basically with the meaning of data.frame(*, check.names = !optional).
#' @param ...	Just as ... in data.frame. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.
#' @name export-methods
#' @import methods
#' @include classes.R
#' 
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}, Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}    
#' 
NULL


############### data.frame - S3 objects so pretty simple (only S3 methods needed)

#' @name export-methods
setAs("Field", "data.frame", function(from) as.data.frame(from@data))


#' @rdname export-methods
#' @export
as.data.frame.Field = function(x, row.names, optional, ...) as(x, "data.frame") 


#' @name export-methods
setAs("Comparison", "data.frame", function(from) as.data.frame(from@data))


#' @rdname export-methods
#' @export
as.data.frame.Comparison = function(x, row.names, optional, ...) as(x, "data.frame") 


#############  data.table - S3 objects so pretty simple (only S3 methods needed)

#' @name export-methods
setAs("Field", "data.table", function(from) from@data)

#' @rdname export-methods
#' @export
as.data.table.Field = function(x, keep.rownames, ...) as(x, "data.table") 

#' @name export-methods
setAs("Comparison", "data.table", function(from) from@data)

#' @rdname export-methods
#' @export
as.data.table.Comparison = function(x, keep.rownames, ...) as(x, "data.table") 


############# Raster - need S3 and S4 methods 

### S3 methods

#' @name export-methods
setAs("Field", "Raster", function(from) {
  
  warning("Export of raster package objects is now soft deprecated and will be removed in a future release.  Please switch to terra.")
  
  field.as.raster = tryCatch({
    promoteToRaster(from@data)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a Raster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  return(field.as.raster)
  
})

#' @name export-methods
setAs("Comparison", "Raster", function(from) { 
  
  warning("Export of raster package objects is now soft deprecated and will be removed in a future release.  Please switch to terra.")
  
  field.as.raster = tryCatch({
    promoteToRaster(from@data)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Comparison to a Raster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  return(field.as.raster)
  
})


### S4 methods

#' Generic method for coercing to raster
#' @name as.Raster
#' @rdname export-methods
#' @exportMethod as.Raster
setGeneric("as.Raster", function(x) {
  standardGeneric("as.Raster")
})

#' @rdname export-methods
#' @export
setMethod("as.Raster", signature("Field"),   function(x) {
  
  warning("Export of raster package objects is now soft deprecated and will be removed in a future release.  Please switch to terra.")
  
  field.as.raster = tryCatch({
    promoteToRaster(x@data)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a Raster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  print("woopwoop")
  return(field.as.raster)
  
})


#' @rdname export-methods
#' @export
setMethod("as.Raster", signature("Comparison"),   function(x){ 
  
  warning("Export of raster package objects is now soft deprecated and will be removed in a future release.  Please switch to terra.")
  
  field.as.raster = tryCatch({
    promoteToRaster(x@data)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Comparison to a Raster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  return(field.as.raster)
  
})

######## SpatRaster and SpatRasterDataset - again S3 and S4 methods

### S3 methods

#' @name export-methods
setAs("Field", "SpatRaster", function(from) {
  
  field.as.Spat = tryCatch({
    terrarize(from)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a SpatRaster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  if(inherits(field.as.Spat, "SpatRaster")) return(field.as.Spat)
  else return(terra::rast(field.as.Spat))
  
})

#' @name export-methods
setAs("Field", "SpatRasterDataset", function(from) {
  
  field.as.Spat = tryCatch({
    terrarize(from)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a SpatRasterDataset object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  if(inherits(field.as.Spat, "SpatRasterDataset")) return(field.as.Spat)
  else return(terra::sds(field.as.Spat))
  
})


#' @name export-methods
setAs("Comparison", "SpatRasterDataset", function(from) { 
  
  Comparison.as.SpatRasterDataset = tryCatch({
    terrarize(from)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Comparison to a SpatRasterDataset object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  return(Comparison.as.SpatRasterDataset)
  
})

### S4

#' Generic method for coercing to SpatRaster
#' @name as.SpatRaster
#' @rdname export-methods
#' @exportMethod as.SpatRaster
setGeneric("as.SpatRaster", function(x) {
  standardGeneric("as.SpatRaster")
})


#' Generic method for coercing to SpatRasterDataset
#' @name as.SpatRasterDataset
#' @rdname export-methods
#' @exportMethod as.SpatRasterDataset
setGeneric("as.SpatRasterDataset", function(x) {
  standardGeneric("as.SpatRasterDataset")
})


#' @rdname export-methods
#' @export
setMethod("as.SpatRaster", signature("Field"),   function(x) {
  
  field.as.Spat = tryCatch({
    terrarize(x)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a SpatRaster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  if(inherits(field.as.Spat, "SpatRaster")) return(field.as.Spat)
  else return(terra::rast(field.as.Spat))
  
})

#' @rdname export-methods
#' @export
setMethod("as.SpatRasterDataset", signature("Field"),   function(x) {
  
  field.as.Spat = tryCatch({
    terrarize(x)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a SpatRasterDataset object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  if(inherits(field.as.Spat, "SpatRasterDataset")) return(field.as.Spat)
  else return(terra::sds(field.as.Spat))
  
})


#' @rdname export-methods
#' @export
setMethod("as.SpatRasterDataset", signature("Comparison"),   function(x){ 
  
  Comparison.as.SpatRasterDataset = tryCatch({
    terrarize(x)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Comparison to a SpatRasterDataset object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  return(Comparison.as.SpatRasterDataset)
  
})


#' Generic method for coercing to an array
#' 
#'     
#'       
#' @name as.array
#' @rdname export-methods
#' @exportMethod as.array
#' @return Either a single array or a list of arrays, depending on if the input Field had one layer or multiple layers 
setGeneric("as.array", function(x,...) standardGeneric("as.array"))

#' @rdname export-methods
#' @aliases as.array
#' @export
setMethod("as.array", signature("Field"), function(x, ...) {
  FieldToArray(x@data, ...)
})


#######################################################################################
####################### HELPER FUNCTIONS ##############################################
#######################################################################################


################################# PROMOTE TO RASTER
#
#' Convert to Raster
#' 
#' Converts a Field, Comparison or data.table to a Raster object, also subsetting the requested layers.  
#' This is generally called in the as.Raster and the setAs functions but can be useful in and of itself.
#'   
#' @param input.data Field, Comparison or data.table
#' @param layers The columns to be selected included in the final Raster* object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced Lon and Lats, 
#' when converting gridded table to a raster.
#' @param grid.topology A character string defining the grid topology when going from a table to raster, used in a call to SpatialPixels 
#' @return A RasterLayer (or RasterBrick)
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
promoteToRaster <- function(input.data, layers = "all", tolerance = 0.01, grid.topology = NULL){
  
  ###  Get class of the object we are dealing with
  this.class = class(input.data)[1]
  
  ###  Define the layers we are pulling out if not defined
  if(is.null(layers) | layers[1] == "all") {
    layers = layers(input.data)
  }
  
  ### If data.table or Field (which contains a data.table) 
  if(this.class == "data.table" | is.Field(input.data) | is.Comparison(input.data)) {
    
    # first make a SpatialPointsDataFrame
    if(this.class == "data.table") {
      data.spdf <- makeSPDFfromDT(input.data, layers, tolerance, grid.topology = grid.topology)
    }
    else if(is.Field(input.data) | is.Comparison(input.data)) {
      data.spdf <- makeSPDFfromDT(input.data@data, layers, tolerance, grid.topology = grid.topology)
    }
    
    # now convert to Raster Layer/Brick
    if(ncol(data.spdf@data) == 1){
      data.raster <- raster::raster(data.spdf)
      rm(data.spdf)
    }
    else {
      data.raster <- raster::brick(data.spdf)
      rm(data.spdf)   
    }
    
  } 
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to promote object of type", class(input.data), "to Raster, which I don't know how to do.", sep = " "))
  }
  
  gc()
  return(data.raster)
  
}


################################# MAKE SPATIALPIXELSDATAFRAME FROM DATA.FRAME OR DATA.TABLE
#
#' Make SpatialPixelDataFrame from a data.table
#' 
#' Converts a data.table (or data.frame) to a SpatialPixelsDataFrame, using the columns "Lon and "Lat" to provide the spatial information.  
#' Mostly is called by \code{promoteToRaster}, but can be useful in and of itself.
#'
#' @param input.data data.table or data.frame, with columns "Lon" and "Lat" which specify the spatial data 
#' @param layers The columns to be selected included in the final SpatialPixelsDataFrame object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats
#' @param grid.topology A GridTopology defining the grid topology for the SpatialPixelsDataFrame object
#' @return A SpatialPixelDataFrame
#' @export
#' @import data.table
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
makeSPDFfromDT <- function(input.data, layers = "all",  tolerance = 0.01, grid.topology = NULL) {
  
  # to stop complaints at build time
  Lon = Lat = NULL
  
  # sort the layers
  if(is.null(layers) | layers[1] == "all") {
    layers = layers(input.data)
  }
  
  # dcast if time dimensions are present
  if("Year" %in% names(input.data) || "Month" %in% names(input.data) || "Day" %in% names(input.data))  {
    input.data <- dcast(input.data, Lon+Lat~..., value.var = layers)
    # also update the layers
    layers = layers(input.data)
  }
  
  # convert to SPDF
  #sp.points <- sp::SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.points <- sp::SpatialPoints(data.frame(input.data[,list(Lon, Lat)]))
  
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    sp.pixels <- sp::SpatialPixels(sp.points, tolerance = tolerance, grid = grid.topology)
  )
  
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    data.spdf <- sp::SpatialPixelsDataFrame(sp.pixels, input.data[,layers,with=FALSE], tolerance = tolerance)
  )
  
  # clean up
  rm(sp.points, sp.pixels)
  
  return(data.spdf)  
  
}

#' Convert a Field to Arrays
#' 
#' Convert a Field (or the data.table from a Field) into a list of multi-deminsional arrays (indexed Lon, Lat and Time),
#' with one array in the list per Layer in the input Field. 
#' 
#' @param x the data.table of a \code{\linkS4class{Field}}
#' @param cname the column name to convert, if not set a list is returned
#' @param invertlat start in the north
#' @param add.missing.cols logical, if TRUE fill empty longitude columns (should be regularly spaced or you'll get silliness), default is TRUE
#' @param add.missing.rows logical, if TRUE fill empty latitude rows (should be regularly spaced or you'll get silliness), default is TRUE
#' @param global.extent logical, if TRUE extend the array to be the entire global extent,  default is FALSE
#' @param verbose print some information
#' @return an array or a list or arrays
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}, Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal


FieldToArray <- function(x, start.date = NULL, calendar = "365_day", add.missing.cols = TRUE, add.missing.rows = TRUE,  global.extent = FALSE, cname=FALSE, invertlat=FALSE, verbose=FALSE) {
  
  
  
  if(verbose) message("* Starting FieldToArray.")
  
  Lon=Lat=Year=Month=Day=Time=Date=variable=NULL
  
  if(verbose) message("** Copying input...")
  if(is.Field(x) || is.Comparison(x)) d <- copy(x@data)
  else  d <- copy(x)
  if(verbose) message("** ...done.")
  
  ## get the full spatial extent
  lon <- extract.seq(d$Lon, force.regular = add.missing.cols)
  lat <- extract.seq(d$Lat, force.regular = add.missing.rows, descending=invertlat)
  if (verbose) {
    message(paste0("Spatial extent: Lon: ", min(lon), " ", max(lon), " (", length(lon), ")\n", 
                   "                Lat: ", min(lat), " ", max(lat), " (", length(lat), ")"))
  }
  
  ## get temporal info
  st.names <- getDimInfo(d)
  
  ## check for time series (temporal data)
  is.temporal <- FALSE
  if("Year" %in% st.names  || "Month" %in% st.names  || "Day" %in% st.names)  is.temporal <- TRUE
  
  # if temporal, make a Time column and define the values for it in the fullgrid (see below)
  if(is.temporal) {
    
    
    # old way, label time axis based on codes
    # keeping this method to be keep the as.array method valid, writing netCDF should do the old way with a start date below
    # TODO - change the to give more useful codes since it doesn't need to be used for writing netCDF any more
    if(missing(start.date) || is.null(start.date)) {
      
      # if it is temporal (but no Year column) make a dummy year
      if(!("Year" %in% st.names)) d[, Year := 01]
      
      # if only annual temporal resolution (no monthly or daily column)
      if(!("Month" %in% st.names) && !("Day" %in% st.names)  ) {
        
        if (verbose)  message("'Year' column present.")
        time <- (sort(unique(d$Year)) * 1000) + 1
        d[, Time := Year * 1000 + 1]
        d[, Year := NULL]
        
        
      }
      ## check for monthly data
      else if("Month" %in% st.names) {
        
        # lookup vector to match month to day of year (ignore leap years and use the centre of the month)
        # this could be more sophisticated 
        lookup.DoY.vector <- c() 
        counter <- 0
        for(month in all.months) {
          lookup.DoY.vector <- append(lookup.DoY.vector, counter + floor(month@days/2))
          counter <- counter + month@days
        }
        
        # note that replacing the step below with some sort of paste command slows things down a lot, faaaar better to use a numeric here
        d[, Time:= Year * 1000 + lookup.DoY.vector[Month]]
        time <- sort(unique(d$Time))
        
        d[, Month := NULL]
        d[, Year := NULL]
      }
      # check if daily
      else if("Day" %in% st.names) {
        
        # note that replacing the step below with some sort of paste command slows things down a lot, faaaar better to use a numeric here
        d[, Time:= Year * 1000 + as.numeric(Day)]
        time <- sort(unique(d$Time))
        
        d[, Day := NULL]
        d[, Year := NULL]
        
      }
      
    }
    
    # here calculate the time dimension label as days since start.date 
    else {
      
      # initial year
      start.year <- as.numeric(format(start.date,"%Y"))
      start.day <- as.numeric(format(start.date,"%j"))
      start.day.offset <- start.day -1
      
      # define column number of years since start years
      if(calendar == "365_day") {
        
        # if daily data
        if("Day" %in% st.names){
          if("Year" %in% st.names) {
            d[, Time := ((Year - start.year) * 365) + Day - start.day.offset]
            d[, Year := NULL]
          } 
          else  {
            d[, Time := Day - start.day.offset]
          }
          d[, Day := NULL]
        }
        
        # if monthly data
        else if("Month" %in% st.names) {
          
          # build look up take for Day of year at centre of month    
          lookup.DoY.vector <- c() 
          counter <- 0
          for(month in all.months) {
            lookup.DoY.vector <- append(lookup.DoY.vector, counter + floor(month@days/2))
            counter <- counter + month@days
          }
          
          
          if("Year" %in% st.names) {
            d[, Time:= ((Year - start.year) * 365) + lookup.DoY.vector[Month] - start.day.offset]
            d[, Year := NULL]
          } 
          else{
            d[, Time:= lookup.DoY.vector[Month] - start.day.offset]
          }
          d[, Month := NULL]
          
        }
        
        # if only years
        else if("Year" %in% st.names) {
          d[, Time := ((Year - start.year) * 365) - start.day.offset]
          d[, Year := NULL]
        }
        
        
      }
      
      # standard calendar: actually quite easy since we can use standard R date manipulation
      else if(calendar == "standard") {
        
        # if daily data
        if("Day" %in% st.names){
          if("Year" %in% st.names) {
            d[, Date := as.Date(paste(Year, Day, sep = "-"), format = "%Y-%j")]
            d[, Year := NULL]
          } else {
            d[, Date := as.Date(paste(9999, Day, sep = "-"), format = "%Y-%j") ]
          }
        }
        # if monthly data
        else if("Month" %in% st.names) {
          
          # build look up take for Day of year at centre of month - not    
          lookup.CentreOfMonth.vector <- c() 
          for(month in all.months) {
            lookup.CentreOfMonth.vector <- append(lookup.CentreOfMonth.vector, floor(month@days/2))
          }
          
          if("Year" %in% st.names) {
            d[, Date := as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")] 
            d[, Year := NULL]
          } else {
            d[, Date := as.Date(paste(9999, Month, "01", sep = "-"), format = "%Y-%m-%d") ]
          } 
          d[, Month := NULL]
        }
        # if only annual
        else if("Year" %in% st.names) {
          d[, Date := as.Date(paste(Year, "01-01", sep = "-"), format = "%Y-%m-%d") ]
          d[, Year := NULL]
        }
        
        # now simple subtract start.data from date
        d[, Time := as.numeric(Date- as.Date(start.date))]
        d[, Date := NULL]
        
        
      }
      # proleptic_gregoriancalendar: not yet implemented here
      else if(calendar == "proleptic_gregorian") {
        stop("\"proleptic_gregorian\" calendar not yet implemented for FieldToArray")
      }
      
      
      # make axis values
      time <- sort(unique(d$Time))
      
    }
    
  }
  
  
  ## also set the appropriate keys
  if(verbose) message("** Setting keys...")
  setKeyDGVM(d)
  if(verbose) message("** ... done.")
  
  
  ## get the desired column name(s) if none was given
  if (missing(cname)) cname <- colnames(d)[!(colnames(d) %in% c("Lon", "Lat", "Time"))]
  
  
  
  ## add dummy NA values for any missing Lons or Lats
  lons.present <- unique(d[["Lon"]])
  lats.present <- unique(d[["Lat"]])
  
  
  if(global.extent) {
    if(verbose) message("** Extending lons/lats to global extent as requested...")
    
    # do latitude first as it is easier
    if(add.missing.rows) {
      lat.max <- 90
      lat.min <- -90
      lat.diff <- unique(lat[2:length(lat)] - lat[1:(length(lat)-1)])
      if(length(lat.diff) == 1) {
        # extend southwards
        while(min(lat) > lat.min) {
          new.lower.lat <- min(lat) - lat.diff
          if(new.lower.lat >= lat.min) lat <- append(new.lower.lat, lat)
          else break
        }
        # extend northwards
        while(max(lat) < lat.max) {
          new.upper.lat <- max(lat) + lat.diff
          if(new.upper.lat <= lat.max) lat <- append(lat, new.upper.lat)
          else break
        }
        
      }
      else {
        if(verbose) message(" **** CANNOT extent lats to global extent as requested, as latitude spacing is not regular!  Skipping this.")
        warning(" CANNOT extent lats to global extent as requested, as latitude spacing is not regular!  Skipping this.")
      }
    }
    else {
      if(verbose) message(" **** NOTE: I am NOT extending lats to global extent as requested because add.missing.rows is set to FALSE. Therefore skipping this.")
      warning(" NOTE: I am NOT extending lats to global extent as requested because add.missing.rows is set to FALSE. Therefore skipping this.")
    }
    
    
    
    # do longitude which is slightly trickier because they can be (-180,180) or (0, 360)
    if(add.missing.cols) {
      lon.diff <- unique(lon[2:length(lon)] - lon[1:(length(lon)-1)])
      
      # here check range
      lon.min <- -180
      lon.max <- 180
      if(max(lon) > lon.max) {
        lon.min <- 0
        lon.max <- 360
      }
      
      if(length(lon.diff) == 1) {
        # extend westwards
        while(min(lon) > lon.min) {
          new.lower.lon <- min(lon) - lon.diff
          if(new.lower.lon >= lon.min) lon <- append(new.lower.lon, lon)
          else break
        }
        # extend eastwards
        while(max(lon) < lon.max) {
          new.upper.lon <- max(lon) + lon.diff
          if(new.upper.lon <= lon.max) lon <- append(lon, new.upper.lon)
          else break
        }
        
      }
      else {
        if(verbose) message(" **** CANNOT extent lons to global extent as requested, as longitude spacing is not regular!  Skipping this.")
        warning(" CANNOT extent lons to global extent as requested, as longitude spacing is not regular!  Skipping this.")
      }
    }
    else {
      if(verbose) message(" **** NOTE: I am NOT extending lons to global extent as requested because add.missing.cols is set to FALSE. Therefore skipping this.")
      warning(" NOTE: I am NOT extending lons to global extent as requested because add.missing.cols is set to FALSE. Therefore skipping this.")
    }
    if(verbose) message("** ... done.")
    
  }  
  
  # here fill the missing lon/lats if requested
  
  # empty data.table for the extra lines 
  dummy.dt <- data.table()
  
  # missing longitude columns
  if(add.missing.cols) {
    if(verbose) message("** Checking for missing longitude columns to be filled in...")
    
    # for each longitude
    for(this_lon in lon) {
      # if a lon is missing add a dummy line for it
      if(!this_lon %in% lons.present) {
        
        if(verbose) message(paste(" **** Filling in missing lon column", this_lon))
        
        temp_newrow <- d[1,]
        temp_newrow[1, Lon := this_lon]
        for(this_layer in layers(temp_newrow)) {
          if(this_layer != "Time") temp_newrow[1, (this_layer) := NA]  
        }
        dummy.dt <- rbind(dummy.dt, temp_newrow)
        
      }
    }
  }
  else {
    if(verbose) message("** Not checking for missing longitude colums to be filled in.")
  }
  
  if(add.missing.rows) {
    if(verbose) message("** Checking for missing latitude rows to be filled in...")
    
    
    # for each latitude is missing for it
    for(this_lat in lat) {
      # if a lat is missing
      if(!this_lat %in% lats.present) {
        if(verbose) message(paste(" **** Filling in missing lat row", this_lat))
        temp_newrow <- d[1,]
        temp_newrow[1, Lat := this_lat]
        for(this_layer in layers(temp_newrow)) {
          if(this_layer != "Time") temp_newrow[1, (this_layer) := NA]  
        }
        dummy.dt <- rbind(dummy.dt, temp_newrow)
      }
    }
  }
  else {
    if(verbose) message("** Not checking for missing latitude rows to be filled in.")
  }
  
  # if there are missing lons/lats, add 'em  and set keys again
  if(nrow(dummy.dt > 0))  {
    
    d <- rbind(d, dummy.dt)
    
    # set keys on d again
    if(verbose) message("**** ... setting keys again ...")
    setKeyDGVM(d)
    if(verbose) message("** ... done.")
    
  } else {
    if(verbose) message("** ... no missing lons or lats found, done.")
  }
  
  
  
  
  
  
  ### NOTE:  At one point I tried doing with with a for loop instead of the lapply below in an attempt to save memory.  
  ###        It didn't help.  But note that I have remove the "full.grid" which was in Joerg's initial implementation.  
  ###        I think by setting "fill = NA" below one can avoid the need for it.
  if(verbose) message("** Producing array(s) from data.table (most time-consuming step) ...")
  t1  <- Sys.time()
  rv <- lapply(cname, function(x) {
    if(verbose) message(paste0(" **** Doing layer ", x))
    if (is.temporal) {
      # this produces an array with 3 dimensions corresponding to Lon, Lat and Time in the field and populates, 
      # leaving NA's where there is no data
      rv <- reshape2::acast(d, Lon ~ Lat ~ Time, value.var=x,  drop = !(add.missing.cols && add.missing.rows), fill = NA)
      if (invertlat)      rv <- rv[,length(lat):1,]
    } else {
      # as above but only two dimensions 
      rv <- reshape2::acast(d, Lon ~ Lat, value.var=x,  drop = !(add.missing.cols && add.missing.rows), fill = NA)
      if (invertlat)  rv <- rv[,length(lat):1]
    }
    return(rv)
  })
  t2<- Sys.time()
  if(verbose) {
    message("** ... done!  Time taken:")
    print(t2 -t1)
  }
  
  # tidy things up and return the array/list of arrays
  names(rv) <- cname
  rm(d);gc()
  return(rv)
  
}


makeTimeColumn <- function(x) {
  
  Year = DOM = Day = Month = Season = Time = NULL 
  
  dims_present <- getDimInfo(x)
  
  # handle year axis
  if(!"Year" %in% dims_present) x[, Year := "0000"]
  
  # handle subannual 
  if("Day" %in% dims_present) {
    x[, DOM := mday(Day)]
    x[, Month := month(Day)]
  }
  else if("Month" %in% dims_present) {
    x[, DOM := 15]
  }
  else if("Season" %in% dims_present) {
    # convert Season to Month (the center month of the season)
    x[, Month := as.integer(factor(Season, levels = c("DJF", "2", "3", "MAM", "5", "6", "JJA", "8", "9", "SON", "11", "12"), labels = paste(1:12)))]
    x[, DOM := 15]
  }
  else{
    x[, Month := 7]
    x[, DOM := 2]
  }
  
  # convert to IDate
  x[, Time := as.IDate(paste(Year, Month, DOM, sep = "-"))]
  
  # tidy by deleting extra columns
  x[, DOM := NULL]
  if(!"Year" %in% dims_present) x[, Year := NULL]
  if(!"Month" %in% dims_present) x[, Month := NULL]
  
  return(x)
  
}


terrarize <- function(x) {
  
  all_dims <- getDimInfo(x)
  this_dt <- copy(x@data)
  this_dt <- makeTimeColumn(this_dt)
  all_times <- sort(unique(this_dt[["Time"]]))
  for(this_time_var in c("Year", "Month", "Day", "Season")) {
    if(this_time_var %in% getDimInfo(this_dt)) this_dt[, (this_time_var) := NULL]
  }
  
  # for each layer make SpatRaster and save it to a list
  all_rst_list <- list()   
  for(this_layer in layers(x)) {
    
    # select the layer, reshape and make SpatRast (including times)
    this_layer_dt <- selectLayers(this_dt, c(this_layer, "Time"))
    this_layer_dt_reshaped <- data.table::dcast(this_layer_dt, Lon + Lat ~ Time, value.var = this_layer)
    this_rst <- terra::rast(this_layer_dt_reshaped)
    terra::time(this_rst) <- all_times
    
    # make and set appropriate layer names
    # year
    if("Year" %in% all_dims)  year_strings <- year(as.IDate(all_times))
    # subannual
    if("Month" %in% all_dims)  subannual_strings <- month(as.IDate(all_times))
    else if("Day" %in% all_dims)  subannual_strings <- yday(as.IDate(all_times))
    else if("Season" %in% all_dims)  subannual_strings <- c("DJF", "MAM", "JJA", "SON")[quarter(as.IDate(all_times))]
    # build and set names
    if(exists("year_strings") && exists("subannual_strings")) name_strings <- paste(year_strings, subannual_strings, sep = "_")
    else if(exists("year_strings"))  name_strings <- year_strings
    else if(exists("subannual_strings"))  name_strings <- subannual_strings
    else name_strings <- rep(this_layer, length(all_times))
    names(this_rst) <- name_strings
    all_rst_list[[this_layer]] <- this_rst
    
  }
  
  # return either a SpatRaster or SpatRasterDataset depending on the number of layers 
  if(length(all_rst_list) == 1) return(unlist(all_rst_list[[this_layer]]))
  else return(terra::sds(all_rst_list))
  
}
