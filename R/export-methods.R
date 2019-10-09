# Methods to coerce Fields and DataObjects to other R objects

#' Coerce from Fields and DataObjects
#' 
#' Functions to coerce Fields and DataObjects into other objects (data.frame, data.table, RasterBrick, SpatialPixelsDataFrame)
#' 
#' Note that for coercing to a Raster* object (RasterLayer or RasterBrick) the function is called "as.Raster" (capital "R") to avoid conflict with  
#' another function in the raster package called "as.raster"
#'
#'
#' @param x A Field or a DataObject
#' @param keep.rownames	If ... is a matrix or data.frame, TRUE will retain the rownames of that object in a column named rn.
#' @param row.names NULL or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional logical. If TRUE, setting row names and converting column names (to syntactic names: see make.names) is optional. 
#' Note that all of R's base package as.data.frame() methods use optional only for column names treatment, 
#' basically with the meaning of data.frame(*, check.names = !optional).
#' @param ...	Just as ... in data.frame. Usual recycling rules are applied to vectors of different lengths to create a list of equal length vectors.
#' @name export-methods
#' @import methods
#' 
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}, Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}    
#' 
NULL


###############    data.frame

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

#############  data.table

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


############# raster

#' @name export-methods
setAs("Field", "Raster", function(from) {
  
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
  
  field.as.raster = tryCatch({
     promoteToRaster(x@data)
  },  warning = function(w) {
    #warning(w)
  }, error = function(e) {
    stop("Can't convert the Field to a Raster object, probably because you have uneven coordinate spacings (perhaps a gaussian grid?) which exceed the default tolerance of 0.1.\n  To force on to an evenly spaced Raster grid try the promoteToRaster() function and specify the tolerance argument.")
  }, finally = {
  })
  
  return(field.as.raster)
  
})

#' @rdname export-methods
#' @export
setMethod("as.Raster", signature("Comparison"),   function(x){ 
  
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

#' Generic method for coercing to raster  
#' @name as.array
#' @rdname export-methods
#' @exportMethod as.array
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

#' Convert a Field to a multi-dimensional array
#' 
#' @param d the data.table of a \code{\linkS4class{Field}}
#' @param cname the column name to convert, if not set a list is returned
#' @param invertlat start in the north
#' @param verbose print some information
#' @return a array or a list or arrays
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @keywords internal
FieldToArray <- function(d, cname=FALSE, invertlat=FALSE, verbose=FALSE) {
  
  Lon=Lat=Year=Month=Day=variable=NULL
  
  ## get the full spatial extent
  lon <- extract.seq(d$Lon)
  lat <- extract.seq(d$Lat, descending=invertlat)
  if (verbose) {
    message(paste0("Spatial extent: Lon: ", min(lon), " ", max(lon), " (", length(lon), ")\n", 
                   "                Lat: ", min(lat), " ", max(lat), " (", length(lat), ")"))
  }
  
  ## get temporal info
  st.names <- getDimInfo(d)
  
  ## check for annual data
  is.temporal <- FALSE
  if("Year" %in% st.names && !("Month" %in% st.names) && !("Day" %in% st.names)  ) {
    if (verbose)
      message("'Year' column present.")
    time <- (sort(unique(d$Year)) * 1000) + 1
    is.temporal <- TRUE
  }
  ## check for monthly data
  else if("Year" %in% st.names && "Month" %in% st.names) {
    cname <- FALSE
    
    # lookup vector to match month to day of year (ignore leap years and use th centr of the month)
    # this could be more sophisticated 
    lookup.DoY.vector <- c() 
    counter <- 0
    for(month in all.months) {
      lookup.DoY.vector <- append(lookup.DoY.vector, counter + floor(month@days/2))
      counter <- counter + month@days
    }
    
    # note that replacing the step below with some sort of paste command slows things down a lot, faaaar better to use a numeric here
    if (is.temporal) {  d[, Year:= Year * 1000 + lookup.DoY.vector[Month]]  }
    time <- sort(unique(d$Year))
    
    d[, Month := NULL]
    is.temporal <- TRUE
  }
  # check if daily
  else if("Year" %in% st.names && "Day" %in% st.names) {
    cname <- FALSE
    
    # note that replacing the step below with some sort of paste command slows things down a lot, faaaar better to use a numeric here
    if (is.temporal) {  d[, Year:= Year * 1000 + as.numeric(Day)]  }
    time <- sort(unique(d$Year))
    
    d[, Day := NULL]
    is.temporal <- TRUE
    
  }
  
  #print(d)
  setKeyDGVM(d)
  
  ## create the target grid
  if (is.temporal) {
    full.grid <- data.table(Lon=rep(rep(lon, length(lat)), length(time)),
                            Lat=rep(rep(lat, each=length(lon)), length(time)),
                            Year=rep(time, each=length(lon) * length(lat)))                     
    setkey(full.grid, Lon, Lat, Year)
  } else {
    full.grid <- data.table(Lon=rep(lon, length(lat)),
                            Lat=rep(lat, each=length(lon)))
    setkey(full.grid, Lon, Lat)
  }
  
  ## get the desired column name(s) if none was given
  if (is.logical(cname))
    cname <- colnames(d)[!(colnames(d) %in% c("Lon", "Lat", "Year"))]
  
  ## create the array(s)
  rv <- lapply(cname, function(x) {
    if (is.temporal) {
      d <- d[full.grid]
      rv <- reshape2::acast(d, Lon ~ Lat ~ Year, value.var=x)
      if (invertlat)
        rv <- rv[,length(lat):1,]
    } else {
      d <- d[full.grid]
      rv <- reshape2::acast(d, Lon ~ Lat, value.var=cname)
      if (invertlat)
        rv <- rv[,length(lat):1]
    }
    return(rv)
  })
  if (length(rv) == 1)
    return(rv[[1]])
  
  names(rv) <- cname
  return(rv)
}
