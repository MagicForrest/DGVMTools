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
#' @importMethodsFrom base as.data.frame
#' @importMethodsFrom data.table as.data.frame
#' @importMethodsFrom sp as.data.frame
#' @importMethodsFrom raster as.data.frame
#' 
#' 
#' 
NULL


###############    data.frame

#' @name export-methods
#' @export
setAs("Field", "data.frame", function(from) as.data.frame(from@data))


#' @rdname export-methods
#' @method as.data.frame Field
#' @export
as.data.frame.Field = function(x, row.names, optional, ...) as(x, "data.frame") 


#' @name export-methods
#' @export
setAs("ComparisonLayer", "data.frame", function(from) as.data.frame(from@data))


#' @rdname export-methods
#' @method as.data.frame ComparisonLayer
#' @export
as.data.frame.ComparisonLayer = function(x, row.names, optional, ...) as(x, "data.frame") 

#############  data.table

#' @name export-methods
#' @export
setAs("Field", "data.table", function(from) from@data)

#' @rdname export-methods
#' @export
as.data.table.Field = function(x, keep.rownames, ...) as(x, "data.table") 

#' @name export-methods
#' @export
setAs("ComparisonLayer", "data.table", function(from) from@data)

#' @rdname export-methods
#' @export
as.data.table.ComparisonLayer = function(x, keep.rownames, ...) as(x, "data.table") 


############# raster

#' @name export-methods
#' @export
setAs("Field", "Raster", function(from) promoteToRaster(from@data))

#' @name export-methods
#' @export
setAs("ComparisonLayer", "Raster", function(from) promoteToRaster(from@data))



#' @name as.Raster
#' @rdname export-methods
#' @export
#' @exportMethod as.Raster
#' Generic method for coercing to raster
setGeneric("as.Raster", function(x) {
  standardGeneric("as.Raster")
})

#' @rdname export-methods
#' @export
#' @exportMethod as.Raster
setMethod("as.Raster", signature("Field"),   function(x) promoteToRaster(x@data))

#' @rdname export-methods
#' @export
#' @exportMethod as.Raster
setMethod("as.Raster", signature("ComparisonLayer"),   function(x) promoteToRaster(x@data))


################################# PROMOTE TO RASTER
#
#' Prepares data as a raster object for plotting.
#' 
#' Converts a data.table, Field or an Spatial*-object to Raster object, also subsetting the requested layers.  
#' It can also handle a RasterLayber/Stack/Brick, in which case the only processing done is the subsetting since the data is already in Raster form.
#' This is generally called in the \code{plotSpatial} function (or potentially before any use of the raster::spplot and raster::plot functions),
#' but can be useful in and of itself.
#'   
#' @param input.data data.table, Field or Spatial*-object to be converted into a raster. Also takes a Raster*-object, in which case he 
#' @param layers The columns to be selected included in the final Raster* object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats,  when converting gridded table to a raster, in the case the the gridded data is not commatters for uneven
#' @param grid.topology A character string defining the grid topology when going from a table to raster, used in a call to SpatialPixels 
#' @return A RasterLayer (or RasterBrick)
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
promoteToRaster <- function(input.data, layers = "all", tolerance = 0.0000001, grid.topology = NULL){
  
  ###  Get class of the object we are dealing with
  this.class = class(input.data)[1]
  
  ###  Define the layers we are pulling out
  # for Field - note could define a methods "names" do cover this exception
  if((is.Field(input.data) || is.ComparisonLayer(input.data) ) & (is.null(layers) | layers[1] == "all")) {layers <- names(input.data@data)} 
  # for data.table or rasters
  else if(is.null(layers) | layers[1] == "all") {
    layers = names(input.data)
    
    # remove things we don't want to plot like "Lon" and "Lat"
    remove.list <- c("Lon", "Lat", "Year")
    for(remove in remove.list){
      if(remove %in% layers) {layers <- layers[-which(layers == remove)]}     
    }
    
  }
  
  
  ###  If SpatialPixelsDataFrame rasterise it directly
  if(this.class == "SpatialPixelsDataFrame"){ 
    print("If error check here: promoteToRaster in veg-runtools.R")
    data.raster <- raster::brick(input.data, layers)
  }
  ### If data.table or Field (which contains a data.table) 
  # could make this a little bit more efficient maybe...
  else if(this.class == "data.table" | is.Field(input.data) | is.ComparisonLayer(input.data)) {
    # first make a SpatialPointsDataFrame
    if(this.class == "data.table") {
      data.spdf <- makeSPDFfromDT(input.data, layers, tolerance, grid.topology = grid.topology)
    }
    
    if(is.Field(input.data) | is.ComparisonLayer(input.data)) {
      data.spdf <- makeSPDFfromDT(input.data@data, layers, tolerance, grid.topology = grid.topology)
    }
    
    
    # now convert to raster
    if(length(layers) == 1){
      data.raster <- raster::raster(data.spdf)
      rm(data.spdf)
    }
    else {
      data.raster <- raster::brick(data.spdf)
      rm(data.spdf)   
    }
    
  } 
  ###  If a single raster layer then we are done
  else if(this.class == "RasterLayer"){
    data.raster <- input.data    
  }
  ### If a stack or a brick, then subset 
  else if(this.class == "RasterBrick" | this.class == "RasterStack"){
    data.raster <- subset(input.data, layers)
    names(data.raster) <- layers
  }
  ### else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to promote object of type", class(input.data), "to Raster, which I don't know how to do.", sep = " "))
  }
  
  gc()
  return(data.raster)
  
}


##########################################################################################################
########### HELPER FUNCTIONS


################################# MAKE SPATIALPIXELSDATAFRAME FROM DATA.FRAME OR DATA.TABLE
#
#' Make SpatialPixelDataFrame from a data.table
#' 
#' Converts a data.table (or data.frame) to a SpatialPixelsDataFrame, using the columns "Lon and "Lat" to provide the spatial information.  
#' Mostly is called by \code{promoteToRaster}, but can be useful in and of itself.
#'
#' @param input.data data.table or data.frame, with columsn "Lon" and "Lat" which specify the spatial data 
#' @param layers The columns to be selected included in the final SpatialPixelsDataFrame object.  Use NULL or "all" if all layers are required.
#' @param tolerance Tolerance (in fraction of gridcell size) for unevenly spaced lon and lats
#' @param grid.topology A GridTopology defining the grid topology for the SpatialPixelsDataFrame object
#' @return A SpatialPixelDataFrame
#' @export
#' @import data.table sp
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
makeSPDFfromDT <- function(input.data, layers = "all",  tolerance = 0.0000001, grid.topology = NULL) {
  
  # to stop complaints at build time
  Lon = Lat = NULL
  
  # sort the layers
  if(is.null(layers) | layers[1] == "all") {layers = names(input.data)}
  
  # remove things we don't want to plot like "Lon" and "Lat"
  remove.list <- c("Lon", "Lat", "Year")
  for(remove in remove.list){
    if(remove %in% layers) {layers <- layers[-which(layers == remove)]}     
  }
  
  # alternate all columns
  if(FALSE) {
    all.cols <- unlist(list("Lon", "Lat", unlist(layers)))
    data.spdf <- data.frame(input.data[,all.cols,with=FALSE])
    coordinates(data.spdf) <- ~Lon+Lat
    gridded(data.spdf) <- TRUE
  }
  
  # convert to SPDF
  #sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
  sp.points <- SpatialPoints(data.frame(input.data[,list(Lon, Lat)]))
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance, grid = grid.topology)
  )
  suppressWarnings( # suppress the "grid has empty column/rows in dimension 1" warning
    data.spdf <- SpatialPixelsDataFrame(sp.pixels, input.data[,layers,with=FALSE], tolerance = tolerance)
  )
  # clean up
  rm(sp.points, sp.pixels)
  
  
  
  return(data.spdf)  
  
}

#' Array methods
#' 
#' Converts a \code{\linkS4class{Field}} to multi-dimensional array(s) all parameters are passed along to \code{\link{modelObject2Array}}.
#' 
#' @param x \code{\linkS4class{Field}}
#' @param ... Other arguments, not currently used
#' @return an lon/lat(/time) array - or a list of arrays - of the modelObjects input data.table.
#' @name Array-methods
#' @rdname Array-methods
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}         
setGeneric("as.array", function(x,...) standardGeneric("as.array"))

#' @rdname Array-methods
#' @aliases as.array
setMethod("as.array", signature("Field"), function(x, ...) {
  FieldToArray(x@data, ...)
})
