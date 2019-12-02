#' Copy layers from one Field to another
#' 
#' This function allows layers (reminder, they are implemented as columns in a data.table) to be copied from one Field, Comaprison or data.table to another.  
#' This function is used extensively internally but can also be useful for user doing more advanced analysis and plotting. For example, is particularly useful for colouring or facetting a plot of one variable by another one.  To give a more concrete example, one could use a biome classification 
#' to split (facet) a data-vs-model scatter plot.
#' 
#' @param from The Field/Comparison/data.table that the layers are to be copied from.
#' @param to The Field/Comparison/data.table that the layers are to be copied to.
#' @param layer.names The layers to be copied from the "from" argument
#' @param new.layer.names The new names that the layers should have in the 'to' object. Use this to avoid naming conflict whereby, for 
#' example, if a layer "Total" is copied to an object which already has a "Total" layer then they layers will be names "Total.x" and "Total.y".  
#' @param keep.all.to Logical, if set to FALSE, all points in the 'to' object which don't have corresponding points in the 'from' object are removed.
#' @param keep.all.from Logical, if set to FALSE, all points in the 'from' object which don't have corresponding points in the 'to' object are removed.
#' @param tolerance Numeric, passed to copyLayers. Defines how close the longitudes and latitudes of the gridcells in \code{from} and \code{to}
#' need to be to the coordinates in order to get a match.  Can be a single numeric (for the same tolerance for both lon and lat) or a vector of two numerics (for lon and lat separately).
#' Default is no rounding (value is NULL) and so is fine for most regular spaced grids.  However, setting this can be useful to force matching of 
#' coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' @param fill.dims Logical, if TRUE (the default) and if the 'from' Field/data.table has less dimensions than the 'to' Field/data.table, then just fill in all the values
#' of that dimension with the same data.  Ie if if the 'from' table has no months but the to table does, just fill the same value into all months.   
#' 
#' @description This function does not check the dimensions columns are identical.  Any points in the 'from' object which are not in the 'to' object are ignored, 
#' and any points in the 'to' object which don't have corresponding points in the 'from' object are assigned NA, unless keep.all.to is set to FALSE .
#'
#' @return A Field, Comparison or data.table comprising the 'to' object with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
copyLayers <- function(from, to, layer.names, new.layer.names = NULL, keep.all.to = TRUE, keep.all.from = TRUE, tolerance = NULL, fill.dims = TRUE) {
  
  Lon = Lat = NULL
  
  # first some pre-amble and checks
  common.dims <- c()
  from.dims <- getDimInfo(from)
  to.dims <- getDimInfo(to)
  for(dim in c("Lon", "Lat", "Year", "Month", "Day", "Season")) {
    
    if(dim %in% from.dims && dim %in% to.dims) { common.dims <- append(common.dims, dim)}         
    else if(!dim %in% from.dims && dim %in% to.dims & !fill.dims) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"to\" argument but not the \"from\" argument (and fill.dims is not set to TRUE)") 
    }
    else if(dim %in% from.dims && !dim %in% to.dims) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"from\" argument but not the \"to\" argument") 
    }
    
  }
  # check tolerance argument for doing nearest gridcell matching
  do.nearest <- FALSE
  if(length(tolerance) == 1) {
    if(!identical(tolerance, 0)) do.nearest <- TRUE
    if(tolerance < 0) stop("Please supply a non-negative tolerance")
  }
  else if(length(tolerance) == 2) {
    for(this.tolerance in tolerance){
      if(!identical(this.tolerance, 0)) do.nearest <- TRUE
      if(this.tolerance < 0) stop("Please supply only non-negative tolerances")
    }
  }
  else if(!is.null(tolerance)) stop("Invalid tolerance.")
  
  
  # extract the data.tables
  if(is.Field(to) || is.Comparison(to)) { to.dt <- copy(to@data) }
  else if(is.data.table(to)) to.dt <- copy(to)
  else stop(paste("Cannot copy layers from object of type:", paste(class(to), collapse = " ")))
  if(is.Field(from) || is.Comparison(from)) layers.to.add.dt <- copy(from@data)
  else if(is.data.table(from)) layers.to.add.dt <- copy(from)
  else stop(paste("Cannot copy layers from object of type:", paste(class(from), collapse = " ")))
  
  # subset the layers to add
  layers.to.add.dt <- layers.to.add.dt[, append(common.dims, layer.names), with=FALSE]
  
  # if requested, fill missing dimensions in the data to be added - doesn't seem to be necessary
  # if(fill.dims){
  #   for(fill.dim in to.dims[which(!to.dims %in% from.dims)]) {
  #     print(fill.dim)
  #   }
  # }
  # set names if required
  if(!is.null(new.layer.names)) setnames(layers.to.add.dt, append(common.dims, new.layer.names))
  
  # handle the coordinate matching if required
  if(do.nearest) {
    
    if(length(tolerance) == 1) tolerance <- c(tolerance, tolerance)
    
    nearest <- function(x, matched.vector) {
      return(matched.vector[as.character(x)])
    }
   
    # get the unique dimensions values
    to.all.dim.values <- getDimInfo(to, "values")
    added.all.dim.values <- getDimInfo(layers.to.add.dt, "values")
    
    # build longitude look-up table
    lon.lookup.vector <- numeric()
    for(add.lon in added.all.dim.values[["Lon"]]) {
      closest <- to.all.dim.values[["Lon"]][which.min(abs(to.all.dim.values[["Lon"]]-add.lon))]
      if(abs(add.lon-closest) < tolerance[1] ) lon.lookup.vector <- append(lon.lookup.vector, closest)
      else lon.lookup.vector <- append(lon.lookup.vector, NA)
    }
    names(lon.lookup.vector) <- as.character(added.all.dim.values[["Lon"]])

    # build latitude look-up table
    lat.lookup.vector <- numeric()
    for(add.lat in added.all.dim.values[["Lat"]]) {
      closest <- to.all.dim.values[["Lat"]][which.min(abs(to.all.dim.values[["Lat"]]-add.lat))]
      if(abs(add.lat-closest) < tolerance[2] ) lat.lookup.vector <- append(lat.lookup.vector, closest)
      else lat.lookup.vector <- append(lat.lookup.vector, NA)
    }
    names(lat.lookup.vector) <- as.character(added.all.dim.values[["Lat"]])

    # do the look up
    layers.to.add.dt[, Lon := apply(.SD, 1, FUN = nearest, lon.lookup.vector), .SDcols = c("Lon")]
    layers.to.add.dt[, Lat := apply(.SD, 1, FUN = nearest, lat.lookup.vector), .SDcols = c("Lat")]
    
    # set key and merge
    setKeyDGVM(to.dt)
    setKeyDGVM(layers.to.add.dt)
    Temp.dt <- merge(x = to.dt, y = layers.to.add.dt,  all.y = keep.all.from, all.x = keep.all.to)
    
  }
  else {
    
    setKeyDGVM(to.dt)
    Temp.dt <- merge(x = to.dt, y = layers.to.add.dt, all.y = keep.all.from, all.x = keep.all.to)
    
  }
  
  # set keys
  setKeyDGVM(Temp.dt)
  
  # return of the relevant starting type
  if(is.data.table(to)) {
    return(setKeyDGVM(Temp.dt))
  }
  else {
    to@data <- setKeyDGVM(Temp.dt)
    return(to)
  }
  
}