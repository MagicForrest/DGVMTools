#' Select gridcells  
#' 
#' Select gridcells from a Field (or a data.table or a data.frame) using either gridcell coordinates or polgyons (from, for example, a shapefile).
#' 
#' @param x The Field, data.table or data.frame from which the gridcells should be selected.  Note that a data.table or data.frame 
#' should have columns "Lon" and "Lat" included.
#' @param gridcells The gridcells to be extracted.  These can be specified as one of the following:
#' \itemize{
#'  \item{A simple two-element numeric to pull out one gridcell (ordering is lon, lat)} 
#'  \item{A data.frame or data.table in which the first two columns are assumed to be longitude and latitude.}
#'  \item{An sp::SpatialPolygonsDataFrame.  By default points will be selected that lie under a polygon (feature), 
#'  but see the argument \code{cover.threshold} to select based on an overlap threshold.}
#'  \item{An sf:sf object.  As with the sp::SpatialPolygonsDataFrame above, by default points will be selected that lie under a polygon (feature), 
#'  but the argument \code{cover.threshold} allows selection based on an overlap threshold.  Note also that is very easy to get sf objects 
#'  representing countries from the rnaturalearth package, which, in combination with this function, provides a very easy way to subset by country.}
#'  \item{An object of type \code{map} as provided by the maps package \emph{provided they are created with fill = TRUE}.  Note the handy \code{region}
#'  argument to the maps::map function with allows one to pull out the data for one country.}
#' }
#' @param spatial.extent.id A character string to describe the gridcells selected.  When selecting gridcells from a Field you *must* specify 
#' a \code{spatial.extent.id} for meta-data consistency (you have free choice here, but avoid spaces).
#' @param tolerance A single numeric specifying how close a required gridcell in gridcells must be to one in \code{x}.  
#' Doesn't currently work, non-exact matching is not implemented!  Contact the author if this is a critical feature for you.
#' @param decimal.places A single numeric specifying how many decimal place to which the coordinates should rounded to facilitate a match.  
#' If not specified not rounding is done.
#' @param cover.threshold An optional numeric specifying what fraction of the gridcell must be covered by a feature in the case the \code{gricells} argument 
#' is a SpatialPolygonsDataFrame.  Note that is done using the \code{getCover} argument of \code{raster::rasterize()} which is only sensitive to about 1\% cover. 
#' fractions 
#' @param ... Further arguments.  Currently not used.
#' 
#' @return A Field, data.table or data.frame depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import maps maptools
#' @export

selectGridcells <- function(x, gridcells, spatial.extent.id = NULL, tolerance = NULL, decimal.places = NULL, cover.threshold = NULL, ...) {
  
  Lon = Lat = Dummy = SLM = layer_OBJECTID = NULL
  
  # deal with the class of x,
  isDataTable <- FALSE
  isDataFrame <- FALSE
  if(is.data.table(x)) {
    isDataTable <- TRUE
    dt <- x
  }
  if(is.data.frame(x)) {
    isDataFrame <- TRUE
    dt <- data.table(x)
  }
  else if(is.Field(x)) {
    if(missing(spatial.extent.id)) stop("When selecting gridcells from a DGVMTools object, please provide a spatial.extent.id when cropping a DGVMTools object to maintain meta-data integrity.")
    dt <- x@data
  }
  else {
    stop(paste("Can't extract gridcells from object of type", class(x)[1], "using getGridcells()", sep = " "))
  }
  
  # check that dt has both Lon and Lat columns
  all.dim.names <- getDimInfo(dt)
  if(!("Lon" %in% all.dim.names && "Lat" %in% all.dim.names)) stop("Argument x to function selectGridcells doesn't seem to have both Lon and Lat info")
  
  
  ### check class  of gridcells
  
  # if numeric
  if(is.numeric(gridcells)){
    if(length(gridcells)==2){
      selection.dt <- as.data.table(list("Lon" = gridcells[1], "Lat" = gridcells[2]))
    }
    else {
      stop("Got a numeric vector but it doesn't have two elements (ie first longitude/x then latitude/y), so I don't know to handle this to I am aborting.")
    }
  }
  # if got "SpatialPolygonsDataFrame" rasterise the SpatialPolygonsDataFrame and make a cell list
  else if(class(gridcells)[1] == "SpatialPolygonsDataFrame") {
    
    warning("The use of classes from the sp package with DGVMTools is SOFT DEPRECATED.  Please switch to using the sf package.")
    
    # first make a raster for the input field
    all.dims <- getDimInfo(x= dt, info = "full")
    all.unique.lonlats <- unique(all.dims[, c("Lon","Lat")])
    all.unique.lonlats[, Dummy := 1]
    x_grid <- promoteToRaster(all.unique.lonlats)
    x_grid <- raster::crop(x = x_grid, y = gridcells, snap = 'out')
    
    # if no threshold apply raster::rasterize() with getCover = FALSE and get a list of gridcells
    # note that this is a bit more code than just using the case below but it should be faster that with getCover == TRUE
    if(is.null(cover.threshold)) {
      
      gridcells_rasterised <- raster::rasterize(x = gridcells, y = x_grid)
      gridcells_dt <- as.data.table(raster::as.data.frame(gridcells_rasterised, xy = TRUE))
      setnames(gridcells_dt, c("x", "y"), c("Lon", "Lat"))
      selection.dt <- stats::na.omit(gridcells_dt)[, c("Lon", "Lat")]
      
    }
    
    # else rasterise with getCover = TRUE and apply the cover.threshold and get the list of gridcells
    else {
      
      gridcells_rasterised <- raster::rasterize(x = gridcells, y = x_grid, getCover = TRUE)
      gridcells_dt <- as.data.table(raster::as.data.frame(gridcells_rasterised, xy = TRUE))
      setnames(gridcells_dt, c("Lon", "Lat", "SLM"))
      gridcells_dt <- gridcells_dt[SLM >= cover.threshold, ]
      selection.dt <- stats::na.omit(gridcells_dt)[, c("Lon", "Lat")]
      
    }
    
  }
  # if it is map (not map should be made with fill = TRUE)
  else if(class(gridcells)[1] == "map") {
    
    # convert the map to SpatialPolygons
    IDs <- sapply(strsplit(gridcells$names, ":"), function(x) x[1])
    gridcells.polygons <- maptools::map2SpatialPolygons(gridcells, IDs=IDs, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
    
    # rasterise the input Field
    input.raster <- promoteToRaster(x)
    gridcell.raster <- raster::rasterize(gridcells.polygons, input.raster)
    gridcells_df <- raster::as.data.frame(gridcell.raster, xy = TRUE)
    names(gridcells_df) <- c("Lon", "Lat", "Dummy")
    selection.dt <- stats::na.omit(as.data.table(gridcells_df))[, c("Lon", "Lat")]
    
  }
  
  # if it is an sf
  else if("sf" %in% class(gridcells)) {
    
    # first make a SpatRast from the input object so that so that we can rasterise the sf object
    all.dims <- getDimInfo(x= dt, info = "full")
    all.unique.lonlats <- unique(all.dims[, c("Lon","Lat")])
    all.unique.lonlats[, Dummy := 1]
    x_grid <- terra::rast(all.unique.lonlats)
    
    # rasterise the sf to a SpatRast (with cover fraction if requested)
    cover_arg <- TRUE
    if(is.null(cover.threshold)) {
      cover_arg <- FALSE
      cover.threshold <- 0
    }
    gridcells_spatrast <- terra::rasterize(gridcells, x_grid, cover = cover_arg)
    
    # convert back to a data.table to produce the selection.dt
    gridcells_dt <- as.data.table(terra::as.data.frame(gridcells_spatrast, xy = TRUE))
    selection.dt <- gridcells_dt[layer >= cover.threshold,][, c("x", "y")]
    setnames(selection.dt, c("Lon", "Lat"))

  }
  
  else if(!(is.data.table(gridcells) || is.data.frame(gridcells))) {
    stop(paste("Arguments 'gridcells' not of correct type in call to getGridcells(), it should be a 'data.table', 'data.frame', 'SpatialPolygonsDataFrame' or a two-element numeric vector (Lon, Lat), got ", class(gridcells)[1], sep = " "))
  }
  else {
    if(ncol(gridcells) < 2) {
      stop("Argument 'gridcells' has insufficent columns, needs to be two (first lon/x, the lat/y)")
    }
    else {
      
      # first warn if not exactly two columns
      if(ncol(gridcells) > 2){
        warning(paste0("Argument 'gridcells' has too many columns (",ncol(gridcells), ")", "should be two (first lon/x, the lat/y). Assuming these are the firt two and tentatively proceeding"))
        
      }
      
      if(!is.data.table(gridcells)) { selection.dt <- as.data.table(copy(gridcells))[,c(1,2)] }
      else  { selection.dt <- copy(gridcells)[,c(1,2)] }
      setnames(selection.dt, c("Lon","Lat"))
    }
  }
  
  ######  ROUNDING FROM DECIMAL PLACES ARGUMENT (IF REQUESTED)
  
  if(!missing(decimal.places) && !is.null(decimal.places)){
    
    dt[,Lon := round(Lon, decimal.places)]
    dt[,Lat := round(Lat, decimal.places)]
    selection.dt[,Lon := round(Lon, decimal.places)]
    selection.dt[,Lat := round(Lat, decimal.places)]
    
  }
  
  
  ####### MATCHING: consider two cases: exact matching or matching within a tolerance
  
  # CASE 1: Exact matching
  if(missing(tolerance) || is.null(tolerance) || tolerance == 0.0) {
    final.dt <- dt[selection.dt, on = c(Lon = "Lon", Lat = "Lat"), nomatch=0]
  }
  
  # CASE 2: Inexact matching - much less efficient
  else if(is.numeric(tolerance) && length(tolerance) == 1) {
    stop("tolerance argument not implemented yet in selectGridcell(), sorry...")
    #final.dt <- dt[abs(dt[["Lon"]] - lon) < tolerance & abs(dt[["Lat"]]- lat) < tolerence,]
  }
  
  # OTHERWISE: badly specified tolerance
  else {
    stop("Poorly formed 'tolerance' parameter in getGridcells(), should just simply be a single numeric (or NULL or left missing).")
  }
  
  final.dt <- setKeyDGVM(final.dt)
  
  ####### RETURN
  
  if(isDataTable) {
    return(final.dt)
  }
  if(isDataFrame) {
    return(data.frame(final.dt))
  }
  else{
    x@data <- final.dt
    x@spatial.extent.id <- spatial.extent.id
    x@spatial.extent <- gridcells
    x@id <- makeFieldID(source = x@source,
                        quant.string = x@quant@id, 
                        sta.info = as(x, "STAInfo"))
    return(x)
  }
  
  
  
}
