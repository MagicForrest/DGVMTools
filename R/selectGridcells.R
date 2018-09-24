#' Select gridcells from a Data/Field
#' 
#' 
#' @param x The Field, DataObject, data.table or data.frame from which the gridcells should be selected.  Note that a data.table or data.frame 
#' should have columns "Lon" and "Lat" included.
#' @param gridcells The gridcells to be extracted.  Can either be a simple two-element numeric to pull out one gridcell (ordering is lon, lat), or it can be a data.frame or data.table
#' in which the first two columns are assumed to be longitude and latitude.
#' @param tolerance A single numeric specifying how close a required gridcell in gridcells must be to one in x.  Doesn't currently work, non-exact matching is not implemented.
#' @param spatial.extent.id A character string to describe the gridcells selected, this is required for meta-data consistency.
#' @param decimal.places A single numeric specifying how many decimal place to which the coordinates should rounded to facilitate a match.  
#' 
#' @return A Field, DataObject, data.table or data.frame depending on the type of the input x.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

selectGridcells <- function(x, gridcells, tolerance = NULL, spatial.extent.id = NULL, decimal.places = NULL) {
  
  Lon = Lat = NULL
  
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
  
  # if 
  else if(!(is.data.table(gridcells) || is.data.frame(gridcells))) {
    stop(paste("Arguments 'gridcells' not of correct type in call to getGridcells(), it should be 'data.table' or 'data.frame', got ", class(gridcells)[1], sep = " "))
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
      
      if(is.data.frame(gridcells)) gridcells <- as.data.table(gridcells)
      selection.dt  <- gridcells[,c(1,2)]
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
    x@spatial.extent <- extent(final.dt)
    x@id <- makeFieldID(source = x@source,
                        var.string = x@quant@id, 
                        sta.info = as(x, "STAInfo"))
    return(x)
  }
  
  
  
}
