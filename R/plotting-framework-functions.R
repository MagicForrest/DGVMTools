#' Sanitise input fields
#' 
#' This is an internal helper function which checks the inputs to a plotXXXX function (which should be a single Field or a list of Fields) and returns a list of Fields
#' 
#' @param fields The input to a plotXXXX() functions to be checked
#' @return Returns a list of DGVMTools::Field objects
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' 
#' 

santiseFieldsForPlotting <- function(fields) {
  
  if(is.Field(fields)) {
    fields <- list(fields)
  }
  else if(class(fields)[1] == "list") {
    for(object in fields){ 
      if(!is.Field(object)) {
        warning("You have passed me a list of items to plot but the items are not exclusively Fields.  Returning NULL")
        return(NULL)
      }
    }
  }
  else{
    stop(paste("plotSpatial can only handle single a DataObject or Field, or a list of Data/Fields can't plot an object of type", class(fields)[1], sep = " "))
  }
  
  return(fields)
  
}



#####################################################################################################################
################ CORRECTS AN ARTEFACT FROM MAPS PACKAGE WHERE EASTERN ASIA IS WRONGLY PLACED ########################
#####################################################################################################################

#' 
#' Fixes a spatial lines object where some of eastern Russia transposed to the other side of the world
#' 
#' 
#' @param spl SpatialLines object to fix
#' @return a the SpatialLines object 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @keywords internal
correct.map.offset <- function(spl) {
  
  we <- raster::crop(spl, raster::extent(-180, 180, -90, 90))
  ww <- raster::crop(spl, raster::extent(179.999, 200, -90, 90))
  
  if(!is.null(ww) & !is.null(we)) {
    
    ww <- raster::shift(ww, -360)
    spl <- raster::bind(we, ww)  
    
  }
  return(spl)
}



#' Make a PFT colour list
#' 
#' This is a helper function for when plotting PFTs by colour.  It takes a list of PFT ids (other things like "Total" or "Tree" can also be specified) and returns a list 
#' of colours with the names of the PFT (which is how ggplot likes colours to be specified).
#' 
#' @param values List of values (as chararacters) for which you want standard colours.
#' @param pfts A list of PFT objects (which should contain PFTs with ids provided in 'values)
#' @param others A list of other name-colour combinations, for example to plot 'Total' as black, "None" as grey, or whatever.  Some defaults are defined.
#' @return Returns a named list of colours, where the names are the values that the colours will represent
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
#' 
#' 
matchPFTCols <- function(values, pfts, others = list(Total = "black", None = "grey75", Tree = "brown", Grass = "green", Shrub = "red")) {
  
  these.cols <- list()
  at.least.one.match <- FALSE
  for(val in values) {
    
    # ignore NAs
    if(!is.na(val)) {
      
      # check if it is a PFT
      done <- FALSE
      for(PFT in pfts){
        if(val == PFT@id) {
          these.cols[[val]] <- PFT@colour
          done <- TRUE
          at.least.one.match <- TRUE
        }
      } 
      
      # if not a PFT, check if it is as 'other' 
      if(!done) {
        for(other in names(others)) {
          if(tolower(val) == tolower(other)) {
            these.cols[[val]] <- others[[other]]
            done <- TRUE
            at.least.one.match <- TRUE
          }
        }
      }
      
      # if no colour can be found to match the value, fail gently
      if(!done && at.least.one.match) {
        warning(paste0("Some value (", val, ") doesn't have a specified colour, so matchPFTCols is returning NULL. Check your inputs and note the you can provide a colour for (", val, ") using the 'others' argument"))
        return(NULL)
      }  
      
    }  # if not NA
    
  } # for each value
  
  return(unlist(these.cols))
  
}

#' Make a map overlay for ggplot2
#' 
#' Take a string and derives an approriate data.frame that can be used to add a map overlay 
#' (eg coast or country lines from the maps and mapdata packages) with the ggplot::geom_path function.
#' 
#' @param map.overlay A character string specifying the overlay to be used a string matching maps package dataset
#' @param all.lons A numeric vector of all the longitudes to be plotted, this is used to determine if it the over lay should be on longitues (-180,180) or (0,360).
#' @param interior.lines A logical, if TRUE include the internal country lines
#' @param xlim A numeric vector of length 2 to giving the longitude window that the overlay should cover
#' @param ylim A numeric vector of length 2 to giving the latitide window that the overlay should cover
#' @return Returns data.frame suitable for plotting with ggplot::geom_path
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
makeMapOverlay <- function(map.overlay, all.lons, interior.lines, xlim, ylim) {
  
  # first check that rgeos package is installed
  if (! requireNamespace("rgeos", quietly = TRUE))  {
    warning("Please install the rgoes R package and, if necessary the GEOS libraries, on your system to make map overlays.")
    return(NULL)
  }
  
  
  ### PREPARE THE MAP OVERLAY
  if(is.character("character")){
    
    # determine if london centered (if not call "maps2" for Pacific centered versions)
    gt.180 <- FALSE
    for(lon in all.lons) {
      if(lon > 180) gt.180 <- TRUE
    }
    
    if(map.overlay=="world" && gt.180) map.overlay <- "world2"
    else if(map.overlay=="worldHires" && gt.180) map.overlay <- "worldHires2"
    else if(map.overlay=="world2" && !gt.180) map.overlay <- "world"
    else if(map.overlay=="world2Hires" && !gt.180) map.overlay <- "worldHires"
    
    # Convert map to SpatialLinesDataFrame, perform the 'Russian Correction' and then fortify() for ggplot2
    proj4str <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0 +no_defs"
    map.sp.lines <- maptools::map2SpatialLines(maps::map(map.overlay, plot = FALSE, interior = interior.lines, xlim=xlim, ylim=ylim, fill=TRUE), proj4string = sp::CRS(proj4str))
    suppressWarnings(df <- data.frame(len = sapply(1:length(map.sp.lines), function(i) rgeos::gLength(map.sp.lines[i, ]))))
    rownames(df) <- sapply(1:length(map.sp.lines), function(i) map.sp.lines@lines[[i]]@ID)
    map.sp.lines.df <- sp::SpatialLinesDataFrame(map.sp.lines, data = df)
    if(!gt.180) map.sp.lines.df <- correct.map.offset(map.sp.lines.df)
    return(fortify(map.sp.lines.df))
    
  }
  else {
    stop(paste0("Can't make an overlay from type ", class(map.overlay)))
  }
  
}