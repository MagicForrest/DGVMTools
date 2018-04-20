########### HANDY PROCESSING FUNCTIONS ########### 

#' Safe division
#' 
#' Function to divide two number but return 0 if the denominator is 0
#' 
#' @param x numerator
#' @param y denominator
#' 
#' Handy little thing.
#' @export
#' 
#' 
"%/0%" <- function(x,y) ifelse(y==0,0,base::"/"(x,y))

#!/usr/bin/Rscript


######## SET KEY ON DATA TABLE USED TO STORE VEG INFORMATION
#### PUT THIS HANDY HELPER FUNCTION FIRST TO AVOID NOTES

#' Sets keys on data.table based in the spatial (Lon, Lat) and temporal (Year, Month, Day), present. 
#' 
#' Keys should be set on all data.table object for sorts, joins, DGVMTool-defined operators etc.  
#'  This function should be called on a data.table stored in a Field after it has been created,
#'  including if it was created by avergaing another data.table because it seems as keys are not conserved.
#'
#' @param dt The data.table for which to set the key
#' @return Returns nothing because changes the original data.table by reference (this is the data.table way)
#' @import data.table
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

setKeyDGVM <- function(dt){
  
  keys.present <- getSTInfo(dt)
  
  if(length(keys.present) > 0) setkeyv(dt, keys.present)
  else warning("No spatial/temporal columns present in this data.table to set as keys!")
  
}



#' London centre a gridcell
#' 
#' Transform a vector of longitude values to lie in the range (-180,180) instead of (0,36)
#'
#' @param lon A vector longitude value to transform 
#' @return The transformed longitude values
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
LondonCentre <- function(lon) {
  
  return(ifelse(lon > 180, lon - 360, lon))
  
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
#' @import raster
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
#' 
#' 
matchPFTCols <- function(values, pfts, others = list(Total = "black", None = "grey75", Tree = "brown", Grass = "green", Shrub = "red")) {
  
  these.cols <- list()
  for(val in values) {
    
    # check if it is a PFT
    done <- FALSE
    for(PFT in pfts){
      if(val == PFT@id) {
        these.cols[[val]] <- PFT@colour
        done <- TRUE
      }
    } 

    # if not a PFT, check if it is as 'other' 
    if(!done) {
      for(other in names(others)) {
        if(tolower(val) == tolower(other)) {
          these.cols[[val]] <- others[[other]]
          done <- TRUE
        }
      }
    }
    
    # if no colour can be found to match the value, fail gently
    if(!done) {
      warning(paste0("Some value (", val, ") doesn't have a specified colour, so matchPFTCols is returning NULL. Check your inputs and note the you can provide a colour for (", val, ") using the 'others' argument"))
      return(NULL)
    }  

  }
  
  return(unlist(these.cols))
  
}


## Some date function
## not yet really needed. However, potentially useful with the daily LPJ-GUESS output.

#' check if a given year is a leap year
#' 
#' @param year year (integer or vector)
#' @param proleptic use leap years even before 1582.
#' @param doy return days of year instead logical.
#' @return logical or integer, if doy is TRUE
#' 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
is.leapyear <- function(year, proleptic=FALSE, doy=FALSE) {
  leap <- sapply(year, function(x) {
    if (!proleptic && x < 1582) return(FALSE)
    if (((x %% 4 == 0) & (x %% 100 != 0)) | (x %% 400 == 0))
      return(TRUE)
    return(FALSE)
  })
  if (doy)
    return(ifelse(leap, 366, 365))
  return(leap)
}