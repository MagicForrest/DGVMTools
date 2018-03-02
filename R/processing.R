#!/usr/bin/Rscript

############# EXTRACT THE PFTS PRESENT IN A RUN USING THE DATA.TABLE HEADER
#' Get the PFTs present in a run
#' 
#' Extract the PFTs present in some data, given the set of possible PFTs.  The data can be represented as a data.table, Field, or Raster*-object.  The 
#' It does thsi based on the names of the layers/columns of the data 
#' @param input The data from which to retrieve the PFTs present (data.table, Field, or Raster*-object)
#' @param PFT.data A list of PFTs which might be present (ie. a superset of those actually present)
#' @return A list of PFT object which are actually present 
#' @export
#' @import data.table raster
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
getPFTs <- function(input, PFT.data){
  
  # Allow for rasters, Veg Objects and data.tables
  input.class <- class(input)[1]
  if(is.Field(input)) suppressWarnings(input.names <- names(input@data))
  else if(input.class == "data.table" | input.class == "RasterLayer" | input.class == "RasterBrick" | input.class == "RasterStack") input.names <- names(input)
  else stop(paste("Can't get PFTs from object of class", input.class, sep = " "))
  
  PFTs.present <- list()
  for(colname in input.names){
    for(PFT in PFT.data){
      if(PFT@id == colname) {
        PFTs.present <- append(PFTs.present, PFT)
      }
    }
  }
  
  return(PFTs.present)
  
}









###################################################################################
##### MAKE PFT, LIFEFORM, PHENOLOGY ETC FRACTIONS
# TODO:  Maybe take a look at the horrible "eval(quote(paste(" syntax below
#'
#' Calculate fractions from the layers of a Field with respect to other layers
#' 
#' This is very useful and important function.  It is fully flexible  -both the numerator and denominator can be specified (although the denominator defaults to "Total").
#' Note that if a layer doesn't exist it will be created if possible.
#' 
#' @param input The Field for which to calculate the new fractional layers
#' @param layers The layers to be divided ie. the numerators (will be calculated by \code{getVegTotals} if the don't exist)
#' @param denominators The denominator layers (will be calculated by \code{getVegTotals} if the don't exist) (defaults to just "Total")
#' @param aggregate.method If the denominators need to be made, which method should be used to make them, "sum" or "mean", defaults to "sum".
#' parame
#'
#' @details
#' Division is safe with respect to a zero denominator, the results of dividing by zero is, in this case, zero.
#' 
#' For convenience, both \code{layers} and \code{denominators} will be expanded using \code{expandLayers}.
#' This allows all lifeforms fractions in a Field to be calculated using a simple call such as
#' 
#'  \code{veg.obj <- divideLayers(veg.obj, c("lifeforms")}
#'  
#' See documention of \code{expandLayers} for details. 
#' 
#' @return A Field (or data.table) with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @seealso expandslayers getVegTotals

divideLayers <- function(input, layers, denominators = list("Total"), aggregate.method = "sum"){
  
  
  ########### SEPARATION CHARACTER
  # I am not sure if I prefer to use " ", "", "_" or "." when constructing variable names such as "EvergreenFractionofTree"
  #' This should clearly be implemented in a more useful way...
  sep.char = ""
  
  
  # To avoid NOTES
  Total = NULL
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@source@pft.set
  PFTs <- getPFTs(dt, input@source@pft.set)
  
  # First, expand denominator layers and make what aren't available
  denominators <- expandLayers(denominators, dt, PFT.data)
  for(denom in denominators) {
    if(!(denom %in% names(dt))) {
      dt <- autoLayer(dt, denom, PFT.data = PFT.data, method = aggregate.method)
    }
  }
  
  
  # Second, expand numerator layers and make what aren't available
  layers <- expandLayers(layers, dt, PFT.data)
  for(layer in layers) {
    if(!(layer %in% names(dt))) {
      dt <- autoLayer(dt, layer, PFT.data = PFT.data, method = aggregate.method)
    }
  }
  
  
  # Finally loop through all denominators and numerators and calculate the fractions
  for(denominator in denominators) {
    for(layer in layers){
      
      if(denominator == "Total") 
        suppressWarnings(dt[, eval(quote(paste(layer, "Fraction", sep = sep.char))) := get(paste(layer, sep = sep.char))%/0%Total])
      else
        suppressWarnings(dt[, eval(quote(paste(layer, "Fraction", "Of", denominator, sep = sep.char))) := get(paste(layer, sep = sep.char))%/0%get(paste(denominator, sep = sep.char))])
      
    }
  }
  
  input@data <- dt
  
  return(input)
  
}



