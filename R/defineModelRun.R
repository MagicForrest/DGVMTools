######## DEFINE A MODELRUN OBJECT 

#' Define a ModelRun object, setting up all the required metadata (only). 
#' 
#' This function is preferred to a \code{new("SourceInfo",...)} initialisation followed by  \code{new("ModelRun",...)} initialisation because it does both intialisations in one step and also performs some extra checks and initialisations of defaults.
#'
#' Note that initialliy, no actual data from the run is stored, only metadata. The data, stored as ModelObjects and added to a ModelRun object, are added built with later with other commands and added to \code{ModelRun} command using \code{addToModelRun}
#'
#' @param id A unique character string to identify this particular model un.  Recommended to be alphanumeric because it is used to construct file names. (Mandatory)
#' @param model A character string to identify what model produced this run.  Can currently be "LPJ-GUESS", "LPJ-GUESS-SPITFIRE" or "aDGVM". (Mandatory)
#' @param pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @param name A character string describing this run, ie. "LPJ-GUESS v3.1"
#' @param run.dir The location of this run on the file system (Mandatory)
#' @param lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the modelled localities.
#' @param year.offset A numeric of length 1 to match be added to the simulation years to convert them to calendar years
#' @param tolerance The tolerance arguement when converting uneven spaced grids to regular rasters for plotting
#' @param london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @param fill.col A string to define an R colour used when plotting this run as a histogram or scatter plot or so
#' @param line.col  A string to define an R colour used when plotting this runs as a line graph
#' @param line.width A numeric to define the width of a line representing this model run
#' @param line.type A numeric to define the line style representing this model run
#' @param driving.data A character string identifying the climate or other data used to produce this model run
#' @param landuseSimulated If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @param contact Name and email address of responsible person (default to OS username).
#' @param institute Name of the institute (default "none").
#' 
#' The parameters are the slots of a SourceInfo object. Note that that \code{id}, \code{model}, \code{pft.set} and \code{dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS configurations.
#' @return A ModelRun object, with metadata defined by empty data slots
#' @export
#' @seealso ModelRun, SourceInfo 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

defineModelRun <- function(id,
                           name,
                           dir,
                           model,
                           pft.set,
                           lonlat.offset = c(0,0),
                           year.offset = 0,
                           tolerance = 0.001,
                           london.centre = TRUE,
                           driving.data = "Not specified",
                           landuseSimulated = FALSE,
                           contact = "Not specified",
                           institute = "Not specified"){
  
  # make SourceInfo object from the supplied meta data
  info <- new("SourceInfo",
              id = id,
              type = "model",
              model = model,
              pft.set = pft.set,
              name = name,
              dir = dir,                              
              driving.data = driving.data,
              lonlat.offset = lonlat.offset,
              year.offset = year.offset,
              tolerance = tolerance,
              london.centre = london.centre,
              landuseSimulated = landuseSimulated,
              contact = contact,
              institute = institute)
  

  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(info@pft.set) == 0) info@pft.set <- NULL
  if(length(info@name) == 0)  info@name <- info@id
  if(length(info@lonlat.offset) == 0)  info@lonlat.offset <- c(0,0)
  if(length(info@year.offset) == 0)  info@year.offset <- 0
  if(length(info@tolerance) == 0)  info@tolerance <- 0.001
  if(length(info@driving.data) == 0)  info@driving.data <- "No forcing data set"
  if(length(info@london.centre) == 0)  info@london.centre <- TRUE
  if(length(info@landuseSimulated) == 0)  info@landuseSimulated <- FALSE
  if(length(info@contact) == 0)  info@contact <- Sys.getenv("USER")
  if(length(info@institute) == 0)  info@institute <- "none"
  
  # return a ModelRun object with empty data fields but meta data filled  
  return(new("ModelRun",
             info))
  
  
}

