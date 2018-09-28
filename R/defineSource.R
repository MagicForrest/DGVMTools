######## DEFINE A SOURCE OBJECT 

#' Define a Source object that represents a model run or data set, setting up all the required metadata (only). 
#' 
#' This function is preferred to a \code{new("Source",...)} initialisation because it does both intialisations in one step and also performs some extra checks and initialisations of defaults.
#'
#' Note that initially no actual data is stored, only metadata. The data, stored as Fields, can be added to a Source object, are added built with later with other commands and added to \code{Source} command using \code{addToSource}
#'
#' @param id A unique character string to identify this particular data.  Recommended to be alphanumeric because it is used to construct file names and to use underscores and full stops (periods) 
#' for separating characters where need be.   For example, "Standard_v4.0" or "Saatchi2011.HD"  (can be derived from \code{name} if a \code{name} is supplied, otherwise mandatory)
#' @param format A character string to identify the format of the files of this data sorce. This can be anything, but should be in order for "getField()" to work corrected 
#' it needs be a \code{supported.format} which is either a supported model that produced this (e.g. "LPJ-GUESS") or "DGVMData" for data files produced by the DGVMData package (Mandatory)
#' @param pft.set A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
#' @param name A character string describing this run, ie. "LPJ-GUESS v3.1" (can be derived from \code{id} if an \code{id} is supplied, otherwise mandatory)
#' @param dir The location of this source on the file system (Mandatory)
#' @param quantities A list of Quantity object what we expect to have in this Source.  The option allows the over-riding the default quantities in the Format object.  
#' Not 100\% sure why this might be useful though.
#' @param lonlat.offset A numeric of length 1 or 2 to define the offsets to Lon and Lat to centre the localities, somewhat legacy for old LPJ-GUESS runs.
#' @param year.offset A numeric of length 1 to match be added to the years to convert them to calendar years,  somewhat legacy for old LPJ-GUESS runs.
#' @param london.centre If TRUE, ensure that the longitudes are (-180,180) instead of (0,360) 
#' @param forcing.data A character string identifying the climate or other data used to produce this model run
#' @param land.use.included If TRUE it can be assumed that land use has been simulated for this run and so no correction for land use need be applied before benchmarking.
#' @param contact Name and email address of responsible person (default to OS username).
#' @param institute Name of the institute (default "none").
#' @param ... extra arguments for call to determinePFTs() to determine the PFTs in a run, for exampel which aDGVM classification scheme to use (normally not needed)
#' 
#' Note that that \code{id}, \code{name}, \code{format}, and \code{dir} are compulsory, the rest will be filled with dummy/default values if left blank.
#' Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for some LPJ-GUESS configurations.
#' @return A Source object including metadata defined by empty data slots
#' @export
#' @seealso Source
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

defineSource <- function(id,
                         name,
                         dir,
                         format,
                         quantities = NULL, # why do we have this?  is it useful?
                         pft.set = list(),
                         lonlat.offset = c(0,0),
                         year.offset = 0,
                         london.centre = TRUE,
                         forcing.data = "Not specified",
                         land.use.included = FALSE,
                         contact = "Not specified",
                         institute = "Not specified",
                         ...){
  
  
  
  # check for name or id
  if(missing(name) && missing(id)) stop("Need to provide at least one of a name or an id")
  if(missing(name))  name <- id
  if(missing(id))  {
    id <- gsub(" ", "_", name, fixed = TRUE)
    id <- gsub("/", ".", id, fixed = TRUE)
  }
      
  # Retreive a Format object if a string is provided
  if(is.character(format)) {
    if(format == "LPJ-GUESS" || format == "GUESS" || format == "LPJ-GUESS-SPITFIRE") {
      format <- GUESS
    }
    else if(format == "DGVMData") {
      format <- DGVMData
    }
    else if(format == "aDGVM") {
      format <- aDGVM
    }
    
  }
  
  # 
  if(missing(pft.set)){
    pft.set <- format@default.pfts
  }
  
  # 
  if(!missing(quantities)){
    format@quantities <- quantities
  }
  
  # make Source object from the supplied meta data
  source <- new("Source",
              id = id,
              format = format,
              pft.set = pft.set,
              name = name,
              dir = dir,                              
              forcing.data = forcing.data,
              lonlat.offset = lonlat.offset,
              year.offset = year.offset,
              london.centre = london.centre,
              land.use.included = land.use.included,
              contact = contact,
              institute = institute)
  
  # look up the PFTs to get the actual PFTs present from the superset
  if(length(source@pft.set) > 0) {
    source@pft.set <- determinePFTs(source, ...)
  }

  
  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(source@pft.set) == 0) source@pft.set <- list()
  if(length(source@lonlat.offset) == 0)  source@lonlat.offset <- c(0,0)
  if(length(source@year.offset) == 0)  source@year.offset <- 0
  if(length(source@forcing.data) == 0)  source@forcing.data <- "No forcing data set"
  if(length(source@london.centre) == 0)  source@london.centre <- TRUE
  if(length(source@land.use.included) == 0)  source@land.use.included <- FALSE
  if(length(source@contact) == 0)  source@contact <- Sys.getenv("USER")
  if(length(source@institute) == 0)  source@institute <- "none"
  
  # return a Source object with empty data fields but meta data filled  
  return(source)
  
  
}

