######## DEFINE A SOURCE OBJECT 

#' Define a Source object that represents a model run or data set, setting up all the required metadata (only). 
#' 
#' This function is preferred to a \code{new("Source",...)} initialisation because it does both the initialisations (including some defaults)
#' and also performs some extra checks.
#'
#' Note that no actual data is stored in the resultant \linkS4class{Source} object, rather this object should be used in calls to \link{getField}
#' to get data as a \linkS4class{Field} object.
#'
#' @param id A unique character string to identify this particular data.  Recommended to be alphanumeric because it is used to construct file names and to use underscores and full stops (periods) 
#' for separating characters where need be.   For example, "Standard_v4.0" or "Saatchi2011.HD"  (can be derived from \code{name} if a \code{name} is supplied, otherwise mandatory)
#' @param format A Format object to describe the type of source.  Can be GUESS, aDGVM, aDGVM2 or DGVMData (note no quotes since these are actual R objects not strings).  (Mandatory) DEPRECATED: alternatively a character string to identify the format of the files of this data sorce. This can be anything, but should be in order for "getField()" to work corrected 
#' it needs be a \code{supported.format} which is either a supported model that produced this (e.g. "LPJ-GUESS") or "DGVMData" for data files produced by the DGVMData package 
#' @param defined.layers A list of PFT objects which includes all the PFTs used is this model run (Mandatory)
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
#' @param ... extra arguments, not used yet
#' Note that that \code{format}, \code{dir} and one of \code{id} or \code{name} are compulsory, the rest will be filled with dummy/default values if left blank.
#' Take care with \code{lon.lat.offset} and \code{year.offset} which are initialised to 0 which is unsuitable for aDGVM1 and some LPJ-GUESS configurations.
#' @return A Source object 
#' @export
#' @seealso Source
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 

defineSource <- function(id,
                         name,
                         dir,
                         format,
                         quantities = NULL, # why do we have this?  is it useful?
                         defined.layers = list(),
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
      warning(paste0('Use for character string to specify the "format" argument in defineSource() is deprecated and will soon be removed. Please replace "', format,'" with GUESS (unquoted)'))
    }
    else if(format == "DGVMData") {
      format <- DGVMData
      warning(paste0('Use for character string to specify the "format" argument in defineSource() is deprecated and will soon be removed. Please replace "', format,'" with DGVMData (unquoted)'))
    }
    else if(format == "aDGVM") {
      format <- aDGVM
      warning(paste0('Use for character string to specify the "format" argument in defineSource() is deprecated and will soon be removed. Please replace "', format,'" with aDGVM (unquoted)'))
      
    }
    else if(format == "aDGVM2") {
      format <- aDGVM2
      warning(paste0('Use for character string to specify the "format" argument in defineSource() is deprecated and will soon be removed. Please replace "', format,'" with aDGVM (unquoted)'))
      
    }
  }
  
  if(missing(defined.layers)){
    defined.layers <- format@predefined.layers
  }

  if(!missing(quantities)){
    format@quantities <- quantities
  }
  
  # make Source object from the supplied meta data
  source <- new("Source",
              id = id,
              format = format,
              defined.layers = defined.layers,
              name = name,
              dir = dir,                              
              forcing.data = forcing.data,
              lonlat.offset = lonlat.offset,
              year.offset = year.offset,
              london.centre = london.centre,
              land.use.included = land.use.included,
              contact = contact,
              institute = institute)
  
  # if things aren't specified set them here because it seems like the 'prototype' field isn't working
  if(length(source@defined.layers) == 0) source@defined.layers <- list()
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

