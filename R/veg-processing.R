#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
###################################### 



########### SEPARATION CHARACTER
# I am not sure if I prefer to use " ", "", "_" or "." when constructing variable names such as "EvergreenFractionofTree"
# but here is an easy way to choose.

#' Separation character for builidng variables names
#' 
#' Currently hard-coded to "", but could also be " ", "_" or "." (for example)
#' This should clearly be implemented in a more useful way...
sep.char = ""



############# EXTRACT THE PFTS PRESENT IN A RUN USING THE DATA.TABLE HEADER
#' Get the PFTs present in a run
#' 
#' Extract the PFTs present in some data, given the set of possible PFTs.  The data can be represented as a data.table, VegObject, or Raster*-object.  The 
#' It does thsi based on the names of the layers/columns of the data 
#' @param input The data from which to retrieve the PFTs present (data.table, VegObject, or Raster*-object)
#' @param PFT.data A list of PFTs which might be present (ie. a superset of those actually present)
#' @return A list of PFT object which are actually present 
#' @export
#' @import data.table raster
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
getPFTs <- function(input, PFT.data){
  
  # Allow for rasters, Veg Objects and data.tables
  input.class <- class(input)[1]
  if(is.VegObject(input)) suppressWarnings(input.names <- names(input@data))
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

############# EXPAND TARGETS
#'
#' Expand plotting/processing targets from convenient short hand codes.
#' 
#' Expands characters strings with particular meaning (eg. "lifefoms", "pfts", "seasons", etc...).  
#' This allows the user to simply specify "lifeforms" to a plotting command instead of c("Tree","Grass","Shrub") when plotting or processing.
#'
#' @details Supported short-hand strings are:
#' \itemize{
#'   \item{"seasons"}{ expands to c("DJF","MAM","JJA","SON")}
#'   \item{"PFTs"}{ expands to a vector of each of the ids of PFTs supplied in the \code{PFTs} argument.}
#'   \item{"lifeforms"}{ expands all the different PFT lifeforms present as determined by the lifeform slots of the \code{PFTs} argument (and optionally "Woody").  For example c("Tree", "Grass", "Woody")}
#'   \item{"zones"}{ expands all the different PFT climactic zones present as determined by the zone slots of the \code{PFTs} argument.  For example c("Temperature", "Boreal", "Tropical")}
#'   \item{"leafforms"}{ expands all the different PFT leaf forms present as determined by the leafforms slot of the \code{PFTs} argument.  For example c("Needleleved", "Broadleaved")}
#'   \item{"phenologies"}{ expands all the different PFT leaf forms present as determined by the phenology slot of the \code{PFTs} argument.  For example c("Evergreen", "Summergreen", "Raingreen", "GrassPhenology)}
#'   \item{"all"}{ expands to c("pfts", "lifeforms", "leafforms", "zones", "phenologies"), and then these are further expanded.}
#' }
#' 
#' 
#' @param targets Character vector of targets to expand
#' @param data The data to which the targets will be applied, may be a \code{VegObject}, a Raster* object, data.table or data.frame, a Spatial*DataFrame. 
#' @param PFT.set List of a superset of PFTs that might be found in this run (only necessary if *not* passing a \code{VegObject}).
#' @param type Character string irdentifying if this is a monthly  (= "monthly") or per PFT (="pft") variable.  Ignored for \code{VegObjects} which supply this data themselves.
#' The function attempts to determine this argument if it is not provided.  
#' @param include.woody If TRUE and "lifeform" is included in the target list "Woody" is also returned
#' @return A list of targets
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
expandTargets <- function(targets, data, PFT.set = NULL, type = "unknown", include.woody = TRUE){
  
  # remove "Lon", "Lat" and "Year" if present
  for(remove.header.from.header in c("Lat", "Lon", "Year")){
    if(remove.header.from.header %in% targets) targets <- targets[-which(targets == remove.header.from.header)]
  }
  
  # read meta-data form VegObject if possible
  if(is.VegObject(data)) {
    type <- data@quant@type
    if(is.null(PFT.set)) PFT.set <- data@run@pft.set
    header <- names(data@data)
  }
  else{
    header <- names(data)
  }
  
  # get PFTs present in data
  PFTs <- getPFTs(data, PFT.set)

  # if type is undertermined, try to figure it out
  # if found at least one PFT and that PFT is not "Total" then assume we have a per PFT variable
  if(type == "unknown"){
    if(length(PFTs) > 0) {
      if(PFTs[[1]]@id != "Total") {
      type <- "pft"
      }
    }
    else{
      type <- "monthly"
      for(month in months) {
        if(!(month@id %in% header)){
          type <- "unknown"
        }
      }
    }
  }
  
  if(tolower(type) == "pft") {
    
    # expand "all"
    if("all" %in% tolower(targets)) targets <- c("pfts", "lifeforms", "leafforms", "zones", "phenologies")
    
    # expands "pfts"
    
    if("pft" %in% tolower(targets) || "pfts" %in% tolower(targets)) {
      for(PFT in PFTs) {
        targets <- append(targets, PFT@id)
      }
      
      if("pft" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "pft")]
      if("pfts" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "pfts")]
    }
    
    
    # expand "lifeforms" 
    if("lifeforms" %in% tolower(targets) | "lifeform" %in% tolower(targets)){
      
      # Find all lifeforms present in PFTs 
      all.lifeforms <- vector()
      for(PFT in PFTs) {all.lifeforms <- append(all.lifeforms, PFT@lifeform)}
      all.lifeforms <- unique(all.lifeforms)
      # MF: Special case to combine Trees and Shrubs inoo Woody category
      if(include.woody) all.lifeforms <- append(all.lifeforms, "Woody")
      
      targets <- append(targets, all.lifeforms)
      
      if("lifeforms" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "lifeforms")]
      if("lifeform" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "lifeform")]
      
    }
    
    
    # expand "zones" 
    if("zones" %in% tolower(targets) | "zone" %in% tolower(targets)){
      
      # Find all zones present in PFTs 
      all.zones <- vector()
      for(PFT in PFTs) {all.zones <- append(all.zones, PFT@zone)}
      all.zones <- unique(all.zones)
      
      targets <- append(targets, all.zones)
      
      if("zones" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "zones")]
      if("zone" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "zone")]    
      
    }
    
    # Expand "leafforms" 
    if("leafforms" %in% tolower(targets) | "leafform" %in% tolower(targets)){
      
      # Find all leafforms present in PFTs 
      all.leafforms <- vector()
      for(PFT in PFTs) {all.leafforms <- append(all.leafforms, PFT@leafform)}
      all.leafforms <- unique(all.leafforms)
      
      targets <- append(targets, all.leafforms)
      
      if("leafforms" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "leafforms")]
      if("leafform" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "leafform")] 
      
    }
    
    # Expand "phenologies" 
    if("phenologies" %in% tolower(targets) | "phenology" %in% tolower(targets)){
      
      # Find all phenologys present in PFTs 
      all.phenologies <- vector()
      for(PFT in PFTs) {all.phenologies <- append(all.phenologies, PFT@phenology)}
      all.phenologies <- unique(all.phenologies)
      #if("NA" in all.phenologies) all.phenologies <- all.phenologies[-which(tolower(all.phenologies) == "NA")]
      
      targets <- append(targets, all.phenologies)
      
      if("phenologies" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "phenologies")]
      if("phenology" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "phenology")] 
      
    }
    
    
  }
  
  else if(tolower(type) == "monthly"){
    
    # Expand seasons
    if("seasons" %in% tolower(targets) | "season" %in% tolower(targets) | "seasonal" %in% tolower(targets)){
      
      targets <- append(targets, c("DJF", "MAM", "JJA", "SON"))
      
      if("seasons" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "seasons")]
      if("season" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "season")] 
      if("seasonal" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "seasonal")] 
      
    }
    
    # Expand months/monthly
    if("months" %in% tolower(targets) | "monthly" %in% tolower(targets)){
      
      for(month in months) {
        targets <- append(targets, month@id)
      }
      
      if("months" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "months")]
      if("monthly" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "monthly")] 
      
    }
    
  }
  
  if("NA" %in% targets) targets <- targets[-which(targets == "NA")]
  if("all" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "all")]

  return(targets)
  
}



###################################################################################
##### COMBINE SHADE-INTOLERANT PFTS WITH SHADE-TOLERANT PFTS FOR FINDING DOMINANT PFT
##### AND DOING BIOME CLASSIFICATIONS ETC
#'
#' Combine shade-intolerant PFTs with their shade-tolerant cousions
#' 
#' The effects of this depend on the shade-tolerant cousin PFTs being defined in the PFT list.
#' 
#' 
#' @param input The VegObject which is to have the shade tolerance classes combined.
#' @return A VegObject with the data for the shade-intolerant PFTs set to zero but their values added to the shade-tolerant versions
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
combineShadeTolerance <- function(input){
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  
  for(colname in names(dt)){
    
    # if the PFT is in the PFT list
    if(!is.null(PFT.data[[colname]])) {
      PFT <- PFT.data[[colname]]
      
      # if PFT is to be combined
      if(tolower(PFT@combine) != "no"){
        
        # if PFT to be added is present the combine them and set the shade intolerant PFT to zero, if not send a warning and do nothing
        if(!is.null(PFT.data[[PFT@combine]])){
          dt[, PFT.data[[PFT@combine]]@id := rowSums(.SD), .SDcols=c(PFT.data[[PFT@combine]]@id, PFT@id)]
          dt[, PFT@id := 0]
        }
        else{
          warning(paste("PFT", PFT, "is supposed to be combined with PFT", PFT@combine, "but that PFT is not present so ignoring.", sep = " " ))
          
        }
      } # if the PFT is to be combined
    } # if the colname is a PFT
  } # for each column name
  
  input@data <- dt
  
  return(input)
  
}

###################################################################################
##### FIND THE DOMINANT PFT (OF TOTAL AND/OR TREE AND/OR WOODY)
##### Notes: Could make this nicer using a list of inputs ("Total", "Tree", "Woody") that can be looped over
##### TODO   Low priority
#'
#' Find dominant PFT
#' 
#' Find the dominant PFT (for each point in space and/or time) and stored in the VegObject.  Can be of all PFTs, of all tree PFTs and/or of all woody PFTs 
#' The resulting layers are called "Dominant", "DominantTree" and "DominantWoody" respectively.
#' 
#' @param input The VegObject which is to have the shade tolerance classes combined.
#' @param do.all If TRUE calculate the dominant PFT out of all PFTs
#' @param do.tree If TRUE calculate the dominant PFT out of all tree PFTs
#' @param do.woody If TRUE calculate teh dominant PFT out of all woody PFTs
#' @return The VegObject with the dominant layers PFTs added
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
addDominantPFT <- function(input, do.all = TRUE, do.tree = FALSE, do.woody = FALSE){
  
  # To avoid NOTES when checking
  Total = Dominant = NULL
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  
  # auxiliary function to be apply'd
  domPFT <- function(x){return(names(x)[which.max(x)])  }  
  
  # get colnames of PFTs
  PFTs.present <- list()
  tree.PFTs.present <- list()
  woody.PFTs.present <- list()
  for(colname in names(dt)){
    for(PFT in PFT.data){
      if(PFT@id == colname) {
        if(do.all) {PFTs.present <- append(PFTs.present, colname)}
        if(do.tree && tolower(PFT@lifeform) == "tree") {tree.PFTs.present <- append(tree.PFTs.present, colname)}
        if(do.woody && (tolower(PFT@lifeform) == "tree" || tolower(PFT@lifeform) == "shrub")) {woody.PFTs.present <- append(woody.PFTs.present, colname)}
      }
    }
  }
  
  
  if(do.all) {
    PFTs.present <- unlist(PFTs.present)
    PFTs.present <- PFTs.present[-which(PFTs.present == "Total")]
    suppressWarnings(dt[, Dominant := apply(dt[,PFTs.present,with=FALSE],FUN=domPFT,MARGIN=1)])
    dt[Total < 0.2, eval(quote(paste("Dominant"))) := "Barren"]
    dt[, eval(quote(paste("Dominant"))) := as.factor(get("Dominant"))]
    
  }
  
  if(do.tree) {
    tree.PFTs.present <- unlist(tree.PFTs.present)
    suppressWarnings(dt[, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := apply(dt[,tree.PFTs.present,with=FALSE],FUN=domPFT,MARGIN=1)])
    dt[Total < 0.2, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := "Barren"]
    dt[, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := as.factor(get(paste("Dominant", "Tree", sep = sep.char)))]
  }
  
  if(do.woody) {
    woody.PFTs.present <- unlist(woody.PFTs.present)
    suppressWarnings(dt[, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := apply(dt[,woody.PFTs.present,with=FALSE],FUN=domPFT,MARGIN=1)])
    dt[Total < 0.2, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := "Barren"]
    dt[, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := as.factor(get(paste("Dominant", "Woody", sep = sep.char)))]
    
    
  }
  
  input@data <- dt
  return(input)
  
}


###################################################################################################
######### BIOME CLASSIFICATION
#'
#' Perform biome classification using this VegObject
#' 
#' This is very inflexible as it only allows the calcualtion of biomes with only one VegQuant.  This is not suitable for many biomes schemes, 
#' so this will need to be re-written
#' 
#' 
#' @param input The VegObject for which to calculate the biomes.
#' @param scheme The biome scheme to use.
#' @return A VegObject with the biomes added
#' @export
#' @import data.table
#' @seealso BiomeScheme-class
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
addBiomes <-function(input, scheme){
  
  message(paste("Classifying biomes using scheme", scheme@name, sep = " "))
  
  
  # Combine shade tolerance classes and add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
  if(scheme@combineShadeTolerance) input <- combineShadeTolerance(input)
  
  # If GDD5 required for classification
  if(scheme@needGDD5 && !any(names(input@data)=="gdd5")) {
    # get gdd5
    gdd5 <- getVegSpatial(input@run, input@temporal.extent, "gdd5", reread.file = FALSE)
    dt <- input@data
    dt.gdd5 <- gdd5@data
    dt <- dt[dt.gdd5]
    input@data <- dt
  }
  
  # Get the dominant tree and dominant woody PFTs
  input <- addDominantPFT(input, do.all = TRUE, do.tree = TRUE, do.woody = FALSE)
  
  # Get the totals required
  input <-addVegTotals(input, targets = c(scheme@fraction.of.total, scheme@fraction.of.tree, scheme@fraction.of.woody, scheme@totals.needed))
  
  # Get the fractions required
  input <- addVegFractions(input, targets = scheme@fraction.of.total, denominators = list("Total"))
  input <- addVegFractions(input, targets = scheme@fraction.of.tree,  denominators = list("Tree"))
  input <- addVegFractions(input, targets = scheme@fraction.of.woody, denominators = list("Woody"))
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  
  # Apply biome rules and return
  if(scheme@id %in% names(dt)) { dt[, scheme@id := NULL, with=FALSE] }
  suppressWarnings(dt[, scheme@id := apply(dt[,,with=FALSE],FUN=scheme@rules,MARGIN=1), with = FALSE])
  input@data <- dt
  return(input)
  
}



###################################################################################
##### MAKE TOTALS (LIFEFORM, PHENOLOGY, ZONE, SEASONAL ETC...)
#'
#' Combine layers of a VegObject (or a data.table)
#' 
#' This is very useful and important function.  It doesn't actually only do totals as suggested by the name, rather it aggregates different layers 
#' of a VegObject according the the desired method (or a sensible default)
#' 
#' @param input The VegObject for which to aggregate layers.
#' @param targets The new layers to produce
#' @param method The method to use to aggregate the layers ie. "mean" or "sum".  However, the most sensible option is leave it unspecified and use the default (see below)
#' @param PFT.data If calling the function on a data.table, it is neccessary to specify a PFT set here.  Normally this function will be called on  VegObject so it is not neccessary. 
#'
#' @details
#' Whilst the \code{method} arguement can be specified for maximum flexibility, the recommended usage is not specify this arguement.
#' In this case, the function uses the default of the VegQuant object of the VegObject, which should be the sensible option.  
#' For example, per-PFT variables (such as lai or cmass) should typically be summed, 
#' but monthly variables (such as soil water content) should be average to calculate the seasonal means.
#' 
#' For convenience, both \code{targets}  will be expanded using \code{expandTargets}.
#' This allows all lifeforms totals in a VegObject to be calculated using a simple call such as
#' 
#'  \code{veg.obj <- addVegTotals(veg.obj, c("lifeforms"))}
#'  
#' See documention of \code{expandTargets} for details.
#' 
#' @return A VegObject (or data.table) with the new layers added
#' @import data.table
#' @export
#' @seealso expandsTargets getVegFractions
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
addVegTotals <- function(input, targets, method = NULL, PFT.data = NULL){
  
  Woody <- NULL
  
  ### HANDLE CLASS OF INPUT OBJECT
  # Here allow the possibility to handle both VegObjects and data.tables directly (for internal calculations)
  # MF: Maybe also handle rasters one day?
  
  # if it is a VegObject 
  if(is.VegObject(input)) {
    # We get a warning about a shallow copy here, suppress it
    suppressWarnings(dt <- input@data)
    PFT.data <- input@run@pft.set
    # also if no specfic method specified, pull it from the the VegObject
    method <- input@quant@aggregate.method
  }
  # Else assume it is a data.table
  else{
    suppressWarnings(dt <- input)
    # if a data.table has been supplied but not method, use sums, but issue a warning
    method <- rowSums
    warning("addVegTotals has been called on a data.table but the aggregation method has not been specified so assuming sum.  Is this what you wanted? ")
    message("addVegTotals has been called on a data.table but the aggregation method has not been specified so assuming sum.  Is this what you wanted? ")
    
  }
  
  
  ### SET UP THE AGGREGATE METHOD 
  if(is.null(method)) {
    method <- input@quant@aggregate.method
  }
  if(tolower(method) == "average" | tolower(method) == "avg" | tolower(method) == "mean"){
    method <- rowMeans
  }
  else if(tolower(method) == "sum"| tolower(method) == "total"){
    method <- rowSums
  }
  else {
    warning(paste("In addVegTotals() not sure how to deal with ", method, ", calculating sums instead!", sep = ""))
    message(paste("In addVegTotals() not sure how to deal with ", method, ", calculating sums instead!", sep = ""))
    method <- rowSums
  }  
  
  
  
  ### GENERAL PREPARATION
  # get PFTs present
  all.PFTs <- getPFTs(dt, PFT.data)
  
  # expands targets
  targets <- expandTargets(targets, dt, PFT.data)
  
  # remove PFTs from targets since PFTs are already present as totals
  for(PFT in all.PFTs){ if(PFT@id %in% targets) targets <- targets[-which(targets == PFT@id)]}
  
  
  
  ### FOR PER-PFT FILES
  for(target in targets){
    
    # build list of columns to combine for target
    target.cols <- c()
    for(PFT in all.PFTs){
      if(PFT@lifeform == target) {target.cols <- append(target.cols, PFT@id)}
      if(PFT@zone == target) {target.cols <- append(target.cols, PFT@id)}
      if(PFT@leafform == target) {target.cols <- append(target.cols, PFT@id)}
      if(PFT@phenology == target) {target.cols <- append(target.cols, PFT@id)}
      # Special case for Woody
      if(target == "Woody" & (PFT@lifeform == "Tree" | PFT@lifeform == "Shrub")) {target.cols <- append(target.cols, PFT@id)}
    }
    
    # now combine the relevant columns
    if(!is.null(target.cols)) suppressWarnings(dt[, eval(target) := method(.SD), .SDcols = target.cols])
    
  }
  
  
  
  ### FOR MONTHLY FILES
  # Loop through all targets to pick out the seasons/annual and make them 
  for(target in targets){
    for(period in all.periods){
      if(target == period@id){
        total.str <- quote(paste(target, sep = ""))
        suppressWarnings(dt[, eval(total.str) := method(.SD), .SDcols = period@contains])
      }
    }
  } 
  
  
  
  if(is.VegObject(input)) {
    input@data <- dt
    return(input)
  }
  else {
    return(dt)
  }
  
  
  
  
}



###################################################################################
##### MAKE PFT, LIFEFORM, PHENOLOGY ETC FRACTIONS
# TODO:  Maybe take a look at the horrible "eval(quote(paste(" syntax below
#'
#' Calculate fractions from the layers of a VegObject with respect to other layers
#' 
#' This is very useful and important function.  It is fully flexible  -both the numerator and denominator can be specified (although the denominator defaults to "Total").
#' Note that if a layer doesn't exist it will be created if possible.
#' 
#' @param input The VegObject for which to calculate the new fractional layers
#' @param targets The layers to be divided (will be calculated by \code{getVegTotals} if the don't exist)
#' @param denominators The denominator layers (will be calculated by \code{getVegTotals} if the don't exist) (defaults to just "Total")
#'
#' @details
#' Division is safe with respect to a zero denominator, the results of dividing by zero is, in this case, zero.
#' 
#' For convenience, both \code{targets} and \code{denominators} will be expanded using \code{expandTargets}.
#' This allows all lifeforms fractions in a VegObject to be calculated using a simple call such as
#' 
#'  \code{veg.obj <- addVegFractions(veg.obj, c("lifeforms")}
#'  
#' See documention of \code{expandTargets} for details. 
#' 
#' @return A VegObject (or data.table) with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @seealso expandsTargets getVegTotals

addVegFractions <- function(input, targets, denominators = list("Total")){
  
  # To avoid NOTES
  Total = NULL
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  PFTs <- getPFTs(dt, input@run@pft.set)
  
  # First, expand denominator targets and make what aren't available
  denominators <- expandTargets(denominators, dt, PFT.data)
  for(denom in denominators) {
    if(!(denom %in% names(dt))) {
      dt <- addVegTotals(dt, denom, PFT.data)
    }
  }
  
  
  # Second, expand numerator targets and make what aren't available
  targets <- expandTargets(targets, dt, PFT.data)
  for(target in targets) {
    if(!(target %in% names(dt))) {
      dt <- addVegTotals(dt, target, PFT.data)
    }
  }
  
  
  # Finally loop through all denominators and numerators and calculate the fractions
  for(denominator in denominators) {
    for(target in targets){
      
      if(denominator == "Total") 
        suppressWarnings(dt[, eval(quote(paste(target, "Fraction", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%Total])
      else
        suppressWarnings(dt[, eval(quote(paste(target, "Fraction", "Of", denominator, sep = sep.char))) := get(paste(target, sep = sep.char))%/0%get(paste(denominator, sep = sep.char))])
      
    }
  }
  
  input@data <- dt
  
  return(input)
  
}


