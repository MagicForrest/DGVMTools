#!/usr/bin/Rscript


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
#' Extract the PFTs present in some data, given the set of possible PFTs.  The data can be represented as a data.table, ModelObject, or Raster*-object.  The 
#' It does thsi based on the names of the layers/columns of the data 
#' @param input The data from which to retrieve the PFTs present (data.table, ModelObject, or Raster*-object)
#' @param PFT.data A list of PFTs which might be present (ie. a superset of those actually present)
#' @return A list of PFT object which are actually present 
#' @export
#' @import data.table raster
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
getPFTs <- function(input, PFT.data){
  
  # Allow for rasters, Veg Objects and data.tables
  input.class <- class(input)[1]
  if(is.ModelObject(input)) suppressWarnings(input.names <- names(input@data))
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

############# EXPAND LAYERS
#'
#' Expand plotting/processing layers from convenient short hand codes.
#' 
#' Expands characters strings with particular meaning (eg. "lifefoms", "pfts", "seasons", etc...).  
#' This allows the user to simply specify, for example, "lifeforms" to a plotting command instead of c("Tree","Grass","Shrub") when plotting or processing.
#'
#' @details Supported short-hand strings are:
#' \itemize{
#'   \item{"seasons"}{ expands to c("DJF","MAM","JJA","SON")}
#'   \item{"PFTs"}{ expands to a vector of each of the ids of PFTs present in the data (requires the \code{PFT.set} argument).}
#'   \item{"lifeforms"}{ expands all the different PFT lifeforms present as determined by the lifeform slots of the \code{PFTs} argument (and optionally "Woody").  For example c("Tree", "Grass", "Woody")}
#'   \item{"zones"}{ expands all the different PFT climactic zones present as determined by the zone slots of the \code{PFTs} argument.  For example c("Temperature", "Boreal", "Tropical")}
#'   \item{"leafforms"}{ expands all the different PFT leaf forms present as determined by the leafforms slot of the \code{PFTs} argument.  For example c("Needleleved", "Broadleaved")}
#'   \item{"phenologies"}{ expands all the different PFT leaf forms present as determined by the phenology slot of the \code{PFTs} argument.  For example c("Evergreen", "Summergreen", "Raingreen", "GrassPhenology)}
#' }
#' 
#' 
#' @param layers Character vector of layers to expand
#' @param input.data The data to which the layers will be applied, may be a \code{ModelObject}, a Raster* object, data.table or data.frame, a Spatial*DataFrame. 
#' @param PFT.set List of a superset of PFTs that might be found in this run (only necessary if *not* passing a \code{ModelObject}).
#' @param type Character string identifying if this is a monthly  (= "monthly") or per PFT (="pft") variable.  Ignored for \code{ModelObjects} which supply this data themselves.
#' The function attempts to determine this argument if it is not provided.  
#' @param include.woody If TRUE and "lifeform" is included in the layer list "Woody" is also returned
#' @return A list of layers
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
expandLayers <- function(layers, input.data, PFT.set = NULL, type = "unknown", include.woody = TRUE){
  
  # remove "Lon", "Lat" and "Year" if present
  for(remove.header.from.header in c("Lat", "Lon", "Year")){
    if(remove.header.from.header %in% layers) layers <- layers[-which(layers == remove.header.from.header)]
  }
  
  # read meta-data form ModelObject if possible
  if(is.ModelObject(input.data)) {
    type <- input.data@quant@type
    if(is.null(PFT.set)) PFT.set <- input.data@run@pft.set
    header <- names(input.data@data)
  }
  else{
    header <- names(input.data)
  }
  
  # get PFTs present in data
  PFTs <- getPFTs(input.data, PFT.set)
  
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
    
    
    
    # expands "pfts"
    if("pft" %in% tolower(layers) || "pfts" %in% tolower(layers)) {
      for(PFT in PFTs) {
        layers <- append(layers, PFT@id)
      }
      
      if("pft" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "pft")]
      if("pfts" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "pfts")]
    }
    
    
    # expand "lifeforms" 
    if("lifeforms" %in% tolower(layers) | "lifeform" %in% tolower(layers)){
      
      # Find all lifeforms present in PFTs 
      all.lifeforms <- vector()
      for(PFT in PFTs) {all.lifeforms <- append(all.lifeforms, PFT@lifeform)}
      all.lifeforms <- unique(all.lifeforms)
      # MF: Special case to combine Trees and Shrubs inoo Woody category
      if(include.woody) all.lifeforms <- append(all.lifeforms, "Woody")
      
      layers <- append(layers, all.lifeforms)
      
      if("lifeforms" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "lifeforms")]
      if("lifeform" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "lifeform")]
      
    }
    
    
    # expand "zones" 
    if("zones" %in% tolower(layers) | "zone" %in% tolower(layers)){
      
      # Find all zones present in PFTs 
      all.zones <- vector()
      for(PFT in PFTs) {all.zones <- append(all.zones, PFT@zone)}
      all.zones <- unique(all.zones)
      
      layers <- append(layers, all.zones)
      
      if("zones" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "zones")]
      if("zone" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "zone")]    
      
    }
    
    # Expand "leafforms" 
    if("leafforms" %in% tolower(layers) | "leafform" %in% tolower(layers)){
      
      # Find all leafforms present in PFTs 
      all.leafforms <- vector()
      for(PFT in PFTs) {all.leafforms <- append(all.leafforms, PFT@leafform)}
      all.leafforms <- unique(all.leafforms)
      
      layers <- append(layers, all.leafforms)
      
      if("leafforms" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "leafforms")]
      if("leafform" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "leafform")] 
      
    }
    
    # Expand "phenologies" 
    if("phenologies" %in% tolower(layers) | "phenology" %in% tolower(layers)){
      
      # Find all phenologys present in PFTs 
      all.phenologies <- vector()
      for(PFT in PFTs) {all.phenologies <- append(all.phenologies, PFT@phenology)}
      all.phenologies <- unique(all.phenologies)
      #if("NA" in all.phenologies) all.phenologies <- all.phenologies[-which(tolower(all.phenologies) == "NA")]
      
      layers <- append(layers, all.phenologies)
      
      if("phenologies" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "phenologies")]
      if("phenology" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "phenology")] 
      
    }
    
    
  }
  
  else if(tolower(type) == "monthly"){
    
    # Expand seasons
    if("seasons" %in% tolower(layers) | "season" %in% tolower(layers) | "seasonal" %in% tolower(layers)){
      
      layers <- append(layers, c("DJF", "MAM", "JJA", "SON"))
      
      if("seasons" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "seasons")]
      if("season" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "season")] 
      if("seasonal" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "seasonal")] 
      
    }
    
    # Expand months/monthly
    if("months" %in% tolower(layers) | "monthly" %in% tolower(layers)){
      
      for(month in months) {
        layers <- append(layers, month@id)
      }
      
      if("months" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "months")]
      if("monthly" %in% tolower(layers)) layers <- layers[-which(tolower(layers) == "monthly")] 
      
    }
    
  }
  
  if("NA" %in% layers) layers <- layers[-which(layers == "NA")]
  
  
  return(layers)
  
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
#' @param input The ModelObject which is to have the shade tolerance classes combined.
#' @return A ModelObject with the data for the shade-intolerant PFTs set to zero but their values added to the shade-tolerant versions
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




###################################################################################################
######### BIOME CLASSIFICATION
#'
#' Perform biome classification using this ModelObject
#' 
#' This is very inflexible as it only allows the calcualtion of biomes with only one Quantity.  This is not suitable for many biomes schemes, 
#' so this will need to be re-written
#' 
#' 
#' @param input The ModelObject for which to calculate the biomes.
#' @param scheme The biome scheme to use.
#' @return A ModelObject with the biomes added
#' @export
#' @import data.table
#' @seealso BiomeScheme-class
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
addBiomes <-function(input, scheme){
  
  message(paste("Classifying biomes using scheme", scheme@name, sep = " "))
  
  Grass = NULL
  
  # Combine shade tolerance classes and add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
  if(scheme@combineShadeTolerance) input <- combineShadeTolerance(input)
  
  # If GDD5 required for classification
  if(scheme@needGDD5 && !any(names(input@data)=="GDD5")) {
    # get gdd5
    stop("input@run is not a class 'ModelRun' it is class 'ModelRunInfo'!")
    gdd5 <- getVegSpatial(input@run, "gdd5", input@temporal.extent, read.full = FALSE)
    dt <- input@data
    dt.gdd5 <- gdd5@data
    dt <- dt[dt.gdd5]
    input@data <- dt
  }

  # Get the dominant tree and dominant woody PFTs
  input <- newLayer(input, layers = scheme@max.needed, method = "max")

  if(!"Grass" %in% names(input@data) ) {
    input@data[, Grass := 0]
  }

  # Get the totals required
  input <-newLayer(input, layers = c(scheme@fraction.of.total, scheme@fraction.of.tree, scheme@fraction.of.woody, scheme@totals.needed), method = "sum")

  # Get the fractions required
  input <- divideLayers(input, layers = scheme@fraction.of.total, denominators = list("Total"))
  input <- divideLayers(input, layers = scheme@fraction.of.tree,  denominators = list("Tree"))
  input <- divideLayers(input, layers = scheme@fraction.of.woody, denominators = list("Woody"))
  
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
#' Combine layers of a ModelObject (or a data.table)
#' 
#' This is very useful and important function.  It aggregates different layers 
#' of a ModelObject according the the desired method (or a sensible default).  Please note some special features of the arguments, designed for convenience, which are described below.
#' 
#' @param input The ModelObject for which to aggregate layers.
#' @param layers The new layers to produce
#' @param method The method to use to aggregate the layers ie. "mean" or "sum".  However, it is often most sensible to leave it unspecified and use the default (see below)
#' @param PFT.data If calling the function on a data.table, it is neccessary to specify a PFT set here.  Normally this function will be called on  ModelObject so it is not neccessary. 
#'
#' @details
#' Whilst the \code{method} argument can be specified for maximum flexibility, the recommended usage is not specify this argument.
#' In this case, the function uses the default of the Quantity object of the ModelObject, which should be the sensible option.  
#' For example, per-PFT variables (such as lai or cmass) should typically be summed, 
#' but monthly variables (such as soil water content) should be average to calculate the seasonal means.
#' 
#' For convenience, both \code{layers} will be expanded using \code{expandLayers}.
#' This allows all lifeforms totals in a ModelObject to be calculated using a simple call such as
#' 
#'  \code{veg.obj <- newLayer(veg.obj, c("lifeforms"))}
#'  
#' See documention of \code{expandLayers} for details.
#' 
#' @return A ModelObject (or data.table) with the new layers added
#' @import data.table
#' @export
#' @seealso expandslayers getVegFractions
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
newLayer <- function(input, layers, method = NULL, PFT.data = NULL){

    Woody = Total = TempTotal = NULL
  
  ### HANDLE CLASS OF INPUT OBJECT
  # Here allow the possibility to handle both ModelObjects and data.tables directly (for internal calculations)
  
  # if it is a ModelObject 
  if(is.ModelObject(input)) {
    # We get a warning about a shallow copy here, suppress it
    suppressWarnings(dt <- input@data)
    PFT.data <- input@run@pft.set
    # also if no specfic method specified, pull it from the the ModelObject
    if(is.null(method)) method <- input@quant@aggregate.method
  }
  # Else assume it is a data.table
  else{
    print("INFO - using newLayer on a data.table")
    warning("INFO - using newLayer on a data.table")
    
    suppressWarnings(dt <- input)
    # if a data.table has been supplied but not method, use sums, but issue a warning
    if(is.null(method)) {
      method <- "sum"
      warning("newLayer() has been called on a data.table but the aggregation method has not been specified so assuming sum.  Is this what you wanted? ")
      message("newLayer() has been called on a data.table but the aggregation method has not been specified so assuming sum.  Is this what you wanted? ")
    }
  }

  # auxiliary function to be apply'd in the case of max and min
  max.layer <- function(x){
    the.max <- names(x)[which.max(x)]
    if(length(the.max) == 0) {
      the.max <- "Barren"
    }
    return(the.max)
  }
  
  min.layer <- function(x){
    the.min <- names(x)[which.min(x)]
    if(length(the.min) == 0) {
      the.min <- "Barren"
    }
    return(the.min)
  }
  
  
  ### SET UP THE AGGREGATE METHOD 
  method <- match.arg(method, c("average", "mean", "sum", "total", "max", "min"))
  method <- switch(method,
                   mean = rowMeans,
                   average = rowMeans,
                   sum = rowSums,
                   total = rowSums,
                   max = max.layer,
                   min = min.layer)
  
  
  # GET PFTs PRESENT
  all.PFTs <- getPFTs(dt, PFT.data)
  
  ### EXPAND LAYERS
  
  # for special case of methods "max" and "min", do not expand "months" and "PFTs" because in these cases we want to provide one layer with the min/max
  # of all months/PFT, not seperate layers for each month/PFTs
  month.present <- FALSE
  PFT.present <- FALSE
  if("Month" %in% layers) {
    layers <- layers[-which(layers == "Month")]
    month.present <- TRUE
  }
  if("PFT" %in% layers) {
    layers <- layers[-which(layers == "PFT")]
    PFT.present <- TRUE
  }
  
  # expands layer
  layers <- expandLayers(layers, dt, PFT.data)
  
  # remove PFTs from layers since a layer for a PFT is already the sum/avg/min/max for that particular PFT
  for(PFT in all.PFTs){ if(PFT@id %in% layers) layers <- layers[-which(layers == PFT@id)]}
  
  # add back in "Month" and "PFT
  if(month.present) layers <- append(layers, "Month")
  if(PFT.present) layers <- append(layers, "PFT")
  
  # DEFINE STRING FOR MIN/MAX
  if(identical(method, max.layer)) method.string <- "Max"
  if(identical(method, min.layer)) method.string <- "Min"
  
  ### FOR PER-PFT FILES
  for(this.layer in layers){

    if(this.layer != "Month") {

      # build list of columns to combine for layer
      layer.cols <- c()
      for(PFT in all.PFTs){
        if(PFT@lifeform == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        if(PFT@zone == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        if(PFT@leafform == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        if(PFT@phenology == this.layer) {layer.cols <- append(layer.cols, PFT@id)}
        # Special case for Woody
        if(this.layer == "Woody" & (PFT@lifeform == "Tree" | PFT@lifeform == "Shrub")) {layer.cols <- append(layer.cols, PFT@id)}
        # Special case for Total and PFT
        if(this.layer == "Total" | this.layer == "PFT") {layer.cols <- append(layer.cols, PFT@id)}
      }
      
      # now combine the relevant columns

      # if not requiring the maximum or minimum
      if(!identical(method, max.layer) & !identical(method, min.layer)) {

        if(!is.null(layer.cols)) suppressWarnings(dt[, eval(this.layer) := method(.SD), .SDcols = layer.cols])

      }
      
      # else it is min/max
      else{

        if("Total" %in% layer.cols) layer.cols <- layer.cols[-which(layer.cols == "Total")]
        # MF TODO: make this faster/nicer?
        suppressWarnings(dt[, eval(paste0(method.string, this.layer) ) := apply(dt[,layer.cols,with=FALSE],FUN=method,MARGIN=1)])
        dt[, eval(quote(paste0(method.string, this.layer))) := as.factor(get(paste0(method.string, this.layer)))]
       
      }
      
    } # if not month
    
  } # for each layer
  
  
  ### FOR MONTHLY FILES
  # Loop through all layers to pick out the seasons/annual and make them 
  for(layer in layers){
    
    # special case to get min/max of all months
    if(layer == "Month" & (identical(method, max.layer) | identical(method, min.layer))) {
      
      # make a vector of all months
      layer.cols <- c()
      for(month in months){
        layer.cols <- append(layer.cols, month@id)
      }
      
      # now calculate the maximum month
      suppressWarnings(dt[, eval(paste0(method.string, layer) ) := apply(dt[,layer.cols,with=FALSE],FUN=method,MARGIN=1)])
      
      # Now, calculate annual total, if this is zero, set it to max or min to "None"
      suppressWarnings(dt[, TempTotal := sum(.SD), .SDcols = layer.cols])
      dt[TempTotal == 0.0, eval(quote(paste0(method.string, this.layer))) := "None"]
      
      # set the variable to factors in a sensible order (consecutive months) for plotting nicely
      factor.order <- c()
      for(month in months) {factor.order<- append(factor.order, month@id)}
      
      dt[, eval(quote(paste0(method.string, this.layer))) := factor(get(paste0(method.string, this.layer)), levels = factor.order)]
      dt[,TempTotal := NULL]
      

    }
    
    # for other layers
    else {
      for(period in all.periods){
        if(layer == period@id){
          total.str <- quote(paste(layer, sep = ""))
          suppressWarnings(dt[, eval(total.str) := method(.SD), .SDcols = period@contains])
        }
      }
    }
  } 
  
  if(is.ModelObject(input)) {
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
#' Calculate fractions from the layers of a ModelObject with respect to other layers
#' 
#' This is very useful and important function.  It is fully flexible  -both the numerator and denominator can be specified (although the denominator defaults to "Total").
#' Note that if a layer doesn't exist it will be created if possible.
#' 
#' @param input The ModelObject for which to calculate the new fractional layers
#' @param layers The layers to be divided ie. the numerators (will be calculated by \code{getVegTotals} if the don't exist)
#' @param denominators The denominator layers (will be calculated by \code{getVegTotals} if the don't exist) (defaults to just "Total")
#' @param aggregate.method If the denominators need to be made, which method should be used to make them, "sum" or "mean", defaults to "sum".
#' parame
#'
#' @details
#' Division is safe with respect to a zero denominator, the results of dividing by zero is, in this case, zero.
#' 
#' For convenience, both \code{layers} and \code{denominators} will be expanded using \code{expandLayers}.
#' This allows all lifeforms fractions in a ModelObject to be calculated using a simple call such as
#' 
#'  \code{veg.obj <- divideLayers(veg.obj, c("lifeforms")}
#'  
#' See documention of \code{expandLayers} for details. 
#' 
#' @return A ModelObject (or data.table) with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @seealso expandslayers getVegTotals

divideLayers <- function(input, layers, denominators = list("Total"), aggregate.method = "sum"){
  
  # To avoid NOTES
  Total = NULL
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  PFTs <- getPFTs(dt, input@run@pft.set)
  
  # First, expand denominator layers and make what aren't available
  denominators <- expandLayers(denominators, dt, PFT.data)
  for(denom in denominators) {
    if(!(denom %in% names(dt))) {
      dt <- newLayer(dt, denom, PFT.data = PFT.data, method = aggregate.method)
    }
  }
  
  
  # Second, expand numerator layers and make what aren't available
  layers <- expandLayers(layers, dt, PFT.data)
  for(layer in layers) {
    if(!(layer %in% names(dt))) {
      dt <- newLayer(dt, layer, PFT.data = PFT.data, method = aggregate.method)
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


#' Copy layers from one VegObject or ModelObject to another
#' 
#' This function allows layers (reminder, they are implemented as columns in a data.table) to be copied from one ModelObject/DataObject to another.  
#' This is particularly useful for colouring or facetting a plot of one variable by another one say.  To give a more concrete example, one could use a biome classification 
#' to split (facet) a data-vs-model scatter plot.
#' 
#' @param from The ModelObject/DataObject that the layers are to be copied from.
#' @param to The ModelObject/DataObject that the layers are to be copied to.
#' @param layer.names The layers to be copied from the "from" argument
#' @param new.layer.names The new names that the layers should have in the 'to' object. Use this to avoid naming conflict whereby, for 
#' example, if a layer "Total" is copied to an object which already has a "Total" layer then they layers will be names "Total.x" and "Total.y".  
#' @param keep.all.to Boolean, if set to FALSE points in the 'to' object which don't have corresponding points in the 'from' object are removed.
#' 
#' 
#' @description This function does not check the Lon, Lat and Year columns are identical.  Any points in the 'from' object which are not in the 'to' object are ignored, 
#' and any points in the 'to' object which don't have corresponding points in the 'from' object are assigned NA, unless keep.all.to is set to FALSE .
#'
#' @return A ModelObject (or data.table) comprising the 'to' object with the new layers added
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}



copyLayers <- function(from, to, layer.names, new.layer.names = NULL, keep.all.to = TRUE) {
  
  
  # first some pre-amble and checks
  common.layers <- c()
  from.dt <- from@data
  for(dim in c("Lon", "Lat", "Year")) {
    
    if(dim %in% names(from@data) && dim %in% names(to@data)) { common.layers <- append(common.layers, dim)}         
    else if(!(dim %in% names(from@data)) && (dim %in% names(to@data))) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"to\" argument but not the \"from\" argument") 
    }
    else if(dim %in% names(from@data) && !(dim %in% names(to@data))) {
      stop(paste0("In copyLayers: Can't copy layers because \""), dim ,"\" is present in the \"from\" argument but not the \"to\" argument") 
    }
  }
  
  
  layers.to.add.dt <- from@data[, append(common.layers, layer.names), with=FALSE]
  if(!is.null(new.layer.names)) setnames(layers.to.add.dt, append(common.layers, new.layer.names))
  Temp.dt <- merge(x = to@data, y = layers.to.add.dt, all.x = FALSE, all.y = keep.all.to)
  to@data <- Temp.dt
  return(to)
  
}
