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



###################################################################################
######  GLOBAL VARIABLES

# I am not sure if I prefer to use " ", "", "_" or "." when constructing variable names such as "EvergreenFractionofTree"
# but here is an easy way to choose.
sep.char = ""



###################################################################################
##### EXTRACT THE PFTS PRESENT IN A RUN USING THE DATA.TABLE HEADER

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



expandTargets <- function(targets, data, PFT.data, include.woody = TRUE){
  
  # remove "Lon", "Lat" and "Year" if present
  for(remove.header.from.header in c("Lat", "Lon", "Year")){
    if(remove.header.from.header %in% targets) targets <- targets[-which(targets == remove.header.from.header)]
  }
  
  # get PFTs
  all.PFTs <- getPFTs(data, PFT.data)
  
  # expand "all"
  if("all" %in% tolower(targets)) targets <- c("pfts", "lifeforms", "leafforms", "zones", "phenologies")
  
  # expands "pfts"
  
  if("pft" %in% tolower(targets) || "pfts" %in% tolower(targets)) {
    for(PFT in all.PFTs) {
      targets <- append(targets, PFT@id)
    }
    
    if("pft" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "pft")]
    if("pfts" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "pfts")]
  }
  
  
  # expand "lifeforms" 
  if("lifeforms" %in% tolower(targets) | "lifeform" %in% tolower(targets)){
    
    # Find all lifeforms present in PFTs 
    all.lifeforms <- vector()
    for(PFT in all.PFTs) {all.lifeforms <- append(all.lifeforms, PFT@lifeform)}
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
    for(PFT in all.PFTs) {all.zones <- append(all.zones, PFT@zone)}
    all.zones <- unique(all.zones)
    
    targets <- append(targets, all.zones)
    
    if("zones" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "zones")]
    if("zone" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "zone")]    
    
  }
  
  # Expand "leafforms" 
  if("leafforms" %in% tolower(targets) | "leafform" %in% tolower(targets)){
    
    # Find all leafforms present in PFTs 
    all.leafforms <- vector()
    for(PFT in all.PFTs) {all.leafforms <- append(all.leafforms, PFT@leafform)}
    all.leafforms <- unique(all.leafforms)
    
    targets <- append(targets, all.leafforms)
    
    if("leafforms" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "leafforms")]
    if("leafform" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "leafform")] 
    
  }
  
  # Expand "phenologies" 
  if("phenologies" %in% tolower(targets) | "phenology" %in% tolower(targets)){
    
    # Find all phenologys present in PFTs 
    all.phenologies <- vector()
    for(PFT in all.PFTs) {all.phenologies <- append(all.phenologies, PFT@phenology)}
    all.phenologies <- unique(all.phenologies)
    #if("NA" in all.phenologies) all.phenologies <- all.phenologies[-which(tolower(all.phenologies) == "NA")]
    
    targets <- append(targets, all.phenologies)
    
    if("phenologies" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "phenologies")]
    if("phenology" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "phenology")] 
    
  }
  
  
  # Expand seasons
  if("seasons" %in% tolower(targets) | "season" %in% tolower(targets) | "seasonal" %in% tolower(targets)){
    
    targets <- append(targets, c("DJF", "MAM", "JJA", "SON"))
    
    if("seasons" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "seasons")]
    if("season" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "season")] 
    if("seasonal" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "seasonal")] 
    
  }
  
  
  if("NA" %in% targets) targets <- targets[-which(targets == "NA")]
  if("all" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "all")]
  
  return(targets)
  
  
}



###################################################################################
##### COMBINE SHADE-INTOLERANT PFTS WITH SHADE-TOLERANT PFTS FOR FINDING DOMINANT PFT
##### AND DOING BIOME CLASSIFICATIONS ETC


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



addDominantPFT <- function(input, do.all = TRUE, do.tree = FALSE, do.woody = FALSE){
  
  # To avoid NOTES when checking
  Dominant = NULL
  
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
######### BIOME CLASSIFICATION  ###################################################################
###################################################################################################


addBiomes <-function(input, scheme = Smith2014.scheme){
  
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
    message("Adding seasonal means")
  }
  else if(tolower(method) == "sum"| tolower(method) == "total"){
    method <- rowSums
    message("Adding seasonal averages")
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

addVegFractions <- function(input, targets = list("pfts", "lifeforms"), denominators = list("Total")){
  
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  
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





###########################################################################################
######################### SEASONAL AVERAGES/AGGREGATES


addSeasonal <- function(input, seasons = c("DJF", "MAM", "JJA", "SON", "Annual"), method = NULL, verbose = FALSE){
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  
  
  # check the input method
  if(is.null(method)) method <- input@quant@aggregate.method
  if(tolower(method) == "average" | tolower(method) == "avg" | tolower(method) == "mean"){
    if(verbose) message("Adding seasonal means")
  }
  else if(tolower(method) == "sum"| tolower(method) == "total"){
    if(verbose) message("Adding seasonal averages")
  }
  else {
    message(paste("In addSeasonal() not sure how to deal with ", method, ", calculating mean instead!", sep = ""))
    method = "mean"
  }  
  
  
  
  # for each season
  for(season.str in seasons){
    season.obj <- all.periods[[season.str]]
    total.str <- quote(paste(season.str, sep = ""))
    suppressWarnings(dt[, eval(total.str) := rowSums(.SD), .SDcols = season.obj@contains])
    if(method == "average" || method == "mean" || method == "avg") dt[, eval(total.str) := get(eval(total.str)) / length(season.obj@contains)]
  } 
  
  input@data <- dt
  
  return(input)
}



