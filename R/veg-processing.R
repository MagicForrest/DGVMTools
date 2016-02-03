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
  
  input.class <- class(input)[1]
  
  if(input.class == "VegSpatial") suppressWarnings(input.names <- names(input@data))
  else if(input.class == "data.table" | input.class == "RasterLayer" | input.class == "RasterBrick" | input.class == "RasterStack") input.names <- names(input)
  
  
  PFTs.present <- list()
  for(colname in input.names){
    for(PFT in PFT.data){
      if(PFT@name == colname) {
        PFTs.present <- append(PFTs.present, PFT)
      }
    }
  }
  
  return(PFTs.present)
  
}



expandTargets <- function(targets, data, PFT.data){
  
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
      targets <- append(targets, PFT@name)
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
    all.lifeforms <- append(all.lifeforms, "Woody")
    
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
          dt[, PFT.data[[PFT@combine]]@name := rowSums(.SD), .SDcols=c(PFT.data[[PFT@combine]]@name, PFT@name)]
          dt[, PFT@name := 0]
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
      if(PFT@name == colname) {
        if(do.all) {PFTs.present <- append(PFTs.present, colname)}
        if(do.tree && tolower(PFT@lifeform) == "tree") {tree.PFTs.present <- append(tree.PFTs.present, colname)}
        if(do.woody && (tolower(PFT@lifeform) == "tree" || tolower(PFT@lifeform) == "shrub")) {woody.PFTs.present <- append(woody.PFTs.present, colname)}
      }
    }
  }
  
  
  if(do.all) {
    PFTs.present <- unlist(PFTs.present)
    PFTs.present <- PFTs.present[-which(PFTs.present == "Total")]
    suppressWarnings(dt[, Dominant := apply(dt[,PFTs.present,with=FALSE],FUN=domPFT,MAR=1)])
    dt[Total < 0.2, eval(quote(paste("Dominant"))) := "Barren"]
    dt[, eval(quote(paste("Dominant"))) := as.factor(get("Dominant"))]
    
  }
  
  if(do.tree) {
    tree.PFTs.present <- unlist(tree.PFTs.present)
    suppressWarnings(dt[, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := apply(dt[,tree.PFTs.present,with=FALSE],FUN=domPFT,MAR=1)])
    dt[Total < 0.2, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := "Barren"]
    dt[, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := as.factor(get(paste("Dominant", "Tree", sep = sep.char)))]
  }
  
  if(do.woody) {
    woody.PFTs.present <- unlist(woody.PFTs.present)
    suppressWarnings(dt[, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := apply(dt[,woody.PFTs.present,with=FALSE],FUN=domPFT,MAR=1)])
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
    if(scheme@needGDD5) {
    # get gdd5
    gdd5 <- getVegSpatial(input@run, input@time.span, "gdd5", forceReAveraging = FALSE)
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
  input <- addVegFractions(input, targets = scheme@fraction.of.total, of.total = TRUE,   of.tree = FALSE, of.woody = FALSE)
  input <- addVegFractions(input, targets = scheme@fraction.of.tree,  of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  input <- addVegFractions(input, targets = scheme@fraction.of.woody, of.total = FALSE,  of.tree = FALSE, of.woody = TRUE)
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  
  # Apply biome rules and return
  if(scheme@id %in% names(dt)) { dt[, scheme@id := NULL, with=FALSE] }
  suppressWarnings(dt[, scheme@id := apply(dt[,,with=FALSE],FUN=scheme@rules,MAR=1), with = FALSE])
  input@data <- dt
  return(input)
  
}



###################################################################################
##### MAKE TOTALS (LIFEFORM, PHENOLOGY, ZONE, ETC...)

addVegTotals <- function(input, targets = c("lifeforms")){
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  
  # get PFTs present
  all.PFTs <- getPFTs(dt, PFT.data)
  
  # expands targets
  targets <- expandTargets(targets, dt, PFT.data)
  
  # remove PFTs from targets since PFTs are already present as totals
  for(PFT in all.PFTs){ if(PFT@name %in% targets) targets <- targets[-which(targets == PFT@name)]}
  
  # make zero columns for each target (provided they are not PFTs)
  for(target in targets){ suppressWarnings(dt[, target := 0, with= FALSE]) }
  
  # loop through PFTs and add the PFT to the appropriate target (ie add TeBS to the "Tree" column)
  for(PFT in all.PFTs){

    # lifeform
    if(PFT@lifeform %in% targets) { dt[, PFT@lifeform := rowSums(.SD), .SDcols = c(PFT@name , PFT@lifeform)] }
    # zone
    if(PFT@zone %in% targets) dt[, PFT@zone := rowSums(.SD), .SDcols = c(PFT@name , PFT@zone)]
    # leafform
    if(PFT@leafform %in% targets) dt[, PFT@leafform := rowSums(.SD), .SDcols = c(PFT@name , PFT@leafform)]
    # phenologies
    if(PFT@phenology %in% targets) dt[, PFT@phenology := rowSums(.SD), .SDcols = c(PFT@name , PFT@phenology)]
    # Woody special case 
    if("Woody" %in% targets & (PFT@lifeform == "Tree" | PFT@lifeform == "Shrub")) dt[, Woody := rowSums(.SD), .SDcols = c(PFT@name , "Woody")]
    
  }
  
  input@data <- dt
  
  return(input)
  
  
  
  
}






###################################################################################
##### MAKE PFT, LIFEFORM, PHENOLOGY ETC FRACTIONS
##### MF TODO:  Can make this much nicer by defining a list of denominators rather than a series of booleans
##### MF TODO:  Also maybe take a look at the horrible "eval(quote(paste(" syntax below
##### MF TODO:  Also maybe simple create each target using getVegTotals() if it doesn't exist

addVegFractions <- function(input, targets = c("pfts", "lifeforms"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE){
  
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  PFT.data <- input@run@pft.set
  
  # First, make Tree/Woody totals if needed and not available
  if(of.woody && !(paste("Tree", sep = sep.char) %in% names(dt))){
    dt <- addLifeformTotals(dt, PFT.data, lifeform = "Tree")
  }
  if(of.woody && !(paste("Woody", sep = sep.char) %in% names(dt))){
    dt <- addLifeformTotals(dt, PFT.data, lifeform = "Woody")
  }
  
  # Second, expand targets
  targets <- expandTargets(targets, dt, PFT.data)
  
    
  for(target in targets){
    
    # make fractions of total/tree/woody
    if(of.total) {
      suppressWarnings(dt[, eval(quote(paste(target, "Fraction", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%Total])
    }
    if(of.tree) {
      suppressWarnings(dt[, eval(quote(paste(target, "FractionofTree", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%get(paste("Tree", sep = sep.char))])
    }
    if(of.woody) {
      suppressWarnings(dt[, eval(quote(paste(target, "FractionofWoody", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%get(paste("Woody", sep = sep.char))])
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
  
  # TODO: move this so to global definitions of Period
  seasons.full <- list("DJF" = c("Dec", "Jan", "Feb"), 
                       "MAM" = c("Mar", "Apr", "May"), 
                       "JJA" = c("Jun", "Jul", "Aug"),
                       "SON" = c("Sep", "Oct", "Nov"),
                       "Annual" = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec")) 
  
  
  # for each season
  for(season in seasons){
    total.str <- quote(paste(season, sep = ""))
    suppressWarnings(dt[, eval(total.str) := rowSums(.SD), .SDcols = seasons.full[[season]]])
    if(method == "average" || method == "mean" || method == "avg") dt[, eval(total.str) := get(eval(total.str)) / length(seasons.full[[season]])]
  } 
  
  input@data <- dt
 
  return(input)
}



