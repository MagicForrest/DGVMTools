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

require(compiler)


###################################################################################
######  GLOBAL VARIABLES

# I am not sure if I prefer to use " ", "", "_" or "." when constructing variable names such as "EvergreenFractionofTree"
# but here is an easy way to choose.
sep.char = ""



###################################################################################
##### EXTRACT THE PFTS PRESENT IN A RUN USING THE DATA.TABLE HEADER

getPFTs <- function(input, PFT.data = PFT_MASTER_LIST){
  
  PFTs.present <- list()
  for(colname in names(input)){
    for(PFT in PFT.data){
      if(PFT@name == colname) {
        PFTs.present <- append(PFTs.present, PFT)
      }
    }
  }
  
  return(PFTs.present)
  
}



expandTargets <- function(targets, input.dt, PFT.data){
  
  # expand "all"
  if("all" %in% tolower(targets)) targets <- c("pfts", "lifeforms", "leafforms", "zones", "phenologies")
  
  # expands "pfts"
  if("pft" %in% targets || "pfts" %in% targets) {
    message("PFTs")
    all.PFTs <- getPFTs(input.dt, PFT.data)
    for(PFT in all.PFTs) {targets <- append(targets, PFT@name)}
    if("pft" %in% targets) targets <- targets[-which(targets == "pft")]
    if("pfts" %in% targets) targets <- targets[-which(targets == "pfts")]
  }
  
  
  # expand "lifeforms" 
  if("lifeforms" %in% tolower(targets) | "lifeform" %in% tolower(targets)){
    
    # Find all lifeforms present in PFTs 
    all.PFTs <- getPFTs(input.dt, PFT.data)
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
    all.PFTs <- getPFTs(input.dt, PFT.data)
    all.zones <- vector()
    for(PFT in all.PFTs) {all.zones <- append(all.zones, PFT@zone)}
    all.zones <- unique(all.zones)
    
    targets <- append(targets, all.zones)
    
    if("zones" %in% targets) targets <- targets[-which(tolower(targets) == "zones")]
    if("zone" %in% targets) targets <- targets[-which(tolower(targets) == "zone")]    
    
  }
  
  # Expand "leafforms" 
  if("leafforms" %in% tolower(targets) | "leafform" %in% tolower(targets)){
    
    # Find all leafforms present in PFTs 
    all.PFTs <- getPFTs(input.dt, PFT.data)
    all.leafforms <- vector()
    for(PFT in all.PFTs) {all.leafforms <- append(all.leafforms, PFT@leafform)}
    all.leafforms <- unique(all.leafforms)
    
    targets <- append(targets, all.leafforms)
    
    if("leafforms" %in% targets) targets <- targets[-which(tolower(targets) == "leafforms")]
    if("leafform" %in% targets) targets <- targets[-which(tolower(targets) == "leafform")] 
    
  }
  
  # Expand "phenologies" 
  if("phenologies" %in% tolower(targets) | "phenology" %in% tolower(targets)){
    
    # Find all phenologys present in PFTs 
    all.PFTs <- getPFTs(input.dt, PFT.data)
    all.phenologies <- vector()
    for(PFT in all.PFTs) {all.phenologies <- append(all.phenologies, PFT@phenology)}
    all.phenologies <- unique(all.phenologies)
    #if("NA" in all.phenologies) all.phenologies <- all.phenologies[-which(tolower(all.phenologies) == "NA")]
    
    targets <- append(targets, all.phenologies)
    
    if("phenologies" %in% targets) targets <- targets[-which(tolower(targets) == "phenologies")]
    if("phenology" %in% targets) targets <- targets[-which(tolower(targets) == "phenology")] 
    
  }
  
  
  if("NA" %in% targets) targets <- targets[-which(targets == "NA")]
  if("all" %in% tolower(targets)) targets <- targets[-which(tolower(targets) == "all")]
  
  return(targets)
  
  
}



###################################################################################
##### COMBINE SHADE-INTOLERANT PFTS WITH SHADE-TOLERANT PFTS FOR FINDING DOMINANT PFT
##### AND DOING BIOME CLASSIFICATIONS ETC


combineShadeTolerance <- function(input.dt, PFT.data){
  
  for(colname in names(input.dt)){
    
    # if the PFT is in the PFT list
    if(!is.null(PFT.data[[colname]])) {
      PFT <- PFT.data[[colname]]
      
      # if PFT is to be combined
      if(tolower(PFT@combine) != "no"){
        
        # if PFT to be added is present the combine them and set the shade intolerant PFT to zero, if not send a warning and do nothing
        if(!is.null(PFT.data[[PFT@combine]])){
          input.dt[, PFT.data[[PFT@combine]]@name := rowSums(.SD), .SDcols=c(PFT.data[[PFT@combine]]@name, PFT@name)]
          input.dt[, PFT@name := 0]
        }
        else{
          warning(paste("PFT", PFT, "is supposed to be combined with PFT", PFT@combine, "but that PFT is not present so ignoring.", sep = " " ))
          
        }
      } # if the PFT is to be combined
    } # if the colname is a PFT
  } # for each column name
  
  return(input.dt)
  
}

###################################################################################
##### FIND THE DOMINANT PFT (OF TOTAL AND/OR TREE AND/OR WOODY)
##### Notes: Could make this nicer using a list of inputs ("Total", "Tree", "Woody") that can be looped over
##### TODO   Low priority



addDominantPFT <- function(input.dt, PFT.data, do.all = TRUE, do.tree = FALSE, do.woody = FALSE){
  
  # auxiliary function to be apply'd
  domPFT <- function(x){return(names(x)[which.max(x)])  }  
  
  # get colnames of PFTs
  PFTs.present <- list()
  tree.PFTs.present <- list()
  woody.PFTs.present <- list()
  for(colname in names(input.dt)){
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
    input.dt[, Dominant := apply(input.dt[,PFTs.present,with=FALSE],FUN=domPFT,MAR=1)]
    input.dt[Total < 0.2, eval(quote(paste("Dominant"))) := "Barren"]
    input.dt[, eval(quote(paste("Dominant"))) := as.factor(get("Dominant"))]
    
  }
  
  if(do.tree) {
    tree.PFTs.present <- unlist(tree.PFTs.present)
    input.dt[, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := apply(input.dt[,tree.PFTs.present,with=FALSE],FUN=domPFT,MAR=1)]
    input.dt[Total < 0.2, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := "Barren"]
    input.dt[, eval(quote(paste("Dominant", "Tree", sep = sep.char))) := as.factor(get(paste("Dominant", "Tree", sep = sep.char)))]
  }
  
  if(do.woody) {
    woody.PFTs.present <- unlist(woody.PFTs.present)
    input.dt[, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := apply(input.dt[,woody.PFTs.present,with=FALSE],FUN=domPFT,MAR=1)]
    input.dt[Total < 0.2, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := "Barren"]
    input.dt[, eval(quote(paste("Dominant", "Woody", sep = sep.char))) := as.factor(get(paste("Dominant", "Woody", sep = sep.char)))]
    
    
  }
  
  return(input.dt)
  
}


###################################################################################
##### MAKE LIFEFORM, PHENOLOGY ETC TOTALS
##### Notes: The following functions perform adequately but could be combined into one function for better usability/maintenence
##### TODO   Low Priority 



addLifeformTotals <- function(input.dt, PFT.data, lifeforms = c("Tree", "Grass", "Shrub", "Woody")){
  
  warning("addLifeformTotals is obselete,call addVegTotals() with targets = 'lifeforms")
  
  do.woody <- FALSE
  
  # for each lifeform
  for(lifeform in lifeforms){
    if(tolower(lifeform) ==  "woody"){
      do.woody <- TRUE
    }
    else {
      total.str <- quote(paste(lifeform, sep = ""))
      input.dt[, eval(total.str) := 0]
      # for each column
      for(PFT in names(input.dt)){
        # if it is a PFT add it to the lifeform total
        if(!is.null(PFT.data[[PFT]]) && tolower(PFT.data[[PFT]]@lifeform) == tolower(lifeform)){
          input.dt[, eval(total.str) := get(PFT.data[[PFT]]@name) + get(eval(total.str))]
        } # if it is a PFT add it to the lifeform total
      } # for each column
    } # is or is not woody
  } #for each lifeform
  
  
  # special case for woody, just add tree and shrub totals
  if(do.woody){
    # Fist make Tree/Shrubs totals if needed and not available
    if(!(paste("Tree", sep = sep.char) %in% names(input.dt))){
      input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Tree")
    }
    if(!(paste("Shrub", sep = sep.char) %in% names(input.dt))){
      input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Shrub")
    }
    input.dt[, eval(quote(paste("Woody", sep = sep.char))) := get(eval(paste("Tree", sep = sep.char))) + get(eval(paste("Shrub", sep = sep.char)))]
  }
  
  return(input.dt)
}


addPhenologyTotals <- function(input.dt, PFT.data, phenologies = c("Evergreen", "Raingreen", "Summergreen")){
  
  warning("addPhenologyTotals is obselete,call addVegTotals() with targets = c('phenology')")
  
  
  # for each phenology
  for(phenology in phenologies){
    total.str <- quote(paste(phenology, sep = ""))
    input.dt[, eval(total.str) := 0]
    # for each column
    for(PFT in names(input.dt)){
      # if it is a PFT add it to the lifeform total
      if(!is.null(PFT.data[[PFT]]) && tolower(PFT.data[[PFT]]@phenology) == tolower(phenology)){
        input.dt[, eval(total.str) := get(PFT.data[[PFT]]@name) + get(eval(total.str))]
      } # if it is a PFT add it to the phenology total
    } # for each column
  } #for each phenology
  
  return(input.dt)  
}


addZoneTotals <- function(input.dt, PFT.data, zones = c("Boreal", "Temperate", "Tropical")){
  
  warning("addZoneTotals is obselete,call addVegTotals() with targets = c('zones')")
  
  
  # for each climate zone
  for(zone in zones){
    total.str <- quote(paste(zone, sep = ""))
    input.dt[, eval(total.str) := 0]
    # for each column
    for(PFT in names(input.dt)){
      # if it is a PFT add it to the zone total
      if(!is.null(PFT.data[[PFT]]) && tolower(PFT.data[[PFT]]@zone) == tolower(zone)){
        input.dt[, eval(total.str) := get(PFT.data[[PFT]]@name) + get(eval(total.str))]
      } # if it is a PFT add it to the zone total
    } # for each column
  } #for each zone
  
  return(input.dt)
}

addAllVegTotals <- function(input.dt, PFT.data){
  
  warning("addAllVegTotals is obselete,call addVegTotals() with targets = c('all')")
  
  input.dt <- addLifeformTotals(input.dt, PFT.data)
  input.dt <- addPhenologyTotals(input.dt, PFT.data)
  input.dt <- addZoneTotals(input.dt, PFT.data)
  
  return(input.dt)
}


# This function superceeds all "getTotal" functions above
addVegTotals <- function(input.dt, PFT.data, targets = c("pfts", "lifeforms")){
  
  # get PFTs present
  all.PFTs <- getPFTs(input.dt, PFT.data)
  
  # expands targets
  targets <- expandTargets(targets, input.dt, PFT.data)
  
  # remove PFTs from targets since PFTs are already present as totals
  for(PFT in all.PFTs){ if(PFT@name %in% targets) targets <- targets[-which(targets == PFT@name)]}
  
  # make zero columns for each target (provided they are not PFTs)
  for(target in targets){ input.dt[, target := 0, with= FALSE] }
  
  # loop through PFTs and add the PFT to the appropriate target (ie add TeBS to the "Tree" column)
  for(PFT in all.PFTs){

    # lifeform
    if(PFT@lifeform %in% targets) { input.dt[, PFT@lifeform := rowSums(.SD), .SDcols = c(PFT@name , PFT@lifeform)] }
    # zone
    if(PFT@zone %in% targets) input.dt[, PFT@zone := rowSums(.SD), .SDcols = c(PFT@name , PFT@zone)]
    # leafform
    if(PFT@leafform %in% targets) input.dt[, PFT@leafform := rowSums(.SD), .SDcols = c(PFT@name , PFT@leafform)]
    # phenologies
    if(PFT@phenology %in% targets) input.dt[, PFT@phenology := rowSums(.SD), .SDcols = c(PFT@name , PFT@phenology)]
    # Woody special case 
    if("Woody" %in% targets & (PFT@lifeform == "Tree" | PFT@lifeform == "Shrub")) input.dt[, Woody := rowSums(.SD), .SDcols = c(PFT@name , "Woody")]
    
  }
  
}



###################################################################################
##### MAKE PFT, LIFEFORM, PHENOLOGY ETC FRACTIONS
#####
##### NOTE:  PFT and lifeform fraction are fraction of total
#####          





addPFTFractions <- function(input.dt, PFT.data = PFT_MASTER_LIST){
  
  PFTs <- getPFTs(input.dt, PFT.data)
  for(PFT in PFTs){
    fraction.str <- quote(paste(PFT@name, "Fraction", sep = ""))
    input.dt[, eval(fraction.str) := get(PFT@name)%/0%Total]
  }
  
  return(input.dt)
  
}


addLifeformFractions <- function(input.dt, PFT.data, lifeforms = c("Tree", "Grass", "Shrub", "Woody"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE){
  
  # Fist make Tree/Woody totals if needed and not available
  if(of.woody && !(paste("Tree", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Tree")
  }
  if(of.woody && !(paste("Woody", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Woody")
  }
  
  
  for(lifeform in lifeforms){
    
    # if lifeform total not already present then make it
    if(!(paste(lifeform, "Total", sep = sep.char) %in% names(input.dt))){
      input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = lifeform)
    }
    
    # make fractions of total/tree/woody
    if(of.total) {
      input.dt[, eval(quote(paste(lifeform, "Fraction", sep = sep.char))) := get(paste(lifeform, sep = sep.char))%/0%Total]
      input.dt[!is.finite(get(paste(lifeform, "Fraction", sep = sep.char))) , get(paste(lifeform, "Fraction", sep = sep.char)) := 0]
    }
    if(of.tree) {
      input.dt[, eval(quote(paste(lifeform, "FractionofTree", sep = sep.char))) := get(paste(lifeform, sep = sep.char))%/0%get(paste("Tree", sep = sep.char))]
      input.dt[!is.finite(get(paste(lifeform, "FractionofTree", sep = sep.char))) , get(paste(lifeform, "FractionofTree", sep = sep.char)) := 0]
    }
    if(of.woody) {
      input.dt[, eval(quote(paste(lifeform, "FractionofWoody", sep = sep.char))) := get(paste(lifeform, sep = sep.char))%/0%get(paste("Woody", sep = sep.char))]
      input.dt[!is.finite(get(paste(lifeform, "FractionofWoody", sep = sep.char))) , get(paste(lifeform, "FractionofWoody", sep = sep.char)) := 0]
    }
    
  }
  
  return(input.dt)
  
}


# This function superceeds all "getFraction" fucntions here
addVegFractions <- function(input.dt, PFT.data, targets = c("pfts", "lifeforms"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE){
  
  # First, make Tree/Woody totals if needed and not available
  if(of.woody && !(paste("Tree", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Tree")
  }
  if(of.woody && !(paste("Woody", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Woody")
  }
  
  # Second, expand targets
  targets <- expandTargets(targets, input.dt, PFT.data)
  
  
  
  #   # Make each PFT fraction
  #   if("pft" %in% targets || "pfts" %in% targets) {
  #     message("PFTs")
  #     all.PFTs <- getPFTs(input.dt, PFT.data)
  #     for(PFT in all.PFTs) {targets <- append(targets, PFT@name)}
  #     if("pft" %in% targets) targets <- targets[-which(targets == "pft")]
  #     if("pfts" %in% targets) targets <- targets[-which(targets == "pfts")]
  #   }
  #   
  #   # Expand lifeforms if necessary and calculate total if necessary
  #   if("lifeforms" %in% tolower(targets)){
  #     lpj.lifeforms <- c("Tree", "Grass", "Shrub")
  #     targets <- append(targets, lpj.lifeforms)
  #     targets <- targets[-which(tolower(targets) == "lifeforms")]
  #   }
  
  for(target in targets){
    
    # make fractions of total/tree/woody
    if(of.total) {
      input.dt[, eval(quote(paste(target, "Fraction", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%Total]
    }
    if(of.tree) {
      input.dt[, eval(quote(paste(target, "FractionofTree", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%get(paste("Tree", sep = sep.char))]
    }
    if(of.woody) {
      input.dt[, eval(quote(paste(target, "FractionofWoody", sep = sep.char))) := get(paste(target, sep = sep.char))%/0%get(paste("Woody", sep = sep.char))]
    }
    
  }
  
  return(input.dt)
  
}


addPhenologyFractions <- function(input.dt, PFT.data,  phenologies = c("Evergreen", "Raingreen", "Summergreen"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE){
  
  # Fist make Tree/Woody totals if needed and not available
  if(of.woody && !(paste("Tree", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Tree")
  }
  if(of.woody && !(paste("Woody", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Woody")
  }
  
  
  for(phen in phenologies){
    
    if(!(paste(phen, "Total", sep = sep.char) %in% names(input.dt))){
      input.dt <- addPhenologyTotals(input.dt, PFT.data, phenologies = phen)
    }
    
    # make fractions of total/tree/woody
    if(of.total) {
      input.dt[, eval(quote(paste(phen, "Fraction", sep = sep.char))) := get(paste(phen, sep = sep.char)) %/0% Total]
      #input.dt[!is.finite(eval(quote(paste(phen, "Fraction", sep = sep.char)))) , eval(quote(paste(phen, "Fraction", sep = sep.char))) := 0]
    }
    if(of.tree) {
      input.dt[, eval(quote(paste(phen, "FractionofTree", sep = sep.char))) := get(paste(phen, sep = sep.char)) %/0% get(paste("Tree", sep = sep.char))]
      #input.dt[!is.finite(eval(quote(paste(phen, "FractionofTree", sep = sep.char)))) , eval(quote(paste(phen, "FractionofTree", sep = sep.char))) := 0]
    }
    if(of.woody) {
      input.dt[, eval(quote(paste(phen, "FractionofWoody", sep = sep.char))) := get(paste(phen, sep = sep.char)) %/0% get(paste("Woody", sep = sep.char))]
      #input.dt[!is.finite(eval(quote(paste(phen, "FractionofWoody", sep = sep.char)))) , eval(quote(paste(phen, "FractionofWoody", sep = sep.char))) := 0]
    }
    
  }
  
  return(input.dt)
  
}

addZoneFractions <- function(input.dt, PFT.data,  zones = c("Boreal", "Temperate", "Tropical"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE){
  
  # Fist make Tree/Woody totals if needed and not available
  if(of.woody && !(paste("Tree", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Tree")
  }
  if(of.woody && !(paste("Woody", sep = sep.char) %in% names(input.dt))){
    input.dt <- addLifeformTotals(input.dt, PFT.data, lifeform = "Woody")
  }
  
  
  for(zone in zones){
    
    if(!(paste(zone, sep = sep.char) %in% names(input.dt))){
      input.dt <- addZoneTotals(input.dt, PFT.data, zone = zone)
    }
    
    # make fractions of total/tree/woody
    if(of.total) {
      input.dt[, eval(quote(paste(zone, "Fraction", sep = sep.char))) := get(paste(zone, "Total", sep = sep.char)) %/0% Total]
      #input.dt[!is.finite(eval(quote(paste(zone, "Fraction", sep = sep.char)))) , eval(quote(paste(zone, "Fraction", sep = sep.char))) := 0]
    }
    if(of.tree) {
      input.dt[, eval(quote(paste(zone, "FractionofTree", sep = sep.char))) := get(paste(zone, sep = sep.char)) %/0% get(paste("Tree", sep = sep.char))]
      #input.dt[!is.finite(eval(quote(paste(zone, "FractionofTree", sep = sep.char)))) , eval(quote(paste(zone, "FractionofTree", sep = sep.char))) := 0]
    }
    if(of.woody) {
      input.dt[, eval(quote(paste(zone, "FractionofWoody", sep = sep.char))) := get(paste(zone, sep = sep.char)) %/0% get(paste("Woody", sep = sep.char))]
      #input.dt[!is.finite(eval(quote(paste(zone, "FractionofWoody", sep = sep.char)))) , eval(quote(paste(zone, "FractionofWoody", sep = sep.char))) := 0]
    }
    
  }
  
  return(input.dt)
  
}





addAllVegFractions <- function(input.dt, PFT.data, PFTs = TRUE, lifeforms = TRUE, phenologies = TRUE, zones = TRUE){
  
  input.dt <- addPFTFractions(input.dt, PFT.data)
  input.dt <- addLifeformFractions(input.dt, PFT.data)
  input.dt <- addPhenologyFractions(input.dt, PFT.data)
  input.dt <- addZoneFractions(input.dt, PFT.data)
  
  return(input.dt)
  
}




#############################################################
#####  GLOBAL BIOME CLASSIFICATION


addGlobalBiomes <-function(input.dt, PFT.data){
  
  
  if("GlobalBiome" %in% names(input.dt)) { input.dt[, GlobalBiome := NULL] }
  
  # Combine shade tolerance classes and add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
  combineShadeTolerance(input.dt, global.PFTs)
  addDominantPFT(input.dt, PFT.data, do.all = FALSE, do.tree = TRUE, do.woody = FALSE)
  addAllVegTotals(input.dt, global.PFTs)
  addVegFractions(input.dt, PFT.data, targets = c("pft"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Tropical"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Grass"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE)
  
  
  # Apply biome rules and return
  input.dt[, GlobalBiome := apply(input.dt[,,with=FALSE],FUN=GlobalBiomeRules,MAR=1)]
  return(input.dt)
  
}

addGlobalBiomesWithShrubs <-function(input.dt, PFT.data){
  
  
  if("GlobalBiome" %in% names(input.dt)) { input.dt[, GlobalBiome := NULL] }
  
  # Combine shade tolerance classes and add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
  combineShadeTolerance(input.dt, global.PFTs)
  addDominantPFT(input.dt, PFT.data, do.all = TRUE, do.tree = TRUE, do.woody = TRUE)
  addAllVegTotals(input.dt, global.PFTs)
  addVegFractions(input.dt, PFT.data, targets = c("pft"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Tropical"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Grass"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Shrub"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE)
  
  # Apply biome rules and return
  input.dt[, GlobalBiome := apply(input.dt[,,with=FALSE],FUN=GlobalBiomeRules.withshrubs,MAR=1)]
  return(input.dt)
  
}


GlobalBiomeRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionofTree']]) > 0.6 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionofTree']]) > 0.6 | as.numeric(lai[['TrBRFractionofTree']])) & (lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(2)}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionofTree']] )> 0.5 &  (lai[['DominantTree']] == "TrBE" | lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(3)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(4)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  lai[['DominantTree']] == "BNS") {return(5)}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionofTree']]) > 0.5 | as.numeric(lai[['TeBSFractionofTree']]) > 0.5) & lai[['DominantTree']] == "TeBE") {return(6)}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionofTree']]) > 0.5 | as.numeric(lai[['TeBSFractionofTree']]) > 0.5) & lai[['DominantTree']] == "TeBS") {return(7)}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(8) }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(9)}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(10)}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return(11)}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return(12)}
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & as.numeric(lai[['Lat']]) >= 54) {return(13)}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(14)}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Tree']]) > 0.2 & as.numeric(lai[['Grass']]) < 1.0) {return(16)}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(lai[['Grass']]) > 0.2) {return(15)}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Total']]) > 0.2) {return(16)}
  
  # BIOME 17 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(17)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
  
  
}


GlobalBiomeRules.withshrubs <- function(lai){
  
  ########################################################################################
  ####### FOREST BIOMES WITH TREE LAI OVER 2.5 (EXCEPT FOR BOREAL WHICH REQUIRE > 0.5)
  
  # BIOME 1 - Tropical Rain Forest
  if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionofTree']]) > 0.6 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionofTree']]) > 0.6 | as.numeric(lai[['TrBRFractionofTree']])) & (lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(2)}
  
  # BIOME 3 - Tropical Seasonal Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionofTree']] )> 0.5 &  (lai[['DominantTree']] == "TrBE" | lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(3)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 1.5 &  (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(4)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 1.5 &  lai[['DominantTree']] == "BNS") {return(5)}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionofTree']]) > 0.5 | as.numeric(lai[['TeBSFractionofTree']]) > 0.5) & lai[['DominantTree']] == "TeBE") {return(6)}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  (as.numeric(lai[['TeBEFractionofTree']]) > 0.5 | as.numeric(lai[['TeBSFractionofTree']]) > 0.5) & lai[['DominantTree']] == "TeBS") {return(7)}
  
  # BIOME 8 - Temperate/Boreal Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(8) }
  
  # BIOME 9 - Temperate Mixed Forest
  else if(as.numeric(lai[['Tree']]) > 2.5) {return(9)}
  
  ########################################################################################
  ####### INTERMEDIATE BIOMES WHICH REQUIRE WOODY > 0.5 (TUNDRA, WOODLANDS, SHRUBSLANDS, SAVANNA)
  
  # BIOME 13 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Woody']]) > 0.5 & lai[['DominantWoody']] == "BESh") {return(13)}
  
  # BIOME 10 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(10)}
  
  # BIOME 11 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return(11)}
  
  # BIOME 12 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return(12)}
  
  # BIOME 13 - Arctic/alpine Tundra
  #else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & as.numeric(lai[['Lat']]) >= 54) {return(13)}
  
  # BIOME 14 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(14)}
  
  # BIOME 16 (1) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Tree']]) > 0.2 & as.numeric(lai[['Grass']]) < 1.0) {return(16)}
  
  # BIOME 15 - Dry Grassland
  else if(as.numeric(lai[['Grass']]) > 0.2) {return(15)}
  
  # BIOME 16 (2) - Arid Shrubland/Steppe
  else if(as.numeric(lai[['Total']]) > 0.2) {return(16)}
  
  # BIOME 17 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(17)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
  
  
}



GlobalBiome.str <- c("Tropical Rain Forest",                     
                     "Tropical Deciduous Forest", 
                     "Tropical Seasonal Forest", 
                     "Boreal Evergreen Forest/Woodland", 
                     "Boreal Deciduous Forest/Woodland",
                     "Temperate Broadleaved Evergreen Forest",
                     "Temperate Deciduous Forest",
                     "Temperate/Boreal Mixed Forest",
                     "Temperate Mixed Forest",
                     "Xeric Woodland/Shrubland",
                     "Moist Savanna",
                     "Dry Savanna",
                     "Arctic/Alpine Tundra",
                     "Tall Grassland",
                     "Dry Grassland",
                     "Arid Shrubland/Steppe",
                     "Desert")


GlobalBiome.cols <- c("Tropical Rain Forest" = "seagreen",                     
                      "Tropical Deciduous Forest" = "orange3", 
                      "Tropical Seasonal Forest" = "green3", 
                      "Boreal Evergreen Forest/Woodland" = "turquoise4",
                      "Boreal Deciduous Forest/Woodland"= "cyan",
                      "Temperate Broadleaved Evergreen Forest" = "dodgerblue3",
                      "Temperate Deciduous Forest" = "chartreuse",
                      "Temperate/Boreal Mixed Forest" = "seagreen1",
                      "Temperate Mixed Forest" =  "darkseagreen1",
                      "Xeric Woodland/Shrubland" = "deeppink3",
                      "Moist Savanna" = "olivedrab2",
                      "Dry Savanna" = "goldenrod2",
                      "Arctic/Alpine Tundra" = "mediumpurple1",
                      "Tall Grassland" =  "gold",                     
                      "Dry Grassland" = "lightgoldenrod2",
                      "Arid Shrubland/Steppe"= "lightcyan",
                      "Desert" = "grey75")



###########################################################################################
######################### SEASONAL AVERAGES/AGGREGATES


addSeasonalTotals <- function(input.dt, seasons = c("DJF", "MAM", "JJA", "SON", doAnnual = TRUE)){
  
  seasons.full <- list("DJF" = c("Dec", "Jan", "Feb"), 
                       "MAM" = c("Mar", "Apr", "May"), 
                       "JJA" = c("Jun", "Jul", "Aug"),
                       "SON" = c("Sep", "Oct", "Nov")) 
  
  # for each season
  for(season in seasons){
    total.str <- quote(paste(season, sep = ""))
    input.dt[, eval(total.str) := 0]
    # for each column
    for(month in names(input.dt)){
      # if it is a PFT add it to the lifeform total
      if(month %in% seasons.full[[season]]){
        input.dt[, eval(total.str) := get(month) + get(eval(total.str))]
      } # if it is a PFT add it to the phenology total
    } # for each column
  } #for each phenology
  
  return(input.dt)  
}


addSeasonalAverage <- function(input.dt, seasons = c("DJF", "MAM", "JJA", "SON"), doAnnual = TRUE){
  
  seasons.full <- list("DJF" = c("Dec", "Jan", "Feb"), 
                       "MAM" = c("Mar", "Apr", "May"), 
                       "JJA" = c("Jun", "Jul", "Aug"),
                       "SON" = c("Sep", "Oct", "Nov")) 
  
  if(doAnnual) {
    annual.str <- quote(paste("Annual", sep = ""))
    input.dt[, eval(annual.str) := 0]
  }
  
  # for each season
  for(season in seasons){
    total.str <- quote(paste(season, sep = ""))
    input.dt[, eval(total.str) := 0]
    # for each column
    for(month in names(input.dt)){
      # if it is a PFT add it to the lifeform total
      if(month %in% seasons.full[[season]]){
        input.dt[, eval(total.str) := get(month)/3 + get(eval(total.str))]
        input.dt[, eval(annual.str) := get(month)/12 + get(eval(annual.str))]
      } # if it is a PFT add it to the phenology total
    } # for each column
  } #for each phenology
  
  return(input.dt)  
}


addSeasonal <- function(input.dt, seasons = c("DJF", "MAM", "JJA", "SON", "Annual"), method = "average", verbose = FALSE){
  
  # check the input method
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
    input.dt[, eval(total.str) := rowSums(.SD), .SDcols = seasons.full[[season]]]
    if(method == "average" || method == "mean" || method == "avg") input.dt[, eval(total.str) := get(eval(total.str)) / length(seasons.full[[season]])]
  } 
  
  return(input.dt)  
}







#############################################################################################################
############################ COMPARE TWO RASTERS 


compareTwoRastersStats <- function(data.raster, model.raster){
  
  
  model.raster <- mask(model.raster, data.raster)
  data.raster <- mask(data.raster, model.raster)
  
  # difference raster
  diff <- model.raster - data.raster
  
  # calculated mean, SD, MSE, RMSE, R^2, Pearson correlation etc
  mean.diff <- cellStats(diff, stat= mean , na.rm=TRUE, asSample=FALSE)
  sd.diff <- cellStats(diff, stat= sd , na.rm=TRUE, asSample=FALSE)
  sd.observed <- cellStats(data.raster, stat= sd , na.rm=TRUE, asSample=FALSE)
  var.observed <- sd.observed^2
  MSE <- cellStats(diff^2, stat= sum, na.rm=TRUE, asSample=FALSE)/ cellStats(is.finite(diff), stat= sum, asSample=FALSE)
  RMSE <- (MSE)^(0.5)
  R.squ <- 1 - (MSE/var.observed)
  
  P.cor.full <- layerStats(stack(data.raster, model.raster), 'pearson', na.rm=TRUE, asSample=FALSE)
  P.cor <- P.cor.full$`pearson correlation coefficient`[1,2]
  
  perc.diff.raster <- (diff / data.raster) * 100
  comparison.results <- list(diff.raster = diff, perc.diff.raster = perc.diff.raster, data.raster = data.raster, model.raster = model.raster, R.squ = R.squ, P.cor = P.cor, RMSE = RMSE, mean.diff = mean.diff, sd.diff = sd.diff)
  
  return(comparison.results)
  
}

compareTwoRasters <- function(combined.stack, dataset.name = "", model.name = "LPJ.GUESS", run = NULL, diff.breaks = NULL, plot.breaks = NULL, histo.plot.range = NULL, doScatterPlots=TRUE, doHistoPlots=TRUE, period = NULL, quant = NULL, ignore.raster = NULL, map.layout.objs = NULL, ...){
  
  warning("+++ Maybe use compareRunToSpatialDataset() ?!?!")
  
  
  # make difference map and a stack to plot
  if(!is.null(ignore.raster)){ combined.stack <- mask(combined.stack, ignore.raster)}
  # also mask the data to exlude areas on NA or NaNs in model, doesn't makes sense to show data where we have no model to compare
  combined.stack <- mask(subset(combined.stack, 1), combined.stack)
  
  
  model.raster <- subset(combined.stack, 1)
  data.raster <- subset(combined.stack, 2)
  
  
  comparison.results <- compareTwoRastersStats(data.raster, model.raster)
  
  ##### DIFFERENCE MAP
  plotLPJMaps(comparison.results$diff.raster,
              quant = quant, 
              period = period, 
              doSummary = TRUE, 
              doIndividual = TRUE, 
              run = run,
              special = "diff",
              special.string = names(data.raster),
              maxpixels = 1000000,
              layout.objs = map.layout.objs,
              ...)
  
  
  
  
  ##### ABSOLUTE MAPS
  plotLPJMaps(combined.stack,
              which.layers = "all",
              quant = quant, 
              period = period, 
              doSummary = TRUE, 
              doIndividual = TRUE, 
              run = run,
              summary.file.name = paste(quant@id, "comp", names(data.raster), "2-up", sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              layout.objs = map.layout.objs,
              ...)
  
  
  
  
  
  ##### HISTOS
  if(doHistoPlots){
    
    plotHistoComparison(model = model.raster, 
                        data = data.raster, 
                        stat.results = comparison.results,
                        run = run, 
                        period = period,
                        data.name = names(data.raster), 
                        quant = quant, 
                        diff.breaks = diff.breaks,
                        plot.range = histo.plot.range)
    
  }
  
  ##### SCATTER PLOT
  if(doScatterPlots){
    
    
    
    plotScatterComparison(model = model.raster, 
                          data = data.raster,  
                          stat.results = comparison.results,
                          model.run = run, 
                          period = period,
                          data.name = names(data.raster), 
                          quant = quant)
    
    
  }
  
  return(comparison.results)
  
}



compareRunToSpatialDataset <- function(dataset, 
                                       model.raster, 
                                       run, 
                                       diff.breaks = NULL, 
                                       plot.breaks = NULL, 
                                       histo.plot.range = NULL, 
                                       doScatterPlots=TRUE, 
                                       doHistoPlots=TRUE, 
                                       quant = NULL, 
                                       ignore.raster = NULL, 
                                       map.layout.objs = NULL, 
                                       ...){
  
  
  # crop all rasters to the same size
  common.extent <- intersect(model.raster, dataset@data)
  model.raster <- crop(model.raster, common.extent)
  names(model.raster) <- run@id
  data.raster <- crop(dataset@data, common.extent)
  names(data.raster) <- dataset@id
  
  # make difference map and a stack to plot
  if(!is.null(ignore.raster)){ 
    model.raster <- mask(model.raster, ignore.raster)
    data.raster <- mask(data.raster, ignore.raster)
  }
  
  # also mask out data where there is no model and vice versa
  model.raster <- mask(model.raster, data.raster)
  data.raster <- mask(data.raster, model.raster)
  
  comparison.results <- compareTwoRastersStats(data.raster, model.raster)
  
  ##### DIFFERENCE MAP
  #plotDifferenceMap(comparison.results$diff.raster, 
  #                  quant.id = paste(quant@id, "diff", dataset@id, sep ="."), 
  #                  quant.str = paste(run@id, "-" , dataset@id, quant@id, sep = " "), 
  #                  run = run, 
  #                  period = dataset@time.span, 
  #                  layout.objs = map.layout.objs,
  #                  at = plot.breaks,
  #                  maxpixels = 1000000) 
  
  
  plotLPJMaps(comparison.results$diff.raster,
              quant = quant, 
              period = dataset@time.span, 
              doSummary = TRUE, 
              doIndividual = FALSE, 
              run = run,
              summary.file.name = paste(quant@id, "comp", dataset@id, sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              layout.objs = map.layout.objs,
              ...)
  
  
  
  ##### ABSOLUTE MAPS
  plotLPJMaps(stack(model.raster, data.raster),
              which.layers = "all",
              quant = quant, 
              period = dataset@time.span, 
              doSummary = TRUE, 
              doIndividual = TRUE, 
              run = run,
              summary.file.name = paste(quant@id, "comp", dataset@id, "2-up", sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              layout.objs = map.layout.objs,
              plot.labels = c(run@description, dataset@name),
              ...)
  
  
  
  
  
  ##### HISTOS
  if(doHistoPlots){
    
    plotHistoComparison(model = model.raster, 
                        data = data.raster, 
                        stat.results = comparison.results,
                        run = run, 
                        period = dataset@time.span,
                        data.name = names(data.raster), 
                        quant = quant, 
                        diff.breaks = diff.breaks,
                        plot.range = histo.plot.range)
    
  }
  
  ##### SCATTER PLOT
  if(doScatterPlots){
    
    plotScatterComparison(model = model.raster, 
                          data = data.raster,  
                          stat.results = comparison.results,
                          model.run = run, 
                          period = dataset@time.span,
                          data.name = dataset@abbreviation, 
                          quant = quant)
    
    
  }
  
  return(comparison.results)
  
}



compareManyRunsToData <- function(list.of.results, 
                                  list.of.runs,
                                  dataset, 
                                  label,
                                  diff.cuts = NULL,
                                  perc.diff.cuts = seq(-100,200,5),
                                  spatial.extent = NULL,
                                  showR2 = TRUE,
                                  layout.objs = layout.objs,
                                  ...) {
  
  message(paste("Comparing runs to ", dataset@name))
  
  
  # BUILD STACKS, NAMES AND PLOT TITLE LISTS FOR PLOTTING
  # raster stacks 
  Absolute.stack <- stack(list.of.results[[1]][["data.raster"]])
  Difference.stack <- stack()
  Percentage.Difference.stack <- stack()
  
  # text lists
  run.ids <- list()
  plot.titles <- list(dataset@name)
  
  # add each model run to stacks and text lists
  for(run in list.of.runs){
    
    run.ids <- append(run.ids, run@id)
    plot.titles <- append(plot.titles, run@description)
    
    Absolute.stack <- addLayer(Absolute.stack,  list.of.results[[run@id]][["model.raster"]])
    Difference.stack <- addLayer(Difference.stack,  list.of.results[[run@id]][["diff.raster"]])
    Percentage.Difference.stack <- addLayer(Percentage.Difference.stack,  list.of.results[[run@id]][["perc.diff.raster"]])
    
  }
  
  
  # CROP/EXTEND RASTERS (if extent specified)
  if(!is.null(spatial.extent)){
    Absolute.stack <- extend(crop(Absolute.stack, spatial.extent), spatial.extent)
    Difference.stack <- extend(crop(Difference.stack, spatial.extent), spatial.extent)
    Percentage.Difference.stack <- extend(crop(Percentage.Difference.stack, spatial.extent), spatial.extent)
  }
  
  
  # ASSIGN NAMES
  names(Absolute.stack) <- append(dataset@id, run.ids)
  names(Difference.stack) <- run.ids
  names(Percentage.Difference.stack) <- run.ids
  
  
  # PLOT ABSOLUTE VALUES
  plotLPJMaps(Absolute.stack,
              which.layers = "all",
              quant = dataset@veg.quant, 
              period = dataset@time.span, 
              doSummary = TRUE, 
              doIndividual = FALSE, 
              summary.file.name = paste(paste("AbsoluteVs", dataset@id, sep = ""), label, sep = "."),
              special.string = "Corrected",               
              plot.labels = plot.titles,
              layout.objs = layout.objs,
              ...)
  
  
  
  # MAKE R^2 LABELS (if required)
  if(showR2){
    for(result.index in 1:length(list.of.results)){
      R2.val <- round(list.of.results[[result.index]]$R.squ, 3)
      layout.objs[[list.of.runs[[result.index]]@id]] <- list("sp.text", c(extent(Difference.stack)@xmin * 0.8, extent(Difference.stack)@ymin * 0.7), bquote(R^2 ~ "=" ~ .(R2.val)), which = result.index, cex = 1.5)
    }
  }
  
  
  # PREPARE CUTS (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@veg.quant@cuts)[2] - range(dataset@veg.quant@cuts)[1] )/ (length(dataset@veg.quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@veg.quant@cuts)), max(dataset@veg.quant@cuts), interval)
  }
  
  
  # PLOT ABSOLUTE DIFFERENCE
  plot.titles <- plot.titles[-1]
  
  plotLPJMaps(Difference.stack,
              which.layers = "all",
              quant = dataset@veg.quant, 
              period = dataset@time.span, 
              doSummary = TRUE, 
              doIndividual = FALSE, 
              summary.file.name = paste(paste("DiffVs", dataset@id, sep = ""), label, sep = "."),
              summary.title = paste("Simulated - ", dataset@id, " (", dataset@units, ")", sep = ""),
              special = "difference",               
              plot.labels = plot.titles,
              override.cuts = diff.cuts,
              text.multiplier = 1.0,
              layout.objs = layout.objs,
              ...)
  
  
  # PLOT PERCENTAGE DIFFERENCE
  plotLPJMaps(Percentage.Difference.stack,
              which.layers = "all",
              quant = dataset@veg.quant, 
              period = dataset@time.span, 
              doSummary = TRUE, 
              doIndividual = FALSE, 
              summary.file.name = paste(paste("PercentageDiffVs", dataset@id, sep = ""), label, sep = "."),
              summary.title = paste("Simulated - ", dataset@id, "(percentage difference)"),
              special = "percentage.difference",               
              plot.labels = plot.titles,
              override.cuts = perc.diff.cuts,
              text.multiplier = 1.0,
              layout.objs = layout.objs,
              ...)
  
  
  
  # Clear up
  rm(run.ids, plot.titles, Absolute.stack, Difference.stack, Percentage.Difference.stack)
  gc()
  
}





doKappa <- function(stack, do.individual = FALSE, labels = NULL, verbose = TRUE){
  
  
  
  # get the unique classes and put them in a vector with no missing values
  unique.classes <- union(unique(subset(stack,1)),unique(subset(stack,2)))
  unique.classes <- sort(unique.classes)
  unique.class.ids <- seq(unique.classes[1], unique.classes[length(unique.classes)])
  
  # assign each gridcell a code based on the classifications and get the frequency table          
  kappa.temp <- overlay(stack,  fun=function(x,y){return(1000*x + y)}, unstack=TRUE)
  freq.table <- as.data.frame(freq(kappa.temp))
  
  # make the empty kappa matrix
  kappa.matrix <- matrix(data = 0, nrow = length(unique.class.ids), ncol = length(unique.class.ids))
  
  # loop through all the entries of the matrix for the kappa calculation
  for(counter.Y in 1:length(unique.class.ids)){
    for(counter.X in 1:length(unique.class.ids)){
      
      # find the element in the freq table corresponding the position in the matrix
      position.code = 1000*counter.X + counter.Y
      position.row <- which(freq.table$value == position.code)
      
      # if it exists in the table stick it in the matric
      if(length(position.row > 0)){ kappa.matrix[counter.Y, counter.X] <-  freq.table$count[position.row]   }
      
    }
  }
  
  # now we have the kappa matrix, calculate the Cohen's Kappa
  # handy sums
  col.totals <- colSums(kappa.matrix)
  row.totals <- rowSums(kappa.matrix)
  ngridcells <- sum(colSums(kappa.matrix))
  
  # total agreement
  total.agreement <- sum(diag(kappa.matrix))
  
  # chance agreement
  chance.agreement <- 0
  for(counter in 1:length(unique.class.ids)){
    chance.agreement <- chance.agreement + (col.totals[counter] * row.totals[counter] / ngridcells)
  }
  
  #finally kappa
  kappa <- (total.agreement - chance.agreement) / (ngridcells - chance.agreement)
  
  if(do.individual){
    if(is.null(labels)) labels <- paste("Class", 1:length(unique.class.ids), " ")
    
    # also calculate the per class agreement (This is ripped from TH Kappa script, I don't understand it but it gives identical results)
    per.class.kappa <- c()
    for(counter in 1:length(unique.class.ids)){
      a  <- kappa.matrix[counter, counter] / ngridcells
      p1 <- row.totals[counter] / ngridcells
      p2 <- col.totals[counter] / ngridcells
      q1 <- 1.0-p1
      q2 <- 1.0-p2
      b  <- p1-a
      c  <- p2-a
      d  <- q1-c
      per.class.kappa <- append(per.class.kappa, (a - p1*p2)/( (p1+p2)/2.0 -p1*p2))
      if(verbose) print(paste(labels[counter], round(per.class.kappa[counter], 3), sep = " "))
    }
    
    if(verbose) print(paste("Overall Kappa", round(kappa, 3), sep = " "))
    
    return(list(kappa, per.class.kappa))
  }
  
  else{
    if(verbose) print(paste("Overall Kappa", round(kappa, 3), sep = " "))
    return(kappa)
  }
}



###################################################################################################
######### BIOME CLASSIFICATION v2.9-STYLE WITH EXTRA FLEXIBILITY ##################################
######### Eventually should make code above obselete ##############################################
###################################################################################################


addBiomes <-function(input.dt, PFT.data, scheme = Smith2014.scheme){
  
  message(paste("Classifying biomes using scheme", scheme@name, sep = " "))
  
  
  if(scheme@id %in% names(input.dt)) { input.dt[, scheme@id := NULL, with=FALSE] }
    
  # Combine shade tolerance classes and add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
  if(scheme@combineShadeTolerance) combineShadeTolerance(input.dt, PFT.data)
  
  # Get the dominant tree and dominant woody PFTs
  addDominantPFT(input.dt, PFT.data, do.all = TRUE, do.tree = TRUE, do.woody = FALSE)
  
  # Get the totals required
  addVegTotals(input.dt, PFT.data, c(scheme@fraction.of.total, scheme@fraction.of.tree, scheme@fraction.of.woody, scheme@totals.needed))
  
  # Get the fractions required
  addVegFractions(input.dt, PFT.data, targets = scheme@fraction.of.total, of.total = TRUE,   of.tree = FALSE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = scheme@fraction.of.tree,  of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = scheme@fraction.of.woody, of.total = FALSE,  of.tree = FALSE, of.woody = TRUE)
  
  # Apply biome rules and return
  input.dt[, scheme@id := apply(input.dt[,,with=FALSE],FUN=scheme@rules,MAR=1), with = FALSE]
 
  return(input.dt)
  
}



#######################################################################################################
################################## MEGABIOME CLASSIFICATION 


GlobalMegabiomesRules <- function(lai){
  
  # BIOME 1 - Tropical Rain Forest
  #if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TrBEFractionofTree']]) > 0.6 &  lai[['DominantTree']] == "TrBE") {return(1)}
  if(as.numeric(lai[['Tree']]) > 2.5 &  lai[['DominantTree']] == "TrBE") {return(1)}
  
  # BIOME 2 - Tropical Deciduous Forest
  #else if(as.numeric(lai[['Tree']]) > 2.5 & (as.numeric(lai[['TrBRFractionofTree']]) > 0.6 | as.numeric(lai[['TrBRFractionofTree']])) & (lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(2)}
  else if(as.numeric(lai[['Tree']]) > 2.5 & lai[['DominantTree']] == "TrBR") {return(2)}
  
  
  # BIOME 3 - Tropical Seasonal Forest
  #else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TropicalFractionofTree']] )> 0.5 &  (lai[['DominantTree']] == "TrBE" | lai[['DominantTree']] == "TrBR" | lai[['DominantTree']] == "TrTBR")) {return(3)}
  
  # BIOME 4 - Boreal Evergreen Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  (lai[['DominantTree']] == "BNE" | lai[['DominantTree']] == "IBS" | lai[['DominantTree']] == "BIBS")) {return(4)}
  
  # BIOME 5 - Boreal Deciduous Forest/Woodland
  else if(as.numeric(lai[['Tree']]) > 0.5 &  lai[['DominantTree']] == "BNS") {return(5)}
  
  # BIOME 6 - Temperate Broadleaved Evergreen Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 & as.numeric(lai[['TemperateFractionofTree']]) > 0.5 & (lai[['DominantTree']] == "TeBE" | lai[['DominantTree']] == "TeNE")) {return(6)}
  
  # BIOME 7 - Temperate Deciduous Forest
  else if(as.numeric(lai[['Tree']]) > 2.5 &  as.numeric(lai[['TemperateFractionofTree']]) > 0.5 & lai[['DominantTree']] == "TeBS") {return(7)}
  
  # BIOME 8 - Xeric Woodland/Shrubland
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(8)}
  #else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['GrassFraction']]) < 0.2) {return(8)}
  
  # BIOME 9 - Moist Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) > 2.5) {return(9)}
  #else if(as.numeric(lai[['Tree']]) > 0.5) {return(9)}
  
  # BIOME 10 - Dry Savanna
  else if(as.numeric(lai[['Tree']]) > 0.5 & as.numeric(lai[['Tree']]) < 2.5 & as.numeric(lai[['Total']]) <= 2.5) {return(10)}
  
  # BIOME 11 - Arctic/alpine Tundra
  else if(as.numeric(lai[['Tree']]) < 0.5 & as.numeric(lai[['Total']]) > 0.5 & (as.numeric(lai[['Lat']]) >= 54 | as.numeric(lai[['GDD5']]) < 400)) {return(11)}
  
  # BIOME 12 - Tall Grassland
  else if(as.numeric(lai[['Grass']]) > 2.0) {return(12)}
  
  # BIOME 13  - Arid Shrubland/Grassland
  else if(as.numeric(lai[['Total']]) > 0.2) {return(13)}
  
  # BIOME 14 - Desert
  else if(as.numeric(lai[['Total']]) < 0.2) {return(14)}
  
  # REMAINDER
  else {
    print(paste("Oops, not classified: Location (", as.numeric(lai[['Lon']]), ",", as.numeric(lai[['Lat']]), ")" ))
    return(NA)
  }
  
}


Megabiome.str <- c("Tropical Rain Forest",                     
                   "Tropical Deciduous Forest", 
                   "Tropical Seasonal Forest", 
                   "Boreal Evergreen Forest/Woodland", 
                   "Boreal Deciduous Forest/Woodland",
                   "Temperate Evergreen Forest",
                   "Temperate Deciduous Forest",
                   "Xeric Woodland/Shrubland",
                   "Moist Savanna",
                   "Dry Savanna",
                   "Arctic/Alpine Tundra",
                   "Tall Grassland",
                   "Arid Shrubland/Grassland",
                   "Desert")


Megabiome.cols <- c("Tropical Rain Forest" = "seagreen",                     
                    "Tropical Deciduous Forest" = "orange3", 
                    "Tropical Seasonal Forest" = "green3", 
                    "Boreal Evergreen Forest/Woodland" = "turquoise4",
                    "Boreal Deciduous Forest/Woodland"= "cyan",
                    "Temperate Evergreen Forest" = "dodgerblue3",
                    "Temperate Deciduous Forest" = "chartreuse",
                    "Xeric Woodland/Shrubland" = "deeppink3",
                    "Moist Savanna" = "olivedrab2",
                    "Dry Savanna" = "goldenrod2",
                    "Arctic/Alpine Tundra" = "mediumpurple1",
                    "Tall Grassland" =  "gold",                     
                    "Arid Shrubland/Grassland"= "lightcyan",
                    "Desert" = "grey75")


