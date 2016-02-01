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
    
    if("zones" %in% targets) targets <- targets[-which(tolower(targets) == "zones")]
    if("zone" %in% targets) targets <- targets[-which(tolower(targets) == "zone")]    
    
  }
  
  # Expand "leafforms" 
  if("leafforms" %in% tolower(targets) | "leafform" %in% tolower(targets)){
    
    # Find all leafforms present in PFTs 
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
######### BIOME CLASSIFICATION v2.9-STYLE WITH EXTRA FLEXIBILITY ##################################
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


