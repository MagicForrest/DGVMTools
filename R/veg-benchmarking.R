

prepareBenchmarkingDatasets <- function(benchmark.list, resolution = "HD"){
  
  benchmarking.datasets.list <- list()
  
  
  # For each benchmark string 
  for(benchmark.string in benchmark.list){
    
    # If it is a biome classification
    if(benchmark.string %in% names(supported.biome.schemes)) {
      
      message(paste("Reading PNV biomes using" ,benchmark.string, "scheme", sep = " "))
      benchmarking.datasets.list[[benchmark.string]] <- new("SpatialDataset",
                                                            id = benchmark.string,
                                                            name = paste("H&P PNV Biomes classified by scheme ", benchmark.string),
                                                            temporal.extent = new("TemporalExtent", name = "PNV", start = 1961, end = 1990) ,
                                                            data = readHandPBiomes(resolution = resolution, classification = benchmark.string),
                                                            veg.quant = lookupVegQuantity("lai"),
                                                            units = ""
      )
      
    } # if it is a biome classification
    
    # If it is the Saatchi data
    if(benchmark.string == "Saatchi2011") { benchmarking.datasets.list[[benchmark.string]] <- getSaatchi2011(resolution = resolution) }
    
    
    # If it is the GFED data
    
  } # for each benchmark
  
  
  
  return(benchmarking.datasets.list)
  
}






#############################################################################################################
############################ COMPARE TWO RASTERS 


compareTwoRastersStats <- function(data.raster, model.raster){
  
  
  model.raster <- mask(model.raster, data.raster)
  data.raster <- mask(data.raster, model.raster)
  
  # difference raster
  diff <- model.raster - data.raster
  names(diff) <- "Model.minus.Data"
  
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
  comparison.results <- new("RasterComparison",
                            diff.raster = diff, 
                            perc.diff.raster = perc.diff.raster, 
                            data.raster = data.raster, model.raster = model.raster, 
                            R.squ = R.squ, 
                            P.cor = P.cor, 
                            RMSE = RMSE, 
                            mean.diff = mean.diff, 
                            sd.diff = sd.diff)
  
  
  return(comparison.results)
  
}





compareRunToSpatialDataset <- function(dataset, 
                                       vegvar, 
                                       layer,
                                       diff.cuts = NULL, 
                                       histo.plot.range = NULL, 
                                       doScatterPlots=TRUE, 
                                       doHistoPlots=TRUE, 
                                       quant = NULL, 
                                       ignore.raster = NULL,
                                       correction.raster = NULL,
                                       tolerance = NULL,
                                       ...){
  
  # PROMOTE MODEL TO RASTER FOR PROCESSING
  model.raster <- promoteToRaster(vegvar, layer, vegvar@run@tolerance)
  
  # PREPARE CUTS AND HISTO PLOT RANGE (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@veg.quant@cuts)[2] - range(dataset@veg.quant@cuts)[1] )/ (length(dataset@veg.quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@veg.quant@cuts)), max(dataset@veg.quant@cuts), interval)
  }
  
  if(is.null(histo.plot.range)) histo.plot.range <- c(diff.cuts[[1]], diff.cuts[[length(diff.cuts)]])
  
  # PREPARE QUANT
  if(is.null(quant)) quant <- dataset@veg.quant 
  
  # crop all rasters to the same size
  temp <- intersectionRVC(model.raster, dataset@data)
  model.raster <- temp[[1]]
  data.raster <- temp[[2]]
  rm(temp)
  
  # set names of raster layers
  names(model.raster) <- layer
  names(data.raster) <- dataset@id
  
  # make difference map and a stack to plot
  if(!is.null(ignore.raster)){ 
    model.raster <- mask(model.raster, ignore.raster)
    data.raster <- mask(data.raster, ignore.raster)
  }
  
  # also mask out data where there is no model and vice versa
  model.raster <- mask(model.raster, data.raster)
  data.raster <- mask(data.raster, model.raster)
  
  # calculate comparison
  comparison.results <- compareTwoRastersStats(data.raster, model.raster)
  
  ##### DIFFERENCE MAPS
  plotVegMaps(comparison.results@diff.raster,
              quant = quant, 
              period = dataset@temporal.extent, 
              doSummary = TRUE, 
              doIndividual = FALSE, 
              run = vegvar@run,
              summary.file.name = paste(quant@id, "Vs", dataset@id, sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              special = "diff",
              ...)
  
  ##### ABSOLUTE MAPS
  plotVegMaps(stack(model.raster, data.raster),
              targets = c(names(model.raster), names(data.raster)),
              quant = quant, 
              period = dataset@temporal.extent, 
              doSummary = TRUE, 
              doIndividual = TRUE, 
              run = vegvar@run,
              summary.file.name = paste(quant@id, "comp", dataset@id, "2-up", sep = "."),
              special.string = "Corrected",
              maxpixels = 1000000,
              plot.labels = c(vegvar@run@description, dataset@name),
              ...)
  
  
  ##### HISTOS
  if(doHistoPlots){
    
    plotHistoComparison(model = model.raster, 
                        data = data.raster, 
                        stat.results = comparison.results,
                        run = vegvar@run, 
                        period = dataset@temporal.extent,
                        data.name = names(data.raster), 
                        quant = quant, 
                        breaks = diff.cuts,
                        plot.range = histo.plot.range)
    
  }
  
  ##### SCATTER PLOT
  if(doScatterPlots){
    
    plotScatterComparison(model = model.raster, 
                          data = data.raster,  
                          stat.results = comparison.results,
                          run = vegvar@run, 
                          period = dataset@temporal.extent,
                          data.name = dataset@name, 
                          quant = quant)
    
    
  }
  comparison.results@id <- dataset@id
  
  return(comparison.results)
  
}



compareManyRunsToData <- function(runs,
                                  dataset, 
                                  label,
                                  diff.cuts = NULL,
                                  perc.diff.cuts = seq(-100,200,5),
                                  spatial.extent = NULL,
                                  showR2 = TRUE,
                                  layout.objs = NULL,
                                  ...) {
  
  message(paste("Comparing runs to ", dataset@name))
  
  
  # BUILD STACKS, NAMES AND PLOT TITLE LISTS FOR PLOTTING
  # raster stacks 
  Absolute.stack <- stack(runs[[1]]@benchmarks[[dataset@id]]@data.raster)
  Difference.stack <- stack()
  Percentage.Difference.stack <- stack()
  
  # text lists
  run.ids <- list()
  plot.titles <- list(dataset@name)
  
  # add each model run to stacks and text lists
  for(run in runs){
    
    comparison.obj <- run@benchmarks[[dataset@id]]
    
    run.ids <- append(run.ids, run@id)
    plot.titles <- append(plot.titles, run@description)
    
    Absolute.stack <- addLayer(Absolute.stack,  comparison.obj@model.raster)
    Difference.stack <- addLayer(Difference.stack,  comparison.obj@diff.raster)
    Percentage.Difference.stack <- addLayer(Percentage.Difference.stack,  comparison.obj@perc.diff.raster)
    
    rm(comparison.obj)
    
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
  plotVegMaps(Absolute.stack,
              quant = dataset@veg.quant, 
              period = dataset@temporal.extent, 
              summary.file.name = paste(paste("AbsoluteVs", dataset@id, sep = ""), label, sep = "."),
              special.string = "Corrected",               
              plot.labels = plot.titles,
              layout.objs = layout.objs,
              ...)
  
  # MAKE R^2 LABELS (if required)
  if(showR2){
    counter <- 0
    for(run in runs){
      counter <- counter + 1
      comparison.obj <- run@benchmarks[[dataset@id]]
      R2.val <- round(comparison.obj@R.squ, 3)
      layout.objs[[run@id]] <- list("sp.text", c(extent(Difference.stack)@xmin * 0.8, extent(Difference.stack)@ymin * 0.7), bquote(R^2 ~ "=" ~ .(R2.val)), which = counter, cex = 1.5)
      rm(comparison.obj)
    }
  }
  
  
  # PREPARE CUTS (if not specificed)
  if(is.null(diff.cuts)) {
    interval <- (range(dataset@veg.quant@cuts)[2] - range(dataset@veg.quant@cuts)[1] )/ (length(dataset@veg.quant@cuts)-1)
    diff.cuts <-  seq(-(max(dataset@veg.quant@cuts)), max(dataset@veg.quant@cuts), interval)
  }
  
  
  # PLOT ABSOLUTE DIFFERENCE
  plot.titles <- plot.titles[-1]
  
  plotVegMaps(Difference.stack,
              quant = dataset@veg.quant, 
              period = dataset@temporal.extent, 
              summary.file.name = paste(paste("DiffVs", dataset@id, sep = ""), label, sep = "."),
              summary.title = paste("Simulated - ", dataset@id, " (", dataset@units, ")", sep = ""),
              special = "difference",               
              plot.labels = plot.titles,
              override.cuts = diff.cuts,
              text.multiplier = 1.0,
              layout.objs = layout.objs,
              ...)
  
  
  # PLOT PERCENTAGE DIFFERENCE
  plotVegMaps(Percentage.Difference.stack,
              quant = dataset@veg.quant, 
              period = dataset@temporal.extent, 
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


###################################################################################################
####################  BIOME COMPARISIONS AND STATS ################################################
###################################################################################################



doKappa <- function(stack, scheme, labels = NULL, verbose = TRUE){
  
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
  
  
  return(new("BiomeComparison",
             id = scheme@id,
             data.raster = subset(stack,2), 
             model.raster = subset(stack,1), 
             scheme = scheme,
             Kappa = kappa, 
             individual.Kappas = per.class.kappa)
  )
  
  
}




compareBiomes <- function(run, variable, period, scheme, plot = TRUE, ...){
  
  # Prepare the veg. spatial object
  this.VegSpatial <- getVegSpatial(run, variable, period, reread.file = FALSE)
  
  #  calculate biomes from model output
  this.VegSpatial <- addBiomes(this.VegSpatial, scheme)
  
  # read expert-derived PNV biomes
  PNV.biomes <- readHandPBiomes(resolution = "HD", classification = scheme@id)
  
  # make the stack for the comparisons
  comparison.stack <- stack(intersectionRVC(promoteToRaster(this.VegSpatial, scheme@id, run@tolerance), PNV.biomes))
  
  # calculate Kappa statistics (and possibily other similarity/dissimilarity metrics)
  Kappa.comparison <- doKappa(comparison.stack, scheme = scheme, labels = scheme@strings, verbose = TRUE)
  
  
  # plot biomes if requested
  if(plot){
    plotBiomeMap(this.VegSpatial, 
                 scheme = scheme,
                 addData = PNV.biomes, 
                 kappa.list = list(Kappa.comparison),
                 Cairo.type = c("png","ps"), 
                 ...
    )
  }
  
  return(Kappa.comparison)
  
}

compareManyRunsToBiomes <- function(runs, biome.dataset, analysis.label = "", ...){
  
  # get biome scheme 
  scheme <- supported.biome.schemes[[biome.dataset@id]]
  
  # make a raster stack with all the runs
  biome.stack <- stack()
  labels <- list()
  for(run in runs){
    biome.stack <- addLayer(biome.stack, run@benchmarks[[biome.dataset@id]]@model.raster)
    labels <- append(labels, run@description)
  }
  
  plotBiomeMap(biome.stack,
               addData = biome.dataset, 
               targets = names(biome.stack),
               file.name = paste("Biomes", scheme@id, analysis.label, sep = "."),
               run.title = scheme@id,
               plot.labels = labels,
               ...)
  
}

# Compare runs to each other
compareVegSpatialObject <- function(runs, veg.spatial.id, target, expand.target = TRUE, base.run.id = NULL, plot.perc.diff = TRUE, special = NULL, ...) {
  
  # To avoid NOTES
  Lon = Lat = NULL
  
  # initialise an empty data.table to hold the comparison
  comparison.dt <- data.table("Lon" = numeric(0), "Lat" = numeric(0))
  setkey(comparison.dt, Lon, Lat)
  
  # for each run
  for(run in runs){
    
    # grab the VegObject that we want from the vegRun
    temp.spatial <- run@objects[[veg.spatial.id]]
    
    # expand the target if necessary
    if(expand.target){ target <- expandTargets(targets = target, getPFTs(temp.spatial, run@pft.set))  }
    
    # Extract the columns that we need and add them to the data.table and set the names appropriately
    comparison.dt <- comparison.dt[temp.spatial@data[,c("Lon","Lat",target),with=FALSE]]
    setnames(comparison.dt, c(names(comparison.dt)[1:(length(names(comparison.dt))-length(target))], paste(run@id, target, sep = "_")))
    
    rm(temp.spatial)
    
  }
  
  
  # now plot the absolute values for each target run on the same plot (one plot per target)
  for(sub.target in target) {
    targets <- c()
    for(run in runs){
      targets <- append(targets, paste(run@id, sub.target, sep = "_"))
    }
    plotVegMaps(comparison.dt,
                targets = targets,
                quant = run@objects[[veg.spatial.id]]@quant,
                summary.title = sub.target,
                special.string = paste("diff", sub.target, sep = "."),
                special = special,
                ...)
  }
  
  
  # if a base.run is supplied, plot the the difference between each run and it
  if(!is.null(base.run.id)){
    
    # for each run that is not the base.run
    for(run in runs){
      if(run@id != base.run.id){
        
        # for each target
        for(sub.target in target){
          
          # make a new column of the data.table by selecting the relevant target
          col.name <- paste(paste(run@id, sub.target, sep = "_"), "minus", paste(base.run.id, sub.target, sep = "_"), sep = ".")
          comparison.dt[, eval(col.name) := get(paste(run@id, sub.target, sep = "_")) - get(paste(base.run.id, sub.target, sep = "_"))]
          
          # plot the difference
          title <- paste(sub.target, paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", IDFromList(base.run.id, runs)@description, sep = " ")
          
          plotVegMaps(comparison.dt,
                      targets = col.name,
                      quant = run@objects[[veg.spatial.id]]@quant,
                      special = "diff",
                      summary.title = title,
                      ...)
          
          if(plot.perc.diff){
            
            # first add the precentage difference column
            col.name <- paste(paste(run@id, sub.target, sep = "_"), "minus", paste(base.run.id, sub.target, sep = "_"), "perc.diff", sep = ".")
            comparison.dt[, eval(col.name) := (get(paste(run@id, sub.target, sep = "_")) - get(paste(base.run.id, sub.target, sep = "_"))) %/0% get(paste(base.run.id, sub.target, sep = "_")) * 100]
           
            
            # plot the difference
            title <- paste(sub.target, paste(run@objects[[veg.spatial.id]]@quant@full.string, ":", sep = ""), run@description, "-", IDFromList(base.run.id, runs)@description, sep = " ")
            
            plotVegMaps(comparison.dt,
                        targets = col.name,
                        quant = run@objects[[veg.spatial.id]]@quant,
                        special = "perc.diff",
                        summary.title = title,
                        limit = TRUE,
                        limits = c(-100,200),
                        ...)
            
          }
          
          
        }
        
      }
    }
    
  } # if base run is specified  
  
}

