

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
                                                            abbreviation = paste("PNV", benchmark.string, sep = "_"),
                                                            time.span = new("TimeSpan", name = "PNV", start = 1961, end = 1990) ,
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

