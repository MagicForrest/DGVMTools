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
#####################################



##########################################################################################################################################
################################################## PLOT LPJ MAPS #########################################################################
##########################################################################################################################################
### This is a heavy lifting function for plotting LPJ variables flexibly but with some degree of automation
### It also acts as wrapper for non-standard plots 




plotLPJMaps <- function(data, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a VegObj 
                        which.layers = NULL,
                        quant = NULL, 
                        period = NULL, 
                        doSummary = TRUE, 
                        doIndividual = FALSE, 
                        run = NULL, 
                        PFT.set = global.PFTs,
                        plot.dir = NULL, 
                        summary.file.name = NULL,
                        summary.title = NULL,
                        special.string = NULL,
                        layout.objs = NULL, 
                        plot.labels =  NULL,
                        plot.bg.col =  "transparent",
                        useLongnames = FALSE,
                        Cairo.units = "px",
                        Cairo.dpi = 72,
                        Cairo.type = "png", 
                        Cairo.width = 1800, 
                        Cairo.height = 1000,
                        Cairo.bg = "transparent",
                        text.multiplier = 1,
                        plot.extent = NULL,
                        limit = FALSE,
                        limits = NULL,
                        override.cols = NULL,
                        override.cuts = NULL,
                        special = NULL,
                        maxpixels = 1E6,
                        ...){
  
  
  ##########################################################################################################
  ################# 
  
  # IF VEGVAR HAS BEEN SUPPLIED MANY THINGS ARE DEFINED FROM IT 
  if(class(data) == "VegObj"){
    run <- data@run
    period <- data@time.span
    PFT.set <- run@pft.set
    if(is.null(quant)) quant <- data@quant  
  }
  
  
  # TOLERANCE - for when making grid
  if(is.null(run)) { tolerance <- 0.02 }
  else {tolerance <- run@tolerance}  
  
  # DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  # QUANTITY AND COLS
  if(is.null(quant)){ quant = lpj.quantities[["generic"]]}
  else if(class(quant) == "character"){ quant = lpj.quantities[[quant]] }
  else if((class(quant) != "VegQuant")){
    warning("Invalid quantity found in plotLPJMaps using generic quantity")
    quant = lpj.quantities[["generic"]]
  }
  
  # SPECIAL
  if(!is.null(special)){
    if(tolower(special) == "difference" | tolower(special) == "diff"){
      minmax <- max(quant@cuts) - min(quant@cuts)
      step <- (max(quant@cuts) - min(quant@cuts)) / (length(quant@cuts) - 1)
      quant@cuts <- seq(from = -minmax, to = minmax, by = step)
      quant@colours <- colorRampPalette(c("green","blue","white","red", "yellow"))
      quant@id <-  paste(quant@id, "diff", sep =".")
      quant@short.string = paste(quant@short.string, "Diff", sep = ".")
      quant@full.string = paste("Difference: ", quant@full.string, sep = "")
      plot.bg.col <- "grey"
    }
    else if(tolower(special) == "percentage.difference" | tolower(special) == "perc.diff"){
      
      quant@cuts <- seq(from = -100, to = 200, by = 10)
      quant@colours <- colorRampPalette(c("blue","white","red", "yellow"))
      quant@id <-  paste(quant@id, "percdiff", sep =".")
      quant@short.string = paste(quant@short.string, "PercDiff", sep = ".")
      quant@full.string = paste("Percentage Difference: ", quant@full.string, sep = "")
      plot.bg.col <- "grey"
      
    }
    else if(tolower(special) == "fraction" | tolower(special) == "frac"){
      
      quant@cuts <- seq(from = 0, to = 1, by = 0.05)
      quant@id <- paste(quant@id, "fraction", sep = ".")
      quant@short.string <- paste(quant@short.string, "fraction", sep = ".")
      quant@full.string <- paste(quant@full.string, "Fraction", sep = " ")
      quant@colours <- colorRampPalette(c("grey85", "black"))
      
    }
    else {
      stop(paste("Special case", tolower(special), "not implemented yet", sep = " "))
    }
  }
  
  # OVERRIDE (CUTS AND COLOUR SCHEME) 
  if(!is.null(override.cols)) {quant@colours <- override.cols}
  if(!is.null(override.cuts)) {quant@cuts <- override.cuts}
  
  
  # parse layer names and promote to raster for plotting
  if(is.null(which.layers)) {
    if(class(data) == "VegObj") which.layers <- names(data@data)
    else which.layers <- names(data)
  }
  which.layers <- expandTargets(which.layers, data, PFT.set)
  data.toplot <- promoteToRaster(data, which.layers, tolerance)
  
  # PLOT LABELS (if no titles are provided use the layer names)
  if(is.null(plot.labels)){
    plot.labels <- names(data.toplot) 
    if(useLongnames) {
      plot.labels <- list()
      for(layer.name in names(data.toplot)){
        # look up PFT
        for(PFT in PFT.set){
          if(layer.name == PFT@name) plot.labels[[length(plot.labels )+1]] <- unlist(PFT@longname)
        }
        #if(layer.name == "Total") plot.labels[[length(plot.labels )+1]] <- "Total"
      }
      plot.labels <- unlist(plot.labels)
    }
  }
  
  
  # EXTENT
  if(!is.null(plot.extent)){ data.toplot <- crop(data.toplot, plot.extent)}
  
  # LIMIT
  if(limit){
    
    if(is.null(limits)){
      if(!is.null(quant)) {
        limits <- c(quant@cuts[0], quant@cuts[length(quant@cuts)]) 
      }
      else{
        warning("plotLPJMaps:: limit set to TRUE but no limits provided and no Quantity object provided either")
      }
      
    }
    
    quant@cuts <- seq(from = limits[1], to = limits[2], length.out = length(quant@cuts))
    data.limited <- stack()
    for(layer in 1:nlayers(data.toplot)) {
      this.layer <- subset(data.toplot, layer)
      this.layer[this.layer < limits[1]] <- limits[1]
      this.layer[this.layer > limits[2]] <- limits[2]
      data.limited <- addLayer(data.limited, this.layer)
      rm(this.layer)
    }
    rm(data.toplot)
    data.toplot <- data.limited
    rm(data.limited)
    
  }
  
  # SUMMARY/INDIVIDUAL
  # If only one layer has been selected, don't plot as summary, plot it as individual, regardless of setting
  if(nlayers(data.toplot) == 1){
    doSummary <- FALSE
    doIndividual <- TRUE
  }
  
 
  
  for(format in Cairo.type){
    
    # print all layers on one plot if summary is TRUE
    if(doSummary){
      
      # FILENAME
      this.id <- makeVariableIDString(quant@id, "Summary", special.string)
      if(is.null(summary.file.name)) this.file.name <- makeFileName(this.id, file.name = summary.file.name, run = run, period = period)
      else this.file.name <- paste(summary.file.name, format, sep = ".")
      
      # PLOT MAIN TITLE
      if(is.null(summary.title)) this.main.title <- makePlotTitle(paste(quant@full.string, "Summary", sep = " "), summary.title, run, period)
      else this.main.title <- summary.title
      
      Cairo(file = file.path(plot.dir, this.file.name), 
            dpi = Cairo.dpi, 
            type = format, 
            width = Cairo.width, 
            height = Cairo.height, 
            title = this.main.title, 
            bg = Cairo.bg)  
        
      print(spplot(data.toplot,
                   which.layers,
                   par.settings = list(panel.background=list(col=plot.bg.col)),
                   xlab = list(label = "Longitude", cex = 3 * text.multiplier),
                   ylab = list(label = "Latitude", cex = 3 * text.multiplier),
                   col.regions= quant@colours,
                   colorkey = list(space = "right", 
                                   col = quant@colours, 
                                   labels = list(cex = 3 * text.multiplier)),                                                                                            
                   at = quant@cuts,
                   scales = list(draw = TRUE, cex = 3 * text.multiplier),
                   as.table = TRUE,
                   main=list(label=this.main.title, 
                             cex = 4 * text.multiplier),
                   par.strip.text = list(#lines = 1.0, 
                     cex = 2 * text.multiplier),
                   sp.layout = layout.objs,
                   maxpixels = maxpixels,
                   names.attr = plot.labels,
                   ...)
      )
      
      dev.off()
      
    }
    
    # print all PFTs on one plot if summary is TRUE
    if(doIndividual){
      
      for(layer in which.layers){
        
        # FILENAME
        this.id <- makeVariableIDString(quant@id, layer, special.string)
        this.file.name <- makeFileName(this.id, file.name = NULL, run = run, period = period)
        
        # PLOT TITLE
        if(!is.null(plot.labels[which(layer == which.layers)])) {plot.title <- plot.labels[which(layer == which.layers)]}
        else { plot.title <- makePlotTitle(paste(quant@full.string, special.string, layer, sep = " "), plot.title, run, period) }
        
        Cairo(file = file.path(plot.dir, this.file.name), 
              dpi = Cairo.dpi, 
              type = Cairo.type, 
              width = Cairo.width, 
              height = Cairo.height, 
              title = plot.title, 
              bg = Cairo.bg)  
        
        print(spplot(data.toplot,
                     layer,
                     par.settings = list(panel.background=list(col=plot.bg.col)),
                     xlab = list(label = "Longitude", cex = 3 * text.multiplier),
                     ylab = list(label = "Latitude", cex = 3 * text.multiplier),
                     col.regions= quant@colours,
                     colorkey = list(space = "right", col = quant@colours, labels = list(cex = 3)),
                     at = quant@cuts,
                     scales = list(draw = TRUE, cex = 3 * text.multiplier),
                     as.table = TRUE,
                     main=list(label = plot.title, 
                               cex = 4 * text.multiplier),
                     par.strip.text = list(lines = 1.0, cex = 2 * text.multiplier),
                     sp.layout = layout.objs,
                     maxpixels = maxpixels,
                     #names.attr = PFT.plottitles,
                     ...)
        )
        dev.off()
        
      }
      
    }
    
  }
  
  # clean up
  rm(data.toplot)
  gc()
  
}


##########################################################################################################################################
################################################## PLOT BIOME MAPS #######################################################################
##########################################################################################################################################


plotBiomeMap <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                         which.layers = NULL,
                         scheme = Smith2014.scheme,
                         biome.strings = NULL,
                         biome.cols = NULL,  
                         run = NULL, 
                         period = NULL, 
                         plot.dir = NULL, 
                         file.name = NULL, 
                         layout.objs = NULL, 
                         main.title =  NULL,
                         plot.labels = NULL,
                         addData = NULL,
                         calcKappa = TRUE,
                         showKappa = TRUE,
                         kappa.position = NULL,
                         Cairo.units = "px",
                         Cairo.dpi = 72,
                         Cairo.type = "png", 
                         Cairo.width = 1800, 
                         Cairo.height = 1000,
                         Cairo.bg = "transparent",
                         plot.extent = NULL,
                         maxpixels = 1E6,
                         ...){
  
  ##### Here take parameters from arguments or from supplied VegRun object
  
  # IF VEGVAR HAS BEEN SUPPLIED MANY THINGS ARE DEFINED FROM IT 
  if(class(data) == "VegObj"){
    run <- data@run
    period <- data@time.span
  }
  
  # IF NO LAYERS, COLS OR STRINGS SUPPLIED, USE THE  DEFAULTSOF THE SCHEME
  if(is.null(which.layers)) which.layers = scheme@id
  if(is.null(biome.strings)) biome.strings = biome.scheme@strings
  if(is.null(biome.cols)) biome.cols = biome.scheme@cols
  
  # TOLERANCE - for when making grid
  if(is.null(run)) { tolerance <- 0.02 }
  else {tolerance <- run@tolerance}  
  
  # DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  ##### Here check data is right class for plotting, process it if not
  data.toplot <- promoteToRaster(data, which.layers, run@tolerance) 
  if(is.null(plot.labels)) {
    if(is.null(run)){
      plot.labels <- list()
      for(layer in names(data.toplot)){
        plot.labels <- append(plot.labels, layer) 
      }
    }
    else{ plot.labels <- run@description}
  }
  
  # EXTENT
  if(!is.null(plot.extent)){ data.toplot <- crop(data.toplot, plot.extent)}
  
  # Add PNV data if requested read it in and compare rasters
  if(class(addData) == "logical") {
    if(addData) addData <- readPNVBiomes()
    else addData <- NULL
  }
  
  if(!is.null(addData)) {
    
    # first check if they are on identical grids, then one can simply add the layers
    if(compareRaster(addData, data.toplot, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=FALSE)){
      print("woohoo!")
    }
    else if(compareRaster(addData, data.toplot, extent=FALSE, rowcol=FALSE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=FALSE)){
      addData <- crop(addData, data.toplot, snap = "out")
      addData <- extend(addData, data.toplot)
    }
    else {
      addData <- resample(addData, data.toplot, method = "ngb")
      
    }
    
    
    addData <- crop(addData, data.toplot)
    data.toplot <- mask(data.toplot, addData)  
    
    # add the PNV raster layer and its title
    data.toplot <- stack(data.toplot, addData) 
    plot.labels <- c(plot.labels, "PNV (Hickler et al. 2006)")
    
  }
  
  # KAPPA
  if(calcKappa & !is.null(addData)){
    # calculate Kappas and show in terminal
    kappa.list <- list()
    for(layer in 1:(nlayers(data.toplot)-1)){
      message(paste("Kappas for run ", names(data.toplot)[layer]))
      kappa.list[[layer]] <- doKappa(subset(data.toplot, c(layer, nlayers(data.toplot))),  do.individual = TRUE, labels = biome.strings, verbose = TRUE)
    }
    # put kappas on plot
    if(showKappa){
      # if only one model run put individual Kappas into biome legend
      if(nlayers(data.toplot)-1 == 1) biome.strings <- paste0(biome.strings, " (", round(kappa.list[[1]][[2]],2), ")", sep = "")
      # place overall Kappa on each modelled biome map 
      if(is.null(kappa.position)) { kappa.position <- c(extent(data.toplot)@xmin * 0.8, extent(data.toplot)@ymin * 0.8) }
      for(layer in 1:(nlayers(data.toplot)-1)){
        
        layout.objs[[paste(layer)]] <- list("sp.text", kappa.position, paste0("Kappa = ", round(kappa.list[[layer]][[1]],3)), which = layer, cex = 1.5)
        #stop()
      }
      
    }   
  }
  
  # PLOT MAIN TITLE
  if(is.null(main.title)) this.main.title <- makePlotTitle(paste("Biomes"), main.title, run, period)
  else this.main.title <- main.title
  
  for(format in Cairo.type){
    
    # FILENAME
    if(is.null(file.name)) this.file.name <- makeFileName("Biomes", file.name = file.name, run = run, period = period, extension = format)
    else this.file.name <- paste(file.name, format, sep = ".")
    
    Cairo(file = file.path(plot.dir, this.file.name), 
          dpi = Cairo.dpi, 
          type = format, 
          width = Cairo.width, 
          height = Cairo.height, 
          title = this.main.title, 
          bg = Cairo.bg)  
    
    print(spplot(data.toplot,
                 xlab = list(label = "Longitude", cex = 2),
                 ylab = list(label = "Latitude", cex = 2),
                 col.regions = biome.cols,
                 at = 0:length(biome.strings),
                 scales = list(draw = TRUE, cex = 2),
                 as.table = TRUE,
                 main=list(label=this.main.title, cex = 3),
                 par.strip.text = list(lines = 1.0, cex = 1.5),
                 sp.layout = layout.objs,
                 names.attr = plot.labels,
                 maxpixels = maxpixels,
                 colorkey = list(col = rev(biome.cols), 
                                 space = "right",
                                 labels = list(labels = rev(biome.strings), 
                                               cex = 2,
                                               at = seq(0, length(biome.strings), 1)+0.5)),
                 ...
    )
    )
    dev.off()
    
  }
  
}


##########################################################################################################################################
################################################## PLOT FIRE RETURN MAPS #################################################################
##########################################################################################################################################


plotFireRT <- function(data, # can be a data.table, SpatialPixelsDataFrame or a raster
                       which.layers = "all",
                       cuts = c(0, 1, 3, 5, 10, 25, 50, 100, 200, 400, 800, 1000),
                       cols = colorRampPalette(c("black", "red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3")),  
                       run = NULL, 
                       period = NULL, 
                       plot.dir = NULL, 
                       file.name = NULL, 
                       layout.objs = NULL, 
                       main.title =  NULL,
                       plot.labels = NULL,
                       Cairo.units = "px",
                       Cairo.dpi = 72,
                       Cairo.type = "png", 
                       Cairo.width = 1800, 
                       Cairo.height = 1000,
                       Cairo.bg = "transparent",
                       ...){
  
  ##### Here take parameters from arguments or from supplied VegRun object
  
  # TOLERANCE - for when making grid
  if(is.null(run)) { tolerance <- 0.02 }
  else {tolerance <- run@tolerance}  
  
  # DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  ##### Here check data is right class for plotting, process it if not
  data.toplot <- promoteToRaster(data, which.layers, run@tolerance) 
  if(is.null(plot.labels)) {
    if(is.null(run)){
      plot.labels <- list()
      for(layer in names(data.toplot)){
        plot.labels <- append(plot.labels, layer) 
      }
    }
    else{ plot.labels <- run@description}
  }
  
  # CUT THE DATA ACCORDING TO THE CUTS
  data.toplot <- cut(data.toplot, cuts) 
  colourkey.labels <- paste(cuts)
  colourkey.labels[length(colourkey.labels)] <- paste0(colourkey.labels[length(colourkey.labels)], "+")
  
  # PLOT MAIN TITLE
  if(is.null(main.title)) this.main.title <- makePlotTitle(paste("Fire Return Time"), main.title, run, period)
  else this.main.title <- main.title
  
  for(format in Cairo.type){
    
    # FILENAME
    if(is.null(file.name)) this.file.name <- makeFileName("FireRT", file.name = file.name, run = run, period = period, extension = format)
    else this.file.name <- file.name
    
    Cairo(file = file.path(plot.dir, this.file.name), 
          dpi = Cairo.dpi, 
          type = format, 
          width = Cairo.width, 
          height = Cairo.height, 
          title = this.main.title, 
          bg = Cairo.bg)  
    
    print(spplot(data.toplot,
                 xlab = list(label = "Longitude", cex = 2),
                 ylab = list(label = "Latitude", cex = 2),
                 col.regions = cols,
                 at = 0:(length(cuts)-1),
                 scales = list(draw = TRUE, cex = 2),
                 as.table = TRUE,
                 main=list(label=this.main.title, cex = 3),
                 par.strip.text = list(lines = 1.0, cex = 1.5),
                 sp.layout = layout.objs,
                 names.attr = plot.labels,
                 colorkey = list(col = cols, 
                                 space = "right",
                                 labels = list(labels = colourkey.labels, 
                                               cex = 2,
                                               at = 0:(length(cuts)-1)
                                 )
                 ),
                 ...
    )
    )
    dev.off()
    
  }
  
}








##########################################################################################################################################
################################################## PLOT DOMINANT #########################################################################
##########################################################################################################################################


plotDominantPFTMap <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                               which.dominant = "Dominant",
                               run = NULL, 
                               PFT.set = global.PFTs,
                               period = NULL, 
                               plot.dir = NULL, 
                               filename = NULL, 
                               layout.objs = NULL, 
                               summary.title =  NULL,
                               run.title = NULL,
                               addData = FALSE,
                               useLongnames = FALSE,
                               background.colour = "transparent"){
  
  
  # IF VEGVAR HAS BEEN SUPPLIED MANY THINGS ARE DEFINED FROM IT 
  if(class(data) == "VegObj"){
    run <- data@run
    period <- data@time.span
    PFT.set <- run@pft.set
  }
  
  
  # TOLERANCE - for when making grid
  if(is.null(run)) { tolerance <- 0.02 }
  else {tolerance <- run@tolerance}  
  
  # DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  
  # CONVERT TO SPDF FOR PLOTTING (PLOTTING FACTORS VIA RASTER IS ANNOYING)
  data.toplot <- makeSPDFfromDT(data@data, layers = which.dominant, tolerance = run@tolerance)
  
  # COLORLIST
  dom.PFT.colourlist <- vector()
  DomPFTs <- vector()
  if(length(which.dominant) == 1 ) {
    class.input.data <- class(data)
    if(class.input.data == "VegObj") {
      DomPFTs <- levels(data.toplot@data[,which.dominant])
    }
  }
  else{
    stop("Construction Site:  plotDominantPFTMaps() cannot currently deal with more than one map")
    #for(var in which.dominant){
    #  DomPFTs <- append(DomPFTs, levels(data.toplot[,var,with=FALSE]))    
    #}
  }
  for(PFT.name in DomPFTs){
    if(PFT.name == "Barren"){ dom.PFT.colourlist[["Barren"]] <- "gray75"}
    else{ 
      if(useLongnames) {dom.PFT.colourlist[[PFT.set[[PFT.name]]@longname]] <- PFT.set[[PFT.name]]@colour}
      else {dom.PFT.colourlist[[PFT.name]] <- PFT.set[[PFT.name]]@colour}
    }
  }
  
  
  # FILENAME
  if(!is.null(filename)){ current.filename <- filename}
  else {
    if(!is.null(period)){ current.filename = paste(which.dominant, ".TA.", period@start, "-", period@start, ".png", sep = "") }
    else { current.filename = paste(which.dominant, ".TA.", "png", sep = "") }
  }
  
  # PLOT MAIN TITLE
  if(is.null(summary.title)) this.main.title <- makePlotTitle("Dominant PFT", summary.title, run, period)
  else this.main.title <- summary.title
  
  
  CairoPNG(file.path(plot.dir, current.filename), width = 1800, height = 1000, title = paste("Domiant PFT", sep = " "), bg = background.colour)  
  print(spplot(data.toplot,
               which.dominant,
               xlab = list(label = "Longitude", cex = 2),
               ylab = list(label = "Latitude", cex = 2),
               col.regions = dom.PFT.colourlist,
               scales = list(draw = TRUE, cex = 2),
               main=list(label=this.main.title, cex = 3),
               par.strip.text = list(lines = 1.0, cex = 1.5),
               sp.layout = layout.objs,
               names.attr = plot.labels,
               colorkey = list(col = dom.PFT.colourlist, 
                               space = "right",
                               labels = list(labels = names(dom.PFT.colourlist),
                                             at = 1:length(dom.PFT.colourlist),
                                             cex = 2))
  ))
  dev.off()
  
}


##########################################################################################################################################
################################################## PLOT BA MAPS #########################################################################
##########################################################################################################################################

plotBAMaps <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented),
                       which.layers = "all",
                       period = NULL, 
                       run = NULL, 
                       plot.dir = NULL, 
                       file.name = NULL, 
                       summary.file.name = NULL,
                       summary.title = NULL,
                       special.string = NULL,
                       layout.objs = NULL, 
                       plot.labels =  NULL,
                       useLongnames = FALSE,
                       fraction = TRUE,
                       background.colour = "transparent",
                       ...){
  
  
  
  stop("plotBAMaps obselete: using plotFireRT instead (code up the inverting!)")
  
  
}





#######################################################################################################################################
################### PLOT HISTOS FOR COMPARING MODEL AND DATA  #########################################################################
#######################################################################################################################################


plotHistoComparison <- function(model, data, run, period, data.name, quant, diff.breaks, plot.range, stat.results = NULL){
  
  if(is.null(stat.results)) stat.results <- compareTwoRastersStats(model, data)
  
  
  CairoPNG(paste(quant@id, run@id, "DiffHisto.Vs", data.name, "png", sep="."), width = 1000, height = 700, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  hist(stat.results$diff.raster,  breaks = diff.breaks, xlim = plot.range, xlab = paste(quant@id, ": ", "LPJ-GUESS - ", data.name, sep = ""), prob = TRUE, main = paste(quant@full.string, ": ", "LPJ-GUESS - ", data.name, sep = ""), cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, maxpixels =100000000, right = FALSE)
  curve(dnorm(x, mean=stat.results$mean.diff, sd=stat.results$sd.diff), add=TRUE)
  abline(v=0,col="green", lwd = 4)
  legend('topright', c( paste("Mean = ", round(stat.results$mean.diff,3)), paste("SD = ", round(stat.results$sd.diff,3))), col = c("red","blue"), text.col = c("red","blue"), cex = 3, bty = "n") 
  
  dev.off()
  
  
  CairoPNG(paste(quant@id, run@id, "OverlayHisto.Vs", data.name, "png", sep="."), width = 1000, height = 700, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  y.height <- 2*max(hist(stat.results$diff.raster, breaks = diff.breaks, plot = FALSE)$counts, hist(data, breaks = diff.breaks, plot = FALSE)$counts,  hist(model, breaks = diff.breaks, plot = FALSE)$counts)
  diff.histo <- hist(stat.results$diff.raster,  breaks = diff.breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@full.string, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis = cex.axis.multi, cex.main = 3, maxpixels =100000000, right = FALSE)
  hist(data,  breaks = diff.breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@full.string, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, border = "red", maxpixels =100000000, right = FALSE)
  hist(model,  breaks = diff.breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@full.string, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, border = "blue", maxpixels =100000000, right = FALSE)
  curve(dnorm(x, mean=stat.results$mean.diff, sd=stat.results$sd.diff)*diff(diff.histo$mids[1:2])*cellStats(is.finite(stat.results$diff.raster), stat= sum, na.rm=TRUE, asSample=FALSE), add=TRUE)
  abline(v=0,col="green", lwd = 4)
  legend('topright', c(data.name, "LPJ-GUESS", paste("LPJ-GUESS -", data.name, sep = " "), paste("Mean = ", round(stat.results$mean.diff,6)), paste("SD =", round(stat.results$sd.diff,6))), col = c("red","blue", "black", "black", "black"), text.col = c("red","blue", "black", "black", "black"), cex = 3, bty = "n") 
  
  dev.off()
  
}


#######################################################################################################################################
################### PLOT SCATTER PLOT FOR COMPARING MODEL AND DATA  ###################################################################
#######################################################################################################################################

plotScatterComparison <- function(model, data, model.run, period, data.name, quant, stat.results = NULL){
  
  if(is.null(stat.results)) stat.results <- compareTwoRastersStats(model, data)
  
  
  CairoPNG(paste(quant@id, run@id, "Scatter.Vs", data.name, "png", sep="."), width = 1000, height = 1000, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  
  plot(model, data, col = rgb(0.1,0.1,0.1,0.1), pch = 20, xlab = paste(run@description, " ", quant@full.string, " (", quant@units, ")", sep = ""), ylab = paste(data.name, " ", quant@full.string, " (", quant@units, ")", sep =""), ylim = c(quant@cuts[1],quant@cuts[length(quant@cuts)]), xlim = c(quant@cuts[1],quant@cuts[length(quant@cuts)]), main = paste("Scatter vs. ", data.name, sep = ""), maxpixels = 100000000, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 4)
  abline(0, 1, col = "red")
  legend('topleft', c(paste("RMSE:", round(stat.results$RMSE, 2), sep = " "), paste("R^2:", round(stat.results$R.squ, 2), sep = " "), paste("Pearsons:", round(stat.results$P.cor, 2), sep = " ")), text.col = c("red", "blue", "green"), cex = 3, bty = "n")
  dev.off()
  
}






#######################################################################################################################################
################### HELPER FUNCTIONS FOR MAKING PLOT TITLES, FILENAMES ETC... #########################################################
#######################################################################################################################################

# Return this plot title,
makePlotTitle <- function(quantity.str, plot.title = NULL, run = NULL, period = NULL){
  
  if(!is.null(plot.title)){ return(plot.title)}
  else if(!is.null(period) & !is.null(run)) { return(paste(quantity.str,  ": ", run@description, " (", period@start, "-", period@end, ")", sep = ""))}
  else if(is.null(period) & !is.null(run)) { return(paste(quantity.str,  ": ", run@description, sep = ""))}
  else if(is.null(run) & !is.null(period)) { return(paste(quantity.str,  ": ", " (", period@start, "-", period@end, ")", sep = ""))}
  else {return (quantity.str) }
  
}


makeFileName <- function(quantity.id, file.name = NULL, run = NULL, period = NULL, extension = "png"){
  
  if(!is.null(file.name)){return(file.name)}
  else if(!is.null(period) & !is.null(run)) { return(paste(quantity.id, ".", run@id, ".TA.", period@start, "-", period@end, ".", extension , sep = ""))}
  else if(is.null(period )& !is.null(run)) { return(paste(quantity.id, ".", run@id, ".TA.", extension , sep = ""))}
  else if(is.null(run)& !is.null(period)) {return(paste(quantity.id, ".TA.", period@start, "-", period@end, ".", extension , sep = ""))}
  else {return (paste(quantity.id, extension, sep = "."))}
  
}


makeVariableIDString  <- function(quantity.id, layer, special = NULL){
  
  if(!is.null(special)) {
    return(paste(quantity.id, special, layer, sep = "."))
  }
  else {
    return(paste(quantity.id, layer, sep = "."))
  }
  
}

# TODO MF: remove this function, I believe it is redundant

parsePFTHeaderList <- function(targets, data, PFT.data, force = FALSE){
  
  if("all" %in% targets) {
    
    if(!force) {
      
      return(names(data))}
    else {
      
      # implement a list of all PFTs, lieforms etc...
      stop("Implement option force all in parseNames")
    }
    
    
  } 
  
  
  # Make each PFT fraction
  if("pft" %in% targets) {
    all.PFTs <- getPFTs(data, PFT.data)
    for(PFT in all.PFTs) {targets <- append(targets, PFT@name)}
    targets <- targets[-which(targets == "pft")]
  }
  
  # Expand lifeforms if necessary and calculate total if necessary
  if("lifeforms" %in% tolower(targets)){
    lpj.lifeforms <- c("Tree", "Grass", "Shrub", "Woody")
    targets <- append(targets, lpj.lifeforms)
    targets <- targets[-which(tolower(targets) == "lifeforms")]
  }
  
  return(targets)
  
}


#TODO MF: replace this function with expandTargets() from lpj-processing

parseLayerList <- function(targets, data, PFT.data, force = FALSE){
  #     
  if("all" %in% targets) {
    
    if(!force) {
      layer.list <- names(data)
      remove.list <- c("Lon", "Lat")
      for(remove in remove.list){
        if(remove %in% layer.list) {layer.list <- layer.list[-which(layer.list == remove)]}     
      }
      return(layer.list)
    }
    
    else {
      
      # implement a list of all PFTs, lieforms etc...
      stop("Implement option force all in parseNames")
    }
    
    
  } 
  
  
  # Make each PFT fraction
  if("pft" %in% targets) {
    all.PFTs <- getPFTs(data, PFT.data)
    for(PFT in all.PFTs) {targets <- append(targets, PFT@name)}
    targets <- targets[-which(targets == "pft")]
  }
  
  # Expand lifeforms if necessary and calculate total if necessary
  if("lifeforms" %in% tolower(targets)){
    lpj.lifeforms <- c("Tree", "Grass", "Shrub", "Woody")
    targets <- append(targets, lpj.lifeforms)
    targets <- targets[-which(tolower(targets) == "lifeforms")]
  }
  
  return(targets)
  
}


