#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################
### This is a heavy lifting function for plotting LPJ variables flexibly but with some degree of automation
### It also acts as wrapper for non-standard plots, or at least it should soon.
#
#' Plots a map from a temporally-averaged \code{VegObject}, a data.table, a Raster* or a SpatialPixelsDataFrame.
#' 
#' The primary use for this function in to plot a map from a \code{VegObject}, although plotting a Raster* is also useful.
#' It has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults,
#' so that in the case of plotting a \code{VegObject} all you *need* to supply is the data itself, everything else is eitehr set to a sensible default,
#' or provided by the \code{VegObject} itself.  Note that the default behaviour is to write a .png to the run directory of the \code{VegObject} or,
#' for other data types, the specified plot.dir (defaulting to the current working directory in no plot.dir is supplied).
#'
#' @param data The data to plot. Can be a VegObject, data.table, a SpatialPixelsDataFrame or a Raster* object.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param expand.layers A boolean, determines wether to expand the layers arguement.  See documentation for \code{expandLayers} for details.
#' @param period The time period (represented by a \code{TemporalExtent} object), used only for plot labels and filenames.   
#' In the case of plotting a \code{VegRun} object this is taken automatically from he object, but this provides an override.
#' @param quant A \code{VegQuantity} object describy the quantity to be plotted.  This provides an override \code{VegQuant} when plotting a \code{VegObject}
#' and is useful to specify metadata (colours, plot ranges, names, etc.) when plotting other objects.
#' @param doSummary Boolean, whether to plot all \code{layers} on one plot.
#' @param doIndividual Boolean, whether to plot all \code{layers} on individual plots.
#' @param run A \code{VegRun} object from which to pull metadata.  Note that normally this information is stored in the \code{VegObject}. 
#' @param PFT.set A PFT set, necessary for exapnding layers and plotting long names.  Normally taken from the \code{VegObject}.
#' @param plot.dir A character string given the location for the plot to be saved. Usually the \code{run.dir} of the \code{VegObject}, but this provides an override.
#' If not a \code{VegObject} and not specified, this defaults to the current directory
#' @param summary.file.name A character string to override the file name of the summary plot (not including the path, just the filename).
#' @param summary.title A character string to override the title on the summary plot.
#' @param tag A character string (no spaces) used in labels and titles to differentiate these plots from similar ones.
#' For example "Corrected" or "EuropeOnly"
#' @param layout.objs List of overlays (for example coastlines or rivers) or other objects to be plotted by \code{spplot} 
#' so see the there for how to build them.  Note that the \code{map.overlay} slot of the relevant \code{VegRun} object will be plotted automatically, 
#' so that need not be specified here.
#' @param plot.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param plot.bg.col Colour string for the plot background.
#' @param useLongnames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. 
#' @param Cairo.units The units to specify the canvas size, for the Cairo graphics function.  
#' See \code{Cairo} documentation, but can generally be left at the default which is"px".
#' @param Cairo.dpi The dpi for the Cairo graphics function.  
#' See \code{Cairo} documentation, but can generally be left at the default which is 72.
#' @param Cairo.type The file format for the plot.  Default is "png"  
#' See \code{Cairo} documentation, change to "x11" for pop-up plots on GNU/linux, "win" for pop-up plots on Microsoft Windows.
#' @param Cairo.width Width of the plots (units specified in Cairo.units argument)
#' @param Cairo.height Height of the plots (units specified in Cairo.units argument)
#' @param Cairo.bg Colour string specifying the background colour of the whole plot.
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa
#' @param plot.extent An extent object to limit the plot area.
#' @param limit Boolean, whether or not to limit the plotted values between a range, either the limits argument below,
#' or the range of the plot.
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values
#' @param override.cols A colour palette function to override the defaults
#' @param override.cuts Cut ranges (a numeric vector) to override the defaults.
#' @param special A character string to indicate certain "special modes" which modifies the behaviour of the function a bit.
#' Special modes are currectly "fraction", "difference", "percentage.difference" and "firert" (fire return time).
#' Biomes, dominant PFT and burnt area fraction should be added soon.
#' @param maxpixels Maximum number of pixels to plot (see \code{raster} version of \code{spplot})
#' @param biome.scheme A biome scheme of type BiomeScheme.  This control the defines the biomes types, names, colours for 
#' plotting etc (only used if argument "special" = "biomes")
#' @param biome.data A biome dataset, as a Raster or a SpatialDataset (can be left NULL to plot no data, 
#' only used if argument "special" = "biomes")
#' @param kappa.list A list of Kappa scores (to be coded in a more friendly style, 
#' only used if argument "special" = "biomes")
#' @param kappa.position A numeric vector with two elements to define the position (in Lon-Lat) of the overall 
#' Kappa score on the plot (only used if argument "special" = "biomes") 
#' @param ... Extra arguments to be be passed to \code{spplot} and therefor also to \code{lattice::levelplot}.
#' 
#' This function is heavily used by the benchmarking functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' However, the \code{plotGGSpatial} function makes pretty plots with a simpler syntax, but with less flexibility.
#' 
#' The function works best for \code{VegObjects} (which contain a lot of useful metadata).   
#' 
#' @return Nothing, just makes the plot
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @importFrom Cairo Cairo
#' @import raster data.table
#' @export 
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotVegMaps <- function(data, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a VegObject
                        layers = NULL,
                        expand.layers = TRUE,
                        quant = NULL, 
                        period = NULL, 
                        doSummary = TRUE, 
                        doIndividual = FALSE, 
                        run = NULL, 
                        PFT.set = global.PFTs,
                        plot.dir = NULL, 
                        summary.file.name = NULL,
                        summary.title = NULL,
                        tag = NULL,
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
                        special = "none",
                        maxpixels = 1E6,
                        biome.scheme = Smith2014.scheme,
                        biome.data = NULL,
                        kappa.list = NULL,
                        kappa.position = NULL,
                        ...){
  
  
  ###################################################################################################
  ### PRE-AMBLE:                                                             ########################
  ### 1. INITIAL ARGUMENT CHACKSWND WARNINGS ABOUT ARGUMENT  COMBINATIONS    ########################
  ### 2. WHEN THINGS ARE NOT SPECIFIED, PULL SOME DEFAULTS                   ########################
  ###################################################################################################
  
  ##### 1. ARGUMENTS, WARNINGS
  
  ### HANDLE SPECIAL
  
  if(!is.null(special)) special <- tolower(special)
  
  special <- match.arg(special, 
                       c("none",
                         "difference", 
                         "percentage.difference",
                         "perc.diff",
                         "fraction",
                         "burnt.area",
                         "burnt.fraction",
                         "firert",
                         "fire.return.time",
                         "biomes",
                         "dominant.pft"))
  
  special <- switch(special,
                    none = "none", 
                    difference = "difference", 
                    percentage.difference  = "percentage.difference",
                    perc.diff= "percentage.difference",
                    fraction = "fraction",
                    burnt.area = "burnt.area",
                    burnt.fraction = "burnt.fraction",
                    firert = "fire.return.time",
                    fire.return.time = "fire.return.time",
                    biomes = "biomes" ,
                    dominant.pft = "dominant" )
  
  # Warn ig quant is being ignored in special
  if(!is.null(quant) & (special == "dominant" | special == "biomes"))
    warning(paste("When using a \"special\" = ", special, ", argument \"quant\" is ignored"))
  
  
  
  
  ##### 2. DEFAULTS
  
  ### IF IS VEGOBJECT MANY THINGS ARE AVAILABLE FROM IT
  if(is.VegObject(data)){
    if(data@is.temporally.averaged){
      run <- data@run
      run.id <- run@id
      period <- data@temporal.extent
      PFT.set <- run@pft.set
      if(is.null(quant)) quant <- data@quant
    } 
    else {
      stop("plotVegMaps:: trying to plot a VegObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
    }
  }
  else{
    if(!is.null(run)) run.id <- run@id
    else run.id <- NULL
  }
  
  ### DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  ### QUANTITY AND COLS
  if(is.null(quant)){ quant = lpj.quantities[["generic"]]}
  else if(class(quant) == "character"){ quant = lpj.quantities[[quant]] }
  else if((class(quant) != "VegQuant")){
    warning("Invalid quantity found in plotLPJMaps using generic quantity")
    quant = lpj.quantities[["generic"]]
  }
  
  
  ### COLORKEY - standard, updated by specials below
  colorkey.list <- list(space = "right", 
                        col = quant@colours, 
                        labels = list(cex = 3 * text.multiplier)
  )
  
  ### LAYOUT OBJECTS - if a run has been supplied and it has a valid map.overlay field the add it
  if(!is.null(run)){
    if(!is.null(run@map.overlay)) {
      layout.objs <- append(layout.objs, run@map.overlay)
    }
  }
  
  
  
  #####################################################################################
  ############# PREPARE DATA AND layer LIST FOR PLOTTING        ######################
  #####################################################################################
  
  ### TOLERANCE - for when making grid to rasterise
  if(is.null(run)) { tolerance <- 0.02 }
  else {tolerance <- run@tolerance}  
  
  ### SPECIAL CASE OF BIOMES OR DOMINANT WITH NULL layer (assume the biome scheme id is the layer name)
  if(is.null(layers) & !is.null(special)){
    if(special == "biomes") layers = biome.scheme@id
    if(special == "dominant") layers = "Dominant"
  }
  
  ### EXPAND layerS
  if(is.null(layers)) {
    if(is.VegObject(data)) layers <- names(data@data)
    else layers <- names(data)
  }
  if(expand.layers) {
    layers <- expandLayers(layers, data, PFT.set)
    if(!is.null(special)){
      if(special == "fraction") layers <- paste(layers, "Fraction", sep = sep.char)
    }
  }
  
  ### PROMOTE TO RASTER AND SANITISE NAMES - also make plot labels (if necessary) before the sanitatisation 
  original.layers <- layers # save for building plot label later
  data.toplot <- promoteToRaster(data, layers, tolerance)
  layers <- names(data.toplot) # update layers to be the names of the raster layers  (which might have changed)
  
  ### CROP THE DATA IF PLOT EXTENT HAS BEEN SPECIFIED
  if(!is.null(plot.extent)){ data.toplot <- crop(data.toplot, plot.extent)}
  
  
  #####################################################################################
  ############# DEAL WITH SPECIAL CASES    ############################################
  #####################################################################################
  
  ### IF SPECIALS 
  
  
  ### PLOT DIFFERENCE MAPS
  if(special == "difference"){
    
    minmax <- max(quant@cuts) - min(quant@cuts)
    step <- (max(quant@cuts) - min(quant@cuts)) / (length(quant@cuts) - 1)
    quant@cuts <- seq(from = -minmax, to = minmax, by = step)
    quant@colours <- colorRampPalette(c("green","blue","white","red", "yellow"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    quant@id <-  paste(quant@id, "diff", sep =".")
    quant@short.string = paste(quant@short.string, "Diff", sep = ".")
    quant@full.string = paste("Difference: ", quant@full.string, sep = "")
    plot.bg.col <- "grey"
    
  }
  
  ### PLOT PERCENTAGE DIFFERENCE MAPS
  else if(special == "percentage.difference"){
    
    quant@cuts <- seq(from = -100, to = 200, by = 10)
    quant@colours <- colorRampPalette(c("blue","white","red", "yellow"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    quant@id <-  paste(quant@id, "percdiff", sep =".")
    quant@short.string = paste(quant@short.string, "PercDiff", sep = ".")
    quant@full.string = paste("Percentage Difference: ", quant@full.string, sep = "")
    plot.bg.col <- "grey"
    
    # SET THE INTERVALS (using either these sensible options or the overrides)
    # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
    if(!is.null(override.cols)) {quant@colours <- override.cols}
    if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
    override.cols = override.cuts = NULL
    
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    # get lowest and highest '50's
    smallest.limit = min(abs(quant@cuts[1]), abs(quant@cuts[length(quant@cuts)]))
    interval <- 50
    if(smallest.limit < 100) interval <- 25
    if(smallest.limit < 50) interval <- 10
    if(smallest.limit < 20) interval <- 5
    if(smallest.limit < 10) interval <- 1
    lower <- ceiling(quant@cuts[1]/interval) * interval
    upper <- floor(quant@cuts[length(quant@cuts)]/interval) * interval
    colourkey.at <- seq(lower, upper, by = interval)
    colorkey.labels <- paste(seq(lower, upper, by = interval))
    for(label.index in 1:length(colorkey.labels)){
      if(!(substr(colorkey.labels[label.index], 1, 1) == "-" | substr(colorkey.labels[label.index], 1, 1) == "0")){
        colorkey.labels[label.index] <- paste0("+", colorkey.labels[label.index])
      }
    }
    if(limit) colorkey.labels[length(colorkey.labels)] <- paste0("\u2265", colorkey.labels[length(colorkey.labels)])
    colorkey.labels <- paste(colorkey.labels, "%", sep = "")
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = colourkey.at)
    colorkey.list[["at"]] <- quant@cuts
    
    
    
  }
  
  #### PLOT FRACTION MAPS
  else if(special == "fraction"){
    
    quant@cuts <- seq(from = 0, to = 1, by = 0.05)
    quant@id <- paste(quant@id, "fraction", sep = ".")
    quant@short.string <- paste(quant@short.string, "fraction", sep = ".")
    quant@full.string <- paste(quant@full.string, "Fraction", sep = " ")
    quant@colours <- colorRampPalette(c("grey85", "black"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    
  }
  
  #### PLOT BURNT FRACTION
  else if(special == "burnt.fraction"){
    
    
    stop("plotVegMaps: special burnt.fraction or ba not impletemted yet")
    
  }
  
  #### PLOT BURNT FRACTION
  else if(special == "burnt.area"){
    
    
    stop("plotVegMaps: special burnt.fraction or ba not impletemted yet")
    
  }
  
  #### PLOT FIRE RETURN TIME
  else if(special == "fire.return.time" | quant@id == "firert"){
    
    # SET THE INTERVALS (using either these sensible options or the overrides)
    quant@cuts <- c(0, 1, 3, 5, 10, 25, 50, 100, 200, 400, 800, 1000)
    quant@colours <-  colorRampPalette(c("black", "red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3"))
    # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
    if(!is.null(override.cols)) {quant@colours <- override.cols}
    if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
    override.cols = override.cuts = NULL
    
    # RECLASSIFY THE DATA ACCORDING TO THE CUTS 
    temp.names <- names(data.toplot)
    data.toplot <- cut(data.toplot, quant@cuts) 
    names(data.toplot) <- temp.names
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.labels <- paste(quant@cuts)
    colorkey.labels[length(colorkey.labels)] <- paste0(colorkey.labels[length(colorkey.labels)], "+")
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = 0:(length(quant@cuts)-1))
    colorkey.list[["at"]] <- 0:(length(quant@cuts)-1)
    quant@cuts = 0:(length(quant@cuts)-1)
    
  }
  
  #### PLOT BIOMES
  else if(special == "biomes"){
    
    # Build a VegQuant object with the appropriate colours and cuts 
    quant <- new("VegQuant",
                 id = biome.scheme@id,
                 short.string = biome.scheme@id,
                 full.string = biome.scheme@name,
                 type = "BiomeClassification",
                 units = "categorical",
                 colours = colorRampPalette(biome.scheme@cols),
                 cuts = 0:length(biome.scheme@strings),
                 aggregate.method = "categorical"
    )
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex * 2/3, 
                                      "labels" = rev(biome.scheme@strings), 
                                      "at" = (0:(length(biome.scheme@strings)-1)) + 0.5)
    colorkey.list[["at"]] <- 0:length(biome.scheme@strings)
    colorkey.list[["col"]] <- rev(biome.scheme@cols)
    
    # Add PNV data if requested read it in and compare rasters
    if(!is.null(biome.data)) {
      data.name <- "Data"
      if(class(biome.data)[[1]] == "SpatialDataset") {
        data.name <- biome.data@name
        biome.data <- biome.data@data
      }
      # first check if they are on identical grids, then one can simply add the layers
      if(compareRaster(biome.data, data.toplot, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=FALSE)){
        print("woohoo!")
      }
      else if(compareRaster(biome.data, data.toplot, extent=FALSE, rowcol=FALSE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=FALSE)){
        biome.data <- crop(biome.data, data.toplot, snap = "out")
        biome.data <- extend(biome.data, data.toplot)
      }
      else {
        biome.data <- resample(biome.data, data.toplot, method = "ngb")
        
      }
      biome.data <- crop(biome.data, data.toplot)
      data.toplot <- mask(data.toplot, biome.data)  
      
      # add the PNV raster layer and its title
      data.toplot <- addLayer(data.toplot, biome.data) 
      layers <- names(data.toplot)
      
      # And finally build plot labels
      
      original.layers <- c(original.layers, data.name)
      if(!is.null(plot.labels)) plot.labels <-  c(plot.labels, data.name) 
      
    }
    
    # KAPPA
    if(!is.null(kappa.list)){
      
      # if only one model run put individual Kappas into biome legend
      if(nlayers(data.toplot)-1 == 1) colorkey.list[["labels"]][["labels"]] <- paste0(colorkey.list[["labels"]][["labels"]], " (", rev(round(kappa.list[[1]]@individual.Kappas,2)), ")", sep = "")
      # place overall Kappa on each modelled biome map 
      if(is.null(kappa.position)) { kappa.position <- c(extent(data.toplot)@xmin * 0.8, extent(data.toplot)@ymin * 0.8) }
      for(layer in 1:(nlayers(data.toplot)-1)) {
        layout.objs[[paste(layer)]]  <- list("sp.text", loc = kappa.position, txt = paste0("Kappa = ", round(kappa.list[[layer]]@Kappa,3)), which = layer, cex = 1.5)
      }
      
    }
    
    
  }
  
  #### PLOT BIOMES
  
  else if(special == "dominant"){
    
    
    # Get Raster Attribute Table and then convert raster back to simple integers instead of factors 
    # because currently (May 2016) raster package doesn't handle factor rasters very well
    RAT <- data.toplot@data@attributes[[1]]
    data.toplot <- deratify(data.toplot, complete = TRUE)
    
    # Set colours to be the list of PFT colours
    col.list <- c()
    label.list <- c()
    
    # for each row of the RAT
    for(row.index in 1:NROW(RAT)){
      
      # get the row
      row <- RAT[row.index,]
      
      # special case if barren
      if(as.character(row$levels == "Barren")) {
        col.list <- append(col.list, "gray75")
        label.list <- append(label.list, "Barren")
      }
      
      # else (assuming others rows are PFTs)
      else{
        
        # get the PFT data, append the colour and the id/long name (as appropriate)
        PFT <- byIDfromList(as.character(row$levels), PFT.set)
        col.list <- append(col.list, PFT@colour)
        if(useLongnames) label.list <- append(label.list, PFT@name)
        else  label.list <- append(label.list, PFT@id)
        
      }
      
    }
    
    
    quant <- new("VegQuant",
                 id = "Domiant",
                 short.string = "Domiant",
                 full.string = "Dominant PFTs",
                 type = "DominantPFTs",
                 units = "categorical",
                 colours = colorRampPalette(col.list),
                 cuts = 0:length(col.list),
                 aggregate.method = "categorical"
    )
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, 
                                      "labels" = rev(label.list), 
                                      "at" = ((0:length(col.list))-1) + 0.5)
    colorkey.list[["at"]] <- 0:length(col.list)
    colorkey.list[["col"]] <- rev(col.list)
    
  }
  
  #### CATCH UNIMPLEMENTED SPECIAL CASES
  else if(!(special != "" | special != "none")){
    stop(paste("Special case", tolower(special), "not implemented yet", sep = " "))
  }
  
  
  ### OVERRIDE (CUTS AND COLOUR SCHEME) 
  if(!is.null(override.cols)) {quant@colours <- override.cols}
  if(!is.null(override.cuts)) {quant@cuts <- override.cuts}
  
  
  
  
  #####################################################################################
  ############# LIMIT THE PLOTTED VARIABLE IF REQUESTED################################
  ## Not really recommended as it hides scientific content, but can make plots nicer ##
  #####################################################################################
  
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
    
    for(layer in 1:nlayers(data.toplot)) {
      this.layer <- subset(data.toplot, layer)
      this.layer[this.layer < limits[1]] <- limits[1]
      this.layer[this.layer > limits[2]] <- limits[2]
      if(exists("data.limited")) data.limited <- addLayer(data.limited, this.layer)
      else data.limited <- brick(this.layer)
      rm(this.layer)
    }
    rm(data.toplot)
    data.toplot <- data.limited
    rm(data.limited)
    
  }
  
  
  
  #####################################################################################
  ############# MAKE THE PLOTS ########################################################
  #####################################################################################
  
  ### CHECK SUMMARY/INDIVIDUAL
  # If only one layer has been selected, don't plot as summary, plot it as individual, regardless of settings
  if(nlayers(data.toplot) == 1){
    doSummary <- FALSE
    doIndividual <- TRUE
  }
  
  for(format in Cairo.type){
    
    ### PRINT SUMMARY PLOT WITH ALL DATA IN ONE PLOT
    if(doSummary){
      
      # FILENAME
      # make a description of the variable
      this.id.string <- makeVariableIDString(quant@id, "Summary",  run.id = run.id, tag)
      if(is.null(summary.file.name)) this.file.path <- file.path(plot.dir, paste(makeVegObjectID(this.id.string, temporal.extent = period, spatial.extent = NULL, temporally.averaged = TRUE, spatially.averaged = FALSE), format, sep = "."))
      else this.file.path <- file.path(plot.dir, paste(summary.file.name, format, sep = "."))
      
      # Special case for frmat "x11", filename should be ""
      if(format == "x11") this.file.path <- ""
      
      
      # PLOT MAIN TITLE
      if(is.null(summary.title)) summary.title <- makePlotTitle(paste(quant@full.string, "Summary", sep = " "), run, period)
      
      # PANEL LABELS - note expand longnames here if requested 
      if(is.null(plot.labels)) {
        plot.labels.here <- original.layers
        ### USE LONGNAMES - for PFTs
        if(useLongnames) {
          for(plot.label.index in 1:length(plot.labels.here)){
            # look up PFT
            for(PFT in PFT.set){
              if(plot.labels.here[plot.label.index] == PFT@id) plot.labels.here[[plot.label.index]] <- unlist(PFT@name)
            }
          }
          plot.labels.here<- unlist(plot.labels.here)
        }
      }
      else plot.labels.here <- plot.labels
      
      Cairo(file = this.file.path, 
            dpi = Cairo.dpi, 
            type = format, 
            width = Cairo.width, 
            height = Cairo.height, 
            title = summary.title, 
            bg = Cairo.bg)  
      
      
      print(spplot(data.toplot,
                   layers,
                   par.settings = list(panel.background=list(col=plot.bg.col)),
                   xlab = list(label = "Longitude", cex = 3 * text.multiplier),
                   ylab = list(label = "Latitude", cex = 3 * text.multiplier),
                   col.regions= quant@colours,
                   colorkey = colorkey.list,                                                                                            
                   at = quant@cuts,
                   scales = list(draw = TRUE, cex = 3 * text.multiplier),
                   as.table = TRUE,
                   main=list(label=summary.title, 
                             cex = 4 * text.multiplier),
                   par.strip.text = list(#lines = 1.0, 
                     cex = 2 * text.multiplier),
                   sp.layout = layout.objs,
                   maxpixels = maxpixels,
                   names.attr = plot.labels.here,
                   ...)
      )
      
      if(format == "x11")  invisible(readline(prompt="Press [enter] to continue"))
      
      dev.off()
      
    }
    
    ### PRINT INDIVIUAL PLOTS
    if(doIndividual){
      
      for(layer in layers){
        
        # FILENAME
        this.id.string <- makeVariableIDString(quant@id, layer, run.id = run.id, tag)
        this.file.path <- file.path(plot.dir, paste(makeVegObjectID(this.id.string, temporal.extent = period, spatial.extent = NULL, temporally.averaged = TRUE, spatially.averaged = FALSE), format, sep = "."))
        # Special case for format "x11", filename should be NULL
        if(format == "x11") this.file.path <- ""
        
        # PLOT TITLES
        # If only one layer overall
        if(length(layers) == 1){
          if(is.null(summary.title)) plot.title <- makePlotTitle(paste(quant@full.string, layer, sep = " "), run, period)
          else plot.title <- summary.title
        }
        # If many individuals
        else {
          if(!is.null(plot.labels[which(layer == layers)])) {
            plot.title <- plot.labels[which(layer == layers)]
          }
          else { 
            plot.title <- makePlotTitle(paste(quant@full.string, tag, layer, sep = " "), run, period) 
            # if expand PFT names
            if(useLongnames) {
              # look up PFT
              for(PFT in PFT.set){
                if(layer == PFT@id) plot.title <- makePlotTitle(paste(quant@full.string, tag, PFT@name, sep = " "), run, period)
              }
            }
          }
        }
        
        
        Cairo(file = this.file.path, 
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
                     colorkey = colorkey.list,
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
        
        if(format == "x11")  invisible(readline(prompt="Press [enter] to continue"))
        
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

# Obselete, Fold into plotVegMaps() with special = "biomes"
.plotBiomeMap <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                          layers = NULL,
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
                          kappa.list = NULL,
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
  
  ### IF VEGOBJECT HAS BEEN SUPPLIED MANY THINGS ARE DEFINED FROM IT 
  if(is.VegObject(data)){
    if(data@is.temporally.averaged){
      run <- data@run
      period <- data@temporal.extent
      PFT.set <- run@pft.set
    } 
    else {
      stop("plotBiomeMaps:: trying to plot a VegObject which has not been temporally averaged.  This is crazy, what do I do with all these years of data?!")
    }
  }
  
  # IF NO LAYERS, COLS OR STRINGS SUPPLIED, USE THE  DEFAULTS OF THE SCHEME
  if(is.null(layers)) layers = scheme@id
  if(is.null(biome.strings)) biome.strings = scheme@strings
  if(is.null(biome.cols)) biome.cols = scheme@cols
  
  # TOLERANCE - for when making grid
  if(is.null(run)) { tolerance <- 0.02 }
  else {tolerance <- run@tolerance}  
  
  # DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  ##### Here check data is right class for plotting, process it if not
  data.toplot <- promoteToRaster(data, layers, run@tolerance) 
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
  
  # LAYOUT OBJECTS - if a run has been supplied and it has a valid map.overlay field
  if(!is.null(run)){
    if(!is.null(run@map.overlay)) {layout.objs <- append(layout.objs, run@map.overlay)}
  }
  
  
  # Add PNV data if requested read it in and compare rasters
  if(!is.null(addData)) {
    if(class(addData)[[1]] == "SpatialDataset") addData <- addData@data
    
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
    data.toplot <- addLayer(data.toplot, addData) 
    plot.labels <- c(plot.labels, "PNV (Hickler et al. 2006)")
    
  }
  
  # KAPPA
  if(!is.null(kappa.list)){
    
    # if only one model run put individual Kappas into biome legend
    if(nlayers(data.toplot)-1 == 1) biome.strings <- paste0(biome.strings, " (", round(kappa.list[[1]]@individual.Kappas,2), ")", sep = "")
    # place overall Kappa on each modelled biome map 
    if(is.null(kappa.position)) { kappa.position <- c(extent(data.toplot)@xmin * 0.8, extent(data.toplot)@ymin * 0.8) }
    for(layer in 1:(nlayers(data.toplot)-1)){
      layout.objs[[paste(layer)]] <- list("sp.text", kappa.position, paste0("Kappa = ", round(kappa.list[[layer]]@Kappa,3)), which = layer, cex = 1.5)
    }
    
  }
  
  # PLOT MAIN TITLE
  if(is.null(main.title)) main.title <- makePlotTitle(paste(scheme@id, "Biomes", sep =""), run, period)
  
  for(format in Cairo.type){
    
    # FILENAME
    if(is.null(file.name)) this.file.name <- .makeFileName(paste(scheme@id, "Biomes", sep =""), file.name = file.name, run = run, period = period, extension = format)
    else this.file.name <- paste(file.name, format, sep = ".")
    
    Cairo(file = file.path(plot.dir, this.file.name), 
          dpi = Cairo.dpi, 
          type = format, 
          width = Cairo.width, 
          height = Cairo.height, 
          title = main.title, 
          bg = Cairo.bg)  
    
    print(spplot(data.toplot,
                 xlab = list(label = "Longitude", cex = 2),
                 ylab = list(label = "Latitude", cex = 2),
                 col.regions = biome.cols,
                 at = 0:length(biome.strings),
                 scales = list(draw = TRUE, cex = 2),
                 as.table = TRUE,
                 main=list(label=main.title, cex = 3),
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
################################################## PLOT DOMINANT #########################################################################
##########################################################################################################################################


.plotDominantPFTMap <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                                which.dominant = "Dominant",
                                quant = NULL, 
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
  
  
  
  
  ### IF IS VEGOBJECT THE SIMILAR
  if(is.VegObject(data)){
    if(data@is.temporally.averaged){
      run <- data@run
      period <- data@temporal.extent
      PFT.set <- run@pft.set
      if(is.null(quant)) quant <- data@quant  
    } 
    else {
      stop("plotDominantPFTMap:: trying to plot a VegObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
    }
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
    if(is.VegObject(data)) {
      DomPFTs <- levels(data.toplot@data[,which.dominant])
    }
  }
  else{
    stop("Construction Site:  plotDominantPFTMaps() cannot currently deal with more than one map")
    #for(var in which.dominant){
    #  DomPFTs <- append(DomPFTs, levels(data.toplot[,var,with=FALSE]))    
    #}
  }
  for(PFT.id in DomPFTs){
    if(PFT.id == "Barren"){ dom.PFT.colourlist[["Barren"]] <- "gray75"}
    else{ 
      if(useLongnames) {dom.PFT.colourlist[[PFT.set[[PFT.id]]@id]] <- PFT.set[[PFT.id]]@colour}
      else {dom.PFT.colourlist[[PFT.id]] <- PFT.set[[PFT.id]]@colour}
    }
  }
  
  # LAYOUT OBJECTS
  if(!is.null(run@map.overlay)) {layout.objs <- append(layout.objs, run@map.overlay)}  
  
  # FILENAME
  if(!is.null(filename)){ current.filename <- filename}
  else {
    if(!is.null(period)){ current.filename = paste(which.dominant, "by", quant@id, ".TA.", period@start, "-", period@start, ".png", sep = "") }
    else { current.filename = paste(which.dominant, "by", quant@id, ".TA.", "png", sep = "") }
  }
  
  # PLOT MAIN TITLE
  if(is.null(summary.title)) summary.title <- makePlotTitle(paste("Dominant PFT by", quant@id, sep = " "), run, period)
  
  CairoPNG(file.path(plot.dir, current.filename), 
           width = 1800, 
           height = 1000, 
           title = paste("Domiant PFT", sep = " "), 
           bg = background.colour)  
  print(spplot(data.toplot,
               which.dominant,
               xlab = list(label = "Longitude", cex = 2),
               ylab = list(label = "Latitude", cex = 2),
               col.regions = dom.PFT.colourlist,
               scales = list(draw = TRUE, cex = 2),
               main=list(label=summary.title, cex = 3),
               par.strip.text = list(lines = 1.0, cex = 1.5),
               sp.layout = layout.objs,
               #names.attr = plot.labels,
               colorkey = list(col = dom.PFT.colourlist, 
                               space = "right",
                               labels = list(labels = names(dom.PFT.colourlist),
                                             at = 1:length(dom.PFT.colourlist),
                                             cex = 2))
  ))
  dev.off()
  
}


#######################################################################################################################################
################### PLOT HISTOS FOR COMPARING MODEL AND DATA  #########################################################################
#######################################################################################################################################

#' Make a histograms by comparing two rasters
#' 
#' Takes either two rasters or a \code{RasterComparion} object and makes a histograms.
#' 
#' Probably should be made a method of \code{RasterComparion}, 
#' with further additions to that class necessary.
#'  
#' @param model Raster of the model
#' @param data Raster of the data
#' @param run The \code{VegRun} object for the run plotted (optional)
#' @param data.name Character string for the data
#' @param quant The quantity plotted (as \code{VegQuant} object)
#' @param plot.range A numerical vector with two elements defining the range to plot on the histogram.
#' @param stat.results The \code{RasterComparion} object if it has already been calculated
#'
#' The plot is saved to the run directory of the run object
#' @importFrom Cairo CairoPNG
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 

plotHistoComparison <- function(model,
                                data, 
                                run, 
                                data.name, 
                                quant, 
                                plot.range, 
                                stat.results = NULL){
  
  if(is.null(stat.results)) stat.results <- compareTwoRastersStats(model, data)
  
  # set min and max in a robust way for the overlay hist
  setMinMax(stat.results@diff.raster)
  setMinMax(data)
  setMinMax(model)
  
  breaks.max <- ceiling(max(maxValue(stat.results@diff.raster), maxValue(data),maxValue(model)))
  breaks.min <- floor(min(minValue(stat.results@diff.raster), minValue(data),minValue(model)))
  if(breaks.max - breaks.min > 100) breaks <- seq(breaks.min, breaks.max, by = 10) 
  else if(breaks.max - breaks.min < 10) breaks <- seq(breaks.min, breaks.max, by = 0.1)
  else breaks <- seq(breaks.min, breaks.max, by = 1)
  
  CairoPNG(file.path(run@run.dir, paste(quant@id, run@id, "DiffHisto.Vs", data.name, "png", sep=".")), width = 1000, height = 700, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  hist(stat.results@diff.raster,  breaks = breaks, xlim = plot.range, xlab = paste(quant@id, ": ", "LPJ-GUESS - ", data.name, sep = ""), prob = TRUE, main = paste(quant@full.string, ": ", "LPJ-GUESS - ", data.name, sep = ""), cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, maxpixels =100000000, right = FALSE)
  x = NULL
  curve(dnorm(x, mean=stat.results@mean.diff, sd=stat.results@sd.diff), add=TRUE)
  abline(v=0,col="green", lwd = 4)
  legend('topright', c( paste("Mean = ", round(stat.results@mean.diff,3)), paste("SD = ", round(stat.results@sd.diff,3))), col = c("red","blue"), text.col = c("red","blue"), cex = 3, bty = "n") 
  
  dev.off()
  
  CairoPNG(file.path(run@run.dir, paste(quant@id, run@id, "OverlayHisto.Vs", data.name, "png", sep=".")), width = 1000, height = 700, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  y.height <- 2*max(hist(stat.results@diff.raster, breaks = breaks, plot = FALSE)$counts, hist(data, breaks = breaks, plot = FALSE)$counts,  hist(model, breaks = breaks, plot = FALSE)$counts)
  diff.histo <- hist(stat.results@diff.raster,  breaks = breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@full.string, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis = cex.axis.multi, cex.main = 3, maxpixels =100000000, right = FALSE)
  hist(data,  breaks = breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@full.string, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, border = "red", maxpixels =100000000, right = FALSE)
  hist(model,  breaks = breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@full.string, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, border = "blue", maxpixels =100000000, right = FALSE)
  curve(dnorm(x, mean=stat.results@mean.diff, sd=stat.results@sd.diff)*diff(diff.histo$mids[1:2])*cellStats(is.finite(stat.results@diff.raster), stat= sum, na.rm=TRUE, asSample=FALSE), add=TRUE)
  abline(v=0,col="green", lwd = 4)
  legend('topright', c(data.name, "LPJ-GUESS", paste("LPJ-GUESS -", data.name, sep = " "), paste("Mean = ", round(stat.results@mean.diff,6)), paste("SD =", round(stat.results@sd.diff,6))), col = c("red","blue", "black", "black", "black"), text.col = c("red","blue", "black", "black", "black"), cex = 3, bty = "n") 
  
  dev.off()
  
}


#######################################################################################################################################
################### PLOT SCATTER PLOT FOR COMPARING MODEL AND DATA  ###################################################################
#######################################################################################################################################

#' Make a scatter plot by comparing two rasters
#' 
#' Takes either two rasters or a \code{RasterComparion} object and makes a scatter plot.
#' 
#' Probably should be made a method of \code{RasterComparion}, 
#' with further additions to that class necessary.
#'  
#' @param model Raster of the model
#' @param data Raster of the data
#' @param run The \code{VegRun} object for the run plotted (optional)
#' @param data.name Character string for the data
#' @param quant The quantity plotted (as \code{VegQuant} object)
#' @param stat.results The \code{RasterComparion} object if it has already been calculated
#'
#' The plot is saved to the run directory of the run object
#' @importFrom Cairo CairoPNG
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 

plotScatterComparison <- function(model, 
                                  data, 
                                  run, 
                                  data.name, 
                                  quant, 
                                  stat.results = NULL){
  
  if(is.null(stat.results)) stat.results <- compareTwoRastersStats(model, data)
  
  
  CairoPNG(file.path(run@run.dir,paste(quant@id, run@id, "Scatter.Vs", data.name, "png", sep=".")), width = 1000, height = 1000, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  
  plot(model, data, col = rgb(0.1,0.1,0.1,0.1), pch = 20, xlab = paste(run@description, " ", quant@full.string, " (", quant@units, ")", sep = ""), ylab = paste(data.name, " ", quant@full.string, " (", quant@units, ")", sep =""), ylim = c(quant@cuts[1],quant@cuts[length(quant@cuts)]), xlim = c(quant@cuts[1],quant@cuts[length(quant@cuts)]), main = paste("Scatter vs. ", data.name, sep = ""), maxpixels = 100000000, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 4)
  abline(0, 1, col = "red")
  legend('topleft', c(paste("RMSE:", round(stat.results@RMSE, 2), sep = " "), paste("R^2:", round(stat.results@R.squ, 2), sep = " "), paste("Pearsons:", round(stat.results@P.cor, 2), sep = " ")), text.col = c("red", "blue", "green"), cex = 3, bty = "n")
  dev.off()
  
}






#######################################################################################################################################
################### HELPER FUNCTIONS FOR MAKING PLOT TITLES, FILENAMES ETC... #########################################################
#######################################################################################################################################

#' Make a plot title
#' 
#' Build an appropriate plot title from some possibly relevant variables.  
#' It will use a string to represent the quantity (obligatory), and optionally a period and an ID.
#' 
#' @param quantity.str Character string for the quantity plotted
#' @param run The \code{VegRun} object for the run plotted (optional)
#' @param period The time period plotted as \code{TemporalExtent} (optional)
#' @return A character string for use as a plot title
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(quantity.str, run = NULL, period = NULL){
  
  
  if(!is.null(period) & !is.null(run)) { return(paste(quantity.str,  ": ", run@description, " (", period@start, "-", period@end, ")", sep = ""))}
  else if(is.null(period) & !is.null(run)) { return(paste(quantity.str,  ": ", run@description, sep = ""))}
  else if(is.null(run) & !is.null(period)) { return(paste(quantity.str,  ": ", " (", period@start, "-", period@end, ")", sep = ""))}
  else {return (quantity.str) }
  
}



# obselete can be removed with plotBiomes(), don't need to document
.makeFileName <- function(quantity.id, file.name = NULL, run = NULL, period = NULL, extension = "png"){
  
  if(!is.null(file.name)){return(file.name)}
  else if(!is.null(period) & !is.null(run)) { return(paste(quantity.id, ".", run@id, ".TA.", period@start, "-", period@end, ".", extension , sep = ""))}
  else if(is.null(period )& !is.null(run)) { return(paste(quantity.id, ".", run@id, ".TA.", extension , sep = ""))}
  else if(is.null(run)& !is.null(period)) {return(paste(quantity.id, ".TA.", period@start, "-", period@end, ".", extension , sep = ""))}
  else {return (paste(quantity.id, extension, sep = "."))}
  
}


#' Make a variable ID
#' 
#' Helper function to build an appropriate variable ID (for use in filenames) from some variables. 
#' 
#' @param quantity.str Character string for the quantity plotted (eg. "lai")
#' @param layer Character string the particular subset of the quantity being plotted (eg. "Evergreen" or a PFT id)
#' @param run.id Character string, the ID of the run
#' @param special Character string, a free choice to provide extar information or differentiate this from other varibles (eg. "LanduseCorrected")
#' @return A character string to describe a variable in a file name.  For example "lai.Evergreen.Test1.LanduseCorrected".
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @export 

makeVariableIDString  <- function(quantity.id, layer = NULL, run.id = NULL, special = NULL){
  
  string <- quantity.id
  if(!is.null(layer)) string <- paste(string, layer, sep = ".")
  if(!is.null(run.id)) string <- paste(string, run.id, sep = ".")
  if(!is.null(special)) string <- paste(string, special, sep = ".")
  return(string)
  
}
