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
#' @param run A \code{VegRun} object from which to pull metadata.  Note that normally this information is stored in the \code{VegObject}. 
#' @param PFT.set A PFT set, necessary for exapnding layers and plotting long names.  Normally taken from the \code{VegObject}.
#' @param plot.dir A character string given the location for the plot to be saved. Usually the \code{run.dir} of the \code{VegObject}, but this provides an override.
#' If not a \code{VegObject} and not specified, this defaults to the current directory
#' @param title A character string to override the title on the summary plot.
#' @param tag A string with which to tag the resulting plots to specify the analysis (to differentiate them from other plots),
#' for example "ForcingDatasetComparison" or "SoilDepths.  Or whatever you want.
#' @param layout.objs List of overlays (for example coastlines or rivers) or other objects to be plotted by \code{spplot} 
#' so see the there for how to build them.
#' @param plot.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param plot.bg.col Colour string for the plot background.
#' @param useLongnames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. 
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
#' @return Returns a plot object (from spplot)
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
                        run = NULL, 
                        PFT.set = global.PFTs,
                        plot.dir = NULL, 
                        title = NULL,
                        tag = NULL,
                        layout.objs = NULL, 
                        plot.labels =  NULL,
                        plot.bg.col =  "transparent",
                        useLongnames = FALSE,
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
      stop("plotVegMaps:: trying to spatially plot a VegObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
    }
  }
  else{
    if(!is.null(run)) run.id <- run@id
    else run.id <- NULL
  }
  
  ### CHECK FOR SPECIAL VARIABLES FOR NICER PLOTTING
  if(special == "none" | is.null(special)){
    
  
    if(quant@id == "burntfraction") special <- "burnt.fraction"

    
  }
  
  
  ### DIRECTORY TO SAVE PLOTS
  if(is.null(plot.dir)){
    if(!is.null(run)){ plot.dir <- run@run.dir} 
    else { plot.dir = "." }
  }
  
  ### QUANTITY AND COLS
  if(class(quant) == "character"){ quant = lpj.quantities[[quant]] }
  else if((class(quant) != "VegQuant")){
    warning("Invalid quantity found in plotLPJMaps using generic quantity")
    quant = lpj.quantities[["generic"]]
  }
  
  
  ### COLORKEY - standard, updated by specials below
  colorkey.list <- list(space = "right", 
                        col = quant@colours, 
                        labels = list(cex = 3 * text.multiplier)
  )
  
  
  #####################################################################################
  ############# PREPARE DATA AND layer LIST FOR PLOTTING        ######################
  #####################################################################################
  
  ### TOLERANCE - for when making grid to rasterise
  if(is.null(run)) { tolerance <- 0.001 }
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
    quant@name = paste("Difference: ", quant@name, sep = "")
    plot.bg.col <- "grey"
    
  }
  
  ### PLOT PERCENTAGE DIFFERENCE MAPS
  else if(special == "percentage.difference"){
    
    quant@cuts <- seq(from = -100, to = 200, by = 10)
    quant@colours <- colorRampPalette(c("blue","white","red", "yellow"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    quant@id <-  paste(quant@id, "percdiff", sep =".")
    quant@name = paste("Percentage Difference: ", quant@name, sep = "")
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
    quant@name <- paste(quant@name, "Fraction", sep = " ")
    quant@colours <- colorRampPalette(c("grey85", "black"))
    # also update colorkey
    colorkey.list[["col"]] <- quant@colours
    
  }
  
  #### PLOT BURNT FRACTION
  else if(special == "burnt.fraction"){
    
    # SET THE INTERVALS (using either these sensible options or the overrides)
    quant@cuts <- c(0, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
    quant@colours <-  colorRampPalette(rev(c("red4", "red","orange","yellow", "palegreen2", "cyan", "dodgerblue2", "blue", "midnightblue")))

    # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
    if(!is.null(override.cols)) {quant@colours <- override.cols}
    if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
    override.cols = override.cuts = NULL
    
    # RECLASSIFY THE DATA ACCORDING TO THE CUTS 
    temp.names <- names(data.toplot)
    data.toplot <- cut(data.toplot, quant@cuts, include.lower = TRUE, right = FALSE) 
    names(data.toplot) <- temp.names
    
    # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
    colorkey.labels <- paste(quant@cuts)
    colorkey.labels[length(colorkey.labels)] <- paste0(colorkey.labels[length(colorkey.labels)], "+")
    colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = 0:(length(quant@cuts)-1))
    colorkey.list[["at"]] <- 0:(length(quant@cuts)-1)
    colorkey.list[["col"]] <- quant@colours
    quant@cuts = 0:(length(quant@cuts)-1)

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
                 name = biome.scheme@name,
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
      if(class(biome.data)[[1]] == "DataObject") {
        data.name <- biome.data@name
        data.id <- biome.data@id
        biome.data <- promoteToRaster(biome.data@data)
      }
      # first check if they are on identical grids, in nor resample with nearest neighbour algorithm
      if(!compareRaster(biome.data, data.toplot, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=FALSE)){
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
      if(is.null(kappa.position)) { 
        this.extent <- extent(data.toplot)
        stats.pos.x <- (this.extent@xmax-this.extent@xmin) * 0.15 + this.extent@xmin
        stats.pos.y <- (this.extent@ymax-this.extent@ymin) * 0.15 + this.extent@ymin
        kappa.position <- c(stats.pos.x, stats.pos.y) }
      for(layer in 1:(nlayers(data.toplot)-1)) {
        layout.objs[[paste(layer)]]  <- list("sp.text", loc = kappa.position, txt = paste0("Kappa = ", round(kappa.list[[layer]]@Kappa,3)), which = layer, cex = 1.5)
      }
      
    }
    
    
  }
  
  #### PLOT DOMINANT
  
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
                 name = "Dominant PFTs",
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
  multi.panel <- TRUE
  if(nlayers(data.toplot) == 1){
    multi.panel <- FALSE
  }
  
  
  # PLOT MAIN TITLE
  if(is.null(title)) title <- makePlotTitle(paste(quant@name, "Summary", sep = " "), run, period)
  
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
 
  return(spplot(data.toplot,
                layers,
                par.settings = list(panel.background=list(col=plot.bg.col)),
                xlab = list(label = "Longitude", cex = 3 * text.multiplier),
                ylab = list(label = "Latitude", cex = 3 * text.multiplier),
                col.regions= quant@colours,
                colorkey = colorkey.list,                                                                                            
                at = quant@cuts,
                scales = list(draw = TRUE, cex = 3 * text.multiplier),
                as.table = TRUE,
                main=list(label=title, 
                          cex = 4 * text.multiplier),
                par.strip.text = list(#lines = 1.0, 
                  cex = 2 * text.multiplier),
                sp.layout = layout.objs,
                maxpixels = maxpixels,
                names.attr = plot.labels.here,
                ...)
  )
  
  # clean up
  rm(data.toplot)
  gc()
  
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
  hist(stat.results@diff.raster,  breaks = breaks, xlim = plot.range, xlab = paste(quant@id, ": ", "LPJ-GUESS - ", data.name, sep = ""), prob = TRUE, main = paste(quant@name, ": ", "LPJ-GUESS - ", data.name, sep = ""), cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, maxpixels =100000000, right = FALSE)
  x = NULL
  curve(dnorm(x, mean=stat.results@mean.diff, sd=stat.results@sd.diff), add=TRUE)
  abline(v=0,col="green", lwd = 4)
  legend('topright', c( paste("Mean = ", round(stat.results@mean.diff,3)), paste("SD = ", round(stat.results@sd.diff,3))), col = c("red","blue"), text.col = c("red","blue"), cex = 3, bty = "n") 
  
  dev.off()
  
  CairoPNG(file.path(run@run.dir, paste(quant@id, run@id, "OverlayHisto.Vs", data.name, "png", sep=".")), width = 1000, height = 700, title = paste(data.name, "Comparisons", quant@id, sep = " "), bg = "transparent")
  
  cex.axis.multi = 2
  par(mar = c(cex.axis.multi*2.5, cex.axis.multi*2.5, cex.axis.multi*2.5, 2) + 0.1)
  y.height <- 2*max(hist(stat.results@diff.raster, breaks = breaks, plot = FALSE)$counts, hist(data, breaks = breaks, plot = FALSE)$counts,  hist(model, breaks = breaks, plot = FALSE)$counts)
  diff.histo <- hist(stat.results@diff.raster,  breaks = breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@name, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis = cex.axis.multi, cex.main = 3, maxpixels =100000000, right = FALSE)
  hist(data,  breaks = breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@name, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, border = "red", maxpixels =100000000, right = FALSE)
  hist(model,  breaks = breaks,   xlim = plot.range, ylim = c(0, y.height), xlab = quant@id, ylab = "#gridcells", main=paste(quant@name, run@description, sep = " "), prob = FALSE, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 3, border = "blue", maxpixels =100000000, right = FALSE)
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
  
  plot(model, data, col = rgb(0.1,0.1,0.1,0.1), pch = 20, xlab = paste(run@description, " ", quant@name, " (", quant@units, ")", sep = ""), ylab = paste(data.name, " ", quant@name, " (", quant@units, ")", sep =""), ylim = c(quant@cuts[1],quant@cuts[length(quant@cuts)]), xlim = c(quant@cuts[1],quant@cuts[length(quant@cuts)]), main = paste("Scatter vs. ", data.name, sep = ""), maxpixels = 100000000, cex.lab =cex.axis.multi, cex.axis =cex.axis.multi, cex.main = 4)
  abline(0, 1, col = "red")
  legend('topleft', c(paste("RMSE:", round(stat.results@RMSE, 2), sep = " "), paste("R^2:", round(stat.results@R.squ, 2), sep = " "), paste("Pearsons:", round(stat.results@P.cor, 2), sep = " ")), text.col = c("red", "blue", "green"), cex = 3, bty = "n")
  dev.off()
  
}



# plotScatter <- function(x.obj, y.obj = NULL, x.layer, y.layer, col, x.quant, y.quant = NULL, comparison.object = NULL){
# 
#   # get the classes
#   class.x <- class(x.obj)
#   class.y <- class(y.obj)
# 
#   # if y.obj 
#   if(!is.null)
# 
# 
# 
# 
# 
# 
# 
# }


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
