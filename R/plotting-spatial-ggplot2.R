#!/usr/bin/Rscript


##########################################################################################################################################
################################################## PLOT VEG MAPS #########################################################################
##########################################################################################################################################


#' Plots a map from a temporally-averaged \code{ModelObject}, a data.table, a Raster* or a SpatialPixelsDataFrame.
#' 
#' This is a heavy lifting function for plotting models variables with flexibly, but with a high degree of automation. It's main use is to plot a map from a \code{ModelObject} or a \code{DataObject}, although plotting a Raster* is also useful.
#' It has a really large amount of parameters for a huge amount of flexibility.  However they are all set to sensible defaults,
#' so that in the case of plotting a \code{ModelObject} all you *need* to supply is the data itself, everything else is either set to a sensible default,
#' or provided by the \code{ModelObject} itself.  It is basically a very complex wrapper for spplot, and can automatically plot things like biomes, dominant PFTs, months of maximum values, 
#' burnt fraction on an approximately logarithic scale etc.  It returns a plot, which will need to be displayed using a \code{print()} command. 
#'
#' @param x The data to plot. Can be a ModelObject, data.table, a SpatialPixelsDataFrame or a Raster* object.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param title A character string to override the default title.
#' @param layout.objs List of overlays (for example coastlines or rivers or statistical values) or other objects to be plotted by \code{spplot} 
#' so see there for how to build them.
#' @param plot.labels List of character strings to be used as panel labels for summary plots and titles for the individual plots.  
#' Sensible titles will be constructed if this is not specified.
#' @param plot.bg.col Colour string for the plot background.
#' @param useLongnames Boolean, if TRUE replace PFT IDs with the PFT's full names on the plots. 
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa
#' @param limit Boolean, whether or not to limit the plotted values between a range, either the limits argument below,
#' or the range of the plot.
#' @param limits A numeric vector with two members (lower and upper limit) to limit the plotted values
#' @param override.cols A colour palette function to override the defaults
#' @param override.cuts Cut ranges (a numeric vector) to override the defaults.
#' @param special A character string to indicate certain "special modes" which modifies the behaviour of the function a bit.
#' Special modes are currectly "fraction", "difference", "percentage.difference", "firert" (fire return time) and "dominant.pft".
#' @param map.overlay A character string specifying which map overlay (from the maps and mapdata packages) should be overlain.  
#' Other things can be overlain on the resulting plot with further ggplot2 commands.
#' 
#' @details  This function is heavily used by the benchmarking functions and can be very useful to the user for making quick plots
#' in standard benchmarking and post-processing.  It is also highly customisable for final results plots for papers and so on.
#' However, the \code{plotGGSpatial} function makes pretty plots with a simpler syntax, but with less flexibility.
#' 
#' The function works best for \code{ModelObjects} (which contain a lot of useful metadata).   
#' 
#' @return Returns a plot object (from spplot)
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' @importFrom maptools map2SpatialLines
#' @importFrom rgeos gLength
#' @importFrom sp SpatialLinesDataFrame
#' @importFrom raster crs
#' 
#' @export 
#' @seealso \code{plotGGSpatial}, \code{expandLayers}, \code{sp::spplot}, \code{latice::levelplot}

plotSpatial2 <- function(data, # can be a data.table, a SpatialPixelsDataFrame, or a raster, or a ModelObject
                         layers = NULL,
                         title = NULL,
                         layout.objs = NULL, 
                         facet.labels =  NULL,
                         facet.order = NULL,
                         plot.bg.col =  "white",
                         useLongnames = FALSE,
                         text.multiplier = 1,
                         xlim = NULL,
                         ylim = NULL,
                         limit = FALSE,
                         limits = NULL,
                         override.cols = NULL,
                         override.cuts = NULL,
                         special = "none",
                         map.overlay = NULL,
                         dont.grid = FALSE,
                         return.data = FALSE,
                         ...){
  # 
  # ###################################################################################################
  # ### PRE-AMBLE:                                                             ########################
  # ### 1. INITIAL ARGUMENT CHECKS AND WARNINGS ABOUT ARGUMENT  COMBINATIONS   ########################
  # ### 2. WHEN THINGS ARE NOT SPECIFIED, PULL SOME DEFAULTS                   ########################
  # ###################################################################################################
  # 
  # ##### 1. ARGUMENTS, WARNINGS
  # 
  # ### HANDLE SPECIAL
  # 
  # if(!is.null(special)) special <- tolower(special)
  # 
  # special <- match.arg(special, 
  #                      c("none",
  #                        "difference", 
  #                        "percentage.difference",
  #                        "perc.diff",
  #                        "fraction",
  #                        "burnt.area",
  #                        "burnt.fraction",
  #                        "firert",
  #                        "fire.return.time",
  #                        "dominant.pft"))
  # 
  # special <- switch(special,
  #                   none = "none", 
  #                   difference = "difference", 
  #                   percentage.difference  = "percentage.difference",
  #                   perc.diff= "percentage.difference",
  #                   fraction = "fraction",
  #                   burnt.area = "burnt.area",
  #                   burnt.fraction = "burnt.fraction",
  #                   firert = "fire.return.time",
  #                   fire.return.time = "fire.return.time",
  #                   dominant.pft = "dominant" )
  # 
  # # Warn ig quant is being ignored in special
  # if(!is.null(quant) & special == "dominant")
  #   warning(paste("When using a \"special\" = ", special, ", argument \"quant\" is ignored"))
  # 
  # 
  # 
  # 
  # ##### 2. DEFAULTS
  # 
  # ### IF IS VEGOBJECT MANY THINGS ARE AVAILABLE FROM IT
  # if(is.ModelObject(data)){
  #   
  #   if(data@is.temporally.averaged){
  #     run <- data@run
  #     run.id <- run@id
  #     period <- data@temporal.extent
  #     PFT.set <- run@pft.set
  #     if(is.null(quant)) quant <- data@quant
  #   } 
  #   else {
  #     stop("plotSpatial:: trying to spatially plot a ModelObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
  #   }
  #   
  # }
  # else if(is.DataObject(data)){
  #   
  #   #if(data@is.temporally.averaged){
  #         if(is.null(quant)) quant <- data@quant
  #   #} 
  #   #else {
  #   #  stop("plotSpatial:: trying to spatially plot a ModelObject which has not been temporally averaged.  This is crazy, what do I do with all the years?!")
  #   #}
  #   
  #   if(!is.null(run)) run.id <- run@id
  #   else run.id <- NULL
  #   
  # }
  # else{
  #   
  #   print(paste("ATTENTION:  plotSpatial() NOT running on a ModelObject or DataObject, plotting an object of class ", class(data)[1]))
  #   warning(paste("ATTENTION:  plotSpatial() NOT running on a ModelObject or DataObject, plotting an object of class ", class(data)[1]))
  #   
  #   if(is.null(quant)) {
  #     
  #     quant <- new("Quantity",
  #                  id = "generic",
  #                  type = "unknown",
  #                  name = "Generic",
  #                  units = "unknown",
  #                  colours = fields::tim.colors, 
  #                  cuts = seq(0,1,0.05),
  #                  model = "none")
  #   } 
  #   
  #   if(!is.null(run)) run.id <- run@id
  #   else run.id <- NULL
  # }
  # 
  # ### CHECK FOR SPECIAL VARIABLES FOR NICER PLOTTING
  # if(special == "none" | is.null(special)){
  # 
  #   if(quant@id == "burntfraction") special <- "burnt.fraction"
  # 
  # }
  # 
  # 
  # ### COLORKEY - standard, updated by specials below
  # if(!is.null(quant)) colours <- quant@colours
  # else colours <- fields::tim.colors
  # colorkey.list <- list(space = "right", 
  #                       col = colours, 
  #                       labels = list(cex = 3 * text.multiplier)
  # )
  # if(!is.null(quant)) {
  #   if(tolower(quant@type) == "categorical") {
  #     colorkey.list[["labels"]] <- list(cex = 2 * text.multiplier,
  #                                       labels = rev(quant@units),
  #                                       at = (0:(length(quant@units)-1)) + 0.5)
  #  
  #    colorkey.list[["col"]] <- colorRampPalette(rev(colorkey.list$col(length(quant@units))))
  #   }
  # }
  # 
  # 
  # #####################################################################################
  # ############# PREPARE DATA AND layer LIST FOR PLOTTING        ######################
  # #####################################################################################
  # 
  # ### TOLERANCE - for when making grid to rasterise
  # if(is.null(run)) { tolerance <- 0.01 }
  # else {tolerance <- run@tolerance}  
  # 
  # ### SPECIAL CASE OF DOMINANT WITH NULL layer
  # if(is.null(layers) & !is.null(special)){
  #   if(special == "dominant") layers = "Dominant"
  # }
  # 
  # ### EXPAND layerS
  # if(is.null(layers)) {
  #   if(is.ModelObject(data)) layers <- names(data@data)
  #   else layers <- names(data)
  # }
  # if(expand.layers) {
  #   layers <- expandLayers(layers, data, PFT.set)
  #   if(!is.null(special)){
  #     if(special == "fraction") layers <- paste(layers, "Fraction", sep = sep.char)
  #   }
  # }
  # 
  # ### PROMOTE TO RASTER AND SANITISE NAMES - also make plot labels (if necessary) before the sanitatisation 
  # original.layers <- layers # save for building plot label later
  # data.toplot <- promoteToRaster(data, layers, tolerance)
  # layers <- names(data.toplot) # update layers to be the names of the raster layers  (which might have changed)
  # 
  # ### CROP THE DATA IF PLOT EXTENT HAS BEEN SPECIFIED
  # if(!is.null(plot.extent)){ data.toplot <- crop(data.toplot, plot.extent)}
  # 
  # 
  # #####################################################################################
  # ############# DEAL WITH SPECIAL CASES    ############################################
  # #####################################################################################
  # 
  # ### IF SPECIALS 
  # 
  # 
  # ### PLOT DIFFERENCE MAPS
  # if(special == "difference"){
  #   
  #   minmax <- max(quant@cuts) - min(quant@cuts)
  #   step <- (max(quant@cuts) - min(quant@cuts)) / (length(quant@cuts) - 1)
  #   quant@cuts <- seq(from = -minmax, to = minmax, by = step)
  #   quant@colours <- colorRampPalette(c("green","blue","white","red", "yellow"))
  #   # also update colorkey
  #   colorkey.list[["col"]] <- quant@colours
  #   quant@id <-  paste(quant@id, "diff", sep =".")
  #   quant@name = paste("Difference: ", quant@name, sep = "")
  #   plot.bg.col <- "grey"
  #   
  # }
  # 
  # ### PLOT PERCENTAGE DIFFERENCE MAPS
  # else if(special == "percentage.difference"){
  #   
  #   quant@cuts <- seq(from = -100, to = 200, by = 10)
  #   quant@colours <- colorRampPalette(c("blue","white","red", "yellow"))
  #   # also update colorkey
  #   colorkey.list[["col"]] <- quant@colours
  #   quant@id <-  paste(quant@id, "percdiff", sep =".")
  #   quant@name = paste("Percentage Difference: ", quant@name, sep = "")
  #   plot.bg.col <- "grey"
  #   
  #   # SET THE INTERVALS (using either these sensible options or the overrides)
  #   # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
  #   if(!is.null(override.cols)) {quant@colours <- override.cols}
  #   if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
  #   override.cols = override.cuts = NULL
  #   
  #   
  #   # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
  #   # get lowest and highest '50's
  #   smallest.limit = min(abs(quant@cuts[1]), abs(quant@cuts[length(quant@cuts)]))
  #   interval <- 50
  #   if(smallest.limit < 100) interval <- 25
  #   if(smallest.limit < 50) interval <- 10
  #   if(smallest.limit < 20) interval <- 5
  #   if(smallest.limit < 10) interval <- 1
  #   lower <- ceiling(quant@cuts[1]/interval) * interval
  #   upper <- floor(quant@cuts[length(quant@cuts)]/interval) * interval
  #   colourkey.at <- seq(lower, upper, by = interval)
  #   colorkey.labels <- paste(seq(lower, upper, by = interval))
  #   for(label.index in 1:length(colorkey.labels)){
  #     if(!(substr(colorkey.labels[label.index], 1, 1) == "-" | substr(colorkey.labels[label.index], 1, 1) == "0")){
  #       colorkey.labels[label.index] <- paste0("+", colorkey.labels[label.index])
  #     }
  #   }
  #   if(limit) colorkey.labels[length(colorkey.labels)] <- paste0("\u2265", colorkey.labels[length(colorkey.labels)])
  #   colorkey.labels <- paste(colorkey.labels, "%", sep = "")
  #   colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = colourkey.at)
  #   colorkey.list[["at"]] <- quant@cuts
  #   
  #   
  #   
  # }
  # 
  # #### PLOT FRACTION MAPS
  # else if(special == "fraction"){
  #   
  #   quant@cuts <- seq(from = 0, to = 1, by = 0.05)
  #   quant@id <- paste(quant@id, "fraction", sep = ".")
  #   quant@name <- paste(quant@name, "Fraction", sep = " ")
  #   quant@colours <- colorRampPalette(c("grey85", "black"))
  #   # also update colorkey
  #   colorkey.list[["col"]] <- quant@colours
  #   
  # }
  # 
  # #### PLOT BURNT FRACTION
  # else if(special == "burnt.fraction"){
  #   
  #   # SET THE INTERVALS (using either these sensible options or the overrides)
  #   quant@cuts <- c(0, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 1)
  #   quant@colours <-  colorRampPalette(rev(c("red4", "red","orange","yellow", "palegreen2", "cyan", "dodgerblue2", "blue", "midnightblue")))
  #   
  #   # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
  #   if(!is.null(override.cols)) {quant@colours <- override.cols}
  #   if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
  #   override.cols = override.cuts = NULL
  #   
  #   # RECLASSIFY THE DATA ACCORDING TO THE CUTS 
  #   temp.names <- names(data.toplot)
  #   data.toplot <- cut(data.toplot, quant@cuts, include.lower = TRUE, right = FALSE) 
  #   names(data.toplot) <- temp.names
  #   
  #   # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
  #   colorkey.labels <- paste(quant@cuts)
  #   colorkey.labels[length(colorkey.labels)] <- paste0(colorkey.labels[length(colorkey.labels)], "+")
  #   colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = 0:(length(quant@cuts)-1))
  #   colorkey.list[["at"]] <- 0:(length(quant@cuts)-1)
  #   colorkey.list[["col"]] <- quant@colours
  #   quant@cuts = 0:(length(quant@cuts)-1)
  #   
  # }
  # 
  # #### PLOT BURNT FRACTION
  # else if(special == "burnt.area"){
  #   
  #   
  #   stop("plotSpatial: special burnt.fraction or ba not impletemted yet")
  #   
  # }
  # 
  # #### PLOT FIRE RETURN TIME
  # else if(special == "fire.return.time" | quant@id == "firert"){
  #   
  #   # SET THE INTERVALS (using either these sensible options or the overrides)
  #   quant@cuts <- c(0, 1, 3, 5, 10, 25, 50, 100, 200, 400, 800, 1000)
  #   quant@colours <-  colorRampPalette(c("black", "red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3"))
  #   # if override cuts and cols specified use them, but note we have to then kill them otherwise they will override the new cuts below
  #   if(!is.null(override.cols)) {quant@colours <- override.cols}
  #   if(!is.null(override.cuts)) {quant@cuts <- override.cuts}  
  #   override.cols = override.cuts = NULL
  #   
  #   # RECLASSIFY THE DATA ACCORDING TO THE CUTS 
  #   temp.names <- names(data.toplot)
  #   data.toplot <- cut(data.toplot, quant@cuts) 
  #   names(data.toplot) <- temp.names
  #   
  #   # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
  #   colorkey.labels <- paste(quant@cuts)
  #   colorkey.labels[length(colorkey.labels)] <- paste0(colorkey.labels[length(colorkey.labels)], "+")
  #   colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, "labels" = colorkey.labels, "at" = 0:(length(quant@cuts)-1))
  #   colorkey.list[["at"]] <- 0:(length(quant@cuts)-1)
  #   quant@cuts = 0:(length(quant@cuts)-1)
  #   
  # }
  # 
  # 
  # #### PLOT DOMINANT
  # 
  # else if(special == "dominant"){
  #   
  #   
  #   quant.id <- quant@id
  #   
  #   # Get Raster Attribute Table and then convert raster back to simple integers instead of factors 
  #   # because currently (May 2016) raster package doesn't handle factor rasters very well
  #   RAT <- data.toplot@data@attributes[[1]]
  #   data.toplot <- deratify(data.toplot, complete = TRUE)
  #   
  #   # Set colours to be the list of PFT colours
  #   col.list <- c()
  #   label.list <- c()
  #   
  #   if(tolower(quant@type) == "pft") {
  #     
  #     # for each row of the RAT
  #     for(row.index in 1:NROW(RAT)){
  #       
  #       # get the row
  #       row <- RAT[row.index,]
  #       
  #       # special case if barren
  #       if(as.character(row$levels == "Barren")) {
  #         col.list <- append(col.list, "gray75")
  #         label.list <- append(label.list, "Barren")
  #       }
  #       
  #       # else (assuming others rows are PFTs)
  #       else{
  #         
  #         # get the PFT data, append the colour and the id/long name (as appropriate)
  #         PFT <- byIDfromList(as.character(row$levels), PFT.set)
  #         col.list <- append(col.list, PFT@colour)
  #         if(useLongnames) label.list <- append(label.list, PFT@name)
  #         else  label.list <- append(label.list, PFT@id)
  #         
  #       }
  #       
  #     } # for each row in RAT
  #     
  #     quant <- new("Quantity",
  #                  id = "Dominant",
  #                  name = "Dominant PFT",
  #                  type = "DominantPFTs",
  #                  units = "categorical",
  #                  colours = colorRampPalette(col.list),
  #                  cuts = 0:length(col.list),
  #                  aggregate.method = "categorical"
  #     )
  #     
  #   } # if per PFT
  #   
  #   else if(tolower(quant@type) == "monthly") {
  #     
  #     # for each row of the RAT
  #     for(row.index in 1:NROW(RAT)){
  #       
  #       # get the row
  #       row <- RAT[row.index,]
  #       
  #       # special case if none
  #       if(as.character(row$levels == "None")) {
  #         col.list <- append(col.list, "gray75")
  #         label.list <- append(label.list, "None")
  #       }
  #       
  #       # else (assuming others rows are months)
  #       else{
  #         
  #         # get the PFT data, append the colour and the id/long name (as appropriate)
  #         month <- byIDfromList(as.character(row$levels), months)
  #         col.list <- append(col.list, month@col)
  #         if(useLongnames) label.list <- append(label.list, month@name)
  #         else  label.list <- append(label.list, month@id)
  #         
  #       }
  #       
  #       quant <- new("Quantity",
  #                    id = "Dominant",
  #                    name = paste("Dominant Month by", quant.id, sep = " "),
  #                    type = "DominantMonth",
  #                    units = "categorical",
  #                    colours = colorRampPalette(col.list),
  #                    cuts = 0:length(col.list),
  #                    aggregate.method = "categorical"
  #       )
  #       
  #       
  #       
  #     } # for each row in RAT
  #     
  #   } # if monthly
  #   
  #   
  #   
  #   # UPDATE LABELS AND CUTS FOR SENSIBLE PLOTTING
  #   colorkey.list[["labels"]] <- list("cex" = colorkey.list[["labels"]]$cex, 
  #                                     "labels" = rev(label.list), 
  #                                     "at" = ((0:length(col.list))-1) + 0.5)
  #   colorkey.list[["at"]] <- 0:length(col.list)
  #   colorkey.list[["col"]] <- rev(col.list)
  #   
  # }
  # 
  # #### CATCH UNIMPLEMENTED SPECIAL CASES
  # else if(!(special != "" | special != "none")){
  #   stop(paste("Special case", tolower(special), "not implemented yet", sep = " "))
  # }
  # 
  # 
  # ### OVERRIDE (CUTS AND COLOUR SCHEME) 
  # if(!is.null(override.cols)) {quant@colours <- override.cols}
  # if(!is.null(override.cuts)) {quant@cuts <- override.cuts}
  # 
  # 
  # 
  # 
  # #####################################################################################
  # ############# LIMIT THE PLOTTED VARIABLE IF REQUESTED################################
  # ## Not really recommended as it hides scientific content, but can make plots nicer ##
  # #####################################################################################
  # 
  # if(limit){
  #   
  #   if(is.null(limits)){
  #     if(!is.null(quant)) {
  #       limits <- c(quant@cuts[0], quant@cuts[length(quant@cuts)]) 
  #     }
  #     else{
  #       warning("plotLPJMaps:: limit set to TRUE but no limits provided and no Quantity object provided either")
  #     }
  #     
  #   }
  #   
  #   quant@cuts <- seq(from = limits[1], to = limits[2], length.out = length(quant@cuts))
  #   
  #   for(layer in 1:nlayers(data.toplot)) {
  #     this.layer <- subset(data.toplot, layer)
  #     this.layer[this.layer < limits[1]] <- limits[1]
  #     this.layer[this.layer > limits[2]] <- limits[2]
  #     if(exists("data.limited")) data.limited <- addLayer(data.limited, this.layer)
  #     else data.limited <- brick(this.layer)
  #     rm(this.layer)
  #   }
  #   rm(data.toplot)
  #   data.toplot <- data.limited
  #   rm(data.limited)
  #   
  # }
  # 
  # 
  # 
  # #####################################################################################
  # ############# MAKE THE PLOTS ########################################################
  # #####################################################################################
  # 
  # # PLOT MAIN TITLE
  # if(is.null(title)) {
  #   if(length(layers) == 1)  title <- makePlotTitle(paste(quant@name, sep = " "), layer = layers, run = run, period = period)
  #   else  title <- makePlotTitle(paste(quant@name, sep = " "), run = run, period = period)
  # }
  # 
  # # PANEL LABELS - note expand longnames here if requested 
  # if(is.null(plot.labels)) {
  #   plot.labels.here <- original.layers
  #   ### USE LONGNAMES - for PFTs
  #   if(useLongnames) {
  #     for(plot.label.index in 1:length(plot.labels.here)){
  #       # look up PFT
  #       for(PFT in PFT.set){
  #         if(plot.labels.here[plot.label.index] == PFT@id) plot.labels.here[[plot.label.index]] <- unlist(PFT@name)
  #       }
  #     }
  #     plot.labels.here<- unlist(plot.labels.here)
  #   }
  # }
  # else plot.labels.here <- plot.labels
  # 
  # # return(spplot(data.toplot,
  # #               layers,
  # #               par.settings = list(panel.background=list(col=plot.bg.col)),
  # #               xlab = list(label = "Longitude", cex = 3 * text.multiplier),
  # #               ylab = list(label = "Latitude", cex = 3 * text.multiplier),
  # #               col.regions= quant@colours,as.data.frame(data.toplot)
  # #               colorkey = colorkey.list,                                                                                            
  # #               at = quant@cuts,
  # #               scales = list(draw = TRUE, cex = 3 * text.multiplier),
  # #               as.table = TRUE,
  # #               main=list(label=title, 
  # #                         cex = 4 * text.multiplier),
  # #               par.strip.text = list(#lines = 1.0, 
  # #                 cex = 2 * text.multiplier),
  # #               sp.layout = layout.objs,
  # #               maxpixels = maxpixels,
  # #               names.attr = plot.labels.here,
  # #               ...)
  # #  )
  # 
  # 
  
  # CHEC
  
  
  
  ### PREPARE DATA FOR PLOTTING
  discrete <- FALSE
  continuous <- FALSE
  single.object <- FALSE
  
  ### CASE 1 - A single ModelObject or DataObject
  if(is.ModelObject(data) || is.DataObject(data)) {
    
    single.object <- TRUE
    grid <- FALSE
    
    # if layers not specified, assume all
    if(is.null(layers)) layers <- names(data)
    
    # check if layers are all continuous or discrete
    for(layer in layers) {
      if(class(data@data[[layer]]) == "factor") discrete <- TRUE
      if(class(data@data[[layer]]) == "numeric") continuous <- TRUE
    }
    if(discrete & continuous) stop("plotSpatial cannot simultaneously plot discrete and continuous layers, check your layers")    
    
    # select layers and convert to a data,table
    data.toplot <- selectLayers(data, layers)
    data.toplot <- as.data.table(data.toplot)
    
    # now melt the layers
    data.toplot <- melt(data.toplot, measure.vars = layers)
    
    if(is.null(override.cols) & continuous) override.cols <- data@quant@colours(20)
    legend.title <- data@quant@units
    quant <- data@quant
    temporal.extent <- data@temporal.extent
    
  }
  
  ### CASE 2 - A single ComparisionLayer
  else if(is.ComparisonLayer(data)) {
    
    single.object <- TRUE
    grid <- FALSE
    original.layers <- layers # this is needed to keep track of the plotting mode since 'layers' is changed
    
    # first check if discrete or continuous
    for(layer in names(data)[1:2]) {
      if(class(data@data[[layer]]) == "factor") discrete <- TRUE
      if(class(data@data[[layer]]) == "numeric") continuous <- TRUE
    }
    if(discrete & continuous) stop("plotSpatial ComparisonObject does not seem to be consistenly defined")    
    
    
    # if layers not specified, assume "Difference"
    if(is.null(layers)  || tolower(layers) == "difference") {
      layers <- "Difference" 
      original.layers <- "Difference"
      if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
    }
    else if(tolower(layers) == "absolute") {
      # put side by side
      layers <- names(data)[1:2]
    }
    else if(tolower(layers) == "percentage.difference" || tolower(layers) == "percentagedifference"){
      
      layers <- "Percentage Difference"
      data.temp <- data@data[, "Percentage Difference" := (get(paste("Difference")) %/0% get(names(data)[2])) * 100]
      data@data <- data.temp
      if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
      
    }
    
    # select layers and convert to a data,table
    data.toplot <- selectLayers(data, layers)
    data.toplot <- as.data.table(data.toplot)
    
    
    # in the special case of absolute, change the layer names at this point from the ugly ids to the nice names
    if(tolower(original.layers) == "absolute") {
      new.layer.names <- c(data@info1@name, data@info2@name)
      setnames(data.toplot, layers, new.layer.names)
      layers <- new.layer.names
    }
    
    # now melt the layers
    data.toplot <- melt(data.toplot, measure.vars = layers)
    
    if(is.null(override.cols) & continuous) override.cols <- data@quant@colours(20)
    legend.title <- data@quant@units
    quant <- data@quant
    temporal.extent <- data@temporal.extent
    
    # special case for difference plots, make limits symmetric around 0
    if(is.null(limits) & (layers == "Difference" || layers == "Percentage Difference")){
      
      min.value <- min(data.toplot[["value"]], na.rm = TRUE)
      max.value <- max(data.toplot[["value"]], na.rm = TRUE)
      abs.max <- max(abs(min.value),abs(max.value))
      if(layers == "Percentage Difference") {
        abs.max <- min(abs.max, 300)
        legend.title <- "%"
      }
      limits <- c(-abs.max, abs.max)
      
    }
    
  }
  
  
  ### CASE 3- A list, hopefully made exclusively of ModelObjects/DataObjects xor ComparisonLayers
  else if(class(data)[1] == "list") {
    
    # PREAMBLE - first determine if the list contains consistent types and then fail if it does not
    only.data.or.model.objects <- TRUE
    only.comparison.layers <- TRUE
    for(object in data){ 
      if(!(is.ModelObject(object) || is.DataObject(object))) only.data.or.model.objects <- FALSE
      if(!is.ComparisonLayer(object)) only.comparison.layers <- FALSE
    }
    
    if(!xor(only.data.or.model.objects, only.comparison.layers)) {
      stop("You have passed me a list of items to plot but the items are exclusively of ModelObjects/DataObjects or ComparisonLayers (note you cannot mix those two types)")
    }
    
    # list of all data.tables to be rbinded at the end
    data.toplot.list <- list()
    
    ### CASE 3A - Plotting a bunch of ModelObjects/DataObjects
    if(only.data.or.model.objects) {
      
      # Loop through the objects and pull layers from each one into a large data.table for plotting
      
      temporal.extent <- NULL
      first <- TRUE
      for(object in data){
        
        # select the layers and mash the data into shape
        these.layers <- selectLayers(object, layers)
        these.layers.melted <- melt(these.layers@data, measure.vars = layers)
        if(is.DataObject(object)) these.layers.melted[, Source := object@name]
        else  these.layers.melted[, Source := object@run@name]
        data.toplot.list[[length(data.toplot.list)+1]] <- these.layers.melted
        
        # check if layers are all continuous or discrete
        for(layer in layers) {
          if(class(object@data[[layer]]) == "factor") discrete <- TRUE
          if(class(object@data[[layer]]) == "numeric") continuous <- TRUE
        }
        if(discrete & continuous) stop("plotSpatial canot simultaneously plot discrete and continuous layers, check your layers")   
        
        # check for meta-data to automagic the plots a little bit if possble
        if(first) {
          if(is.null(override.cols) & continuous) override.cols <- object@quant@colours(20)
          legend.title <- object@quant@units
          quant <- object@quant
          temporal.extent <- object@temporal.extent
        }
        else {
          # check for consistent temporal extent
          if(temporal.extent@start != object@temporal.extent@start || temporal.extent@end != object@temporal.extent@end) temporal.extent <- NULL
          # check for consistent Quantity
          if(!identical(quant, object@quant, ignore.environment = TRUE)) warning("Not all of the Data/ModeObjects supplied in the list have the same Quantity, I am using the Quantity from the first one")
        }
        
        first <- FALSE
        
      }
    }
    
    
    ### CASE 3B - Plotting a bunch of ComparisonLayers
    else if(only.comparison.layers) {
      
      # This follows Case 2 above
      original.layers <- layers # this is needed to keep track of the plotting mode since 'layers' is changed
      for(comp.layer in data){  
        
        # first check if discrete or continuous
        for(layer in names(comp.layer)[1:2]) {
          if(class(comp.layer@data[[layer]]) == "factor") discrete <- TRUE
          if(class(comp.layer@data[[layer]]) == "numeric") continuous <- TRUE
        }
        if(discrete & continuous) stop("plotSpatial ComparisonObject does not seem to be consistenly defined")    
        
        
        # if layers not specified, assume "Difference"
        if(is.null(original.layers)  || tolower(original.layers) == "difference") {
          layers <- "Difference" 
          original.layers <- layers
          if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
        }
        else if(tolower(original.layers) == "absolute") {
          # changes the names of the ComparisonLayer 
          
          layers <- names(comp.layer)[1:2]
        }
        else if(tolower(original.layers) == "percentage.difference" || tolower(original.layers) == "percentagedifference"){
          
          layers <- "Percentage Difference"
          comp.layer.temp <- comp.layer@data[, "Percentage Difference" := (get(paste("Difference")) %/0% get(names(comp.layer)[2])) * 100]
          comp.layer@data <- comp.layer.temp
          if(is.null(override.cols) & continuous) override.cols <- rev(brewer.pal(11, "RdBu"))
          
        }
        
        # select layers and convert to a data,table
        data.toplot <- selectLayers(comp.layer, layers)
        data.toplot <- as.data.table(data.toplot)
        
        # in the special case of absolute, change the layer names at this point from the ugly ids to the nice names
        if(tolower(original.layers) == "absolute") {
          new.layer.names <- c(comp.layer@info1@name, comp.layer@info2@name)
          setnames(data.toplot, layers, new.layer.names)
          layers <- new.layer.names
        }
        
        # now melt the layers and add the Source
        data.toplot <- melt(data.toplot, measure.vars = layers)
        if(!tolower(original.layers) == "absolute") { data.toplot[, Source := comp.layer@name] }
        else {
          setnames(data.toplot, "variable", "Source")
          data.toplot[, variable := "Absolute"]
        }
        
        # save 
        data.toplot.list[[length(data.toplot.list)+1]] <- data.toplot
        
        
        if(is.null(override.cols) & continuous) override.cols <- comp.layer@quant@colours(20)
        legend.title <- comp.layer@quant@units
        quant <- comp.layer@quant
        temporal.extent <- comp.layer@temporal.extent
        
      } # for each ComparisonLayer
      
      # special case for difference plots, make limits symmetric around 0
      if(is.null(limits) & (layers == "Difference" || layers == "Percentage Difference")){
        
        min.value <- min(data.toplot[["value"]], na.rm = TRUE)
        max.value <- max(data.toplot[["value"]], na.rm = TRUE)
        abs.max <- max(abs(min.value),abs(max.value))
        if(layers == "Percentage Difference") {
          abs.max <- min(abs.max, 300)
          legend.title <- "%"
        }
        limits <- c(-abs.max, abs.max)
        
      } # if special case
      
    } # for each ComparisonLayer in the list
    
    
    # finally mash them all togther to make the final data.table to plot
    data.toplot <- rbindlist(data.toplot.list)
    rm(data.toplot.list)
    
  } 
  
  else {
    
    stop("plotSpatial can only handle single a DataObject or ModelObject, or a list of Data/ModelObjects")
    
  }
  
  ### Rename "variable" to "Layer" which makes more conceptual sense
  setnames(data.toplot, "variable", "Layer")
  setnames(data.toplot, "value", "Value")
  
  
  ### VERBOSE
  #if(discrete) print("Printing in mode for discrete variables")
  #if(continuous) print("Printing in mode for continuous variables")
  #if(!continuous & !discrete) stop("Neither discrete nor continuous")
  
  
  
  ### APPLY CUSTOM CUTS TO DISCRETISE IF NECESSARY
  if(continuous & !is.null(override.cuts)) {
    data.toplot[,Value:= cut(Value, override.cuts, right = FALSE, include.lowest = TRUE)]
    discrete <- TRUE
    continuous <- FALSE
    breaks <- waiver()
  }
 
  
  ### RETURN DATA ONLY IF REQUESTED
  if(return.data) return(data.toplot)
  
  ### CALCULATE THE RANGE OF LONGITUDE AND LATITUDE TO BE PLOTTED
  all.lons <- sort(unique(data.toplot[["Lon"]]))
  all.lats <- sort(unique(data.toplot[["Lat"]]))
  
  if(is.null(ylim)) ylim <- c(min(all.lats), max(all.lats)) 
  if(is.null(xlim)) xlim <- c(min(all.lons), max(all.lons)) 
  
  
  
  ### PREPARE THE MAP OVERLAY
  if(class(map.overlay)[1] == "character"){
    
    # determine if london centered (if not call "maps2" for Pacific centered versions)
    gt.180 <- FALSE
    for(lon in all.lons) {
      if(lon > 180) gt.180 <- TRUE
    }
    if(tolower(map.overlay)=="world" && gt.180) map.overlay <- "world2"
    else if(tolower(map.overlay)=="worldHires" && gt.180) map.overlay <- "worldHires2"
    else if(tolower(map.overlay)=="world2" && !gt.180) map.overlay <- "world"
    else if(tolower(map.overlay)=="world2ires" && !gt.180) map.overlay <- "worldHires"
    
    # Convert map to SpatialLinesDataFrame, perform the 'Russian Correction' and then fortify() for ggplot2
    proj4str <- "+proj=longlat +datum=WGS84"
    map.sp.lines <- map2SpatialLines(map(map.overlay, plot = FALSE, interior = TRUE, xlim=xlim, ylim=ylim, fill=TRUE), proj4string = CRS(proj4str))
    df <- data.frame(len = sapply(1:length(map.sp.lines), function(i) rgeos::gLength(map.sp.lines[i, ])))
    rownames(df) <- sapply(1:length(map.sp.lines), function(i) map.sp.lines@lines[[i]]@ID)
    map.sp.lines.df <- SpatialLinesDataFrame(map.sp.lines, data = df)
    map.sp.lines.df <- correct.map.offset(map.sp.lines.df)
    map.overlay <- fortify(map.sp.lines.df)
    
    rm(df, map.sp.lines, map.sp.lines.df)   
    
  }
  else if(!is.null(map.overlay)) {
    stop("Some other overlay type...")
  }
  
  ### IF PLOT IS DISCRETE, BUILD THE COLOURS 
  if(discrete & is.null(override.cols)){
    print("YES, discrete")
    # make a list of all the unique values (factors), each of these will need a colour
    unique.vals <- unique(data.toplot[["Value"]])
    
    # Final results of stuff below
    cols <- c()
    is.PFTs <- FALSE
    is.categorical <- FALSE
    
    ###  If the Quantity if specifically defined as categorical then use the colours defined in the Quantity's units slot
    if(tolower(quant@type) == "categorical") {
      
      legend.title = NULL
      
      # reverse engineer colours from palette
      quant.cols <- quant@colours(length(quant@units))
      names(quant.cols) <- quant@units
      
      for(val in unique.vals) {
        if(!is.na(val)){
          for(factor.value in quant@units) {
            if(val == factor.value) cols[[val]] <- quant.cols[[val]]
          }  
        }
      }
      
      if(length(cols) == length(unique.vals)){
        is.categorical <- TRUE
      }
      
      
    }
    
    ### Else check if the factors are PFTs 
    # TODO - implement months below!!
    else {
      
      # check if the factors are PFTs, and if so assign them their meta-data colour
      pft.superset <- NULL
      if(is.ModelObject(data)) {
        pft.superset <- data@run@pft.set 
      }
      else {
        
        for(object in data) {
          if(is.ModelObject(object)) {
            pft.superset <- append(pft.superset, object@run@pft.set)
          }
          
        }
      }
      
      for(val in unique.vals) {
        for(PFT in pft.superset) {
          if(val == PFT@id) cols[[val]] <- PFT@colour
        }    
      }
      
      if(length(cols) == length(unique.vals)) is.PFTs <- TRUE
      
      # if not PFTs, look for months
      if(!is.PFTs){
        
        print("Here look for months")
        
      }
      
    }
    
    
    # If found colours for all the factors, set the values for plotting
    if(is.PFTs) {
      if(is.null(override.cols)) override.cols <- cols
      legend.title <- "PFT"
      breaks <- sort(names(override.cols))
    }
    else if(is.categorical) {
      if(is.null(override.cols)) override.cols <- cols
      legend.title <- quant@name
      breaks <- quant@units
    }
    
  }
  
  ### HANDLE THE FACET GRID/WRAP ISSUE
  multiple.sources <- FALSE
  multiple.layers <- FALSE
  
  # Do we have multiple sources?
  if("Source" %in% names(data.toplot)) {
    multiple.sources <- TRUE
  }
  
  # Do we have multiple layers?
  if(length(unique(data.toplot[["Layer"]])) > 1) {
    multiple.layers <- TRUE
  }
  
  #  CASE 1 - single source and single layer 
  #           ie. one map so don't wrap or grid
  if(!multiple.layers & !multiple.sources) {
    grid <- FALSE
    wrap <- FALSE
  }
  
  # CASE 2 - single source and multiple layers
  #          then wrap, gridding doesn't make sense
  else if(multiple.layers & !multiple.sources) {
    grid <- FALSE
    wrap <- TRUE
    facet.string <- "~Layer"
  }
  
  # CASE 3 - multiple sources and single layer (opposite of case 2)
  #          then wrap, gridding doesn't make sense
  else if(!multiple.layers & multiple.sources) {
    grid <- FALSE
    wrap <- TRUE
    facet.string <- "~Source"
  }
  
  # CASE 4 - multiple sources and multiple layers
  #          the grid (unless special instructions not to)
  else if(multiple.layers & multiple.sources) {
    if(!dont.grid) {
      grid <- TRUE
      wrap <- FALSE
      facet.string <- "Layer~Source"
    }
    else {
      stop("If you want some 'dont.grid' option action then code it up!")
    }
  }
  
  # if wrapping and facet order has been provided in facet.order, re-order the factor to re-order the facets
  if(wrap & !is.null(facet.order)) {
    if(multiple.layers & !multiple.sources) data.toplot[, Layer := factor(Layer, facet.order)]
    else  if(!multiple.layers & multiple.sources) data.toplot[, Source := factor(Source, levels = facet.order)]
  }
  
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(is.null(title)) {
    if(single.object) {
      if(length(unique(data.toplot[["Layer"]])) > 1) {
        title <- makePlotTitle(quant@name, layer = NULL, source = data, period = data@temporal.extent) 
      }
      else {
        title <- makePlotTitle(quant@name, layer = layers, source = data, period = data@temporal.extent) 
      }
    }
    else {
      if(length(unique(data.toplot[["Layer"]])) > 1) title <- makePlotTitle(quant@name, layer = NULL, source = NULL, period = temporal.extent) 
      else title <- makePlotTitle(quant@name, layer = layers, source = NULL, period = temporal.extent) 
    }
  }
  
  
  ### BUILD THE PLOT

  # basic plot building
  mp <- ggplot(data = as.data.frame(data.toplot))
  mp <- mp + geom_raster(aes_string(x = "Lon", y = "Lat", fill = "Value"))

  # facet with grid or wrap 
  if(grid) mp <- mp + facet_grid(as.formula(paste(facet.string)), switch = "y", labeller = as_labeller(facet.labels))
  else if(wrap) mp <- mp + facet_wrap(as.formula(facet.string), labeller = as_labeller(facet.labels))
  
  # colour bar
  if(continuous)  {
    mp <- mp + scale_fill_gradientn(name = legend.title, limits = limits, colors = override.cols)
    mp <- mp + guides(fill = guide_colorbar(barwidth = 2, barheight = 20))
  }
  if(discrete) {
    mp <- mp + scale_fill_manual(values = override.cols, breaks = breaks)
    mp <- mp + guides(fill = guide_legend(keyheight = 2))
  }
  
  # crop to xlim and ylim as appropriate and fix the aspect ratio 
  mp <- mp + xlim(xlim)
  mp <- mp + ylim(ylim)
  mp <- mp + coord_fixed()
  
  # labels and positioning
  mp <- mp + labs(title = title, y = "Latitude", x = "Longitude")
  if(!is.null(legend.title)) {mp <- mp + labs(fill=legend.title) }
  else { mp <- mp + theme(legend.title = element_blank()) }
  mp <- mp + theme(plot.title = element_text(hjust = 0.5))
  mp <- mp + theme(text = element_text(size=30))
  
  # set background colour of panel
  mp <- mp + theme(#panel.background = element_rect(fill = plot.bg.col) # bg of the panel
    plot.background = element_rect(fill = plot.bg.col), # bg of the plot
    #, panel.grid.major = element_blank() # get rid of major grid
    #, panel.grid.minor = element_blank() # get rid of minor grid
    legend.background = element_rect(fill = "transparent")#, # get rid of legend bg
    #legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )
  
  
  # map overlay
  if(!is.null(map.overlay)) mp <- mp + geom_path(data=map.overlay, size=0.1, color = "black", aes(x=long, y=lat, group = group))
  
  return(mp)
  
  
}

######################### CORRECTS AN ARTEFACT FROM MAPS PACKAGE WHERE EASTERN ASIA IS WRONGLY PLACED #####################################################################
#' 
#' Fixes a spatial lines object where some of eastern Russia transposed to the other side of the world
#' 
#' 
#' @param spl SpatialLines object to fix
#' @return a the SpatialLines object 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import raster
correct.map.offset <- function(spl) {
  we <- crop(spl, extent(-180, 180, -90, 90))
  ww <- crop(spl, extent(179.999, 200, -90, 90))
  if(!is.null(ww) & !is.null(we)) {
    ww <- raster::shift(ww, -360)
    spl <- raster::bind(we, ww)  
  }
  return(spl)
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
#' @param layer The names of the layer (or another identifier)
#' @param run The \code{ModelRun} object for the run plotted (optional)
#' @param period The time period plotted as \code{TemporalExtent} (optional)
#' @return A character string for use as a plot title
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(quantity.str, layer = NULL, source = NULL, period = NULL){
  
  # Quantity.str must be supplied
  string <- quantity.str
  
  # A layer name may be supplied
  if(!is.null(layer)) string <- paste(string, layer, sep = " ")
  
  # A source may be supplied (either a DataObject, ModelObject or ComparisonLayer)
  if(!is.null(source)) {
    if(is.ModelObject(source)) string <- paste(string, source@run@name, sep = " ")
    if(is.DataObject(source)) string <- paste(string, source@name, sep = " ")
    if(is.ComparisonLayer(source)) string <- paste(string, source@name, sep = " ")
  }
  
  # Ands a period may be suppled  
  if(!is.null(period)) string <- paste(string, paste("(", period@start, "-", period@end, ")", sep = ""), sep = " ")
  return(string)
  
}
