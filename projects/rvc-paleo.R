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

##############################################
max.absent.fraction.4class <- 0.05
max.trace.fraction.4class <- 0.15
min.dominant.fraction.4class <- 0.5

dom.dom <- 2
agree <- 1
disagree <- 0
badly.disagree <- -1
very.badly.disagree <- -2
absent.absent <- 0

doClassification <- function(sp.df, PFTs){
  
  message("Classifying PFTs")
  
  for(PFT in PFTs){
    
    input.col.name <- PFT@name
    output.col.name <- paste(PFT@name, "Status", sep = ".")
    sp.df@data$tempcol <- factor(as.vector(unlist(lapply(sp.df@data[,which(names(sp.df@data)==input.col.name)],  FUN = get4ClassStatus))))
    colnames(sp.df@data)[length(names(sp.df@data))] <- output.col.name
    
  }
  
  print("Classified all PFTs in run")
  
  return(sp.df)
  
}

# Version with data.table.  More elegant, probably faster but difference is not important
doClassificationDT <- function(input.dt, PFT.masterlist){
  
  PFTs <- getPFTs(input.dt, PFT.masterlist)
  for(PFT in PFTs){
    input.dt[, eval(quote(paste(PFT@name, "Status", sep = ""))):= factor(apply(input.dt[,paste(PFT@name, "Fraction", sep = sep.char),with=FALSE],FUN=get4ClassStatus,MAR=1))]
  }
  return(input.dt)
  
}



getAgreementIndex <- function(model.status, fossil.status){
  
  # if model gets it right (exceptng both PFT being absent in both)
  if(model.status == fossil.status && model.status == "dominant") return(dom.dom)
  else if(model.status == fossil.status && model.status != "absent") return(agree)
  else if(model.status == fossil.status && model.status == "absent") return(absent.absent)
  # if model gets it very wrong
  else if (model.status == "dominant" && fossil.status == "absent") return(very.badly.disagree)
  else if (fossil.status == "dominant" && model.status == "absent") return(very.badly.disagree)
  # if the model gets is a bit wrong
  else if (model.status == "subdominant" && fossil.status == "absent") return(badly.disagree)
  else if (fossil.status == "subdominant" && model.status == "absent") return(badly.disagree)
  else if (model.status == "dominant" && fossil.status == "trace") return(badly.disagree)
  else if (fossil.status == "dominant" && model.status == "trace") return(badly.disagree)
  # all else is ok
  else return(disagree)
  
}


# getAgreementIndex <- function(model.status, fossil.status){
#   
#   # if model gets it right (exceptng both PFT being absent in both)
#   if(model.status == fossil.status && model.status == "dominant") return(dom.dom)
#   else if(model.status == fossil.status && model.status != "absent") return(agree)
#   # if model gets it very wrong
#   else if (model.status == "dominant" && fossil.status == "absent") return(very.badly.disagree)
#   else if (fossil.status == "dominant" && model.status == "absent") return(very.badly.disagree)
#   # if the model gets is a bit wrong
#   else if (model.status == "subdominant" && fossil.status == "absent") return(-1)
#   else if (fossil.status == "subdominant" && model.status == "absent") return(-1)
#   else if (model.status == "dominant" && fossil.status == "trace") return(-1)
#   else if (fossil.status == "dominant" && model.status == "trace") return(-1)
#   # all else is ok
#   else return(0)
#   
# }

getMaximumAgreementIndex <- function(fossil.status){
  
  if(fossil.status == "dominant") return(dom.dom)
  else if(fossil.status == "subdominant" || fossil.status == "trace") return(agree)
  else return(absent.absent)
  
}

getMinimumAgreementIndex <- function(fossil.status, first.absent){
  
  # This is more complex than a previous version since necessary to modify this since there can be only on dominant PFT in both the fossil data and the model output
  
  # if there is a dominant PFT in the fossil data it is possible for the model to get this total wrong contributing -2 to the lowest possible agreement
  if(fossil.status == "dominant") return(very.badly.disagree)
  # if the PFT is absent in the fossil data it is possible to contribute -2 *but only if the model predicts this as the domiannt PFT*
  # this can only happen once since the model can only predict (at most) one dominant PFT
  else if(fossil.status == "absent" && first.absent) return(very.badly.disagree)
  # afterwards each absent PFT counts contributes -1 because the model can only simulate the PFT as sub-dominant, which contributes only -1 necause it is a lesser disagreement
  else if(fossil.status == "absent" || fossil.status == "subdominant" || fossil.status == "trace") return(badly.disagree)
  else return(absent.absent)
  
}

get4ClassStatus <- function(fraction){
  
  if(fraction > min.dominant.fraction.4class) return("dominant")
  else if (fraction <= max.absent.fraction.4class) return("absent")
  else if (fraction <= max.trace.fraction.4class) return("trace")
  else if (fraction < min.dominant.fraction.4class) return("subdominant")
  
}

Get4ClassStatus <- function(fraction, dominant){
  
  if(dominant & fraction > min.dominant.fraction.4class) return("dominant")
  else if (fraction <= max.absent.fraction.4class) return("absent")
  else if (fraction <= max.trace.fraction.4class) return("trace")
  else if (fraction < min.dominant.fraction.4class) return("subdominant")
  
}


############################################################################################################################
### Function to add dominant PFT and lifeform and phenological summaries and fractions the the dataframe of the sp object

processFossils <- function(fossils, PFTs, combine = FALSE, do.dominantPFT = TRUE, do.lifeforms = TRUE, do.phenologies = TRUE, do.PFTfractions = TRUE, verbose = FALSE){
  
  #print(head(fossils))
  
  #fossils$TeBE <- fossils$TeSBE + fossils$TeBE
  #fossils$TeSBE <- 0
  
  #print(head(fossils))
  
  #print("after")
  #print(avg.lai.comparable[[this.run@id]]@data@data[fossil.data.frame$model.row[fossil.counter],])
  
  if(do.lifeforms){
    
    # Tree summary
    fossils$TreeTotal <- apply(fossils, 1, FUN = getLifeformTotal, PFTs, "Tree")
    fossils$TreeFraction <- apply(fossils,1, FUN = getSafeFraction, "TreeTotal", "Total")
    
    # Grass summary
    fossils$GrassTotal <- apply(fossils, 1, FUN = getLifeformTotal, PFTs, "Grass")
    fossils$GrassFraction <- apply(fossils,1, FUN = getSafeFraction, "GrassTotal", "Total")
    
    # Shrub summary
    fossils$ShrubTotal <- apply(fossils, 1, FUN = getLifeformTotal, PFTs, "Shrub")
    fossils$ShrubFraction <- apply(fossils,1, FUN = getSafeFraction, "ShrubTotal", "Total")
    
  }
  
  if(do.phenologies){
    
    # Evergreen summary
    fossils$EvergreenTotal <- apply(fossils, 1, FUN = getPhenTotal, PFTs, "Evergreen")
    fossils$EvergreenFraction <- apply(fossils,1, FUN = getSafeFraction, "EvergreenTotal", "Total")
    
    # Summergreen summary
    fossils$SummergreenTotal <- apply(fossils, 1, FUN = getPhenTotal, PFTs, "Summergreen")
    fossils$SummergreenFraction <- apply(fossils,1, FUN = getSafeFraction, "SummergreenTotal", "Total")
    
    # Raingreen summary
    fossils$RaingreenTotal <- apply(fossils, 1, FUN = getPhenTotal, PFTs, "Raingreen")
    fossils$RaingreenFraction <- apply(fossils,1, FUN = getSafeFraction, "RaingreenTotal", "Total")
    
  }
  
  if(do.dominantPFT){
    
    # Dominant PFT
    fossils$Dominant <- as.factor(apply(fossils, 1, FUN = getDominant, PFTs))
    fossils$DominantFraction <- apply(fossils, 1, FUN = getDominantSafeFraction)
    
  }
  
  
  # PFT Fractions of Total
  if(do.PFTfractions){
    for(current.PFT in PFTs){
      col.name <- paste(current.PFT@name, "Fraction", sep = ".")
      fossils$tempcol <- apply(fossils,1, FUN = getSafeFraction, current.PFT@name, "Total")
      colnames(fossils)[length(names(fossils))] <- col.name
    }
  }
  
  print("...done")
  
  return(fossils)
  
}


###########################################
### REDUNDANT WITH RASTER?????????????? ###
###########################################

#######################################################################################
###
### FUNCTION - matchPointToGridcell 
###
### Find which gridcell a point lies in from the lon/lat of the point and  a list of lons and lats
### returns both the lon/lat of the gridcell and the lon/lat indices
### Also should handle -ve lons

matchPointToGridcell <- function(point.lon, point.lat, list.lons, list.lats) {
  
  
  gridcell.lon <- NULL
  gridcell.lat <- NULL
  gridcell.lon.index <- NULL
  gridcell.lat.index <- NULL
  
  
  # get the latitude
  for(lat.counter in 1:(length(list.lats))){
    # deal with first and last values seperately otherwise array will go out of range
    if(lat.counter == 1) {
      lat.lower = -90 
      lat.upper = (list.lats[lat.counter] + list.lats[lat.counter+1])/2
    }
    else if(lat.counter == length(list.lats)) {
      lat.lower = (list.lats[lat.counter-1] + list.lats[lat.counter])/2  
      lat.upper = 90
    }
    else {
      lat.lower = (list.lats[lat.counter-1] + list.lats[lat.counter])/2
      lat.upper = (list.lats[lat.counter] + list.lats[lat.counter+1])/2
    }
    # if 
    if(point.lat > lat.lower && point.lat <= lat.upper) {
      gridcell.lat <- list.lats[lat.counter]
      gridcell.lat.index <- lat.counter
    }    
  }
  
  # check if there are negative longitudes and set upper and lower limits accordingly
  if(min(list.lons) < 0) {
    lon.lower.limit <- -180
    lon.upper.limit <- 180
  }
  else {
    lon.lower.limit <- 0
    lon.upper.limit <- 360
  }
  
  
  # get the longitude
  for(lon.counter in 1:(length(list.lons))){
    if(lon.counter == 1) {
      lon.lower = lon.lower.limit 
      lon.upper = (list.lons[lon.counter] + list.lons[lon.counter+1])/2
    }
    else if(lon.counter == length(list.lons)) {
      lon.lower = (list.lons[lon.counter-1] + list.lons[lon.counter])/2  
      lon.upper = lon.upper.limit
    }
    else {
      lon.lower = (list.lons[lon.counter-1] + list.lons[lon.counter])/2
      lon.upper = (list.lons[lon.counter] + list.lons[lon.counter+1])/2
    }
    if(point.lon > lon.lower && point.lon <= lon.upper) {
      #model.data.table$LPJ.lon.index[point.counter] <- lon.counter
      gridcell.lon <- list.lons[lon.counter]
      gridcell.lon.index <- lon.counter
    }
  }
  
  coords <- c(gridcell.lon, gridcell.lat)
  indices <- c(gridcell.lon.index, gridcell.lat.index)
  output <- list("coords" = coords, "indices" = indices)
  return(output)
  
  
}



##########################################################################################################################################
################################################## PLOT DOMINANT WITH FOSSIL POINTS ######################################################
##########################################################################################################################################


plotDominantPFTMapWithDataPoints <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                                             which.dominant = "Dominant",
                                             points,
                                             which.points,
                                             run = NULL, 
                                             period = NULL, 
                                             plot.dir = NULL, 
                                             filename = NULL, 
                                             layout.objs = NULL, 
                                             plot.title =  NULL,
                                             addData = FALSE){
  
  
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
  
  # if we have received a SpatialPixelsDataFrame we can plot it straight away, we just need a list of PFTs
  if(class(data) == "RasterLayer"){
    stop("Raster Not Supported")
  }
  
  else if(class(data) == "SpatialPixelsDataFrame"){
    data.toplot <- data
  }
  
  # if we have received a data.table promote it to Raster
  else if(class(data) == "data.table"){
    
    sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance)
    data.toplot <- SpatialPixelsDataFrame(sp.pixels, data.frame(data[, which.dominant, with=FALSE]), tolerance = tolerance)      
    names(data.toplot) <- which.dominant
    #clean up
    rm(sp.points, sp.pixels, sp.pixels.df)
  }  
  # else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to plot object of type", class(data), "which I don't know how to do.", sep = ""))
  }
  
  
  # COLORLIST
  dom.PFT.colourlist <- vector()
  # make list of PFTs which are dominant somewhere
  DomPFTs <- vector()
  if(length(which.dominant) == 1 ) {DomPFTs <- levels(data.toplot@data[which.dominant,])}
  else{
    stop("Construction Site:  plotDominantPFTMaps() cannot currently deal with more than one map")
    #for(var in which.dominant){
    #  DomPFTs <- append(DomPFTs, levels(data.toplot[,var,with=FALSE]))    
    #}
  }
  DomPFTS <- unique(DomPFTs)  
  for(PFT.name in DomPFTs){
    if(PFT.name == "Barren"){ dom.PFT.colourlist[["Barren"]] <- append(dom.PFT.colourlist, "gray75")}
    else{ dom.PFT.colourlist[[PFT.name]] <- global.PFTs[[PFT.name]]@colour}
  }
  
  # FILENAME
  if(!is.null(filename)){ current.filename <- filename}
  else {
    if(!is.null(period)){ current.filename = paste("DominantPFTWithData.", run@id, ".TA.", period@start, "-", period@end, ".png", sep = "")}
    else { current.filename = paste("DominantPFTWithData.", run@id, ".TA.png", sep = "") }
  }
  
  # POINTS
  layout.objs <- list() 
  # now map the list of points to overlay with sp.layout
  for(PFT.name in DomPFTs){
    if(PFT.name != "Barren"){
      current.PFT <- global.PFTs[[PFT.name]]
      points.layout.object <- list("sp.points",points[which(points$Dominant == current.PFT@name),], fill = current.PFT@colour, col = "black", pch = 21, cex = 1.3)
      layout.objs[[current.PFT@name]] <- points.layout.object
    }
  }
  
  # PLOT TITLE
  
  if(is.null(plot.title)){plot.title <- paste("Dominant PFT:", run@description, sep = " ")}
  
  CairoPNG(paste(plot.dir, current.filename, sep = "/"), width = 1800, height = 1000, title = paste("Domiant PFT", sep = " "))  
  print(spplot(data.toplot,
               which.dominant,
               xlab = list(label = "Longitude", cex = 2),
               ylab = list(label = "Latitude", cex = 2),
               col.regions = dom.PFT.colourlist,
               scales = list(draw = TRUE, cex = 2),
               main=list(label=plot.title, cex = 3),
               par.strip.text = list(lines = 1.0, cex = 1.5),
               sp.layout = layout.objs,
               names.attr = plot.labels,
               colorkey = list(col = dom.PFT.colourlist, 
                               space = "right",
                               labels = list(labels = names(dom.PFT.colourlist),
                                             at = 1:length(dom.PFT.colourlist),
                                             cex = 2))
  )
  )
  dev.off()
  
}

##########################################################################################################################################
################################################## PLOT GLOBAL BIOME MAP WITH AGREEMENT INDEX ################################################################
##########################################################################################################################################


plotGlobalBiomeMapWithAI <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                                     run = NULL, 
                                     fossils,
                                     period = NULL, 
                                     plot.dir = NULL, 
                                     filename = NULL, 
                                     layout.objs = NULL, 
                                     plot.title =  NULL,
                                     run.title = NULL,
                                     addData = FALSE,
                                     AI.min = -6, 
                                     AI.max = 4){
  
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
  
  # if we have received a SpatialPixelsDataFrame we can plot it straight away, we just need a list of PFTs
  if(class(data) == "RasterLayer"){
    data.toplot <- data
  }
  
  else if(class(data) == "SpatialPixelsDataFrame"){
    if("GlobalBiome" %in% names(data)){ data.toplot <- raster(data, "GlobalBiome")} 
    else if ("Biome" %in% names(data)){  data.toplot <- raster(data, "Biome") }
    else { stop(paste("Error! You are wanting me to plot the Global Biomes but I can't find the correct column in the SpatialPixelsDataFrame"))}
  }
  
  # if we have received a data.table promote it to Raster
  else if(class(data) == "data.table"){
    sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance)
    sp.pixels.df <- SpatialPixelsDataFrame(sp.pixels, data.frame(data[, GlobalBiome]), tolerance = tolerance) 
    names(sp.pixels.df) <- "GlobalBiome"
    data.toplot <- raster(sp.pixels.df, "GlobalBiome")
    # clean up
    rm(sp.points, sp.pixels, sp.pixels.df)
  }  
  # else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to plot object of type", class(data), "which I don't know how to do.", sep = ""))
  }
  
  # Add PNV data if requested read it in and compare rasters
  to.plot <- c(1)
  if(addData){
    
    PNV.raster <- readPNVBiomes()
    # first check if they are on identical grids, then one can simply add the layers
    if(compareRaster(PNV.raster, data.toplot, extent=TRUE, rowcol=TRUE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=TRUE)){
      print("woohoo!")
    }
    else if(compareRaster(PNV.raster, data.toplot, extent=FALSE, rowcol=FALSE, crs=TRUE, res=TRUE, orig=FALSE, rotation=TRUE, values=FALSE, stopiffalse=FALSE, showwarning=TRUE)){
      #print("Different Extent only")
      PNV.raster <- crop(PNV.raster, data.toplot, snap = "out")
      PNV.raster <- extend(PNV.raster, data.toplot)
      data.toplot <- stack(data.toplot, PNV.raster)  
      plot.labels <- c(run.title, "PNV Hickler et al. 2006")
      
    }
    else {
      
      stop("Data and model run biome maps don't have the same resolution or coordinate system, code in an NGB regridding or something")
      
    }
    
    to.plot <- c(1,2)      
  }
  
  # AI INDEX
  AI.min = min(AI.min, min(fossils$index))
  AI.max = max(AI.max, max(fossils$index))
  breaks = seq(AI.min, AI.max, 1)
  AI.palette = colorRampPalette(c("gray90", "black")) #this is a function which returns a list of colous
  AI.colours = AI.palette(length(breaks))
  
  # now map the list of layout objects to overlay with sp.layout
  layout.objs <- list()
  
  # now prepare the list of points to overlay with sp.layout
  fossil.cols <- list()
  for(row.counter in 1:nrow(fossils)){
    for(break.counter in 1:(length(breaks)-1)){
      if(fossils$index[row.counter] == breaks[break.counter]) fossil.cols[row.counter] <- AI.colours[break.counter]
    }
  }
  fossil.cols <- unlist(fossil.cols)
  
  points.layout.object <- list("sp.points", fossils, bg = fossil.cols, col = fossil.cols, fill = fossil.cols, pch = 21, cex = 1.2)
  layout.objs[["AI"]] <- points.layout.object
  
  #breaks = seq(min(fossils$index), max(fossils$index),1)
  #AI.palette = colorRampPalette(c("black","gray90")) #this is a function which returns a list of colous
  #AI.colours = AI.palette(length(breaks))
  
  
  
  # FILENAME
  if(!is.null(filename)){ current.filename <- filename}
  else {
    if(!is.null(period)){ current.filename <- paste("GlobalBiomesWithAI.", run@id, ".TA.", period@start, "-", period@end, ".png", sep = "") }
    else { current.filename = paste("GlobalBiomesWithAI.", run@id, ".TA.png", sep = "")}
  }
  
  # PLOT TITLE
  if(is.null(plot.title)){
    plot.title <- paste("Global Biomes with AI:", run@description, sep = " ")
  }
  
  CairoPNG(paste(plot.dir, current.filename, sep = "/"), width = 1800, height = 1000, title = paste("Global Biomes", sep = " "))  
  print(spplot(data.toplot,
               to.plot,
               xlab = list(label = "Longitude", cex = 2),
               ylab = list(label = "Latitude", cex = 2),
               col.regions = GlobalBiome.cols,
               at = 0:length(GlobalBiome.str),
               scales = list(draw = TRUE, cex = 2),
               as.table = TRUE,
               main=list(label=plot.title, cex = 3),
               par.strip.text = list(lines = 1.0, cex = 1.5),
               sp.layout = layout.objs,
               names.attr = plot.labels,
               maxpixels = 100000,
               colorkey = list(col = rev(GlobalBiome.cols), 
                               space = "right",
                               labels = list(labels = rev(GlobalBiome.str), 
                                             cex = 2,
                                             at = seq(0.5, length(GlobalBiome.str), 1))),
               legend = list(left = list(fun = draw.key, args = list(key = list(points = list(pch = c(21), fill = AI.colours, col = AI.colours, cex = 1.2), text = list(paste(AI.min:AI.max), cex = 2), title = "AI", cex = 2), draw=FALSE, vp=NULL)))
               
  )
  )
  dev.off()
  
}



##########################################################################################################################################
################################################## PLOT AI CLASSES MAP AND POINTS ########################################################
##########################################################################################################################################


plotAIMapAndPoints <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                               run = NULL, 
                               fossils,
                               period = NULL, 
                               plot.dir = NULL, 
                               filename = NULL, 
                               layout.objs = NULL, 
                               plot.title =  NULL,
                               run.title = NULL,
                               addData = FALSE){
  
  
  AI.cols <- list(absent = "grey90", dominant = "green", subdominant = "yellow", trace = "red")
  AI.strings <- list("absent", "trace", "subdominant", "dominant")
  
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
  
  # if we have received a SpatialPixelsDataFrame we can plot it straight away, we just need a list of PFTs
  if(class(data) == "RasterLayer"){
    data.toplot <- data
  }
  
  else if(class(data) == "SpatialPixelsDataFrame"){
    if("GlobalBiome" %in% names(data)){ data.toplot <- raster(data, "GlobalBiome")} 
    else if ("Biome" %in% names(data)){  data.toplot <- raster(data, "Biome") }
    else { stop(paste("Error! You are wanting me to plot the Global Biomes but I can't find the correct column in the SpatialPixelsDataFrame"))}
  }
  
  # if we have received a data.table promote it to Raster
  else if(class(data) == "data.table"){
    sp.points <- SpatialPoints(data.frame(data[,list(Lon, Lat)]), proj4string = CRS("+proj=longlat +datum=WGS84"))
    sp.pixels <- SpatialPixels(sp.points, tolerance = tolerance)
    data.toplot <- SpatialPixelsDataFrame(sp.pixels, data.frame(data[,]), tolerance = tolerance) 
    # clean up
    rm(sp.points, sp.pixels)
  }  
  # else error 
  else{
    # catch -proper exceptions later?
    stop(paste("Trying to plot object of type", class(data), "which I don't know how to do.", sep = ""))
  }
  
  PFTs <-getPFTs(data, global.PFTs)
  
  
  for(PFT in PFTs){
    
    if(PFT@name %in% names(fossils)){
      
      status.colourlist <- list()
      status.labellist <- list()
      
      # make the list of colours etc for each AI status
      for(status in levels(data.toplot@data[,which(names(data)==paste(PFT@name, "Status", sep = ""))])){
        status.colourlist[[status]]<- AI.cols[[status]]
        status.labellist[[status]] <- status
      }
      
             
      # now map the list of layout objects to overlay with sp.layout
      layout.objs <- list()
      
      # make a list of colours for the points, based on PFT status at each fossil site
      points.status.colourlist <- list()  
      for(row.counter in 1:nrow(fossils)){
        status.string <- as.character(fossils@data[row.counter,which(names(fossils@data)==paste(PFT@name, "Status", sep = "."))])   
        points.status.colourlist <- append(points.status.colourlist, AI.cols[[status.string]])
        
      }    
      
      points.status.colourlist <- unlist(points.status.colourlist)
      
      # make the sp.layout object  
      points.layout.object <- list("sp.points", fossils, bg = points.status.colourlist, col = "black", fill = points.status.colourlist, pch = 21, cex = 1.2)
      layout.objs[["AI"]] <- points.layout.object
      

      # FILENAME
      if(!is.null(filename)){ current.filename <- filename}
      else {
        if(!is.null(period)){ current.filename <- paste("AIStatusMap.", PFT@name, ".", run@id, ".TA.", period@start, "-", period@end, ".png", sep = "") }
        else { current.filename = paste("AIStatusMap.", PFT@name, ".", run@id, ".TA.png", sep = "")}
      }
      
      # PLOT TITLE
      plot.title <- paste("AI Status ", PFT@name, ": ", run@description, sep = "")

         
      CairoPNG(paste(plot.dir, current.filename, sep = "/"), width = 1800, height = 1000, title = paste("Global Biomes", sep = " "))  
      print(spplot(data.toplot,
                   paste(PFT@name, "Status", sep = ""),
                   xlab = list(label = "Longitude", cex = 2),
                   ylab = list(label = "Latitude", cex = 2),
                   col.regions = unlist(status.colourlist),
                   #at = 0:length(GlobalBiome.str),
                   scales = list(draw = TRUE, cex = 2),
                   as.table = TRUE,
                   main=list(label=plot.title, cex = 3),
                   par.strip.text = list(lines = 1.0, cex = 1.5),
                   sp.layout = layout.objs,
                   names.attr = plot.labels,
                   maxpixels = 100000,
                   colorkey = list(col = unlist(status.colourlist), 
                                   space = "right",
                                   labels = list(labels = unlist(status.labellist), 
                                                 cex = 2,
                                                 at = seq(1, length(unlist(status.labellist)), 1)
                                   ))
      )
      )
      dev.off()
      
    }
    
  }
}


#### NOTE: The ordering of these is important because R orders factors alphabetically.
####       I  
AI.strings <- list("absent", "trace", "subdominant", "dominant")


plotAIStatus <-function(vegrun, PFTs, points){
  
  
  # got to the run dir
  start.dir <- getwd()
  setwd(vegrun@path)
  
  
  for(PFT in PFTs){
    
    title <- paste(PFT@name, "Status", sep = " ")
    
    status.colourlist <- list()
    status.labellist <- list()
    
    
    # make the list of colours etc for each AI status
    for(status in levels(vegrun@data@data[,which(names(vegrun@data@data)==paste(PFT@name, "Status", sep = "."))])){
      status.colourlist[[status]]<- AI.cols[[status]]
      status.labellist[[status]] <- AI.strings[[status]]
      
    }
    
    
    # now map the list of layout objects to overlay with sp.layout
    layout.objs <- list()
    
    # make a list of colours for the points, based on PFT status at each fossil site
    points.status.colourlist <- list()  
    for(row.counter in 1:nrow(points)){
      
      status.string <- as.character(points@data[row.counter,which(names(points@data)==name.string)])     
      points.status.colourlist <- append(points.status.colourlist, AI.cols[[status.string]])
      
    }    
    
    points.status.colourlist <- unlist(points.status.colourlist)
    
    # make the sp.layout object  
    points.layout.object <- list("sp.points", points, bg = points.status.colourlist, col = "black", fill = points.status.colourlist, pch = 21, cex = 0.7)
    layout.objs[["AI"]] <- points.layout.object
    
    
    png(paste(vegrun@filename, name.string, "png", sep = "."), width = 1800, height = 1000, title = paste(vegrun@filename, name.string))
    print(spplot(vegrun@data,
                 zcol = name.string,
                 xlab = "Longitude",
                 ylab = "Latitude",
                 col.regions= unlist(status.colourlist),
                 scales = list(draw = TRUE),
                 main=list(label=title),
                 sp.layout = layout.objs
    ))
    
    dev.off()
    
  } 
  
  # return to initial directory
  setwd(start.dir)
  
}


readPNVBiomes.paleo <- function(plot = FALSE, plot.dir = "."){
  
  file.string = globalbiome.file
  
  # get the number of columns which is 
  ncols <- length(names(read.table(file.string, header=TRUE, dec=".",  colClasses="numeric", comment.char="", nrows = 1)))
  
  # read using fread function from data.table but with the white spaces handled correctly using an awk command
  # because at time of writing fread does not handles multiple whitespaces as separators
  PNV.dt <- awkFread(file.string, colNums = c(1:ncols), header=T)
  
  # divide Lon and Lat by 10, offset and London centre
  PNV.dt[,Lon := (Lon/10) + 0.25]
  PNV.dt[,Lat := (Lat/10) + 0.25]
  PNV.dt[, Lon := vapply(PNV.dt[,Lon], 1, FUN = LondonCentre)]
  
  
  # Make into a raster
  PNV.spdf <- makeSPDFfromDT(PNV.dt, tolerance = 0.00001)
  PNV.raster <- raster(PNV.spdf, "Biome")
  
  # reclassify with subs with following logic
  
  #           ORIGINAL ORDERING IN DATA (HICKLER ET AL. 2006)     NEW ORDERING FOR 'MEGA' PALEO BIOMES    
  # BIOME 1 - Boreal deciduous forest/woodland                    BIOME 5
  # BIOME 2 - Boreal evergreen forest/woodland                    BIOME 4
  # BIOME 3 - Temperate/Boreal mixed forest                       BIOME 7 - Temperate Deciduous
  # BIOME 4 - Temperate conifer forest                            BIOME 6 - Temperate Evergreen
  # BIOME 5 - Temperate deciduous forest                          BIOME 7 - Temperate Deciduous
  # BIOME 6 - Temperate evergreen forest                          BIOME 6 - Temperate Evergreen
  # BIOME 7 - Temperate mixed forest                              BIOME 7 - Temperate Deciduoud
  # BIOME 8 - Tropical seasonal forest                            BIOME 3
  # BIOME 9 - Tropical rain forest                                BIOME 1
  # BIOME 10 - Tropical Deciduous forest                          BIOME 2
  # BIOME 11 - Moist Savannas                                     BIOME 9
  # BIOME 12 - Dry Savannas                                       BIOME 10
  # BIOME 13 - Tall Grassland                                     BIOME 12 
  # BIOME 14 - Dry Grassland                                      BIOME 13 - Arid shrubland/grasslands
  # BIOME 15 - Xeric woodland/shrub                               BIOME 8  
  # BIOME 16 - Arid shrubland/steppe                              BIOME 13 - Arid shrubland/grasslands
  # BIOME 17 - Desert                                             BIOME 14
  # BIOME 18 - Arctic/alpine tundra                               BIOME 11
  
  
  # from above orderings
  subs.rules <- data.frame(id=1:18, v=c(5,4,7,6,7,6,7,3,1,2,9,10,12,13,8,13,14,11))
  PNV.raster <- subs(PNV.raster, subs.rules)
  
  # plot if requested and return
  if(plot){ plotGlobalBiomeMap(PNV.raster, plot.dir = plot.dir, filename = "PNV2006.png") }
  return(PNV.raster)
  
}


addGlobalBiomesPaleo <-function(input.dt, PFT.data){
  
  
  if("GlobalBiome" %in% names(input.dt)) { input.dt[, GlobalBiome := NULL] }
  
  # Combine shade tolerance classes and add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
  combineShadeTolerance(input.dt, global.PFTs)
  addDominantPFT(input.dt, PFT.data, do.all = FALSE, do.tree = TRUE, do.woody = FALSE)
  addAllVegTotals(input.dt, global.PFTs)
  addVegFractions(input.dt, PFT.data, targets = c("pft"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Tropical"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Temperate"), of.total = FALSE,  of.tree = TRUE, of.woody = FALSE)
  addVegFractions(input.dt, PFT.data, targets = c("Grass"), of.total = TRUE,  of.tree = FALSE, of.woody = FALSE)
  
  
  # Apply biome rules and return
  input.dt[, GlobalBiome := apply(input.dt[,,with=FALSE],FUN=GlobalBiomeRulesPaleo,MAR=1)]
  return(input.dt)
  
}




GlobalBiomeRulesPaleo <- function(lai){
  
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


PaleoBiome.str <- c("Tropical Rain Forest",                     
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


PaleoBiome.cols <- c("Tropical Rain Forest" = "seagreen",                     
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



