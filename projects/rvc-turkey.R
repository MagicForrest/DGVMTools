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

require(rgdal)
require(raster)
require(fields)
require(Cairo)

require(compiler)

# biome file
biome.file <- "/home/forrest/Projects/Turkey/BiomeMap/v2/Biome_Matt/biome_09.img"


# colour schemes.strings
TurkeyBiome.legend.str <- c("Water",
                            "Not\nclassifiable",
                            "Needleleaved\nEvergreen\nWoodlands",
                            "Broadleaved\nEvergreen\nWoodlands",
                            "Deciduous\nForest",
                            "Cold\nMontane\nForest",
                            "Presteppe\nWoodlands",
                            "Grass\nsteppe",
                            "Woody steppe/\nShrublands"
                            
)


TurkeyBiome.legend.colours <- c("cadetblue1",    
                                "black",
                                "darkolivegreen",
                                "orangered4",
                                "green3",
                                "royalblue4",
                                "sandybrown",
                                "darkseagreen1",
                                "tan")

doTurkishBiomeAggregation <- function(plot.biome.map = FALSE, dir = ""){

original.map <- raster(biome.file)  

# build the reclassifying matrix
classify.df <-  data.frame(fine = c(0), coarse = c(0)) # 0 = water
classify.df <- rbind(classify.df, c(1, 1)) # unknown and unclassifiable
classify.df <- rbind(classify.df, c(2, 5)) # "H10 Cedar forests of the Taunus" -> Cold conifer
classify.df <- rbind(classify.df, c(3, 5)) # "H11 Cedar-fir forests of the Taunus" -> Cold conifer
classify.df <- rbind(classify.df, c(4, 2)) # "H12 Montane presteppe juniper scrub" -> Needleleaved forest - ATTENTION a bit scrubbier??
classify.df <- rbind(classify.df, c(5, 2)) # "H6 Anatolian pine forests with Pinus pallasiana" -> Needleleaved forest - ATTENTION maybe more mixed?? 
classify.df <- rbind(classify.df, c(6, 2)) # "H7 Aegean forests with Pinus pallasiana" -> Needleleaved forest - ATTENTION a bit scrubbier...
classify.df <- rbind(classify.df, c(7, 2)) # "H8 Pine forests of the Taurus" -> Needleleaved forest - ATTENTION a mixed
classify.df <- rbind(classify.df, c(8, 3)) # "M10 Anatolian oak woods with Quercus  calliprinos" -> Mediterranean Evergreen Scrub/Woodland 
classify.df <- rbind(classify.df, c(9, 2)) # "M12 Natural pine forests of stone pine" -> Needleleaved forest
classify.df <- rbind(classify.df, c(10, 2)) # "M13 Natural pine forest with Pinus brutia" -> Evergreen Forest/Woodland 
classify.df <- rbind(classify.df, c(11, 3)) # "M7 Hellenic oakwoods with Quercus ilex" -> Evergreen Forest/Woodlands
classify.df <- rbind(classify.df, c(12, 3)) # "M8 Anatolian oakwoods with Quercus ilex" ->  Evergreen Forest/Woodland 
classify.df <- rbind(classify.df, c(13, 3)) # "M8a with Pinus brutia" -> Evergreen Forest/Woodland 
classify.df <- rbind(classify.df, c(14, 3)) # "M8b  inner plains" -> Mixed woodlands
classify.df <- rbind(classify.df, c(15, 4)) # "Q1  Sub-Pontic oak-hornbeam forests" -> Deciduous forest
classify.df <- rbind(classify.df, c(16, 4)) # "Q1a with fir" -> Deciduous forest
classify.df <- rbind(classify.df, c(17, 4)) # "Q2 Western sub-Pontic beech forests" -> Deciduous forest
classify.df <- rbind(classify.df, c(18, 4)) # "Q3 Eastern sub-Pontic beech forests" -> Deciduous forest
classify.df <- rbind(classify.df, c(19, 4)) # "Q4 Sub-Pontic oak forest with conifers" -> Mixed woodland
classify.df <- rbind(classify.df, c(20, 5)) # "Q5 Subalpine sub-Pontic fir forests" -> Cold conifer
classify.df <- rbind(classify.df, c(21, 5)) # "Q6 Sub-Pontic pine forests" -> Cold conifer
classify.df <- rbind(classify.df, c(22, 5)) # "Q7 Eastern fir forests with Abies nordmanniana" -> Cold conifer
classify.df <- rbind(classify.df, c(23, 5)) # "Q7 Subalpine pine- fir forests" -> Cold conifer
classify.df <- rbind(classify.df, c(24, 4)) # "R Pontic vegetaion" -> Deciduous forest
classify.df <- rbind(classify.df, c(25, 4)) # "R2 Western oak-hornbearn woodlands" -> Deciduous forest
classify.df <- rbind(classify.df, c(26, 4)) # "R4 Pontic beechwoods with Rhododendron" -> Deciduous forest
classify.df <- rbind(classify.df, c(27, 4)) # "R5 Pontic beech-fir woods" -> Deciduous forest - maybe mixed?
classify.df <- rbind(classify.df, c(28, 4)) # "R6 Eastern montane beechwoods" -> Deciduous forest 
classify.df <- rbind(classify.df, c(29, 4)) # "R7 Eastern beech-spruce forest" -> Deciduous forest - maybe mixed?
classify.df <- rbind(classify.df, c(30, 4)) # "S10 Anatolian oak forests with Quercus frainetto" -> Deciduous forest 
classify.df <- rbind(classify.df, c(31, 3)) # "S11 Anatolian  forests with Ostrya, Carpinus and Pinus brutia" -> Needleleaved forest
classify.df <- rbind(classify.df, c(32, 1)) # "S12 Cultivated supra - Mediterranean plains" -> Unknown
classify.df <- rbind(classify.df, c(33, 6)) # "S13 Presteppe oak forests with Quercus anatolicas" -> Presteppe Woodlands - require deciduous?
classify.df <- rbind(classify.df, c(34, 6)) # "S14 Presteppe oak forest with Juniperus excelsa" -> Presteppe Woodlands  require evergreen?
classify.df <- rbind(classify.df, c(35, 8)) # "T3 Eastern  humid thermo - Mediterranean zone" -> Woody Steppe
classify.df <- rbind(classify.df, c(36, 2)) # "T4 Pine forest with Pinus brutia and cypress" -> Needleleaved forest
classify.df <- rbind(classify.df, c(37, 8)) # "T5 Coastal thermo-Mediterranean plains" ->  Woody Steppe
classify.df <- rbind(classify.df, c(38, 7)) # "W Pontic alpine zone" ->  Grass Steppe
classify.df <- rbind(classify.df, c(39, 8)) # "X1 Steppe woodland with Quercus brantii" ->  Woody Steppe
classify.df <- rbind(classify.df, c(40, 8)) # "X2 Arid steppe with Quercus brantii" ->  Woody Steppe
classify.df <- rbind(classify.df, c(41, 5)) # "X3 Presteppe pine forests with Scots pine" ->  Cold Conifder
classify.df <- rbind(classify.df, c(42, 8)) # "X4+Y2 Steppe complex with pistachio-and almond-tree or Artemisia" ->  Woody Steppe
classify.df <- rbind(classify.df, c(43, 7)) # "Y1 Astragalus steppes" ->  Grass Steppe
classify.df <- rbind(classify.df, c(44, 7)) # "Y3 Eastern montane steppes" ->  Grass Steppe
classify.df <- rbind(classify.df, c(45, 7)) # "Y4 Subalpine steppes" ->  Grass Steppe
classify.df <- rbind(classify.df, c(46, 7)) # "Y5 High mountain vegatation" ->  Grass Steppe
classify.df <- rbind(classify.df, c(47, 7)) # "Y6 Salt steppes" ->  Grass Steppe
classify.df <- rbind(classify.df, c(48, 7)) # "Z Alpine Mediterranean zone" ->  Grass Steppe

aggregate.map <- subs(original.map, classify.df)

if(plot.biome.map){

# and plot into the directory 
start.dir <- getwd()
setwd(dir)

CairoPNG(paste("Turkey", "PNV1987", "Biome", "Map.png", sep="."), width = 1800, height = 1000, title = paste("EUR1987 Biomes Aggregated", sep = " "))

message("Plotting PNV Biomes")

print(spplot(aggregate.map, 
             col.regions = TurkeyBiome.legend.colours, 
             maxpixels = 1e8,
             cuts = 8,
             main=list(label="Potential Natural Biomes",cex=5),
             colorkey = list(col = TurkeyBiome.legend.colours, 
                             space = "bottom",
                             labels = list(labels = TurkeyBiome.legend.str,
                                           cex = 2.5,
                                           at = seq(0,8,1)))
             )
      )     

dev.off()

setwd(start.dir)

}

          
return(aggregate.map)


}


#############################################################
#####  TURKISH/MEDITERRANEAN BIOME CLASSIFICATION


addTurkeyBiomes <-function(input.dt){
 
  if("Biome" %in% names(input.dt)) { input.dt[, TurkeyBiome := NULL] }
  input.dt[, TurkeyBiome := apply(input.dt[,,with=FALSE],FUN=TurkeyBiomeRules,MAR=1)]
  return(input.dt)
  
}


TurkeyBiomeRules <- function(lai){
    
  # BIOME 0 - Water
  # added after using CORINE data
  # BIOME 4 - Deciduous forest
  if(as.numeric(lai[['Woody']]) > 2.0 && as.numeric(lai[['SummergreenFractionofTree']]) > 0.5) {return(4)}  
  # BIOME 5 - Cold Montane forest
  else if(as.numeric(lai[['Woody']]) > 1.0 && as.numeric(lai[['BorealFractionofTree']]) > 0.5) {return(5)}
  # BIOME 2 - Needle-leaved evergreen forest
  else if(as.numeric(lai[['Woody']]) > 1.0 && lai[['DominantWoody']] == "TeNE") {return(2)}
  # BIOME 3 - Mediterranean woodland/scrub
  else if(as.numeric(lai[['Woody']]) > 1.0 && lai[['DominantWoody']] == "TeBE") {return(3)}
  # BIOME 6 - Pre-steppe deciduous woodlands
  else if(as.numeric(lai[['Woody']]) > 1.0  && as.numeric(lai[['Grass']]) > 0.5 && lai[['DominantWoody']] == "TeBS") {return(6)}
  # BIOME 8 - Shrublands/Shrub Steppe
  else if((as.numeric(lai[['Woody']]) > 1.0 && (lai[['DominantWoody']] == "MeES" || lai[['DominantWoody']] == "MeRS" ))  
          || as.numeric(lai[['Woody']]) > as.numeric(lai[['Grass']])) {return(8)}
  # BIOME 7 - Grass Steppe  
  else if(as.numeric(lai[['Grass']]) > 1.0 & as.numeric(lai[['Woody']] < 1.0)) {return(7)}
  # BIOME 1 - Unclassified
  else{
    return(1)
  }

}


##########################################################################################################################################
################################################## PLOT TURKEY BIOME MAPS ################################################################
##########################################################################################################################################


plotTurkeyBiomeMap <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                               watermask, 
                               run = NULL, 
                               period = NULL, 
                               plot.dir = NULL, 
                               filename = NULL, 
                               layout.objs = NULL, 
                               plot.title =  NULL,
                               run.title = NULL,
                               addData = FALSE,
                               plotDataAlone = FALSE,
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
  
  #data.toplot <- raster(this.TA.spdf, "TurkeyBiome")
  data.toplot <- extend(data, CORINE.water)
  data.toplot[CORINE.water > 0.5] <- 0 
  

  # Add PNV data if requested read it in and compare rasters
  to.plot <- c(1)
  label.pos = "bottom"
  if(addData){
    
    PNV.raster <- doTurkishBiomeAggregation(plotDataAlone, plot.dir)
    PNV.raster <- resample(PNV.raster, data.toplot, method = "ngb")
    data.toplot <- stack(data.toplot, PNV.raster)  
    plot.labels <- c(run@description, "Council of Europe 1987")
    to.plot <- c(1,2)      
    label.pos = "right"
 
  }
 
 
  # FILENAME
  if(!is.null(filename)){ current.filename <- filename}
  else {
    if(!is.null(period)){ current.filename <- paste("TurkeyBiomes.", run@id, ".TA.", period@start, "-", period@end, ".png", sep = "") }
    else { current.filename = paste("TurkeyBiomes.", run@id, ".TA.png", sep = "")}
  }
  
  CairoPNG(paste(plot.dir, current.filename, sep = "/"), width = 1800, height = 1000, title = paste("Turkey Biomes", sep = " "))  
  
  print(spplot(data.toplot,
               to.plot,
               par.settings = list(panel.background=list(col="grey")),
               xlab = list(label = "Longitude", cex = 2),
               ylab = list(label = "Latitude", cex = 2),
               col.regions = TurkeyBiome.legend.colours,
               cuts = 8,
               scales = list(draw = TRUE, cex = 2),
               as.table = TRUE,
               main=list(label=makePlotTitle("Turkey Biomes", plot.title, run, period), 
                         cex = 4),
               par.strip.text = list(lines = 1.0, cex = 1.5),
               sp.layout = layout.objs,
               names.attr = plot.labels,
               maxpixels = 10000000,
               colorkey = list(col = TurkeyBiome.legend.colours, 
                               space = label.pos,
                               labels = list(labels = TurkeyBiome.legend.str, 
                                             cex = 2)),
               ...
  )
  )
  dev.off()
  
}










plotTurkishBiomeMapMultiple <- function(data, # can be a data.table, SpatialPixelsDataFrame, VegVarTA (not implemented) or a raster (not implemented)
                               which.layers = "TurkeyBiome",
                               watermask, 
                               run = NULL, 
                               period = NULL, 
                               plot.dir = NULL, 
                               file.name = NULL, 
                               layout.objs = NULL, 
                               main.title =  NULL,
                               plot.labels = NULL,
                               addData = FALSE,
                               plotDataAlone = FALSE,
                               plot.height = 1000,
                               plot.width = 1800,
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

  #data.toplot <- raster(this.TA.spdf, "TurkeyBiome")
  data.toplot <- extend(data, CORINE.water)
  data.toplot[CORINE.water > 0.5] <- 0 
  
   
  ##### Here check data is right class for plotting, process it if not
  data.toplot <- promoteToRaster(data.toplot, which.layers, run@tolerance) 
  to.plot <- seq(1:nlayers(data.toplot))
  if(is.null(plot.labels)) {
    if(is.null(run)){
      plot.labels <- list()
      for(layer in names(data.toplot)){
        plot.labels <- append(plot.labels, layer) 
      }
    }
    else{ plot.labels <- run@description}
  }
  label.pos = "right"
  
  
  # Add PNV data if requested read it in and compare rasters
  if(addData){
    
    PNV.raster <- doTurkishBiomeAggregation(plotDataAlone, plot.dir)
    PNV.raster <- resample(PNV.raster, data.toplot, method = "ngb")
     
    
    # add the PNV raster layer and its title
    data.toplot <- stack(data.toplot, PNV.raster) 
    to.plot <- append(to.plot, length(to.plot)+1)
    plot.labels <- c(plot.labels, "Council of Europe 1987")
    
    
  }
   
  # FILENAME
  if(is.null(file.name)) this.file.name <- makeFileName("TurkeyBiomes", file.name = file.name, run = run, period = period)
  else this.file.name <- file.name
  
  # PLOT MAIN TITLE
  if(is.null(main.title)) this.main.title <- makePlotTitle(paste("Turkey Biomes"), main.title, run, period)
  else this.main.title <- main.title
  
  
  
  CairoPNG(file.path(plot.dir, this.file.name), width = plot.width, height = plot.height, title = paste("Turkey Biomes", sep = " "))  
  print(spplot(data.toplot,
               to.plot,
              par.settings = list(panel.background=list(col="grey")),
                 xlab = list(label = "Longitude", cex = 2),
                ylab = list(label = "Latitude", cex = 2),
                col.regions = TurkeyBiome.legend.colours,
                cuts = 8,
                scales = list(draw = TRUE, cex = 2),
                as.table = TRUE,
                main=list(label=this.main.title, 
                          cex = 4),
                par.strip.text = list(lines = 1.0, cex = 1.5),
                sp.layout = layout.objs,
                names.attr = plot.labels,
                maxpixels = 10000000,
                colorkey = list(col = TurkeyBiome.legend.colours, 
                                space = label.pos,
                                labels = list(labels = TurkeyBiome.legend.str, 
                                              cex = 2)),
              ...
  )
  )
  
  
              


  
          
    
  
  
  dev.off()
  
}






