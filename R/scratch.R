# ############## RECYCLING BIN and SCRATCH PAD #####################
# # 
# # Here keep keep code and functions whci are obselete but why may still be useful
# #!/usr/bin/Rscript
# 
# ### LAI PFT PLOTTING MINI-PACKAGE ########################### 
# 
# # Here define the the colours and limits
# forrest.nlevel = 50
# forrest.palette = colorRampPalette(c("blue", "lightskyblue1", "palevioletred", "khaki1", "yellowgreen", "forestgreen", "saddlebrown","black" )) #this is a function which returns a list of colours
# forrest.colours = forrest.palette(forrest.nlevel)
# forrest.zlim = c(log(0.06),log(8))
# forrest.ticks<- c(0.06, 0.125, 0.25, 0.5, 1, 2,4,8)
# 
# lai.diff.nlevel = 40
# lai.diff.palette = colorRampPalette(c("green", "blue", "white", "red", "yellow")) #this is a function which returns a list of colours
# lai.diff.colours = lai.diff.palette(lai.diff.nlevel)
# lai.diff.zlim = c(-1,1)
# lai.diff.ticks<- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0)
# 
# ################################################################
# ###### CMASS PFT PLOTTING MINI-PACKAGE #########################
# ################################################################
# 
# Biomass.abs.colours.palette = colorRampPalette(c("lemonchiffon","peru", "forestgreen", "dodgerblue4", "orchid4", "hotpink", "red4"))
# Biomass.abs.nlevel = 20
# Biomass.abs.colours = Biomass.abs.colours.palette(Biomass.abs.nlevel)
# Biomass.abs.zlim = c(log(0.044194174),log(45.254833996))
# Biomass.abs.ticks<- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32)
# 
# 
# 
# Biomass.diff.colours.palette = colorRampPalette(c("green","blue","white","red", "yellow")) #this is a function which returns a list of colours
# Biomass.small.diff.colours.palette = colorRampPalette(c("blue","white","red")) #this is a function which returns a list of colours
# Biomass.diff.nlevel = 21
# Biomass.small.diff.colours = Biomass.small.diff.colours.palette(Biomass.diff.nlevel)
# Biomass.diff.colours = Biomass.diff.colours.palette(Biomass.diff.nlevel)
# Biomass.diff.zlim = c(-21,21)
# Biomass.diff.ticks =  seq(-20,20,5)
# 
# 
# Biomass.percdiff.colours.palette = colorRampPalette(c("orchid4","blue","turquoise2","white","yellow", "orange","red4")) #this is a function which returns a list of colours
# Biomass.percdiff.nlevel = 21
# Biomass.percdiff.colours = Biomass.percdiff.colours.palette(Biomass.percdiff.nlevel)
# Biomass.percdiff.zlim = c(-350,350)
# 
# 
# ### WATER CONTENT PLOTTING MINI-PACKAGE ###########################
# 
# # Here define the the colours and limits
# wcont.nlevel = 20
# wcont.colours = tim.colors(wcont.nlevel)[wcont.nlevel:1]
# wcont.zlim = c(0,1)
# wcont.ticks =  seq(0,1,0.1)
# 
# wcont.diff.nlevel = 20
# wcont.diff.palette = colorRampPalette(c("orchid4","blue","turquoise2","white","yellow", "orange","red4"))
# wcont.diff.colours = wcont.diff.palette(wcont.diff.nlevel)[wcont.diff.nlevel:1]
# wcont.diff.zlim = c(-1,1)
# wcont.diff.ticks =  seq(-1,1,0.1)
# 
# 
# 
# 
# ### FIRE INTENSITY PLOTTING MINI-PACKAGE ###########################
# 
# # Here define the the colours and limits
# fire.intens.nlevel = 25
# fire.intens.colours = tim.colors(fire.intens.nlevel)
# fire.intens.zlim = c(0,50)
# fire.intens.ticks =  seq(0,50,2)
# 
# ### FIRE REISDENCE time PLOTTING MINI-PACKAGE ###########################
# 
# # Here define the the colours and limits
# residence.time.nlevel = 20
# residence.time.colours = tim.colors(residence.time.nlevel)
# residence.time.zlim = c(0,20)
# residence.time.ticks =  seq(0,20,2)
# 
# ### FIRE REISDENCE time PLOTTING MINI-PACKAGE ###########################
# 
# # Here define the the colours and limits
# fuel1hr.nlevel = 50
# fuel1hr.colours = tim.colors(fuel1hr.nlevel)
# fuel1hr.zlim = c(0,1000)
# fuel1hr.ticks =  seq(0,1000,100)
# 
# ### GLOBFIRM ###########################
# 
# # Here define the the colours and limits
# firert.diff.nlevel = 20
# firert.diff.colours = tim.colors(firert.diff.nlevel)
# firert.diff.zlim = c(-1000,1000)
# firert.diff.ticks =  seq(-1000,1000,100)
# 
# 
# buildColumListToAverage <- function(input.dt){
#   
#   # make a list of the columns to be average 
#   # we are assuming all except "Lon", "Lat" and "year"
#   cols.to.average <- "list("
#   for(name in names(input.dt)){
#     if(name != "Lon" & name != "Lat" & name != "Year"){
#       cols.to.average <- paste0(cols.to.average, name, "=mean(", name, ")", sep = "")
#       if(name != tail(names(input.dt), n=1)){
#         cols.to.average <- paste0(cols.to.average, ",", sep = "")
#       }
#     }
#     if(name == tail(names(input.dt), n=1)){
#       cols.to.average <- paste0(cols.to.average, ")", sep = "")
#     }
#   }
#   cols.to.average <- parse(text = cols.to.average)
# }


#!/usr/bin/Rscript


###### MF 2016-03-03
# Much code commented out here because I updated the old way of making map overlays.
# I left the code here because those files might be useful some day 


##########################################################################################
### SYSTEM SPECIFIC PATHS TO DATAFILES AND 

# Turkey.extent <- extent(c(26.05144,44.81644,35.82306,42.09606))
# 
# #Modern countries at high-res
# modern.countries.hires.file <- file.path(shapefile.dir, 'countries', 'countries.shp')
# modern.countries.hires.layer <- 'countries'
# modern.countries.hires.splayout <- NULL
# 
# #Modern countries
# modern.countries.file <- file.path(shapefile.dir, 'countries-lowres')
# modern.countries.layer <- 'world_by_country'
# modern.countries.splayout <- NULL
# 
# 
# #Modern coastlines at low-res
# modern.coastlines.file <- file.path(shapefile.dir, 'ne_110m_coastline.shp')
# modern.coastlines.layer <- 'ne_110m_coastline'
# modern.coastlines.splayout <- NULL
# 
# 
# #Tortonian continents
# tortonian.continents.file <- file.path(shapefile.dir, 'Coastlines_Tortonian_9.43Ma.shp')
# tortonian.continents.layer <- 'Coastlines_Tortonian_9.43Ma'
# tortonian.continents.splayout <- NULL
# 
# #Turkey outline
# turkey.splayout <- NULL
# 
# 
# makeOverlayOld <- function(coastlines, verbose = FALSE) {
#    
#   if(is.null(coastlines) || coastlines == "") {
#     if(verbose) message("Adding No Coastlines")
#     return(NULL)
#   }
#   
#   else if((is.logical(coastlines) && coastlines == TRUE) || tolower(coastlines) == "coastlines"){
#     if(verbose) message("Adding Modern Coastlines")
#     if(is.null(modern.coastlines.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(modern.coastlines.file, modern.coastlines.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       modern.coastlines.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl)
#       gc()
#      }
#     return(modern.coastlines.splayout)
#   }
#   
#   else if(tolower(coastlines) == "countries" || tolower(coastlines) == "lowres"|| tolower(coastlines) == "low-res"){
#     if(verbose) message("Adding Modern Coastlines")
#     if(is.null(modern.countries.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       print(modern.countries.file)
#       print(modern.countries.layer)
#        polygons <- readOGR(dsn = modern.countries.file, layer = modern.countries.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       modern.countries.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl)
#       gc()
#     }
#     return(modern.countries.splayout)
#   }
#   
#   
#   else if(tolower(coastlines) == "hires" || tolower(coastlines) == "hi-res"){
#     if(verbose) message("Adding Modern Countries at Hi-Res")
#     if(is.null(modern.countries.hires.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(modern.countries.hires.file, modern.countries.hires.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       modern.countries.hires.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl) 
#       gc()
#     }
#     return(modern.countries.hires.splayout)
#   }
#  
#   else if(tolower(coastlines) == "tortonian"){
#     if(verbose) message("Adding Tortonian Coastlines")
#     if(is.null(tortonian.continents.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(tortonian.continents.file, tortonian.continents.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       tortonian.continents.splayout <<-list("sp.lines", SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl)
#       gc()
#     }
#     return(tortonian.continents.splayout)
#   }
# 
#   else if(tolower(coastlines) == "turkey"){
#     if(verbose) message("Adding Turkish Coastlines")
#     if(is.null(turkey.splayout)) {
#       if(verbose) message("Reading shapefile, should be the first time only")
#       polygons <- readOGR(modern.countries.hires.file, modern.countries.hires.layer)
#       temp.sl<-as(polygons, "SpatialLines")
#       turkey.splayout <<-list("sp.lines", crop(SpatialLinesDataFrame(temp.sl, data=polygons@data, match.ID = TRUE), Turkey.extent), lwd = 0.5)
#       rm(polygons)
#       rm(temp.sl) 
#       gc()
#     }
#     return(turkey.splayout)
#   }
#   
#   
#   return(NULL)
#   
# }


# # Multiple plot function
# #
# # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# # - cols:   Number of columns in layout
# # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# #
# # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# # then plot 1 will go in the upper left, 2 will go in the upper right, and
# # 3 will go all the way across the bottom.
# #
# multiplot <- function(..., plotlist=NULL, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }

# stat_smooth_func <- function(mapping = NULL, data = NULL,
#                              geom = "smooth", position = "identity",
#                              ...,
#                              method = "auto",
#                              formula = y ~ x,
#                              se = TRUE,
#                              n = 80,
#                              span = 0.75,
#                              fullrange = FALSE,
#                              level = 0.95,
#                              method.args = list(),
#                              na.rm = FALSE,
#                              show.legend = NA,
#                              inherit.aes = TRUE,
#                              xpos = NULL,
#                              ypos = NULL) {
#   layer(
#     data = data,
#     mapping = mapping,
#     stat = StatSmoothFunc,
#     geom = geom,
#     position = position,
#     show.legend = show.legend,
#     inherit.aes = inherit.aes,
#     params = list(
#       method = method,
#       formula = formula,
#       se = se,
#       n = n,
#       fullrange = fullrange,
#       level = level,
#       na.rm = na.rm,
#       method.args = method.args,
#       span = span,
#       xpos = xpos,
#       ypos = ypos,
#       ...
#     )
#   )
# }
# 
# 
# StatSmoothFunc <- ggproto("StatSmooth", Stat,
#                           
#                           setup_params = function(data, params) {
#                             # Figure out what type of smoothing to do: loess for small datasets,
#                             # gam with a cubic regression basis for large data
#                             # This is based on the size of the _largest_ group.
#                             if (identical(params$method, "auto")) {
#                               max_group <- max(table(data$group))
#                               
#                               if (max_group < 1000) {
#                                 params$method <- "loess"
#                               } else {
#                                 params$method <- "gam"
#                                 params$formula <- y ~ s(x, bs = "cs")
#                               }
#                             }
#                             if (identical(params$method, "gam")) {
#                               params$method <- mgcv::gam
#                             }
#                             
#                             params
#                           },
#                           
#                           compute_group = function(data, scales, method = "auto", formula = y~x,
#                                                    se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
#                                                    xseq = NULL, level = 0.95, method.args = list(),
#                                                    na.rm = FALSE, xpos=NULL, ypos=NULL) {
#                             if (length(unique(data$x)) < 2) {
#                               # Not enough data to perform fit
#                               return(data.frame())
#                             }
#                             
#                             if (is.null(data$weight)) data$weight <- 1
#                             
#                             if (is.null(xseq)) {
#                               if (is.integer(data$x)) {
#                                 if (fullrange) {
#                                   xseq <- scales$x$dimension()
#                                 } else {
#                                   xseq <- sort(unique(data$x))
#                                 }
#                               } else {
#                                 if (fullrange) {
#                                   range <- scales$x$dimension()
#                                 } else {
#                                   range <- range(data$x, na.rm = TRUE)
#                                 }
#                                 xseq <- seq(range[1], range[2], length.out = n)
#                               }
#                             }
#                             # Special case span because it's the most commonly used model argument
#                             if (identical(method, "loess")) {
#                               method.args$span <- span
#                             }
#                             
#                             if (is.character(method)) method <- match.fun(method)
#                             
#                             base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
#                             model <- do.call(method, c(base.args, method.args))
#                             
#                             m = model
#                             eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                                              list(a = format(coef(m)[1], digits = 3), 
#                                                   b = format(coef(m)[2], digits = 3), 
#                                                   r2 = format(summary(m)$r.squared, digits = 3)))
#                             func_string = as.character(as.expression(eq))
#                             
#                             if(is.null(xpos)) xpos = min(data$x)*0.9
#                             if(is.null(ypos)) ypos = max(data$y)*0.9
#                             data.frame(x=xpos, y=ypos, label=func_string)
#                             
#                           },
#                           
#                           required_aes = c("x", "y")
# )
# 
# lm_eqn <- function(df){
#   m <- lm(y ~ x, df);
#   eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                    list(a = format(coef(m)[1], digits = 2), 
#                         b = format(coef(m)[2], digits = 2), 
#                         r2 = format(summary(m)$r.squared, digits = 3)))
#   as.character(as.expression(eq));                 
# }
# 

