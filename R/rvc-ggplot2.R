#######################################################################
## ggplot2 themes for nicer plots #####################################
#######################################################################

.lpj.map_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                             panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                             panel.background  = element_rect(fill="#cae1ff"),
                             panel.border      = element_rect(fill=NA, linetype = "solid", colour = "black"),
                             axis.line         = element_blank(),
                             axis.text         = element_text(size=10, colour = "black"),
                             axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                             axis.ticks.length = unit(1.5, "points"),
                             axis.title        = element_blank(),
                             legend.text       = element_text(size=10),
                             legend.title      = element_blank(),
                             legend.position   = "bottom",
                             legend.key        = element_rect(colour = "black"),
                             legend.key.width  = unit(0.08, "npc"),
                             plot.background   = element_blank(),
                             plot.title        = element_text(size=22),
                             strip.background  = element_rect(fill=NA)))

.lpj.scatter_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                 panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                 panel.background  = element_blank(),
                                 panel.border      = element_blank(),
                                 axis.line         = element_line(linetype="solid", colour="black"),
                                 axis.text         = element_text(size=10, colour = "black"),
                                 axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                 axis.ticks.length = unit(1.5, "points"),
                                 axis.title        = element_text(size=10, face="bold"),
                                 legend.text       = element_text(size=10),
                                 legend.title      = element_blank(), #element_text(size=10, face="bold"),
                                 legend.position   = "bottom",
                                 legend.key        = element_blank(), #element_rect(colour = "black"),
                                 legend.key.width  = unit(0.08, "npc"),
                                 plot.background   = element_blank(),
                                 plot.title        = element_text(size=22),
                                 strip.background  = element_rect(fill=NA)))

#######################################################################
## helper functions ###################################################
#######################################################################


## add deg N/S/E/W returned as labels
## make that optional

.ll.breaks <- function(limits, n.min=5, n.max=9, descending=FALSE) {
  if (length(limits)<2) {
    stop("'limits' must have at least length 2")
  } else if (length(limits==2)) {
    if (limits[1]>limits[2])
      descending=TRUE
  } else if (!is.unsorted(limits) && limits[1]>limits[2]) {
    descending=TRUE
  }
  limits <- c(min(limits), max(limits))

  lab <- seq(limits[1], limits[2], 1/12 * 3)
  
  for (i in c(1/6, 1/3, 2/3, 1:5, 10, 15)) {
    if (length(lab) <= n.max && length(lab)>=n.min)
      break
    ext <- limits
    ext[1] = ceiling(limits[1] / i / 3)*i*3
    ext[2] = floor(limits[2] / i / 3)*i*3
    lab <- seq(ext[1], ext[2], i * 3)
  }

  if (descending)
    return(rev(lab))
  return(lab)
}

#######################################################################
## map ################################################################
#######################################################################

plotGGMap <- function(input, column=NA, colors=NA, sym.col=FALSE, wrap=1) {
  ## check if a VegSpatial or a list of VegSpatial is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegSpatial(input)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (all(colnames(input@data) != column))
      stop(paste("No column named '",column,"' present!", sep=""))
    london.centre <- input@london.centre
    map.overlay <- input@map.overlay
    d <- input@data[, c("Lon", "Lat", column), with = FALSE]
    setnames(d, column, "value")
  } else if (is.list(input)) {
    for (n in names(input)) {
      if (!is.VegSpatial(input[[n]]))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      if (n==names(input)[1]) {
        london.centre <- input[[n]]@london.centre
        map.overlay <- input[[n]]@map.overlay
        d <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        d[, sens:=n, ]
       } else {
        d.tmp <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        d.tmp[, sens:=n, ]
        d <- rbindlist(list(d, d.tmp))
        rm(d.tmp)
     }
    }
    setnames(d, column, "value")
    d <- d[, sens:=factor(sens, names(input))]
  } else {
    stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
  }

  ## if colors is a valid named vector or a data.frame with columns 'color' and 'name'
  ## plot discrete values instead of continuous
  discrete <- FALSE
  if (!any(is.na(colors))) {
    if (is.vector(colors) && !is.null(names(colors))) {
      d[, name:=names(colors)[value], ]
      discrete <- TRUE
    } else if  (is.data.frame(colors) && any(names(colors)=="color") && any(names(colors)=="name")) {
      d[, name:=colors$name[value], ]
      discrete <- TRUE
    }
  }

  ## calculate the map resolution
  lon <- sort(unique(d$Lon))
  lat <- sort(unique(d$Lat))
  res <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)],
             lat[2:length(lat)] - lat[1:(length(lat)-1)])

  ## define plot region
  lon.limit <- c(min(lon) - res/2, max(lon) + res/2)
  lat.limit <- c(min(lat) - res/2, max(lat) + res/2)

  ## Either highres/lowres, otherwise assume a country name was given
  if (london.centre) {
    if (tolower(map.overlay)=="lowres" || tolower(map.overlay)=="low.res") {
      worldmap <- fortify(map("world", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else if  (tolower(map.overlay)=="highres" || tolower(map.overlay)=="high.res") {
      worldmap <- fortify(map("worldHires", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else {
        worldmap <- fortify(map("worldHires", map.overlay, fill=TRUE, plot=FALSE))
    }
  } else {
    if (tolower(map.overlay)=="lowres" || tolower(map.overlay)=="low.res") {
      worldmap <- fortify(map("world2", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else if  (tolower(map.overlay)=="highres" || tolower(map.overlay)=="high.res") {
      worldmap <- fortify(map("world2Hires", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else {
      worldmap <- fortify(map("world2Hires", map.overlay, fill=TRUE, plot=FALSE))
    }
  }

  ## define axis labels
  ## This needs an update for non lon/lat grids
  lon <- seq(lon.limit[1], lon.limit[2], res)
  lat <- seq(lat.limit[1], lat.limit[2], res)

  ## exclude outer most longitude labels
  lon <- lon[2:(length(lon)-1)]
 
  if (lon.limit[2] - lon.limit[1] >= 360) {
    lon <- sort(unique(floor(abs(lon/60)) * sign(lon) * 60))
  } else if (lon.limit[2] - lon.limit[1] >= 180) {
    lon <- sort(unique(floor(abs(lon/45)) * sign(lon) * 45))
  } else {
    lon <- sort(unique(floor(abs(lon/30)) * sign(lon) * 30))
  }
  if (lat.limit[2] - lat.limit[1] >= 180) {
    lat <- sort(unique(floor(abs(lat/45)) * sign(lat) * 45))
  } else if (lat.limit[2] - lat.limit[1] >= 90) {
    lat <- sort(unique(floor(abs(lat/30)) * sign(lat) * 30))
  } else {
    lat <- sort(unique(floor(abs(lat/15)) * sign(lat) * 15))
  }

 # sprintf("%X", as.integer(charToRaw("°"))) => [1] "C2" "B0"
  # paste0("\u00B0") => "°"
  lon.lab <- lon
  lon.lab[lon < 0]  = paste(abs(lon[lon < 0]), "\u00B0W", sep="")
  lon.lab[lon >= 0] = paste(abs(lon[lon >= 0]), "\u00B0E", sep="")
  lat.lab <- lat
  lat.lab[lat < 0]  = paste(abs(lat[lat < 0]), "\u00B0S", sep="")
  lat.lab[lat >= 0] = paste(abs(lat[lat >= 0]), "\u00B0N", sep="")

  ## create plot
  p <- ggplot(d, aes(y=Lat, x=Lon))
  p <- p + .lpj.map_theme

  if (discrete) {
    p <- p + geom_raster(aes(fill=name))
    if (is.vector(colors)) {
      p <- p + scale_fill_manual(values=colors, breaks=names(colors), na.value="grey", guide=guide_legend(ncol=3))
    } else if (is.data.frame(colors)) {
      p <- p + scale_fill_manual(values=colors$color, breaks=colors$name, na.value="grey", guide=guide_legend(ncol=3))
    }
    p <- p + theme(legend.key.width  = unit(0.03, "npc"))
  } else {
    p <- p + geom_raster(aes(fill=value))
    if (sym.col) {
      p <- p + expand_limits(fill = c(-max(abs(d$value), na.rm=TRUE), max(abs(d$value), na.rm=TRUE)))
    }

    if (any(colnames(colors)=="color") && any(colnames(colors)=="value")) {
      p <- p + scale_fill_gradientn(colors=colors$color, values=colors$value, expand=c(0, 0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0, 0)))
    } else if (!any(is.na(colors))) {
      p <- p + scale_fill_gradientn(colors=colors, expand=c(0, 0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0, 0)))
      ##warning(paste("colors has wrong column names:", paste(colnames(colors))))
    }
  }
  p <- p + geom_path(data=worldmap, size=0.1, color = "black", aes(x=long, y=lat, group=group))
  p <- p + scale_x_continuous(breaks=lon, labels=lon.lab, expand=c(0, 0))
  p <- p + scale_y_continuous(breaks=lat, labels=lat.lab, expand=c(0, 0)) 
  p <- p + coord_fixed(xlim=lon.limit, ylim=lat.limit)
  p <- p + xlab("Longitude")
  p <- p + ylab("Latitude")

# if (!is.na(title))
#    p <- p + labs(title=title)
  if (any(colnames(d)=="sens"))
    p <- eval(parse(text=paste("p + facet_wrap(~sens, ncol=",wrap,")", sep="")))
  ##  if (is.na(variable))
  ##p <- p + facet_wrap(~variable, ncol=wrap)
  
  return(p)
}

#######################################################################
## meridional #########################################################
#######################################################################

plotGGMeridional <- function(input, column=NA, what=list(center="mn", var="sd"), alpha=c(0.8, 0.1), colors=NA, ...) {
  if (!any(names(what)=="center"))
    stop("No definition for meridional average given. Must be either 'mn', 'mean' or 'md', 'median'.")
  if (!any(names(what)=="var"))
    what[["var"]] = NA
  
  ## check if a VegObj or a list of VegObj is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegObj(input)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (all(colnames(input@data) != column))
      stop(paste("No column named '",column,"' present!", sep=""))
    london.centre <- input@london.centre
    map.overlay <- input@map.overlay
    units <- input@quant@units
    d <- input@data[, c("Lon", "Lat", column), with = FALSE]
    setnames(d, column, "value")
  } else if (is.list(input)) {
    for (n in names(input)) {
      if (!is.VegObj(input[[n]]))
        stop("'input' must either be a RCVTools::VegObj or a list of them!")
      if (n==names(input)[1]) {
        london.centre <- input[[n]]@london.centre
        map.overlay <- input[[n]]@map.overlay
        units <- input[[n]]@quant@units
        d <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        d[, sens:=n, ]
       } else {
        d.tmp <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        d.tmp[, sens:=n, ]
        d <- rbindlist(list(d, d.tmp))
        rm(d.tmp)
     }
    }
    setnames(d, column, "value")
    d <- d[, sens:=factor(sens, names(input))]
  } else {
    stop("'input' must either be a RCVTools::VegObj or a list of them!")
  }
  
  if (length(what[["var"]])>=2 || is.numeric(what[["var"]])) {
    qt <- c(min(what[["var"]]), max(what[["var"]]))
    if (qt[1]==qt[2]) {
      qt[1] = (1 - qt[1]) / 2
      qt[2] = 1 - qt[1]
    }
    what[["var"]]="mm"
  } else {
    qt <- c(0, 1)
  }

  if (any(colnames(d)=="sens")) {
    if (what[["center"]] == "md" || what[["center"]] == "median") {
      md <- d[, list(value.center=median(value),
                     value.sd=sd(value),
                     value.se=sd(value)/sqrt(length(value)),
                     value.min=quantile(value, qt[1]),
                     value.max=quantile(value, qt[2])), by = c("Lat", "sens")]
    } else {
      md <- d[, list(value.center=mean(value),
                     value.sd=sd(value),
                     value.se=sd(value)/sqrt(length(value)),
                     value.min=quantile(value, qt[1]),
                     value.max=quantile(value, qt[2])), by = c("Lat", "sens")]
    }
    p <- ggplot(md, aes(x=Lat, y=value.center, col=sens))
  } else {
    if (what[["center"]] == "md" || what[["center"]] == "median") {
      md <- d[, list(value.center=median(value),
                     value.sd=sd(value),
                     value.se=sd(value)/sqrt(length(value)),
                     value.min=quantile(value, qt[1]),
                     value.max=quantile(value, qt[2])), by = c("Lat")]
    } else {
      md <- d[, list(value.center=mean(value),
                     value.sd=sd(value),
                     value.se=sd(value)/sqrt(length(value)),
                     value.min=quantile(value, qt[1]),
                     value.max=quantile(value, qt[2])), by = c("Lat")]
    }    
    p <- ggplot(md, aes(x=Lat, y=value.center))
  }

  p <- p + .lpj.scatter_theme

  if (!is.na(what[["var"]])) {
    if (any(colnames(d)=="sens")) {
      if (tolower(what[["var"]])=="sd") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.sd, ymin = value.center - value.sd, fill=sens), alpha = alpha[2])
      } else if (tolower(what[["var"]])=="se") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.se, ymin = value.center - value.se, fill=sens), alpha = alpha[2])
      } else if (tolower(what[["var"]])=="mm" || tolower(what[["var"]])=="minmax") {
        p <- p + geom_ribbon(aes(ymax = value.max, ymin = value.min, fill=sens), alpha = alpha[2])
      }
    } else {
      if (tolower(what[["var"]])=="sd") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.sd, ymin = value.center - value.sd), alpha = alpha[2])
      } else if (tolower(what[["var"]])=="se") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.se, ymin = value.center - value.se), alpha = alpha[2])
      } else if (tolower(what[["var"]])=="mm" || tolower(what[["var"]])=="minmax") {
        p <- p + geom_ribbon(aes(ymax = value.max, ymin = value.min), alpha = alpha[2])
      }
    }
  }
  p <- p + geom_line(size=1, alpha=alpha[1])
  p <- p + scale_x_continuous(breaks=.ll.breaks(c(min(d$Lat), max(d$Lat))), expand=c(0,0))
  if (!is.na(colors) && any(colnames(d)=="sens")) {
    p <- p + scale_fill_manual(values=colors, guide=guide_legend(ncol=3))
    p <- p + scale_color_manual(values=colors, guide=guide_legend(ncol=3))
  } else {
    p <- p + scale_fill_manual(values=brewer.pal(length(unique(d$sens)), "Set1"), guide=guide_legend(ncol=3))
    p <- p + scale_color_manual(values=brewer.pal(length(unique(d$sens)), "Set1"), guide=guide_legend(ncol=3))
  }
  p <- p + xlab("Latitude")

  p <- p + ylab(units)
  p <- p + coord_flip()
  return(p)
}

#######################################################################
## scatter ############################################################
#######################################################################

## plotGGScatter <- function(x, y, x.column, y.column,
##                           wrap.column=NA,
##                           color.column=NA, colors=NA, alpha=0.7,
##                           lines=NA,
##                           labels="", label.pos="bottomright",
##                           sym.col=FALSE,
##                           equal.axis=FALSE,
##                           wrap=0) {

##   if (is.list(input)) {
    
##   } else if (!is.VegObj(x)) {
##     stop("x must be a VegObj or a list if VegObj!")
##   }

##   if (!is.VegObj(y) || !is.raster(y)) {
##     stop("y must be a VegObj or a raster!")
##   }
  
## }
