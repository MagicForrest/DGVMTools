
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

plotGGMmap <- function(input, column=NA, colors=NA, sym.col=FALSE, wrap=1) {
  ## check if a VegObj or a list of VegObj is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegObj(input)) {
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
      if (!is.VegObj(input[[n]]))
        stop("'input' must either be a RCVTools::VegObj or a list of them!")
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
    stop("'input' must either be a RCVTools::VegObj or a list of them!")
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

