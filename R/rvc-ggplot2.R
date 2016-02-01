
lpj.map_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
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
                            plot.title        = element_text(size=22)))




rvc.ggmap <- function(input, variable=NA, cols=NA, title=NA, sym.col=FALSE, wrap.variable=NA, wrap=1) {
  d <- input@data
  ## check data columns
  if (is.na(variable) && all(colnames(d) != "value"))
    stop("No column name given and no column named 'value' present!")
  if (all(colnames(d) != variable))
    stop(paste("No column named '",variable,"' present!", sep=""))
  
  ## calculate the map resolution
  lon <- sort(unique(d$Lon))
  lat <- sort(unique(d$Lat))
  res <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)],
             lat[2:length(lat)] - lat[1:(length(lat)-1)])

  # define plot region
  lon.limit <- c(min(lon) - res/2, max(lon) + res/2)
  lat.limit <- c(min(lat) - res/2, max(lat) + res/2)

  ## Either highres/lowres, otherwise assume a country name was given
  if (input@london.centre) {
    if (tolower(input@map.overlay)=="lowres" || tolower(input@map.overlay)=="low.res") {
      worldmap <- fortify(map("world", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else if  (tolower(input@map.overlay)=="highres" || tolower(input@map.overlay)=="high.res") {
      worldmap <- fortify(map("worldHires", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else {
        worldmap <- fortify(map("worldHires", input@map.overlay, fill=TRUE, plot=FALSE))
    }
  } else {
    if (tolower(input@map.overlay)=="lowres" || tolower(input@map.overlay)=="low.res") {
      worldmap <- fortify(map("world2", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else if  (tolower(input@map.overlay)=="highres" || tolower(input@map.overlay)=="high.res") {
      worldmap <- fortify(map("world2Hires", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
    } else {
      worldmap <- fortify(map("world2Hires", input@map.overlay, fill=TRUE, plot=FALSE))
    }
  }

  # define axis labels
  lon <- seq(lon.limit[1], lon.limit[2], res)
  lat <- seq(lat.limit[1], lat.limit[2], res)

  # exclude outer most longitude labels
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

  # create plot
  p <- ggplot(d, aes(y=Lat, x=Lon))
  p <- p + lpj.map_theme

  if (is.na(variable)) {
    p <- p + geom_raster(aes(fill=value))
  } else {
    p <- eval(parse(text=paste("p + geom_raster(aes(fill=",variable,"))", sep="")))
  }

 if (sym.col && is.na(variable)) {
    p <- p + expand_limits(fill = c(-max(abs(d$value), na.rm=TRUE), max(abs(d$value), na.rm=TRUE)))
  } else if (sym.col) {
    p <- eval(parse(text=paste('p + expand_limits(fill = c(-max(abs(d$', variable, '), na.rm=TRUE), max(abs(d$',variable,'), na.rm=TRUE)))',sep="")))
  }

  if (!any(is.na(cols))) {
    if (any(colnames(cols)=="colour") && any(colnames(cols)=="value")) {
      p <- p + scale_fill_gradientn(colours=cols$colour, values=cols$value, expand=c(0,0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0,0)))
    } else if (any(colnames(cols)=="colour") && any(colnames(cols)=="name")) {
      p <- p + scale_fill_manual(values=cols$colour, na.value="grey",guide=guide_legend(ncol=4))
      p <- p + theme(legend.key.width  = unit(0.03, "npc"))
    } else {
      p <- p + scale_fill_gradientn(colours=cols, expand=c(0,0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0,0)))
      #warning(paste("cols has wrong column names:", paste(colnames(cols))))
    }
  }
  p <- p + geom_path(data=worldmap, size=0.1, colour = "black", aes(x=long, y=lat, group=group))
  p <- p + scale_x_continuous(breaks=lon, labels=lon.lab, expand=c(0,0))
  p <- p + scale_y_continuous(breaks=lat, labels=lat.lab, expand=c(0,0)) 
  p <- p + coord_fixed(xlim=lon.limit, ylim=lat.limit)
  p <- p + xlab("Longitude")
  p <- p + ylab("Latitude")

 if (!is.na(title))
    p <- p + labs(title=title)
  if (!is.na(wrap.variable))
    p <- eval(parse(text=paste("p + facet_wrap(~", wrap.variable, ", ncol=",wrap,")", sep="")))
#  if (is.na(variable))
#    p <- p + facet_wrap(~variable, ncol=wrap)

  return(p)
}

