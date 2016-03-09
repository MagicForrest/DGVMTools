#######################################################################
## ggplot2 themes for nicer plots #####################################
#######################################################################

.rvc.spatial_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
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

.rvc.scatter_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
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

.rvc.temporal_theme <- list(theme(panel.grid.minor  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                  panel.grid.major  = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                  panel.background  = element_blank(),
                                  panel.border      = element_blank(),
                                  axis.line         = element_line(size=0.1, linetype="solid", colour="black"),
#                             axis.line         = element_blank(),
                                  axis.text         = element_text(size=12, colour = "black"),
                                  axis.ticks        = element_line(size=0.1, colour = "black", linetype = "dotted"),
                                  axis.ticks.length = unit(1.5, "points"),
                                  axis.title        = element_text(size=12, face="bold"),
                                  legend.text       = element_text(size=12),
                                  legend.title      = element_text(size=12, face="bold"),
                                  legend.position   = "bottom",
                                  legend.key        = element_rect(colour = "black"),
#                             legend.key.width  = unit(0.08, "npc"),
                                  plot.background   = element_blank(),
                                  plot.title        = element_text(size=22),
                                  strip.background  = element_rect(fill=NA)))

#######################################################################
## exported helper functions ##########################################
#######################################################################
#' ggplot2 themes
#' 
#' Get the internal ggplot2 theme definitions
#' 
#' @param x string: "temporal", "spatial", or "scatter"
#' @return theme list
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
rvc.ggplot.theme <- function(x) {
  if(x=="scatter") {
    return(.rvc.scatter_theme)
  } else if (x=="temporal" || x=="ts" || x=="timeseries") {
    return(.rvc.temporal_theme)
  } else if (x=="spatial" || x=="map") {
    return(.rvc.spatial_theme)
  } else {
    return(FALSE)
  }
}

#######################################################################
## internal helper functions ##########################################
#######################################################################

## This needs an update for non lon/lat grids
.ll.breaks <- function(limits, n.min=5, n.max=9, label=NA, descending=FALSE) {
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
  
  for (i in c(1/6, 1/3, 2/3, 1:5, 10, 15, 30, 45, 60)) {
    lab.old <- lab
    if (length(lab) <= n.max && length(lab) >= n.min)
      break
    ext <- limits
    ext[1] = ceiling(limits[1] / i / 3)*i*3
    ext[2] = floor(limits[2] / i / 3)*i*3
    lab <- seq(ext[1], ext[2], i * 3)
    if (length(lab) < n.min) {
      lab <- lab.old
      break
    }
  }

  if (length(lab)<n.min || length(lab)>n.max)
    warning(paste("Did not find suitable levels between ", n.min, " and ", n.max, ", used ", length(lab), " instead.", sep=""))

  if (!is.na(label)) {
    if (tolower(label)=="lon" || label=="long" || label=="longitude") {
      ## sprintf("%X", as.integer(charToRaw("°"))) => [1] "C2" "B0"
      ## paste0("\u00B0") => "°"
      names.lab <- lab
      names.lab[lab < 0]  = paste(abs(lab[lab < 0]), "\u00B0W", sep="")
      names.lab[lab >= 0] = paste(abs(lab[lab >= 0]), "\u00B0E", sep="")
      names(lab) <- names.lab
    } else if (tolower(label)=="lat" || label=="latitude") {
      names.lab <- lab
      names.lab[lab < 0]  = paste(abs(lab[lab < 0]), "\u00B0S", sep="")
      names.lab[lab >= 0] = paste(abs(lab[lab >= 0]), "\u00B0N", sep="")
      names(lab) <- names.lab
    }
  }
  if (descending)
    return(rev(lab))
  return(lab)
}

.transpose.df <- function(df) {
  oldSAF <- options()$stringsAsFactors
  options(stringsAsFactors=FALSE)
  rn.df <- rownames(df)

  df <- as.data.frame(t(df))

  if (rn.df[1] != "1")
    colnames(df) <- rn.df
  options(stringsAsFactors=oldSAF)
  return(df)
}

## from http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
.is.color <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}

#######################################################################
## Spatial plot (map) #################################################
#######################################################################
#' Spatial plot (map) with ggplot2
#' 
#' Plots a map (or a set of maps) using the geom_raster from the ggplot2 library
#' 
#' @param input A VegSpatial or a list of VegSpatial objects.
#' @param column Name of the column(s) in the data slot of input, which should be plotted. Several columns can only be supplied with a single VegSpatial. Default: 'value'.
#' @param colors A (named) vector of colors or a data.frame with the columns 'color' and ('name' or 'value'), to be used for plotting.
#' @param sym.col boolean, if the colors should be distributed symetrically around 0.
#' @param wrap a single number of facet_wrap columns or a vector/list with the run, VegSpatial name, column and optionally ncol (number of columns), which is used to split the data in different panels. Only valid when a vector of column names or a list of VegSpatial was given as input. Otherwise it is ignored.
#' @param terr.bg which colour should be used for missing terrestial pixels (e.g. Greenland)
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Additional parameters, which are ignored so far.
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @examples See templates/Example.ggplot.R
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @import raster sp maps ggplot2 data.table
#' @export
plotGGSpatial <- function(input, column='value', colors=NA, sym.col=FALSE, wrap=1, terr.bg=NA, long.title=TRUE, plot=TRUE, ...) {
  ## check if a VegSpatial or a list of VegSpatial is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegObject(input, spatial=TRUE)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (!is.na(column) && length(column)>1) {
      for (cn in column) {
        if (all(colnames(input@data)!=cn))
          stop(paste("No column named '", cn, "' present!", sep=""))
      }
    } else if (all(colnames(input@data) != column)) {
      stop(paste("No column named '",column,"' present!", sep=""))
    }
    london.centre <- input@run@london.centre
    if (is.character(input@run@map.overlay)) {
      map.overlay <- input@map.overlay
    } else {
      map.overlay <- fortify(SpatialLinesDataFrame(input@run@map.overlay[[2]],
                                                   data.frame(ID=getSLLinesIDSlots(input@run@map.overlay[[2]]))))
    }
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column)==1) {
      setnames(dt, column, "value")
    } else {
      dt <- melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
      if (length(wrap)>1 || !is.numeric(wrap))
        wrap <- ceiling(sqrt(length(column)))
    }
    if (length(wrap)>1) {
      if (!is.list(wrap))
        wrap <- list(run=wrap[1], name=wrap[2], column=wrap[3],
                    ncol={if (length(wrap)>=4) wrap[4] else 1},
                     map=NA, stringsAsFactors=FALSE)
      ## needs a tryCatch, if the data is not available
      dt.wrap <- eval(parse(text=paste(wrap$run, "@spatial[['", wrap$name, "']]@data[, c('Lon', 'Lat', '", wrap$column,"'), with=FALSE]", sep="")))
      setnames(dt.wrap, wrap$column, "wrap.tmp")
      dt <- dt[dt.wrap]
    
      ## if wrap$map is a valid named vector the names are used for x-labels
      if (!any(is.na(wrap$map))) {
        if (is.vector(wrap$map) && !is.null(names(wrap$map))) {
          eval(parse(text=paste('dt[, name:=names(wrap$map)[wrap.tmp], ]', sep="")))
        } else if (is.vector(wrap$map)) {
          eval(parse(text=paste('dt[, name:=wrap$map[wrap.tmp], ]', sep="")))
        }
        setnames(dt, "name", "sens")
      } else {
        setnames(dt, "wrap.tmp", "sens")
      }

      wrap <- wrap$ncol
    }
  } else if (is.list(input)) {
    if (!is.numeric(wrap)) {
      warning("'wrap' must be numeric if input is a list. Setting it to 1 column.")
      wrap <- 1
    }
    for (i in 1:length(input)) {
      if (!is.VegObject(input[[i]], spatial=TRUE))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      
      if (is.na(column) && all(colnames(input[[i]]@data) != "value"))
        stop("No column name given and no column named 'value' present!")
      if (!is.na(column) && length(column)>1) {
        warning("Several column names are supplied. Will only use the first one!")
        column <- column[1]
      }
      if (all(colnames(input[[i]]@data) != column)) {
        stop(paste("No column named '",column,"' present!", sep=""))
      }
      
      if (i==1) {
        london.centre <- input[[i]]@run@london.centre
        if (is.character(input[[i]]@run@map.overlay)) {
          map.overlay <- input[[i]]@map.overlay
        } else {
          map.overlay <- fortify(SpatialLinesDataFrame(input[[i]]@run@map.overlay[[2]],
                                                       data.frame(ID=getSLLinesIDSlots(input[[i]]@run@map.overlay[[2]]))))
        }
        dt <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[i]]@run@description, ]
          titles <- input[[i]]@run@description
        } else {
          dt[, sens:=input[[i]]@run@id, ]
          titles <- input[[i]]@run@id
        }
      } else {
        dt.tmp <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@run@description, ]
          titles <- append(titles, input[[i]]@run@description)
        } else {
          dt.tmp[, sens:=input[[i]]@run@id, ]
          titles <- append(titles, input[[i]]@run@id)
        }
        dt <- rbindlist(list(dt, dt.tmp))
        rm(dt.tmp)
      }
    }
    setnames(dt, column, "value")
    dt <- dt[, sens:=factor(sens, titles)]
  } else {
    stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
  }

  ## if colors is a valid named vector or a data.frame with columns 'color' and 'name'
  ## plot discrete values instead of continuous
  discrete <- FALSE
  if (!any(is.na(colors))) {
    if (is.vector(colors) && !is.null(names(colors))) {
      dt[, name:=names(colors)[value], ]
      discrete <- TRUE
    } else if  (is.data.frame(colors) && any(names(colors)=="color") && any(names(colors)=="name")) {
      dt[, name:=colors$name[value], ]
      discrete <- TRUE
    }
  }

  if (!plot)
    return(dt)

  ## calculate the map resolution
  lon <- extract.seq(dt$Lon)
  lat <- extract.seq(dt$Lat)
  res <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)],
             lat[2:length(lat)] - lat[1:(length(lat)-1)])

  ## define plot region
  lon.limit <- c(min(lon) - res/2, max(lon) + res/2)
  lat.limit <- c(min(lat) - res/2, max(lat) + res/2)

  ## if a valid color for the land is requested
  if (!is.na(terr.bg) && .is.color(terr.bg)) {
    to <- raster(nrows=length(lat), ncols=length(lon),
                 xmn=min(lon) - res/2, xmx=max(lon) + res/2,
                 ymn=min(lat) - res/2, ymx=max(lat) + res/2,
                 crs=CRS("+proj=longlat +ellps=WGS84"))
    data(slm)
    slm <- projectRaster(slm, to, "nbg")
    slm <- as.data.frame(slm, xy=TRUE)
    colnames(slm) <- c("Lon", "Lat", "value")
    slm <- subset(slm, value>0)
    if (any(colnames(dt)=="sens")) {
      for (sens in levels(dt$sens)) {
        if (!any(colnames(slm)=="sens")) {
          slm$sens = sens
        } else {
          slm <- rbind(slm, data.frame(slm[c("Lon","Lat","value")], sens=sens))
        }
      }
    }
  } else {
    terr.bg <- NA
  }
  
  ## If map.overlay is not yet defined properly, use
  ## either highres/lowres, otherwise assume a country name was given
  if (is.character(map.overlay)) {
    if (london.centre) {
      if (tolower(map.overlay)=="lowres" || tolower(map.overlay)=="low.res") {
        map.overlay <- fortify(map("world", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
      } else if  (tolower(map.overlay)=="highres" || tolower(map.overlay)=="high.res") {
        map.overlay <- fortify(map("worldHires", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
      } else {
        map.overlay <- fortify(map("worldHires", map.overlay, fill=TRUE, plot=FALSE))
      }
    } else {
      if (tolower(map.overlay)=="lowres" || tolower(map.overlay)=="low.res") {
        map.overlay <- fortify(map("world2", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
      } else if  (tolower(map.overlay)=="highres" || tolower(map.overlay)=="high.res") {
        map.overlay <- fortify(map("world2Hires", xlim=lon.limit, ylim=lat.limit, fill=TRUE, plot=FALSE))
      } else {
        map.overlay <- fortify(map("world2Hires", map.overlay, fill=TRUE, plot=FALSE))
      }
    }
  }
  
  ## define axis labels
  ## This needs an update for non lon/lat grids
  lon <- seq(lon.limit[1], lon.limit[2], res)
  lat <- seq(lat.limit[1], lat.limit[2], res)

  ## calculate aspect ratio (required for lat, lon labels in non-global plots)
  fr_lat <- length(lat) / max(c(length(lat) + length(lon))) 
  fr_lon <- length(lon) / max(c(length(lat) + length(lon))) 


  if (wrap==1) {
    lon <- .ll.breaks(lon[2:(length(lon)-1)], label="lon", n.min=4, n.max=3+ceiling(fr_lon*4))
    lat <- .ll.breaks(lat[2:(length(lat)-1)], label="lat", n.min=4, n.max=3+ceiling(fr_lat*4))
  } else {
    lon <- .ll.breaks(lon[2:(length(lon)-1)], label="lon", n.min=2, n.max=1+ceiling(fr_lon*2))
    lat <- .ll.breaks(lat[2:(length(lat)-1)], label="lat", n.min=2, n.max=1+ceiling(fr_lat*2))
  }
    
  ## create plot
  p <- ggplot(dt, aes(y=Lat, x=Lon))
  p <- p + .rvc.spatial_theme

  if (!is.na(terr.bg))
    p <- p + geom_raster(data=slm, fill=terr.bg)

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
      p <- p + expand_limits(fill = c(-max(abs(dt$value), na.rm=TRUE), max(abs(dt$value), na.rm=TRUE)))
    }

    if (any(colnames(colors)=="color") && any(colnames(colors)=="value")) {
      p <- p + scale_fill_gradientn(colors=colors$color, values=colors$value, expand=c(0, 0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0, 0)))
    } else if (!any(is.na(colors))) {
      p <- p + scale_fill_gradientn(colors=colors, expand=c(0, 0))
      p <- p + guides(fill=guide_colorbar(nbin = 101, expand=c(0, 0)))
    }
  }
  p <- p + geom_path(data=map.overlay, size=0.1, color = "black", aes(x=long, y=lat, group=group))
  p <- p + scale_x_continuous(breaks=lon, labels=names(lon), expand=c(0, 0))
  p <- p + scale_y_continuous(breaks=lat, labels=names(lat), expand=c(0, 0)) 
  p <- p + coord_fixed(xlim=lon.limit, ylim=lat.limit)
  p <- p + xlab("Longitude")
  p <- p + ylab("Latitude")

  if (any(colnames(dt)=="sens")) {
    p <- eval(parse(text=paste("p + facet_wrap(~sens, ncol=",wrap,")", sep="")))
  } else if (long.title) {
    p <- p + labs(title=input@run@description)
  }
    
  ## make axis labels smaller for facet plots
  if(wrap>1){
    p <- p + theme(axis.text         = element_text(size=7))
  }

  ## return the plot for further manipulation or direct printing
  return(p)
}

#######################################################################
## meridional #########################################################
#######################################################################
#' Plot a meridional mean with ggplot2
#' 
#' Plots vertical lines as average over latitudinal bands.
#' 
#' @param input A VegSpatial or a list of VegSpatial objects.
#' @param column Name of the column(s) in the data slot of input, which should be plotted. Several columns can only be supplied with a single VegSpatial. Default: 'value'.
#' @param what A list with the elements 'center' and 'var'. 'center' can be one of 'mn', 'mean', 'md', 'median' for the line. 'var' is for the variability polygon drawn in the background. 'var' can be 'sd' (sandard deviation), 'se' (standard error), 'mm'/'minmax', a numeric value (0-1) of quantile to be shown arround the central line or a vector specifying the upper and lower quantile.
#' @param alpha Vector of two elements: Transparency values for the line (first) and background polygon (second).
#' @param colors Colors for the diffent VegSpatial objects.
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @examples See templates/Example.ggplot.R
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @import ggplot2 data.table
plotGGMeridional <- function(input, column='value', what=list(center="mn", var="sd"), alpha=c(0.8, 0.1), colors=NA, long.title=TRUE, plot=TRUE, ...) {
  if (!any(names(what)=="center"))
    stop("No definition for meridional average given. Must be either 'mn', 'mean' or 'md', 'median'.")
  if (!any(names(what)=="var"))
    what[["var"]] = NA
  
  ## check if a VegObj or a list of VegObj is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegObject(input, spatial=TRUE)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    
    if (!is.na(column) && length(column)>1) {
      for (cn in column) {
        if (all(colnames(input@data)!=cn))
          stop(paste("No column named '", cn, "' present!", sep=""))
      }
    } else if (all(colnames(input@data) != column)) {
      stop(paste("No column named '",column,"' present!", sep=""))
    }
    
    units <- input@quant@units
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column)==1) {
      setnames(dt, column, "value")
    } else {
      dt <- melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
    }
  } else if (is.list(input)) {
    for (i in 1:length(input)) {
      if (!is.VegObject(input[[i]], spatial=TRUE))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      if (i==1) {
        units <- input[[i]]@quant@units
        dt <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[i]]@run@description, ]
          titles <- input[[i]]@run@description
        } else {
          dt[, sens:=input[[i]]@run@id, ]
          titles <- input[[i]]@run@id
        }
       } else {
        dt.tmp <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@run@description, ]
          titles <- append(titles, input[[i]]@run@description)
        } else {
          dt.tmp[, sens:=input[[i]]@run@id, ]
          titles <- append(titles, input[[i]]@run@id)
        }
        dt <- rbindlist(list(dt, dt.tmp))
        rm(dt.tmp)
     }
    }
    setnames(dt, column, "value")
    dt <- dt[, sens:=factor(sens, titles)]
  } else {
    stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
  }
  
  if (length(what[["var"]])>=2 || is.numeric(what[["var"]])) {
    qt <- c(min(what[["var"]]), max(what[["var"]]))
    if (qt[1]<0)
      qt[1]=0
    if (qt[2]>1)
      qt[2]=1
    if (qt[1]==qt[2]) {
      qt[1] = (1 - qt[1]) / 2
      qt[2] = 1 - qt[1]
    }
    what[["var"]]="mm"
  } else {
    qt <- c(0, 1)
  }

  if (any(colnames(dt)=="sens")) {
    if (what[["center"]] == "md" || what[["center"]] == "median") {
      md <- dt[, list(value.center=median(value),
                      value.sd=sd(value),
                      value.se=sd(value)/sqrt(length(value)),
                      value.min=quantile(value, qt[1]),
                      value.max=quantile(value, qt[2])), by = c("Lat", "sens")]
    } else {
      md <- dt[, list(value.center=mean(value),
                      value.sd=sd(value),
                      value.se=sd(value)/sqrt(length(value)),
                      value.min=quantile(value, qt[1]),
                      value.max=quantile(value, qt[2])), by = c("Lat", "sens")]
    }
    p <- ggplot(md, aes(x=Lat, y=value.center, col=sens))
  } else {
    if (what[["center"]] == "md" || what[["center"]] == "median") {
      md <- dt[, list(value.center=median(value),
                      value.sd=sd(value),
                      value.se=sd(value)/sqrt(length(value)),
                      value.min=quantile(value, qt[1]),
                      value.max=quantile(value, qt[2])), by = c("Lat")]
    } else {
      md <- dt[, list(value.center=mean(value),
                      value.sd=sd(value),
                      value.se=sd(value)/sqrt(length(value)),
                      value.min=quantile(value, qt[1]),
                      value.max=quantile(value, qt[2])), by = c("Lat")]
    }
    if (!is.character(colors) || !.is.color(colors)) {
      colors="black"
    } 
    p <- ggplot(md, aes(x=Lat, y=value.center))
  }

  if (!plot)
    return(md)
  
  p <- p + .rvc.scatter_theme

  if (!is.na(what[["var"]])) {
    if (any(colnames(dt)=="sens")) {
      if (tolower(what[["var"]])=="sd") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.sd,
                                 ymin = value.center - value.sd, fill=sens),
                             alpha = alpha[2])
      } else if (tolower(what[["var"]])=="se") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.se,
                                 ymin = value.center - value.se, fill=sens),
                             alpha = alpha[2])
      } else if (tolower(what[["var"]])=="mm" || tolower(what[["var"]])=="minmax") {
        p <- p + geom_ribbon(aes(ymax = value.max,
                                 ymin = value.min, fill=sens),
                             alpha = alpha[2])
      }
    } else {
      if (tolower(what[["var"]])=="sd") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.sd,
                                 ymin = value.center - value.sd),
                             color=colors, fill=colors, alpha = alpha[2])
      } else if (tolower(what[["var"]])=="se") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.se,
                                 ymin = value.center - value.se),
                             color=colors, fill=colors, alpha = alpha[2])
      } else if (tolower(what[["var"]])=="mm" || tolower(what[["var"]])=="minmax") {
        p <- p + geom_ribbon(aes(ymax = value.max, ymin = value.min),
                             color=colors, fill=colors, alpha = alpha[2])
      }
    }
  }
  if (!any(colnames(dt)=="sens")) {
    p <- p + geom_line(size=1, alpha=alpha[1], color=colors)
  } else {
    p <- p + geom_line(size=1, alpha=alpha[1])
  }
  lat <- .ll.breaks(c(min(dt$Lat), max(dt$Lat)), label="lat", n.min=4, n.max=9)
  p <- p + scale_x_continuous(breaks=lat, labels=names(lat), expand=c(0,0))
  if (!is.na(colors) && any(colnames(dt)=="sens")) {
    p <- p + scale_fill_manual(values=colors, guide=guide_legend(ncol=3))
    p <- p + scale_color_manual(values=colors, guide=guide_legend(ncol=3))
  } else {
    p <- p + scale_fill_manual(values=brewer.pal(length(unique(dt$sens)), "Set1"), guide=guide_legend(ncol=3))
    p <- p + scale_color_manual(values=brewer.pal(length(unique(dt$sens)), "Set1"), guide=guide_legend(ncol=3))
  }
  p <- p + xlab("Latitude")
  
  p <- p + ylab(units)
  p <- p + coord_flip()
  return(p)
}

#######################################################################
## categorial aggregated scatter ######################################
#######################################################################
#' Plot a categorically aggregated summary.
#' 
#' Plots vertical lines as average over latitudinal bands.
#' 
#' @param input A VegSpatial or a list of VegSpatial objects.
#' @param targets data.frame/list, either specifying x/y columns as c('VegSpatial name', 'column name') or slot/column columns as c('x', 'y').
#' @param name.map a named vector, to translate numerical y-data into human understandable names
#' @param area.weighted weight the mean by the gridcell area (default: TRUE) or not.
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param vertical boolean: values on the y-axis, categories on the x-axis
#' @param bar boolean: use bars instead of points
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @examples See templates/Example.ggplot.R
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @import RColorBrewer ggplot2 data.table
plotGGCategorialAggregated <- function(input, targets=NULL, name.map=NA, area.weighted=TRUE, long.title=TRUE, vertical=FALSE, bar=FALSE, plot=TRUE, ...) {
  if (is.null(targets)) {
    stop("Don't know what to do, if you are not telling me!")
  } else if (is.list(targets)) {
    targets <- as.data.frame(targets, stringsAsFactors=FALSE)
  }

  if (colnames(targets)[1] == "x" && colnames(targets)[2] == "y")
    targets <- .transpose.df(targets)

  if (ncol(targets)==2 && ncol(targets)==2) {
    colnames(targets) <- c("slot", "column")
    rownames(targets) <- c("x", "y")
  } else {
    stop("Don't know what to do, if you are not telling me!")
  }

  ## make sure to have chars instead of factors
  for (i in 1:2)
    targets[, i] = as.character(targets[, i])  
  
  if (is.VegRun(input)) {
    if (all(names(input@spatial) != targets$slot[1]))
      stop(paste("No VegSpatial object'", targets$slot[1], "' present in input!"))
    if (all(names(input@spatial) != targets$slot[2]))
      stop(paste("No VegSpatial object'", targets$slot[2], "' present in input!"))

    vsx <- eval(parse(text=paste("input@spatial[['",targets$slot[1],"']]", sep="")))
    if (all(colnames(vsx@data) != targets$column[1]))
      stop(paste("No column named '",targets$column[1],"' present in VegSpatial ",targets$slot[1]," of input run!", sep=""))
    units <- vsx@quant@units
    vsy <- eval(parse(text=paste("input@spatial[['",targets$slot[2],"']]", sep="")))
    if (all(colnames(vsy@data) != targets$column[2]))
      stop(paste("No column named '",targets$column[2],"' present in VegSpatial ",targets$slot[2]," of input run!", sep=""))

    if (area.weighted && any(colnames(vsx@data) == "area")) {
      dx <- vsx@data[, c("Lon", "Lat", targets$column[1], "area"), with = FALSE]
      dy <- vsy@data[, c("Lon", "Lat", targets$column[2]), with = FALSE]
    } else if (area.weighted && any(colnames(vsy@data) == "area")) {
      dx <- vsx@data[, c("Lon", "Lat", targets$column[1]), with = FALSE]
      dy <- vsy@data[, c("Lon", "Lat", targets$column[2], "area"), with = FALSE]
    } else if (area.weighted) {
      vsy <- addArea(vsy)
      dx <- vsx@data[, c("Lon", "Lat", targets$column[1]), with = FALSE]
      dy <- vsy@data[, c("Lon", "Lat", targets$column[2], "area"), with = FALSE]
    }
    print(str(dx))
    print(str(dy))
    dt <- dy[dx]
    setnames(dt, targets$column[1], "value")
    setnames(dt, targets$column[2], "category")
    if (area.weighted) {
      dt <- eval(parse(text=paste("dt[, list(value=weighted.mean(value, area, na.rm=TRUE)), by=list(category)]", sep="")))
    } else {
      dt <- eval(parse(text=paste("dt[, list(value=mean(value, na.rm=TRUE)), by=list(category)]", sep="")))
    }
  } else if (is.list(input)) {
    for (n in 1:length(input)) {
      if (is.VegRun(input[[n]])) {
        if (all(names(input[[n]]@spatial) != targets$slot[1]))
          stop(paste("No VegSpatial object'", targets$slot[1], "' present in input ", n, "!", sep=""))
        if (all(names(input[[n]]@spatial) != targets$slot[2]))
          stop(paste("No VegSpatial object'", targets$slot[2], "' present in input ", n, "!", sep=""))
    
        vsx <- eval(parse(text=paste("input[[n]]@spatial[['",targets$slot[1],"']]", sep="")))
        if (all(colnames(vsx@data) != targets$column[1]))
          stop(paste("No column named '",targets$column[1],"' present in VegSpatial ",targets$slot[1]," of input run ", n,"!", sep=""))
        units <- vsx@quant@units
    
        vsy <- eval(parse(text=paste("input[[n]]@spatial[['",targets$slot[2],"']]", sep="")))
        if (all(colnames(vsy@data) != targets$column[2]))
          stop(paste("No column named '",targets$column[2],"' present in VegSpatial ",targets$slot[2]," of input run ", n, "!", sep=""))

        if (n==1) {
          if (area.weighted && any(colnames(vsx@data) == "area")) {
            dx <- vsx@data[, c("Lon", "Lat", targets$column[1], "area"), with = FALSE]
            dy <- vsy@data[, c("Lon", "Lat", targets$column[2]), with = FALSE]
          } else if (area.weighted && any(colnames(vsy@data) == "area")) {
            dx <- vsx@data[, c("Lon", "Lat", targets$column[1]), with = FALSE]
            dy <- vsy@data[, c("Lon", "Lat", targets$column[2], "area"), with = FALSE]
          } else if (area.weighted) {
            vsy <- addArea(vsy)
            dx <- vsx@data[, c("Lon", "Lat", targets$column[1]), with = FALSE]
            dy <- vsy@data[, c("Lon", "Lat", targets$column[2], "area"), with = FALSE]
          }
          dt <- dy[dx]
          if (long.title) {
            dt[, sens:=vsx@run@description, ]
            titles <- vsx@run@description
          } else {
            dt[, sens:=vsx@run@id, ]
            titles <- vsx@run@id
          }
        } else {
          if (area.weighted && any(colnames(vsx@data) == "area")) {
            dx <- vsx@data[, c("Lon", "Lat", targets$column[1], "area"), with = FALSE]
            
            dy <- vsy@data[, c("Lon", "Lat", targets$column[2]), with = FALSE]
          } else if (area.weighted && any(colnames(vsy@data) == "area")) {
            dx <- vsx@data[, c("Lon", "Lat", targets$column[1]), with = FALSE]
            dy <- vsy@data[, c("Lon", "Lat", targets$column[2], "area"), with = FALSE]
          } else if (area.weighted) {
            vsy <- addArea(vsy)
            dx <- vsx@data[, c("Lon", "Lat", targets$column[1]), with = FALSE]
            dy <- vsy@data[, c("Lon", "Lat", targets$column[2], "area"), with = FALSE]
          }
          dt.tmp <- dy[dx]
          if (long.title) {
            dt.tmp[, sens:=vsx@run@description, ]
            titles <- append(titles, vsx@run@description)
          } else {
            dt.tmp[, sens:=vsx@run@id, ]
            titles <- append(titles, vsx@run@id)
          }
          dt <- rbindlist(list(dt, dt.tmp))
          rm(dt.tmp)
        }
      }     
    }
    dt <- dt[, sens:=factor(sens, titles)]
    setnames(dt, targets$column[1], "value")
    setnames(dt, targets$column[2], "category")
    if (area.weighted) {
      dt <- eval(parse(text=paste("dt[, list(value=weighted.mean(value, area)), by=list(category, sens)]", sep="")))
    } else {
      dt <- eval(parse(text=paste("dt[, list(value=mean(value)), by=list(category, sens)]", sep="")))
    }
  } else {
    stop("'input' must either be a RCVTools::VegRun or a list of them!")
  }
  rm(dx, dy)
  
  ## if name.map is a valid named vector the names are used for x-labels
  if (!any(is.na(name.map))) {
    if (is.vector(name.map) && !is.null(names(name.map))) {
      eval(parse(text=paste("dt[, name:=names(name.map)[category], ]", sep="")))
    } else if (is.vector(name.map)) {
      eval(parse(text=paste("dt[, name:=name.map[category], ]", sep="")))
    }
  }

  if (!plot)
    return(dt)

  if (!bar) {
    if (any(colnames(dt) == "sens")) {
      if (any(colnames(dt) == "name")) {
        p <- ggplot(dt, aes(x=value, y=name, col=sens))
      } else {
        p <- ggplot(dt, aes(x=value, y=category, col=sens))
      }
      p <- p + geom_point(size=2.5, position = position_jitter(w = 0, h = 0.2))
      p <- p + scale_color_manual(values=brewer.pal(length(unique(dt$sens)), "Set1"),
                                  na.value="grey", guide=guide_legend(ncol=2))
    } else {
      if (any(colnames(dt) == "name")) {
        p <- ggplot(dt, aes(x=value, y=name))
      } else {
        p <- ggplot(dt, aes(x=value, y=category))
      }
      p <- p + geom_point(size=2.5)
    }
    p <- p + xlab(units) + ylab("")
    p <- p + guides(colour = guide_legend(override.aes = list(size=4), ncol=2))
  } else {
    if (any(colnames(dt) == "sens")) {
      if (any(colnames(dt) == "name")) {
        p <- ggplot(dt, aes(x=name, y=value, fill=sens))
      } else {
        p <- ggplot(dt, aes(x=category, y=value, fill=sens))
      }
      p <- p + scale_fill_manual(values=brewer.pal(length(unique(dt$sens)), "Set1"),
                                 na.value="grey", guide=guide_legend(ncol=2))
      p <- p + geom_bar(position="dodge", stat="identity")
    } else {
      if (any(colnames(dt) == "name")) {
        p <- ggplot(dt, aes(x=name, y=value))
      } else {
        p <- ggplot(dt, aes(x=category, y=value))
      }
      
      p <- p + geom_bar(position="dodge", stat="identity")
      
    }
    p <- p + xlab("") + ylab(units)
  }
  p <- p + .rvc.scatter_theme
  if ((vertical && !bar) || (!vertical && bar))
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

#######################################################################
## timeseries #########################################################
#######################################################################
#' Plot a timeseries 
#' 
#' Plot a timeseries with ggplot. Either as line default, overlaying area,
#' applicable p.e. for height or stacked polygons (e.g. for succession).
#' 
#' @param input a VegTemporal object or a list of several.
#' @param columns The column(s) to display. Default: 'value'.
#' @param scale a scaling factor for multiplication
#' @param colors Colors for the diffent VegSpatial objects or columns. Must have the same length otherwise colors are choosing automatically.
#' @param type "line", overlayed "area" or "stack"ed area
#' @param wrap if input is a list and columns has more than one element use "sens" or "column" fro wrapping intp several panels.
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param lty Only used if type="line" and if input is a list and columns has more than one element and no wrapping is desired, use either "sens" or "column" for different line types.
#' @param alpha alpha value for plots
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @export
#' @import RColorBrewer ggplot2 
#' 
plotGGTemporal <- function(input, columns='value', scale=1., colors=NA, type="line", wrap=NA, long.title=TRUE, lty="sens", alpha=NA, plot=TRUE, ...) {
  if (is.VegObject(input, temporal=TRUE)) {
    if (is.na(columns) && all(colnames(input@data) != "value"))
      stop("No columns name given and no column named 'value' present!")

    if (!is.na(columns) && length(columns)>1) {
      for (cn in columns) {
        if (all(colnames(input@data)!=cn))
          stop(paste("No column named '", cn, "' present!", sep=""))
      }
    } else if (all(colnames(input@data) != columns)) {
      stop(paste("No column named '",columns,"' present!", sep=""))
    }
    dt <- input@data[, c("Year", columns), with = FALSE]
    dt <- melt(dt, "Year", columns)
    if (length(columns)>1) {
      dt$variable <- as.character(dt$variable)
      dt$variable <- factor(dt$variable, levels=columns)
    }
    quant = input@quant
  } else if (is.list(input)) {
    for (i in 1:length(input)) {
      if (!is.VegObject(input[[i]], temporal=TRUE))
        stop("'input' must either be a temporal RCVTools::VegObject or a list of them!")
      if (i==1) {
        if (is.na(columns) && all(colnames(input[[i]]@data) != "value")) {
          stop("No column name given and no column named 'value' present!")
          columns <- "value"
        } else {
          for (cn in columns) {
            if (all(colnames(input[[i]]@data)!=cn))
              stop(paste("No column named '", cn, "' present!", sep=""))
          }
        }
        dt <- input[[i]]@data[, c("Year", columns), with = FALSE]
        quant = input[[i]]@quant
        if (long.title) {
          dt[, sens:=input[[i]]@run@description, ]
          titles <- input[[i]]@run@description
        } else {
          dt[, sens:=input[[i]]@run@id, ]
          titles <- input[[i]]@run@id
        }
      } else {
        for (cn in columns) {
          if (all(colnames(input[[i]]@data)!=cn))
            stop(paste("No column named '", cn, "' present!", sep=""))
        }
        dt.tmp <- input[[i]]@data[, c("Year", columns), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@run@description, ]
          titles <- append(titles, input[[i]]@run@description)
        } else {
          dt.tmp[, sens:=input[[i]]@run@id, ]
          titles <- append(titles, input[[i]]@run@id)
        }
        dt <- rbindlist(list(dt, dt.tmp))
        rm(dt.tmp)
      }
    }
    dt <- dt[, sens:=factor(sens, titles)]
    dt <- melt(dt, c("Year", "sens"), columns)
    if (length(columns)>1) {
      dt$variable <- as.character(dt$variable)
      dt$variable <- factor(dt$variable, levels=columns)
    }
  } else {
    stop("'input' must either be a RCVTools::VegTemporal or a list of them!")
  }
  
  dt[,value:=value*scale]
  
  if (!plot)
    return(dt)
  
  if (any(!is.finite(dt$value)))
    warning("Data contains infinite values. This might mess up the plot!")
  
  if (any(!.is.color(colors)) || any(is.na(colors)))
    colors="black"

  p <- ggplot(dt, aes(x=Year, y=value))
  p <- p + .rvc.temporal_theme
  if (grepl("^l", type)) {
    if (!is.numeric(alpha))
      alpha=1
    if (length(columns)==1 && length(input)==1) {
      p <- p + geom_line(color=colors, size=1)
    } else {
      if (length(columns)>1 && length(input)==1) {
        p <- p + geom_line(aes(col=variable), alpha=alpha, size=1)
        if (length(colors)!=length(unique(dt$variable)))
          colors <- brewer.pal(length(unique(dt$variable)), "Set1")
      } else if (length(columns)==1 && length(input)>1) {
        p <- p + geom_line(aes(col=sens), alpha=alpha, size=1)
        if (length(colors)!=length(unique(dt$sens)))
          colors <- brewer.pal(length(unique(dt$sens)), "Set1")
      } else {
        if (grepl("^c", wrap) || grepl("^v", wrap)) {
          p <- p + geom_line(aes(col=sens), alpha=alpha, size=1)
          p <- p + facet_wrap(~variable, ncol=1) 
          if (length(colors)!=length(unique(dt$sens)))
            colors <- brewer.pal(length(unique(dt$sens)), "Set1")
        } else if (grepl("^r", wrap) || grepl("^s", wrap)) {
          p <- p + geom_line(aes(col=variable), alpha=alpha)
          p <- p + facet_wrap(~sens, ncol=1)
          if (length(colors)!=length(unique(dt$variable)))
            colors <- brewer.pal(length(unique(dt$variable)), "Set1")
        } else {
          if (grepl("^v", lty) || grepl("^c", lty)) {
            p <- p + geom_line(aes(col=sens, lty=variable), alpha=alpha)
            if (length(colors)!=length(unique(dt$sens)))
              colors <- brewer.pal(length(unique(dt$sens)), "Set1")
          } else {
            p <- p + geom_line(aes(col=variable, lty=sens), alpha=alpha)
            if (length(colors)!=length(unique(dt$variable)))
              colors <- brewer.pal(length(unique(dt$variable)), "Set1")
          }
        }
      }
      p <- p + scale_color_manual(values=colors, guide=guide_legend(title="", ncol=length(columns), override.aes=list(size=2)))
    }
  } else {
    if (grepl("^a", type)) {
      if (!is.numeric(alpha))
        alpha=1/length(columns)
    } else {
      if (!is.numeric(alpha))
        alpha=1
    }
    if (length(input)==1 && length(columns)==1) {
      p <- p + geom_area(alpha=alpha, fill=colors, position="stack")
    } else {
      if (length(input)==1 && length(columns)>1) {
        if (grepl("^a", type)) {
          p <- p + geom_area(aes(fill=variable), alpha=alpha, position = position_dodge(width=0))
        } else {
          p <- p + geom_area(aes(fill=variable), alpha=alpha, position = "stack")
        }
        if (length(colors)!=length(unique(dt$variable)))
          colors <- brewer.pal(length(unique(dt$variable)), "Set1")
      } else if (length(input)>1 && length(columns)==1) {
        if (grepl("^a", type)) {
          p <- p + geom_area(aes(fill=sens), alpha=alpha, position = position_dodge(width=0))
        } else {
          warning("Stacking runs above each other. Is this really what you want?")
          p <- p + geom_area(aes(fill=sens), alpha=alpha, position = "stack")
        }
        if (length(colors)!=length(unique(dt$sens)))
          colors <- brewer.pal(length(unique(dt$sens)), "Set1")
      } else {
        if (grepl("^c", wrap) || grepl("^v", wrap)) {
          if (grepl("^a", type)) {
            p <- p + geom_area(aes(fill=sens), alpha=alpha, position = position_dodge(width=0))
          } else {
            if (length(input)>1)
              warning("Stacking runs above each other. Is this really what you want?")
            p <- p + geom_area(aes(fill=sens), alpha=alpha, position = "stack")
          }
          if (length(colors)!=length(unique(dt$sens)))
            colors <- brewer.pal(length(unique(dt$sens)), "Set1")
        } else {
          if (grepl("^a", type)) {
            p <- p + geom_area(aes(fill=variable), alpha=alpha, position = position_dodge(width=0))
          } else {
            p <- p + geom_area(aes(fill=variable), alpha=alpha, position = "stack")
          }
          if (length(colors)!=length(unique(dt$variable)))
            colors <- brewer.pal(length(unique(dt$variable)), "Set1")
        }
        if (grepl("^c", wrap) || grepl("^v", wrap)) {
          p <- p + facet_wrap(~variable, ncol=1) 
        } else if (grepl("^r", wrap) || grepl("^s", wrap)) {
          p <- p + facet_wrap(~sens, ncol=1)
        }
      }
      p <- p + scale_fill_manual(values=colors, guide=guide_legend(title=""))
    }
  }

  p <- p + scale_x_continuous(expand=c(0, 0))
  if (grepl("^s", type) || grepl("^a", type)) 
    p <- p + scale_y_continuous(limits=c(0, NA), expand=c(0, 0))
  p <- p + ylab(paste(quant@full.string, " [", quant@units,"]", sep=""))

  return(p)
}

#' Plot a histogram
#' 
#' Plot the given columns as histogram
#' 
#' @param input a VegSpatial object or a list of several.
#' @param column The column(s) to display. Default: 'value'.
#' @param colors Colors for the diffent VegSpatial objects or columns. Must have the same length otherwise colors are choosing automatically.
#' @param bins number of bins for histogram (default: 25)
#' @param bars draw bars of counted occurence (default TRUE).
#' @param lines also draw density lines (default FALSE).
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @import RColorBrewer
#' @export
plotGGHist <- function(input, column='value', colors=NA, bars=TRUE, lines=FALSE, bins=10, long.title=TRUE, plot=TRUE, ...) {
  ## check if a VegSpatial or a list of VegSpatial is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegObject(input, spatial=TRUE)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (!is.na(column) && length(column)>1) {
      for (cn in column) {
        if (all(colnames(input@data)!=cn))
          stop(paste("No column named '", cn, "' present!", sep=""))
      }
    } else if (all(colnames(input@data) != column)) {
      stop(paste("No column named '",column,"' present!", sep=""))
    }
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column)==1) {
      setnames(dt, column, "value")
    } else {
      dt <- melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
    }
  } else if (is.list(input)) {
    for (i in 1:length(input)) {
      if (!is.VegObject(input[[i]], spatial=TRUE))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      
      if (is.na(column) && all(colnames(input[[i]]@data) != "value"))
        stop("No column name given and no column named 'value' present!")
      if (!is.na(column) && length(column)>1) {
        warning("Several column names are supplied. Will only use the first one!")
        column <- column[1]
      }
      if (all(colnames(input[[i]]@data) != column)) {
        stop(paste("No column named '",column,"' present!", sep=""))
      }
      
      if (i==1) {
        dt <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[i]]@run@description, ]
          titles <- input[[i]]@run@description
        } else {
          dt[, sens:=input[[i]]@run@id, ]
          titles <- input[[i]]@run@id
        }
      } else {
        dt.tmp <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@run@description, ]
          titles <- append(titles, input[[i]]@run@description)
        } else {
          dt.tmp[, sens:=input[[i]]@run@id, ]
          titles <- append(titles, input[[i]]@run@id)
        }
        dt <- rbindlist(list(dt, dt.tmp))
        rm(dt.tmp)
      }
    }
    setnames(dt, column, "value")
    dt <- dt[, sens:=factor(sens, titles)]
  } else {
    stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
  }

  if (is.na(bins))
    bins=10

  if (any(colnames(dt)=="sens") && is.na(colors))  {
    colors <- brewer.pal(length(unique(dt$sens)), "Set1")
  } else if (any(colnames(dt)=="sens") && length(colors)!=length(unique(dt$sens))) {
    colors <- brewer.pal(length(unique(dt$sens)), "Set1")
  } else if (is.na(colors)) {
    colors="#333333"
  }

  warning("Finding breaks is currently not properly implemented!")
  brks <- seq(floor(min(dt$value)), ceiling(max(dt$value)), length=bins+1)
  
  lbls <- paste(brks[1:(length(brks)-1)]+(brks[2:length(brks)] - brks[1:(length(brks)-1)])/2)
  dt[,bin:=findInterval(value, brks, rightmost.closed = TRUE)]
  
  if (any(colnames(dt)=="sens")) {
    binned.dt <- dt[, list(.N), by=.(bin, sens)]
    p <- ggplot(binned.dt, aes(x=bin, y=N, fill=sens))
    p <- p + scale_fill_manual(values=colors)
  } else {
    binned.dt <- dt[, list(.N), by=bin]
    p <- ggplot(binned.dt, aes(x=bin, y=N))
  }
  
  if (!plot)
    return(binned.dt)

  p <- p + rvc.ggplot.theme("scatter")
  if (bars)
    p <- p + geom_bar(stat="identity", position="dodge")
  if (any(colnames(dt)=="sens") && lines) {
    p <- p + geom_line(aes(color=sens), size=1)
    p <- p + scale_color_manual(values=colors)
  } else if (lines) {
    p <- p + geom_line(color=colors, size=1)
  }
  p <- p + xlab("")
  p <- p + scale_x_continuous(breaks=seq(0.5, bins+0.5, 1), labels=brks, expand=c(0.01,0))
  p <- p + scale_y_continuous(expand=c(0,0))
  p <- p + theme(panel.grid.minor.x = element_line(NA))
  return(p)
}