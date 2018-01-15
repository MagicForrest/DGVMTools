#######################################################################
## ggplot2 themes for nicer plots #####################################
#######################################################################

.dgvm.spatial_theme <- list(theme(panel.grid.minor = element_line(size=0.1, colour = "black", linetype = "dotted"),
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

.dgvm.scatter_theme <- list(theme(panel.grid.minor = element_line(size=0.1, colour = "black", linetype = "dotted"),
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
                                 plot.title        = element_text(size=22)))
#                                 strip.background  = element_rect(fill=NA)))

.dgvm.temporal_theme <- list(theme(panel.grid.minor = element_line(size=0.1, colour = "black", linetype = "dotted"),
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
dgvm.ggplot.theme <- function(x) {
  if(x=="scatter") {
    return(.dgvm.scatter_theme)
  } else if (x=="temporal" || x=="ts" || x=="timeseries") {
    return(.dgvm.temporal_theme)
  } else if (x=="spatial" || x=="map") {
    return(.dgvm.spatial_theme)
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
    warning(paste0("Did not find suitable levels between ", n.min, " and ", n.max, ", used ", length(lab), " instead."))

  if (!is.na(label)) {
    if (tolower(label)=="lon" || label=="long" || label=="longitude") {
      ## sprintf("%X", as.integer(charToRaw("-was a degree sign-"))) => [1] "C2" "B0"
      ## paste0("\u00B0") => "-was a degree sign-"
      names.lab <- lab
      names.lab[lab < 0]  = paste0(abs(lab[lab < 0]), "\u00B0W")
      names.lab[lab >= 0] = paste0(abs(lab[lab >= 0]), "\u00B0E")
      names(lab) <- names.lab
    } else if (tolower(label)=="lat" || label=="latitude") {
      names.lab <- lab
      names.lab[lab < 0]  = paste0(abs(lab[lab < 0]), "\u00B0S")
      names.lab[lab >= 0] = paste0(abs(lab[lab >= 0]), "\u00B0N")
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

## from http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-colour-representation
.is.colour <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(grDevices::col2rgb(X)), 
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
#' @param input A temporally averaged Field or a list of them.
#' @param column Name of the column(s) in the data slot of input, which should be plotted. Several columns can only be supplied with a single Field. Default: 'value'.
#' @param colours A (named) vector of colours or a data.frame with the columns 'colour' and ('name' or 'value'), to be used for plotting.
#' @param sym.col boolean, if the colours should be distributed symetrically around 0.
#' @param wrap a single number of facet_wrap columns or a vector/list with the run, VegSpatial name, column and optionally ncol (number of columns), which is used to split the data in different panels. Only valid when a vector of column names or a list of VegSpatial was given as input. Otherwise it is ignored.
#' @param miss.bg which colour should be used for missing pixels if wraped by another Field or column.
#' @param map.overlay if set to 'lowres' or 'highres' national borders are included (uses package maps). It can also be a data.frame as returned by \code{ggplot2::fortify}.
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Additional parameters, which are ignored so far.
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @examples message("See templates/Example.ggplot.R")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @importFrom raster raster projectRaster as.data.frame
#' @export
plotGGSpatial <- function(input, column='value', colours=NA, sym.col=FALSE, wrap=1, miss.bg=NA, map.overlay=NA, long.title=TRUE, plot=TRUE, ...) {
  ## to avoid "no visible binding for global variable" during check
  Lon = Lat = group = long = name = value = sens = NULL
  ## much simpler than the following Field
  if (is.Field(input)) {
    if (!all(is.na(column))) {
      if (length(column) == 1 && all(colnames(input@data) != column)) {
        stop(paste0("No column named '",column,"' present!"))
      } else {
        for (cn in column) {
          if (all(colnames(input@data)!=cn))
            stop(paste0("No column named '", cn, "' present!"))
        }
      }
    } else {
      column <- colnames(input@data)[which(colnames(input@data) != "Lon" & colnames(input@data) != "Lat")]      
    }
    london.centre <- TRUE
    if (max(input@data$Lon) > 180)
      london.centre = FALSE

    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column)==1) {
      setnames(dt, column, "value")
    } else {
      dt <- data.table::melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
      if (length(wrap) > 1 || !is.numeric(wrap))
        wrap <- ceiling(sqrt(length(column)))
    }

        
  ## check if a VegSpatial or a list of VegSpatial is given as input
  ## check data column names for given column name or column name 'value'
  } else if (is.Field(input, spatial=TRUE)) {
    if (all(is.na(column)) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (!all(is.na(column)) && length(column) > 1) {
      for (cn in column) {
        if (all(colnames(input@data) != cn))
          stop(paste0("No column named '", cn, "' present!"))
      }
    } else if (all(colnames(input@data) != column)) {
      stop(paste0("No column named '",column,"' present!"))
    }
    london.centre <- input@source@london.centre
    ##if (is.character(input@source@map.overlay)) {
    ##  map.overlay <- input@map.overlay
    ##} else if (is.null(input@source@map.overlay)) {
    ##  map.overlay <- "lowres"
    ##} else {
    ##  map.overlay <- fortify(SpatialLinesDataFrame(input@source@map.overlay[[2]],
    ##                                               data.frame(ID=getSLLinesIDSlots(input@source@map.overlay[[2]]))))
    ##}
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column) == 1) {
      setnames(dt, column, "value")
    } else {
      dt <- data.table::melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
      if (length(wrap) > 1 || !is.numeric(wrap))
        wrap <- ceiling(sqrt(length(column)))
    }
    if (length(wrap) > 1) {
      if (!is.list(wrap))
        wrap <- list(name=wrap[1], column=wrap[2],
                    ncol={if (length(wrap) >=3 ) wrap[3] else 1},
                     map=NA, stringsAsFactors=FALSE)
    
      if (!eval(parse(text=paste0("is.Field(",wrap$name, ", spatial=TRUE)"))))
        stop(paste0("'", wrap$name, "' is not a spatial Field"))
    
      dt.wrap <- eval(parse(text=paste0(wrap$name, "@data[, c('Lon', 'Lat', '", wrap$column,"'), with=FALSE]")))
      setnames(dt.wrap, wrap$column, "wrap.tmp")
      dt <- dt[dt.wrap]
    
      ## if wrap$map is a valid named vector the names are used for x-labels
      if (!any(is.na(wrap$map))) {
        if (is.vector(wrap$map) && !is.null(names(wrap$map))) {
          eval(parse(text=paste0('dt[, name:=names(wrap$map)[wrap.tmp], ]')))
        } else if (is.vector(wrap$map)) {
          eval(parse(text=paste0('dt[, name:=wrap$map[wrap.tmp], ]')))
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
      if (!is.Field(input[[i]], spatial=TRUE))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      
      if (all(is.na(column)) && all(colnames(input[[i]]@data) != "value"))
        stop("No column name given and no column named 'value' present!")
      if (!all(is.na(column)) && length(column) > 1) {
        warning("Several column names are supplied. Will only use the first one!")
        column <- column[1]
      }
      if (all(colnames(input[[i]]@data) != column)) {
        stop(paste0("No column named '",column,"' present!"))
      }
      
      if (i==1) {
        london.centre <- input[[i]]@source@london.centre
        ##if (is.character(input[[i]]@source@map.overlay)) {
        ##  map.overlay <- input[[i]]@map.overlay
        ##} else if (is.null(input[[i]]@source@map.overlay)) {
        ##  map.overlay <- "lowres"
        ##} else {
        ##  ## did not find a way how to replace depricated "getSLLLinesIDSlots" by "over"
        ##  map.overlay <- fortify(SpatialLinesDataFrame(input[[i]]@source@map.overlay[[2]],
        ##                                               data.frame(ID=getSLLinesIDSlots(input[[i]]@source@map.overlay[[2]]))))
        ##}
        dt <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[i]]@source@name, ]
          titles <- input[[i]]@source@name
        } else {
          dt[, sens:=input[[i]]@source@id, ]
          titles <- input[[i]]@source@id
        }
      } else {
        dt.tmp <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@source@name, ]
          titles <- append(titles, input[[i]]@source@name)
        } else {
          dt.tmp[, sens:=input[[i]]@source@id, ]
          titles <- append(titles, input[[i]]@source@id)
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

  ## if colours is a valid named vector or a data.frame with columns 'colour' and 'name'
  ## plot discrete values instead of continuous
  discrete <- FALSE
  if (!any(is.na(colours))) {
    if (is.vector(colours) && !is.null(names(colours))) {
      dt[, name:=names(colours)[value], ]
      discrete <- TRUE
    } else if  (is.data.frame(colours) && any(names(colours)=="colour") && any(names(colours)=="name")) {
      dt[, name:=colours$name[value], ]
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

  ## if a valid colour for the land is requested
  if (!is.na(miss.bg) && .is.colour(miss.bg)) {
    if (any(colnames(dt) == "sens")) {
      slm <- data.table(Lon=dt$Lon, Lat=dt$Lat)
      setkey(slm, Lon, Lat)
      slm = unique(slm, by=c("Lon", "Lat"))
      setkey(slm, Lon, Lat)
      slm.grd = copy(slm)
      nonsense = as.character(unique(dt$sens))
      slm$sens = nonsense[1]
      for (i in 2:length(nonsense)) {
        slm = rbind(slm, slm.grd[, sens := nonsense[i]])
      }
      rm(nonsense, slm.grd)
    } else {
      slm <- data.table(Lon=dt$Lon, Lat=dt$Lat)
      setkey(slm, Lon, Lat)
      slm = unique(slm, by=c("Lon", "Lat"))
    }
    slm$value = 1
  } else {
    miss.bg <- NA
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

  if (wrap == 1) {
    lon <- .ll.breaks(lon[2:(length(lon)-1)], label="lon", n.min=4, n.max=3+ceiling(fr_lon*4))
    lat <- .ll.breaks(lat[2:(length(lat)-1)], label="lat", n.min=4, n.max=3+ceiling(fr_lat*4))
  } else {
    lon <- .ll.breaks(lon[2:(length(lon)-1)], label="lon", n.min=2, n.max=1+ceiling(fr_lon*2))
    lat <- .ll.breaks(lat[2:(length(lat)-1)], label="lat", n.min=2, n.max=1+ceiling(fr_lat*2))
  }
    
  ## create plot
  p <- ggplot(dt, aes(y=Lat, x=Lon))
  p <- p + .dgvm.spatial_theme

  if (!is.na(miss.bg))
    p <- p + geom_raster(data=slm, fill=miss.bg)

  if (discrete) {
    p <- p + geom_raster(aes(fill=name))
    if (is.vector(colours)) {
      p <- p + scale_fill_manual(values=colours, breaks=names(colours), na.value="grey", guide=guide_legend(ncol=3))
    } else if (is.data.frame(colours)) {
      p <- p + scale_fill_manual(values=colours$colour, breaks=colours$name, na.value="grey", guide=guide_legend(ncol=3))
    }
    p <- p + theme(legend.key.width  = unit(0.03, "npc"))
  } else {
    p <- p + geom_raster(aes(fill=value))
    if (sym.col) {
      p <- p + expand_limits(fill = c(-max(abs(dt$value), na.rm=TRUE), max(abs(dt$value), na.rm=TRUE)))
    }

    if (any(colnames(colours)=="colour") && any(colnames(colours)=="value")) {
      p <- p + scale_fill_gradientn(colours=colours$colour, values=colours$value, expand=c(0, 0))
      p <- p + guides(fill=guide_colourbar(nbin = 101, expand=c(0, 0)))
    } else if (!any(is.na(colours))) {
      p <- p + scale_fill_gradientn(colours=colours, expand=c(0, 0))
      p <- p + guides(fill=guide_colourbar(nbin = 101, expand=c(0, 0)))
    }
  }
  if (is.data.frame(map.overlay))
    p <- p + geom_path(data=map.overlay, size=0.1, colour = "black", aes(x=long, y=lat, group=group))
  p <- p + scale_x_continuous(breaks=lon, labels=names(lon), expand=c(0, 0))
  p <- p + scale_y_continuous(breaks=lat, labels=names(lat), expand=c(0, 0)) 
  p <- p + coord_fixed(xlim=lon.limit, ylim=lat.limit)
  p <- p + xlab("Longitude")
  p <- p + ylab("Latitude")

  if (any(colnames(dt)=="sens")) {
    p <- eval(parse(text=paste0("p + facet_wrap(~sens, ncol=",wrap,")")))
  } else if (long.title) {
    p <- p + labs(title=input@source@name)
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
#' @param colours colours for the diffent VegSpatial objects.
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @examples message("See templates/Example.ggplot.R")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @import ggplot2 data.table
#' @importFrom RColorBrewer RColorBrewer::brewer.pal
plotGGMeridional <- function(input, column='value', what=list(center="mn", var="sd"), alpha=c(0.8, 0.1), colours=NA, long.title=TRUE, plot=TRUE, ...) {
  ## to avoid "no visible binding for global variable" during check
  Lat = sens = value = value.center = value.sd = value.se = value.max = value.min = NULL

  if (!any(names(what)=="center"))
    stop("No definition for meridional average given. Must be either 'mn', 'mean' or 'md', 'median'.")
  if (!any(names(what)=="var"))
    what[["var"]] = NA
  
  ## check if a VegObj or a list of VegObj is given as input
  ## check data column names for given column name or column name 'value'
  if (is.Field(input, spatial=TRUE)) {
    if (all(is.na(column)) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    
    if (!all(is.na(column)) && length(column) > 1) {
      for (cn in column) {
        if (all(colnames(input@data)!=cn))
          stop(paste0("No column named '", cn, "' present!"))
      }
    } else if (all(colnames(input@data) != column)) {
      stop(paste0("No column named '",column,"' present!"))
    }
    
    units <- input@quant@units
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column)==1) {
      setnames(dt, column, "value")
    } else {
      dt <- data.table::melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
    }
  } else if (is.list(input)) {
    for (i in 1:length(input)) {
      if (!is.Field(input[[i]], spatial=TRUE))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      if (i==1) {
        units <- input[[i]]@quant@units
        dt <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[i]]@source@name, ]
          titles <- input[[i]]@source@name
        } else {
          dt[, sens:=input[[i]]@source@id, ]
          titles <- input[[i]]@source@id
        }
       } else {
        dt.tmp <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@source@name, ]
          titles <- append(titles, input[[i]]@source@name)
        } else {
          dt.tmp[, sens:=input[[i]]@source@id, ]
          titles <- append(titles, input[[i]]@source@id)
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
      md <- dt[, list(value.center=stats::median(value),
                      value.sd=stats::sd(value),
                      value.se=stats::sd(value)/sqrt(length(value)),
                      value.min=stats::quantile(value, qt[1]),
                      value.max=stats::quantile(value, qt[2])), by = c("Lat", "sens")]
    } else {
      md <- dt[, list(value.center=mean(value),
                      value.sd=stats::sd(value),
                      value.se=stats::sd(value)/sqrt(length(value)),
                      value.min=stats::quantile(value, qt[1]),
                      value.max=stats::quantile(value, qt[2])), by = c("Lat", "sens")]
    }
    p <- ggplot(md, aes(x=Lat, y=value.center, col=sens))
  } else {
    if (what[["center"]] == "md" || what[["center"]] == "median") {
      md <- dt[, list(value.center=stats::median(value),
                      value.sd=stats::sd(value),
                      value.se=stats::sd(value)/sqrt(length(value)),
                      value.min=stats::quantile(value, qt[1]),
                      value.max=stats::quantile(value, qt[2])), by = c("Lat")]
    } else {
      md <- dt[, list(value.center=mean(value),
                      value.sd=stats::sd(value),
                      value.se=stats::sd(value)/sqrt(length(value)),
                      value.min=stats::quantile(value, qt[1]),
                      value.max=stats::quantile(value, qt[2])), by = c("Lat")]
    }
    if (!is.character(colours) || !.is.colour(colours)) {
      colours="black"
    } 
    p <- ggplot(md, aes(x=Lat, y=value.center))
  }

  if (!plot)
    return(md)
  
  p <- p + .dgvm.scatter_theme

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
                             colour=colours, fill=colours, alpha = alpha[2])
      } else if (tolower(what[["var"]])=="se") {
        p <- p + geom_ribbon(aes(ymax = value.center + value.se,
                                 ymin = value.center - value.se),
                             colour=colours, fill=colours, alpha = alpha[2])
      } else if (tolower(what[["var"]])=="mm" || tolower(what[["var"]])=="minmax") {
        p <- p + geom_ribbon(aes(ymax = value.max, ymin = value.min),
                             colour=colours, fill=colours, alpha = alpha[2])
      }
    }
  }
  if (!any(colnames(dt)=="sens")) {
    p <- p + geom_line(size=1, alpha=alpha[1], colour=colours)
  } else {
    p <- p + geom_line(size=1, alpha=alpha[1])
  }
  lat <- .ll.breaks(c(min(dt$Lat), max(dt$Lat)), label="lat", n.min=4, n.max=9)
  p <- p + scale_x_continuous(breaks=lat, labels=names(lat), expand=c(0,0))
  if (!is.na(colours) && any(colnames(dt)=="sens")) {
    p <- p + scale_fill_manual(values=colours, guide=guide_legend(ncol=3))
    p <- p + scale_colour_manual(values=colours, guide=guide_legend(ncol=3))
  } else {
    p <- p + scale_fill_manual(values=RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1"), guide=guide_legend(ncol=3))
    p <- p + scale_colour_manual(values=RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1"), guide=guide_legend(ncol=3))
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
#' @param x A Field or a list of them.
#' @param cat A Field containing the categories to summarize 'x' or a list of them.
#' @param x.col.name The column name of 'x' data.table to use. Default: 'value'.
#' @param cat.col.name The column name of the grouping variable. Should be either of type character, factor or integer.
#' @param name.map a named vector, to translate numerical category-data into human understandable names.
#' @param area.weighted weight the mean by the gridcell area (default: TRUE) or not.
#' @param vertical boolean: values on the y-axis, categories on the x-axis
#' @param bar boolean: use bars instead of points.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters.
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @examples message("See templates/Example.ggplot.R")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
#' @export
#' @import ggplot2 data.table
#' @importFrom RColorBrewer RColorBrewer::brewer.pal
## TODO: Calculate not only means but also sums (with and without area)
plotGGCategorialAggregated <- function(x, cat, x.col.name="value", cat.col.name="category", name.map=NA, area.weighted=TRUE, vertical=FALSE, bar=FALSE, plot=TRUE, ...) {
  ## to avoid "no visible binding for global variable" during check
  Lon = Lat = sens = value = name = category = NULL
  if (missing(x) && missing(cat))
    stop("Don't know what to do, if you are not telling me!")
 
  melt.dt <- function(x, col.name, vn) {
    if (is.Field(x)) {
      if (all(colnames(x@data) != col.name))
        stop(paste0("No column named '", col.name, "' present in Field '", vn, "'!"))
      if (area.weighted && any(colnames(x@data) == "area")) {
        dx <- x@data[, c("Lon", "Lat", col.name, "area"), with = FALSE]
      } else {
        dx <- x@data[, c("Lon", "Lat", col.name), with = FALSE]
      }
      setkey(dx, Lon, Lat)
    } else if (is.list(x)){ 
      for (n in 1:length(x)) {
        if (is.Field(x[[n]])) {
          if (all(colnames(x[[n]]@data) != col.name))
            stop(paste0("No column named '", col.name, "' in input ", n, " of '", vn, "'!"))
          if (n == 1) {
            if (area.weighted && any(colnames(x[[n]]@data) == "area")) {
              dx <- x[[n]]@data[, c("Lon", "Lat", col.name, "area"), with = FALSE]
            } else {
              dx <- x[[n]]@data[, c("Lon", "Lat", col.name), with = FALSE]
            }
            dx[, sens := x[[n]]@source@id]
          } else {
            if (area.weighted && any(colnames(x[[n]]@data) == "area")) {
              dx.tmp <- x[[n]]@data[, c("Lon", "Lat", col.name, "area"), with = FALSE]
            } else {
              dx.tmp <- x[[n]]@data[, c("Lon", "Lat", col.name), with = FALSE]
            }
            dx.tmp[, sens := x[[n]]@source@id]
            dx = rbind(dx, dx.tmp)
          }
        } else {
          stop(paste0("Element ", n, " is not a Field in list '", vn, "'!"))
        }
      }
      setkey(dx, Lon, Lat, sens)
    }
    return(dx)
  }
  dx <- melt.dt(x, x.col.name, "x")
  setnames(dx, x.col.name, "value")
  dcat <- melt.dt(cat, cat.col.name, "cat")
  setnames(dcat, cat.col.name, "category")

  if (is.list(x)) {
    dt = dx[dcat]
  } else {
    dt <- dcat[dx]
  }

  if (is.Field(x)) {
    units = x@quant@units
  } else if (is.list(x)) {
    units = x[[1]]@quant@units
  }
  
  if (any(colnames(dt) == "sens")) {
    class.cols = c("category", "sens")
  } else {
    class.cols = c("category")
  }
  
  if (area.weighted) {
    if (all(colnames(dt) != "area"))
      dt = addArea(dt)
    dt <- eval(parse(text=paste0("dt[, list(value=stats::weighted.mean(value, area, na.rm=TRUE)), by=list(", paste(class.cols, collapse = ", "), ")]")))
  } else {
    dt <- eval(parse(text=paste0("dt[, list(value=mean(value, na.rm=TRUE)), by=list(", paste(class.cols, collapse = ", "), ")]")))
  }

  if (!any(is.na(name.map))) {
    if (is.vector(name.map) && !is.null(names(name.map))) {
      eval(parse(text=paste0("dt[, name := names(name.map)[category], ]")))
    } else if (is.vector(name.map)) {
      eval(parse(text=paste0("dt[, name := name.map[category], ]")))
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
      p <- p + geom_point(size=2.5, position = position_jitter(width = 0, height = 0.2))
      p <- p + scale_colour_manual(values=RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1"),
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
      p <- p + scale_fill_manual(values=RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1"),
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
  p <- p + .dgvm.scatter_theme
  if ((vertical && !bar) || (!vertical && bar))
    p <- p + coord_flip()
  
  return(p)  
}


#######################################################################
## scatter ############################################################
#######################################################################

#' Create a various forms of scatterplots
#' 
#' Create a point cloud (scatterplot) or density plot with several metrics and line fits.
#' 
#'
#' @param x A Field or a list of them.
#' @param y A Field, dataframe/data.table or raster.
#' @param x.column Column name of x to use for the scatter.
#' @param y.column Column name of y to use for the scatter.
#' @param limit A data.frame with the possible columns min, max, xmin, xmax, ymin, ymax to exclude data from x and/or y. If a data.column 'inclusive' is present and TRUE, the given values will remain in the data.
#' @param flip Should x and y be on the other axis. Default TRUE, means x is on the y-axis and y on the x-axis.
#' @param density Use a ggplot density_2d function instead of a point cloud. Values: 'polygon', 'raster' and everything else than FALSE will result in 'hexagonal'.
#' @param wrap.column A column name in x, which should be used for faceting.
#' @param colour.column A column name for colouring the points (NOT working currently).
#' @param colours A data.frame for colours to use mandatory column names: XXX
#' @param sym.col Should the colours be symmetrical (NOT working currently).
#' @param alpha transparency value for points (ignored in density mode).
#' @param lines Character vector of any of '1:1', 'lm' and/or 'gam'. Or a data.frame with the columns 'lines', 'colour', 'type', 'width'.
#' @param labels Character string of metrics to be added. Possible values 'mae', 'rmsd', 'nme', 'nmse', 'me', 'eq', 'rsq', 'rmse'
#' @param label.pos Position of the labels as character consiting 'top' or 'bottom' and 'left', 'center', 'right'
#' @param equal.axis Should the x and y- axis be scaled equally.
#' @param wrap number of column, when wrapping
#' @param plot Should a ggplot-object be returned (default) or the data.table.
#' @param verbose print some stupid messages.
#'
#' @return a data.table or a ggplot2-object.
#' @export
#' @import data.table
#' @importFrom raster isLonLat
#'
#' @references Mayer, D. G. and Butler, D. G.: Statistical validation, Ecological Modelling, 68(1-2), 21-32, \href{https://dx.doi.org/10.1016/0304-3800(93)90105-2}{doi:10.1016/0304-3800(93)90105-2}, 1993.
#' Kelley, et al.: A comprehensive benchmarking system for evaluating global vegetation models, Biogeosciences, 10(5), 3313-3340, \href{https://dx.doi.org/10.5194/bg-10-3313-2013}{doi:10.5194/bg-10-3313-2013, 2013.}
#' 
#' @examples message("See templates/Example.ggplot.R")
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
plotGGScatter <- function(x, y, x.column="value", y.column="value", limit=NULL,
                          flip=TRUE, density=FALSE, wrap.column=NULL,
                          colour.column=NULL, colours="orange", sym.col=FALSE, alpha=0.1,
                          lines=NA,
                          labels=NA, label.pos="bottomright",
                          equal.axis=TRUE,
                          wrap=2,
                          plot=TRUE, verbose=TRUE) {
  ..density.. = ..level.. = Lat = Lon = Year = sens = c.value = w.value = x.value = y.value = NULL
  spatial  <- FALSE
  temporal <- FALSE

  DT <- NULL
  if (is.list(x)) {
    for (i in 1:length(x)) {
      if (!is.Field(x[[i]], spatial=TRUE))
        stop(paste0("'x[[", i, "]]' must be a DGVMTools::Field!"))
      if (all(colnames(x[[i]]@data) != x.column))
        stop(paste0("'", x.column, "' not present in list object no. ", i, "!"))
      if (any(colnames(x[[i]]@data) == "Lon") && any(colnames(x[[i]]@data) == "Year")) {
        DT.tmp <- x[[i]]@data[, c("Lon", "Lat", "Year", x.column, wrap.column, colour.column), with=FALSE]
        spatial  <- TRUE
        temporal <- TRUE
      } else if (any(colnames(x[[i]]@data) == "Lon")) {
        DT.tmp <- x[[i]]@data[, c("Lon", "Lat", x.column, wrap.column, colour.column), with=FALSE]
        spatial <- TRUE
      } else if (any(colnames(x[[i]]@data) == "Year")) {
        DT.tmp <- x[[i]]@data[, c("Year", x.column, wrap.column, colour.column), with=FALSE]
        temporal <- TRUE
      } else {
        stop("No column named 'Lon'/'Lat' and/or 'Year' present!")
      }
      DT.tmp[, sens := paste0(x[[i]]@source@id, ".", x[[i]]@id)]
      DT <- rbind(DT, DT.tmp)
    }
  } else if (is.Field(x)) {
    if (all(colnames(x@data) != x.column))
      stop(paste0("'", x.column, "' not present in 'x'!"))
    if (any(colnames(x@data) == "Lon") && any(colnames(x@data) == "Year")) {
      spatial  <- TRUE
      temporal <- TRUE
      DT <- x@data[, c("Lon", "Lat", "Year", x.column, wrap.column, colour.column), with=FALSE]
    } else if (any(colnames(x@data) == "Lon")) {
      spatial <- TRUE
      DT <- x@data[, c("Lon", "Lat", x.column, wrap.column, colour.column), with=FALSE]
    } else if (any(colnames(x@data) == "Year")) {
      temporal <- TRUE
      DT <- x@data[, c("Year", x.column, wrap.column, colour.column), with=FALSE]
    } else {
      stop("No column named 'Lon'/'Lat' and/or 'Year' present!")
    }
  } else {
    stop("x and y must be a DGVMTools::Field or a list if them!")
  }

  if (flip) {
    setnames(DT, x.column, "y.value")
  } else {
    setnames(DT, x.column, "x.value")
  }
    
  if (!is.null(colour.column))
    setnames(DT, colour.column, "c.value")
  if (!is.null(wrap.column))
    setnames(DT, wrap.column, "w.value")
  
  if (temporal) {
    warning("temporal dimension present in 'x'")
    message("temporal dimension present in 'x'")
  }

  if (spatial && temporal) {
    setkey(DT, Lon, Lat, Year)
  } else if (spatial) {
    setkey(DT, Lon, Lat)
  } else {
    setkey(DT, Year)
  }

  if (is.Field(y)) {
    stop("'y' as Field not yet ready!")
  
  } else if (is.data.frame(y)) {
    if (all(colnames(y) != y.column))
      stop(paste0("No column named '", y.column, "' present in 'y'!"))
    if (!is.data.table(y)) {
      y <- as.data.table(y)
    }
    if (spatial && temporal) {
      y <- y[, c("Lon", "Lat", "Year", y.column), with=FALSE]
      setkey(y, Lon, Lat, Year)
    } else if (spatial) {
      y <- y[, c("Lon", "Lat", y.column), with=FALSE]
      setkey(y, Lon, Lat)
    } else if (temporal) {
      y <- y[, c("Year", y.column), with=FALSE]
      setkey(y, Year)
    }
    if (flip) {
      setnames(y, y.column, "x.value")
    } else {
      setnames(y, y.column, "y.value")
    }
    DT <- y[DT]

  } else if (raster::isLonLat(y)) {
    if (!spatial) {
      stop("If 'y' is a raster, x must be spatially!")
    }
    stop("'y' as Raster not yet ready!")
  } else {
    stop("'y' must be a Field, a data.frame/data.table or a raster in Lon/Lat projection!")
  }

  ## return data.table, if requested
  if (!plot)
    return(DT)

  ## apply some data filters
  if (sum(!stats::complete.cases(DT)) > 0) {
    warning(paste0("Removing ", sum(!stats::complete.cases(DT)), " rows with missing data!"))
    message(paste0("Removing ", sum(!stats::complete.cases(DT)), " rows with missing data!"))
    DT=DT[stats::complete.cases(DT), ]    
  }

  if (verbose)
    print(utils::str(DT))
  
  if (!is.null(limit)) {
    if (is.data.frame(limit) || is.list(limit)) {
      if (!is.null(limit$inclusive)) {
        if (!is.null(limit$min)) {
          DT = subset(DT, x.value >= limit$min & y.value >= limit$min)
        } else {
          if (!is.null(limit$xmin))
            DT = subset(DT, x.value >= limit$xmin)
          if (!is.null(limit$ymin))
            DT = subset(DT, y.value >= limit$ymin)
        }
        if (!is.null(limit$max)) {
          DT = subset(DT, x.value <= limit$max & y.value <= limit$max)
        } else {
          if (!is.null(limit$xmax))
            DT = subset(DT, x.value <= limit$xmax)
          if (!is.null(limit$ymax))
            DT = subset(DT, y.value <= limit$ymax)
        }
      } else {
        if (!is.null(limit$min)) {
          DT = subset(DT, x.value > limit$min & y.value > limit$min)
        } else {
          if (!is.null(limit$xmin))
            DT = subset(DT, x.value > limit$xmin)
          if (!is.null(limit$ymin))
            DT = subset(DT, y.value > limit$ymin)
        }
        if (!is.null(limit$max)) {
          DT = subset(DT, x.value < limit$max & y.value < limit$max)
        } else {
          if (!is.null(limit$xmax))
            DT = subset(DT, x.value < limit$xmax)
          if (!is.null(limit$ymax))
            DT = subset(DT, y.value < limit$ymax)
        }
      }
    } else {
      warning("Wrong format of 'limit', use either list or data.frame!")
      message("Wrong format of 'limit', use either list or data.frame!")
    }
  }

  ## convert to factors to keep order
  if (is.character(DT$sens))
    DT$sens <- factor(DT$sens, levels = unique(DT$sens))
  if (is.character(DT$w.value))
    DT$w.value <- factor(DT$w.value, levels = unique(DT$w.value))

  # create plot
  if (density != FALSE) {
    message("2D density plot is experimental.")
    warning("2D density plot is experimental.")
    p <- ggplot(DT, aes(x=x.value, y=y.value))
    if (density=="polygon") {
      p <- p + stat_density_2d(aes(fill = ..level..), geom = "polygon")
    } else if (density=="raster") {
      p <- p + stat_density_2d(aes(fill = ..density..), geom = "raster", contour=FALSE)
    } else {
      p <- p + geom_hex()
    }
  } else if (is.null(colour.column)) {
    p <- ggplot(DT, aes(x=x.value, y=y.value))
    p <- p + geom_point(alpha=alpha, size=1.5, shape=16, col=colours)
  } else {
    p <- ggplot(DT, aes(x=x.value, y=y.value, col=c.value))
    p <- p + geom_point(alpha=alpha, size=1.5, shape=16)
  }
  p <- p + .dgvm.scatter_theme

  if (flip) {
    p <- p + xlab(y.column) + ylab(x.column)
  } else {
    p <- p + xlab(x.column) + ylab(y.column)
  }
  
  if (equal.axis && density=="hex") {
    p <- p + coord_fixed()
  } else if (equal.axis) {
    p <- p + coord_fixed(expand=FALSE)
  }

  ## not tested yet!
  if (is.data.frame(colours)) {
    warning("JS_DEBUG: colouring not tested yet.")
    message("JS_DEBUG: colouring not tested yet.")
    if (any(colnames(colours)=="value") && any(colnames(colours)=="colour")) {
      p <- p + scale_colour_gradientn(colours=colours$colour, values=colours$value)
      p <- p + scale_fill_gradientn(colours=colours$colour, values=colours$value)
      if (sym.col && !density)
        p <- p + expand_limits(colour = c(-max(abs(DT$c.value)), max(abs(DT$c.value))))
    } else if (any(colnames(colours)=="name") && any(colnames(colours)=="colour")) {
      colours <- eval(parse(text=paste0("c('",paste0(colours$name, colours$colour, sep="'='", collapse="','"),"')")))
      p <- p + scale_fill_manual(values=colours, na.value="grey", guide=guide_legend(ncol=4))
      p <- p + scale_colour_manual(values=colours, na.value="grey", guide=guide_legend(ncol=4))
      p <- p + theme(legend.key.width  = unit(0.03, "npc"))
    } else {
      warning(paste("'colours' has wrong column names:", paste(colnames(colours), collapse=", ")))
    }
  }

  ## draw trend lines
  if (!all(is.na(lines))) {
    if (is.data.frame(lines)) {
      if (all(colnames(lines) != "method"))
        stop(paste0("No column named 'method' present in data.frame 'lines'!"))
      if (all(colnames(lines) != "se"))
        lines$se=FALSE
      if (all(colnames(lines) != "col"))
        lines$col="black"
      if (all(colnames(lines) != "type"))
        lines$type="solid"
      if (all(colnames(lines) != "size"))
        lines$size=1
    } else if (is.vector(lines) && typeof(lines) == "character") {
      lines <- data.frame(method=lines, se=FALSE, col="blue", type="solid", size=1)
    }
  }

  if (!all(is.na(lines))) {
    for (i in 1:nrow(lines)) {
      if (lines$method[i]=="1:1") {
        p <- eval(parse(text=paste0("p + geom_abline(col='", lines$col[i], 
                             "', linetype='", lines$type[i],
                             "', size=", lines$size[i], ")")))
      } else if (lines$method[i]=="gam") {
        p <- eval(parse(text=paste0('p + geom_smooth(method="', lines$method[i],
                                   '", formula=y ~ s(x, bs = "cs")',
                                   ', se=', lines$se[i],
                                   ', col="', lines$col[i],
                                   '", linetype="', lines$type[i],
                                   '", size=', lines$size[i], ")")))
      } else {
        p <- eval(parse(text=paste0('p + geom_smooth(method="', lines$method[i],
                                   '", se=', lines$se[i],
                                   ', col="', lines$col[i],
                                   '", linetype="', lines$type[i],
                                   '", size=', lines$size[i], ")")))
      }
    }
  }

  ## add labels
  if (all(!is.na(labels))) {
    get.label <- function(l, d, s=NA, w=NA) {
      if (!is.na(s) && is.na(w)) {
        d <- subset(d, sens==s)
      } else if (is.na(s) && !is.na(w)) {
        d <- subset(d, w.value==w)
      } else if (!is.na(s) && !is.na(w)) {
        d <- subset(d, sens==s & w.value==w)
      }
      lm.d <- stats::lm(d$y.value ~ d$x.value)
      sum.lm <- summary(lm.d)

      ## mean absolute error
      if (tolower(l)=="mae") {
        label <- paste("MAE ==", signif( mean(d$y.value) - mean(d$x.value), 2))
      ## root mean square deviation
      } else if (tolower(l)=="rmsd") {
        label <- paste("RMSD ==", signif(sqrt(mean((d$y.value - d$x.value)^2)), 2))
      ## Model efficiency (Mayer & Butler, 1993)
      } else if (tolower(l)=="me") {
        label <- paste("ME ==", signif(1 - sum((d$y.value-d$x.value)^2, na.rm=TRUE) / sum((d$y.value-mean(d$y.value))^2, na.rm=TRUE), 2))
      ## normalized mean error
      } else if (tolower(l)=="nme") {
        label <- paste("NME ==", signif(sum(abs(d$y.value - d$x.value)) / sum(abs(d$x.value - mean(d$x.value))), 2))
      ## normalized mean squared error
      } else if (tolower(l)=="nsme") {
        label <- paste("NMSE ==", signif(sum((d$y.value - d$x.value)^2) / sum((d$x.value - mean(d$x.value))^2), 2))
      ## 
      ## linear model measures
      ##
      ## equation
      } else if (grepl("eq", l)) {
        label <- paste("y == ", signif(sum.lm$coefficients[2,1], 2), "* x +", signif(sum.lm$coefficients[1,1], 2))
      ## root mean square error
      } else if (tolower(l)=="rmse") {
        label <- paste("RMSE ==", signif(mean(sqrt((stats::residuals(lm.d))^2), na.rm=TRUE), 2))
      ## R^2
      } else if (tolower(l)=="rsq") {
        ## eventually include the p-value sum.lm$coefficients[2,4]
        label <- paste("R^2 ==", signif(sum.lm$r.squared, 2))
      ## adj. R^2
      } else if (tolower(l)=="adjrsq") {
        ## eventually include the p-value sum.lm$coefficients[2,4]
        label <- paste("adj.~R^2 ==", signif(sum.lm$adj.r.squared, 2))
      } else {
        warning(paste0("Label '",l, "' not implemented!"))
        message(paste0("Label '",l, "' not implemented!"))
        next
      }
      return(label)
    }

    ## calculate the label position
    x.max = max(DT$x.value)
    x.min = min(DT$x.value)
    y.max = max(DT$y.value)
    y.min = min(DT$y.value)
    if (grepl("right", label.pos)) {
      label.x <- x.max
      label.hjust <- 1
    } else if (grepl("left", label.pos)) {
      label.x <- x.min
      label.hjust <- 0
    } else {
      label.x <- (x.max + x.min) / 2
      label.hjust <- 0.5
    }
    if (grepl("top", label.pos)) {
      label.y <- y.max
      label.vjust <- 1
      label.dy <- ((x.max-x.min)/(y.max-y.min)) * (y.min - y.max) / 12
    } else {
      label.y <- y.min
      label.vjust <- 0
      label.dy <- ((x.max-x.min)/(y.max-y.min)) * (y.max - y.min) / 12
    }
    
    llabels <- list()
    for (i in 1:length(labels)) {
      ## create the labels
      if (is.null(DT$sens)) {
        if (is.null(DT$w.value)) {
          llabels[[labels[i]]] = append(llabels[[labels[i]]], get.label(labels[i], DT))
        } else {
          if (is.factor(DT$w.value)) {
            for (j in as.numeric(unique(DT$w.value)))
              llabels[[labels[i]]] = append(llabels[[labels[i]]], get.label(labels[i], DT, w=levels(DT$w.value)[j]))
          } else {
            for (j in sort(unique(DT$w.value)))
              llabels[[labels[i]]] = append(llabels[[labels[i]]], get.label(labels[i], DT, w=j))
          }
        }
      } else {
        for (j in as.numeric(unique(DT$sens))) {
          if (is.null(DT$w.value)) {
            llabels[[labels[i]]] = append(llabels[[labels[i]]], get.label(labels[i], DT, s=levels(DT$sens)[j]))
          } else {
            if (is.factor(DT$w.value)) {
              for (k in as.numeric(unique(DT$w.value)))
                llabels[[labels[i]]] = append(llabels[[labels[i]]], get.label(labels[i], DT, s=levels(DT$sens)[j], w=levels(DT$w.value)[k]))
            } else {
              for (k in sort(unique(DT$w.value)))
                llabels[[labels[i]]] = append(llabels[[labels[i]]], get.label(labels[i], DT, s=levels(DT$sens)[j], w=k))
            }
          }
        }
      }
      p <- p + annotate("text", x=label.x, y=label.y+label.dy*(i-1),
                        vjust=label.vjust, hjust=label.hjust, label=llabels[[labels[i]]], parse=TRUE)

      if (verbose) {
        strout <- paste0(labels[i], ": ", paste(llabels[[labels[i]]], collapse=",\n\t"))
        cat(strout)
        cat("\n")
      }
    }
  }
  
  if (!is.null(wrap.column) && is.list(x)) {
    p <- p + facet_grid(w.value~sens)
  } else if (is.list(x)) {
    p <- p + facet_wrap(~sens, ncol=wrap)
  } else if (!is.null(wrap.column)) {
    p <- p + facet_wrap(~w.value, ncol=wrap)
  }
  return(p)
}

#######################################################################
## timeseries #########################################################
#######################################################################
#' Plot a timeseries 
#' 
#' Plot a timeseries with ggplot. Either as line default, overlaying area,
#' applicable p.e. for height or stacked polygons (e.g. for succession).
#' 
#' @param input a Field or a list of several.
#' @param columns The column(s) to display. Default: 'value'.
#' @param scale a scaling factor for multiplication
#' @param colours colours for the diffent VegSpatial objects or columns. Must have the same length otherwise colours are choosing automatically.
#' @param type "line", overlayed "area" or "stack"ed area
#' @param wrap if input is a list and columns has more than one element use "sens" or "column" fro wrapping intp several panels.
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param lty Only used if type="line" and if input is a list and columns has more than one element and no wrapping is desired, use either "sens" or "column" for different line types.
#' @param alpha alpha value for plots
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @return A ggplot object, which can either be printed directly or further modified, or a data.table if plot is FALSE.
#' @export
#' @import ggplot2 
#' @importFrom RColorBrewer RColorBrewer::brewer.pal
## TODO Implement monthly data (input@quant@type == "monthly")
plotGGTemporal <- function(input, columns='value', scale=1., colours=NA, type="line", wrap=NA, long.title=TRUE, lty="sens", alpha=NA, plot=TRUE, ...) {
  ## to avoid "no visible binding for global variable" during check
  Year = sens = value = variable = NULL
  if (is.Field(input, temporal=TRUE)) {
    if (all(is.na(columns)) && all(colnames(input@data) != "value"))
      stop("No columns name given and no column named 'value' present!")

    if (!all(is.na(columns)) && length(columns) > 1) {
      for (cn in columns) {
        if (all(colnames(input@data)!=cn))
          stop(paste0("No column named '", cn, "' present!"))
      }
    } else if (all(colnames(input@data) != columns)) {
      stop(paste0("No column named '",columns,"' present!"))
    }
    dt <- input@data[, c("Year", columns), with = FALSE]
    dt <- data.table::melt(dt, "Year", columns)
    if (length(columns)>1) {
      dt$variable <- as.character(dt$variable)
      dt$variable <- factor(dt$variable, levels=columns)
    }
    quant = input@quant
  } else if (is.list(input)) {
    for (i in 1:length(input)) {
      if (!is.Field(input[[i]], temporal=TRUE))
        stop("'input' must either be a temporal RCVTools::Field or a list of them!")
      if (i == 1) {
        if (all(is.na(columns)) && all(colnames(input[[i]]@data) != "value")) {
          stop("No column name given and no column named 'value' present!")
          columns <- "value"
        } else {
          for (cn in columns) {
            if (all(colnames(input[[i]]@data) != cn))
              stop(paste0("No column named '", cn, "' present!"))
          }
        }
        dt <- input[[i]]@data[, c("Year", columns), with = FALSE]
        quant = input[[i]]@quant
        if (long.title) {
          dt[, sens:=input[[i]]@source@name, ]
          titles <- input[[i]]@source@name
        } else {
          dt[, sens:=input[[i]]@source@id, ]
          titles <- input[[i]]@source@id
        }
      } else {
        for (cn in columns) {
          if (all(colnames(input[[i]]@data)!=cn))
            stop(paste0("No column named '", cn, "' present!"))
        }
        dt.tmp <- input[[i]]@data[, c("Year", columns), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@source@name, ]
          titles <- append(titles, input[[i]]@source@name)
        } else {
          dt.tmp[, sens:=input[[i]]@source@id, ]
          titles <- append(titles, input[[i]]@source@id)
        }
        dt <- rbindlist(list(dt, dt.tmp))
        rm(dt.tmp)
      }
    }
    dt <- dt[, sens:=factor(sens, titles)]
    dt <- data.table::melt(dt, c("Year", "sens"), columns)
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
  
  if (any(!.is.colour(colours)) || any(is.na(colours)))
    colours="black"

  print(length(columns))
  print(length(input))
  print(type)  
  
  p <- ggplot(dt, aes(x=Year, y=value))
  p <- p + .dgvm.temporal_theme
  if (grepl("^l", type)) {
    if (!is.numeric(alpha))
      alpha = 1
    if (length(columns) == 1 && length(input) == 1) {
      p <- p + geom_line(colour=colours, size=1)
    } else {
      if (length(columns) > 1 && length(input) == 1) {
        p <- p + geom_line(aes(col=variable), alpha=alpha, size=1)
        if (length(colours) != length(unique(dt$variable)))
          colours <- RColorBrewer::brewer.pal(length(unique(dt$variable)), "Set1")
      } else if (length(columns) == 1 && length(input) > 1) {
        p <- p + geom_line(aes(col=sens), alpha=alpha, size=1)
        if (length(colours) != length(unique(dt$sens)))
          colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
      } else {
        if (grepl("^c", wrap) || grepl("^v", wrap)) {
          p <- p + geom_line(aes(col=sens), alpha=alpha, size=1)
          p <- p + facet_wrap(~variable, ncol=1) 
          if (length(colours) != length(unique(dt$sens)))
            colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
        } else if (grepl("^r", wrap) || grepl("^s", wrap)) {
          p <- p + geom_line(aes(col=variable), alpha=alpha)
          p <- p + facet_wrap(~sens, ncol=1)
          if (length(colours) != length(unique(dt$variable)))
            colours <- RColorBrewer::brewer.pal(length(unique(dt$variable)), "Set1")
        } else {
          if (grepl("^v", lty) || grepl("^c", lty)) {
            p <- p + geom_line(aes(col=sens, lty=variable), alpha=alpha)
            if (length(colours) != length(unique(dt$sens)))
              colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
          } else {
            p <- p + geom_line(aes(col=variable, lty=sens), alpha=alpha)
            if (length(colours) != length(unique(dt$variable)))
              colours <- RColorBrewer::brewer.pal(length(unique(dt$variable)), "Set1")
          }
        }
      }
      p <- p + scale_colour_manual(values=colours, guide=guide_legend(title="", ncol=length(columns), override.aes=list(size=2)))
    }
  } else {
    if (grepl("^a", type)) {
      if (!is.numeric(alpha))
        alpha=1/length(columns)
    } else {
      if (!is.numeric(alpha))
        alpha=1
    }
    if (length(input) == 1 && length(columns) == 1) {
      p <- p + geom_area(alpha=alpha, fill=colours, position="stack")
    } else {
      if (length(input) == 1 && length(columns) > 1) {
        if (grepl("^a", type)) {
          p <- p + geom_area(aes(fill=variable), alpha=alpha, position = position_dodge(width=0))
        } else {
          p <- p + geom_area(aes(fill=variable), alpha=alpha, position = "stack")
        }
        if (length(colours) != length(unique(dt$variable)))
          colours <- RColorBrewer::brewer.pal(length(unique(dt$variable)), "Set1")
      } else if (length(input) > 1 && length(columns) == 1) {
        if (grepl("^a", type)) {
          p <- p + geom_area(aes(fill=sens), alpha=alpha, position = position_dodge(width=0))
        } else {
          warning("Stacking runs above each other. Is this really what you want?")
          p <- p + geom_area(aes(fill=sens), alpha=alpha, position = "stack")
        }
        if (length(colours) != length(unique(dt$sens)))
          colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
      } else {
        if (grepl("^c", wrap) || grepl("^v", wrap)) {
          if (grepl("^a", type)) {
            p <- p + geom_area(aes(fill=sens), alpha=alpha, position = position_dodge(width=0))
          } else {
            if (length(input) > 1)
              warning("Stacking runs above each other. Is this really what you want?")
            p <- p + geom_area(aes(fill=sens), alpha=alpha, position = "stack")
          }
          if (length(colours) != length(unique(dt$sens)))
            colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
        } else {
          if (grepl("^a", type)) {
            p <- p + geom_area(aes(fill=variable), alpha=alpha, position = position_dodge(width=0))
          } else {
            p <- p + geom_area(aes(fill=variable), alpha=alpha, position = "stack")
          }
          if (length(colours) != length(unique(dt$variable)))
            colours <- RColorBrewer::brewer.pal(length(unique(dt$variable)), "Set1")
        }
        if (grepl("^c", wrap) || grepl("^v", wrap)) {
          p <- p + facet_wrap(~variable, ncol=1) 
        } else if (grepl("^r", wrap) || grepl("^s", wrap)) {
          p <- p + facet_wrap(~sens, ncol=1)
        }
      }
      p <- p + scale_fill_manual(values=colours, guide=guide_legend(title=""))
    }
  }

  p <- p + scale_x_continuous(expand=c(0, 0))
  if (grepl("^s", type) || grepl("^a", type)) 
    p <- p + scale_y_continuous(limits=c(0, NA), expand=c(0, 0))
  p <- p + ylab(paste0(quant@name, " [", quant@units,"]"))

  return(p)
}

#' Plot a histogram
#' 
#' Plot the given columns as histogram
#' 
#' @param input a VegSpatial object or a list of several.
#' @param column The column(s) to display. Default: 'value'.
#' @param colours colours for the diffent VegSpatial objects or columns. Must have the same length otherwise colours are choosing automatically.
#' @param bins number of bins for histogram (default: 25)
#' @param bars draw bars of counted occurence (default TRUE).
#' @param lines also draw density lines (default FALSE).
#' @param long.title If the description (default) should be used as titles or the shorter id.
#' @param plot If FALSE only the data is returned, without drawing the map.
#' @param ... Ignored further parameters
#' @export
#' @import ggplot2
#' @importFrom RColorBrewer RColorBrewer::brewer.pal
plotGGHist <- function(input, column='value', colours=NA, bars=TRUE, lines=FALSE, bins=10, long.title=TRUE, plot=TRUE, ...) {
  ## to avoid "no visible binding for global variable" during check
  sens = bin = value = N = NULL
  ## check if a VegSpatial or a list of VegSpatial is given as input
  ## check data column names for given column name or column name 'value'
  if (is.Field(input, spatial=TRUE)) {
    if (all(is.na(column)) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (!all(is.na(column)) && length(column)>1) {
      for (cn in column) {
        if (all(colnames(input@data)!=cn))
          stop(paste0("No column named '", cn, "' present!"))
      }
    } else if (all(colnames(input@data) != column)) {
      stop(paste0("No column named '",column,"' present!"))
    }
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    if (length(column)==1) {
      setnames(dt, column, "value")
    } else {
      dt <- data.table::melt(dt, key(dt), column)
      setnames(dt, "variable", "sens")
      dt <- dt[, sens:=factor(sens, column)]
    }
  } else if (is.list(input)) {
    for (i in 1:length(input)) {
      if (!is.Field(input[[i]], spatial=TRUE))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      
      if (all(is.na(column)) && all(colnames(input[[i]]@data) != "value"))
        stop("No column name given and no column named 'value' present!")
      if (!all(is.na(column)) && length(column) > 1) {
        warning("Several column names are supplied. Will only use the first one!")
        column <- column[1]
      }
      if (all(colnames(input[[i]]@data) != column)) {
        stop(paste0("No column named '",column,"' present!"))
      }
      
      if (i==1) {
        dt <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[i]]@source@name, ]
          titles <- input[[i]]@source@name
        } else {
          dt[, sens:=input[[i]]@source@id, ]
          titles <- input[[i]]@source@id
        }
      } else {
        dt.tmp <- input[[i]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[i]]@source@name, ]
          titles <- append(titles, input[[i]]@source@name)
        } else {
          dt.tmp[, sens:=input[[i]]@source@id, ]
          titles <- append(titles, input[[i]]@source@id)
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

  if (any(colnames(dt)=="sens") && is.na(colours))  {
    colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
  } else if (any(colnames(dt)=="sens") && length(colours)!=length(unique(dt$sens))) {
    colours <- RColorBrewer::brewer.pal(length(unique(dt$sens)), "Set1")
  } else if (is.na(colours)) {
    colours="#333333"
  }

  warning("Finding breaks is currently not properly implemented!")
  brks <- seq(floor(min(dt$value)), ceiling(max(dt$value)), length=bins+1)
  
  lbls <- paste(brks[1:(length(brks)-1)]+(brks[2:length(brks)] - brks[1:(length(brks)-1)])/2)
  dt[,bin:=findInterval(value, brks, rightmost.closed = TRUE)]
  
  if (any(colnames(dt)=="sens")) {
    binned.dt <- dt[, list("N"), by=c("bin", "sens")]
    p <- ggplot(binned.dt, aes(x=bin, y=N, fill=sens))
    p <- p + scale_fill_manual(values=colours)
  } else {
    binned.dt <- dt[, list("N"), by="bin"]
    p <- ggplot(binned.dt, aes(x=bin, y=N))
  }
  
  if (!plot)
    return(binned.dt)

  p <- p + dgvm.ggplot.theme("scatter")
  if (bars)
    p <- p + geom_bar(stat="identity", position="dodge")
  if (any(colnames(dt)=="sens") && lines) {
    p <- p + geom_line(aes(colour=sens), size=1)
    p <- p + scale_colour_manual(values=colours)
  } else if (lines) {
    p <- p + geom_line(colour=colours, size=1)
  }
  p <- p + xlab("")
  p <- p + scale_x_continuous(breaks=seq(0.5, bins+0.5, 1), labels=brks, expand=c(0.01,0))
  p <- p + scale_y_continuous(expand=c(0,0))
  p <- p + theme(panel.grid.minor.x = element_line(NA))
  return(p)
}
