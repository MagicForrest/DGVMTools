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
  
  for (i in c(1/6, 1/3, 2/3, 1:5, 10, 15)) {
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

#######################################################################
## map ################################################################
#######################################################################

plotGGMap <- function(input, column=NA, colors=NA, sym.col=FALSE, wrap=1, long.title=TRUE, plot=TRUE, ...) {
  ## check if a VegSpatial or a list of VegSpatial is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegSpatial(input)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (all(colnames(input@data) != column))
      stop(paste("No column named '",column,"' present!", sep=""))
    london.centre <- input@run@london.centre
    if (is.character(input@run@map.overlay)) {
      map.overlay <- input@map.overlay
    } else {
      map.overlay <- fortify(SpatialLinesDataFrame(input@run@map.overlay[[2]],
                                                   data.frame(ID=getSLLinesIDSlots(input@run@map.overlay[[2]]))))
    }
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    setnames(dt, column, "value")
  } else if (is.list(input)) {
    for (n in 1:length(input)) {
      if (!is.VegSpatial(input[[n]]))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      if (n==1) {
        london.centre <- input[[n]]@run@london.centre
        if (is.character(input[[n]]@run@map.overlay)) {
          map.overlay <- input[[n]]@map.overlay
        } else {
          map.overlay <- fortify(SpatialLinesDataFrame(input[[n]]@run@map.overlay[[2]],
                                                       data.frame(ID=getSLLinesIDSlots(input[[n]]@run@map.overlay[[2]]))))
        }
        dt <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[n]]@run@description, ]
          titles <- input[[n]]@run@description
        } else {
          dt[, sens:=input[[n]]@run@id, ]
          titles <- input[[n]]@run@id
        }
       } else {
        dt.tmp <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[n]]@run@description, ]
          titles <- append(titles, input[[n]]@run@description)
        } else {
          dt.tmp[, sens:=input[[n]]@run@id, ]
          titles <- append(titles, input[[n]]@run@id)
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
  lon <- sort(unique(dt$Lon))
  lat <- sort(unique(dt$Lat))
  res <- min(lon[2:length(lon)] - lon[1:(length(lon)-1)],
             lat[2:length(lat)] - lat[1:(length(lat)-1)])

  ## define plot region
  lon.limit <- c(min(lon) - res/2, max(lon) + res/2)
  lat.limit <- c(min(lat) - res/2, max(lat) + res/2)

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
  lon <- .ll.breaks(lon[2:(length(lon)-1)], label="lon")
  lat <- .ll.breaks(lat[2:(length(lat)-1)], label="lat", n.min=4, n.max=7)

  ## create plot
  p <- ggplot(dt, aes(y=Lat, x=Lon))
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

  ## return the plot for further manipulation or direct printing
  return(p)
}

#######################################################################
## meridional #########################################################
#######################################################################

plotGGMeridional <- function(input, column=NA, what=list(center="mn", var="sd"), alpha=c(0.8, 0.1), colors=NA, long.title=TRUE, plot=TRUE, ...) {
  if (!any(names(what)=="center"))
    stop("No definition for meridional average given. Must be either 'mn', 'mean' or 'md', 'median'.")
  if (!any(names(what)=="var"))
    what[["var"]] = NA
  
  ## check if a VegObj or a list of VegObj is given as input
  ## check data column names for given column name or column name 'value'
  if (is.VegSpatial(input)) {
    if (is.na(column) && all(colnames(input@data) != "value"))
      stop("No column name given and no column named 'value' present!")
    if (all(colnames(input@data) != column))
      stop(paste("No column named '",column,"' present!", sep=""))
    units <- input@quant@units
    dt <- input@data[, c("Lon", "Lat", column), with = FALSE]
    setnames(dt, column, "value")
  } else if (is.list(input)) {
    for (n in 1:length(input)) {
      if (!is.VegSpatial(input[[n]]))
        stop("'input' must either be a RCVTools::VegSpatial or a list of them!")
      if (n==1) {
        units <- input[[n]]@quant@units
        dt <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt[, sens:=input[[n]]@run@description, ]
          titles <- input[[n]]@run@description
        } else {
          dt[, sens:=input[[n]]@run@id, ]
          titles <- input[[n]]@run@id
        }
       } else {
        dt.tmp <- input[[n]]@data[, c("Lon", "Lat", column), with = FALSE]
        if (long.title) {
          dt.tmp[, sens:=input[[n]]@run@description, ]
          titles <- append(titles, input[[n]]@run@description)
        } else {
          dt.tmp[, sens:=input[[n]]@run@id, ]
          titles <- append(titles, input[[n]]@run@id)
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
    p <- ggplot(md, aes(x=Lat, y=value.center))
  }

  if (!plot)
    return(md)
  
  p <- p + .lpj.scatter_theme

  if (!is.na(what[["var"]])) {
    if (any(colnames(dt)=="sens")) {
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
plotGGCategorialAggregated <- function(input, targets=NULL, name.map=NA, area.weighted=TRUE, long.title=TRUE, plot=TRUE, ...) {
  if (is.null(targets)) {
    stop("Don't know what to do, if you are not telling me!")
  } else if (is.list(targets)) {
    targets <- as.data.frame(targets, stringsAsFactors=FALSE)
  } else if (is.data.frame(targets)) {
    for (i in 1:2)
      targets[, 1] = as.character(targets[, 1])
  }

  if (colnames(targets)[1] == "x" && colnames(targets)[2] == "y")
    targets <- .transpose.df(targets)

  if (ncol(targets)==2 && ncol(targets)==2) {
    colnames(targets) <- c("slot", "column")
    rownames(targets) <- c("x", "y")
  } else {
    stop("Don't know what to do, if you are not telling me!")
  }

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
    dt <- dy[dx]
    setnames(dt, targets$column[1], "value")
    if (area.weighted) {
      dt <- eval(parse(text=paste("dt[, list(value=weighted.mean(value, area)), by=list(", targets$column[2],")]", sep="")))
    } else {
      dt <- eval(parse(text=paste("dt[, list(value=mean(value)), by=list(", targets$column[2],")]", sep="")))
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
    if (area.weighted) {
      dt <- eval(parse(text=paste("dt[, list(value=weighted.mean(value, area)), by=list(", targets$column[2],", sens)]", sep="")))
    } else {
      dt <- eval(parse(text=paste("dt[, list(value=mean(value)), by=list(", targets$column[2],", sens)]", sep="")))
    }
  } else {
    stop("'input' must either be a RCVTools::VegRun or a list of them!")
  }
  rm(dx, dy)
  
  ## if name.map is a valid named vector the names are used for x-labels
  if (!any(is.na(name.map))) {
    if (is.vector(name.map) && !is.null(names(name.map))) {
      eval(parse(text=paste("dt[, name:=names(name.map)[",targets$column[2],"], ]", sep="")))
    }
  }

  if (!plot)
    return(dt)

  if (any(colnames(dt) == "sens")) {
    if (any(colnames(dt) == "name")) {
      p <- ggplot(dt, aes(x=value, y=name, col=sens))
    } else {
      eval(parse(text=paste("p <- ggplot(dt, aes(x=value, y=",targets$column[2],", col=sens))", sep="")))
    }
    p <- p + .lpj.scatter_theme
    p <- p + scale_color_manual(values=brewer.pal(length(unique(dt$sens)),"Set1"),
                                na.value="grey", guide=guide_legend(ncol=2))
    p <- p + geom_point(size=2.5, position = position_jitter(w = 0, h = 0.15))
    p <- p + xlab(units) + ylab("")
    p <- p + guides(colour = guide_legend(override.aes = list(size=4), ncol=2))
  } else {
    if (any(colnames(dt) == "name")) {
      p <- ggplot(dt, aes(x=value, y=name))
    } else {
      eval(parse(text=paste("p <- ggplot(dt, aes(x=value, y=",targets$column[2],"))", sep="")))
    }
    p <- p + .lpj.scatter_theme
    p <- p + geom_point(size=2.5)
    p <- p + xlab(units) + ylab("")
    p <- p + guides(colour = guide_legend(override.aes = list(size=4), ncol=2))
  }

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
