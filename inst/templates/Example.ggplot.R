#detach('package:DGVMTools', unload=TRUE)
if("package:DGVMTools" %in% search()) detach(name = "package:DGVMTools", unload = TRUE)
suppressMessages(library(DGVMTools))
library(RColorBrewer)

## Define a TIME PERIOD over which to average
period = new("TemporalExtent", id = "Reference", name = "Reference", start = 1981, end = 2010)

## Define the simulation runs
runs.def <- data.frame(id=c("base", "daily", "cryptCover"),
                       name=c("base run with conservative N fixation",
                              "sensitivity run with conservative quasi daily N fixation",
                              "sensitivity run with conservative N fixation elevated to cryptogamic N fixation"),
                       dir=c("/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons",
                             "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons_daily",
                             "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons_CC"),
                       stringsAsFactors = FALSE)

runs <- list()
for (i in 1:nrow(runs.def)) {
  runs[[runs.def$id[i]]] <- defineModelRun(run.dir = runs.def$dir[i],
                                          model = "LPJ-GUESS",
                                          pft.set = lpj.global.PFTs,
                                          id = runs.def$id[i],
                                          name = runs.def$name[i],
                                          driving.data = "PGF",
                                          lonlat.offset = c(0,0),
                                          year.offset = 0
  )
}

########################################################################
### The functions return a ggplot object, which can be further modified
### or printed to the currently open graphics device, which is the
### default behaviour.
########################################################################

### Spatial plots

## Fill the runs with time averaged spatial data
lai.sp    <- list()
biomes.sp <- list()
gpp.sp    <- list()
cpool.sp  <- list()
for (run in runs) {
  lai.sp[[run@id]] = getModelObject(run, 'lai', period, temporally.average=TRUE, write=TRUE)
  biomes.sp[[run@id]] <- calcBiomes(lai.sp[[run@id]], Smith2014.scheme)
  gpp.sp[[run@id]] = getModelObject(run, 'agpp', period, temporally.average=TRUE, write=TRUE)
  cpool.sp[[run@id]] = getModelObject(run, 'cpool', period, temporally.average=TRUE, write=TRUE)
}

## single panel without title
plotGGSpatial(lai.sp[[runs.def$id[1]]], "Total", colours=brewer.pal(9, "YlGn"), long.title=FALSE)

## 2 panels with overriding the default number of legend columns
cols <- Smith2014.scheme@colours(length(Smith2014.scheme@units))
names(cols) <- Smith2014.scheme@units
p <- plotGGSpatial(biomes.sp, "Smith2014", colours=cols, terr.bg="white")
p <- p + guides(fill = guide_legend(ncol = 2))
print(p)

## split into panels by different data.table columns
plotGGSpatial(gpp.sp[[runs.def$id[1]]], c("BNE", "BNS", "TeBS", "TrBE", "C3G", "Total"), colours=brewer.pal(9, "YlGn"), wrap=3)

## split into different panels by another spatial data
plotGGSpatial(gpp.sp[[runs.def$id[1]]], "Total", wrap=c("biomes.sp[[1]]", "Smith2014", 4), colours=brewer.pal(9, "YlGn"), long.title=FALSE)

plotGGSpatial(gpp.sp[[runs.def$id[1]]], "Total", wrap=c("biomes.sp[[1]]", "Smith2014", 4), colours=brewer.pal(9, "YlGn"), long.title=FALSE, miss.bg = "white")

## 2 panels of different runs (dataset also possible, but use the same colorbar)
plotGGSpatial(gpp.sp, "Total", colours=brewer.pal(9, "YlGn"))

## 2 panels with overriding the default number of legend columns
p <- plotGGSpatial(list(biomes.sp[[1]], biomes.sp[[3]]), "Smith2014", colours=cols)
p <- p + guides(fill = guide_legend(ncol = 3))
print(p)

## single meridional plot
plotGGMeridional(gpp.sp[[1]], "Total", what=list(center="md", var=c(0.25, 0.75)), colors="red")

## multiple columns
plotGGMeridional(gpp.sp[[1]], c("BNE", "TeBS", "TrBE", "Total"), what=list(center="mn", var="sd"))


## several sensitivity runs with short names
p <- plotGGMeridional(gpp.sp, "Total", long.title=FALSE)
p <- p + guides(fill = guide_legend(ncol = 1)) + theme(legend.position = "right")
print(p)

## categorically aggregated summary plot of one run
## specifying either slot/column or x/y as either list or data.frame
plotGGCategorialAggregated(gpp.sp[[1]], biomes.sp[[1]], x.col.name = "Total", cat.col.name = "Smith2014")

## several sensitivity runs by one biome definition
p <- plotGGCategorialAggregated(list(gpp.sp[["base"]], gpp.sp[["daily"]], gpp.sp[["cryptCover"]]), biomes.sp[[1]], x.col.name = "Total", cat.col.name = "Smith2014")
p <- p + guides(col = guide_legend(ncol = 1))
print(p)

## several sensitivity runs by individual biome definition 
p <- plotGGCategorialAggregated(list(gpp.sp[["base"]], gpp.sp[["daily"]], gpp.sp[["cryptCover"]]), 
                                list(biomes.sp[["base"]], biomes.sp[["daily"]], biomes.sp[["cryptCover"]]),
                                x.col.name = "Total", cat.col.name = "Smith2014", vertical=TRUE)
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + guides(col = guide_legend(ncol = 1))
print(p)

## the same data, but displayed as vertical bars instead of horizontal points
## please note that bars use "fill" instead of "col" in guide
p <- plotGGCategorialAggregated(list(gpp.sp[["base"]], gpp.sp[["daily"]], gpp.sp[["cryptCover"]]), 
                                list(biomes.sp[["base"]], biomes.sp[["daily"]], biomes.sp[["cryptCover"]]),
                                x.col.name = "Total", cat.col.name = "Smith2014", bar=TRUE, vertical=TRUE)
p <- p + guides(fill = guide_legend(ncol = 1))
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

### Time series

period = new("TemporalExtent", id = "Reference", name = "Reference", start = 1951, end = 2010)
## Fill the runs with time averaged spatial data
gpp.ts   <- list()
cflux.ts <- list()
for (run in runs) {
  cflux.ts[[run@id]] = getModelObject(run, 'cflux', period, temporally.average=FALSE, spatially.average=TRUE, write=TRUE)
  gpp.ts[[run@id]] = getModelObject(run, 'agpp', period, temporally.average=FALSE, spatially.average=TRUE, write=TRUE)
}

## plot several columns together
plotGGTemporal(gpp.ts[["base"]], c("BNE", "TeBS", "TrIBE", "TrBR"))

lon <- extract.seq(lai.sp[['base']]@data$Lon)
lat <- extract.seq(lai.sp[['base']]@data$Lat)
area2d <- gridarea2d(lon, lat, scale=1.e-12)
land.area <- sum(area2d[lai.sp[['base']]@data]$area)
## plot several
p <- plotGGTemporal(gpp.ts, "Total", scale=land.area)
p <- p + guides(col = guide_legend(title="", ncol = 1))
print(p)

## adding Le Quere land sink data
library(xlsx)
LeQuere2015 <- read.xlsx("/home/jsteinkamp/Cloud/Dropbox/WIP/Establishment/Global_Carbon_Budget_2015_v1.1.xlsx", sheetName = "Global Carbon Budget", startRow=22, endRow=78, header=TRUE)
LeQuere2015$land.sink <- -1 * LeQuere2015$land.sink 
p <- plotGGTemporal(cflux.ts, "NEE", scale=land.area)
p = p + guides(col = guide_legend(title="", ncol = 1))
p = p + geom_line(data=LeQuere2015, aes(x=Year, y=land.sink))
print(p)


### simple Arithmetics

## create a new quantity description for Carbon residence time,
## which will be calculated
RT.quant <- new("Quantity",
                id="CResidenceTime",
                name="Carbon Residence Time",
                type="pools",
                units="years")
## spatial
residence.time <- calcNewModelObj(cpool.sp[[1]], gpp.sp[[1]], "/", "Total", "Total", quant=RT.quant)
key.names <- key(residence.time@data)
val.names <- names(residence.time@data)
val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
## set negative RT to NA
for (j in val.names)
  set(residence.time@data, which(residence.time@data[[j]] < 0), j, NA)

## use the logarithm, otherwise everything will be just yellow
residence.time@data[, logTotal:=log10(Total), ]
p <- plotGGSpatial(residence.time, "logTotal")
p <- p + scale_fill_gradientn(colours=brewer.pal(9, "YlOrBr"), labels=c(1,10,100,1000), breaks=c(1,2,3,4))
print(p)

plotGGCategorialAggregated(residence.time, biomes.sp[[1]], "Total", "Smith2014")


## several sesitivity time series
rt.ts <- list()
for (run in runs) {
  rt.ts[[run@id]] <- calcNewModelObj(cpool.ts[[run@id]], gpp.ts[[run@id]], "/", "Total", "Total", quant=RT.quant)
}
p <- plotGGTemporal(rt.ts, "Total")
p <- p + guides(col = guide_legend(title="", ncol = 1))
print(p)

