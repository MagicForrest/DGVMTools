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
lai.sp    <-  list()
biomes.sp <- list()
gpp.sp    <-  list()
for (run in runs) {
  lai.sp[[run@id]] = getModelObject(run, 'lai', period, temporally.average=TRUE, write=TRUE)
  biomes.sp[[run@id]] <- calcBiomes(lai.sp[[run@id]], Smith2014.scheme)
  gpp.sp[[run@id]] = getModelObject(run, 'agpp', period, temporally.average=TRUE, write=TRUE)
}

## single panel without title
plotGGSpatial(lai.sp[[runs.def$id[1]]], "Total", colors=brewer.pal(9, "YlGn"), long.title=FALSE)

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

plotGGSpatial(gpp.sp[[runs.def$id[1]]], "Total", wrap=c("biomes.sp[[1]]", "Smith2014", 4), colours=brewer.pal(9, "YlGn"), long.title=FALSE, terr.bg = "white")

## 2 panels of different runs (dataset also possible, but use the same colorbar)
plotGGSpatial(gpp.sp, "Total", colours=brewer.pal(9, "YlGn"))

## 2 panels with overriding the default number of legend columns
p <- plotGGSpatial(list(biomes.sp[[1]], biomes.sp[[3]]), "Smith2014", colors=cols)
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



## Continue here


## several sensitivity runs
p <- plotGGCategorialAggregated(list(base, sens_constCO2, sens_daily), 
                                targets=data.frame(slot=c("gpp.sp", "lai.sp"), column=c("Total", "Smith2014")), 
                                name.map=Smith2014.scheme@cols)
p <- p + guides(col = guide_legend(ncol = 1))
print(p)

## the same data, but displayed as vertical bars instead of horizontal points
## please note that bars use "fill" instead of "col" in guide
p <- plotGGCategorialAggregated(list(base, sens_constCO2, sens_daily), 
                                targets=data.frame(slot=c("gpp.sp", "lai.sp"), column=c("Total", "Smith2014")), 
                                name.map=Smith2014.scheme@cols, bar=TRUE, vertical=TRUE)
p <- p + guides(fill = guide_legend(ncol = 1))
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

### Time series
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@objects[['gpp.ts']] <- getVegTemporal(",run,", 'agpp', reread.file = FALSE, write=TRUE)", sep="")))
  eval(parse(text=paste(run, "@objects[['cpool.ts']] <- getVegTemporal(",run,", 'cpool', reread.file = FALSE, write=TRUE)", sep="")))
}

## plot several columns together
plotGGTemporal(base@objects[['gpp.ts']], c("BNE", "TeBS", "TrIBE", "TrBR"))

lon <- extract.seq(base@objects[['lai.sp']]@data$Lon)
lat <- extract.seq(base@objects[['lai.sp']]@data$Lat)
area2d <- gridarea2d(lon, lat, scale=1.e-12)
land.area <- sum(area2d[base@objects[['lai.sp']]@data]$area)
## plot several
p <- plotGGTemporal(list(base@objects[['gpp.ts']],
                         sens_constCO2@objects[['gpp.ts']],
                         sens_daily@objects[['gpp.ts']],
                         sens_CC@objects[['gpp.ts']],
                         sens_centr@objects[['gpp.ts']],
                         sens_CLM@objects[['gpp.ts']]), "Total", scale=land.area)
p <- p + guides(col = guide_legend(title="", ncol = 1))
print(p)

### simple Arithmetics

## Add some more time averaged spatial data
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@objects[['cpool.sp']] <- getVegSpatial(",run,", 'cpool', period)", sep="")))
}

## create a new quantity description for Carbon residence time,
## which will be calculated
RT.quant <- new("VegQuant",
                id="CResidenceTime",
                short.string="CResidenceTime",
                full.string="Carbon residence time",
                type="pools",
                units="years")
## spatial
t <- list(x=c("cpool.sp"), y=c("gpp.sp", "Total"))
residence.time <- calcNewVegObj(base, t, "/", quant=RT.quant)
key.names <- key(residence.time@data)
val.names <- names(residence.time@data)
val.names <- val.names[sapply(val.names, function(x) {!any(x==key.names)})]
## set negative RT to NA
for (j in val.names)
  set(residence.time@data, which(residence.time@data[[j]]<0), j, NA)

## use the logarithm, otherwise everything will be just yellow
residence.time@data[, logTotal:=log10(Total), ]
plotGGSpatial(residence.time, "logTotal", colors=brewer.pal(9, "YlOrBr"))

base@objects[['rt.sp']] = residence.time
plotGGCategorialAggregated(base, targets=list(slot=c("rt.sp", "lai.sp"), col=c("Total", "Smith2014")), name.map=Smith2014.scheme@cols)

## temporal
plotGGTemporal(base@objects[['cpool.ts']], "Total", colors="green")

## This one has the wrong axis unit!
plotGGTemporal(base@objects[['cpool.ts']], c("VegC", "LitterC", "SoilC"), scale=land.area)

t <- list(x=c("cpool.ts"), y=c("gpp.ts", "Total"))
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  message(run)
  eval(parse(text=paste(run, "@objects[['rt.ts']] <- calcNewVegObj(", run, ", t, '/', quant=RT.quant)", sep="")))
}
p <- plotGGTemporal(list(base@objects[['rt.ts']],
                         sens_constCO2@objects[['rt.ts']],
                         sens_daily@objects[['rt.ts']],
                         sens_CC@objects[['rt.ts']],
                         sens_centr@objects[['rt.ts']],
                         sens_CLM@objects[['rt.ts']]), "Total")
p <- p + guides(col = guide_legend(title="", ncol = 1))
print(p)

## The code below here is not yet working!
q()
#################################################
### get some temporal stand data (site level) ###
#################################################
MoMo1 <- defineVegRun(run.dir = "/data/WIP/MoMo/output/Hyytiala/miroc-esm-chem/rcp8p5/longevity/",
                      model = "LPJ-GUESS",
                      pft.set = global.PFTs,
                      id = "MoMo1",
                      description= "Future projection in Hyytiala Finnland with RCP8.5",
                      driving.data = "ISI-MIP fasttrack",
                      map.overlay = "",
                      lonlat.offset = c(0,0),
                      year.offset = 0)

MoMo2 <- defineVegRun(run.dir = "/data/WIP/MoMo/output/Hyytiala/miroc-esm-chem/rcp2p6/longevity/",
                      model = "LPJ-GUESS",
                      pft.set = global.PFTs,
                      id = "MoMo1",
                      description= "Future projection in Hyytiala Finnland with RCP2.6",
                      driving.data = "ISI-MIP fasttrack",
                      map.overlay = "",
                      lonlat.offset = c(0,0),
                      year.offset = 0)

Hyy <- new("SpatialExtent", id="Hyy", name="Hyytiala", extent=extent(rep(24.25, 2),rep(61.75, 2)))
MoMo1 <- defineVegRun(run.dir = "/data/WIP/MoMo/output/Hyytiala/miroc-esm-chem/rcp8p5/longevity/",
                      model = "LPJ-GUESS",
                      pft.set = global.PFTs,
                      id = "MoMo1",
                      description= "Future projection in Hyytiala Finnland with RCP8.5",
                      driving.data = "ISI-MIP fasttrack",
                      map.overlay = "",
                      lonlat.offset = c(0,0),
                      year.offset = 0)
MoMo1@objects[['height']] <- getVegObject(MoMo1, 'height')

MoMo1@objects[['height']]@data

hgt.quant <- new("VegQuant",
                id="height",
                short.string="height",
                full.string="Average tree height",
                type="",
                units="m")
ba.quant <- new("VegQuant",
                id="ba",
                short.string="ba",
                full.string="basal area",
                type="",
                units="m^2 ha^-1")
MoMo1@temporal[['height']] <- getVegTemporal(",run,", 'height', spatial.extent=Hyy, area.weighted=FALSE)

for (run in c("MoMo1", "MoMo2")) {
  message(run)
  eval(parse(text=paste(run, "@temporal[['height']] <- getVegTemporal(",run,", 'height', spatial.extent=Hyy, area.weighted=FALSE, forceReAveraging = FALSE)", sep="")))
  eval(parse(text=paste(run, "@temporal[['height']]@quant <- hgt.quant", sep="")))
  eval(parse(text=paste(run, "@temporal[['ba']] <- getVegTemporal(",run,", 'ba', spatial.extent=Hyy, area.weighted=FALSE, forceReAveraging = FALSE)", sep="")))
  eval(parse(text=paste(run, "@temporal[['ba']]@quant <- ba.quant", sep="")))
}
dt=plotGGTemporal(list(MoMo1@temporal[['ba']], MoMo2@temporal[['ba']]),
                    columns=c("Bet_pub", "Pic_abi", "Pop_tre", "C3_gr"),
                    type="s", plot=FALSE)
