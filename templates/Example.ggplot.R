#detach('package:RVCTools', unload=TRUE)
if("package:RVCTools" %in% search()) detach(name = "package:RVCTools", unload = TRUE)
suppressMessages(library(RVCTools))
library(RColorBrewer)

## Define a TIME PERIOD over which to average
period = new("TemporalExtent", id = "Reference", name = "Reference", start = 1981, end = 2010)

## Define the simulation runs
base <- defineVegRun(run.dir = "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons",
                     model = "LPJ-GUESS",
                     pft.set = global.PFTs,
                     id = "base",
                     description= "base run with conservative N fixation",
                     driving.data = "PGF",
                     map.overlay = "lowres",
                     lonlat.offset = c(0,0),
                     year.offset = 0
                     )

sens_constCO2 <- defineVegRun(run.dir = "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons_constCO2",
                              model = "LPJ-GUESS",
                              pft.set = global.PFTs,
                              id = "sens_constCO2",
                              description= "sensitivity run with constant CO2 (300ppm) and conservative N fixation",
                              driving.data = "PGF",
                              map.overlay = "lowres",
                              lonlat.offset = c(0,0),
                              year.offset = 0
                              )

sens_daily <- defineVegRun(run.dir = "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons_daily",
                           model = "LPJ-GUESS",
                           pft.set = global.PFTs,
                           id = "sens_daily",
                           description= "sensitivity run with conservative quasi daily N fixation",
                           driving.data = "PGF",
                           map.overlay = "lowres",
                           lonlat.offset = c(0,0),
                           year.offset = 0
                           )

sens_CC <- defineVegRun(run.dir = "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_cons_CC",
                        model = "LPJ-GUESS",
                        pft.set = global.PFTs,
                        id = "sens_CC",
                        description= "sensitivity run with conservative N fixation elevated to cryptogamic N fixation",
                        driving.data = "PGF",
                        map.overlay = "lowres",
                        lonlat.offset = c(0,0),
                        year.offset = 0
                        )

sens_centr <- defineVegRun(run.dir = "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/Cleveland_centr",
                           model = "LPJ-GUESS",
                           pft.set = global.PFTs,
                           id = "sens_centr",
                           description= "sensitivity run with central N fixation",
                           driving.data = "PGF",
                           map.overlay = "lowres",
                           lonlat.offset = c(0,0),
                           year.offset = 0
                           )

sens_CLM <- defineVegRun(run.dir = "/senckenberg.de/cub/bigdata/LPJ/output/global/Nfix/CLM",
                         model = "LPJ-GUESS",
                         pft.set = global.PFTs,
                         id = "sens_CLM",
                         description= "sensitivity run with CLM-like N fixation",
                         driving.data = "PGF",
                         map.overlay = "lowres",
                         lonlat.offset = c(0,0),
                         year.offset = 0
                         )

########################################################################
### The functions return a ggplot object, which can be further modified
### or printed to the currently open graphics device, which is the
### default behaviour.
########################################################################

### Spatial analyses

## Fill the runs with time averaged spatial data
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@spatial[['lai']] <- getVegObject(",run,", 'lai', period, temporally.average=TRUE, write=TRUE)", sep="")))
  eval(parse(text=paste(run, "@spatial[['gpp']] <- getVegObject(",run,", 'agpp', period, temporally.average=TRUE, write=TRUE)", sep="")))
  eval(parse(text=paste(run, "@spatial[['lai']] <- addBiomes(",run, "@spatial[['lai']], Smith2014.scheme)", sep="")))
}

## single panel without title
plotGGSpatial(base@spatial[['lai']], "Total", colors=brewer.pal(9, "YlGn"), long.title=FALSE)

## split into panels by different data.table columns
plotGGSpatial(base@spatial[['gpp']], c("BNE", "BNS", "TeBS", "TrBE", "C3G", "Total"), colors=brewer.pal(9, "YlGn"), wrap=3)

## split into different panels by another spatial data
plotGGSpatial(base@spatial[['lai']], "Total", wrap=c("base", "lai", "Smith2014", 4), colors=brewer.pal(9, "YlGn"), long.title=FALSE)
## the same maps as above but nicer labels and missing terrestial cells are filled with white (terr.bg),
##using either a named vector (then the names are used) or an unnamed vetor (then the values are used)
wrap <- list(run="base", name="lai", column="Smith2014", ncol=4, map=Smith2014.scheme@cols)
wrap <- list(run="base", name="lai", column="Smith2014", ncol=4, map=names(Smith2014.scheme@cols))
plotGGSpatial(base@spatial[['lai']], "Total", wrap=wrap, colors=brewer.pal(9, "YlGn"), long.title=FALSE, terr.bg="white")

## 2 panels of different runs (dataset also possible, but use the same colorbar)
plotGGSpatial(list(base@spatial[['gpp']], sens_constCO2@spatial[['gpp']]), "Total", colors=brewer.pal(9, "YlGn"))

## 2 panels with overriding the default number of legend columns
p <- plotGGSpatial(list(a=base@spatial[['lai']], b=sens_constCO2@spatial[['lai']]), 
                   "Smith2014", colors=Smith2014.scheme@cols, terr.bg="white")
p <- p + guides(fill = guide_legend(ncol = 2))
print(p)

## single meridional plot
plotGGMeridional(base@spatial[['gpp']], "Total", what=list(center="md", var=c(0.25,0.75)), colors="red")

## multiple columns
plotGGMeridional(base@spatial[['gpp']], c("BNE", "TeBS", "TrBE", "Total"), what=list(center="mn", var="sd"))


## several sensitivity runs with short names
p <- plotGGMeridional(list(base@spatial[['gpp']],
                           sens_constCO2@spatial[['gpp']],
                           sens_daily@spatial[['gpp']],
                           sens_CC@spatial[['gpp']],
                           sens_centr@spatial[['gpp']],
                           sens_CLM@spatial[['gpp']]),
                      "Total", long.title=FALSE)
p <- p + guides(fill = guide_legend(ncol = 1)) + theme(legend.position = "right")
print(p)

## categorically aggregated summary plot of one run
## specifying either slot/column or x/y as either list or data.frame
plotGGCategorialAggregated(base, targets=list(slot=c("gpp", "lai"), col=c("Total", "Smith2014")))
plotGGCategorialAggregated(base, targets=data.frame(x=c("gpp", "Total"), y=c("lai", "Smith2014")))

## several sensitivity runs
p <- plotGGCategorialAggregated(list(base, sens_constCO2, sens_daily), 
                                targets=data.frame(slot=c("gpp", "lai"), column=c("Total", "Smith2014")), 
                                name.map=Smith2014.scheme@cols)
p <- p + guides(col = guide_legend(ncol = 1))
print(p)

## the same data, but displayed as vertical bars instead of horizontal points
## please note that bars use "fill" instead of "col" in guide
p <- plotGGCategorialAggregated(list(base, sens_constCO2, sens_daily), 
                                targets=data.frame(slot=c("gpp", "lai"), column=c("Total", "Smith2014")), 
                                name.map=Smith2014.scheme@cols, bar=TRUE, vertical=TRUE)
p <- p + guides(fill = guide_legend(ncol = 1))
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
print(p)

### Time series
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@temporal[['gpp']] <- getVegTemporal(",run,", 'agpp', reread.file = FALSE, write=TRUE)", sep="")))
  eval(parse(text=paste(run, "@temporal[['cpool']] <- getVegTemporal(",run,", 'cpool', reread.file = FALSE, write=TRUE)", sep="")))
}

## plot several columns together
plotGGTemporal(base@temporal[['gpp']], c("BNE", "TeBS", "TrIBE", "TrBR"))

lon <- extract.seq(base@spatial[['lai']]@data$Lon)
lat <- extract.seq(base@spatial[['lai']]@data$Lat)
area2d <- gridarea2d(lon, lat, scale=1.e-12)
land.area <- sum(area2d[base@spatial[['lai']]@data]$area)
## plot several
p <- plotGGTemporal(list(base@temporal[['gpp']],
                         sens_constCO2@temporal[['gpp']],
                         sens_daily@temporal[['gpp']],
                         sens_CC@temporal[['gpp']],
                         sens_centr@temporal[['gpp']],
                         sens_CLM@temporal[['gpp']]), "Total", scale=land.area)
p <- p + guides(col = guide_legend(title="", ncol = 1))
print(p)

### Arithmetics

## Add some more time averaged spatial data
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@spatial[['cpool']] <- getVegSpatial(",run,", 'cpool', period)", sep="")))
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
t <- list(x=c("spatial", "cpool"), y=c("spatial", "gpp", "Total"))
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

base@spatial[['residence.time']] = residence.time
plotGGCategorialAggregated(base, targets=list(slot=c("residence.time", "lai"), col=c("Total", "Smith2014")), name.map=Smith2014.scheme@cols)

## temporal
plotGGTemporal(base@temporal[['cpool']], "Total", colors="green")

## This one has the wrong axis unit!
plotGGTemporal(base@temporal[['cpool']], c("VegC", "LitterC", "SoilC"), scale=land.area)

t <- list(x=c("temporal", "cpool"), y=c("temporal", "gpp", "Total"))
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  message(run)
  eval(parse(text=paste(run, "@temporal[['residence.time']] <- calcNewVegObj(", run, ", t, '/', quant=RT.quant)", sep="")))
}
p <- plotGGTemporal(list(base@temporal[['residence.time']],
                         sens_constCO2@temporal[['residence.time']],
                         sens_daily@temporal[['residence.time']],
                         sens_CC@temporal[['residence.time']],
                         sens_centr@temporal[['residence.time']],
                         sens_CLM@temporal[['residence.time']]), "Total")
p <- p + guides(col = guide_legend(title="", ncol = 1))
print(p)

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
MoMo1@temporal[['height']] <- getVegTemporal(",run,", 'height', spatial.extent=Hyy, area.weighted=FALSE)

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
