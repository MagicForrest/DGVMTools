detach('package:RVCTools', unload=TRUE)
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

### Spatial analyses first

## Fill the runs with time averaged spatial data
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@spatial[['lai']] <- getVegSpatial(",run,", period, 'lai', forceReAveraging = FALSE)", sep="")))
  eval(parse(text=paste(run, "@spatial[['gpp']] <- getVegSpatial(",run,", period, 'agpp', forceReAveraging = FALSE)", sep="")))
  eval(parse(text=paste(run, "@spatial[['lai']] <- addBiomes(",run, "@spatial[['lai']], Smith2014.scheme)", sep="")))
}

## single panel without title
plotGGMap(base@spatial[['lai']], "Total", colors=brewer.pal(9, "YlGn"), long.title=FALSE)

## 2 panel
plotGGMap(list(base@spatial[['gpp']], sens_constCO2@spatial[['gpp']]), "Total", colors=brewer.pal(9, "YlGn"))

## 2 panel with overriding the default number of legend columns
p <- plotGGMap(list(a=base@spatial[['lai']], b=sens_constCO2@spatial[['lai']]), 
               "Smith2014", colors=Smith2014.scheme@cols)
p <- p + guides(fill = guide_legend(ncol = 2))
print(p)

## meridional plot
plotGGMeridional(base@spatial[['gpp']], "Total", what=list(center="md", var=c(0.25,0.75)), colors="red")

## meridional plot with short names
p <- plotGGMeridional(list(base@spatial[['gpp']],
                           sens_constCO2@spatial[['gpp']],
                           sens_daily@spatial[['gpp']],
                           sens_CC@spatial[['gpp']],
                           sens_centr@spatial[['gpp']],
                           sens_CLM@spatial[['gpp']]),
                      "Total", long.title=FALSE)
p <- p + guides(fill = guide_legend(ncol = 1)) + theme(legend.position = "right")
print(p)

## categorically aggregated summary plot
plotGGCategorialAggregated(base, targets=list(slot=c("gpp", "lai"), col=c("Total", "Smith2014")))

p <- plotGGCategorialAggregated(list(base, sens_constCO2, sens_daily), 
                                targets=list(slot=c("gpp", "lai"), col=c("Total", "Smith2014")), 
                                name.map=Smith2014.scheme@cols)
p <- p + guides(col = guide_legend(ncol = 1))
print(p)

p <- plotGGCategorialAggregated(list(base, sens_constCO2, sens_daily),
                                targets=list(x=c("gpp", "Total"), y=c("lai", "Smith2014")), 
                                name.map=Smith2014.scheme@cols)
p <- p + guides(col = guide_legend(ncol = 1))
print(p)

### Arithmetics

## Add some more time averaged spatial data
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
  eval(parse(text=paste(run, "@spatial[['cpool']] <- getVegSpatial(",run,", period, 'cpool', forceReAveraging = FALSE)", sep="")))
  eval(parse(text=paste(run, "@spatial[['npp']] <- getVegSpatial(",run,", period, 'anpp', forceReAveraging = FALSE)", sep="")))
}



t <- list(x=c("spatial", "gpp"), y=c("spatial", "npp"))
xxx = calcNewVegObj(base, t, "/")




### Time series
for (run in c("base", "sens_constCO2", "sens_daily", "sens_CC", "sens_centr", "sens_CLM")) {
   eval(parse(text=paste(run, "@temporal[['npp']] <- getVegTemporal(",run,", 'anpp', forceReAveraging = FALSE)", sep="")))
   eval(parse(text=paste(run, "@temporal[['gpp']] <- getVegTemporal(",run,", 'agpp', forceReAveraging = FALSE)", sep="")))
   eval(parse(text=paste(run, "@temporal[['cpool']] <- getVegTemporal(",run,", 'cpool', forceReAveraging = FALSE)", sep="")))
}

## plot(base@timeseries$npp$Total*130 ~ base@timeseries$cpool$Year, type="l")


