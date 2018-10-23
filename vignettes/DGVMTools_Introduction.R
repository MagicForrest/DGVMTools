## ----packages, echo=FALSE------------------------------------------------
library(DGVMTools, quietly = TRUE, warn.conflicts = FALSE)
#library(raster, quietly = TRUE, warn.conflicts = FALSE)
#library(data.table, quietly = TRUE, warn.conflicts = FALSE)
#library(plyr, quietly = TRUE, warn.conflicts = FALSE)
#library(viridis, quietly = TRUE, warn.conflicts = FALSE)
#library(Cairo, quietly = TRUE, warn.conflicts = FALSE)


## ----setup, include=FALSE------------------------------------------------

## ----Define source, echo=TRUE--------------------------------------------

# a little bit of magic to get the path to the example data on your system
example.run.directory <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
print(example.run.directory)

GUESS.run <- defineSource(id = "LPJ-GUESS_Example",
                          dir = example.run.directory, # this would normlly just be a character string containing a path
                          format = GUESS,
                          name = "LPJ-GUESS Example Run")



## ----Source info, echo=TRUE----------------------------------------------
class(GUESS.run)
print(GUESS.run)

## ----Get data, echo=TRUE-------------------------------------------------
LAI.full <- getField(source = GUESS.run, 
                           var = "lai")

## ----Field info, echo=TRUE-----------------------------------------------
print(LAI.full)

## ----Get data with year aggregation, echo=TRUE---------------------------
LAI.year.mean <- getField(source = GUESS.run, 
                           var = "lai", 
                           year.aggregate.method = "mean")


## ----Check dimensions, echo = TRUE---------------------------------------
getDimInfo(LAI.full)
getDimInfo(LAI.year.mean)

## ----Get data with spatial aggregation, echo = TRUE----------------------
LAI.spatial.mean <- getField(source = GUESS.run, 
                           var = "lai", 
                           spatial.aggregate.method = "mean")
getDimInfo(LAI.spatial.mean)

## ----Spatial plot, out.width = "95%", fig.width = 8, fig.asp = 1, echo = TRUE----
print(plotSpatial(LAI.year.mean))


## ----Spatial plot layers, out.width = "95%", fig.width = 8, fig.asp = 0.5, echo = TRUE----
print(plotSpatial(LAI.year.mean, layers = c("TeBS", "Total")))


## ----Temporal plot, out.width = "95%", fig.width = 8, fig.asp = 1, echo = TRUE----
print(plotTemporal(LAI.spatial.mean))


## ----Fail plot, out.width = "95%", fig.width = 8, fig.asp = 1, echo = TRUE----
print(plotSpatial(LAI.spatial.mean))
print(plotTemporal(LAI.year.mean))

