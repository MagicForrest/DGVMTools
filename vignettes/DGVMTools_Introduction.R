## ----packages and setup, echo=FALSE--------------------------------------
  knitr::opts_chunk$set(echo=FALSE, fig.width=8, autodep=TRUE)
library(DGVMTools, quietly = TRUE, warn.conflicts = FALSE)


## ----Define source, echo=TRUE--------------------------------------------

# a little bit of magic to get the path to the example data included in the package on your system
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

## ----layerOp, echo=TRUE--------------------------------------------------

# calculate tree total the long way around
LAI.year.mean <- layerOp(x = LAI.year.mean, operator = "+", layers = c("BINE", "BNE", "BNS", "IBS", "TeBS", "TeBE", "TeNE", "TrBE", "TrIBE", "TrBR"), new.layer = "TreeTotal1")

# calculate tree total using dot notation
LAI.year.mean <- layerOp(x = LAI.year.mean, operator = "+", layers = ".Tree", new.layer = "TreeTotal2")

# check the difference (subtract, print and plot)
LAI.year.mean <- layerOp(x = LAI.year.mean, operator = "-", layers = c("TreeTotal1", "TreeTotal2"), new.layer = "TreeTotalDiff")
print(LAI.year.mean)
print(plotSpatial(LAI.year.mean, c("TreeTotal1", "TreeTotal2", "TreeTotalDiff")))


## ----layerOp max, echo=TRUE----------------------------------------------

# calculate maximum PFT 
LAI.year.mean <- layerOp(LAI.year.mean, "max.layer", ".PFT", "MaxPFT")

# calculate maximum  Tree
LAI.year.mean <- layerOp(LAI.year.mean, "max.layer", ".Tree", "MaxTree")

# plot
print(plotSpatial(LAI.year.mean, c("MaxPFT", "MaxTree")))

# calculate total tree and grass totals.  Note that further to `"+"` as used above, addition of layers can be defined with the with character string `"sum"` or and arbitary function i.e. `sum`
LAI.year.mean <- layerOp(LAI.year.mean, "sum", ".Grass", "Grass")
LAI.year.mean <- layerOp(LAI.year.mean, sum, ".Tree", "Tree")


# Calculate if trees or grasssis dominant 
LAI.year.mean <- layerOp(LAI.year.mean, "max.layer", c("Tree", "Grass"), "MaxLifeform")

# plot
print(plotSpatial(LAI.year.mean, c("MaxLifeform")))

## ----layerOp fraction, echo=TRUE-----------------------------------------

# calculate maximum PFT 
LAI.year.mean <- layerOp(LAI.year.mean, "/", c("Tree", "Total"), "TreeFraction")
LAI.year.mean <- layerOp(LAI.year.mean, "/", c("Grass", "Total"), "GrassFraction")

# plot
print(plotSpatial(LAI.year.mean, c("TreeFraction", "GrassFraction")))

## ----open data, echo=TRUE------------------------------------------------

# new Source (different model run, this time over fFrica)
GUESS.Africa.run <- defineSource(id = "LPJ-GUESS_Example_Africa",
                                 dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralAfrica", package = "DGVMTools"), 
                                 format = GUESS,
                                 name = "LPJ-GUESS Africa Example Run")

# get standard variable vegC_std between the years 2000 and 2010
GUESS.vegC <- getField(GUESS.Africa.run,
                       "vegC_std",
                       first.year = 2000,
                       last.year = 2010,
                       year.aggregate.method = "mean")

# calculate Tree total and have a plot
GUESS.vegC <- layerOp(GUESS.vegC, "+", ".Tree", "Tree")
print(plotSpatial(GUESS.vegC, "Tree"))


# define the data Source (Saatchi biomass data @ HD)
Saatchi.dataset <- defineSource(id = "Saatch2011",
                                name = "Saatchi et al. 2011 tropical biomass",
                                format = DGVMData,
                                dir = system.file("extdata", "DGVMData", "Saatchi2011", "HD", package = "DGVMTools"))

# get the data field
Saatchi.vegC <- getField(source = Saatchi.dataset,
                         var = "vegC_std")

# have a look and plot
print(Saatchi.vegC)
print(plotSpatial(Saatchi.vegC))

## ----comparison, fig.asp = 1, echo=TRUE----------------------------------
# compare layers to produce a Comparison object
vegC.comparison <- compareLayers(field1 = GUESS.vegC, field2 = Saatchi.vegC, layers1 = "Tree", layers2 = "Tree")

# have a look at this comparison object (lots of metadata tracked)
print(vegC.comparison )

# plot difference map
print(plotSpatialComparison(vegC.comparison))

# plot side-by-side
print(plotSpatialComparison(vegC.comparison, type = "values"))

# make scatter plot
print(plotScatterComparison(vegC.comparison))


## ----select, fig.asp = 1, echo=TRUE--------------------------------------

# define a gridecll and the get a Field of the monthly LAI for the gridcell
gridcell <- data.frame(Lon = c(0.25), Lat = c(51.25))
London.mlai <- getField(source = GUESS.run, 
                     var = "mlai",
                     spatial.extent = gridcell,
                     spatial.extent.id = "London baby")


# plot LAI
print(plotSubannual(field = London.mlai,
                   year.col.gradient = TRUE,
                   alpha = 0.5))


## ----model biomes, echo=TRUE---------------------------------------------

# read and plot global biomes
biomes.model <- getScheme(GUESS.run, Smith2014BiomeScheme, year.aggregate.method = "mean")
#print(plotSpatial(biomes.model, map.overlay = "world"))
print(plotSpatial(biomes.model))

## ----data biomes, echo=TRUE----------------------------------------------

# data biomes source
biomes.source <- defineSource(id = "DataBiomes",
                              dir = system.file("extdata", "DGVMData", "HandP_PNV", "HD", package = "DGVMTools"), 
                              format = DGVMData,
                              name = "Haxeltine and Prentice Biomes")
biomes.data <- getField(source = biomes.source, var = "Smith2014")
print(plotSpatial(biomes.data))


## ----biomes comparison, echo=TRUE----------------------------------------

biome.comparison <- compareLayers(field1 = biomes.model, field2 = biomes.data, layers1 = "Smith2014", layers2 = "Smith2014")

# have a look at this comparison object
print(biome.comparison )

# plot side-by-side
print(plotSpatialComparison(biome.comparison, type = "values"))

# plot difference map
print(plotSpatialComparison(biome.comparison, type = "difference"))



## ----facetting, echo=TRUE------------------------------------------------
# make a plot with silly facets
lifeform.plot <- plotSpatial(LAI.year.mean, layers = c("Tree", "Grass", "Total"), ylim = c(35,45))
print(lifeform.plot)

# arrange facets with three rows
lifeform.plot <- lifeform.plot + facet_wrap(~Facet, nrow =3)
print(lifeform.plot)

## ----legends, echo=TRUE--------------------------------------------------

# ove and format legends, and make the text slightly smaller
biome.plot <- plotSpatialComparison(biome.comparison, type = "values")
print(biome.plot)
biome.plot <- biome.plot + theme(legend.position = "bottom", text = element_text(size = theme_get()$text$size * 0.75))
biome.plot <- biome.plot+ guides(fill = guide_legend(ncol = 2))
print(biome.plot)

