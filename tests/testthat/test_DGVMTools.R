library(testthat)
library(DGVMTools)
library(stats)
library(compiler)



# SOURCE 
context("Source")

GUESS.Europe.test.Source <- defineSource(id = "LPJ-GUESS_Example",
                                         dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools"), 
                                         format = GUESS,
                                         name = "LPJ-GUESS Europe Example Run")

GUESS.Africa.test.Source <- defineSource(id = "LPJ-GUESS_Example",
                                         dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralAfrica", package = "DGVMTools"), 
                                         format = GUESS,
                                         name = "LPJ-GUESS Africa Example Run")


DGVMData.PNVBiomes.test.Source <- defineSource(id = "HandP_PNV",
                                               dir = system.file("extdata", "DGVMData", "HandP_PNV", "HD", package = "DGVMTools"), 
                                               format = DGVMData,
                                               name = "Haxeltine & Prentice 1996 PNV Biomes")


DGVMData.SaatchiBiomass.test.Source <- defineSource(id = "Saatchi2011",
                                                    dir = system.file("extdata", "DGVMData", "Saatchi2011", "HD", package = "DGVMTools"), 
                                                    format = DGVMData,
                                                    name = "Saatchi et al. 2011 Vegetation Carbon")

# test Source
test_that("Sources",{
  
  expect_is(GUESS.Europe.test.Source, "Source")
  expect_is(GUESS.Africa.test.Source, "Source")
  expect_is(DGVMData.SaatchiBiomass.test.Source, "Source")
  expect_is(DGVMData.PNVBiomes.test.Source, "Source")
  
})


# QUANTITY OBJECTS 
context("Quantity")

vegC_std.Quantity <- lookupQuantity("vegC_std")


test_that("Quantity",{
  
  expect_is(vegC_std.Quantity , "Quantity")

  expect_is(availableQuantities(GUESS.Europe.test.Source, names = TRUE), "character")
  expect_is(availableQuantities(GUESS.Europe.test.Source, names = FALSE), "list")
  
  expect_is(availableQuantities(GUESS.Africa.test.Source, names = TRUE), "character")
  expect_is(availableQuantities(GUESS.Africa.test.Source, names = FALSE), "list")
  
  expect_is(availableQuantities(DGVMData.SaatchiBiomass.test.Source, names = TRUE), "character")
  expect_is(availableQuantities(DGVMData.SaatchiBiomass.test.Source, names = FALSE), "list")
  
  expect_is(availableQuantities(DGVMData.PNVBiomes.test.Source, names = TRUE), "character")
  expect_is(availableQuantities(DGVMData.PNVBiomes.test.Source, names = FALSE), "list")
  
  
  
  
})



# FIELD
context("Field")

# get Fields for checking and also using later
GUESS.mlai.Field.full <- getField(GUESS.Europe.test.Source, "mlai")
GUESS.lai.Field.full <- getField(GUESS.Europe.test.Source, "lai")
GUESS.cmass.Field.full <- getField(GUESS.Africa.test.Source, "cmass")
GUESS.vegC_std.Field.full <- getField(GUESS.Africa.test.Source, vegC_std.Quantity)
Saatchi.Field.full <- getField(DGVMData.SaatchiBiomass.test.Source, vegC_std.Quantity)
Biomes.Field.full <- getField(DGVMData.PNVBiomes.test.Source, "Smith2014")


# test Fields
test_that("Field",{
  
  # Normal LPJ-GUESS variable
  expect_is(GUESS.mlai.Field.full, "Field")
  expect_is(GUESS.lai.Field.full, "Field")
  expect_is(GUESS.cmass.Field.full, "Field")
  expect_is(GUESS.vegC_std.Field.full, "Field")
  expect_is(Saatchi.Field.full, "Field")
  expect_is(Biomes.Field.full, "Field")
  
  
  # Also check "Standard" variables
  Standard.Field <- getField(GUESS.Europe.test.Source, "LAI_std")
  expect_is(Standard.Field, "Field")

  # Check the data are the same
  expect_identical(Standard.Field@data, GUESS.lai.Field.full@data)

  # check the DGVMData
  expect_is(Saatchi.Field.full, "Field")
  expect_is(Biomes.Field.full, "Field")
  
  
})

# AVERAGING FIELDS

context("Averaging Fields")


# test Fields
test_that("Field",{
  
  # open a another (actually duplicate) Field for averaging
  GUESS.Europe.test.Source.2 <- defineSource(id = "LPJ-GUESS_Example_2",
                                           dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools"), 
                                           format = GUESS,
                                           name = "LPJ-GUESS Europe Example Run Duplicate")
  GUESS.lai.Field.full.2 <- getField(GUESS.Europe.test.Source.2, "lai")
  
  # do average (and also standard deviation)
  GUESS.lai.Field.full.mean <- averageFields(list(GUESS.lai.Field.full, GUESS.lai.Field.full.2))
  GUESS.lai.Field.full.sd <- averageFields(list(GUESS.lai.Field.full, GUESS.lai.Field.full.2), method = sd)

  # and checks
  expect_is(GUESS.lai.Field.full.mean, "Field")
  expect_is(GUESS.lai.Field.full.sd, "Field")
  expect_identical(GUESS.lai.Field.full.mean@data, GUESS.lai.Field.full@data)
  
  # expect standard deviation to be zero everywhere (since the files were the same)
  for(this.layer in layers(GUESS.lai.Field.full.sd)) {
      expect_identical(sum(GUESS.lai.Field.full.sd@data[[this.layer]]), 0)
  }
  
  
  
})


# Layers
context("Layers")

# test Layers
test_that("Layers",{
  
  
  # Test return Layer objects from a Field with different criteria (and also NULL - ie no criteria)
  all.criteria = list("Grass", "Tree", "Evergreen", "Summergreen", "Raingreen", "Boreal", "Temperate", "Tropical", "Needleleaved", "Broadleaved")
  
  for(criteria in all.criteria) {
    
    # get list of Layers from a Field
    list.Layers <- whichLayers(x = GUESS.lai.Field.full, criteria = criteria, return.ids = FALSE)
    expect_is(list.Layers, "list")
    for(Layer in list.Layers) {
      expect_is(Layer, "Layer")
    }
    
    # get vector of Layer ids (character strings) from a Field
    list.Layers <- whichLayers(x = GUESS.lai.Field.full, criteria = criteria, return.ids = TRUE)
    expect_is(list.Layers, "character")
    for(Layer in list.Layers) {
      expect_is(Layer, "character")
    }
    
    # get list of Layers from a list of Layers
    list.Layers <- whichLayers(x = GUESS@defined.layers, criteria = criteria, return.ids = FALSE)
    expect_is(list.Layers, "list")
    for(Layer in list.Layers) {
      expect_is(Layer, "Layer")
    }
    
    # get vector of Layer ids from a list of Layers
    list.Layers <- whichLayers(x = GUESS@defined.layers, criteria = criteria, return.ids = TRUE)
    expect_is(list.Layers, "character")
    for(Layer in list.Layers) {
      expect_is(Layer, "character")
    }
    
  }
  
})



# AGGREGATIONS
context("Aggregations")

# subannual to monthly
GUESS.Field.monthly.mean.1 <- getField(GUESS.Europe.test.Source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Year")
GUESS.Field.monthly.mean.2 <- aggregateSubannual(input.obj = GUESS.mlai.Field.full, method = "mean", target = "Year")
# subannual to seasonal
GUESS.Field.seasonal.mean.1 <- getField(GUESS.Europe.test.Source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Season")
GUESS.Field.seasonal.mean.2 <- aggregateSubannual(input.obj = GUESS.mlai.Field.full, method = "mean", target = "Season")
# yearly
GUESS.Field.yearly.mean.1 <- getField(GUESS.Europe.test.Source, "mlai", year.aggregate.method = "mean")
GUESS.Field.yearly.mean.2 <- aggregateYears(GUESS.mlai.Field.full, "mean")
# spatial
GUESS.Field.spatial.mean.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.aggregate.method = "mean")
GUESS.Field.spatial.mean.2 <- aggregateSpatial(GUESS.mlai.Field.full, "mean")
# spatial weighted sum
GUESS.Field.spatial.wmean.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.aggregate.method = "w.mean")
GUESS.Field.spatial.wmean.2 <- aggregateSpatial(GUESS.mlai.Field.full, "w.mean")

# test aggregations
test_that("Aggregation",{
  
  # check they give Field
  expect_is(GUESS.Field.monthly.mean.1, "Field")
  expect_is(GUESS.Field.monthly.mean.2, "Field")
  expect_is(GUESS.Field.seasonal.mean.1, "Field")
  expect_is(GUESS.Field.seasonal.mean.2, "Field")
  expect_is(GUESS.Field.yearly.mean.1, "Field")
  expect_is(GUESS.Field.yearly.mean.2, "Field")
  expect_is(GUESS.Field.spatial.mean.1, "Field")
  expect_is(GUESS.Field.spatial.mean.2, "Field")
  expect_is(GUESS.Field.spatial.wmean.1, "Field")
  expect_is(GUESS.Field.spatial.wmean.2, "Field")
  
  # check the results are the same
  expect_identical(GUESS.Field.monthly.mean.1 ,  GUESS.Field.monthly.mean.2)
  expect_identical(GUESS.Field.yearly.mean.1 ,  GUESS.Field.yearly.mean.2)
  expect_identical(GUESS.Field.seasonal.mean.1 ,  GUESS.Field.seasonal.mean.2)
  expect_identical(GUESS.Field.spatial.mean.1 ,  GUESS.Field.spatial.mean.2)
  expect_identical(GUESS.Field.spatial.wmean.1 ,  GUESS.Field.spatial.wmean.2)
  
  
  
})


### SELECTIONS
context("Selections and Cropping")

# years
GUESS.Field.selected.years.1 <- getField(GUESS.Europe.test.Source, "mlai", first.year = 2001, last.year = 2005)
GUESS.Field.selected.years.2 <- selectYears(x = GUESS.mlai.Field.full, first = 2001, last = 2005)

# months (not available in getField but test by numbers and abbreviation)
GUESS.Field.selected.months.1 <- selectMonths(x = GUESS.mlai.Field.full, months = c(1,4,12) )
GUESS.Field.selected.months.2 <- selectMonths(x = GUESS.mlai.Field.full, months = c("Jan","Apr","Dec") )

# days (not available in test data)

# seasons (not available in getField)
GUESS.Field.selected.seasons.1 <- selectSeasons(x = GUESS.Field.seasonal.mean.1, seasons = c("JJA", "SON") )

# single gridcell
test.gridcell <- c(16.25, 58.75)
GUESS.Field.selected.gridcell.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.extent = test.gridcell, spatial.extent.id = "TestGridcell")
GUESS.Field.selected.gridcell.2 <- selectGridcells(x = GUESS.mlai.Field.full, gridcells = test.gridcell, spatial.extent.id = "TestGridcell")

# by a data.frame
test.gridcells.df <- data.frame("Lon" = c(16.25, 7.25, 3.75), "Lat" = c(58.75, 49.25, 50.75))
GUESS.Field.selected.gridcells.df.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.extent = test.gridcells.df, spatial.extent.id = "TestGridcellsDF")
GUESS.Field.selected.gridcells.df.2 <- selectGridcells(x = GUESS.mlai.Field.full, gridcells = test.gridcells.df, spatial.extent.id = "TestGridcellsDF")

# by a data.table
test.gridcells.dt <- data.table("Lon" = c(16.25, 7.25, 3.75), "Lat" = c(58.75, 49.25, 50.75))
GUESS.Field.selected.gridcells.dt.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.extent = test.gridcells.dt, spatial.extent.id = "TestGridcellsDT")
GUESS.Field.selected.gridcells.dt.2 <- selectGridcells(x = GUESS.mlai.Field.full, gridcells = test.gridcells.dt, spatial.extent.id = "TestGridcellsDT")

# by a polygon?
# - not sure, maybe pull something from the maps package

# crop by a raster
test.raster <- raster::raster(ymn=48, ymx=59, xmn=4, xmx=17, resolution = 0.5, vals=0)
GUESS.Field.selected.raster.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.extent = test.raster, spatial.extent.id = "TestExtent")
GUESS.Field.selected.raster.2 <- crop(x = GUESS.mlai.Field.full, y = test.raster, spatial.extent.id = "TestExtent")

# crop by an extent 
test.extent <- extent(test.raster)
GUESS.Field.selected.extent.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.extent = test.extent, spatial.extent.id = "TestExtent")
GUESS.Field.selected.extent.2 <- crop(x = GUESS.mlai.Field.full, y = test.extent, spatial.extent.id = "TestExtent")

# crop by another Field 
GUESS.Field.selected.Field.1 <- getField(GUESS.Europe.test.Source, "mlai", spatial.extent = GUESS.Field.selected.extent.1, spatial.extent.id = "TestExtent")
GUESS.Field.selected.Field.2 <- crop(x = GUESS.mlai.Field.full, y = GUESS.Field.selected.extent.1, spatial.extent.id = "TestExtent")




# test aggregations
test_that("Selections and Cropping",{
  
  # check they give Fields
  expect_is(GUESS.Field.selected.years.1, "Field")
  expect_is(GUESS.Field.selected.years.2, "Field")
  expect_is(GUESS.Field.selected.months.1, "Field")
  expect_is(GUESS.Field.selected.months.2, "Field")
  expect_is(GUESS.Field.selected.seasons.1, "Field")
  expect_is(GUESS.Field.selected.gridcell.1, "Field")
  expect_is(GUESS.Field.selected.gridcell.2, "Field")
  expect_is(GUESS.Field.selected.gridcells.df.1, "Field")
  expect_is(GUESS.Field.selected.gridcells.df.2, "Field")
  expect_is(GUESS.Field.selected.gridcells.dt.1, "Field")
  expect_is(GUESS.Field.selected.gridcells.dt.2, "Field")
  expect_is(GUESS.Field.selected.raster.1, "Field")
  expect_is(GUESS.Field.selected.raster.2, "Field")
  expect_is(GUESS.Field.selected.extent.1, "Field")
  expect_is(GUESS.Field.selected.extent.2, "Field")
  expect_is(GUESS.Field.selected.Field.1, "Field")
  expect_is(GUESS.Field.selected.Field.2, "Field")
  
  
  # check the results are the same by two different routes
  expect_identical(GUESS.Field.selected.years.1,  GUESS.Field.selected.years.2)
  expect_identical(GUESS.Field.selected.months.1 ,  GUESS.Field.selected.months.2)
  expect_identical(GUESS.Field.selected.gridcell.1 ,  GUESS.Field.selected.gridcell.2)
  expect_identical(GUESS.Field.selected.gridcells.df.1,  GUESS.Field.selected.gridcells.df.2)
  expect_identical(GUESS.Field.selected.gridcells.dt.1,  GUESS.Field.selected.gridcells.dt.2)
  expect_identical(GUESS.Field.selected.raster.1,  GUESS.Field.selected.raster.2)
  expect_identical(GUESS.Field.selected.extent.1,  GUESS.Field.selected.extent.2)
  expect_identical(GUESS.Field.selected.Field.1,  GUESS.Field.selected.Field.2)
  expect_identical(GUESS.Field.selected.extent.1,  GUESS.Field.selected.Field.1)
  expect_identical(GUESS.Field.selected.raster.1,  GUESS.Field.selected.Field.1)
  
})




### PLOTTING - expand these to test more plotting options
context("Plotting")


test_that("Plotting", {
  
  # spatial plotting
  expect_is(plotSpatial(GUESS.Field.monthly.mean.1), "ggplot")
  expect_is(plotSpatial(GUESS.Field.yearly.mean.1), "ggplot")
  expect_is(plotSpatial(Biomes.Field.full), "ggplot")
  expect_is(plotSpatial(Saatchi.Field.full), "ggplot")
  
  # test plot options?
  
  # temporal plotting
  expect_is(plotTemporal(GUESS.Field.spatial.mean.1), "ggplot")
  
  # subannual cycle plotting
  expect_is(plotSubannual(GUESS.Field.spatial.mean.1), "ggplot")

})





### NUMERIC COMPARISONS AND BENCHMARKING
context("Numeric Comparisons and Benchmarks")

test_that("Numeric Comparisons and Benchmarks", {
  
  # build and test a numeric Comparison
  GUESS.Field.vegC_std.annual <- aggregateYears(GUESS.vegC_std.Field.full, "mean")
  GUESS.Field.vegC_std.annual <- layerOp(GUESS.Field.vegC_std.annual, "+", ".Tree", "Tree")
  expect_is(GUESS.Field.vegC_std.annual, "Field")
  Saatchi.comparison <- compareLayers(GUESS.Field.vegC_std.annual, Saatchi.Field.full, layers1 = "Tree", verbose = FALSE, show.stats = FALSE)
  expect_is(Saatchi.comparison, "Comparison")
  
  # plot said numeric Comparison
  expect_is(plotSpatialComparison(Saatchi.comparison), "ggplot")
  expect_is(plotSpatialComparison(Saatchi.comparison, type = "difference"), "ggplot")
  expect_is(plotSpatialComparison(Saatchi.comparison, type = "percentage.difference"), "ggplot")
  expect_is(plotSpatialComparison(Saatchi.comparison, type = "values"), "ggplot")
  # expect_is(plotSpatialComparison(Saatchi.comparison, type = "nme"), "ggplot")
  
  # test with a dummy benchmark
  dummy.benchmark <- function(x, layer1, layer2) { return(1)}
  Saatchi.comparison.with.dummy.benchmark <- compareLayers(GUESS.Field.vegC_std.annual, Saatchi.Field.full, layers1 = "Tree", verbose = FALSE, custom.metrics = list("Dummy" = dummy.benchmark), show.stats = FALSE)
  expect_is(Saatchi.comparison.with.dummy.benchmark, "Comparison")
  
  # can possible place extra tests on metrics here
  
  
})

### CATEGORICAL QUANTITIES
context("Categorical Quantities")

# biomes
# Note: Known and deliberate warning when calculating biomes, suppress for clarity in results
GUESS.Smith2014.Biomes <- suppressWarnings(getScheme(source = GUESS.Europe.test.Source, scheme = Smith2014BiomeScheme, year.aggregate.method = "mean"))
GUESS.Forrest2015.Biomes <- suppressWarnings(getScheme(source = GUESS.Europe.test.Source, scheme = Forrest2015BiomeScheme, year.aggregate.method = "mean"))

# max PFT
GUESS.Field.lai.annual <- aggregateYears(GUESS.lai.Field.full, "mean")
GUESS.Field.lai.annual <- layerOp(GUESS.Field.lai.annual, operator = "max.layer", ".PFTs", "MaxPFT")


test_that("Categorical Quantities", {
  
  expect_is(GUESS.Smith2014.Biomes, "Field")
  expect_is(GUESS.Forrest2015.Biomes, "Field")
  expect_is(GUESS.Field.lai.annual, "Field")
  
  ### Test averaging feature of get biomes
  
  # copy of Source for doing average biomes
  GUESS.Europe.test.Source.2 <- defineSource(id = "LPJ-GUESS_Example_2",
                                           dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools"), 
                                           format = GUESS,
                                           name = "LPJ-GUESS Europe Example Run 2 (copy)")
  
  # Dummy Source for averaged biomes
  expect_is(GUESS.Europe.test.Source.Averaged <- defineSource(id = "LPJ-GUESS_Example_Averaged",
                                             dir = getwd(), 
                                             format = GUESS,
                                             name = "LPJ-GUESS Europe Example Run Averaged"),
            "Source")
  
  GUESS.Smith2014.Biomes.averaged <- suppressWarnings(getScheme(source = list(GUESS.Europe.test.Source, GUESS.Europe.test.Source), 
                                                                scheme = Smith2014BiomeScheme, 
                                                                year.aggregate.method = "mean",
                                                                averaged.source = GUESS.Europe.test.Source.Averaged)
                                                      )
  
  expect_is(GUESS.Smith2014.Biomes.averaged, "Field")
  
  expect_is(plotSpatial(GUESS.Smith2014.Biomes), "ggplot")
  expect_is(plotSpatial(GUESS.Smith2014.Biomes.averaged), "ggplot")
  expect_is(plotSpatial(list(GUESS.Smith2014.Biomes, GUESS.Smith2014.Biomes.averaged)), "ggplot")
  expect_identical(GUESS.Smith2014.Biomes@data, GUESS.Smith2014.Biomes.averaged@data)
  expect_is(plotSpatial(GUESS.Field.lai.annual, layer = "MaxPFT"), "ggplot")
  
})


### CATEGORIAL COMPARISONS AND BENCHMARKING
context("Categorical Comparisons and Benchmarks")

test_that("Categorical Comparisons and Benchmarks", {
  
  # build and test a categorical Comparison
  Biomes.comparison <- compareLayers(GUESS.Smith2014.Biomes, Biomes.Field.full, layers1 = "Smith2014", verbose = FALSE, show.stats = FALSE)
  expect_is(Biomes.comparison, "Comparison")
  
  # plot said categorical Comparison
  expect_is(plotSpatialComparison(Biomes.comparison), "ggplot")
  expect_is(plotSpatialComparison(Biomes.comparison, type = "difference"), "ggplot")
  expect_warning(plotSpatialComparison(Biomes.comparison, type = "percentage.difference"))
  expect_is(plotSpatialComparison(Biomes.comparison, type = "values"), "ggplot")

  # test with a dummy benchmark
  dummy.benchmark <- function(x, layer1, layer2) { return(1)}
  Biomes.comparison.with.dummy.benchmark <- compareLayers(GUESS.Smith2014.Biomes, Biomes.Field.full, layers1 = "Smith2014", verbose = FALSE, custom.metrics = list("Dummy" = dummy.benchmark), show.stats = FALSE)
  expect_is(Biomes.comparison.with.dummy.benchmark, "Comparison")
  
})


### SEASONAL COMPARISONS AND BENCHMARKING
context("Seasonal Comparisons and Benchmarks")

test_that("Seasonal Comparisons and Benchmarks", {
  
  test.Field.2000_2005 <-  getField(source = GUESS.Europe.test.Source, var = "mlai", year.aggregate.method = "mean", first.year = 2000, last.year = 2005)
  test.Field.2006_2010 <-  getField(source = GUESS.Europe.test.Source, var = "mlai", year.aggregate.method = "mean", first.year = 2006, last.year = 2010)
  
  # build and test a seasonal Comparison
  Seasonal.comparison <- compareLayers(test.Field.2000_2005, test.Field.2006_2010, layers1 = "mlai", do.seasonality = TRUE, verbose = FALSE, show.stats = FALSE)
  expect_is(Seasonal.comparison, "Comparison")
  
  # plot said seasonal Comparison
  expect_is(plotSpatialComparison(Seasonal.comparison), "ggplot")
  expect_is(plotSpatialComparison(Seasonal.comparison, type = "values"), "ggplot")
  expect_is(plotSpatialComparison(Seasonal.comparison, do.phase = TRUE), "ggplot")
  expect_is(plotSpatialComparison(Seasonal.comparison, type = "values", do.phase = TRUE), "ggplot")
  
  
  # test with a dummy benchmark
  dummy.benchmark <- function(x, layer1, layer2) { return(1) }
  Seasonal.comparison.with.dummy.benchmark <- compareLayers(test.Field.2000_2005, test.Field.2006_2010, layers1 = "mlai", do.seasonality = TRUE, verbose = FALSE, custom.metrics = list("Dummy" = dummy.benchmark), show.stats = FALSE)
  expect_is(Seasonal.comparison.with.dummy.benchmark, "Comparison")
  
})


### ADD AREA FUNCTION
context("Add Area")

test_that("Add Area", {
  
  # simple m^2 case
  area.m2 <- addArea(GUESS.Field.lai.annual)
  expect_is(area.m2 , "Field")
  
  # km^2 case
  area.km2 <- addArea(GUESS.Field.lai.annual, unit = "km^2")
  expect_is(area.km2 , "Field")
  
  # ha case
  area.ha <- addArea(GUESS.Field.lai.annual, unit = "ha")
  expect_is(area.ha , "Field")
  
  # case with ellipse instead of circle
  area.m2.ellipse <- addArea(GUESS.Field.lai.annual, ellipse = TRUE)
  expect_is(area.m2.ellipse  , "Field")
  
  # check the sums 
  expect_identical(sum(area.m2@data[["Area"]]), sum(area.km2@data[["Area"]]) * 10^6)
  expect_identical(sum(area.m2@data[["Area"]]), sum(area.ha@data[["Area"]]) * 10^4)
  
})


###  EXPORTING

