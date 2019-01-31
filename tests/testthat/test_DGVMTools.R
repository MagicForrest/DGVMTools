library(testthat)
library(DGVMTools)
library(FireMIPTools)
library(stats)
library(compiler)



# SOURCE 
context("Source")

GUESS.CE.test.Source <- defineSource(id = "LPJ-GUESS_Example",
                            dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools"), 
                            format = GUESS,
                            name = "LPJ-GUESS Europe Example Run")


DGVMData.Saatchi.test.Source <- defineSource(id = "Saatchi2011",
                                     dir = system.file("extdata", "DGVMData", "Saatchi2011", "HD", package = "DGVMTools"), 
                                     format = DGVMData,
                                     name = "Saatchi et al. 2011 Vegetation Carbon")

# test Source and Field
test_that("Sources",{
  
  expect_is(GUESS.CE.test.Source, "Source")
  expect_is(DGVMData.Saatchi.test.Source, "Source")
  
})


# QUANTITY OBJECTS 
context("Quantity")

vegC_std.Quantity <- lookupQuantity("vegC_std")
LAI_FireMIP.Quantity <- lookupQuantity("lai", context = FireMIP.quantities)


test_that("Quantity",{

  expect_is(vegC_std.Quantity , "Quantity")
  expect_is(LAI_FireMIP.Quantity  , "Quantity")
  
})



# FIELD
context("Field")

test.field.full <- getField(GUESS.CE.test.Source, "mlai")
Saatchi.Field.full <- getField(DGVMData.Saatchi.test.Source, vegC_std.Quantity)

# test Source and Field
test_that("Field",{
  
  # check the quantity
  
  # Normal LPJ-GUESS variable
  expect_is(test.field.full, "Field")
  
  # Also check LPJ-GUESS, "Standard" and FireMIP variables
  GUESS.Field <- getField(GUESS.CE.test.Source, "lai")
  Standard.Field <- getField(GUESS.CE.test.Source, "LAI_std")
  FireMIP.Field <- getField(GUESS.CE.test.Source, LAI_FireMIP.Quantity)
  expect_is(GUESS.Field, "Field")
  expect_is(Standard.Field, "Field")
  expect_is(FireMIP.Field, "Field")
  
  # Check the data are the same
  expect_identical(Standard.Field@data, GUESS.Field@data)
  expect_identical(FireMIP.Field@data, GUESS.Field@data)
  
  # check the DGVMData
  expect_is(Saatchi.Field.full, "Field")
  
})


# AGGREGATIONS
context("Aggregations")

# subannual to monthly
test.field.monthly.mean.1 <- getField(GUESS.CE.test.Source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Year")
test.field.monthly.mean.2 <- aggregateSubannual(input.obj = test.field.full, method = "mean", target = "Year")
# subannual to seasonal
test.field.seasonal.mean.1 <- getField(GUESS.CE.test.Source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Season")
test.field.seasonal.mean.2 <- aggregateSubannual(input.obj = test.field.full, method = "mean", target = "Season")
# yearly
test.field.yearly.mean.1 <- getField(GUESS.CE.test.Source, "mlai", year.aggregate.method = "mean")
test.field.yearly.mean.2 <- aggregateYears(test.field.full, "mean")
# spatial
test.field.spatial.mean.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.aggregate.method = "mean")
test.field.spatial.mean.2 <- aggregateSpatial(test.field.full, "mean")


# test aggregations
test_that("Aggregation",{
  
  # check they give Field
  expect_is(test.field.monthly.mean.1, "Field")
  expect_is(test.field.monthly.mean.2, "Field")
  expect_is(test.field.seasonal.mean.1, "Field")
  expect_is(test.field.seasonal.mean.2, "Field")
  expect_is(test.field.yearly.mean.1, "Field")
  expect_is(test.field.yearly.mean.2, "Field")
  expect_is(test.field.spatial.mean.1, "Field")
  expect_is(test.field.spatial.mean.2, "Field")
  
  # check the results are the same
  expect_identical(test.field.monthly.mean.1 ,  test.field.monthly.mean.2)
  expect_identical(test.field.yearly.mean.1 ,  test.field.yearly.mean.2)
  expect_identical(test.field.spatial.mean.1 ,  test.field.spatial.mean.2)
  expect_identical(test.field.seasonal.mean.1 ,  test.field.seasonal.mean.2)
 
  
  
})


### SELECTIONS
context("Selections and Cropping")

# years
test.field.selected.years.1 <- getField(GUESS.CE.test.Source, "mlai", first.year = 2001, last.year = 2005)
test.field.selected.years.2 <- selectYears(x = test.field.full, first = 2001, last = 2005)

# months (not available in getField but test by numbers and abbreviation)
test.field.selected.months.1 <- selectMonths(x = test.field.full, months = c(1,4,12) )
test.field.selected.months.2 <- selectMonths(x = test.field.full, months = c("Jan","Apr","Dec") )

# days (not available in test data)

# seasons (not available in getField)
test.field.selected.seasons.1 <- selectSeasons(x = test.field.seasonal.mean.1, seasons = c("JJA", "SON") )

# single gridcell
test.gridcell <- c(16.25, 58.75)
test.field.selected.gridcell.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.extent = test.gridcell, spatial.extent.id = "TestGridcell")
test.field.selected.gridcell.2 <- selectGridcells(x = test.field.full, gridcells = test.gridcell, spatial.extent.id = "TestGridcell")

# by a data.frame
test.gridcells.df <- data.frame("Lon" = c(16.25, 7.25, 3.75), "Lat" = c(58.75, 49.25, 50.75))
test.field.selected.gridcells.df.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.extent = test.gridcells.df, spatial.extent.id = "TestGridcellsDF")
test.field.selected.gridcells.df.2 <- selectGridcells(x = test.field.full, gridcells = test.gridcells.df, spatial.extent.id = "TestGridcellsDF")

# by a data.table
test.gridcells.dt <- data.table("Lon" = c(16.25, 7.25, 3.75), "Lat" = c(58.75, 49.25, 50.75))
test.field.selected.gridcells.dt.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.extent = test.gridcells.dt, spatial.extent.id = "TestGridcellsDT")
test.field.selected.gridcells.dt.2 <- selectGridcells(x = test.field.full, gridcells = test.gridcells.dt, spatial.extent.id = "TestGridcellsDT")

# by a polygon?
# - not sure, maybe pull something from the maps package

# crop by a raster
test.raster <- raster::raster(ymn=48, ymx=59, xmn=4, xmx=17, resolution = 0.5, vals=0)
test.field.selected.raster.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.extent = test.raster, spatial.extent.id = "TestExtent")
test.field.selected.raster.2 <- crop(x = test.field.full, y = test.raster, spatial.extent.id = "TestExtent")

# crop by an extent 
test.extent <- extent(test.raster)
test.field.selected.extent.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.extent = test.extent, spatial.extent.id = "TestExtent")
test.field.selected.extent.2 <- crop(x = test.field.full, y = test.extent, spatial.extent.id = "TestExtent")

# crop by another Field 
test.field.selected.Field.1 <- getField(GUESS.CE.test.Source, "mlai", spatial.extent = test.field.selected.extent.1, spatial.extent.id = "TestExtent")
test.field.selected.Field.2 <- crop(x = test.field.full, y = test.field.selected.extent.1, spatial.extent.id = "TestExtent")




# test aggregations
test_that("Aggregation",{
  
  # check they give Fields
  expect_is(test.field.selected.years.1, "Field")
  expect_is(test.field.selected.years.2, "Field")
  expect_is(test.field.selected.months.1, "Field")
  expect_is(test.field.selected.months.2, "Field")
  expect_is(test.field.selected.seasons.1, "Field")
  expect_is(test.field.selected.gridcell.1, "Field")
  expect_is(test.field.selected.gridcell.2, "Field")
  expect_is(test.field.selected.gridcells.df.1, "Field")
  expect_is(test.field.selected.gridcells.df.2, "Field")
  expect_is(test.field.selected.gridcells.dt.1, "Field")
  expect_is(test.field.selected.gridcells.dt.2, "Field")
  expect_is(test.field.selected.raster.1, "Field")
  expect_is(test.field.selected.raster.2, "Field")
  expect_is(test.field.selected.extent.1, "Field")
  expect_is(test.field.selected.extent.2, "Field")
  expect_is(test.field.selected.Field.1, "Field")
  expect_is(test.field.selected.Field.2, "Field")
  
 
  # check the results are the same by two different routes
 
  expect_identical(test.field.selected.years.1,  test.field.selected.years.2)
  expect_identical(test.field.selected.months.1 ,  test.field.selected.months.2)
  expect_identical(test.field.selected.gridcell.1 ,  test.field.selected.gridcell.2)
  expect_identical(test.field.selected.gridcells.df.1,  test.field.selected.gridcells.df.2)
  expect_identical(test.field.selected.gridcells.dt.1,  test.field.selected.gridcells.dt.2)
  expect_identical(test.field.selected.raster.1,  test.field.selected.raster.2)
  expect_identical(test.field.selected.extent.1,  test.field.selected.extent.2)
  expect_identical(test.field.selected.Field.1,  test.field.selected.Field.2)
  expect_identical(test.field.selected.extent.1,  test.field.selected.Field.1)
  
  expect_identical(test.field.selected.raster.1,  test.field.selected.Field.1)

})




### PLOTTING - expand these to test more plotting options
context("Plotting")


test_that("Plotting", {
  
  # spatial plotting
  expect_is(plotSpatial(test.field.monthly.mean.1), "ggplot")
  expect_is(plotSpatial(test.field.yearly.mean.1), "ggplot")
  
  # temporal plotting
  expect_is(plotTemporal(test.field.spatial.mean.1), "ggplot")
  
  # seaconal cycle plotting -- update after plotSeasonal rebuilt
  #expect_is(plotSeasonal(test.field.spatial.mean.1), "ggplot")
  
 
})



### READ DGVMData 



### COMPARISIONS AND BENCHMARKING


###  EXPORTING

