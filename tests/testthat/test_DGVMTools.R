library(testthat)
library(DGVMTools)
library(stats)
library(compiler)

context("DGVMTools")

# SIMPLE SOURCE AND FIELD

test.source <- defineSource(id = "LPJ-GUESS_Example",
                            dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools"), 
                            format = GUESS,
                            name = "LPJ-GUESS Europe Example Run")
test.field.full <- getField(test.source, "mlai")

# test Source and Field
test_that("Sources and Fields",{
 
  expect_is(test.source, "Source")
  expect_is(test.field.full, "Field")
  
  
})


# AGGREGATIONS

# subannual to monthly
test.field.monthly.mean.1 <- getField(test.source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Year")
test.field.monthly.mean.2 <- aggregateSubannual(input.obj = test.field.full, method = "mean", target = "Year")
# subannual to seasonal
test.field.seasonal.mean.1 <- getField(test.source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Season")
test.field.seasonal.mean.2 <- aggregateSubannual(input.obj = test.field.full, method = "mean", target = "Season")
# yearly
test.field.yearly.mean.1 <- getField(test.source, "mlai", year.aggregate.method = "mean")
test.field.yearly.mean.2 <- aggregateYears(test.field.full, "mean")
# spatial
test.field.spatial.mean.1 <- getField(test.source, "mlai", spatial.aggregate.method = "mean")
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
  
  # checl the results are the same
  expect_identical(test.field.monthly.mean.1@data ,  test.field.monthly.mean.2@data)
  expect_identical(test.field.yearly.mean.1@data ,  test.field.yearly.mean.2@data)
  expect_identical(test.field.spatial.mean.1@data ,  test.field.spatial.mean.2@data)
  expect_identical(test.field.seasonal.mean.1@data ,  test.field.seasonal.mean.2@data)
  
  # MF NOTE:  metadata by both methods not identical, work on this.
  
  
})


### SELECTIONS

# years
test.field.selected.years.1 <- getField(test.source, "mlai", first.year = 2001, last.year = 2005)
test.field.selected.years.2 <- selectYears(x = test.field.full, first = 2001, last = 2005)

# months (not available in getField but test by numbers and abbreviation)
test.field.selected.months.1 <- selectMonths(x = test.field.full, months = c(1,4,12) )
test.field.selected.months.2 <- selectMonths(x = test.field.full, months = c("Jan","Apr","Dec") )

# days (not available in test data)

# seasons (not available in getField)
test.field.selected.seasons.1 <- selectSeasons(x = test.field.seasonal.mean.1, seasons = c("JJA", "SON") )

# single gridcell
test.gridcell <- c(16.25, 58.75)
test.field.selected.gridcell.1 <- getField(test.source, "mlai", spatial.extent = test.gridcell, spatial.extent.id = "TestGridcell")
test.field.selected.gridcell.2 <- selectGridcells(x = test.field.full, gridcells = test.gridcell, spatial.extent.id = "TestGridcell")

# by a data.frame
test.gridcells.df <- data.frame("Lon" = c(16.25, 7.25, 3.75), "Lat" = c(58.75, 49.25, 50.75))
test.field.selected.gridcells.df.1 <- getField(test.source, "mlai", spatial.extent = test.gridcells.df, spatial.extent.id = "TestGridcellsDF")
test.field.selected.gridcells.df.2 <- selectGridcells(x = test.field.full, gridcells = test.gridcells.df, spatial.extent.id = "TestGridcellsDF")

# by a data.table
test.gridcells.dt <- data.table("Lon" = c(16.25, 7.25, 3.75), "Lat" = c(58.75, 49.25, 50.75))
test.field.selected.gridcells.dt.1 <- getField(test.source, "mlai", spatial.extent = test.gridcells.dt, spatial.extent.id = "TestGridcellsDT")
test.field.selected.gridcells.dt.2 <- selectGridcells(x = test.field.full, gridcells = test.gridcells.dt, spatial.extent.id = "TestGridcellsDT")





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
  
 
  # checl the results are the same
  expect_identical(test.field.selected.years.1@data ,  test.field.selected.years.2@data)
  expect_identical(test.field.selected.months.1@data ,  test.field.selected.months.2@data)
  expect_identical(test.field.selected.gridcell.1@data ,  test.field.selected.gridcell.2@data)
  expect_identical(test.field.selected.gridcells.df.1@data ,  test.field.selected.gridcells.df.2@data)
  expect_identical(test.field.selected.gridcells.dt.1@data ,  test.field.selected.gridcells.dt.2@data)
  #expect_identical(test.field.spatial.mean.1@data ,  test.field.spatial.mean.2@data)
  #expect_identical(test.field.seasonal.mean.1@data ,  test.field.seasonal.mean.2@data)
  
  # MF NOTE:  metadata by both methods not identical, work on this.
  
  
})







test_that("Plotting", {
  
  # spatial plotting
  expect_is(plotSpatial(test.field.monthly.mean.1), "ggplot")
  expect_is(plotSpatial(test.field.yearly.mean.1), "ggplot")
  
  # temporal plotting
  expect_is(plotTemporal(test.field.spatial.mean.1), "ggplot")
  
  # seaconal cycle plotting
  # expect_is(plotSeasonal(test.field.spatial.mean.1), "ggplot")
  
 
})