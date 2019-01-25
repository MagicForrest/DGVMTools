library(testthat)
library(DGVMTools)
library(stats)
library(compiler)

context("DGMTools")

test_that("Sources, Fields and Aggregating",{
  
  # open a source
  test.source <- defineSource(id = "LPJ-GUESS_Example",
                                   dir = system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools"), 
                                   format = GUESS,
                                   name = "LPJ-GUESS Europe Example Run")
  
  expect_is(test.source, "Source")
  
  # read a monthly field
  test.field.full <- getField(test.source, "mlai")
  expect_is(test.field.full, "Field")
  
  # test monthly averaging
  test.field.monthly.mean.1 <- getField(test.source, "mlai", subannual.aggregate.method = "mean", subannual.resolution = "Year")
  test.field.monthly.mean.2 <- aggregateSubannual(input.obj = test.field.full, method = "mean", target = "Year")
  expect_equal(test.field.monthly.mean.1@data ,  test.field.monthly.mean.2@data)
  
  # test yearly averaging
  test.field.yearly.mean.1 <- getField(test.source, "mlai", year.aggregate.method = "mean")
  test.field.yearly.mean.2 <- aggregateYears(test.field.full, "mean")
  expect_identical(test.field.yearly.mean.1@data ,  test.field.yearly.mean.2@data)
  
  # test spatial averaging
  test.field.spatial.mean.1 <- getField(test.source, "mlai", spatial.aggregate.method = "mean")
  test.field.spatial.mean.2 <- aggregateSpatial(test.field.full, "mean")
  expect_identical(test.field.spatial.mean.1@data ,  test.field.spatial.mean.2@data)
  
})
