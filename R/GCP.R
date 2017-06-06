#!/usr/bin/Rscript

getGCP <- function(location = "/home/forrest/Data/Productivity/GCP/2016/") {
  
  
  filepath <- file.path(location, "Global_Carbon_Budget_2016.txt")
  
  GCP.data <- read.table(filepath, header = TRUE)
  
  GCP.data <- data.table(GCP.data)
  
  setnames(GCP.data, c("land.sink"), c("NEE"))
  
  
  dataset <- new("DataObject",
                 id = "GCP",
                 name = "GCP Carbon Budget",
                 temporal.extent = new("TemporalExtent", id = "GCPPeriod", name = "GCP Period", start = 1959, end = 2015),
                 data = GCP.data,
                 quant = new("Quantity",
                             id = "aNEE_std",
                             name = "Annual land sink (NEE)",
                             type = "annual",
                             units = "GtC/year",
                             colours = veg.palette,
                             model = c("Standard"))
                   ,
                 spatial.extent = new("SpatialExtent", id = "Global", name = "Global", extent(-180,180,-90,90)),
                 correction.layer =  "")
  
  
  
  return(dataset)
  
}