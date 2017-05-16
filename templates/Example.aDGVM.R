# start the timer
t1 <- Sys.time()

# Load DGVMTools package
library(DGVMTools)


############################################################################
##### PREAMBLE: Define the run settings and an averaging period

# Define the RUNs to process
run1 <- defineModelRun(run.dir = "/home/sscheiter/results",
                    model = "aDGVM",
                    pft.set = aDGVM.PFTs,
                    id = "3_1_wshrubs_wannuals", # this is, by former aDGVM way, <runid>_<fire>
                    name= "Example aDGVM run 1",
                    driving.data = "CRU"
)

# Define the RUNs to process
run2 <- defineModelRun(run.dir = "/home/sscheiter/results",
                     model = "aDGVM",
                     pft.set = aDGVM.PFTs,
                     id = "3_1_wshrubs_wannuals", # this is, by former aDGVM way, <runid>_<fire>
                     name= "Example aDGVM run 2",
                     driving.data = "CRU"
)

# Define a TIME PERIOD over which to average - this is ignored, the last 20 years is hard-coded
period = new("TemporalExtent", name = "Reference", start = 81, end = 100)



############################################################################
##### EXAMPLE 1: LAI with classification scheme 1

# Define the VARIABLE to look at
variable <- "lai"

# Read the lai from both runs
lai.1 <- getModelObject(run = run1, 
#                       temporal.extent  = period,
                       temporally.average = TRUE,
                       var = variable, 
                       adgvm.scheme = 1,
                       write = TRUE,
                       read.full = TRUE)

lai.2 <- getModelObject(run = run2, 
                       temporal.extent = period, 
                       temporally.average = TRUE,
                       var = variable, 
                       adgvm.scheme = 1,
                       write = TRUE,
                       read.full = TRUE)



### Simple summary plots of each "PFT" - one plot for each PFT and a summary plot of them all

print(plotSpatial(lai.1))
print(plotSpatial(lai.2))


### Fractions of each PFT

# calculate the PFT fractions
lai.1 <- divideLayers(lai.1, layers = "pfts")

# Print the names to see what happened to out ModelObject
print(names(lai.1))

# Plot the tree and grass fraction
print(plotSpatial(lai.1,
                   layers = c("TrFraction", "C4GFraction"),
                   override.cols = colorRampPalette(c("grey85", "black"))(20)))



# calculate and plot dominant PFT
lai.1 <- newLayer(lai.1, layers = c("PFT"), method = "max")
print(plotSpatial(data = lai.1, 
                   layer = c("MaxPFT")))




############################################################################
##### EXAMPLE 2: AGB with classification scheme 2

# Define the VARIABLE to look at
variable <- "agb"

# Open the lai.out file, and average over the reference period
agb.1 <- getModelObject(run = run1, 
                          temporal.extent  = period,
                          temporally.average = TRUE,
                          var = variable, 
                          adgvm.scheme = 2,
                          write = TRUE,
                          read.full = TRUE)

### Simple summary plots

# Plot all PFTs on one figure and each one individually
print(plotSpatial(data = agb.1))



### TOTALS

# Calculate the lifeform totals, the temperate total and the evergeen total
agb.1 <- newLayer(agb.1, layers = c("Lifeforms"))

print(plotSpatial(data = agb.1,
                   layers = c("Tree", "Grass")))




### FRACTIONS

# calculate the tree and grass fractions (if no denominators specified, "Total" is assumed)
agb.1 <- divideLayers(agb.1, layers = "lifeforms")

# Plot the tree and grass fraction
print(plotSpatial(data = agb.1,
                   layers = expandLayers(input.data = agb.1,layers = "Lifeforms")))





### DOMINANT PFT

# calculate and plot dominant PFT
agb.1 <- newLayer(agb.1, layers = c("PFT"), method = "max")
print(plotSpatial(data = agb.1, 
                   layer = c("MaxPFT")))

# print the time
t2 <- Sys.time()
print(t2-t1)










