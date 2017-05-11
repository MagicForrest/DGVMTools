# start the timer
t1 <- Sys.time()

# Load DGVMTools package
library(DGVMTools)


############################################################################
##### PREAMBLE: Define the run settings and an averaging period

# Define the RUNs to process
run1 <- defineModelRun(run.dir = "/home/forrest/FastData/RVCExamples/Example.aDGVM/Run1",
                    model = "aDGVM",
                    pft.set = aDGVM.PFTs,
                    id = "44_0", # this is, by former aDGVM way, <runid>_<fire>
                    name= "Example aDGVM run 1",
                    driving.data = "CRU"
)

# Define the RUNs to process
run2 <- defineModelRun(run.dir = "/home/forrest/FastData/RVCExamples/Example.aDGVM/Run2",
                     model = "aDGVM",
                     pft.set = aDGVM.PFTs,
                     id = "45_0", # this is, by former aDGVM way, <runid>_<fire>
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
                       temporal.extent  = period,
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

print(plotSpatial2(lai.1))
print(plotSpatial2(lai.2))


### Fractions of each PFT

# calculate the PFT fractions
lai.1 <- divideLayers(lai.1, layers = "pfts")

# Plot the tree and grass fraction
print(plotSpatial2(lai.1,
                   layers = c("TrFraction", "C4GFraction"),
                   override.cols = colorRampPalette(c("grey85", "black"))(20)))



# calculate and plot dominant PFT
lai.1 <- newLayer(lai.1, layers = c("PFT"), method = "max")
print(plotSpatial2(data = lai.1, 
                   layer = c("MaxPFT")))




############################################################################
##### EXAMPLE 2: AGB with classification scheme 2

# Define the VARIABLE to look at
variable <- "agb"

# Open the lai.out file, and average over the reference period
lai.reference.period <- getVegSpatial(run = run, period = period, var = variable, adgvm.scheme = 2)


### Simple summary plots

# Plot all PFTs on one figure and each one individually
plotVegMaps(lai.reference.period, 
            doSummary = TRUE, 
            doIndividual = TRUE)


### TOTALS

# Calculate the lifeform totals, the temperate total and the evergeen total
lai.reference.period <- aggregateLayers(lai.reference.period, layers = c("Lifeforms"))

plotVegMaps(lai.reference.period, 
            layers = c("Tree", "Grass", "Total"),
            tag = "Lifeforms")



### FRACTIONS

# calculate the tree and grass fractions (if no denominators specified, "Total" is assumed)
lai.reference.period <- divideLayers(lai.reference.period, layers = "lifeforms")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            layers = c("lifeforms"),
            tag = "Lifeform",
            special = "fraction")




### DOMINANT PFT


# calculate  and plot dominant PFT
lai.reference.period <- addDominantPFT(lai.reference.period)
plotDominantPFTMap(lai.reference.period)


# print the time
t2 <- Sys.time()
print(t2-t1)










