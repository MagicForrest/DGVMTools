# start the timer
t1 <- Sys.time()

# Load RVCTools package, unload the package first in case we are actively developing the package
if("package:RVCTools" %in% search()) detach(name = "package:RVCTools", unload = TRUE)
library(RVCTools)


############################################################################
##### PREAMBLE: Define the run settings and an averaging period

# Define the RUNs to process
run1 <- defineVegRun(run.dir = "/home/forrest/FastData/RVCExamples/Example.aDGVM/Run1",
                    model = "aDGVM",
                    pft.set = aDGVM.PFTs,
                    id = "44_0", # this is, by former aDGVM way, <runid>_<fire>
                    description= "Example aDGVM run 1",
                    driving.data = "CRU",
                    map.overlay = "world"
)

# Define the RUNs to process
run2 <- defineVegRun(run.dir = "/home/forrest/FastData/RVCExamples/Example.aDGVM/Run2",
                     model = "aDGVM",
                     pft.set = aDGVM.PFTs,
                     id = "45_0", # this is, by former aDGVM way, <runid>_<fire>
                     description= "Example aDGVM run 2",
                     driving.data = "CRU",
                     map.overlay = "world"
)

# Define a TIME PERIOD over which to average - this is ignored, the last 20 years is hard-coded
period = new("TemporalExtent", name = "Reference", start = 81, end = 100)



############################################################################
##### EXAMPLE 1: LAI with classification scheme 1

# Define the VARIABLE to look at
variable <- "lai"

# Read the lai from both runs
lai.1 <- getVegSpatial(run = run1, 
                       period = period,
                       var = variable, 
                       adgvm.scheme = 1,
                       write = TRUE,
                       reread.file = FALSE)

lai.2 <- getVegSpatial(run = run2, 
                       period = period, 
                       var = variable, 
                       adgvm.scheme = 1,
                       write = TRUE,
                       reread.file = FALSE)

stop()

### Simple summary plots of each "PFT" - one plot for each PFT and a summary plot of them all
plotVegMaps(lai.1, 
            doSummary = TRUE, 
            doIndividual = TRUE)

plotVegMaps(lai.2, 
            doSummary = TRUE, 
            doIndividual = TRUE)

### Fractions of each PFT

# calculate the PFT fractions
lai.reference.period <- divideLayers(lai.reference.period, targets = "pfts")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            targets = c("pfts"),
            special.string = "PFT.Fractions",
            special = "fraction")




# calculate and plot dominant PFT
lai.reference.period <- addDominantPFT(lai.reference.period)
plotDominantPFTMap(lai.reference.period)




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
lai.reference.period <- aggregateLayers(lai.reference.period, targets = c("Lifeforms"))

plotVegMaps(lai.reference.period, 
            targets = c("Tree", "Grass", "Total"),
            special.string = "Lifeforms")



### FRACTIONS

# calculate the tree and grass fractions (if no denominators specified, "Total" is assumed)
lai.reference.period <- divideLayers(lai.reference.period, targets = "lifeforms")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            targets = c("lifeforms"),
            special.string = "Lifeform",
            special = "fraction")




### DOMINANT PFT


# calculate  and plot dominant PFT
lai.reference.period <- addDominantPFT(lai.reference.period)
plotDominantPFTMap(lai.reference.period)


# print the time
t2 <- Sys.time()
print(t2-t1)










