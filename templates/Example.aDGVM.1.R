# start the timer
t1 <- Sys.time()

# Load RVCTools package, unload the package first in case we are actively developing the package
if("package:RVCTools" %in% search()) detach(name = "package:RVCTools", unload = TRUE)
library(RVCTools)


############################################################################
##### PREAMBLE: Define the run settings and an averaging period

# Define a RUN to process
run <- new("VegRun",
           run.dir = "/data/forrest/aDGVM",
           model = "aDGVM",
           pft.set = aDGVM.PFTs,
           id = "44_0", # this is, by former aDGVM way, <runid>_<fire>
           description= "An example aDGVM run",
           driving.data = "CRU",
           map.overlay = "lowres",
           lonlat.offset = c(0.25,0.25),
           year.offset = 1401
)

# Define a TIME PERIOD over which to average - this is ignored, the last 20 years is hard-coded
period = new("TimeSpan", name = "Reference", start = 81, end = 100)




############################################################################
##### EXAMPLE 1: LAI with classification scheme 1

# Define the VARIABLE to look at
variable <- "lai"

# Open the lai.out file, and average over the reference period
lai.reference.period <- getVegSpatial(run, period, variable, adgvm.scheme = 1)


### Simple summary plots of each "PFT"

# Plot all PFTs on one figure and each one individually
# plotVegMaps(lai.reference.period, 
#             doSummary = TRUE, 
#             doIndividual = TRUE)


### Fractions of each PFT

# calculate the PFT fractions
lai.reference.period <- addVegFractions(lai.reference.period, targets = "pfts")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            which.layers = c("pfts"),
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
lai.reference.period <- getVegSpatial(run, period, variable, adgvm.scheme = 2)


### Simple summary plots

# Plot all PFTs on one figure and each one individually
plotVegMaps(lai.reference.period, 
            doSummary = TRUE, 
            doIndividual = TRUE)


### TOTALS

# Calculate the lifeform totals, the temperate total and the evergeen total
lai.reference.period <- addVegTotals(lai.reference.period, target = c("Lifeforms"))

plotVegMaps(lai.reference.period, 
            which = c("Tree", "Grass", "Total"),
            special.string = "Lifeforms")



### FRACTIONS

# calculate the tree and grass fractions
lai.reference.period <- addVegFractions(lai.reference.period, targets = "lifeforms")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            which = c("lifeforms"),
            special.string = "Lifeform",
            special = "fraction")




### DOMINANT PFT


# calculate  and plot dominant PFT
lai.reference.period <- addDominantPFT(lai.reference.period)
plotDominantPFTMap(lai.reference.period)


# print the time
t2 <- Sys.time()
print(t2-t1)










