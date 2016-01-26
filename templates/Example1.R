# start the timer
t1 <- Sys.time()

# Load RVCTools package, unload the package first in case we are actively developing the package
if("package:RVCTools" %in% search()) detach(name = "package:RVCTools", unload = TRUE)
library(RVCTools)

##### STEP ONE: Define the settings, open the run and average over the period we want

# Define a RUN to process
run <- new("VegRun",
           run.dir = "/data/forrest/GuessRuns/N-version/Standard",
           pft.set = global.PFTs,
           id = "ExampleRun1",
           description= "An example LPJ-GUESS run",
           driving.data = "CRU",
           map.overlay = "lowres",
           lonlat.offset = c(0.25,0.25),
           year.offset = 1401
)

# Define the VARIABLE to look at
variable <- "lai"

# Define a TIME PERIOD over which to average
period = new("TimeSpan", name = "Reference", start = 1961, end = 1990)

# Open the lai.out file, and average over the reference period
lai.reference.period <- getVegObj(run, period, variable, forceReAveraging = FALSE)


##### STEP TWO: Simple summary plots

# Plot all PFTs on one figure and each one individually
plotVegMaps(lai.reference.period, 
            doSummary = TRUE, 
            doIndividual = TRUE)



##### STEP THREE: More advanced analysis and plots

# Define the PFT set that was used with this run
PFT.set <- global.PFTs


### TOTALS

# Calculate the lifeform totals, the temperate total and the evergeen total
lai.reference.period <- addVegTotals(lai.reference.period, target = c("Lifeforms", "Temperate", "Evergreen"))

# Plot the Evergreen and Temperate totals indivdually
plotVegMaps(lai.reference.period, 
            which = c("Evergreen", "Temperate"),
            doSummary = FALSE, 
            doIndividual = TRUE)


plotVegMaps(lai.reference.period, 
            which = c("Tree", "Grass", "Total"),
            special.string = "Lifeforms")



### FRACTIONS

# calculate the tree and grass fractions
lai.reference.period <- addVegFractions(lai.reference.period, targets = "lifeforms")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            which = c("TreeFraction", "GrassFraction"),
            special.string = "Lifeform",
            special = "fraction")


# calculate the TeBS fraction of trees
lai.reference.period <- addVegFractions(lai.reference.period, targets = c("TeBS"), of.total = FALSE, of.tree = TRUE)

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            which = c("TeBSFractionofTree"),
            special = "fraction")



### DOMINANT PFT

# add shade intolerant PFTs to shade tolerant equivalents when considering dominance
lai.reference.period <- combineShadeTolerance(lai.reference.period)

# calculate dominant PFT
lai.reference.period <- addDominantPFT(lai.reference.period)

# plot dominant PFT
plotDominantPFTMap(lai.reference.period)



### BIOME CLASSIFICATION

# specificy the biome scheme to use
biome.scheme <- Smith2014.scheme

#  calculate biomes from model output
lai.reference.period <- addBiomes(lai.reference.period, biome.scheme)

# read expert-derived PNV biomes
#PNV.biomes <- readPNVBiomes(resolution = "HD", classification = biome.scheme@id)

# plot biomes
plotBiomeMap(lai.reference.period, 
             scheme = biome.scheme,
             addData = FALSE,#PNV.biomes, 
             Cairo.type = c("png","ps"), 
)

# print the time
t2 <- Sys.time()
print(t2-t1)










