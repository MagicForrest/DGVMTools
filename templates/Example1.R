##### PREAMBLE: Load the packages and start the timer

# Load RVCTools package, unload the package first in case we are actively developing the package
library(RVCTools)

# start the timer
t1 <- Sys.time()

#### STEP ONE: Define the settings, open the run and average over the period we want

# Define a RUN to process
run <- defineVegRun(run.dir = "/home/forrest/FastData/Example1",
                    model = "LPJ-GUESS",
                    pft.set = global.PFTs,
                    id = "ExampleRun1",
                    description= "An example LPJ-GUESS run",
                    driving.data = "CRU",
                    map.overlay = "world", # adding even low resolution country outlines more than doubles run time and plot files sizes :-/
                    lonlat.offset = c(0.0,0.0),
                    year.offset = 0
)

# Define the VARIABLE to look at
variable <- "lai"

# Define a time period or TemporalExtent over which to average
period = new("TemporalExtent", id = "Reference", name = "Reference", start = 1961, end = 1990)

# Open the lai.out file, and average over the reference period
lai.reference.period <- getVegObject(run, 
                                     variable, 
                                     temporal.extent = period, 
                                     temporally.average = TRUE, 
                                     write = TRUE,
                                     reread.file = FALSE,
                                     store.internally = FALSE)

##### STEP TWO: Simple summary plots

# Plot all PFTs on one figure and plot it to the screen
plotVegMaps(lai.reference.period, 
            doSummary = TRUE, 
            doIndividual = FALSE,
            Cairo.type = "png")



# Plot each PFT individually in a file for study later
plotVegMaps(lai.reference.period, 
            doSummary = FALSE, 
            doIndividual = TRUE,
            Cairo.type = "png")


##### STEP THREE: More advanced analysis and plots

### TOTALS

# Calculate the lifeform totals, the temperate total and the evergeen total
lai.reference.period <- aggregateLayers(lai.reference.period, layer = c("Lifeforms", "Temperate", "Evergreen"))

# Plot the Evergreen and Temperate totals indivdually (to the screen)
plotVegMaps(lai.reference.period, 
            layers = c("Evergreen", "Temperate"),
            doSummary = FALSE, 
            doIndividual = TRUE,
            Cairo.type = "png")


# Plot the Tree, Grass and Total LAIs all on one plot in a file 
plotVegMaps(lai.reference.period, 
            layers = c("Tree", "Grass", "Total"),
            tag = "Lifeforms")



### FRACTIONS

# calculate the tree and grass fractions (if no denominator specified, Total is assumed)
lai.reference.period <- divideLayers(lai.reference.period, layers = "lifeforms")

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            layers = c("lifeforms"),
            tag = "Lifeform",
            special = "fraction")


# calculate the TeBS fraction of trees
lai.reference.period <- divideLayers(lai.reference.period, layers = c("TeBS"), denominators = list("Tree"))

# Plot the tree and grass fraction
plotVegMaps(lai.reference.period, 
            layers = c("TeBSFractionOfTree"),
            expand.layers = FALSE,
            special = "fraction")



### DOMINANT PFT

# add shade intolerant PFTs to shade tolerant equivalents when considering dominance
lai.reference.period <- combineShadeTolerance(lai.reference.period)

# calculate dominant PFT
lai.reference.period <- addDominantPFT(lai.reference.period)

# plot dominant PFT
plotVegMaps(lai.reference.period, special = "Dominant")


### BIOME CLASSIFICATION

# specificy the biome scheme to use
biome.scheme <- Forrest2015.scheme

#  calculate biomes from model output
lai.reference.period <- addBiomes(lai.reference.period, biome.scheme)

# read expert-derived PNV biomes
PNV.biomes <-  readHandPBiomes(classification = biome.scheme@id)

# plot biomes (first to the screen, then as both as a .png and .ps file on disk)
plotVegMaps(lai.reference.period, 
            biome.scheme = biome.scheme, 
            special = "biomes", 
            biome.data = PNV.biomes, 
            Cairo.type = c("x11", "png","ps")
)


# print the time
t2 <- Sys.time()
print(t2-t1)



