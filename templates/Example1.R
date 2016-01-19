# start the timer
t1 <- Sys.time()

# Load RVCTools v2.9
source("~/Tools/RVCTools/v2.9/rvc-tools.R")

##### STEP ONE: Define the settings, open the run and average over the period we want

# Define a RUN to process
run <- new("VegRun",
           run.dir = "/data/forrest/GuessRuns/N-version/Standard",
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
lai.reference.period.dt <- getTADT(run, period, variable, forceReAveraging = FALSE)



##### STEP TWO: Simple summary plots

# Plot all PFTs on one figure and each one individually
plotLPJMaps(lai.reference.period.dt, 
            run = run,
            quant = variable, 
            period = period, 
            doSummary = TRUE, 
            doIndividual = TRUE)



##### STEP THREE: More advanced analysis and plots

# Define the PFT set that was used with this run
PFT.set <- global.PFTs


### TOTALS

# Calculate the lifeform totals, the temperate total and the evergeen total
addVegTotals(lai.reference.period.dt, PFT.set, target = c("Lifeforms", "Temperate", "Evergreen"))

# Plot the Evergreen and Temperate totals indivdually
plotLPJMaps(lai.reference.period.dt, 
            which = c("Evergreen", "Temperate"),
            run = run,
            quant = variable, 
            period = period, 
            doSummary = FALSE, 
            doIndividual = TRUE)


plotLPJMaps(lai.reference.period.dt, 
            which = c("Tree", "Grass", "Total"),
            run = run,
            quant = variable, 
            period = period, 
            special.string = "Lifeforms")



### FRACTIONS

# calculate the tree and grass fractions
addVegFractions(lai.reference.period.dt, PFT.set, targets = "lifeforms")

# Plot the tree and grass fraction
plotLPJMaps(lai.reference.period.dt, 
            which = c("TreeFraction", "GrassFraction"),
            run = run,
            quant = variable, 
            period = period, 
            special.string = "Lifeform",
            special = "fraction")


# calculate the TeBS fraction of trees
addVegFractions(lai.reference.period.dt, PFT.set, targets = c("TeBS"), of.total = FALSE, of.tree = TRUE)

# Plot the tree and grass fraction
plotLPJMaps(lai.reference.period.dt, 
            which = c("TeBSFractionofTree"),
            run = run,
            quant = variable, 
            period = period, 
            special = "fraction")




### DOMINANT PFT

# add shade intolerant PFTs to shade tolerant equivalents when considerign dominance
combineShadeTolerance(lai.reference.period.dt, PFT.set)

# calculate dominant PFT
addDominantPFT(lai.reference.period.dt, PFT.set)

# plot dominant PFT
plotDominantPFTMap(lai.reference.period.dt,
                   which.dominant = "Dominant",
                   run = run, 
                   period = period)




### BIOME CLASSIFICATION

# specificy the biome scheme to use
biome.scheme <- Smith2014.scheme

#  calculate biomes from model output
addBiomes(lai.reference.period.dt, PFT.set, biome.scheme)

# read expert-derived PNV biomes
PNV.biomes <- readPNVBiomes(resolution = "HD", classification = biome.scheme@id)

plotBiomeMap(lai.reference.period.dt, 
             which.layers = biome.scheme@id,
             biome.strings = biome.scheme@strings, 
             biome.cols= biome.scheme@cols, 
             run = run, 
             period = period, 
             addData = PNV.biomes, 
             run.title = run@id, 
             maxpixels = 100000,
             Cairo.type = c("png","ps"), 
)


# print the time
t2 <- Sys.time()
print(t2-t1)










