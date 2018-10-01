## **DGVMTools**:  R tools for processing, analysing and plotting output from DGVMs (Dynamic Global Vegetation Models)


### Features

DGVMTools is a high-level framework for analysing DGVM data output.  The framework enables a complete DGVM analysis workflow, taking raw model output through comprehensive analysis and evaluation to publication-quality figures.  It also easily interfaces with both the raster pakage and base R functionality. Functionality includes:

* Read raw output from supported DGVMs (currently LPJ-GUESS, aDGVM and the FireMIP output).
* Read pre-prepared benchmarking datasets at commonly used spatial resolutions (contact matthew.forrest@senckenberg.de for access to data files). 
* Crop and aggregate the data space and time (and sub-annual dimensions).
* Convenient and flexible potting of data in time and space (also seasonal cycles).  Plots further customisable with ggplot2.
* Easy aggregation across layers PFTs, to calculate for example, total tree biomass, grass productivety or evergreen tree cover.
* Compare models and data and calculate benchmarking metrics.
* Perform biomisations.
* Export data as R rasters or data.frames, also save data to disk in portable format with convenient netCDF writing functionality.
* Thorough tracking of metadata.

==== 

### News

**2018-10-01** - DGVMTools is publicly available.  Whilst package is pretty much fully featured, there may be bugs, so it should be considered in the late-beta phase.  New v0.6 release to come this month.

====

### Installation

First release for CRAN is in preparation.

To install the latest development version, first install **[devtools](https://cran.r-project.org/package=devtools)** and subsequently run

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "dev")
```

====

### Contact

Please file bug reports and feature requests at https://github.com/MagicForrest/DGVMTools/issues.

