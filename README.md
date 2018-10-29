## **DGVMTools**

R tools for processing, analysing and plotting output from DGVMs (Dynamic Global Vegetation Models)


### Features

DGVMTools is a high-level framework for analysing DGVM data output.  The framework enables a complete DGVM analysis workflow, taking raw model output through comprehensive analysis and evaluation to publication-quality figures.  It also easily interfaces with both the raster pakage and base R functionality. Functionality includes:

* Read raw output from supported DGVMs, currently LPJ-GUESS, aDGVM (and the FireMIP output with the companion FireMIPTools package).
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

**2018-10-29** - Release v0.6.2 Removed dependmcy om external gzip on windows machines for better portability.

**2018-10-26** - Release v0.6.1 featuring aggregation and time series plotting of daily data, use of gzip for compressed DGVMData (also on windows systems), extended vignette and bug fixes.

**2018-10-23** - Release v0.6.0 for FireMIP workshop.

**2018-10-12** - Release v0.5.2 for visiting collaborators.  Release v0.6 still planned in the next two weeks.

**2018-10-01** - DGVMTools is now publicly available!  Whilst package is pretty much fully featured, there may be bugs, so it should be considered in the late-beta phase.  New v0.6 release to come this month.

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

