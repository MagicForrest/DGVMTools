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

---

### Installation

First release for CRAN is in preparation.

We now recommend that you use the current master branch, small bug fixes and small non-breaking feature improvements will be pulled directly into master. First install **[devtools](https://cran.r-project.org/package=devtools)**. Inconveniently, the devtools package is currently undergoing reorganisation which means the depending on the version that is installed, you now have one of two possibilities:

If you have devtools version 1.x.y (ie < 2.0.0) then run:

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "master", dependencies = TRUE, build_vignettes = TRUE)
```

If you have devtools >= 2.0.0 then run:

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "master", dependencies = TRUE, build_opts = c("--no-resave-data", "--no-manual"), force=T)
```

(thanks to Peter Anthoni for reporting)

--- 

### News and Releases

**2019-02-27 Patch Release v0.7.1 **

Bugfix due to a small regression in listPFTs().  Also took the opportunity to include examples for listPFTs, unit tests for listPFTs, and changed the 'print' functions to 'show' functions as is appropriate for S4 methods (but this does not effect users).

**2019-02-27 Release v0.7.0 **

Changes are mostly bug fixes and behind the scenes improvements. However there have been a couple of user-facing changes which could break stuff (hence the minor version number increment from 0.6 to 0.7):

Potential breaks:

* Statistics class has been removed in favour of a simpler and more flexible list
* determineQuantities() and determinePFTs() are not availableQuantities() and availablePFTs()
* The 'sources' argument in plotSpatial() is not more accurately called 'fields'

Feature improvements:

* Shapefiles can be now used to select gridcells by using them as an an argument to selectGridcells(), this can also be done in getField() which allows lower memory footprint when selecting regions from large files.    
* aDGVM support improved (thanks Simon!)
* New benchmarking metrics included, also enhanced flexibility allowing custem metrics
* plotSpatial() can call geom_tile() instead of geom_raster() which allows reprojections (ie polar views)

Bug fixes:

* Many! (see commits, but none of them resulted in wrong results, but rather failing code)
    
Behind the scenes:     

* Unit tests!
* Plotting code refactorised for constistency and efficient re-use within the package.
* Benchmarking code re-worked
* Day/Month/Year are now generally stored as integers (not numerics) to save a little memory
    
Also, from now on the master branch will be consider stable and usable.  Bug fixes and small feature improvements will be pulled directly into master.  For developers, there will be a dev-v0.8 branch to collect larger features and potentially breaking changes.

**2018-10-29 Minor Release v0.6.2 **

Removed dependncy on external gzip on windows machines for better portability.

**2018-10-26 Release v0.6.1 **

Featuring aggregation and time series plotting of daily data, use of gzip for compressed DGVMData (also on windows systems), extended vignette and bug fixes.

**2018-10-23 Release v0.6.0 **

For FireMIP workshop.

**2018-10-12 Release v0.5.2 **

For visiting collaborators.  Release v0.6 still planned in the next two weeks.

**2018-10-01 First Release v0.5.0 ** 

DGVMTools is now publicly available!  Whilst package is pretty much fully featured, there may be bugs, so it should be considered in the late-beta phase.  New v0.6 release to come this month.

---

### Contact

Please file bug reports and feature requests at https://github.com/MagicForrest/DGVMTools/issues.

