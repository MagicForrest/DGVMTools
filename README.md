## **DGVMTools**

R tools for processing, analysing and plotting output from DGVMs (Dynamic Global Vegetation Models)


### Features

DGVMTools is a high-level framework for analysing DGVM data output.  The framework enables a complete DGVM analysis workflow, taking raw model output through comprehensive analysis and evaluation to publication-quality figures.  It also easily interfaces with both the raster package and base R functionality. Functionality includes:

* Read raw output from supported DGVMs, currently LPJ-GUESS, aDGVM (and the FireMIP output with the companion FireMIPTools package).
* Read pre-prepared benchmarking data sets at commonly used spatial resolutions (contact matthew.forrest@senckenberg.de for access to data files). 
* Crop and aggregate the data space and time (and sub-annual dimensions).
* Convenient and flexible potting of data in time and space (also seasonal cycles).  Plots further customisable with ggplot2.
* Easy aggregation across layers PFTs, to calculate for example, total tree biomass, grass productivity or evergreen tree cover.
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

Current release is v0.8.1.  See [NEWS.md](NEWS.md).

---

### Notes and Issues

* Currently plotSpatial gives unnecessary warnings about raster pixels being placed at uneven horizontal and being shifted.  This is nothing to worry about and can be ignored.  Unfortunately these warnings are difficult to suppress because they occur when the plot is being rendered (i.e. with a print command) and not when they are made (with plotSpatial).  To suppress them one can either wrap the the print command in "suppressWarnings" or call plotSpatial with "tile = TRUE" (with the downside that the plot will take a little bit longer to render).  Neither of these options are particularly satisfactory, but currently there is no other fix (apart form simply ignoring the warnings).  

---

### Contact

Please file bug reports and feature requests at https://github.com/MagicForrest/DGVMTools/issues.

