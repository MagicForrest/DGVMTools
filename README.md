## **DGVMTools**

R tools for processing, analysing and plotting output from DGVMs (Dynamic Global Vegetation Models)


### Features

DGVMTools is a high-level framework for analysing DGVM data output.  The framework enables a complete DGVM analysis workflow, taking raw model output through comprehensive analysis and evaluation to publication-quality figures.  It also easily interfaces with both the raster package and base R functionality. Functionality includes:

* Read raw output from supported DGVMs, currently LPJ-GUESS, aDGVM(1) and aDGVM2.
* Read a wide diversity of spatial netCDF files spatial netCDF files via a general netCDF reader. In general, files which closely follow the standard structure and the CF convention for metadata should read nicely.  In particular, output from model intercomparison projects, such as ISIMIP biome sector and TRENDY, are well supported.
* Crop and aggregate the data space and time (and sub-annual dimensions).
* Convenient and flexible potting of data in time and space (also seasonal cycles).  Plots further customisable with ggplot2.
* Easy aggregation across layers PFTs, to calculate for example, total tree biomass, grass productivity or evergreen tree cover.
* Compare models and data and calculate benchmarking metrics.
* Perform biomisations.
* Export data as R rasters or data.frames, also save data to disk in portable format with convenient netCDF writing functionality.
* Thorough tracking of metadata.

---

### Installation

Since the core features of the package are now very stable recommend that you use the master branch.  We will pull small bug fixes and small non-breaking feature improvements frequently into the master for small incremental releases, so hopefully that master is never to far behind. First install **[devtools](https://cran.r-project.org/package=devtools)**. Inconveniently, the devtools package is currently undergoing reorganisation which means the depending on the version that is installed, you now have one of two possibilities:


If you have devtools >= 2.0.0 then run:

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "master", dependencies = TRUE, build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE, force=T)
```

If you have devtools version 1.x.y (ie < 2.0.0) then run:

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "master", dependencies = TRUE, build_vignettes = TRUE)
```

(thanks to Peter Anthoni for reporting)

Eventually release on CRAN is anticipated.


#### Installation troubleshooting

* If your installation fails when building the vignettes, try updating your version of devtools.

--- 

### News and Releases

Current release is v0.10.0.  See [NEWS.md](NEWS.md).

---

### Notes and Issues

* Currently plotSpatial gives unnecessary warnings about raster pixels being placed at uneven horizontal and being shifted.  This is nothing to worry about and can be ignored.  Unfortunately these warnings are difficult to suppress because they occur when the plot is being rendered (i.e. with a print command) and not when they are made (with plotSpatial).  To suppress them one can either wrap the the print command in "suppressWarnings" or call plotSpatial with "tile = TRUE" (with the downside that the plot will take a little bit longer to render).  Neither of these options are particularly satisfactory, but currently there is no other fix (apart form simply ignoring the warnings).  

---

### Contact

Please file bug reports and feature requests at https://github.com/MagicForrest/DGVMTools/issues.

