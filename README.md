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

Since the core features of the package are now very stable, it is recommend that you use the master branch.  Small bug fixes and non-breaking feature improvements will be pulled frequently into the master for small incremental releases.  There are three ways to install DGVMTools (it isn't on CRAN).

#### Method 1: (devtools >= 2.0.0)

This is the easiest method, but you need to you have devtools >= 2.0.0 installed.  Simply run:

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "master", dependencies = TRUE, build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE, force=T)
```

#### Method 2: (devtools < 2.0.0)

For older versions of devtools runs: 

```S
devtools::install_github("MagicForrest/DGVMTools", ref = "master", dependencies = TRUE, build_vignettes = TRUE)
```

#### Method 3: Download package file and install through R

If you can't install any version of devtools, you can download the source code and install it via the "Packages" panel in RStudio.  For Linux/Mac use the .tar.gz file available [here](https://github.com/MagicForrest/DGVMTools/archive/refs/tags/v1.0.0.tar.gz), for Windows use the .zip file from [here](https://github.com/MagicForrest/DGVMTools/archive/refs/tags/v1.0.0.zip).


#### Installation troubleshooting

* If your installation fails when building the vignettes, try updating your version of devtools.

--- 

### News and Releases

Current release is v1.1.0.  See [NEWS.md](NEWS.md).
A description paper is currently being prepared for peer-review and eventually release on CRAN is anticipated.

---

### Notes and Issues

* Currently plotSpatial gives unnecessary warnings about raster pixels being placed at uneven horizontal and being shifted.  This is nothing to worry about and can be ignored.  Unfortunately these warnings are difficult to suppress because they occur when the plot is being rendered (i.e. with a print command) and not when they are made (with plotSpatial).  To suppress them one can either wrap the the print command in "suppressWarnings" or call plotSpatial with "tile = TRUE" (with the downside that the plot will take a little bit longer to render).  Neither of these options are particularly satisfactory, but currently there is no other fix (apart form simply ignoring the warnings).  

---

### Contact

Please file bug reports and feature requests at https://github.com/MagicForrest/DGVMTools/issues.

