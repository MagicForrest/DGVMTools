
# DGVMTools v1.1.0 release (2023-12-15)

The primary purpose of this release is to ditch the dependencies on removed or deprecated functionality from other packages.  In particular dropping `maps`, `maptools` and `mapdata` packages and replacing them with `rnaturalearth` and `rnaturalearthdata`.  There are other bugfixes and changes, see below.

## Modernization
* The `maps`, `maptools` and `mapdata` packages have been replaced with `rnaturalearth` and `rnaturalearthdata` (also removing the dependency on `rgeos`) for building country and continental outlines.   The options available for the `map.overlay` argument are now  "world", "countries" or "ne_countries" for full country outlines (all equivalent) or "coastlines" or "ne_coastlines" for coastlines only (both equivalent).  Note these packages also have lots of other nice things what you can you on your plots (using `sf` and `ggplot2`)
* In a similar vein, `selectGridcells()` (and therefore `getField()`) can now read take `sf` objects (but no longer `maps`) objects. 
* `SpatRast` objects from `terra` are now handled without conversion to `raster`.
* `raster` and `sp` are still used in the package but only for exporting objects of those packages.  Overall,  the functionality of `DGVMTools` doesn't depend on `sp` or `raster` so they can be easily deprecated and removed in future releases.
* Plotting arguments are more consistent with `ggplot2`, with `linewidth` replacing `size` for `geom_line` 
* The deprecated `aes_string()` function has been removed (behind the scenes only)

## Breaking changes
* The `map.overlay` argument now takes strings based on `rnaturalearth` rather than `maps`  (see above).
* `selectGridcels()` now takes `sf` objects instead of `maps` objects.  The  `rnaturalearth`  package is a good place to get country outlines to use here, it is a great replacement for `maps`.
* The `plotSubannual()`  function now has arguments `size` and `linewidth` instead of `point.size` and `line.width` respectively.  

##  Other improvements 
*  Can now specify lons and lats to function addArea() which is useful for area calculations in sparse grids.

## Bugfixes
* Argument `text.multiplier` now handled properly in `plotSubannual()`.  Thanks Drew! 
* Bugfix when first.year/last.year is NULL
* Bugfix in `plotSubannual()` to use the specified summary function and not always the mean.





#  DGVMTools v1.0.0 release (2022-02-25)

"Version 1.0 release" - no major features, but many refinements and improvements.  Some potentially breaking changes, see below.

## Improvements and Refinements

* plotSubannual() function has been re-worked for increased flexibility and interface consistency with plotTemporal().
* Support for terra package objects (converting to and from).
* Layers may now be selected in getField() calls (potentially saving both time and memory).
* 'Quick read functionality'- getField()/getScheme() arguments 'write' and 'read.full' have been replace with 'quick.read' and 'quick.read.file' (details below)
* Spatial benchmark scores are now area-weighted where possible.
* Argument name consistency in getField() ("var" became "quant").
* Improvements to writeCFD() allowing filling in of missing longitude/latitude bands and customisation of layer axis.
* Units superscripts now rendered properly in plot labels.
* It is now possible to supply only one of first.year and last.year arguments to getField() and selectYears() (as opposed to both or neither).
* Colour palettes are now almost exclusively from viridis (the old fields::tim.colours palette has been purged)
* Automatic legend title in plotSpatial() and plotSpatialComparison().

## Breaking Changes

* Arguments to getField().  Sorry, this is annoying but had to be done.  Firstly, "var" was changed to "quant" to be consistent with everywhere else. Secondly, 'write' and 'read.full' have been replace with 'quick.read.file' and 'quick.read.autodelete' (also for getScheme(), usage details below).  
* Functions plotResidualHistos() and plotScatterComparison() were removed as they are not up to standard for a v1.0 release.  They may be reinstated in an improved form at a later date.  If you need these functions, please raise an issue on Github.
* plotSeasonal() was removed, functionality has been fully superseded by plotSubannual()

## Quick Read Functionality in getField() and getScheme()

For various reasons the old behaviour for saving and rereading processed Fields (with the 'write' and 'read.full' arguments) was becoming restrictive and not safe against unexpected behaviour.  However, it is still extremely advantageous to be able to conveniently save Fields after they have been read and processed for quicker rereading later.  To facilitate this, new functionality has been implemented and behaves as follows:

1. As before, no automatic saving and reading of processed files is done.  Unless the functionality is explicitly enabled, the raw data is reread every time.
1. To enable the functionality, the _quick.read.file_ (character string) must be specified.  The extension ".RData" will be automatically added.
1. The _quick.read.file_ specifies the name of the file to be written and re-read.  Note that since this is under the user's control they are responsible for making sure it is 
specified appropriately. 
1. If _quick.read.file_ is set to TRUE then one of two things will happen.  If a file with a matching nameexists on disk it will be read and checked, and it will be used if the layers and dimensions match.  If it doesn't exist, or the layers and dimensions don't match, the raw data will be read and the result saved as _quick.read.file_. 
1.The _quick.read.autodelete_ argument can be set to TRUE to delete any existing file with name _quick.read.file_, thus ensuring that the sata will be read (and saved) afresh.

## Behind-the-scenes

* fields package no longer required
* units package required


#  DGVMTools v0.10.0 (2021-04-09) 

Feature release, and a good one!  A new flexible netCDF file reader is now included! This completely supersedes the previous "DGVMData"" Format (which has been removed), please now use the "NetCDF"" Format instead (it should also read any old DGVMData files without problems). For further details of this and other features and bugfixes, see below.

## Main Feature

* New 'NetCDF' Format is implemented and completely replaces the old 'DGVMData' Format.  This new Format will attempt to read any netCDF file, with the following considerations:
  - Time axes (if present) can be relative (units "days/months/years/seconds since...") or absolute (units "years/months").
  - Longitude and latitude dimensions must be present in the netCDF files with a (single) corresponding variable defined i.e. no rotated grids (CDO will regrid a properly-defined rotated grid to a normal) or "land_id"-style 1D index of points.   
  - Aside from lon, lat and time, variables can have only *one* further dimension (to represent, say, PFT or carbon pool, etc).    Correspondingly, DGVMTools interprets this dimension as a Layer dimensison .
  These limitation can be addressed if there is user demand for more flexibility. So if you want something, reach out to the author by raising an Issue on Github. 
This new Format has been well-tested and works with ISIMIP biome sector output, TRENDY/FireMIP2019 output and a broad sample of netCDF files found "in the wild".  However, if you have problems with your, again please just get in touch and raise an Issue.

## Minor features/improvements

* aggregateXXXX() functions now accept "mode" and "median" to their method argument. 
* plotTrend argument set to FALSE by default in plotTemporal().
* Added "global.extent" as an argument to writeNetCDF() to automatically make a netCDF file that covers the whole globe.
* All possible factor levels are included in the return from getScheme() (even if not all levels are present in the data).
* Implemented "gap-making" in writeNetCDF() to ensure ocean-only bands are present with NAs (and not just missing).
* plotSubannual() aesthetics now consistent with plotTemporal().
* Legend order now matches input order of Fields in plotTemporal(). 

## Breaking changes

* DGVMData Format removed, NetCDF Format should be a drop in replacement.  Any problems, let me know.
* The 'file.name' argument has been moved higher in the argument list for getField().  This is to reflect its new importance when 
  using the NetCDF Format.

## Bug fixes

* Minor corrections to the logic of Smith2014 and Forrest2015 biome schemes (now strictly based on the original Lund script and logic).
* Unused 'facet.labels' argument remove from plotSpatial().  Please pass in standard ggplot arguments to control facetting.
* Bug fix to plotSubannual where function failed if Year had been averaged away.
* Corrected dimension ordering when writing "Layer" dimension in netCDF files.

## Under the hood

* When writing netCDF files, arguments to ncdf4 functions are passed through automatically with "...".
* FieldToArray reworked for efficiency, clarity and consistency.  Labelling of time axis of arrays now of form "days since". 
* Much netCDF reading code has been factored out into read-netcdf-utility-functions.R, for clarity and re-usability.

*Thank you to everyone for your bug reports and feature requests!  Keep 'em coming :-)*



#  DGVMTools v0.9.0 (2020-08-04) 

This is a feature release!  Main features are aDGVM1 support, the inclusion of plotTemporalComparison() and the change from PFT objects to the more flexible Layer objects.  However there have been many other bug-fixes and tweaks.

## Main features

* aDGVM1 now supported!  Thanks Glenn Moncrieff! The original aDGVM Format object, which referred to aDGVM2, has been renamed to ... drum roll please... aDGVM2.
* Biome schemes have been included specifically for aDGVM1.
* plotTemporalComparison() has been added.
* The PFT object has been replaced with a Layer object which is designed to be much more flexible.  Where PFT objects had fixed slots to represent their properties, Layer objects feature an R list of properties, which can be user-defined and customised for each supported model.  PFTs are now simply defined as Layer objects, but Layers can also be other things (C pools, soils depths, whatever).
* Sources now have slots "predefined.layers" to contain, yes you got it, their pre-defined Layers.
* Facetting has been reworked for plotSpatial() and is now consistent with the other plotXXX() functions.  Note that an automatic calls to facet_grid() are no longer done in plotSpatial(), but of course they can be done afterwards by the user.  Number of rows or columns of facets in a spatial plot should be specified by suppling arguments "nrow" and "ncol" to plotSpatial() (or plotSpatialComparison()).  These are actually completely equivalent to the standard arguments to facet_wrap() in ggplot2.

## Minor features/improvements

* Can now use a map object for selecting gridcells with selectGridcells().
* Functions defineLayer() and defineQuantity() added.
* Added the addTo() function to add Layer or Quantity objects to Format or Source objects.  This is simply a user-friendly convience so that users don't need to explicitly access the slots in the S4 class structure, and is designed to work in conjunction with the nee defineLayer() and defineQuantity() function above.
* Added the legend.title argument to plotSpatial().
* Coefficient of variation is now a possible method when aggregating with aggregateSpatial(), aggregateSubannual() or aggregateYears().
* Improved Field to Raster conversion.  It now supports multiple Layers, months and years.

## Breaking changes

* Using a string when specifying the format argument to defineSource() has been disabled.  Please specify the Format object, i.e. Guess instead of "GUESS" or "LPJ-GUESS".
* BiomeScheme renamed to Scheme, getBiomes() renamed to getScheme().
* Functions copyLayers() and compareLayers() now use a "tolerance" argument instead of "dec.places" which is more intuitive, efficient and reliable.
* Function determinePFTs() has been removed as it was not particularly useful and PFT objects have now been generalised to Layer objects.
* plotScatter() argument names changed to 'x' and 'y' instead of 'field1' and 'field2'
* First argument to plotScatterComparision() and plotResidualComparison() is now called 'comparisons' instead of 'input.CLayers'.

## Under the hood

* In Format-GUESS, crop and aggregating is now done earlier to save memory.
* Precision of Lon and Lat handled better in addArea().
* plotSpatial() now correctly identifies different year periods and puts them in different panels (thanks to Peter Antoni for reporting).
* Removed correct.map.offset internal function because it now seems unnecesary and just caused problems.



#  DGVMTools v0.8.2 (2019-10-04) 

This is a bug-fix and "feature tweaking" release - i.e. there is no new major functionality but enhanced useability and other tweaks.


## Feature tweaks

* Reformulated arguments for plotTemporal() giving finer control of facetting vs line colours/styles/widths.
* "..." argument can be now used to pass arguments to internal call to facet_wrap() in plotTemporal() and plotSubannual().
* Exporting Fields to Raster now handles multiple years, months and layers.
* copyLayers() can take dec.places for Lon and Lat deperately.
* Added extra attibutes in writeNetCDF().

## Potential breaks

* Aguments to plotTemporal() have been changed.

## Bug fixes

* Correctly set keys in copyLayers().
* Correctly set start date when writing Fields in writeNetCDF().

## Behind-the-scences

* More unifying of plotting code including new sanitiseComparisonsForPlotting() and mergeFieldsForPlotting() functions and completely reworked plotTemporal() to use plotting framework functions.


#  DGVMTools v0.8.1 (2019-04-30) 

A few new features in this release.  

## Feature improvements:

* When a call to getField() is made to read data, if an appropriate Quantity object is not found then it will be automatically created.  This is particularly useful for LPJ-GUESS users who want to open their own custom non-standard 'xxxxx.out' files.  It is no longer necessary to define metadata for such a file before reading it.  The only drawback is that it won't have the units defined or a nicely descriptive name. 
* Writing daily data to netCDF with function writeNetCDF now implemented (time axis is always "days since ..."). A "365_day" calendar is assumed.
* Reading daily data from netCDF using DGVMData now implemented (time axis is always "days since ...").  In principle all common calendars are supported, but aggregateSubannual() assumes a 365 day year, so only "365_day" is fully supported by the package and is therefore preferred. 
* The function getField() now takes a 'file.name' argument for files which do not have their standard name, or for Formats which don't have a guaranteed unique or easily comstructable file name.  
* Function copyLayers() will fill data across dimensions in the 'to' argument which don't exist in the 'from' argument, provide that new argument 'fill.dims' is set to TRUE.

## Potential breaks:

* The new 'file.name' argument to getField() may cause problems with the argument ordering in pre-existing scripts.
* Some DGVMData files may no longer be readable due to changes in reading the time axis.
* Arguments to writeNetCDF() changed.  Function now only takes 'time.intervals' and 'start.date', the 'monthly' and 'annual' arguments have been removed as it is now redundant since the time axis is defined purely in terms of 'time.intervals' since 'start.date'.

## Bug fixes:

* crop() methods now called setKeyDGVM().

## Behind the scenes:

* getField_DGVMData() substantially re-worked and is now much more robust.
* writeNetCDF() and FieldToArray re-worked somewhat to define time dimension labels in terms of days only.


#  DGVMTools v0.8.0 (2019-03-18) 

This release has a couple of new features, few minor bug fixes and some tidying and cleaning.  See also new repository for example scripts at https://github.com/MagicForrest/DGVMTools_Scripts

## Feature improvements:

* The getBiomes() function can now take a list of Source objects and average over them before calculating the biomes (#19)
* Seasonal comparison metrics, including Mean Phase Difference, now included.  plotSpatial also works for seasonal comparisons.
* The compareLayers() function can now calculate user-defined metrics.
* Function plotSubannual() included as a replacement for plotSeasonal(). plotSeasonal() had an inconsistent interface with the other plot functions and is now deprecated.  In contrast, plotSubannual() has consistent arguments and also re-uses more code internally, however **facetting is not yet fully implemented**.
* In selectGridcells(), the @spatial.extent now stores the gridcells or shapefile that was used.
* Comparison objects now has an @type characters slot to say if they are "continuous", "categorical", "seasonal" or "relative.abundance" (this is also a potential break).

## Potential breaks:

* Fields are now saved to disk using the .RData file extension instead of .DGVMField to be more consistent with the R universe. You can simply rename your files of necessary.  
* Ugly LAI colour palette removed.
* Function countCategoricalData() removed.
* Function byIDfromList() now internal.
* Function addArea() makes layers called "Area" instead of "area".
* The @stats slot of a Comparison object is now a simple R list instead of a class, giving much more flexibility and the possibility for user-defined metrics.

## Bug fixes:

* Fields ids now done more consistently.
* Function renameLayers() now actually returns the Field

## Behind the scenes:

* More code from plotSpatial() factored out into plotting-framework-functions.R
* Benchmarking code re-organised
* getBiomes() much slimmed down by removing code for cropping/aggregating (this was redundant since it was done in the getField calls)
* udunits2 dependency removed


#  DGVMTools v0.7.1 (2019-02-27) 

Bug fix due to a small regression in listPFTs().  Also took the opportunity to include examples for listPFTs, unit tests for listPFTs, and changed the 'print' functions to 'show' functions as is appropriate for S4 methods (but this does not effect users).


#  DGVMTools v0.7.0 (2019-02-27)

Changes are mostly bug fixes and behind the scenes improvements. However there have been a couple of user-facing changes which could break stuff (hence the minor version number increment from 0.6 to 0.7):


## Potential breaks:

* Statistics class has been removed in favour of a simpler and more flexible list
* determineQuantities() and determinePFTs() are not availableQuantities() and availablePFTs()
* The 'sources' argument in plotSpatial() is not more accurately called 'fields'

## Feature improvements:

* Shapefiles can be now used to select gridcells by using them as an an argument to selectGridcells(), this can also be done in getField() which allows lower memory footprint when selecting regions from large files.    
* aDGVM support improved (thanks Simon!)
* New benchmarking metrics included, also enhanced flexibility allowing custom metrics
* plotSpatial() can call geom_tile() instead of geom_raster() which allows reprojections (ie polar views)

## Bug fixes:

* Many! (see commits, but none of them resulted in wrong results, but rather failing code)
    
## Behind the scenes:     

* Unit tests!
* Plotting code refactorised for consistency and efficient re-use within the package.
* Benchmarking code re-worked
* Day/Month/Year are now generally stored as integers (not numerics) to save a little memory
    
Also, from now on the master branch will be consider stable and usable.  Bug fixes and small feature improvements will be pulled directly into master.  For developers, there will be a dev-v0.8 branch to collect larger features and potentially breaking changes.


# DGVMTools v0.6.2 (2018-10-29)

Removed dependency on external gzip on windows machines for better portability.


# DGVMTools v0.6.1 (2018-10-26)

Featuring aggregation and time series plotting of daily data, use of gzip for compressed DGVMData (also on windows systems), extended vignette and bug fixes.


# DGVMTools v0.6.0 (2018-10-23)

For FireMIP workshop.


# DGVMTools v0.5.2 (2018-10-12)

For visiting collaborators.  Release v0.6 still planned in the next two weeks.


# DGVMTools v0.5.0 (2018-10-01) 

DGVMTools is now publicly available!  Whilst package is pretty much fully featured, there may be bugs, so it should be considered in the late-beta phase.  New v0.6 release to come this month.
