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
