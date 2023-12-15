#!/usr/bin/Rscript

#' Compare layers to each other
#' 

#' Compare two layers (each from a \code{\linkS4class{Field}}) to calculated various statistic metric and also the error (at every spatial/temporal locality) 
#' which is returned as a \code{\linkS4class{Comparison}} object. Usually this only acts on a single layer from each \code{\linkS4class{Field}}, 
#' but see the special case for "relative abundance" in details  below. 
#'
#' 
#' @param field1 A \code{\linkS4class{Field}} from which to get the first \code{Layer} for comparison. For the normalised metrics, this is the *modelled* values.
#' @param field2 A \code{\linkS4class{Field}} from which to get the second \code{Layer} for comparison. For the normalised metrics, this is the *observed* values.
#' @param layers1 The name of the \code{Layer} to be compared from field1 (character string).  In general this should only be a *single* \code{Layer}, 
#' only in the special case of a "relative abundance" style comparison should this be multiple layers.
#' @param layers2 The name of the \code{Layer} to be compared from field2 (character string).  If not defined taken to the be the same as layers1. In general this should 
#' only be a *single* \code{Layer}, only in the special case of a "relative abundance" style comparison should this be multiple layers.
#' @param do.seasonality Logical, if TRUE use monthly values to calculate the seasonal concentration and phase, and then return NME/NSME of the concentration and MPD 
#' (mean phase difference) on the phase.
#' @param keepall1 Logical, if TRUE keep all data points in layers1 even if there is not corresponding data in layers2 
#' @param keepall2 Logical, if TRUE keep all data points in layers2 even if there is not corresponding data in layers2 
#' @param override.quantity Logical, if TRUE ignore situation where field1 and field2 have non-identical Quantities and use the Quantity from field1 for the returned Comparison
#' @param match.NAs Logical, if TRUE copy NAs from one layer to the other.  This is mostly to make sure that when you later plot the data in the final Comparison side-by-side,
#' that the both have 'no data' (typically grey areas) plotted on both maps.
#' @param tolerance Numeric, passed to copyLayers. Defines how close the longitudes and latitudes of the gridcells in \code{field1} and \code{field2}
#' need to be to the coordinates in order to get a match.  Can be a single numeric (for the same tolerance) or a vector of two numerics (for lon and lat separately).
#' Default is no rounding (value is NULL) and so is fine for most regular spaced grids. However, setting this can be useful to force matching of 
#' coordinates with many decimal places which may have lost a small amount of precision and so don't match exactly.
#' @param show.stats Logical, if TRUE print the summary statistics
#' @param area Logical, if TRUE (default) weight the comparison metrics by gridcell area (not yet implemented for seasonal, proportions or categorical comparisons)
#' @param custom.metrics A named list of functions (defined by the user) to calculate additional custom metrics.  The functions must take a data.table and 
#' two character vectors of layer names to be compared (in order in the case of multi-layer comparisons).  Spatial-temporal-annual column names of Lon, Lat, Year, Month and Day
#' can be assumed in the data.table.  The name of the item in the list is used as the metric name.  
#' @param verbose Logical, if TRUE print some informative output
#' 
#' @returns A \code{Comparison} object, which includes points only where *both* input \code{\linkS4class{Field}} object have data.
#' 
#' The returned \code{Comparison} object has the same dimensions as the input \code{\linkS4class{Field}} objects (if they don't have the same dimensions as each other the code will fail).  
#' Depending on these dimensions, the \code{Comparison} can be plotted using \code{\link{plotSpatialComparison}} (to give the absolute difference, original values side-by-side 
#' and percentage difference, also the NME spatially - to be implemented) or \code{\link{plotTemporalComparison}} (again, absolute difference, percentage difference or original values).
#' The stats slot (which contains list) holds information such as RSME, NME, Nash-Sutcliffe Model Efficiency, etc. between the datasets in the case of a continuous data.
#' In the case of comparing categorical data (ie \code{Layers} which hold factors) it contains Cohen's Kappa.
#' 
#' There are two further special cases.  In the case of monthly data, one can instead compare the seasonal cycles (enable with the "\code{do.seasonality} argument). In this
#' case the metrics reported are the Mean Phase Difference and the NME calculation is done on seasonal concentration (derived from the monthly values) instead of each data point.
#'   
#' The second special case allows comparison of the "relative abundance" of multiple layers via the Manhattan Metric or the Square Chord Difference. 
#' In this case you can specify multiple layers (same number from each Field) and for each \code{\linkS4class{Field}} the provided layers should sum to 1.0  (this is not checked by 
#' DGVMTools so please check this yourself).
#' 
#' For definitions, details and applicability of metrics such as Normalised Mean Error, Manhattan Metric, Mean Phase Difference, etc. please see:
#' 
#'  Kelley, D. I., Prentice, I. C., Harrison, S. P., Wang, H., Simard, M., Fisher, J. B., and Willis, K. O.: 
#'  A comprehensive benchmarking system for evaluating global vegetation models, 
#'  Biogeosciences, 10, 3313–3340, https://doi.org/10.5194/bg-10-3313-2013, 2013. 
#' 
#' @seealso \code{\link{plotSpatialComparison}}, \code{\link{plotTemporalComparison}}
#' 
#' @examples #' 
#' \donttest{
#'  
#' ##### Continuous (single-layer) comparison
#' 
#' # Load Saatchi data and LPJ-GUESS data over Africa
#' 
#' africa.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralAfrica", package = "DGVMTools")
#' africa.Source <- defineSource(name = "LPJ-GUESS", dir = africa.dir,  format = GUESS)
#' model.cmass <- getField(source = africa.Source, quant = "cmass", year.aggregate.method="mean")
#' 
#' Saatchi.dir <- system.file("extdata", "NetCDF", "Saatchi2011", "HD", package = "DGVMTools")
#' Saatchi.Source <- defineSource(name = "Saatchi Biomass", dir = Saatchi.dir,  format = NetCDF)
#' Saatchi.cmass <- getField(source = Saatchi.Source , quant = "vegC_std") #' 
#'
#' ## Calculate veg C of trees in model, compare layers, and print the statistics 
#' model.cmass <- layerOp(model.cmass, "+", ".Tree", "Tree")
#' vegC.comp <- compareLayers(field1 = model.cmass, 
#'                            field2 = Saatchi.cmass, 
#'                            layers1 = "Tree", 
#'                            layers2 = "vegC_std")
#' print(vegC.comp@stats)
#'
#' # plot maps with plotSpatialComparison
#' print(plotSpatialComparison(vegC.comp)) 
#' print(plotSpatialComparison(vegC.comp, type = "values"))
#' 
#' ##### Categorical (single-layer) comparison
#' # classification is by Smith et al 2014
#' # Load Haxeltine and Prentice PNV biomes data and calculate LPJ-GUESS biomes over Europe
#' 
#' europe.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' europe.Source <- defineSource(name = "LPJ-GUESS", dir = europe.dir,  format = GUESS)
#' model.biomes <- getScheme(source = europe.Source, 
#'                           scheme = Smith2014BiomeScheme, 
#'                           year.aggregate.method="mean")
#' 
#' PNV.dir <- system.file("extdata", "NetCDF", "HandP_PNV", "HD", package = "DGVMTools")
#' PNV.Source <- defineSource(name = "H and P PNV", dir = PNV.dir,  format = NetCDF)
#' PNV.biomes <- getField(source = PNV.Source , quant = "Smith2014")
#'
#' ## Compare biomes, print the statistics
#' biomes.comparison <- compareLayers(field1 = model.biomes, 
#'                                    field2 = PNV.biomes, 
#'                                    layers1 = "Smith2014")
#' print(biomes.comparison@stats)
#' 
#' # plotSpatialComparisons
#' print(plotSpatialComparison(biomes.comparison))
#' print(plotSpatialComparison(biomes.comparison, type = "values"))
#' 
#' 
#' ##### Seasonal comparison
#' 
#' # Load monthly LPJ-GUESS LAI data over Europe for two periods
#' 
#' mlai.2000_2005 <-  getField(source = europe.Source, 
#'                             quant = "mlai", 
#'                             year.aggregate.method = "mean", 
#'                             first.year = 2000, last.year = 2005)
#'                             
#' mlai.2006_2010 <-  getField(source = europe.Source, 
#'                             quant = "mlai", 
#'                             year.aggregate.method = "mean", 
#'                             first.year = 2006, last.year = 2010)
#' 
#' # make comparison and show stats
#' seasonal.comparison <- compareLayers(field1 = mlai.2000_2005, 
#'                                      field2 = mlai.2006_2010, 
#'                                      layers1 = "mlai", 
#'                                      do.seasonality = TRUE, 
#'                                      verbose = TRUE, 
#'                                      show.stats = TRUE)
#' print(seasonal.comparison@stats)
#' 
#' 
#' # plotSpatialComparisons
#' print(plotSpatialComparison(seasonal.comparison))
#' print(plotSpatialComparison(seasonal.comparison, type = "values")) 
#' 
#' 
#' 
#' }
#' 
#' @return A Comparison object
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
compareLayers <- function(field1, 
                          field2=field1, 
                          layers1, 
                          layers2=layers1,
                          do.seasonality = FALSE,
                          keepall1 = FALSE, 
                          keepall2 = FALSE, 
                          override.quantity = FALSE, 
                          verbose = FALSE, 
                          match.NAs = FALSE,
                          show.stats = TRUE,
                          area = TRUE,
                          custom.metrics = list(),
                          tolerance = NULL){
  
  ### Check that the object have the same dimensions and that we have the same number of layers, if not fail immediately
  if(!identical(getDimInfo(field1), getDimInfo(field2))) stop("Trying to compare layers with different dimenisons.  Definitely can't do this.  Check your dimension and/or averaging")
  if(length(layers1) != length(layers2)) stop("Trying to compare a different number of layers between two Fields.  Definitely can't do this.  Check you layers1 and layers2 argument.")  
  
  ### Check that the Layers to be compared are actually present in the Fields
  for(this_layer in layers1){
    if(!this_layer %in% layers(field1)) stop(paste0("Comparing layer ", this_layer, " from field1 was requested but it wasn't there.  Available layers are: ", paste0(layers(field1),  collapse = ", ")))
  }
  for(this_layer in layers2){
    if(!this_layer %in% layers(field2)) stop(paste0("Comparing layer ", this_layer, " from field2 was requested but it wasn't there.  Available layers are: ", paste0(layers(field2), collapse = ", ")))
  }
  
  # If both field2 and layers2 arguments are missing then it is a trivial comparison
  if(missing(field2) & missing(layers2)) warning("You are comparing the same Layer(s) from the same Field.  These are tautologically identical, are you sure you wanted to do this?")
  
  ### Find what nature of comparison we are doing
  type <- NULL
  # comparing one layer from each
  if(length(layers1) == 1){
    type1 <- class(field1@data[[layers1]])
    type2 <- class(field2@data[[layers2]])
    if(type1 == type2){
      if(type1 == "numeric") {
        if(do.seasonality) {
          if("Month" %in% getDimInfo(field1)) {
            type <- "seasonal"
            if(verbose) message("Comparing seasonailty of two single numeric layers with monthly data.")
          }
          else {
            stop("Argument 'do.seasonality' = TRUE but no monthly dimension found in input datasets.")
          }
        }
        else {
          type <- "continuous"
          if(verbose) message("Comparing two single numeric layers.")
        }
      }
      if(type1 == "factor") {
        type <- "categorical"
        if(verbose) message("Comparing two single categorical layers.")
      }
    }
    else {
      stop("Layer type don't match, check your layers1 and layers2 arguments and your input Fields")
    }
  }
  else {
    type <- "relative.abundance"
    if(verbose) warning("Doing relative abundance comparison, no checks have been done on layer types...")
  }
  
  
  ###  First get the layers - maybe do a nicer error check here   
  layer.field1 <- selectLayers(field1, layers1)
  layer.field2 <- selectLayers(field2, layers2)
  
  ###  Pull out the source info objects
  source1 <- field1@source
  source2 <- field2@source
  
  
  ###  Set the names by appending the id so that they are not identical when we combine them, also save the ids so that we can use them later
  new.ids.1 <- c()
  for(layer in layers1) { 
    temp.id <- paste(layer, field1@id, sep = ".")
    new.ids.1 <- append(new.ids.1, temp.id)
    setnames(layer.field1@data, layer, temp.id)  
  }
  
  new.ids.2 <- c()
  for(layer in layers2) { 
    temp.id <- paste(layer, field2@id, sep = ".")
    new.ids.2 <- append(new.ids.2, temp.id)
    setnames(layer.field2@data, layer, temp.id)  
  }
  
  if(verbose){
    message("First dataset:")
    print(layer.field1@data)
    message("Second dataset:")
    print(layer.field2@data)
  }
  
  ### Check the case that the longitudes, latitudes and years are identical
  
  ### Easy-life case, both objects are on exactly the same domain
  if(identical(getDimInfo(field1, "full"), getDimInfo(field2, "full"))) {
    if(verbose) message("Easy life! Both fields have the same dimensions, can do a data.table join operation.")
    new.data <- layer.field1@data[layer.field2@data] 
  }
  
  ### Else, not-so-easy-life is having to check the domains and keeping points or not
  else {
    if(verbose) message("Not so easy life! Fields don't have the same dimensions, doing a copyLayers() operation.")
    
    new.data <- copyLayers(from = layer.field2, 
                           to = layer.field1, 
                           layer.names = new.ids.2, 
                           new.layer.names = NULL, 
                           keep.all.to = keepall1, 
                           keep.all.from = keepall2, 
                           tolerance = tolerance)@data
    
  }
  

  # match NAs if necessary
  if(match.NAs) {
    are.NAs <- which(is.na(new.data[[new.ids.1]] * new.data[[new.ids.2]]))
    new.data[are.NAs, (c(new.ids.1, new.ids.2)) := .SD[NA], .SDcols =c(new.ids.1, new.ids.2)]
  }
  
  ### Check we made have valid overlap and print if verbose
  if(nrow(new.data) == 0) stop("The fields you selected to compare have no common points, so layer comparison can't be done! Check your input data and also note that the 'decimal.places' argument to this function may be useful for truncating coordinates to a common precision.") 
  
  if(verbose){
    message("Merged dataset")
  }
  
  # make new id for the Comparison
  id <- paste0(new.ids.1, "-", new.ids.2)
  
  # warn/stop if quantities are different
  if(!identical(field1@quant, field2@quant)) {
    if(!override.quantity) warning(paste0("Quantity objects from compared objects do not match (", field1@quant@id, " and ", field2@quant@id, "), proceeding using quantity ", field1@quant@id))
  }
  
  ### Calculate the approriate statistical comparisons
  
  if(type == "continuous") {

    new.name <- paste(source1@name, "-",  source2@name)
    stats <- continuousComparison(x = new.data, 
                                  layers1 = new.ids.1, 
                                  layers2 = new.ids.2,
                                  additional = custom.metrics, 
                                  verbose = show.stats, 
                                  area = area, 
                                  tolerance = tolerance)
    
  }
  else if(type == "seasonal") {
    
    new.name <- paste("Seasonal comparison", source1@name, "vs.",  source2@name)
    
    # special case to return a list with both the stats and the data/table with the seasonal concentration and phase
    temp.list <- seasonalComparison(x = new.data, layers1 = new.ids.1, layers2 = new.ids.2, additional = custom.metrics, verbose = show.stats, area = TRUE)
    new.data <- temp.list[["dt"]]
    stats<- temp.list[["stats"]]
    
    setnames(new.data, 
             c("C_1", "C_2", "P_1", "P_2"), 
             c(paste("Seasonal Concentration", field1@id, sep = "."), 
               paste("Seasonal Concentration", field2@id, sep = "."),
               paste("Seasonal Phase", field1@id, sep = "."), 
               paste("Seasonal Phase", field2@id, sep = "."))
    )
    
  }
  
  else if(type == "categorical") {
    
    new.name <- paste(source1@name, "vs.",  source2@name)
    stats <- categoricalComparison(x = new.data, layers1 = new.ids.1, layers2 = new.ids.2, additional = custom.metrics, verbose = show.stats, area = TRUE)
    # new.data[, "Difference" := as.character(get(new.ids.1)) == as.character(get(new.ids.2))]
    
  }
  else if(type == "relative.abundance") {
    
    
    new.name <- paste("Relative abundance", source1@name, "vs.",  source2@name)
    
    # Calculate Manhattan Metric and Squared Chord Distance
    stats <- proportionsComparison(x = new.data, layers1 = new.ids.1, layers2 = new.ids.2, additional = custom.metrics, verbose = show.stats)
    
  }
  
  # Build a common sta.info -- not done, should do?
  
  
  
  ### Finally build the layer and return
  comparison.layer <- new("Comparison",
                          id = id,
                          name = new.name,
                          type = type,
                          data = new.data,
                          quant1 = field1@quant,
                          quant2 = field2@quant,
                          source1 = source1,
                          source2 = source2,
                          layers1 = layers1,
                          layers2 = layers2,
                          sta.info1 = as(field1, "STAInfo"),
                          sta.info2 = as(field2, "STAInfo"),
                          stats = stats
  )
  
  return(comparison.layer)
  
}