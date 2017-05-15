#!/usr/bin/Rscript


#' Print a DGVMTools object in a reasonably nice and space efficient way.   
#'
#' Returns TRUE if an object of the particular object type and FALSE otherwise 
#' 
#' @param x Any DGVMTools object to be printed
#' @param ... other arguments to be passed on to other print function, not currently used
#' @name print.object-methods
#' @rdname print.object-methods
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
NULL

#' @rdname print.object-methods
#' @export
print.PFT <- function(x, ...) {
  
  cat(paste0("PFT: ", x@id," (", x@name, "): ", "Lifeform=",  x@lifeform, ", Leafform=", x@leafform, ", Phenology=", x@phenology, ", Climate zone=", x@zone, ", Preferred colour=", x@colour, ", Combine with=", x@combine, "\n"))

}

#' @rdname print.object-methods
#' @export
print.Quantity <- function(x, ...) {
  
  cat(paste0("Quantity: ", x@id," (", x@name, "): ", "Type=",  x@type, ", Units=", x@units, ", Aggregate Method=", x@aggregate.method, ", Defined for models: ", paste0(unlist(x@model), collapse = ', '), "\n"))

}

#' @rdname print.object-methods
#' @export
print.TemporalExtent <- function(x, ...) {
  
  cat(paste0("Temporal Extent:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  cat(paste0("start = ", x@start, "\n"))
  cat(paste0("end = ", x@end, "\n"))
  
}

#' @rdname print.object-methods
#' @export
print.SpatialExtent <- function(x, ...) {
  
  cat(paste0("Spatial Extent:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  cat(paste0("extent: xmin = ", x@extent@xmin, "\n"))
  cat(paste0("        xmax = ", x@extent@xmax, "\n"))
  cat(paste0("        ymin = ", x@extent@ymin, "\n"))
  cat(paste0("        ymax = ", x@extent@ymax, "\n"))
  
}

#' @rdname print.object-methods
#' @export
print.Period <- function(x, ...) {
  
  cat(paste0("Sub-annual time period:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  cat(paste0("abbreviation = ", "\"", x@abbreviation, "\"", "\n"))
  cat(paste0("index = ", x@index, "\n"))
  cat(paste0("padded.index = ", "\"", x@padded.index, "\"", "\n"))
  cat(paste0("contains = ", x@contains, "\n"))
  cat(paste0("days = ", x@days, "\n"))
  cat(paste0("days.leap = ", x@days.leap, "\n"))
  
}



#' @rdname print.object-methods
#' @export
print.ModelRunInfo <- function(x, ...) {
  
  cat(paste0("Model run:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  cat(paste0("run directory = ", "\"", x@run.dir, "\"", "\n"))
  cat(paste0("driving data = ", "\"", x@driving.data, "\"", "\n"))
  cat(paste0("lon-lat offset = (", x@lonlat.offset[1], ",", x@lonlat.offset[2], ")\n"))
  cat(paste0("year offset = ", x@year.offset, "\n"))
  cat(paste0("resolution tolerance = ", x@tolerance, "\n"))
  cat(paste0("london.centre = ", x@london.centre, "\n"))
  cat(paste0("landuseSimulated = ", x@landuseSimulated, "\n"))
  cat(paste0("institute = ", "\"", x@institute, "\"", "\n"))
  cat(paste0("contact = ", "\"", x@contact, "\"", "\n"))
  cat(paste0("PFT superset (all possible, not just those in the run):", "\n"))
  for(PFT in x@pft.set){
    print(PFT)
  }
  
}

#' @rdname print.object-methods
#' @export
print.ModelRun <- function(x, ...) {
  
  print(as(x, "ModelRunInfo"))
  cat("The following ModelObjects have been stored internally in this ModelRun:\n")
  if(length(x@objects) > 0){
    for(object in x@objects) cat(paste0(object@id, "\n"))
  }
  else{
    cat("(None)\n")
  }

}


#' @rdname print.object-methods
#' @export
print.ModelObject <- function(x, ...) {
  
  cat(paste0("Model object:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  #cat(paste0("name = ", x@name, "\"", "\n"))
  print(x@quant)
  cat(paste0("Spatially Averaged =  ",  x@is.spatially.averaged, "\n"))
  print(x@spatial.extent)
  cat(paste0("Temporally Averaged =  ",  x@is.temporally.averaged, "\n"))
  print(x@temporal.extent)
  cat(paste0("Is a site single (not really used) =  ",  x@is.site, "\n"))
  cat(paste0("Data: ",  "\n"))
  print(x@data)
  cat(paste0("Model Run = ", "\"", x@run@name, "\"", "\n"))
  cat("For full ModelRun metadata type \"print(X@run)\", where X is this ModelObject")
  
  }

#' @rdname print.object-methods
#' @export
print.DatasetInfo <- function(x, ...) {
  
  cat(paste0("DatasetInfo:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  print(x@quant)
 
  
}

#' @rdname print.object-methods
#' @export
print.DataObject <- function(x, ...) {
  
  cat(paste0("DataObject:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", x@name, "\"", "\n"))
  print(x@quant)
  #cat(paste0("Spatially Averaged =  ",  x@is.spatially.averaged, "\n"))
  print(x@spatial.extent)
  #cat(paste0("Temporally Averaged =  ",  x@is.temporally.averaged, "\n"))
  print(x@temporal.extent)
  cat("Data:")
  print(x@data)
  if(x@correction.layer == "") cat("No correction layer has been defined")
  else cat(paste0("Correction layer is \"", x@correction.layer, "\""))

}


#' @rdname print.object-methods
#' @export
print.ComparisonLayer <- function(x, ...) {
  
  cat(paste0("Comparison Layer:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", x@name, "\"", "\n"))
  print(x@quant)
  cat(paste0("Spatially Averaged =  ",  x@is.spatially.averaged, "\n"))
  print(x@spatial.extent)
  cat(paste0("Temporally Averaged =  ",  x@is.temporally.averaged, "\n"))
  print(x@temporal.extent)
  cat(paste0("Is a site single (not really used) =  ",  x@is.site, "\n"))
  cat(paste0("Data: ",  "\n"))
  print(x@data)
  cat(paste0("Source for first layer: \n"))
  print(x@info1)
  cat(paste0("Source for second layer: \n"))
  print(x@info2)

}



#' @rdname print.object-methods
#' @export
print.SpatialComparison <- function(x, ...) {
  
  cat(paste0("Spatial Comparison:\n"))
  cat("Continuous comparison metrics")
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("Mean Error (ME) = ", round(x@ME,4),  "\n"))
  cat(paste0("Normalised Mean Error (NME) = ", round(x@NME,4),  "\n"))  
  cat(paste0("Normalised Mean Squares Error (NMSE) = ", round(x@NMSE,4),  "\n"))
  cat(paste0("Root Mean Squared Error (RMSE) = ", round(x@RMSE,4),  "\n"))
  cat(paste0("Coefficient of Determination (R2) = ", round(x@R2,4),  "\n"))
  cat(paste0("Nash Sutcliffe Modell Efficiency (R2.eff) = ", round(x@R2.eff,4),  "\n"))  
  cat(paste0("Pearson Product Moment Correlation Coefficient (P.cor) = ", round(x@P.cor,4),  "\n"))  
  cat(paste0("Standard Deviation of Errors (sd.diff) = ", round(x@sd.diff,4),  "\n"))  
  cat("Relative proportion comparison metrics")
  cat(paste0("Manhattan Metric (MM) = ", round(x@MM,4),  "\n"))
  cat(paste0("Square Chord Distance (SCD) = ", round(x@SCD,4),  "\n"))  
  cat("Categorical comparison metrics")
  cat(paste0("(Cohen's) Kappa (Kappa) = ", round(x@Kappa,4),  "\n"))  
  for(kappa in x@individual.Kappas) {
    print(kappa)
  }

}


#' @rdname print.object-methods
#' @export 
print.BiomeScheme <- function(x, ...) {
  
  cat(paste0("Biome Scheme:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", x@name, "\"", "\n"))
  cat(paste0("Combine shade tolerance classes: ", x@combineShadeTolerance, "\n"))  
  cat(paste0("Totals needed: ", paste0(x@totals.needed, collapse = ' '), "\n"))
  cat(paste0("Maximums needed: ", paste0(x@max.needed, collapse = ' '), "\n"))
  cat(paste0("Fraction of Total needed: ", paste0(x@fraction.of.total, collapse = ' '), "\n"))
  cat(paste0("Fraction of Tree needed: ", paste0(x@fraction.of.tree, collapse = ' '), "\n"))
  cat(paste0("Fraction of Toody needed: ", paste0(x@fraction.of.woody, collapse = ' '), "\n"))
  cat(paste0("Need Annual GDD5 data: ", "\"", x@needGDD5,  "\"", "\n"))  
  cat(paste0("Data reference: ", "\"", x@data.reference,  "\"", "\n"))  
  cat(paste0("Published reference: ", "\"", x@published.reference,  "\"", "\n"))  
  cat(paste0("Biomes:\n"))
  for(type in x@units) {
    cat(paste0("     ", type,"\n"))    
  }

}


