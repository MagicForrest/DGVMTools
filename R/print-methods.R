#!/usr/bin/Rscript

#' Print a DGVMTools object
#' 
#' Print a DGVMTools object in a reasonably nice and space efficient way. 
#' 
#' @param x Any DGVMTools object to be printed
#' @param ... other arguments to be passed on to a further print functions, not currently used.
#' @name print
#' @rdname print
#' @aliases print
#' 
#' @details Simple stuff, implemented as S4 methods.  Further tweaking of the formatting may be in order, feedback from users welcome.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
NULL

setGeneric("print", function(x, ...) standardGeneric("print"))


#' @rdname print
#' @export
setMethod("print", signature(x="PFT"), function(x) {
  
  cat(paste0("PFT: ", x@id," (", x@name, "): ", "Growth form=",  x@growth.form, ", Leaf form=", x@leaf.form, ", Phenology=", x@phenology, ", Climate zone=", x@climate.zone, ", Shade tolerance=", x@shade.tolerance, ", Preferred colour=", x@colour, "\n"))

})

#' @rdname print
#' @export
setMethod("print", signature(x="Quantity"), function(x) {
  
  cat(paste0("Quantity: ", x@id," (", x@name, "): ",  ", Units=", x@units, ", Defined for format: ", paste0(unlist(x@format), collapse = ', '), "\n"))

})





#' @rdname print
#' @export
setMethod("print", signature(x="Period"), function(x) {
  
  cat(paste0("Sub-annual time period:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  cat(paste0("abbreviation = ", "\"", x@abbreviation, "\"", "\n"))
  cat(paste0("index = ", x@index, "\n"))
  cat(paste0("padded.index = ", "\"", x@padded.index, "\"", "\n"))
  cat(paste0("contains = ", x@contains, "\n"))
  cat(paste0("days = ", x@days, "\n"))
  cat(paste0("days.leap = ", x@days.leap, "\n"))
  
})




#' @rdname print
#' @export
setMethod("print", signature(x="Source"), function(x) {
  
  cat(paste0("Source:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", "\"", x@name, "\"", "\n"))
  cat(paste0("format = ", "\"", x@format@id, "\"", "\n"))
  cat(paste0("directory = ", "\"", x@dir, "\"", "\n"))
  cat(paste0("forcing data = ", "\"", x@forcing.data, "\"", "\n"))
  cat(paste0("lon-lat offset = (", x@lonlat.offset[1], ",", x@lonlat.offset[2], ")\n"))
  cat(paste0("year offset = ", x@year.offset, "\n"))
  cat(paste0("london.centre = ", x@london.centre, "\n"))
  cat(paste0("land.use.included = ", x@land.use.included, "\n"))
  cat(paste0("institute = ", "\"", x@institute, "\"", "\n"))
  cat(paste0("contact = ", "\"", x@contact, "\"", "\n"))
  cat(paste0("PFT superset (all possible, not just those in the run):", "\n"))
  for(PFT in x@pft.set){
    print(PFT)
  }
  
})



#' @rdname print
#' @export
setMethod("print", signature(x="Field"), function(x) {
  
  cat(paste0("Field:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  #cat(paste0("name = ", x@name, "\"", "\n"))
  print(x@quant)
  cat(paste0("Spatial aggregation =  ",  x@spatial.aggregate.method, "\n"))
  print(x@spatial.extent)
  cat(paste0("Yearly aggregation =  ",  x@year.aggregate.method, "\n"))
  print(x@first.year)
  print(x@last.year)
  cat(paste0("Data: ",  "\n"))
  print(x@data)
  cat(paste0("Source = ", "\"", x@source@name, "\"", "\n"))
  cat("For full Source metadata type \"print(X@source)\", where X is this Field\n")
  
})



#' @rdname print
#' @export
setMethod("print", signature(x="Comparison"), function(x) {
  
  cat(paste0("Comparison Layer:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", x@name, "\"", "\n"))
  print(x@quant)
  cat(paste0("Spatially Averaged =  ",  x@spatial.aggregate.method, "\n"))
  print(x@spatial.extent)
  cat(paste0("Temporally Averaged =  ",  x@temporal.aggregate.method, "\n"))
  print(x@temporal.extent)
  cat(paste0("Data: ",  "\n"))
  print(x@data)
  cat(paste0("Source for first layer: \n"))
  print(x@source1)
  cat(paste0("Source for second layer: \n"))
  print(x@source2)

})



#' @rdname print
#' @export
setMethod("print", signature(x="Statistics"), function(x) {
  
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

})


#' @rdname print
#' @export 
setMethod("print", signature(x="BiomeScheme"), function(x) {
  
  cat(paste0("Biome Scheme:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", x@name, "\"", "\n"))
  cat(paste0("Totals needed: ", paste0(x@totals.needed, collapse = ' '), "\n"))
  cat(paste0("Maximums needed: ", paste0(x@max.needed, collapse = ' '), "\n"))
  #cat(paste0("Fraction of Total needed: ", paste0(x@fraction.of.total, collapse = ' '), "\n"))
  #cat(paste0("Fraction of Tree needed: ", paste0(x@fraction.of.tree, collapse = ' '), "\n"))
  #cat(paste0("Fraction of Toody needed: ", paste0(x@fraction.of.woody, collapse = ' '), "\n"))
  cat(paste0("Need Annual GDD5 data: ", "\"", x@needGDD5,  "\"", "\n"))  
  cat(paste0("Data reference: ", "\"", x@data.reference,  "\"", "\n"))  
  cat(paste0("Published reference: ", "\"", x@published.reference,  "\"", "\n"))  
  cat(paste0("Biomes:\n"))
  for(type in x@units) {
    cat(paste0("     ", type,"\n"))    
  }

})

#' @rdname print
#' @export 
setMethod("print", signature(x="Format"), function(x) {
  
  cat(paste0("Format:\n"))
  cat(paste0("id = \"", x@id, "\"", "\n"))
  cat(paste0("Default PFTs:\n"))
  for(PFT in x@default.pfts){
    print(PFT)
  }
  cat(paste0("Defined Quantities:\n"))
  for(quant in x@quantities){
    print(quant)
  }
  
 
})


