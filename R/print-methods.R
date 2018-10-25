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
  
  cat(paste0("Quantity:\n"))
  cat(paste0("\t\t", x@id," (", x@name, "): ",  "Units=", x@units, ", Defined for format: ", paste0(unlist(x@format), collapse = ', '), "\n"))
  
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
  cat(paste0("\tid = ", "\"", x@id, "\"", "\n"))
  cat(paste0("\tname = ", "\"", x@name, "\"", "\n"))
  cat(paste0("\tformat = ", "\"", x@format@id, "\"", "\n"))
  cat(paste0("\tlon-lat offset = (", x@lonlat.offset[1], ",", x@lonlat.offset[2], ")\n"))
  cat(paste0("\tyear offset = ", x@year.offset, "\n"))
  cat(paste0("\tdirectory = ", "\"", x@dir, "\"", "\n"))
  cat(paste0("\tforcing data = ", "\"", x@forcing.data, "\"", "\n"))
  cat(paste0("\tlondon.centre = ", x@london.centre, "\n"))
  cat(paste0("\tland.use.included = ", x@land.use.included, "\n"))
  cat(paste0("\tinstitute = ", "\"", x@institute, "\"", "\n"))
  cat(paste0("\tcontact = ", "\"", x@contact, "\"", "\n"))
  cat(paste0("\tPFTs defined:\n"))
  all.PFTs <- c()
  for(PFT in x@pft.set){
    all.PFTs <- append(all.PFTs, PFT@id)
  }
  cat(paste0(all.PFTs))
  

})


#' @rdname print
#' @export
setMethod("print", signature(x="STAInfo"), function(x) {
  
  cat(paste0("Spatial Info:\n"))
  cat(paste0("\t\tSpatial aggregation = ",  x@spatial.aggregate.method, "\n"))
  cat(paste0("\t\tSpatial extent id = ",  x@spatial.extent.id, "\n"))
  cat(paste0("\t\tSpatial extent: \n"))
  extent.string <- utils::capture.output(print(x@spatial.extent))
  for(part in extent.string){
    cat(paste0("\t\t\t", part, "\n"))
  }
  cat(paste0("Year Info:\n"))
  cat(paste0("\t\tYearly aggregation =  ",  x@year.aggregate.method, "\n"))
  cat(paste0("\t\tFirst year = ", x@first.year, "\n"))
  cat(paste0("\t\tLast year = ", x@last.year, "\n"))
  cat(paste0("Subannual Info: \n"))
  cat(paste0("\t\tSubannual original = ",  x@subannual.original, "\n"))
  cat(paste0("\t\tSubannual aggregation = ",  x@subannual.aggregate.method, "\n"))
  cat(paste0("\t\tSubannual resolution = ",  x@subannual.resolution, "\n"))
  
})


#' @rdname print
#' @export
setMethod("print", signature(x="Field"), function(x) {
  
  cat(paste0("A Field object\n"))
  print(x@quant)
  cat(paste0("Id:\n"))
  cat(paste0("\t\t", "\"", x@id,  "\"", "\n"))
  cat(paste0("Layers: ",  "\n"))
  cat(paste0("\t\t", paste0(names(x), collapse = " "), "\n"))
  cat(paste0("Dimensions: ",  "\n"))
  cat(paste0("\t\t", paste0(getDimInfo(x), collapse = " "), "\n"))
  print(as(x, "STAInfo"))
  cat(paste0("Data: ",  "\n"))
  print(x@data)
  cat("\n")
  cat(paste0("Source name:\n"))
  cat(paste0("\t\t ", "\"", x@source@name, "\"", "\n"))
  cat("(For full Source metadata type \"print(X@source)\", where X is this Field)\n")
  
})



#' @rdname print
#' @export
setMethod("print", signature(x="Comparison"), function(x) {
  
  cat(paste0("Comparison Layer:\n"))
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("name = ", x@name, "\"", "\n"))
  print(x@stats)
  cat(paste0("First layers: \n"))
  print(x@layers1)
  cat(paste0("Second layers: \n"))
  print(x@layers2)
  cat(paste0("Quantity for first layer: \n"))
  print(x@quant1)
  cat(paste0("Quantity of second layer: \n"))
  print(x@quant2)
  cat(paste0("STAInfo for first layer: \n"))
  print(x@sta.info1)
  cat(paste0("STAInfo of second layer: \n"))
  print(x@sta.info2)
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
  cat(" ** Continuous comparison metrics ** \n")
  cat(paste0("id = ", "\"", x@id, "\"", "\n"))
  cat(paste0("Mean Error (ME) = ", round(x@ME,4),  "\n"))
  cat(paste0("Normalised Mean Error (NME) = ", round(x@NME,4),  "\n"))  
  cat(paste0("Normalised Mean Squares Error (NMSE) = ", round(x@NMSE,4),  "\n"))
  cat(paste0("Root Mean Squared Error (RMSE) = ", round(x@RMSE,4),  "\n"))
  cat(paste0("Coefficient of Determination (R2) = ", round(x@R2,4),  "\n"))
  cat(paste0("Nash Sutcliffe Modell Efficiency (R2.eff) = ", round(x@R2.eff,4),  "\n"))  
  cat(paste0("Pearson Product Moment Correlation Coefficient (P.cor) = ", round(x@P.cor,4),  "\n"))  
  cat(paste0("Standard Deviation of Errors (sd.diff) = ", round(x@sd.diff,4),  "\n"))  
  cat(" ** Relative proportion comparison metrics ** \n")
  cat(paste0("Manhattan Metric (MM) = ", round(x@MM,4),  "\n"))
  cat(paste0("Square Chord Distance (SCD) = ", round(x@SCD,4),  "\n"))  
  cat(" ** Categorical comparison metrics ** \n")
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
  #cat(paste0("Totals needed: ", paste0(x@totals.needed, collapse = ' '), "\n"))
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


