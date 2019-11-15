#!/usr/bin/Rscript

#' Show a DGVMTools object
#'
#' Show a DGVMTools object in a reasonably nice and space efficient way.
#'
#' @param object Any DGVMTools object to be shown (printed)
#' @param ... other arguments to be passed on to a further show functions, not currently used.
#' @name show
#'
#' @importMethodsFrom methods show
#' @exportMethod show
#' 
#' @details Simple stuff, implemented as both S3 and S4 methods.  The reason for both is that appears S3 needs to be implemented for printing inside lists
#' or other objects. Further tweaking of the formatting may be in order, feedback from users welcome.
#'
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
NULL


if(!isGeneric("show")) setGeneric("show", function(object, ...) standardGeneric("show"))


#' @rdname show
#' @export
setMethod('show', signature(object="Layer"), function(object) {
  
  cat(paste0("Layer: ", object@id," (", object@name, "), plot colour =  ", object@colour, "\n" ))
  prop.string <- "\t With properties:"
  for(this.name in names(object)) {
    prop.string <- paste0(prop.string, this.name, "=", object[[this.name]], ", ")
  }
  if(length(names(object)) > 0) prop.string <- substr(prop.string, 1, nchar(prop.string) -2)
  prop.string <- paste0(prop.string, "\n")
  cat(prop.string)
  
})



#' @rdname show
#' @export
setMethod("show", signature(object="Quantity"), function(object) {
  
  cat(paste0("Quantity:\n"))
  cat(paste0("\t\t", object@id," (", object@name, "): ",  "Units=", object@units, ", Defined for format: ", paste0(unlist(object@format), collapse = ', '), "\n"))
  
})





#' @rdname show
#' @export
setMethod("show", signature(object="Period"), function(object) {
  
  cat(paste0("Sub-annual time period:\n"))
  cat(paste0("id = ", "\"", object@id, "\"", "\n"))
  cat(paste0("name = ", "\"", object@name, "\"", "\n"))
  cat(paste0("abbreviation = ", "\"", object@abbreviation, "\"", "\n"))
  cat(paste0("index = ", object@index, "\n"))
  cat(paste0("padded.index = ", "\"", object@padded.index, "\"", "\n"))
  cat(paste0("contains = ", object@contains, "\n"))
  cat(paste0("days = ", object@days, "\n"))
  cat(paste0("days.leap = ", object@days.leap, "\n"))
  
})




#' @rdname show
#' @export
setMethod("show", signature(object="Source"), function(object) {
  
  cat(paste0("Source:\n"))
  cat(paste0("\tid = ", "\"", object@id, "\"", "\n"))
  cat(paste0("\tname = ", "\"", object@name, "\"", "\n"))
  cat(paste0("\tformat = ", "\"", object@format@id, "\"", "\n"))
  cat(paste0("\tlon-lat offset = (", object@lonlat.offset[1], ",", object@lonlat.offset[2], ")\n"))
  cat(paste0("\tyear offset = ", object@year.offset, "\n"))
  cat(paste0("\tdirectory = ", "\"", object@dir, "\"", "\n"))
  cat(paste0("\tforcing data = ", "\"", object@forcing.data, "\"", "\n"))
  cat(paste0("\tlondon.centre = ", object@london.centre, "\n"))
  cat(paste0("\tland.use.included = ", object@land.use.included, "\n"))
  cat(paste0("\tinstitute = ", "\"", object@institute, "\"", "\n"))
  cat(paste0("\tcontact = ", "\"", object@contact, "\"", "\n"))
  cat(paste0("\tPFTs defined:\n"))
  all.PFTs <- c()
  for(PFT in object@pft.set){
    all.PFTs <- append(all.PFTs, PFT@id)
  }
  cat(paste0(all.PFTs))
  
  
})


#' @rdname show
#' @export
setMethod("show", signature(object="STAInfo"), function(object) {
  
  cat(paste0("Spatial Info:\n"))
  cat(paste0("\t\tSpatial aggregation = ",  object@spatial.aggregate.method, "\n"))
  cat(paste0("\t\tSpatial extent id = ",  object@spatial.extent.id, "\n"))
  cat(paste0("\t\tSpatial extent: \n"))
  extent.string <- utils::capture.output(print(object@spatial.extent))
  for(part in extent.string){
    cat(paste0("\t\t\t", part, "\n"))
  }
  cat(paste0("Year Info:\n"))
  cat(paste0("\t\tYearly aggregation =  ",  object@year.aggregate.method, "\n"))
  cat(paste0("\t\tFirst year = ", object@first.year, "\n"))
  cat(paste0("\t\tLast year = ", object@last.year, "\n"))
  cat(paste0("Subannual Info: \n"))
  cat(paste0("\t\tSubannual original = ",  object@subannual.original, "\n"))
  cat(paste0("\t\tSubannual aggregation = ",  object@subannual.aggregate.method, "\n"))
  cat(paste0("\t\tSubannual resolution = ",  object@subannual.resolution, "\n"))
  
})


#' @rdname show
#' @export
setMethod("show", signature(object="Field"), function(object) {
  
  cat(paste0("A Field object\n"))
  print(object@quant)
  cat(paste0("Id:\n"))
  cat(paste0("\t\t", "\"", object@id,  "\"", "\n"))
  cat(paste0("Layers: ",  "\n"))
  cat(paste0("\t\t", paste0(names(object), collapse = " "), "\n"))
  cat(paste0("Dimensions: ",  "\n"))
  cat(paste0("\t\t", paste0(getDimInfo(object), collapse = " "), "\n"))
  print(as(object, "STAInfo"))
  cat(paste0("Data: ",  "\n"))
  print(object@data)
  cat("\n")
  cat(paste0("Source name:\n"))
  cat(paste0("\t\t ", "\"", object@source@name, "\"", "\n"))
  cat("(For full Source metadata type \"print(object@source)\", where X is this Field)\n")
  
})



#' @rdname show
#' @export
setMethod("show", signature(object="Comparison"), function(object) {
  
  cat(paste0("Comparison Layer:\n"))
  cat(paste0("id = ", "\"", object@id, "\"", "\n"))
  cat(paste0("name = ", object@name, "\"", "\n"))
  cat(paste0("type = ", object@type, "\"", "\n"))
  cat(paste0("statistical metrics: \n"))
  for(counter in 1:length(object@stats)) {
    stat <- object@stats[[counter]]
    if(length(stat) == 1){
      cat(paste0("  ", names(object@stats)[counter], ": ", round(stat, 5), "\n"))
    }
    else {
      print(paste0(names(stat)[counter], ":\n"))
      for(counter2 in 1:length(stat)) {
        cat(paste0("    ", names(stat)[counter2], ": ", round(stat[[counter2]], 5), "\n"))
      }
    }
  }
  cat(paste0("First layers: \n"))
  print(object@layers1)
  print(object@layers2)
  cat(paste0("Quantity for first layer: \n"))
  print(object@quant1)
  cat(paste0("Quantity of second layer: \n"))
  print(object@quant2)
  cat(paste0("STAInfo for first layer: \n"))
  print(object@sta.info1)
  cat(paste0("STAInfo of second layer: \n"))
  print(object@sta.info2)
  cat(paste0("Data: ",  "\n"))
  print(object@data)
  cat(paste0("Source for first layer: \n"))
  print(object@source1)
  cat(paste0("Source for second layer: \n"))
  print(object@source2)
  cat(paste0("\n"))
})



#' @rdname show
#' @export
setMethod("show", signature(object="Scheme"), function(object) {
  
  cat(paste0("Scheme:\n"))
  cat(paste0("id = ", "\"", object@id, "\"", "\n"))
  cat(paste0("name = ", object@name, "\"", "\n"))
  #cat(paste0("Totals needed: ", paste0(object@totals.needed, collapse = ' '), "\n"))
  cat(paste0("Data reference: ", "\"", object@data.reference,  "\"", "\n"))
  cat(paste0("Published reference: ", "\"", object@published.reference,  "\"", "\n"))
  cat(paste0("Categories:\n"))
  for(type in object@units) {
    cat(paste0("     ", type,"\n"))
  }
  
})

#' @rdname show
#' @export
setMethod("show", signature(object="Format"), function(object) {
  
  cat(paste0("Format:\n"))
  cat(paste0("id = \"", object@id, "\"", "\n"))
  cat(paste0("Default PFTs:\n"))
  for(PFT in object@defined.layers){
    print(PFT)
  }
  cat(paste0("Defined Quantities:\n"))
  for(quant in object@quantities){
    print(quant)
  }
  
  
})


