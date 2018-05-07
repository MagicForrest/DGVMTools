#!/usr/bin/Rscript



###################################################################################################
######### BIOME CLASSIFICATION
#'
#' Perform biome classification using this.Field
#' 
#' This is very inflexible as it only allows the calcualtion of biomes with only one Quantity.  This is not suitable for many biomes schemes, 
#' so this will need to be re-written
#' 
#' 
#' @param input The Field for which to calculate the biomes.
#' @param scheme The biome scheme to use.
#' @return A new Field with the biomes
#' @export
#' @import data.table
#' @seealso BiomeScheme-class
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
calcBiomes <-function(input, scheme){
  
  message(paste("Classifying biomes using scheme", scheme@name, sep = " "))
  
  Grass = Biome = NULL
  
  ### Add the relevant totals, fractions and dominant PFTs which are needed for the classifaction
 
  # If GDD5 required for classification
  #if(scheme@needGDD5 && !any(names(input@data)=="GDD5")) {
  # get gdd5
  if(scheme@needGDD5){
    temp.model.run <- new("Field", input@source)
    gdd5 <- getField(temp.model.run, 
                     "gdd5", 
                     first.year = input@first.year, 
                     last.year = input@last.year,
                     read.full = FALSE)
    dt <- input@data
    dt.gdd5 <- gdd5@data
    dt <- dt[dt.gdd5]
    input@data <- dt
    rm(temp.model.run)
  }
  
  # Get the maximums needed
  for(max.of.layers in scheme@max.needed) {
    input <- layerOp(x = input, operator = "max.layer", layers = max.of.layers[[1]], new.layer = max.of.layers[[2]])
  }

  # Get the totals needed
  for(total.of.layers in scheme@totals.needed) {
    input <- layerOp(x = input, operator = "sum", layers = total.of.layers[[1]], new.layer = total.of.layers[[2]])
  }
  # maybe not needed
  if(!"Grass" %in% names(input@data) ) {
    input <- input@data[, Grass := 0]
  }
  
  # Get the fractions required - now somewhat long way around (since remove divideLayers() but whatevs) 
  if(length(scheme@fractions.needed) > 0) {
    for(fractions in scheme@fractions.needed) {
      input <- layerOp(input, "/", c(fractions[[1]], fractions[[2]]), fractions[[3]])
    }
  }

 
  
  # We get a warning about a shallow copy here, suppress it
  suppressWarnings(dt <- input@data)
  
  # Apply biome rules and return
  if(scheme@id %in% names(dt)) { dt[, scheme@id := NULL, with=FALSE] }
  
  # get spatial and temporal columns
  st.cols <- getDimInfo(input)
  
  # calculate the biomes (first add it to the existing data.table then subset to make a new data.table and then delete the column from the original)
  suppressWarnings(dt[, Biome := as.factor(apply(dt[,,with=FALSE],FUN=scheme@rules,MARGIN=1))])
  biome.dt <- dt[,append(st.cols, "Biome"), with = FALSE]
  dt[, Biome := NULL]
  message("Done.")
  
  # now make a new Field and return
  sta.info <- as(input, "STAInfo")
  biomes <- new("Field",
                id = makeFieldID(source = input@source, 
                                 var.string = scheme@id, 
                                 sta.info = sta.info),
                data = biome.dt,
                quant = as(scheme, "Quantity"),
                sta.info,
                source = input@source)
  
  return(biomes)
  
}
