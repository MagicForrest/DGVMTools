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
  
  # Get each layer
  for(layer in scheme@layers.needed) {
    
    # build a list of arguments and call layerOp    
    input <- do.call(layerOp, args = list(x=input, operator = layer$operator, layers = layer$layers, new.layer = layer$new.layer))
   
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
