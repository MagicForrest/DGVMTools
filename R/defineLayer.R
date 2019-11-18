######## DEFINE A LAYER OBJECT 

#' @title Define a Layer object
#' 
#' @description This function defines a \linkS4class{Layer} object represents a sub-component or one particular aspect of a \linkS4class{Field} object, 
#' for example one PFT or one carbon pool.  It preferred to a \code{new("Layer",...)} initialisation because it does both the initialisation 
#' and also optionally adds the Field to to a \linkS4class{Format} object with the \code{add.to} argument.
#' 
#' @param id A unique character string to identify this newly defined \linkS4class{Layer}. This could be, for example, a PFT abbreviation.
#' @param name A more readable character string to describe the \linkS4class{Layer}, for example the full PFT name.  
#' Optional, if omitted the id argument will be used here.
#' @param colour A character string giving the preferred plot colour for this \linkS4class{Layer}.
#' @param properties A list with named items containing metadata describing the Layer.  These are used with the \link{whichLayers} and \link{layerOp}
#' functions to automagically select layers.  There is a lot of flexibility here, but it is recommended that the list includes at least an
#'  element called "type", and then ideally all the proprties required to describe the layer.  An example for a broadleaved summergreen tree PFT could look like: \cr
#' \code{properties = list(type = "PFT", growth.form = "Tree", phenology = "Summergreen")}.  \cr \cr
#' A slow litter soil carbon pool could look like: \cr
#' \code{properties = list(type = "CPool", speed = "Slow", zone = "Soil", comprises = "Litter")}. 
#' @param add.to A Format object to which this newly defined layer should be added (optional). 
#' 
#' 
#' @details Only the \code{id} and \code{colour} argument are compulsory, the rest will be filled with dummy/default values if left blank.
#' However, one advantage of formally defining a \linkS4class{Layer} object is that one can use the \code{properties} slot of the 
#' \linkS4class{Layer} to conveniently select, aggregate and operate on layers with functions \link{whichLayers} and \link{layerOp},
#' so it is recommended to define the properties to take advantage of this. 
#' When defining Layers it is important that the properties are defined consistently between Layer so that the functions 
#' \link{whichLayers} and \link{layerOp} work as expected. \cr
#' Note that no actual data is stored in the resultant \linkS4class{Layer}, only metadata. 
#' 
#' @return A \linkS4class{Layer} object
#' @export
#' @seealso \linkS4class{Layer}, \linkS4class{Format}, \link{whichLayers} and \link{layerOp}. 
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
#' 
#' @examples
#' 
#' ## Define a couple of new Layers
#' 
#' 
#' ## a hypothetical shrub PFT and add it to the GUESS format
#' TeBES.PFT <- defineLayer(id = "TeBES",
#'                        name = "Temperate Broadleaved Evergreen Shrub",
#'                        colour = "red",
#'                        properties = list(type = "PFT",      
#'                                          climate.zone = "Temperate",
#'                                          leaf.form = "Broadleaved",
#'                                          phenology = "Evergreen",
#'                                          growth.form = "Shrub"),
#'                        add.to = GUESS)
#' 
#' # check is is there: 
#' print(GUESS@defined.layers)
#' 
#' ## Also add a couple of aggregate Layers
#' 
#' # Note, not defining growth.form etc because they might get double counted 
#' Grass  <- defineLayer(id = "Grass",
#'                        name = "Grass Total",
#'                        colour = "red",
#'                        properties = list(type = "Aggregate"),
#'                        add.to = GUESS)
#'                        
#' Tree  <- defineLayer(id = "Tree",
#'                        name = "Tree Total",
#'                        colour = "gray",
#'                        properties = list(type = "Aggregate"),
#'                        add.to = GUESS)
#' 
#'
#' \donttest{
#'  
#' ## Show the plot colours in action
#' 
#' # Load a multi-Layer (one layer per-PFT) Field from the example data averaged in space
#' run.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(id = "LPJ-GUESS_Example", dir = run.dir,  format = GUESS)
#' test.Field <- getField(source = test.Source, var = "lai", spatial.aggregate.method = "mean", 
#'                        spatial.extent.id = "CentralEurope") 
#' 
#' # make calculate Tree and Grass sums
#' test.Field <- layerOp(test.Field, "+", ".Tree", "Tree")
#' test.Field <- layerOp(test.Field, "+", ".Grass", "Grass")
#' 
#' # plot - notice the defined colours for the Tree and Grass layer are plotted
#' print(plotTemporal(test.Field))
#' 
#' # also plot all the 'Aggregate' layers
#' print(plotTemporal(test.Field, whichLayers(test.Field, criteria = "Aggregate")))
#' 
#' 
#' }
#' 
#' @export

defineLayer <- function(id,
                        name = id,
                        colour,
                        properties = list(type = "Undefined"),
                        add.to){
  
  # check that properties is a full-named list
  if(!is.list(properties) || length(names(properties)) != length(properties) || "" %in% names(properties)) stop("The 'properties' argument to defineLayer() must be a named list")
  
  # make Source object from the supplied meta data
  layer <- new("Layer",
               id = id,
               name = name,
               colour = colour,
               properties = properties)
  
  if(!missing(add.to)) {
    
    # quick sanity check, require that add.to is a Format 
    if(!is.Format(add.to)) stop("add.to argument to defineLayer() must be a Format object (if supplied)")
    
    # okay, so we definitely have a Format object, so we have some already defined layers
    these.defined.layers <- add.to@defined.layers
    
    # now look for a Layer with the required id, in the exisiting Format
    # and if it exists get its index to over-write it but also give a warning
    # if not, add it to the end
    this.index <- length(these.defined.layers)+1
    for(this.layer.index in 1:length(these.defined.layers)){
      if(these.defined.layers[[this.layer.index]]@id == id) {
        warning(paste("Function defineLayer() is replacing an exisiting Layer with id =", id, "in Format", deparse(substitute(add.to)), sep = " "))
        this.index <- this.layer.index
      }
    }
    these.defined.layers[[this.index]] <- layer
    
    # make an updated Format object
    this.format <- copy(add.to)
    this.format@defined.layers <- these.defined.layers
    
    # finally, add the updated Format object to the global environment
    assign(x = deparse(substitute(add.to)), value = this.format, pos = "package:DGVMTools")
    
  }
  
  # return the Layer
  return(layer)
  
  
}

