######## DEFINE A LAYER OBJECT 

#' @title Define a Layer object
#' 
#' @description This function defines a \linkS4class{Layer} object represents a sub-component or one particular aspect of a \linkS4class{Field} object, 
#' for example one PFT or one carbon pool.  It preferred to a \code{new("Layer",...)} initialisation because it does both the initialisation 
#' and also optionally adds the \linkS4class{Layer} to to a \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} object with the \code{add.to} argument.
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
#' @param add.to A \linkS4class{Format}, or a \linkS4class{Source}, or a \linkS4class{Field} object to which this newly defined layer should be 
#' added (optional). If this is specified then the defineQuantity() returns the  \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} 
#' with the \linkS4class{Quantity} added.
#' 
#' @details Only the \code{id} and \code{colour} arguments are compulsory, the rest will be filled with dummy/default values if left blank.
#' However, one advantage of formally defining a \linkS4class{Layer} object is that one can use the \code{properties} slot of the 
#' \linkS4class{Layer} to conveniently select, aggregate and operate on layers with functions \link{whichLayers} and \link{layerOp},
#' so it is recommended to define the properties to take advantage of this. 
#' When defining Layers it is important that the properties are defined consistently between Layer so that the functions 
#' \link{whichLayers} and \link{layerOp} work as expected. \cr
#' Note that no actual data is stored in the resultant \linkS4class{Field}, only metadata. 
#' 
#' @return Either an \linkS4class{Layer} object (if "add.to" argument not supplied), or an updated \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} 
#' object if "add.to" was specified.
#' @export
#' @seealso \linkS4class{Layer}, \linkS4class{Format}, \link{whichLayers}, \link{layerOp}, \link{addTo}
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
#' 
#' @examples
#' 
#' ## Define a couple of new Layers
#' 
#' 
#' ## a hypothetical shrub PFT 
#' TeBES.PFT <- defineLayer(id = "TeBES",
#'                        name = "Temperate Broadleaved Evergreen Shrub",
#'                        colour = "red",
#'                        properties = list(type = "PFT",      
#'                                          climate.zone = "Temperate",
#'                                          leaf.form = "Broadleaved",
#'                                          phenology = "Evergreen",
#'                                          growth.form = "Shrub"))
#' 
#' # print it
#' print(TeBES.PFT)
#' 
#' # and add it to the GUESS format and check it is there
#' GUESS <- addTo(TeBES.PFT, GUESS)
#' print(GUESS@predefined.layers)
#' 
#' ## Also add a couple of aggregate Layers *directly* to the GUESS source
#' ## by using the "add.to" argument
#' 
#' # Note, not defining growth.form etc because they might get double counted 
#' GUESS  <- defineLayer(id = "Grass",
#'                        name = "Grass Total",
#'                        colour = "red",
#'                        properties = list(type = "Aggregate"),
#'                        add.to = GUESS)
#'                        
#' GUESS  <- defineLayer(id = "Tree",
#'                        name = "Tree Total",
#'                        colour = "gray",
#'                        properties = list(type = "Aggregate"),
#'                        add.to = GUESS)
#'                        
#'                        
#' # check them out 
#'  print(GUESS@predefined.layers)
#'
#' \donttest{
#'  
#' ## Show the plot colours in action
#' 
#' # Load a multi-Layer (one layer per-PFT) Field from the example data averaged in space
#' run.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(id = "LPJ-GUESS_Example", dir = run.dir,  format = GUESS)
#' test.Field <- getField(source = test.Source, quant = "lai", spatial.aggregate.method = "mean", 
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
#' print(whichLayers(test.Field, criteria = "Aggregate"))
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
  
  # make Layer object from the supplied meta data
  layer <- new("Layer",
               id = id,
               name = name,
               colour = colour,
               properties = properties)
  
  # return updated object if "add.to" argument supplied
  if(!missing(add.to)) {
    return(addTo(layer, add.to))
  }
  # if not, return the Layer
  else return(layer)
  
}

