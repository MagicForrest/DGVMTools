######## DEFINE A QUANTITY OBJECT 

#' @title Define a Quantity object
#' 
#' @description This function defines a \linkS4class{Quantity} object which represents physical variables say LAI or evaoptranspiration.  It is 
#' preferred to a \code{new("Quantityr",...)} initialisation because it does  the initialisation (inclduing some defaults)
#' and also optionally adds the newly defined \linkS4class{Quantity} to a \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} object with the \code{add.to} argument.
#' 
#' @param id A unique character string to identify this newly defined \linkS4class{Quantity}. This could be, for example, a "LAI" or "mNPP". (mandatory)
#' @param name A more readable character string to describe the \linkS4class{Quantity}, for example the "Leaf Area Index" or "Monthly NPP".  
#' Optional, if omitted the id argument will be used here.
#' @param units A character string describing the units of the \linkS4class{Quantity} (mandatory)
#' @param colours A  functions giving the preferred colour palette for this \linkS4class{Quantity}.  Defaults to viridis.
#' @param format A character string (or a list of character strings) defining which \linkS4class{Format} this quantity is defined for.  Should match the id/ids
#' of the \linkS4class{Format}s.
#' @param standard_name A character string givien the standard CF-compliant name for writing netCDF files, e.g. "leaf_area_index" (optional, such standard names
#' don't exist for all variables)
#' @param add.to A \linkS4class{Format}, or a \linkS4class{Source}, or a \linkS4class{Field} object to which this newly defined
#'  \linkS4class{Quantity} should be added (optional). If this is specified then the defineQuantity() returns the
#'   \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} with the \linkS4class{Quantity} added. Note that for a 
#'   \linkS4class{Field} this over-writes the  previous \linkS4class{Quantity} (ie the \code{'quant'} slot) of the \linkS4class{Field}
#' 
#' 
#' @details Use this function when you either:
#' \itemize{
#'  \item{} Want to examine new output from a model/dataset which is not defined in the default \linkS4class{Format}.  You can specify the \linkS4class{Format}
#'  (or \linkS4class{Source}) in the '\code{add.to}' argument so that this \linkS4class{Quantity} is sensibly defined and visible.
#'  \item{} Have modified a \linkS4class{Format} or calculated a new one entirely, and then want to update the \linkS4class{Quantity} to reflext this.  
#'  In this case you can specify the modified/created \linkS4class{Field} in the '\code{add.to}' argument so that the \linkS4class{Field} is updated.#'  
#' } \cr \cr
#' 
#' Note that when the '\code{add.to}' argument is used the function returns a modified version of that object *not*  a \linkS4class{Quantity} \cr \cr
#' 
#' Only the \code{id} and \code{units} arguments are compulsory, the rest will be filled with dummy/default values if left blank.
#' Note that no actual data is stored in the resultant \linkS4class{Quantity}, only metadata. 
#' 
#' @return Either an \linkS4class{Quantity} object (if "add.to" argument not supplied), or an updated \linkS4class{Format}/\linkS4class{Source}/\linkS4class{Field} 
#' object if "add.to" was specified.
#' @export
#' @seealso \linkS4class{Source}, \linkS4class{Format}, \linkS4class{Field}, \link{availableQuantities}, \link{addTo}
#' @include classes.R
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 
#' 
#' @examples
#' 
#' ## Define a new Quantity
#' 
#' ## eg. C turnover time
#' Cturnover.Quantity <- defineQuantity(id = "Cturnover",
#'                                      name = "Carbon Turnover Time",
#'                                      colours = viridis::magma,
#'                                      units = "y") # note no CF standard name is defined
#' 
#' # print it
#' print(Cturnover.Quantity)
#' 
#' # and add it to the GUESS format and check it is there
#' GUESS.updated <- addTo(Cturnover.Quantity, GUESS)
#' print(GUESS.updated@quantities)
#'
#' ## Also add it *directly* to the GUESS Source by using the "add.to" argument of the defineQuantity
#' 
#' GUESS.updated2 <- defineQuantity(id = "Cturnover2",
#'                         name = "Carbon Turnover Time",
#'                         colours = viridis::magma,
#'                         units = "y",
#'                         add.to = GUESS) 
#'                        
#'                        
#' # check it out 
#' print(GUESS.updated2@quantities)
#'  
#' \donttest{
#'  
#' # Define a Source and make a dummy test Field
#' run.dir <- system.file("extdata", "LPJ-GUESS_Runs", "CentralEurope", package = "DGVMTools")
#' test.Source <- defineSource(id = "LPJ-GUESS_Example", dir = run.dir,  format = GUESS)
#' test.Field <- getField(source = test.Source, quant = "lai", spatial.aggregate.method = "mean", 
#'                        spatial.extent.id = "CentralEurope") 
#'                        
#' # Define a new Quantity and add it to the Source
#' test.Source.new <- defineQuantity(id = "Cturnover",
#'                         name = "Carbon Turnover Time",
#'                         colours = viridis::magma,
#'                         units = "y",
#'                         add.to = test.Source)
#'                         
#' # check that it is added correctly to the end of @quantities slot of the @format slot of the Source
#' print(test.Source.new@format@quantities[[length(test.Source@format@quantities)]]) 
#' 
#' ## also define it for the test Field
#' ## note that is actually changes the Quantity of the Field
#' test.Field <- defineQuantity(id = "Cturnover",
#'                         name = "Carbon Turnover Time",
#'                         colours = viridis::magma,
#'                         units = "y",
#'                         add.to = test.Field)
#'  print(test.Field)    
#'                     
#' # plot - notice the updated metadata 
#' print(plotTemporal(test.Field))
#'
#' 
#' }

defineQuantity <- function(id,
                        name = id,
                        units,
                        colours = viridis::viridis,
                        format = "Standard",
                        standard_name = "unknown", 
                        add.to){
  
  # check the important arguments
  if(missing(id) || !is.character(id)) stop("Please supply id (as a simple alphanumeric character string, no spaces or special characters) when defining a Quantity")
  if(missing(units) || !is.character(units)) stop("Please supply units (as a character string) when defining a Quantity")
  if(missing(name)) warning("No nice human-readable 'name' argument supplied when calling defineQuantity.  This is not a huge problem (just using 'id' instead), but plot axis/titles might look ugly.")
  
 
  # make Quantity object from the supplied meta data
  quantity <- new("Quantity",
               id = id,
               name = name,
               units = units,
               colours = colours,
               format = format,
               standard_name = standard_name)
  
  # return updated object if "add.to" argument supplied
  if(!missing(add.to)) {
    return(addTo(quantity, add.to))
  }
  # if not, return the Layer
  else return(quantity)
  
}

