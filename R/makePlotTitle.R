
#####################################################################################################################
################### HELPER FUNCTIONS FOR MAKING  SPATAIL/TEMPORAL PLOT TITLES #######################################
#####################################################################################################################

#' Make a plot title
#' 
#' Build an appropriate plot title from some possibly relevant variables.  
#' It will use a string to represent the quantity (obligatory), and optionally a period and an ID.
#' 
#' @param quantity.str Character string for the quantity plotted
#' @param layer The names of the layer (or another identifier)
#' @param source The Field or Comparison that is being plotted
#' @param first.year The first year of the data plotted
#' @param last.year The last year of the data plotted
#' @param extent.str The spatial extent plotted as described by a character string (optional)
#' @return A character string for use as a plot title
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(quantity.str, layer = NULL, source = NULL, first.year = NULL, last.year = NULL, extent.str = NULL){
  
  
  print(extent.str)
  
  # quantity.str must be supplied
  string <- quantity.str
  
  # A layer name may be supplied
  if(!is.null(layer)) string <- paste(string, layer, sep = " ")

  # A source may be supplied (either a Field or Comparison)
  if(!is.null(source)) {
    if(is.Field(source)) string <- paste(string, source@source@name, sep = " ")
    if(is.Comparison(source)) string <- paste(string, source@name, sep = " ")
  }

  # A temporal extent may be supplied
  in.brackets <- NULL
  if(!is.null(extent.str)) {
    in.brackets <- extent.str
  }

  # a first and last year may be supplied
  if(!is.null(first.year) & !is.null(last.year)){
    if(is.null(in.brackets)) in.brackets <- paste(first.year, last.year, sep = "-")
    else in.brackets <- paste0(in.brackets, ", ", paste(first.year, last.year, sep = "-"))
  }

  # add year and spatial extent if present
  if(!is.null(in.brackets)){
    string <- paste0(string, " (", in.brackets, ")")
  }
  
  
  return(string)
  
}
