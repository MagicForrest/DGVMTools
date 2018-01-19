#' Supported datasets
#' 
#' List of datasets 
#'  
#' @format A list of SourceInfo objects for each of the supported datasets
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
supported.datasets <- list(
  
  Saatchi2011 = new("SourceInfo",
                   id = "Saatchi2011",
                   type = "data",
                   model = "NA",
                   pft.set = list(),
                   name = "Saatchi et al. Veg Carbon",
                   dir = "~/DGVMData/Saatchi2011",                              
                   driving.data = "NA",
                   lonlat.offset =  c(0,0),
                   year.offset = 0,
                   london.centre = TRUE,
                   land.use.included = FALSE,
                   contact = "Sassan S. Saatchi saatchi@jpl.nasa.gov",
                   institute = "Jet Propulsion Laboratory, California Institute of Technology")
  
)