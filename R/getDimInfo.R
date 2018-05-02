#' Returns the spatio-temporal information 
#' 
#' This function returns information about the spacio-temporal dimensions of a DataObject, Field, Comparison or data.table
#' 
#' @param x A DataObject, Field, Comparison or data.tables
#' @param info A character string to define what spatio-temporal info you want.  Can be:
#' 
#' \describe{
#' 
#'  \item{"names"}{to give the names of the spatio-temporal dimensions (a character vector)}
#'  \item{"range"}{to give the range of each dimension (list)}
#'  \item{"size"}{to give the number of points in a dimension}
#'  \item{"values"}{to give the actual value for each point in a dimension}
#'  \item{"full"}{to return a data.table with the full spatio-temporal information}
#'
#' }
#' 
#' @return See above
#' @import data.table
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#TODO Guess resolutions

getDimInfo <- function(x, info = "names") {
  
  
  # sort classes
  if(is.Field(x) | is.Comparison(x)) x <- x@data
  else if(class(x)[1] != "data.table" ) stop(paste("Cant get spatio-temporal info from class", class(x)[1], sep = " "))
  
  # set up list and vector/columns present
  if(info == "names" || info == "full") {
    st.info <- c()
  }
  else if(info == "range" || info == "size" || info == "values") {
    st.info <- list()
  }
  else {
    warning(paste("Don't know what spatio-temporal information to return for", info, "so just returning the name of the dimensions present."))
    info <- "names"
    st.info <- c()
  }
  
  
  all.cols <- names(x)
  for(dim in c("Lon", "Lat", "Year", "Season", "Month", "Day")) {
    
    if(dim %in% all.cols) {
      
      if(info == "names"  || info == "full") {
        st.info <- append(st.info, dim)
      }
      else if(info == "range") {
        unique.values <- unique(x[,dim,with=FALSE])
        st.info[[dim]] <- c(min(unique.values), max(unique.values))
      }
      else if(info == "size") {
        st.info[[dim]] <- length(unique(x[[dim]]))
      }
      else if(info == "values") {
        st.info[[dim]] <- unique(x[[dim]])
      }
      
    }
    
  }
  
  if(info == "full") {
    st.info <- x[, st.info, with = FALSE]
    setKeyDGVM(st.info)
  }
  
  return(st.info)
  
}
