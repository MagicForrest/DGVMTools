
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
makePlotTitle <- function(fields){
  
  
  ####### MAKE A SUBTITLE WITH THE STAINFO
  
  ##### get common STAInfo
  sta.info <- commonSTAInfo(fields)

  ##### make spatial section
  spatial.string <- character(0)
  if(length(sta.info@spatial.aggregate.method) > 0) spatial.string <- paste(spatial.string, sta.info@spatial.aggregate.method)
  if(length(sta.info@spatial.extent.id) > 0 && sta.info@spatial.extent.id != "Full") spatial.string <-paste(spatial.string, sta.info@spatial.extent.id)
  spatial.string <- trimws(spatial.string)
  
  ##### make temporal section
  temporal.string <- character(0)
  if(length(sta.info@year.aggregate.method) > 0) temporal.string <- paste(temporal.string, sta.info@year.aggregate.method)
  if(length(sta.info@first.year) > 0 && length(sta.info@last.year) > 0) temporal.string <- paste(temporal.string, paste(sta.info@first.year, sta.info@last.year, sep = "-"))
  temporal.string <- trimws(temporal.string)
  
  ##### make subannual section
  subannual.string <- character(0)
  if(length(sta.info@subannual.resolution) > 0) subannual.string <- trimws(paste(subannual.string, sta.info@subannual.resolution, sep = " "))
  if(length(sta.info@subannual.aggregate.method) > 0) subannual.string <- trimws(paste(subannual.string, sta.info@subannual.aggregate.method, "of", sep = " "))
  if(length(sta.info@subannual.original) > 0 && length(sta.info@subannual.aggregate.method) > 0 )   {
    if(sta.info@subannual.original != sta.info@subannual.resolution){
    subannual.string <- trimws(paste(subannual.string, sta.info@subannual.original, sep = " "))
    }
  }
    
  #### combine 
  subtitle <- character(0)
  if(length(subannual.string) > 0 )  subtitle <- trimws(paste0(subtitle, ", ", subannual.string))
  if(length(spatial.string) > 0 )  subtitle <- trimws(paste0(subtitle, ", " , spatial.string))
  if(length(temporal.string) > 0 )  subtitle <- trimws(paste0(subtitle, ", ", temporal.string))
  
  
  
  
  #######  MAKE THE MAIN TITLE
  
  # if a common subannual resolution check to see if there is a single value
  if(length(sta.info@subannual.resolution) > 0)  {

    subannual.period <- character(0)
    
    if(sta.info@subannual.resolution == "Monthly") {
      all.months.present <- c()
      for(field in fields) all.months.present <- append(all.months.present, getDimInfo(field, "values")[["Month"]])
      all.months.present <- unique(all.months.present)
      if(length(all.months.present) ==1 ) subannual.period <- all.months.present[1]
    }

    else if(sta.info@subannual.resolution == "Seasonal") {
      all.seasons.present <- c()
      for(field in fields) all.seasons.present <- append(all.seasons.present, getDimInfo(field, "values")[["Season"]])
      all.seasons.present <- unique(all.seasons.present)
      if(length(all.seasons.present) ==1 ) subannual.period <- all.seasons.present[1]
    }
    
    else if(sta.info@subannual.resolution == "Daily") {
      all.days.present <- c()
      for(field in fields) all.days.present <- append(all.days.present, getDimInfo(field, "values")[["Day"]])
      all.days.present <- unique(all.days.present)
      if(length(all.days.present) ==1 ) subannual.period <- all.days.present[1]
    }
 
  }
  
  sources.vec <- c()
  quants.vec <- c()
  layers.vec <- c()
  for(field in fields) {
    layers.vec <- append(layers.vec, names(field))
    sources.vec <- append(sources.vec, field@source@name )
    quants.vec <- append(quants.vec, field@quant@name )
  }
  
  sources.vec <- unique(sources.vec)
  quants.vec <- unique(quants.vec)
  layers.vec <- unique(layers.vec)
  
  
  # put them al together
  title.string <- character(0)
   if(length(subannual.period) == 1) title.string <- paste(title.string, subannual.period)
  if(length(layers.vec) == 1) title.string <- paste(title.string, layers.vec)
  if(length(quants.vec) == 1) title.string <- paste(title.string, quants.vec)
  if(length(sources.vec) == 1) title.string <- paste(title.string, sources.vec)
  
  

  
  return(list(title = trimws(title.string),
              subtitle = trimws(subtitle)))
  
  
 
  
}
