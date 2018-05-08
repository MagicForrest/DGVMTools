
###################################################################################################
################### HELPER FUNCTIONS FOR MAKING PLOT TITLES #######################################
###################################################################################################

#' Make a plot titles
#' 
#' Build an appropriate plot title (and subtitle) from the meta-data from the fields (whihc will be plotted)
#' 
#' @param fields A list of fields which will be plotted and the so the plot title should describe
#' @return A list of two character strings.  The first (called "title") is the main title.  The second (called "subtitle" is the subtitle) 
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#'
#' @export 
makePlotTitle <- function(fields){
  
  # if is a single field make it into a list of fields (for looping)
  if(is.Field(fields)) fields <- list(fields)
  
  
  ####### MAKE A SUBTITLE WITH THE STAINFO
  
  ##### get common STAInfo
  sta.info <- commonSTAInfo(fields)

  ##### make spatial section
  spatial.string <- character(0)
  if(sta.info@spatial.aggregate.method != "none") spatial.string <- paste(spatial.string, sta.info@spatial.aggregate.method)
  if(length(sta.info@spatial.extent.id) > 0) {
    if(sta.info@spatial.extent.id == "Full")  spatial.string <- paste(spatial.string, "full spatial extent")
    else spatial.string <- paste(spatial.string, sta.info@spatial.extent.id)
  }
  spatial.string <- trimws(spatial.string)
  
  ##### make year section
  year.string <- character(0)
  if(sta.info@year.aggregate.method != "none") year.string <- paste(year.string, sta.info@year.aggregate.method)
  if(length(sta.info@first.year) > 0 && length(sta.info@last.year) > 0) year.string <- paste(year.string, paste(sta.info@first.year, sta.info@last.year, sep = "-"))
  year.string <- trimws(year.string)
  
  ##### make subannual section
  subannual.string <- character(0)
  if(length(sta.info@subannual.resolution) > 0) subannual.string <- trimws(paste(subannual.string, sta.info@subannual.resolution, sep = " "))
  if(sta.info@subannual.aggregate.method != "none") subannual.string <- trimws(paste(subannual.string, sta.info@subannual.aggregate.method, "of", sep = " "))
  if(length(sta.info@subannual.original) > 0 && sta.info@subannual.aggregate.method != "none")   {
    if(sta.info@subannual.original != sta.info@subannual.resolution){
    subannual.string <- trimws(paste(subannual.string, sta.info@subannual.original, sep = " "))
    }
  }
    
  #### combine 
  subtitle <- character(0)
  if(length(subannual.string) > 0)  {
    if(length(subtitle) > 0) subtitle <- trimws(paste0(subtitle, ", ", subannual.string))
    else subtitle <- subannual.string
  }
  if(length(spatial.string) > 0 )  {
    if(length(subtitle) > 0) subtitle <- trimws(paste0(subtitle, ", ", spatial.string))
    else subtitle <- spatial.string
  }
  if(length(year.string) > 0 )  {
    if(length(subtitle) > 0) subtitle <- trimws(paste0(subtitle, ", ", year.string))
    else subtitle <- year.string
  }
  print(subannual.string)

  
  
  #######  MAKE THE MAIN TITLE
  
  # if a common subannual resolution check to see if there is a single value
  if(length(sta.info@subannual.resolution) > 0)  {

    subannual.period <- character(0)
    
    if(sta.info@subannual.resolution == "Monthly") {
      all.months.present <- c()
      for(field in fields) all.months.present <- append(all.months.present, getDimInfo(field, "values")[["Month"]])
      all.months.present <- unique(all.months.present)
      if(length(all.months.present) == 1) subannual.period <- all.months[[all.months.present]]@id 
    }

    else if(sta.info@subannual.resolution == "Season") {
      all.seasons.present <- c()
      for(field in fields) all.seasons.present <- append(all.seasons.present, getDimInfo(field, "values")[["Season"]])
      all.seasons.present <- unique(all.seasons.present)
      if(length(all.seasons.present) ==1 ) subannual.period <- all.seasons.present[1]
    }
    
    else if(sta.info@subannual.resolution == "Daily") {
      all.days.present <- c()
      for(field in fields) all.days.present <- append(all.days.present, getDimInfo(field, "values")[["Day"]])
      all.days.present <- unique(all.days.present)
      if(length(all.days.present) ==1 ) subannual.period <- paste0("Day ", all.days.present[1])
    }
 
  }
  
  sources.vec <- c()
  quants.vec <- c()
  quants.id.vec <- c()
  layers.vec <- c()
  for(field in fields) {
    layers.vec <- append(layers.vec, names(field))
    sources.vec <- append(sources.vec, field@source@name )
    quants.vec <- append(quants.vec, field@quant@name )
    quants.id.vec <- append(quants.id.vec, field@quant@id)
  }
  
  sources.vec <- unique(sources.vec)
  quants.vec <- unique(quants.vec)
  quants.id.vec <- unique(quants.id.vec)
  layers.vec <- unique(layers.vec)
  
  
  # put them all together - note that sometimes layers are the same as quant@id, we have a special cause to stop them being included in this case
  title.string <- character(0)
  if(length(subannual.period) == 1) title.string <- paste(title.string, subannual.period)
  if(length(layers.vec) == 1) {
    if(quants.id.vec != layers.vec)
    title.string <- paste(title.string, layers.vec)
  }
  if(length(quants.vec) == 1) title.string <- paste(title.string, quants.vec)
  if(length(sources.vec) == 1) title.string <- paste(title.string, sources.vec)
  
  

  
  return(list(title = trimws(title.string),
              subtitle = trimws(subtitle)))
  
  
 
  
}
