
#' Plots sub-annual cycles
#' 
#' For a list of Runs and a list of Quantities, plots the sub-annual cycles.  Note this function actually reads the data, 
#' so might not be too efficient unless the arguments which are passes through to getField() via the "..." argument are optimised.  
#' For example, by using "store.full = TRUE" and the combination "write = TRUE" and "read.full = FALSE".
#' 
#' @param runs A Source or a list of Sources to plot
#' @param quants A list of Quantity objects to plot
#' @param title A character string to override the default title.
#' @param subtitle A character string to override the default subtitle.
#' @param spatial.extent For which spatial extent to plot the seasonal cycle.  For details of how to make this selection see the documnetation for getField().
#' @param spatial.aggregate.method Charatacer string specifying how to spatially aggregate if more than one gridcell are included in the spatial.extent.  Again, see documentation getField for  details.
#' The default is "mean", simply straight averaging.
#' @param spatial.extent.id A character string describing the spatial extent over which the data should be aggregated
#' @param plotAverage Boolean, if TRUE plot the mean of all years
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param facet.scales Character string.  If faceting (see above) use "fixed" to specify same scales on each ribbon (default), or "free"/"free_x"/"free_y" for tailored scales
#' @param alpha A numeric (range 0-1), to give the transparency (alpha) of the annual lines
#' @param year.col.gradient A colour palette as a function to use to colour the annual lines according to their Year.  Only works for a single Quantity and a single Source.
#' @param ... Arguments passed to getField().  Of particular relevance are \code{spatial.extent} and \code{spatial.aggregate.method} (to determine over 
#' which spatial extent to plot the seasonal cycle and, if that extent includes more that one gridcell, how to aggregate across that extent)
#' 
#' @return Returns either a ggplot2 object or a data.table (depending on the 'plot' argument)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export



plotSeasonal <- function(runs, 
                         quants, 
                         title = NULL,
                         subtitle = NULL,
                         spatial.extent = NULL, 
                         spatial.aggregate.method = "mean", 
                         spatial.extent.id,  
                         plotAverage = TRUE,
                         text.multiplier = NULL,
                         plot = TRUE,
                         facet.scales = "fixed",
                         year.col.gradient = NULL,
                         alpha = 0.2,
                         ...) {
  
  
  Quantity = Month = Source = Value = Year = NULL
  
  ###### 
  getQuant_local <- function(quant.str, list.of.runs){
    
    for(run in list.of.runs){
      result = tryCatch({ return(lookupQuantity(quant, run@format)) })
    }
    
  }
  
  ##### Year colour gradient
  if(!missing(year.col.gradient) && !is.null(year.col.gradient)) {
    
    if(length(quants) > 1) {
      warning("Since plotSeasonal is already trying to plot multiple quantities, and therefore multiple line colours, the year.col.gradient argument is being ignored ")
      year.col.gradient <- NULL
    }
    
    else if(is.logical(year.col.gradient) && year.col.gradient) year.col.gradient <- viridis::viridis
    
    else if(is.logical(year.col.gradient) && !year.col.gradient) year.col.gradient <- NULL
    
  }
  
  
  ### SOURCES - check the sources
  if(is.Source(runs)) {
    runs <- list(runs)
  }
  else if(class(runs)[1] == "list") {
    for(object in runs){ 
      if(!is.Source(object)) {
        warning("You have passed me a list of items to plot but the items are not exclusively of Source/DataObjects.  Returning NULL")
        return(NULL)
      }
    }
  }
  else{
    stop(paste("plotSpatial can only handle single a Source or DataObject, or a list of Source/DataObjects can't plot an object of type", class(runs)[1], sep = " "))
  }
  
  
  
  ###### PREAMBLE ######
  
  ### Sort out quantities    
  
  quants.temp <- list()
  unit.str <- list()
  quant.str <- list()
  id.str <- list()
  for(quant in quants) {
    
    # at this stage rebuild the list so it has proper Quantity objects rather than shorthand strings
    if(class(quant)[1] == "character") {
      quant <- getQuant_local(quant, runs) 
    }
    quants.temp <- append(quants.temp, quant)
    
    # pull out the unit string, id and long name string
    unit.str <- append(unit.str, quant@units)
    quant.str <- append(quant.str, quant@name)
    id.str <- append(id.str, quant@id)
  }
  
  # update the quants list
  quants <- quants.temp
  
  # check the units
  if(length(unique(unit.str)) == 1){ 
    unit.str <- unique(unit.str) 
  }  
  else {
    unit.str <- paste(unique(unit.str), collapse = ", ")
    warning("Quants to be plotted have non-identical units in plotSeasonal().")
  }   
  
  
  # check the strings
  if(length(unique(quant.str)) == 1){ quant.str <- unique(quant.str) }  
  else{ quant.str <- paste(id.str, sep = ", ", collapse = ", ") }   
  
  
  ###### PREPARE THE DATA FOR PLOTTING ######
  
  ## for all runs
  run.dts <- list()
  all.fields <- list()
  for(run in runs){
    
    ## for all quants
    quants.dts <- list()
    for(quant in quants) {
      
      # open the and pull out the data that we want
      this.Field <- getField(run, 
                             quant@id, 
                             spatial.extent = spatial.extent,
                             spatial.aggregate.method = spatial.aggregate.method,
                             spatial.extent.id = spatial.extent.id,
                             ...)
      all.fields[[paste(run@id, quant@id, "_")]] <- this.Field
      this.dt <-this.Field@data
      setnames(this.dt, quant@id, "Value")
      this.dt <- this.dt[, Quantity := quant@name]
      setKeyDGVM(this.dt)
      
      quants.dts[[length(quants.dts)+1]] <- this.dt
      
      rm(this.dt)
      
    } # for each Quantity
    
    
    # combine data.tables for all different Quantities
    this.run.dt <- rbindlist(quants.dts, fill = TRUE)
    rm(quants.dts)
    
    # set the Source and save for later
    this.run.dt[, Source := run@name]
    run.dts[[paste(run@id, quant@id, sep = "_")]] <- this.run.dt
    rm(this.run.dt)
    
    
  }  # for each run
  
  
  ## combine all runs
  all.dt <- rbindlist(run.dts, fill = FALSE)
  setKeyDGVM(all.dt)
  rm(run.dts)
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(all.fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  # return the data if plot = FALSE
  if(!plot) return(all.dt)
  
  ###### MAKE THE PLOT ######
  
  # basic plot
  if(is.null(year.col.gradient)) {
    p <- ggplot(as.data.frame(all.dt), aes(Month, Value, colour = Quantity, group = interaction(Year, Quantity)), alpha = alpha) + geom_line(alpha = alpha)
    if(plotAverage) {
      p <- p + stat_summary(aes(group=Quantity, color=paste("mean", Quantity)), fun.y=mean, geom="line", size = 1, linetype = "longdash")
    }
  }
  else {
    p <- ggplot(as.data.frame(all.dt), aes(Month, Value, colour = Year, group = interaction(Year, Quantity)), alpha = alpha) + geom_line(alpha = alpha)
    p <- p + scale_color_gradientn(colours = year.col.gradient(100))
    if(plotAverage) {
      p <- p + stat_summary(aes(group=Quantity, linetype = "mean year"), fun.y=mean, geom="line", size = 1)
      p <- p + scale_linetype_manual(values=c("mean year"="longdash"), name = element_blank())
    }
  }
  
  # add average line if chosen
 
  
  
  
  # set the x-axis
  p <- p + scale_x_continuous(breaks = 1:12,labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  
  # set the title
  p <- p + labs(title = title, subtitle = subtitle,  y = paste(quant.str, " (", unit.str, ")", sep = ""), x = "Month")
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5))
  
  # set legend 
  if(is.null(year.col.gradient)) p <- p + theme(legend.title=element_blank())
  p <- p + theme(legend.position = "right", legend.key.size = unit(2, 'lines'))
  
  # wrap to split by source
  p <- p + facet_wrap(~Source, ncol = 1, scales = facet.scales)
  
  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  return(p)
  
}
