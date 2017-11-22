
#' Plots sub-annual cycles
#' 
#' For a list of Runs and a list of Quantities, plots the sub-annual cycles.  Note this function actually reads the data, 
#' so might not be too efficient unless the arguments which are passes through to getModelObject() via the "..." argument are optimised.  
#' For example, by using "store.full = TRUE" and the combination "write = TRUE" and "read.full = FALSE".
#' 
#' @param runs A list of ModelRun objects to plot
#' @param quants A list of Quantity objects to plot
#' @param title A character string to override the default title.
#' @param spatial.extent For which spatial extent to plot the seasonal cycle.  For details of how to make this selection see the documnetation for getModelObject().
#' @param spatial.aggregate.method Charatacer string specifgin how to spatially aggregate if more than one gridcell are included in the spatial.extent.  Again, see documentation getModelObject for  details.
#' The default is "mean", simply straight averaging.
#' @param plotAverage Boolean, if TRUE plot the mean of all years
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param ... Arguments passed to getModelObject().  Of particular relevance are \code{spatial.extent} and \code{spatial.aggregate.method} (to determine over 
#' which spatial extent to plot the seasonal cycle and, if that extent includes more that one gridcell, how to aggregate across that extent)
#' 
#' @return Returns either a ggplot2 object or a data.table (depending on the 'plot' argument)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de} 



plotSeasonal <- function(runs, 
                         quants, 
                         title = NULL,
                         spatial.extent = NULL, 
                         spatial.aggregate.method = "mean", 
                         plotAverage = TRUE,
                         text.multiplier = NULL,
                         plot = TRUE,
                         ...) {
  
  
  Quantity = Type = Month = Source = Value = Year = NULL
  
  ###### PREAMBLE ######
  
  ### Sort out quantities    
  
  quants.temp <- list()
  unit.str <- list()
  quant.str <- list()
  id.str <- list()
  for(quant in quants) {
    
    # at this stage rebuild the list so it has proper Quantity objects rather than shorthand strings
    if(class(quant)[1] == "character") {
      quant <- lookupQuantity(quant, run@model)  
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
    warning("Quants to be plotted have non-identical units in plotSeasonal(). Using the first only for the axis, label.")
  }   
  
  
  # check the strings
  if(length(unique(quant.str)) == 1){ 
    quant.str <- unique(quant.str) 
  }  
  else {
    quant.str <- paste(id.str, sep = ", ", collapse = ", ")
  }   
  
  print(quant.str)
  
  
  
  
  
  ###### PREPARE THE DATA FOR PLOTTING ######
  
  ## for all runs
  run.dts <- list()
  for(run in runs){
    
    ## for all quants
    quants.dts <- list()
    for(quant in quants) {
      
      # open the and pull out the data that we want
      this.ModelObject <- getModelObject(run, 
                                         quant@id, 
                                         spatial.extent = spatial.extent,
                                         spatial.aggregate.method = spatial.aggregate.method,
                                         ...)
      this.dt <-this.ModelObject@data
      this.dt <- melt(this.dt, id.vars = c("Lon", "Lat", "Year"), variable.name = "Month", value.name = "Value")
      this.dt <- this.dt[, Quantity := quant@name]
      this.dt <- this.dt[, Type := "Single Year"]
      setKeyDGVM(this.dt)
      
      quants.dts[[length(quants.dts)+1]] <- this.dt
      
      # also maybe plot average
      if(plotAverage) {
        average.dt <- this.dt[,lapply(.SD, mean), by=Month, .SDcols = c("Value")]
        average.dt[, Type := "Average"]
        average.dt[, Quantity := quant@name]
        quants.dts[[length(quants.dts)+1]] <- average.dt
        rm(average.dt)
      }
      
      rm(this.dt)
      
    } # for each Quantity
    
    
    # combine data.tables for all different Quantities
    this.run.dt <- rbindlist(quants.dts, fill = TRUE)
    rm(quants.dts)
    
    # set the Source and save for later
    this.run.dt[, Source := run@name]
    run.dts[[run@id]] <- this.run.dt
    rm(this.run.dt)
    
    
  }  # for each run
  
  
  ## combine all runs
  all.dt <- rbindlist(run.dts, fill = FALSE)
  rm(run.dts)
  
  ## swap Jan,Feb,Mar,... for simple 1,2,3...
  month.list <- c()
  for(month in all.months) {
    month.list <- append(month.list, month@id)
  }
  get.pos <- function(x, v){
    return(which(x == v))
  }
  all.dt[, Month := unlist(lapply(all.dt[["Month"]], FUN = get.pos, month.list))]
  setKeyDGVM(all.dt)
  
  
  
  
  
  ###### MAKE THE PLOT ######
  
  # basic plot
  p <- ggplot(as.data.frame(all.dt), aes(Month, Value, colour = Quantity, group = interaction(Year, Quantity)), alpha = 0.1) + geom_line(alpha = 0.1)
  
  # add average line if chosen
  if(plotAverage) p <- p + stat_summary(aes(group=Quantity), fun.y=mean, geom="line", size = 1)
  
  # set the x-axis
  p <- p + scale_x_continuous(breaks = 1:12,labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  
  # set the title
  p <- p + labs(title = title, y = paste(quant.str, " (", unit.str, ")", sep = ""), x = "Month")
  p <- p + theme(plot.title = element_text(hjust = 0.5))
  
  # set legend 
  p <- p + theme(legend.title=element_blank())
  p <- p + theme(legend.position = "right", legend.key.size = unit(2, 'lines'))
  
  # wrap to split by source
  p <- p + facet_wrap(~Source, ncol = 1, scales = "free")
  
  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  
  
  return(p)
  
  
}
