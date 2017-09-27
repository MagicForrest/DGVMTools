



plotSeasonal <- function(runs, 
                         quants, 
                         title = NULL,
                         lon,
                         lat,
                         spatial.extent = NULL, 
                         spatial.aggregate.method = "none", 
                         temporal.extent = NULL,
                         plotAverage = TRUE,
                         text.multiplier = NULL) {
  
  
  ###### PREAMBLE ######
  
  # Take the meta-data from the first 
  
  
  
  
  
  
  ###### PREPARE THE DATA FOR PLOTTING ######
  
  ## for all runs
  run.dts <- list()
  for(run in runs){
    
    ## for all quants
    quants.dts <- list()
    for(quant in quants) {
      
      # sort out quantities    
      if(class(quant)[1] == "character") {quant <- lookupQuantity(quant, run@model)  }
      
      # open the site and pull out the site we want
      this.ModelObject <- getModelObject(run, quant@id, verbose = FALSE, temporal.extent = temporal.extent)
      this.dt <-this.ModelObject@data
      this.dt <- this.dt[abs(this.dt[["Lon"]]- lon) < 0.00001 & abs(this.dt[["Lat"]]- lat) < 0.00001,] 
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
  for(month in months) {
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
  p <- p + labs(title = title, y = paste("Dummy Quant", " (", "Dummy Unit", ")", sep = ""), x = "Month")
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
