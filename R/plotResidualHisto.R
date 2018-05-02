#' Plot the residuals from a Comparison or list of Comparisons as histogram
#' 
#' Function will overlay histograms if more than one Comparison is provided.  
#' Line colours, line types and fill colours can all be specified but have sensible defaults. 
#' 
#' @param input.CLayers The Comparison or Comparisons for which to plot the residuals
#' @param cols,types,fills Vector of line colours, line types and fill colours respectively. Each can be left empty but if defined they must have one entry for each set of residuals you are plotting.
#' @param labels Vector of character for label the histos. Can be left empty (defaults to the run id) but if defined it must have one entry for each set of residuals you are plotting.
#' @param title Character for plot title (optional)
#' @param xlab Character for x-axis label (optional)
#' @param ylab Character for y-axis label (optional)
#' @param alpha Numerical value [0,1] to specify opacity of histo fill colours (optional)
#' @param reverse Logical if TRUE reverse the layering of the histograms (optional, default = FALSE)
#' @param bin.width Numeric, width of bins on histogram (can be left blank) 
#' @param xlim Numeric, if provided should be a vector of two values defining the range on the x-axis 
#' 
#' @details
#' This function should be called after a call to \code{benckmarkSpatial} for a DataObject.  It plots the residuals for each model run to which it was compared.  
#' It is called automatically by \code{benckmarkSpatial}, but can be called again for better flexibility.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2
#' @export
#' @return A ggplot2 plot.

plotResidualsHisto <- function(input.CLayers, 
                               cols = NULL, 
                               fills = NULL, 
                               types = NULL,
                               labels = NULL,
                               title = NULL,
                               xlab = NULL,
                               ylab = NULL,
                               alpha = NULL, 
                               reverse = FALSE, 
                               bin.width = NULL,
                               xlim = NULL) {
  
  Source = Value = NULL
  
  # checks
  # for a single Comparison
  if(is.Comparison(input.CLayers)) {
    
    temp.dt <- stats::na.omit(input.CLayers@data[, c("Difference"), with=FALSE])
    setnames(temp.dt, input.CLayers@name) 
    diff.layers <- input.CLayers@name
    if(is.null(labels)) labels <- input.CLayers@info1@name
    
    # melt the data.table and set new names
    temp.dt <- melt.data.table(temp.dt, measure.vars = names(temp.dt))
    setnames(temp.dt, c("value", "variable"), c("Value", "Source"))
    
  }
  # for list of comparison layers 
  else if(class(input.CLayers)[1] == "list") {
    
    list.of.dts <- list()
    new.labels <- c()
    diff.layers <- c()
    for(thing in input.CLayers){
      
      if(!is.Comparison(thing)) warning("plotResidualsHisto(): One of the items in the list is not a comparison layer, so ingoring it!")
      else {
        
        really.temp.dt <- stats::na.omit(thing@data[, c("Difference"), with=FALSE])
        setnames(really.temp.dt, thing@name) 
        diff.layers <- append(diff.layers, thing@name)
        if(is.null(labels)) new.labels <- append(new.labels, thing@info1@name)
        
        # melt the data.table and set new names
        really.temp.dt <- melt.data.table(really.temp.dt, measure.vars = names(really.temp.dt))
        setnames(really.temp.dt, c("value", "variable"), c("Value", "Source"))
        
        list.of.dts[[thing@id]] <- really.temp.dt
        
      }
    }
    
    temp.dt <- rbindlist(list.of.dts)
    if(is.null(labels)) labels <- new.labels
    
    rm(list.of.dts)
    
    # get the data.table, select the residual layers (all the ones with names ending "_Error") from the incoming DataObject and remove NAs
  }
  else {
    warning("plotResidualsHisto(): Not received either a Comparison oor a list of comparison layers, returning a NULL plot")
    return(NULL)
  }
  
  
  # if there are no labels supplied, strip the "_Error" from the names and use that  
  #if(is.null(labels)) {
  #  labels <- gsub("_Error", "", diff.layers)
  #  names(temp.dt) <- append(names(temp.dt)[1:(length(names(temp.dt))-length(labels))], labels)
  #}
  
  
  # if cols, fills or types no spcified, set to some defaults
  if(is.null(fills)) { fills <- rep("transparent", length(diff.layers)) }
  if(is.null(cols)) { cols <- fields::tim.colors(length(diff.layers)) }
  if(is.null(types)) { types <- rep(1, length(diff.layers)) }
  
  # also set sensible default axis and main titles
  #if(is.null(title)) title <- paste("Residuals vs", input.CLayers@name, input.CLayers@quant@name, sep = " ")
  if(is.null(ylab)) ylab <-"# gridcells"
  #if(is.null(xlab)) xlab<- paste("Residuals vs", input.CLayers@name, input.CLayers@quant@name, paste0("(",input.CLayers@quant@units,")"), sep = " ")
  
  
  
  
  # deal with reverse for plotting histos in opposite order
  if(reverse) {
    temp.dt[,Source := factor(Source, ordered = TRUE)]
    temp.dt[,Source := factor(Source, levels=rev(levels(Source)))]
    if(!is.null(cols)) cols <- rev(cols)
    if(!is.null(fills)) fills <- rev(fills)
    labels <- rev(labels)
  }
  
  # make the plot and set the xlim 
  # also note special handling of alpha becuase if alpha is specified, "transparent" seems to become "white" with the alpha transparency level, this can be annoying
  histo.plot <- ggplot(as.data.frame(temp.dt), aes(x = Value, colour = Source)) 
  
  if(!is.null(alpha)) histo.plot <- histo.plot + geom_histogram(aes(colour = Source, fill = Source, linetype = Source), binwidth = bin.width, position="identity", alpha = alpha)
  else histo.plot <- histo.plot + geom_histogram(aes(colour = Source, fill = Source, linetype = Source), binwidth = bin.width, position="identity")
  if(!is.null(xlim)) histo.plot <- histo.plot + xlim(xlim)
  
  
  
  # cols, fills and types
  histo.plot <- histo.plot + scale_colour_manual(NULL, labels = labels, values = cols)
  histo.plot <- histo.plot + scale_fill_manual(NULL, labels = labels, values = fills)
  histo.plot <- histo.plot + scale_linetype_manual(NULL, labels = labels, values = types)
  histo.plot <- histo.plot + guides(colour = guide_legend(keywidth = 5, keyheight = 3, reverse=reverse), 
                                    fill = guide_legend(keywidth = 5, keyheight = 3, reverse=reverse),
                                    linetype = guide_legend(keywidth = 5, keyheight = 3, reverse=reverse))
  
  
  # Standard titles, labels etc...
  histo.plot <- histo.plot + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))
  histo.plot <- histo.plot + xlab(xlab)   
  histo.plot <- histo.plot + ylab(ylab)   
  histo.plot <- histo.plot + theme(text = element_text(size=30), legend.position=c(0.2,0.85))
  histo.plot <- histo.plot + geom_vline(xintercept = 0, size = 1, colour = "black")
  
  
  # consider implementing automatic faceting
  #histo.plot <- histo.plot + facet_grid( Run ~ ., labeller = as_labeller(labeller))
  
  return(histo.plot)
  
}

