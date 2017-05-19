#' Plot the residuals from a ComparisonLayer or list of ComparisonLayers as histogram
#' 
#' Function will overlay histograms if more than one ComaprisonLayer is provided.  
#' Line colours, line types and fill colours can all be specified but have sensible defaults. 
#' 
#' @param input.CLayers The ComparisonLayer or ComparisonLayers for which to plot the residuals
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
  # for a single ComparisonLayer
  if(is.ComparisonLayer(input.CLayers)) {
    
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
      
      if(!is.ComparisonLayer(thing)) warning("plotResidualsHisto(): One of the items in the list is not a comparison layer, so ingoring it!")
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
    warning("plotResidualsHisto(): Not received either a ComparisonLayer oor a list of comparison layers, returning a NULL plot")
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
  histo.plot <- histo.plot + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  histo.plot <- histo.plot + xlab(xlab)   
  histo.plot <- histo.plot + ylab(ylab)   
  histo.plot <- histo.plot + theme(text = element_text(size=30), legend.position=c(0.2,0.85))
  histo.plot <- histo.plot + geom_vline(xintercept = 0, size = 1, colour = "black")
  
  
  # consider implementing automatic faceting
  #histo.plot <- histo.plot + facet_grid( Run ~ ., labeller = as_labeller(labeller))
  
  return(histo.plot)
  
}





#' Plot scatter of model vs data
#' 
#' Function will makes multiple scatter plots on one page if more than one ModelRun has been compared to the the DataObject.  
#' 
#' @param input.CLayers The DataObject for which to plot the residual
#' @param run.ids The character vector of run ids of the runs to scatter against the data (must be a vector, not a list).  Leave blank to compare all runs that have been previouslty compared to this dataset.
#' @param run.labels A vector of more descriptive strings from each run (each the run@names)
#' @param wrap An optional string identifying an additional column by which to subdivide (facet) that data
#' @param facet.labels An option vector of strings giving descriptive labels for facets, if provided it *must* be a named vector, 
#' where each element has a name corresponding to the value in the facet column. 
#' @param xlim An optional vector of two numerics to define the x plot range
#' @param ylim An optional vector of two numerics to define the y plot range
#' @param xlab An optional character string for the x-axis
#' @param ylab An optional character string for the y-axis
#' @param showFitLine Boolean, if TRUE shows a linear fit and the fit equation
#' @param showStats Boolean, if TRUE show some stats do quantify how well the model fits the data
#' @param labels Character vector of labels (one for each run).  If not provided uses the run ids
#' @param alpha Numeric for the transparency of tre points (range (0,1], default = 0.05))
#' @param text.size Numeric to scale the text size on the plot (default = 6)
#' 
#' 
#' @details
#' This function should be called after a call to \code{benckmarkSpatial} for a DataObject.  It plots the scatters for each model run to which it was compared .  
#' It is called automatically by \code{benckmarkSpatial}, but can be called again for better flexibility.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2
#' @export
#' @return A ggplot2 plot.


plotScatterComparison <- function(input.CLayers, 
                                   run.ids = NULL,
                                   run.labels = NULL,
                                   wrap = TRUE,
                                   facet.labels = NULL,
                                   xlim = NULL,
                                   ylim = NULL,
                                   xlab = NULL,
                                   ylab = NULL,
                                   showFitLine = TRUE,
                                   showStats = TRUE,
                                   alpha = 0.05,
                                   text.size = 6){
  
  Run = Model = OtherFacet = facet = NULL
  
  # checks
  # for a single ComparisonLayer
  if(is.ComparisonLayer(input.CLayers)) {
    
    temp.dt <- stats::na.omit(input.CLayers@data[, names(input.CLayers)[1:2], with=FALSE])
    setnames(temp.dt, c("ValueX", "ValueY")) 
    if(is.null(labels)) labels <- input.CLayers@info1@name

    wrap <- FALSE
    
  }
  # for list of comparison layers 
  else if(class(input.CLayers)[1] == "list") {
    
    list.of.dts <- list()
    new.labels <- c()
    for(thing in input.CLayers){
      
      if(!is.ComparisonLayer(thing)) warning("plotResidualsHisto(): One of the items in the list is not a comparison layer, so ingoring it!")
      else {
        

        really.temp.dt <- stats::na.omit(thing@data[, names(thing)[1:2], with=FALSE])
        setnames(really.temp.dt, c("ValueX", "ValueY")) 
        
        # also add the source of the comparison
        really.temp.dt <- really.temp.dt[, "Source" := paste(thing@info1@name, "vs", thing@info2@name, sep = " ")]
        if(is.null(labels)) new.labels <- append(new.labels, thing@info1@name)
        list.of.dts[[thing@id]] <- really.temp.dt
        
      }
 
    }
    
    temp.dt <- rbindlist(list.of.dts)
    if(is.null(labels)) labels <- new.labels
    
    rm(list.of.dts)
  }
  
  
 
  
  # consider facetting
  facet.run <- FALSE
  facet.other <- FALSE
  title.substring <- "Simulation"
  # if there is more than one run present then facet by run (that means thet there must more than two columns at this stage)
  
  
  
  # if facet is not NULL, ignore the different runs and instead facet by the specified column
  # if(!is.null(facet))  {
  #   
  #   facet.other <- TRUE
  #   
  #   # Make labeller
  #   if(is.null(facet.labels)) {
  #     unique.facets <- sort(unique(input.CLayers@data[[facet]]))
  #     facet.labels <- unique.facets
  #     names(facet.labels) <- unique.facets
  #   }
  #   
  #   # append the facet layer as we will need it for plotting
  #   #layers.for.plotting <- append(layers.for.plotting, facet)
  #   # don't show the overall stats on each panel, that is silly
  #   warning("In plotScatterComparison, not displaying statisics despite showStats = TRUE because the plot is to be facetted, and I don't have the stats for each facet.")
  #   showStats = FALSE
  #   
  # }
  

 
  
  ### Get the xlim and ylim (for placing statistics labels) if they are not specified
  if(is.null(ylim) | is.null(xlim)) {
    # get min/max of y values
    all.y <- temp.dt[["ValueY"]]
    all.x <-  temp.dt[["ValueX"]]
    
    if(is.null(ylim) & is.null(xlim)) {
      ylim <- c(min(append(all.x, all.y)), max(append(all.x, all.y)))
      xlim <- ylim
    }
    else if (is.null(xlim)) {
      xlim <- c(min(all.x), max(all.x, all.y))
    }
    else if (is.null(ylim)) {
      ylim <- c(min(all.y), max(all.x, all.y))
    }
    
    
  }
  
  # The basic plot
  scatter.plot <- ggplot(as.data.frame(stats::na.omit(temp.dt)), aes_string(x="ValueX", y="ValueY")) +  geom_point(size=3, alpha =alpha)
  
  
  scatter.plot <- scatter.plot + theme(text = element_text(size=25))
  if(is.null(wrap)) scatter.plot <- scatter.plot + ggtitle(paste(input.CLayers@info1@name, "vs.", input.CLayers@info2@name)) + theme(plot.title = element_text(lineheight=.8, face="bold", hjust = 0.5))

  # x and y labels
  if(is.null(wrap)) {
    scatter.plot <- scatter.plot +  xlab(input.CLayers@info1@name) + ylab(input.CLayers@info2@name)  
  }
  else{
    if(!is.null(xlab)) scatter.plot <- scatter.plot +  xlab(xlab)
    if(!is.null(ylab)) scatter.plot <- scatter.plot +  ylab(ylab)
  }
  
  #else  scatter.plot <- scatter.plot +  xlab(input.CLayers@info1@name) + ylab(input.CLayers@info2@name)     
  
  
  # crop to xlim and ylim as appropriate and fix the aspect ratio 
  scatter.plot <- scatter.plot + scale_x_continuous(limits = xlim, expand = c(0, 0))
  scatter.plot <- scatter.plot + scale_y_continuous(limits = ylim, expand = c(0, 0))
  scatter.plot <- scatter.plot + coord_fixed()

  # Plot one-to-one line
  scatter.plot <- scatter.plot + geom_abline(intercept = 0, slope = 1, size= 1, colour = "red3")
  
  # X and 
  
  
  if(wrap) scatter.plot <- scatter.plot + facet_wrap(~ Source)
  
  return(scatter.plot)
  

  
  
  if( facet.run && !facet.other) { scatter.plot <- scatter.plot + facet_wrap(~ Run, labeller = as_labeller(run.labels)) }
  if(!facet.run &&  facet.other) { scatter.plot <- scatter.plot + facet_wrap(~ OtherFacet, labeller = as_labeller(facet.labels))}
  if(facet.other && facet.run)   { scatter.plot <- scatter.plot + facet_grid(OtherFacet ~ Run, labeller = labeller(.rows = as_labeller(facet.labels), .cols = as_labeller(run.labels))) }
  
  
  
  # show the fit line and equation
  if(showFitLine) {
    
    scatter.plot <- scatter.plot + geom_smooth(method = "lm", se = FALSE, size = 1, colour = "blue3")
    
    # now make the equations, and same them in a data.frame formatted for the geom_text call
    text.vector <- c()
    
    runs.vector <- c()
    other.vector <- c()
    
    for(run.id in run.ids){
      
      
      if(!facet.other) {
        temp.temp.dt <- temp.dt
        setkey(temp.temp.dt,Run)
        temp.temp.dt <- temp.temp.dt[paste(run.id)]
        temp.df <- as.data.frame(temp.temp.dt[, "Run" := NULL, with = FALSE])
        names(temp.df) <- c("x", "y")
        text.vector <- append(text.vector, lm_eqn(stats::lm(y ~ x, temp.df)))
        rm(temp.df, temp.temp.dt)
        runs.vector <- append(runs.vector, run.id)
      }
      
      else {
        
        for(sub.facet in sort(unique(input.CLayers@data[[facet]]))) {
          
          temp.temp.dt <- temp.dt
          setkey(temp.temp.dt,Run)
          temp.temp.dt <- temp.temp.dt[OtherFacet == sub.facet & Run == run.id]
          temp.df <- as.data.frame(temp.temp.dt[, c("OtherFacet","Run") := NULL, with = FALSE])
          names(temp.df) <- c("x", "y")
          text.vector <- append(text.vector, lm_eqn(stats::lm(y ~ x, temp.df)))
          rm(temp.df, temp.temp.dt)
          runs.vector <- append(runs.vector, run.id)
          other.vector <- append(other.vector, sub.facet)
          
        }
        
        
      }
      
    }
    
    if(!facet.other) text.df <- data.frame(Run = runs.vector, label = text.vector)
    else  text.df <- data.frame(Run = runs.vector, OtherFacet = other.vector, label = text.vector)
    
    scatter.plot <- scatter.plot + geom_text(data=text.df, 
                                             aes(x = (xlim[2] - xlim[1]) * 0.10, 
                                                 y = (ylim[2] - ylim[1]) * 0.95, 
                                                 label=label), 
                                             parse = TRUE, 
                                             inherit.aes=FALSE, 
                                             size = text.size, 
                                             colour = "blue3",
                                             hjust = 0)
    
  }
  
  # show the stats
  if(showStats) {
    
    
    # now make the equations, and same them in a data.frame formatted for the geom_text call
    text.vector <- c()
    
    for(run.id in run.ids){
      
      comparison.obj <- byIDfromList(paste(run.id, input.CLayers@id, sep = "."), input.CLayers@comparisons)
      
      text.vector  <- append(text.vector,
                             paste("NME = ", signif(comparison.obj@NME, 3), 
                                   #"\nNMSE = ", signif(comparison.obj@NMSE, 3), 
                                   "\nRMSE = ", signif(comparison.obj@RMSE, 3), 
                                   #"\nR^2 = ", signif(comparison.obj@R2, 3), 
                                   #"\nR^2_eff = ", signif(comparison.obj@R2.eff, 3), 
                                   "\nPearson Corr. = ", signif(comparison.obj@P.cor, 3), sep = "")
                             
      )
      
    }
    
    text.df <- data.frame(Run = run.ids, label = text.vector)
    scatter.plot <- scatter.plot + geom_text(data=text.df, 
                                             aes(x = (xlim[2] - xlim[1]) * 0.6, 
                                                 y = (ylim[2] - ylim[1]) * 0.2, 
                                                 label=label), 
                                             parse = FALSE, 
                                             inherit.aes=FALSE, 
                                             size = text.size, 
                                             colour = "red3",
                                             hjust = 0)
    
  }
  
  return(scatter.plot)
  
}
