#!/usr/bin/Rscript


#################################################################################################################################################
################################################## PLOT COMPARISON MAPS #########################################################################
#################################################################################################################################################


#' Plot a comparison between two spatial layers
#' 
#' This function is for plotting maps from Comparison objects (or a list of those Comparisons).  Three types of comparisons plots are supported: 'difference' - 
#' a difference map; "values" - the absolute values plotted in panels and "percentage.difference" - the percentage differences.  
#' 
#' @param comparisons The data to plot, must be a Comparison or a list of Comparisons
#' @param type A character specifying what type of plot to make. Can be "points" (default, for geom_points), "hex" (for hex binning), "bin2d" (for square binning).
#' There might be more useful options to add later. 


#' @details  A wrapper for around \link{plotSpatial} to plot the spatial Comparisons as maps.  Extra arguments to \link{plotSpatial} can also be specified. 
#' 
#' @return Returns a ggplot object
#'  
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ggplot2 data.table
#' 
#' @export 
#' @seealso \link{plotSpatial},  \link{compareLayers}

plotXYComparison <- function(comparisons,
                             type = c("points", "hex", "bin2d"),
                             fit_line_col = NULL, 
                             perfect_line_col = NULL,
                             matchLimits = TRUE,
                             metrics = c(),
                             metric_text_size = waiver(),
                             text.multiplier = NULL,
                             
                             ...){
  
  Source = Value = Lat = Lon = Layer = long = lat = group = NULL
  Day = Month = Year = Season = NULL
  
  # sort type argument
  type <- match.arg(type)
  
  
  
  ### CHECK TO SEE EXACTLY WHAT WE SHOULD PLOT
  
  ### 1. COMPARISONS - check the input Comparison objects (and if it is a single Comparison put it into a one-item list)
  
  comparisons <- santiseComparisonsForPlotting(comparisons)
  if(is.null(comparisons)) return(NULL)
  
  
  ### 2. DIMENSIONS - check the dimensions (require that all fields the same dimensions)
  
  dim.names <- santiseDimensionsForPlotting(comparisons)
  # need at least one dimension
  if(is.null(dim.names)) return(NULL)
  
  
  ### 3. CONVERT - the Comparisons into a big data.table for plotting 
  
  plotting_dt <- data.table()
  fit_lines_dt <- data.table()
  metrics_dt <- data.table()
  for(object in comparisons){
    
    pretty_comparison_name <- gsub(pattern = " - ", replacement = " vs ", x = object@name)
    
    tmp_dt <- copy(object@data)
    all_names <- names(tmp_dt)
    old_names <- all_names[(length(all_names)-2):length(all_names)]
    setnames(tmp_dt, old = old_names, new = c("Y", "X", "Difference"))
    tmp_dt[ , Difference := NULL]
    tmp_dt[ , Comparison := pretty_comparison_name]
    plotting_dt <- rbind(plotting_dt, tmp_dt)
    fit_lines_dt <- rbind(data.table(slope = object@stats$m, intercept = object@stats$c, Comparison = pretty_comparison_name),
                          fit_lines_dt)
    if(length(metrics) > 0 ){
      tmp_metrics_list <- list()
      for(metric in metrics){
        metric_text <- metric
        if(metric == "m") metric_text <- "Slope"
        if(metric == "c") metric_text <- "Intercept"
        if(metric == "R2") metric_text <- "R^2"
        if(metric == "r2") metric_text <- "r^2"
        tmp_metrics_list[[metric_text]] <- paste0(metric_text, "==", signif(object@stats[[metric]], 2))
      }
      tmp_metrics_string <- paste(tmp_metrics_list, collapse = " ")
      tmp_dt <- data.table(Comparison = pretty_comparison_name, label = gsub(" ", "~", tmp_metrics_string))
      metrics_dt <- rbind(metrics_dt, tmp_dt)
    }
    
    
    
    # # first make a list of the layers that we expect to be present in the data.table, based on the meta-data in the Comparison object
    # layers.names <- names(object)
    # expected.layers.1 <- paste(object@layers1, makeFieldID(source = object@source1, quant.string = object@quant1@id, sta.info = object@sta.info1), sep = ".")
    # expected.layers.2 <- paste(object@layers2, makeFieldID(source = object@source2, quant.string = object@quant2@id, sta.info = object@sta.info2), sep = ".")
    # 
    # # check the layers
    # for(this.layer in expected.layers.1) if(!this.layer %in% layers.names) stop(paste("Layer", this.layer, "expected in Comparison object but not found"))
    # for(this.layer in expected.layers.2) if(!this.layer %in% layers.names) stop(paste("Layer", this.layer, "expected in Comparison object but not found"))
    # 
    # # adjust the source ids if they are identical 
    # if(object@source1@id == object@source2@id) {
    #   
    #   # include the first and last years if they are not the same
    #   if((object@sta.info1@first.year != object@sta.info2@first.year) && (object@sta.info1@last.year != object@sta.info2@last.year)){
    #     object@source1@name <- paste0(object@source1@name, " (", object@sta.info1@first.year, "-", object@sta.info1@last.year, ")")
    #     object@source2@name <- paste0(object@source2@name, " (", object@sta.info2@first.year, "-", object@sta.info2@last.year, ")")
    #   }
    #   
    # }
    # 
    # 
    # 
    # # SECOND INFO - putting this first because this is the 'base' dataset ("one minus two" convention)
    # new.dt <- object@data[, append(getDimInfo(object), expected.layers.2), with=FALSE]
    # #setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant2@id )
    # setnames(new.dt, expected.layers.2, object@layers2)
    # layers.to.plot <- append(layers.to.plot, object@layers2)
    # objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
    #                                                     id = object@id,
    #                                                     data = new.dt,
    #                                                     quant = object@quant2,
    #                                                     source = object@source2,
    #                                                     object@sta.info2)
    # 
    # # FIRST INFO
    # new.dt <- object@data[, append(getDimInfo(object), expected.layers.1), with=FALSE]
    # #setnames(new.dt, names(new.dt)[length(names(new.dt))], object@quant1@id )
    # setnames(new.dt, expected.layers.1, object@layers1 )
    # layers.to.plot <- append(layers.to.plot, object@layers1)
    # objects.to.plot[[length(objects.to.plot)+1]] <- new("Field",
    #                                                     id = object@id,
    #                                                     data = new.dt,
    #                                                     quant = object@quant1,
    #                                                     source = object@source1,
    #                                                     object@sta.info1)
    # 
  }
  
  # make a legend title if one has not been supplied
  #if(missing(legend.title)) legend.title <- stringToExpression(standardiseUnitString(object@quant1@units))
  
  # if plotting a single comparison object
  #   - 
  
  # default labels
  y_label <- "Change me with '+ ylab()'"
  x_label <- "Change me with '+ xlab()'"
  subtitle <- waiver()
  titles <- makePlotTitle(comparisons)
  subtitle <- titles[["subtitle"]]
  title <-titles[["title"]]
  if(length(comparisons) == 1) {
    x_label <- stringToExpression(paste0(comparisons[[1]]@source1@name, " ", comparisons[[1]]@quant1@name, " (", standardiseUnitString(comparisons[[1]]@quant1@units), ")"))
    y_label <- stringToExpression(paste0(comparisons[[1]]@source2@name, " ", comparisons[[1]]@quant2@name, " (", standardiseUnitString(comparisons[[1]]@quant2@units), ")"))
  }
  
  
  
  #### MAKE THE PLOT INCLUDING SELECTIONG THE CORRECT GEOM_ ####
  xy_plot <- ggplot(plotting_dt, aes(x = X, y = Y)) 
  if(type == "hex"){
    xy_plot <- xy_plot + geom_hex() + viridis::scale_fill_viridis(option = "F", direction = -1, trans = "log10")
  }
  else if(type == "points"){
    xy_plot <- xy_plot + geom_point()
  }
  else if(type == "bin2d"){
    xy_plot <- xy_plot + geom_bin2d() + viridis::scale_fill_viridis(option = "F", direction = -1, trans = "log10")
  }
  
  
  #### FACET IF NECESSARY ####
  if(length(comparisons) > 1) {
    xy_plot <- xy_plot + facet_wrap(facets = vars(Comparison))
  }
  
  
  #### HANDLE LIMITS ####
  mylims <- range(with(plotting_dt, c(X, Y)))
  if(matchLimits) {
    xy_plot <- xy_plot + coord_fixed(xlim = mylims, ylim = mylims)
  } 
  
  
  #### ADD LINES ####
  if(!missing(perfect_line_col) & !is.null(fit_line_col))  xy_plot <- xy_plot + geom_abline(slope=1, intercept = 0, col = perfect_line_col, linetype = "dashed")
  if(!missing(fit_line_col) & !is.null(fit_line_col))   xy_plot <- xy_plot + geom_abline(data = fit_lines_dt, aes(slope=slope, intercept = intercept), col = fit_line_col)
  
  
  #### ADD METRICS ####
  if(length(metrics) > 0){
    metrics_dt[ , x := mylims[1]]
    metrics_dt[ , y := mylims[2]]
    if(!is.null(text.multiplier)) metric_size <- theme_get()$text$size * text.multiplier
    else metric_size <- theme_get()$text$size
    xy_plot <- xy_plot + geom_text(data = metrics_dt,  
                                   mapping = aes(x = x, y = y, label = label), 
                                   size = metric_size, 
                                   size.unit = "pt",
                                   vjust = 0, 
                                   hjust = 0,
                                   parse = TRUE)
    #, size = settings$map_annotation_text_size)
  }
  
  #### ADD TITLES ####
  xy_plot <- xy_plot + labs(title = title,
                            subtitle = subtitle,
                            y = y_label,
                            x = x_label)
  
  
  #### SET THEME ####
  # set the theme to theme_bw, simplest way to set the background to white
  xy_plot <- xy_plot + theme_bw()
  
  #### TEXT MULTIPLIER ####
  if(!is.null(text.multiplier)) xy_plot <- xy_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  return(xy_plot)
  
  
  
  
  
  
  
}