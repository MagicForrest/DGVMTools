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
#' @param col.by,size.by,shape.by,alpha.by Character strings defining the aspects of the data which which should be used to set the colour, line type, line width, point size and point shape and alpha (transparency).
#' Can meaningfully take the values "Layer", "Source", "Site" or "Quantity". By default \code{col.by} is set to "Layer" and all others set to NULL, which means the different aspects are 
#' distinguished by different facet panels.  Thus the standard behaviour is that different Layers are distinguished by different colours, but everything is separated into different panels.

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
                             metric_size = waiver(),
                             metric_x_pos = 0.025,
                             metric_y_pos = 0.975,
                             text.multiplier = NULL,
                             col.by = NULL,
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
  comparison_obj_factors <- c()
  for(object in comparisons){
   
    pretty_comparison_name <- gsub(pattern = " - ", replacement = " vs ", x = object@name)
    comparison_obj_factors <- append(comparison_obj_factors, pretty_comparison_name)
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
        
        # for something represing new lines
        if(metric %in% c("", "\n")) {
          tmp_metrics_list[[length(tmp_metrics_list)+1]] <- "\n"
        }
        # for a real metric
        else {
          metric_text <- metric
          if(metric == "m") metric_text <- "Slope"
          if(metric == "c") metric_text <- "Intercept"
          if(metric == "R2") metric_text <- "R^2"
          if(metric == "r2") metric_text <- "r^2"
          tmp_metrics_list[[metric_text]] <- paste0(metric_text, "==", signif(object@stats[[metric]], 2))
        }
      }
      tmp_metrics_string <- paste(tmp_metrics_list, collapse = " ")
      tmp_dt <- data.table(Comparison = pretty_comparison_name, label = gsub(" ", "~", tmp_metrics_string))
      metrics_dt <- rbind(metrics_dt, tmp_dt)
    }
 
  }
  
  
  #### set the facet ordering by using the factor - Gahhh!!  why doesn't this work??
  plotting_dt[ , Comparison := factor(x = Comparison, 
                                      levels = comparison_obj_factors)]
  

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
    
    # first make the "symbols" for the ggplot2 call.  A bit of a pain -since they ggplot2 folks took away aes_string()- but what can you do...
    col.sym <- if(is.character(col.by)) ensym(col.by) else NULL  
    #alpha.sym <- if(is.character(alpha.by))  ensym(alpha.by) else NULL 
    #size.sym <- if(is.character(size.by)) ensym(size.by) else  NULL  
    #shape.sym <- if(is.character(shape.by)) ensym(shape.by) else NULL  
    #linewidth.sym <- if(is.character(linewidth.by)) ensym(linewidth.by) else NULL
    #linetype.sym <- if(is.character(linetype.by)) ensym(linetype.by) else  NULL
    xy_plot <- xy_plot + geom_point(aes(col = !! col.sym))
  }
  else if(type == "bin2d"){
    xy_plot <- xy_plot + geom_bin2d() + viridis::scale_fill_viridis(option = "F", direction = -1, trans = "log10")
  }
  
 
  #### HANDLE LIMITS ####
  mylims <- range(with(plotting_dt, c(X, Y)), na.rm = TRUE)
  if(matchLimits) {
    xy_plot <- xy_plot + coord_fixed(xlim = mylims, ylim = mylims)
  } 
  
  
  #### ADD LINES ####
  if(!missing(perfect_line_col) & !is.null(fit_line_col))  xy_plot <- xy_plot + geom_abline(slope=1, intercept = 0, col = perfect_line_col, linetype = "dashed")
  if(!missing(fit_line_col) & !is.null(fit_line_col))   xy_plot <- xy_plot + geom_abline(data = fit_lines_dt, aes(slope=slope, intercept = intercept), col = fit_line_col)
  
  
  #### ADD METRICS ####
  if(length(metrics) > 0){
    xlims <- range(with(plotting_dt, c(X)), na.rm = TRUE)
    ylims <- range(with(plotting_dt, c(Y)), na.rm = TRUE)
    metrics_dt[ , x := xlims[2] * metric_x_pos]
    metrics_dt[ , y := ylims[2] * metric_y_pos]
    if(!is.null(text.multiplier)) metric_size <- metric_size * text.multiplier
    xy_plot <- xy_plot + geom_text(data = metrics_dt,  
                                   mapping = aes(x = x, y = y, label = label), 
                                   size = metric_size, 
                                   size.unit = "pt",
                                   vjust = 0, 
                                   hjust = 0,
                                   parse = TRUE,
                                   lineheight = 100)
    #, size = settings$map_annotation_text_size)
  }
  
  #### ADD TITLES ####
  xy_plot <- xy_plot + labs(title = title,
                            subtitle = subtitle,
                            y = y_label,
                            x = x_label)
  
  
  #### SET THEME AND OTHER LAYOUT OPTIONS ####
  # set the theme to theme_bw, simplest way to set the background to white
  xy_plot <- xy_plot + theme_bw()
  xy_plot <- xy_plot + theme(plot.title = element_text(hjust = 0.5),
                             plot.subtitle = element_text(hjust = 0.5))
  
  #### TEXT MULTIPLIER ####
  if(!is.null(text.multiplier)) xy_plot <- xy_plot + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  #### DONT EXPAND LIMITS
  xy_plot <- xy_plot + scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))  
  
  
  
  #### FACET IF NECESSARY ####
  if(length(comparisons) > 1) {
    xy_plot <- xy_plot + facet_wrap(~Comparison, ...)
  }
  
  return(xy_plot)
  
  
  
  
  
  
  
}