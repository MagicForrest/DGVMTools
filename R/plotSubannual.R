
#' Plots sub-annual cycles
#' 
#' For a list of Fields, plots the sub-annual cycles of the selected layers on on top of each other.
#' 
#' @param fields The data to plot. Can be a Field or a list of Fields.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param gridcells A list of gridcells to be plotted (either in different panels or the same panel). For formatting of this argument see \code{selectGridcells}.  
#' Leave empty or NULL to plot all gridcells (but note that if this involves too many gridcells the code will stop) 
#' @param title A character string to override the default title.
#' @param subtitle A character string to override the default subtitle.
#' @param col.by Character string defining the aspects of the data which which should be used to set the colour of the lines in each panel,
#'  all other aspects will be put in separate panels.  Can meaningfully take the values "Year", Layer", "Source", "Site" or "Quantity".
#'  By default \code{col.by} is set to "Year" which means that the years are plotted according to a colour gradient, and all other aspects of the 
#'  the data are distinguished by different facet panels.  
#' @param cols A vector of colours to control the colours of the lines (as defined by the \code{col.by} argument above).  If \code{col.by} is set to "Year",
#' the colours are used to form a colour palette.   Otherwise the vector can/should be named to match particular colour values
#' to particular Layers/Sources/Sites/Quantities.    
#' @param col.labels A vector of character strings which are used as the labels for the lines. As for the \code{cols} arguement it must have
#' the same length as the number of Layers/Sources/Sites/Quantities in the plot and the vectors can/should be named to match particular 
#' labels to  particular Layers/Sources/Sites/Quantities.    
#' @param plotAverage Boolean, if TRUE plot the mean of all years
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
#' @param alpha A numeric (range 0-1), to give the transparency (alpha) of the annual lines
#' @param plot Boolean, if FALSE return a data.table with the final data instead of the ggplot object.  This can be useful for inspecting the structure of the facetting columns, amongst other things.
#' @param ... Arguments passed to \code{ggplot2::facet_wrap()}.  See the ggplot2 documentation for full details but the following are particularly useful.
#' \itemize{
#'  \item{"nrow"}{The number of rows of facets}
#'  \item{"ncol"}{The number of columns of facets}
#'  \item{"scales"}{Whether the scales (ie. x and y ranges) should be fixed for all facets.  Options are "fixed" (same scales on all facets, default)
#'  "free" (all facets can their x and y ranges), "free_x" and "free_y"  (only x and y ranges can vary, respectively).}
#'  \item{"labeller"}{A function to define the labels for the facets.  This is a little tricky, please look to the ggplot2 documentation} 
#' }
#' 
#' @details 
#' 
#' Note that like all \code{DGVMTools} plotting functions, \code{plotSubannual} splits the data into separate panels using the \code{ggplot2::facet_wrap()}.  If you want to 'grid' the facets
#' using \code{ggplot2::facet_grid()} you can do so afterwards. 'gridding the facets' implies the each column and row of facets vary by one specific aspect.
#' For example you might have one column for each Source, and one row for each "Quantity".
#' 
#' 
#' 
#' @return Returns either a ggplot2 object or a data.table (depending on the 'plot' argument)
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export



plotSubannual <- function(fields, # can be a Field or a list of Fields
                          layers = NULL,
                          gridcells = NULL,
                          title = NULL,
                          subtitle = NULL,
                          cols = NULL,
                          col.by = "Year",
                          col.labels = waiver(),
                          plotAverage = TRUE,
                          text.multiplier = NULL,
                          plot = TRUE,
                          alpha = 0.2,
                          ...) {
  
  
  Quantity = Month = Source = Value = Year = NULL
  
  
  
  ### SANITISE FIELDS, LAYERS AND DIMENSIONS
  
  ## 1. FIELDS - check the input Field objects (and if it is a single Field put it into a one-item list)
  
  fields <- santiseFieldsForPlotting(fields)
  if(is.null(fields)) return(NULL)
  
  ## 2. LAYERS - check the number of layers
  
  layers <- santiseLayersForPlotting(fields, layers)
  if(is.null(layers)) return(NULL)
  
  
  ## 3. DIMENSIONS - check the dimensions (require that all fields the same dimensions) and then that one and only subannual dimension is present )
  
  # check the Fields for consistent dimensions
  dim.names <- santiseDimensionsForPlotting(fields)
  if(is.null(dim.names)) return(NULL)
  
  # get the subannual dimensions and check one and only one
  subannual.dimension <- dim.names[which(dim.names %in% c("Day", "Month", "Season")) ]
  if(length(subannual.dimension) == 0) stop("No subannual dims found for plotSubannual ")
  else if((length(subannual.dimension) > 1) ) stop(paste0("Multiple subannual dimensions found in plotSubannual: ", paste(subannual.dimension)))
  
  
  
  ### PREPARE AND CHECK DATA FOR PLOTTING
  final.fields <- trimFieldsForPlotting(fields, layers, gridcells = gridcells)
  
  ### MERGE DATA FOR PLOTTING INTO ONE BIG DATA.TABLE
  # MF TODO maybe make some clever checks on these switches
  if("Lon" %in% dim.names & "Lat" %in% dim.names) add.Site <- TRUE
  else add.Site <- FALSE
  add.Region <- TRUE
  
  # MF TODO: Consider adding add.TimePeriod?
  data.toplot <- mergeFieldsForPlotting(final.fields, add.Quantity = TRUE, add.Site = add.Site, add.Region = TRUE)
  
  ### XX. FACETTING
  
  # all column names, used a lot below 
  all.columns <- names(data.toplot)
  
  # check the col.by arguments 
  if(!missing(col.by) && !is.null(col.by) && !col.by %in% all.columns) stop(paste("Colouring lines by", col.by, "requested, but that is not available, so failing."))
 
  # ar first assume facetting by everything except for...
  dontFacet <- c("Value", "Time", "Year", "Month", "Season", "Day", "Lon", "Lat", col.by)
  facet.vars <- all.columns[!all.columns %in% dontFacet]
  
  # then remove facets with only one unique value
  for(this.facet in facet.vars) {
    if(length(unique(data.toplot[[this.facet]])) == 1) facet.vars <- facet.vars[!facet.vars == this.facet]
  }
  
  ### MATCH LAYER COLOUR 
  
  # if cols is not specified and plots are to be coloured by Layers, look up line colours from Layer meta-data
  if(missing(cols) & col.by == "Layer"){
    
    # check the defined Layers present in the Fields and make a unique list
    # maybe also here check which one are actually in the layers to plot, since we have that information
    all.layers.defined <- list()
    for(object in fields){
      all.layers.defined <- append(all.layers.defined, object@source@defined.layers)
    }
    all.layers.defined <- unique(all.layers.defined)
    
    
    all.layers <- unique(as.character(data.toplot[["Layer"]]))
    cols <- matchLayerCols(all.layers, all.layers.defined)
  }
  
  #### MAKE LIST OF QUANTITIES AND UNITS FOR Y LABEL
  
  unit.str <- list()
  quant.str <- list()
  id.str <- list()
  for(this.field in final.fields) {
    
    # pull out the unit string, id and full name string
    quant <- this.field@quant
    unit.str <- append(unit.str, quant@units)
    quant.str <- append(quant.str, quant@name)
    id.str <- append(id.str, quant@id)
    
  }
  
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
  

  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(final.fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  # return the data if plot = FALSE
  if(!plot) return(data.toplot)
  
 
  ###### MAKE THE PLOT ######
  
  # basic plot and colour scale
  p <- ggplot(as.data.frame(data.toplot), aes(get(subannual.dimension), Value, colour = get(col.by), group = interaction(Year, get(col.by))), alpha = alpha) + geom_line(alpha = alpha)
  if(!is.null(col.by) & !is.null(cols)) p <- p + scale_color_manual(values=cols, labels=col.labels) 
  
  # if colouring by Year add a continuous scale (special case)
  if(col.by == "Year") {
    p <- p + scale_color_gradientn(colours = viridis::viridis(100), name = "Year")
    if(plotAverage) {
         p <- p + stat_summary(aes(group=col.by, linetype = "mean year"), fun.y=mean, geom="line", size = 1)
         p <- p + scale_linetype_manual(values=c("mean year"="longdash"), name = element_blank())
    }
  }
  # else colour discretely
  else {
    p <- p + labs(colour=col.by)

    if(plotAverage) {
 
        # get the colours to colour the points by
        g <- ggplot_build(p)
        extracted.cols <- unique(g$data[[1]][["colour"]])
       
        # add the mean stat and the fill scale
        p <- p + stat_summary(aes(group=get(col.by), fill = get(col.by)), fun.y=mean, geom="point", color="black", shape = 21)
        p <- p + scale_fill_manual(values =extracted.cols, name = "Mean year", labels = col.labels)
     
    }
  }
  
  
  # set the x-axis
  if(subannual.dimension == "Month") p <- p + scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  
  # set the title
  p <- p + labs(title = title, subtitle = subtitle,  y = paste(quant.str, " (", unit.str, ")", sep = ""), x = subannual.dimension)
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5))
  
  # set legend 
  p <- p + theme(legend.position = "right", legend.key.size = unit(2, 'lines'))
  
  # wrap to split by source
  p <- p + facet_wrap(facet.vars, ...)
  
  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  return(p)
  
}
