
#' Plots sub-annual cycles
#' 
#' For a list of Fields, plots the sub-annual cycles of the selected layers.  Automatically splits data into separate panels based on Source, Quantity, 
#' Sire and/or Region, but instead these can be distinguished by aesthetics - see the \code{col.by}, \code{linetype.by} and \code{alpha.by} arguments.
#' Summary or aggregate function can optionally be applied and plotted on top. 
#' 
#' @param fields The data to plot. Can be a Field or a list of Fields.
#' @param layers A list of strings specifying which layers to plot.  Defaults to all layers.  
#' @param gridcells A list of gridcells to be plotted (either in different panels or the same panel). For formatting of this argument see \code{selectGridcells}.  
#' Leave empty or NULL to plot all gridsizrecells (but note that if this involves too many gridcells the code will stop) 
#' @param title A character string to override the default title.
#' @param subtitle A character string to override the default subtitle.
#' @param x.label,y.label Character strings (or expressions) for the x and y axes (optional)
#' @param col.by,linetype.by,alpha.by Character strings defining the aspects of the data which which should be used to set the colour, line type and alpha (transparency).
#' Can meaningfully take the values "Layer", "Source", "Site", "Region" or "Quantity". 
#' NOTE SPECIAL DEFAULT CASE:  By default, \code{col.by} is set to "Year" which means that the years are plotted according to a colour gradient, and all other aspects of the 
#' the data are distinguished by different facet panels.  To change this behaviour and colour the lines according to something different, set the "col.by" argument to one of the
#' strings suggested above.
#' @param cols,linetypes,alphas A vector of colours, line types, or alpha values (respectively) to control the aesthetics of the lines.  
#' Only "cols" makes sense without a corresponding "xxx.by" argument (see above).  The vectors can/should be named to match particular col/linetype/alpha values
#' to particular Layers/Sources/Sites/Quantities/Regions.    
#' @param col.labels,linetype.labels,alpha.labels A vector of character strings which are used as the labels for the lines. Must have the same length as the
#' number of Sources/Layers/Sites/Quantities in the plot.  The vectors can/should be named to match particular col/linewtype/alpha values to particular Layers/Sources/Sites/Quantities/Region.    
#' @param linewidth Numeric (as ggplot2), width of the lines on the plot, consistent with ggplot2. Note the width is doubled for the aggregate/summary line.
#' @param size Numeric, size of the points for the aggregate/summary data, consistent with ggplot2.
#' @param summary.function A function to summarise (aggregate) across year and plot on top.  Obvious choice is \code{mean}, but there is flexibility to anything that operates on 
#' a vector of numerics - eg median, a 95th percentile, standard deviation.
#' @param summary.function.label An optional character string to give a pretty label to the summary function legend.
#' @param text.multiplier A number specifying an overall multiplier for the text on the plot.  
#' Make it bigger if the text is too small on large plots and vice-versa.
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
                          linetypes = NULL,
                          linetype.by = NULL,
                          linetype.labels = waiver(),
                          alphas = NULL,
                          alpha.by = NULL,
                          alpha.labels = waiver(),
                          linewidth = 0.5,
                          size = 3 ,
                          y.label = NULL,
                          x.label = NULL,
                          summary.function,
                          summary.function.label = deparse(substitute(summary.function)),
                          text.multiplier = NULL,
                          plot = TRUE,
                          ...) {
  
  
  Quantity = Month = Source = Value = Year = PlotGroup = StatsGroup = NULL
  
  ### CHECK INPUTS AND HANDLE SPECIAL CASES
  
  # set a flag for the special case of colouring by year
  special_case_year_colour <- FALSE
  if(identical(col.by,"Year")) {
    special_case_year_colour <- TRUE
    if(!missing(cols) || !is.null(cols)) {
      warning("'cols' argument ignored since colouring by Year and therefore using a continuous scale")
      cols <- NULL
    } 
  }
  
  
  
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
  
  ### CHECK IS YEAR IS PRESENT 
  if(!("Year" %in% names(data.toplot))) {
    special_case_year_colour <- FALSE
    if(missing(col.by)) col.by = NULL
  }
  
  
  ### XX. FACETTING
  
  # all column names, used a lot below 
  all.columns <- names(data.toplot)
  
  # check the "xxx.by" arguments 
  if(!missing(col.by) && !is.null(col.by) && !col.by %in% all.columns) stop(paste("Colouring by", col.by, "requested, but that is not available, so failing."))
  if(!missing(linetype.by) && !is.null(linetype.by) && !linetype.by %in% all.columns) stop(paste("Setting linetypes by", linetype.by, "requested, but that is not available, so failing."))
  if(!missing(alpha.by) && !is.null(alpha.by) && !alpha.by %in% all.columns) stop(paste("Setting alphas by", alpha.by, "requested, but that is not available, so failing."))
  
  # ar first assume facetting by everything except for...
  dontFacet <- c("Value", "Time", "Year", "Month", "Season", "Day", "Lon", "Lat", col.by,  linetype.by, alpha.by)
  facet.vars <- all.columns[!all.columns %in% dontFacet]
  
  # then remove facets with only one unique value
  for(this.facet in facet.vars) {
    if(length(unique(data.toplot[[this.facet]])) == 1) facet.vars <- facet.vars[!facet.vars == this.facet]
  }
  
  
  ### LEGEND ENTRY ORDERING
  # ## Fix order of items in legend(s) by making them factors with levels corresponding to the order of the input fields
  # all.sources <- list()
  # # first loop across the fields
  # for(this.field in fields) {
  #   all.sources <- append(all.sources, this.field@source@name)
  # }
  # if("Source" %in% names(data.toplot)) data.toplot[, Source := factor(Source, levels = unique(all.sources))]
  
  
  ### 7. MAKE THE Y-AXIS LABEL
  if(is.null(y.label)) {
    y.label <- makeYAxis(final.fields)
  }
  
  ### MATCH LAYER COLOUR 
  
  # if cols is not specified and plots are to be coloured by Layers, look up line colours from Layer meta-data
  if(missing(cols) && !is.null(col.by) && col.by == "Layer"){
    
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
  
  ### DEPRECATED BY CALL TO makeYAxis() above, but some functionality might be needed to keep for now.
  
  # unit.str <- list()
  # quant.str <- list()
  # id.str <- list()
  # for(this.field in final.fields) {
  #   
  #   # pull out the unit string, id and full name string
  #   quant <- this.field@quant
  #   unit.str <- append(unit.str, quant@units)
  #   quant.str <- append(quant.str, quant@name)
  #   id.str <- append(id.str, quant@id)
  #   
  # }
  # 
  # # check the units
  # if(length(unique(unit.str)) == 1){
  #   unit.str <- unique(unit.str)
  # }
  # else {
  #   unit.str <- paste(unique(unit.str), collapse = ", ")
  #   warning("Quants to be plotted have non-identical units in plotSeasonal().")
  # }
  # 
  # # check the strings
  # if(length(unique(quant.str)) == 1){ quant.str <- unique(quant.str) }
  # else{ quant.str <- paste(id.str, sep = ", ", collapse = ", ") }
  
  
  ### MAKE A DESCRIPTIVE TITLE IF ONE HAS NOT BEEN SUPPLIED
  if(missing(title) || missing(subtitle)) {
    titles <- makePlotTitle(final.fields)  
    if(missing(title)) title <- titles[["title"]]
    else if(is.null(title)) title <- waiver()
    if(missing(subtitle)) subtitle <- titles[["subtitle"]]
    else if(is.null(subtitle)) subtitle <- waiver()
  }
  
  
  
  ### MAKE THE GROUPS - these are columns for ggplot which are essentially interaction terms to group the data
  
  # The plot group 
  
  # include Year only if not colouring by year
  if(special_case_year_colour) interaction_list <- c()
  else {
    if("Year" %in% names(data.toplot))  interaction_list <- c("Year")
    else  interaction_list <- c()
  }
  
  # include the other columns if required for an aesthetic
  if(!is.null(col.by)) {
    interaction_list <- append(interaction_list, col.by) # special case for col.by because if missing is taken to be "Year"
  }
  if(!missing(linetype.by) && !is.null(linetype.by)) interaction_list <- append(interaction_list, linetype.by)
  if(!missing(alpha.by) && !is.null(alpha.by)) interaction_list <- append(interaction_list, alpha.by)
  
  # and now make the interaction column
  data.toplot[, PlotGroup := interaction(.SD), .SDcols = interaction_list]
  
  
  # The Stats Group - this is much easier since we every possible combination must have it's own stat
  stats_SDcols <- names(data.toplot) %in% c("Source", "Quantity", "Layer", "Region", "Site")
  data.toplot[, StatsGroup := interaction(.SD), .SDcols = stats_SDcols]
  
  
  
  ###### MAKE THE PLOT #####
  
  # return the data if plot = FALSE
  if(!plot) return(data.toplot)
  
  # make the "symbols" for the ggplot2 call.  A bit of a pain -since they ggplot2 folks took away aes_string()- but what can you do...
  col.sym <- if(is.character(col.by)) ensym(col.by) else NULL  
  alpha.sym <- if(is.character(alpha.by))  ensym(alpha.by) else NULL 
  linetype.sym <- if(is.character(linetype.by)) ensym(linetype.by) else  NULL
  
  # build the basic plot
  p <- ggplot(as.data.frame(data.toplot), aes(x = .data[[subannual.dimension]], y = Value, group = PlotGroup,
                                              col = !! col.sym, 
                                              alpha = !! alpha.sym,
                                              linetype = !! linetype.sym))
  
  # build arguments for aesthetics to geom_line/geom_line and/or fixed arguments outside
  geom_args <- list()

  # col, alpha and linetyp
  if(!is.null(cols) &&  is.null(col.by)) geom_args[["colour"]] <- cols
  if(!is.null(alphas) && is.null(alpha.by)) geom_args[["alpha"]] <- alphas
  if(!is.null(linetypes) &&  is.null(linetype.by)) geom_args[["linetype"]] <- linetypes
  
  # line width if a fixed value for all 
  geom_args[["linewidth"]] <-  linewidth
  
  # call geom_line (with fixed aesthetics define above)
  p <- p + do.call(geom_line, geom_args)
  
  
  # add scales for the defined aesthetics
  
  # colour scale is a special case for two reasons,
  # 1. if col.by not variable provided, then the special case is activated and colour by year (and hence we have a continuous colour scale)
  # 2. if no colours provided, use a viridis pallete to override ggplot2 
  if(special_case_year_colour) p <- p + viridis::scale_color_viridis(name = "Year")
  else if (!is.null(col.by) & !is.null(cols)) p <- p + scale_color_manual(values=cols, labels=col.labels) 
  else p <- p + viridis::scale_colour_viridis(discrete=TRUE, option = "C", end = 0.9 ) 
  
  # these are simply defined by the arguments, no special cases
  if(!is.null(linetype.by) & !is.null(linetypes)) p <- p + scale_linetype_manual(values=linetypes, labels=linetype.labels)
  if(!is.null(alpha.by) & !is.null(alphas)) p <- p + scale_alpha_manual(values=alphas, labels=alpha.labels)
  
  # set the theme to theme_bw, simplest way to set the background to white
  p <- p + theme_bw()
  
  # make scale a bit bigger - consider removing for purity??  this can easy be controlled by the user
  p <- p + theme(legend.key.size = unit(2, 'lines'))
  
  
  # if chosen, plot the average year
  if(!missing(summary.function)) {
    
    # if is the special case where we colour by year
    if(special_case_year_colour) {
      
      # NOTE in this case we always use black
      
      # special case to add a line type scale if it wasn't already added
      # honestly not sure exactly why this works and many other things I tried didn't work
      if(missing(linetype.by) || is.null(linetype.by)) {
        
        p <- p + stat_summary(aes(group=col.by, linetype = "dummy string"), fun=summary.function, geom="line", color="black", linewidth = linewidth * 2)
        p <- p + scale_linetype_manual(values=c("dummy string"="solid"), labels = c("dummy string" = summary.function.label), name = element_blank())
        
      } 
      # if linetypes are already specified
      else{
        p <- p + stat_summary(aes(group=StatsGroup, linetype = .data[[linetype.by]]), fun=summary.function, geom="line", color="black", linewidth = linewidth * 2)
        # title the legend
        p <- p + labs(linetype=summary.function.label) 
      }
      
      
      
    }
    #if *not* the special case where we colour by year
    else {
      
      # add the stats
      p <- p + stat_summary(aes(group=StatsGroup, fill = get(col.by)), fun=summary.function, geom="point", color="black", shape = 21, size = size)
      
      # colour the points appropriately
      if(!is.null(cols)) p <- p + scale_fill_manual(values = cols, name = summary.function.label, labels = col.labels)
      else p <- p + viridis::scale_fill_viridis(name = summary.function.label, labels = col.labels, discrete = TRUE, option = "C", end = 0.9 )
      p <- p + labs(shape=summary.function.label ) 
      
      # also add linetype if necessary
      if(!missing(linetype.by) && !is.null(linetype.by)) {
        p <- p + stat_summary(aes(group=StatsGroup, linetype = .data[[linetype.by]]), fun=summary.function, geom="line", color="black", linewidth = linewidth * 2)
      } 
      
    }
    
  }
  
  
  # set the x-axis
  if(subannual.dimension == "Month") p <- p + scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep","Oct","Nov","Dec"))
  
  # set the title
  p <- p + labs(title = title, subtitle = subtitle,  y = y.label, x = subannual.dimension)
  
  p <- p + theme(plot.title = element_text(hjust = 0.5),
                 plot.subtitle = element_text(hjust = 0.5))
  
  # set legend 
  p <- p + theme(legend.position = "right", legend.key.size = unit(2, 'lines'))
  
  # facet if necessary
  if(length(facet.vars) > 0) p <- p + facet_wrap(facet.vars, ...)
  
  # overall text multiplier
  if(!missing(text.multiplier)) p <- p + theme(text = element_text(size = theme_get()$text$size * text.multiplier))
  
  return(p)
  
}
