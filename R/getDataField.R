#!/usr/bin/Rscript

################################# GET MODEL FIELD - Does a lot! #########################################
#
#' Get a \code{ModelField}, optionally with spatial/temporal averaging/cropping 
#' 
#' Given a \code{Source} object a \code{Quantity} object, return an appropriate spatially-averaged \code{ModelField} oject for that run and quantity. Arguments can also be provided for averaging over different spatial or temporal extents (very useful) or optionall just cropping to those extents
#' 
#' Note that because there are two types of averaging available, the resulting \code{Source} object can either be full spatial-temporal dataset, a spatial-only dataset (map), a temporal only datasey a time-series) or an average across both space and time, i.e. a single number.
#' Also not that the data is stored internal as a data.table object, but this is mostly not important to the user.
#'   
#' @param source The \code{Source} object for which the spatially-averaged \code{ModelField} should be built (eg. "lai")
#' @param var The quantity (either a \code{Quantity} or a string containing its \code{id}) 
#' @param temporal.extent The temporal extent (as a \code{TemporalExtent} object over which the data is to be averaged)
#' @param temporal.extent.id A character string to give an identifier for the temporal period this ModelField covers.
#' @param temporal.aggregate.method A character string describing the method by which to temporally aggregate the data.  Leave blank or use "none" to apply no temporal aggregation. Can currently be "mean", "sum", "max", "min", "sd" and "var".
#' For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param spatial.extent An extent in space to which this Field should be cropped, supplied as a raster::extent object or an object from which a raster::extent object can be derived - eg. a Raster* object or another Field object.
#' @param spatial.extent.id A character string to give an identifier for the spatial extent this ModelField covers.
#' @param spatial.aggregate.method  A character string describing the method by which to spatially aggregate the data.  Leave blank or use "none" to apply no spatially aggregation. Can currently be "weighted.mean", "w.mean", "mean", 
#' "weighted.sum", "w.sum", "sum", "max", "min", "sd" or "var".  For technical reasons these need to be implemented in the package in the code however it should be easy to implement more, please just contact the author!
#' @param read.full If TRUE ignore any pre-averaged file on disk, if FALSE use one if it is there (can save a lot of time if averaged file is already saved on disk)
#' @param verbose If TRUE give a lot of information for debugging/checking.
#' @param write If TRUE, write the data of the \code{ModelField} to disk as text file.
#' @param store.internally If TRUE store the resulting \code{ModelField} in the \code{Source} for using later
#' @param store.full If TRUE save the full temporal and spatial output in memory (if it is read) to save time if making more \code{ModelFields} from the variable later.  However, use with caution, saving too many full variables can easily fill up your system's RAM memory!
#' @param adgvm.scheme In the case of analysing an aDGVM run, select the PFT classification scheme for when post-hoc assigning the individuals into PFTS.
#' 
#' @return A \code{ModelField}. 
#' @export
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}

getDataField <- function(source,
                         var, 
                         layers,
                         spatial.resolution = NULL,
                         spatial.extent = NULL, 
                         spatial.extent.id = NULL, 
                         spatial.aggregate.method = NULL,
                         
                         temporal.resolution = NULL,
                         temporal.extent = NULL, 
                         temporal.extent.id = NULL,
                         temporal.aggregate.method = NULL, 
                         
                         subannual.resolution = NULL,
                         subannual.original = NULL,
                         subannual.aggregate.method = NULL, 
                         
                         write = FALSE, 
                         read.full = TRUE, 
                         verbose = TRUE, 
                         store.internally = FALSE,
                         store.full = FALSE,
                         adgvm.scheme = 1){
  
  
  ######### CHOOSE HERE BASED ON INITIAL RESOLUTION AS CONTAINED IN THE SCOURCE
  
  
  #### NEED A 'makeDataFileName function here'
  
  #source.id.quantity.spatial.resolution.spatial.method.temporal.resolution.first.year
  
  
  # To avoid annoying NOTES when R CMD CHECK-ing
  Lon = Lat = Year = NULL  
  
  
  ### CONVERT STRING TO VEGQUANT
  if(class(var) == "character") {
    quant <- lookupQuantity(var, "Standard")
    var.string <- var
  }
  else {
    quant <- var
    var.string <- quant@id
  }
  
  ### MAKE UNIQUE IDENTIFIER OF THIS VEGOBJECT VARIABLE AND FILENAME - this describes completely whether we want the files spatially or temporally aggregated and reduced in extent

  
  data.field.id <- makeFieldID(source.info =  as(source, "SourceInfo"), 
                               var.string = var.string, 
                               spatial.resolution = spatial.resolution,
                               temporal.resolution = temporal.resolution,
                               subannual.resolution = subannual.resolution,
                               temporal.extent.id = temporal.extent.id, 
                               spatial.extent.id = spatial.extent.id, 
                               temporal.aggregate.method = temporal.aggregate.method, 
                               spatial.aggregate.method = spatial.aggregate.method,
                               subannual.aggregate.method = subannual.aggregate.method
  )

  file.string <- file.path(source@dir, data.field.id)
  file.name.nc  <- paste(file.string, "nc", sep = ".")
  file.name.DGVMData <- paste(file.string, "DGVMData", sep = ".")
  if(verbose) message(paste("Seeking Field with id = ", data.field.id, sep = ""))
  

  
  
  ### CASE 1 - USE THE EXACT FIELD IF IT HAS ALREADY BEEN COMPUTED AND SAVED IN THE SOURCE IN MEMORY
  if(data.field.id %in% names(source@objects)){
    
    # if it is present it in memory then can be returned directly
    if(verbose) message(paste("Exact ModelField (with id = ", data.field.id, ") already found in memory for this.Field, so using that.", sep = ""))
    return(source@objects[[data.field.id]])
    
  }
  
  
  
  ### CASE 2 - USE THE PREAVERAGED/CROPPED VEGOBJECT FROM DISK IF AVAILABLE (and if we are not forcing a re-read)
  if(file.exists(paste(file.name.DGVMData )) & !read.full){
    
    # get the object from disk
    if(verbose) {message(paste("File",  file.name.DGVMData , "found in",  dir, "(and read.full not selected) so reading it from disk and using that.",  sep = " "))}
    data.field <- readRDS(file.name.DGVMData )
    
    # Update the Source, that might have (legitimately) changed compared to the id that was used when this model.field was created
    # for example it might be assigned a new id.
    data.field@source <- as(source, "SourceInfo")
    
    
    # Check that the spatial extent matches before returning
    # Note that there are various cases to check here (the full spatial extent and specifically defined extents)
    if(is.null(spatial.extent) & is.null(spatial.extent.id)
       | identical(spatial.extent, data.field@spatial.extent)){
      if(store.internally) {source <<- addToSource(data.field, source)}
      return(data.field)
    }  
    
    # Otherwise we must discard this Field and we need to re-average (and maybe also re-read) using the cases below 
    message(paste("Details of the spatial extent",  spatial.extent.id, "didn't match.  So file on disk ignored and the original data is being re-read"))
    rm(data.field)
    gc()
    
  }
  
  
  
  ############################################################################################################  
  #### NOTE: We don't pass this point if the correct pre-averaged data is available (on disk or in memory) ###
  #### as we will have already returned in CASE 1 or CASE 2 above.                                         ###
  ############################################################################################################
  
  
  
  ### CASE 3 - IF THE WHOLE FILE HAS BEEN READ AND STORED IN MEMORY AS A VEGOBJECT, THEN TAKE THAT AND EXTRACT THE DATA.TABLE AND THEN AVERAGE IT BELOW
  if(var.string %in% names(source@objects)){
    
    if(verbose) message(paste(var.string, " is already read, so using that internal copy.", sep = ""))
    all.dt <- source@objects[[var.string]]@data
    setKeyDGVM(all.dt)
    
  }
  
  
  
  ### CASE 4 - ELSE CALL THE MODEL SPECIFIC FUNCTIONS TO READ THE RAW MODEL OUTPUT AND THEN AVERAGE IT BELOW 
  else {
    
  
      message(paste0("Opening file ", file.name.nc))     
      
      
      this.nc <- ncdf4::nc_open(file.name.nc, readunlim=FALSE, verbose=verbose, suppress_dimvals=FALSE )
      this.lat <- ncdf4::ncvar_get(this.nc,"lat",verbose=verbose)
      this.lon <- ncdf4::ncvar_get(this.nc,"lon",verbose=verbose)
      
      if(missing(layers)) {
        all.vars <- c()
        for(variable in this.nc$var) {
          all.vars <- append(all.vars, paste(quant@id, variable, sep = "."))
        }
      }
      else {
        all.vars <- paste(layers, quant@id, sep = ".")
      }
      
      print(all.vars)
      
      all.dt <- data.table(Lon = numeric(0), Lat = numeric(0))
      setkey(all.dt, Lon, Lat)
      
      for(this.var in all.vars) {
        
        # Get the actual data and set the dimension names    
        this.slice <- ncdf4::ncvar_get(this.nc, this.var, start = c(1,1), count = c(-1,-1))
        dimnames(this.slice) <- list(this.lon, this.lat)
        
        # melt to a data.table, via data.frame
        this.slice.dt <- as.data.table(melt(this.slice))
        
        # remove NAs
        this.slice.dt <- stats::na.omit(this.slice.dt)
        
        # look up attributes for meta-data for this layer/variable
        
        # quantity
        quant.str <- ncatt_get(this.nc, this.var, attname="DGVMTools_quant", verbose=FALSE)$value
        
        # set the name to something equivalent in the model
        layer.name <- ncatt_get(this.nc, this.var, attname="DGVMTools_layer.name", verbose=FALSE)$value
        setnames(this.slice.dt, c("Lon", "Lat", layer.name))
        
        # London centre
        this.slice.dt[, Lon := LondonCentre(Lon)]
        
        # also set the key 
        setkey(this.slice.dt, Lon, Lat)
        
        # now join this to all.dt
        all.dt <- all.dt[this.slice.dt]
        
      }
      
      all.dt <- setKeyDGVM(all.dt)
      print(all.dt)
      # overall meta-data for this DataObject
      
      # dataset name
      dataset.name <- ncatt_get(this.nc, 0, attname="DGVMTools_name", verbose=FALSE)$value
      
      # dataset id
      dataset.id <- ncatt_get(this.nc, 0, attname="DGVMTools_id", verbose=FALSE)$value
      
      
      # spatial extent
      spatial.extent.id <- ncatt_get(this.nc, 0, attname="DGVMTools_spatial.extent.id", verbose=FALSE)$value
      # Determine
      spatial.extent <- extent(this.slice.dt)
      
      # temporal extent
      temporal.extent <- new("TemporalExtent", 
                             id = ncatt_get(this.nc, 0, attname="DGVMTools_temporal.extent.id", verbose=FALSE)$value,
                             name = ncatt_get(this.nc, 0, attname="DGVMTools_temporal.extent.name", verbose=FALSE)$value,
                             start = ncatt_get(this.nc, 0, attname="DGVMTools_temporal.extent.start", verbose=FALSE)$value,
                             end = ncatt_get(this.nc, 0, attname="DGVMTools_temporal.extent.end", verbose=FALSE)$value
      )
 
      
      this.Field <- new("Field",
                        id = data.field.id,
                        data = all.dt,
                        quant = quant,
                        spatial.extent = extent(all.dt),
                        spatial.extent.id = "Full",
                        temporal.extent = temporal.extent,
                        temporal.extent.id = "Full",
                        spatial.aggregate.method = "none",
                        temporal.aggregate.method = "none",
                        source = as(object = source, Class = "SourceInfo"))
      
      
      
      print(this.Field)
      
   
    
    

    ### !!! END CALL MODEL SPECIFIC FUNCTIONS !!!
    
    
    ### STORE THE FULL MODEL OUTPUT AS AN UNAVERAGED VEGOBJECT IF REQUESTED
    # Note we gotta do this now before the cropping and averaging below
    if(store.full){
      
      # Build the full object
      # year.range <- range(all.dt[,Year])
      # model.field.full <- new("Field",
      #                         id = var.string,
      #                         data = all.dt,
      #                         quant = quant,
      #                         spatial.extent = extent(all.dt),
      #                         spatial.extent.id = "Full",
      #                         temporal.extent = new("TemporalExtent",
      #                                               id = "FullTS",
      #                                               name = "Full simulation duration",
      #                                               start = year.range[1],
      #                                               end = year.range[length(year.range)]),
      #                         temporal.extent.id = "Full",
      #                         spatial.aggregate.method = "none",
      #                         temporal.aggregate.method = "none",
      #                         source = as(run, "SourceInfo"))
      
      # name and store
      #names(model.field.full) <- var.string
      source <<- addToSource(this.Field, source)
      
    } # end if(store.full)
    
  } # end option 4
  
  
  ### CROP THE SPATIAL AND TEMPORAL EXTENTS IF REQUESTED, AND CHECK THAT WE HAVE A VALID DATA.TABLE
  if(!is.null(spatial.extent) )  {
    print(spatial.extent)
    # if the provided spatial yields a valid extent, use the crop function
    possible.error <- try ( extent(spatial.extent), silent=TRUE )
    if (class(possible.error) != "try-error") {
      all.dt <- crop(all.dt, spatial.extent)  
      this.spatial.extent <- extent(spatial.extent)
      if(missing(spatial.extent.id)) spatial.extent.id <- "CroppedToExtent"
    }
    
    # else check if some gridcells to be selected with getGridcells
    else if(is.data.frame(spatial.extent) || is.data.table(spatial.extent) || is.numeric(spatial.extent) || is.list(spatial.extent)){
      all.dt <- selectGridcells(all.dt, spatial.extent)
      this.spatial.extent <- spatial.extent
      if(missing(spatial.extent.id)) spatial.extent.id <- "SubsetOfGridcells"
    }
    
    # else fail with error message
    else {
      stop(paste("Trying to select a spatial extent using an object of class", class(spatial.extent)[1], "which isn't really working for me right now.  If this a Spatial* class, contact the authors and they might implement it"))
    }
    
  }
  else {
    
    this.spatial.extent <- extent(all.dt)
    
    if(verbose) message(paste("No spatial extent specified, setting spatial extent to full simulation domain: Lon = (",  this.spatial.extent@xmin, ",", this.spatial.extent@xmax, "), Lat = (" ,  this.spatial.extent@ymin, ",", this.spatial.extent@ymax, ").", sep = ""))
    
  }
  
  if(!is.null(temporal.extent))  all.dt <- selectYears(all.dt, temporal.extent)   
  
  if(length(all.dt) == 0) stop("getModelField() has produced an empty data.table, so subsequent code will undoubtedly fail.  Please check your input data and the temporal.exent and spatial.extent that you have requested.")
  
  
  
  # ### GET THE SPATAIAL AND TEMPORAL EXTENTS BEFORE THEY MAY BE AVERAGED AWAY
  # ordered.years = NULL
  # if("Year" %in% names(all.dt)) { 
  #   ordered.years <- sort(unique(all.dt[,Year]))
  # }
  # 
  # 
  # ###  DO SPATIAL AGGREGATION - must be first because it fails if we do spatial averaging after temporal averaging, not sure why
  # if(tolower(spatial.aggregate.method) != "none"){
  #   all.dt <- aggregateSpatial(all.dt, method = spatial.aggregate.method, verbose = verbose)
  #   if(verbose) {
  #     message("Head of spatially aggregated data.table:")
  #     print(utils::head(all.dt))
  #   }
  # }
  
  
  # ###  DO TIME AGGREGATATION
  # if(tolower(temporal.aggregate.method) != "none"){
  #   all.dt <- aggregateTemporal(all.dt, method = temporal.aggregate.method, verbose = verbose)
  #   if(verbose) {
  #     message("Head of time aggregated data.table:")
  #     print(utils::head(all.dt))
  #   }
  # }
  # 
  
  ### IF NO EXTENTS SPECIFIED, GET THE EXTENTS FOR THE FINAL VEGOBJECT TO RETURN
  
  # TEMPORAL
  if(is.null(temporal.extent)) {
    temporal.extent<- new("TemporalExtent",
                          id = "FullTS",
                          name = "Full simulation duration",
                          start = ordered.years[1],
                          end = ordered.years[length(ordered.years)]
    )
    if(verbose) message(paste("No temporal extent specified, setting temporal extent to whole simulation duration (",  temporal.extent@start, "-", temporal.extent@end, ")", sep = ""))
  }
  
  
  
  # ### BUILD THE FINAL VEGOBJECT, STORE IT IF REQUESTED AND RETURN IT
  # model.field <- new("Field",
  #                    id = data.field.id,
  #                    data = all.dt,
  #                    quant = quant,
  #                    spatial.extent = this.spatial.extent,
  #                    temporal.extent = temporal.extent,
  #                    spatial.extent.id = spatial.extent.id,
  #                    temporal.extent.id = temporal.extent.id,
  #                    spatial.aggregate.method = spatial.aggregate.method,
  #                    temporal.aggregate.method = temporal.aggregate.method,
  #                    source = as(run, "SourceInfo"))
  
  
  ### WRITE THE VEGOBJECT TO DISK AS AN DGVMData OBJECT IF REQUESTED
  if(write) {
    if(verbose) {message("Saving as a .DGVMData object...")}
    saveRDS(this.Field, file = file.name)
    if(verbose) {message("...done.")}
  }
  
  ### ADD TO THE MODELRUN OBJECT IF REQUESTED
  if(store.internally) {
    source <<- addToSource(this.Field, source)
  }
  
  return(this.Field)
  
}
