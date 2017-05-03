#!/usr/bin/Rscript

## check if two classes are comparable
#' Checks two DGVMTools metadata objects for equality
#' 
#' @description checks if two objects have the same quantity, spatial or temporal extent
#' 
#' @param a a DGVMTools metadata object
#' @param b another DGVMTools metadata object of the same type
#' @return logical
#' @name is.equal
#' @rdname is.equal
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
setGeneric("is.equal", function(a, b) standardGeneric("is.equal"))

#' @describeIn is.equal Checks two Quantity objects for equality
setMethod("is.equal", signature("Quantity", "Quantity"), function(a, b) {
  if (a@type==b@type && a@units==b@units && a@aggregate.method==b@aggregate.method)
    return(TRUE)
  return(FALSE)
})

#' @describeIn is.equal Checks two TemoralExtent objects for equality
setMethod("is.equal", signature("TemporalExtent", "TemporalExtent"), function(a, b) {
  if (a@start==b@start && a@end==b@end)
    return(TRUE)
  return(FALSE)
})

#' @describeIn is.equal Checks two SpatialExtent objects for equality
setMethod("is.equal", signature("SpatialExtent", "SpatialExtent"), function(a, b) {
  if (all(!is.finite(c(a@extent@xmin, b@extent@xmin, a@extent@xmax, b@extent@xmax, 
                       a@extent@ymin, b@extent@ymin, a@extent@ymax, b@extent@ymax))))
    return(TRUE)
  if (a@extent@xmin==b@extent@xmin && a@extent@xmax==b@extent@xmax && 
      a@extent@ymin==b@extent@ymin && a@extent@ymax==b@extent@ymax)
    return(TRUE)
  return(FALSE)
})

## print an experimental summary of the class ModelRun.
##
## Don't know, which were the really important information to return,
## therfore this is just a template.
## so far simply returns a list. 
## Needs to also include a generic "print" for pretty output
##

#' Summary methods
#' 
#' Print easy to read summaries of DGVMTools objects
#' 
#' @param object a DGVMTools object
#' @param ... Other arguments, not currently used
#' @return A list of strings
#' @name Summary-methods
#' @rdname Summary-methods
#' @exportMethod 
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}         
setGeneric("summary", function(object,...) standardGeneric("summary"))

#' @rdname Summary-methods
#' @aliases summary
setMethod("summary", signature("ModelRun"), function(object, ...) {
  ret <- list(id=object@id, name=object@name, model=object@model)
  
  spatial=NULL
  temporal=NULL
  full=NULL
  for (n in names(object@objects)) {
    if (object@objects[[n]]@is.spatially.averaged) {
      temporal[[n]]=list(name=n,
                         description=object@objects[[n]]@quant@name,
                         units=object@objects[[n]]@quant@units,
                         colnames=colnames(object@objects[[n]]@data))
    } else if (object@objects[[n]]@is.temporally.averaged) {
      spatial[[n]]=list(name=n,
                        description=object@objects[[n]]@quant@name,
                        units=object@objects[[n]]@quant@units,
                        colnames=colnames(object@objects[[n]]@data))
    } else {
      full[[n]]=list(name=n,
                     description=object@objects[[n]]@quant@name,
                     units=object@objects[[n]]@quant@units,
                     colnames=colnames(object@objects[[n]]@data))
    }
  }
  if (!is.null(spatial)) {
    ret[['spatial']] = spatial
  }
  if (!is.null(temporal)) {
    ret[['temporal']] = temporal
  }
  if (!is.null(full)) {
    ret[['full']] = full
  }
  
  return(ret)
})


#' Crop ModelObjects (or data.tables, or Raster* objects)
#' 
#' A more flexible version of raster::crop() which also take ModelObjects and data.tables for cropping, 
#' and can use a SpatialExtent object to define the domain.  SHOULD BE DEFINED AS A METHOD EXTENDING raster::crop()!
#' 
#' @param input The ModelObject, data.table or Raster* object to be cropped
#' @param extent The spatial extent to be be cropped to, defined as a SpatialExtent or raster::extent
#' 
#' @return A ModelObject, data.table or Raster* object cropped to the desired extent.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster data.table
#' @export

cropDGVM <- function(input, extent){
  
  Lat = Lon = NULL
  
  
  
  # determine the class of the object to which we are cropping
  extent.class <- class(extent)[1]
  if(extent.class == "Extent") {
    this.extent <- extent
  }
  else if(extent.class == "SpatialExtent"){
    this.extent <- extent@extent
    extent.class <- class(this.extent)[1] 
  }
  
  

  # determine the class of the object to be cropped
  input.class <- class(input)[1]

  
  # Do cropping depending on first the class of extent 'extent' 
  
  # CASE 1.X - target extent is a raster::Extent
  if(extent.class == "Extent") {

    # CASE 1.1 - object to be cropped is a raster - just use raster::crop
    if(input.class == "RasterBrick" | input.class == "RasterStack" | input.class == "RasterLayer"){
      return(raster::crop(input, this.extent))    
    }
    # CASE 1.2 - object to be cropped is a data.table - select on Lon and Lat
    else if(input.class == "data.table"){
      return(input[Lat < this.extent@ymax & Lat > this.extent@ymin & Lon < this.extent@xmax & Lon > this.extent@xmin,])
    }
    # CASE 1.3 - object to be cropped is a ModelObject or DataObject - pull out the data.table and select on Lon and Lat
    else if(input.class == "ModelObject" | input.class == "DataObject"){
      dt <- input@data
      dt[Lat < this.extent@ymax & Lat > this.extent@ymin & Lon < this.extent@xmax & Lon > this.extent@xmin,]
      input@data <- dt
      input@spatial.extent <- extent
      return(input)
    }
    
  }
  
  # CASE 2.X - target extent is a data.table
  else if(extent.class == "data.table") {
    
    # CASE 2.1 - object to be cropped is a raster - just use raster::crop
    if(input.class == "RasterBrick" | input.class == "RasterStack" | input.class == "RasterLayer"){
      
      stop("cropDGVM: Cropping a Raster* object to an extent defined by a data.table is not yet defined")
      
    }
    # CASE 2.2 - object to be cropped is a data.table - select on Lon and Lat by merging
    else if(input.class == "data.table"){
      
      # first remove any lines with NA (since the are probably not part of the extent) and pull out just the "Lon" and "Lat" columns
      this.extent <- na.omit(this.extent)[,c("Lon", "Lat")]
      
      # now merge the data.tables on Lon and Lat, requiring that all Lons and Lats in the new extent are included and return
      return(merge(input, this.extent, by=c("Lon","Lat"), all.x = FALSE, all.y = TRUE))
      
    }
    # CASE 2.3 - object to be cropped is a ModelObject or DataObject - select on Lon and Lat by merging
    else if(input.class == "ModelObject" | input.class == "DataObject"){
      
      # first remove any lines with NA (since the are probably not part of the extent) and pull out just the "Lon" and "Lat" columns
      this.extent <- na.omit(this.extent)[,c("Lon", "Lat"),with=FALSE]
      
      # now pull out the data.table merge the data.tables on Lon and Lat, requiring that all Lons and Lats in the new extent are included and return
      dt <- input@data
      #round only Lon and Lat fields to maintain precision of the data
      dt[, Lon := round(Lon, 4)]
      dt[, Lat := round(Lat, 4)]
      dt <- merge(dt, round(this.extent,4), by=c("Lon","Lat"), all.x = FALSE, all.y = TRUE)
      
      # put in new data.table to original object, update meta-data and return
      input@data <- dt
      input@spatial.extent <- extent
      input@id <- makeModelObjectID(input@quant@id, temporal.extent = input@temporal.extent, spatial.extent = extent, temporally.averaged = input@is.temporally.averaged, spatially.averaged = input@is.spatially.averaged)
      
      return(input)
    }
    
  }
  
  # CASE 3.X - target extent is a site
  else if(extent.class == "numeric" & length(input == 2)) {
    
    stop("cropDGVM: Cropping any object to sites not yet implemented  ")
    
  }
  
  # CASE 3.X - target extent is a site
  else {
    
    stop(paste("cropDGVM: Cropping any object to extent defined by object type", extent.class, "with length =", length(extent), "is not defined", sep = " "))
    
  }
  
}


setMethod("is.equal", signature("Quantity", "Quantity"), function(a, b) {
  if (a@type==b@type && a@units==b@units && a@aggregate.method==b@aggregate.method)
    return(TRUE)
  return(FALSE)
})




##### RETRIEVES AN OBJECT FROM A LIST BASED ON THE 'id' SLOTS
#
#' Retrieves an object from a list based on it's \code{id} slot
#' 
#' Looks through a list of arbitary object until it finds one which has a slot called "id" whose value matches the imput argument.
#' The idea is that you can use this fucntion to pull a particular PFT from a list of PFT objects, or a a particular run from a list of ModelRun objects etc.
#' 
#' @param id The id sought (must be string)
#' @param list The list to be scanned (must be a list)
#' 
#' @return The matching object from the list or NULL.  If NULL it will give a message and a warning, the following code will probably fail. 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export
#' 
byIDfromList <- function(id, list) {
  
  
  # Error checking
  if(class(id)[1] != "character") {
    stop(paste("From function IDfromList(): Argument 'id' not a string therefore not valid.  It has class = ",  class(id)[1], sep = ""))
  }
  
  if(class(list)[1] != "list") {
    stop(paste("From function IDfromList(): Argument 'list' is not a list therefore not valid.  It has class =",  class(list)[1], sep = ""))
  }
  
  for(item in list){
    
    tryCatch(
      {
        if(item@id == id) return(item)
      },
      error= function(cond){
        message(paste("Caught an expection. Function byIDfromList(id, list) found an item in the list argument with no slots. ", sep = ""))
      },
      warning=function(cond) {
      },
      finally={}
    )    
    
  }
  
  message(paste("ATTENTION! No object with id = ", id, " found in list ", deparse(substitute(list)), ", so probably your script will now fail.", sep = ""))
  
  
}


#' Convert a ModelObject, DataObject or data.table to a matrix
#' 
#' This function is useful for making a matrix of data (particularly for writing to disk as a netCDF file) whilst maintain the original longitudes and latitudes.  
#' The longitudes and latitudes are stored as the column and row names respectively. 
#' This is necessary because making a Raster objects converts unevenly spaced coordinates to evenly spaced ones.
#' 
#' @param input The input data, either as a ModelObject, DataObject or data.table
#' @param Lons A numeric vector of longitude value, used for gap-filling and ensuring that the longitudes are exactly what you want them to be.
#' @param Lats A numeric vector of latitude value, used for gap-filling and ensuring that the latitudes are exactly what you want them to be. 
#' @param gap.fill Logical, if true use fill in missing longitide columns and latitudes rows using either the provide Lons and Lats, or by filling in spaces if the grid spacing is regular.
#' @param layer Character string specifying which layer from the input should be used to make the matrix
#' @param tol Numeric specifying how close the differences between adjacant latitudes/longitudes should be when determining ir they are evenly spaced or not.    
#' 
#' @return A matrix (2D array) of the data with the longitudes and latitudes recored as the column and row names
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

convertToMatrix <- function(input, Lons = NULL, Lats = NULL, gap.fill = TRUE, layer, tol = 1E-10) {
  
  
  #### FUNCTIONS TO INSERT ROWS/COLUMNS INTO A MATRIX
  
  # function to insert a column of NAs
  insertColumns <- function(mat, col.name, fill.value = NA) {
    
    # get the columns names and find which is before and after the colum to add
    c.names <- sort(append(as.numeric(colnames(mat)), as.numeric(col.name)))
    new.index <- which(c.names ==  col.name)
    
    if(new.index < dim(mat)[2] && new.index > 1) {

      # split the matrix at the correct point
      first.half <- mat[,1:(new.index-1)]
      second.half <- mat[,new.index:length(colnames(mat))]
    } else if(new.index == 1) {
      first.half <- NULL
      second.half <- mat
    }  else {
      first.half <- mat
      second.half <- NULL
    }
    
    # make the new data and cbind it into the matrix
    new.col.data <- rep(NA, NROW(mat))
    new.mat <- cbind(first.half, new.col.data, second.half)
    colnames(new.mat) <- c.names
    
    return(new.mat)
    
  }
  
  
  # function to insert a row of NAs
  insertRows <- function(mat, row.name, fill.value = NA) {
    
    # get the columns names and find which is before and after the colum to add
    c.names <- sort(append(as.numeric(rownames(mat)), as.numeric(row.name)))
    new.index <- which(c.names == row.name)
    
    if(new.index < dim(mat)[1] && new.index > 1) {
      # split the matrix at the correct point
      first.half <- mat[1:(new.index-1),]
      second.half <- mat[new.index:length(rownames(mat)),]
    } else if(new.index == 1) {
      first.half <- NULL
      second.half <- mat
    }  else {
      first.half <- mat
      second.half <- NULL
    }
    
    # make the new data and cbind it into the matrix
    new.row.data <- rep(NA, NCOL(mat))
    new.mat <- rbind(first.half, new.row.data, second.half)
    rownames(new.mat) <- c.names
    
    return(new.mat)
    
  }
  
  
  ### MAKE THE MATRIX
  
  # input data type
  if(is.ModelObject(input) || is.DataObject(input)) input <- input@data
  
  # dcast the data so that columns at longitudes and rows are latitudes and then convert to matrix
  input.dcasted <- dcast(input, formula = Lat ~ Lon, value.var = layer, drop = FALSE)
  input.matrix <- as.matrix(input.dcasted) 
  
  # the first column comes out as latitude values, so use those values as row names and delete that first column
  rownames(input.matrix) <- input.matrix[,1]
  input.matrix <- input.matrix[,-1]
  
  # reverse the latitudes 
  input.matrix[nrow(input.matrix):1,]
  
  
  
  ##### GAP FILL MISSING ROWS AND COLUMNS
  
  if(gap.fill){
    
    #### COLUMNS/LONGITUDES
    
    # if latitudes provided
    if(!is.null(Lons))  { 
      
      # find the lats that are missing and we need to add
      lons.we.want <- Lons
      lons.we.have <- as.numeric(colnames(input.matrix))
      need.to.add <- lons.we.want[which(!lons.we.want %in% lons.we.have)]
      
      for(newcol in need.to.add){
        
        # check precision issues
        close.lon <- NULL
        for(check.lon in lons.we.have) {
          if(abs(newcol - check.lon) < tol) {
            close.lon <- check.lon
          }
        }
        
        # if no matched longitude then insert a new row for it
        if(is.null(close.lon)) {
          input.matrix <- insertColumns(input.matrix, newcol, fill.value = NA)
        }
        # if there was one matched closely, change label to the exact numeric
        else {
          # get the index for the one we want to change
          close.index <- which(as.numeric(colnames(input.matrix)) == close.lon)
          # get the names 
          all.names <- as.numeric(colnames(input.matrix))
          # substitute the longitude with the correct precision
          all.names[close.index] <- newcol
          # set the names back
          colnames(input.matrix) <- all.names
        }
        
      } # for each longitude we need to add
      
    } # end if longitudes provided
    
    # if lons not provided
    else {
      
      # get the differences between adjacent lats
      diff.lons <- diff(as.numeric(colnames(input.matrix)))
      
      # if diffences are not all the same do gap-filling (otherwise do nothing)
      if(!(abs(max(diff.lons) - min(diff.lons)) < 0.00000001)){
        
        # get a vector of the spacing
        spacings <- sort(unique(diff.lons))

        # check if the spacings are even
        even.spacings <- TRUE
        for(spacing in spacings){ if(!(spacing %% spacings[1] == 0)) even.spacings <- FALSE }
        
        # if even.spacings then we have even spacings and can just insert rows based on equal spacings 
        if(even.spacings){
          
          lons.we.have <- as.numeric(colnames(input.matrix))
          lons.we.want <- seq(lons.we.have[1], lons.we.have[length(lons.we.have)], by = spacings[1])
          need.to.add <- lons.we.want[which(!lons.we.want %in% lons.we.have)]
          for(newcol in need.to.add){
            input.matrix <- insertColumns(input.matrix, newcol, fill.value = NA)
          }
        }
        # else need to do something rather more clever
        else {
          stop("TROUBLE WITH UNEVENLY SPACED LONGITUDES WHEN GAP-FILLING A MATRIX")
        } # end if even.spacings
        
      } # end if differences are not all the same
      
    } # end if lats not provided
    
    
    #### ROWS/LATITUDES
    
    # if latitudes provided
    if(!is.null(Lats))  { 
      
      # find the lats that are missing and we need to add
      lats.we.want <- Lats
      lats.we.have <- as.numeric(rownames(input.matrix))
      need.to.add <- lats.we.want[which(!lats.we.want %in% lats.we.have)]
      
      
      for(newrow in need.to.add){
        
        # check precision issues
        close.lat <- NULL
        for(check.lat in lats.we.have) {
          if(abs(newrow - check.lat) < tol) {
            close.lat <- check.lat
          }
        }
        
        # if no matched latitude then insert a new row for it
        if(is.null(close.lat)) {
          input.matrix <- insertRows(input.matrix, newrow, fill.value = NA)
        }
        # if there was one matched closely, change label to the exact numeric
        else {
          # get the index for the one we want to change
          close.index <- which(as.numeric(rownames(input.matrix)) == close.lat)
          # get the names 
          all.names <- as.numeric(rownames(input.matrix))
          # substitute the latitude with the correct precision
          all.names[close.index] <- newrow
          # set the names back
          rownames(input.matrix) <- all.names
        }
        
      } # for each latitude we need to add
      
    } # end if latitudes provided
    
    # if lats not provided
    else {
      
      # get the differences between adjacent lats
      diff.lats <- diff(as.numeric(rownames(input.matrix)))
      
      # if diffences are not all the same do gap-filling (otherwise do nothing)
      if(!(abs(max(diff.lats) - min(diff.lats)) < 0.00000001)){
        
        # get a vector of the spacing
        spacings <- sort(unique(diff.lats))
        
        # check if the spacings are even
        even.spacings <- TRUE
        for(spacing in spacings){ if(!(spacing %% spacings[1] == 0)) even.spacings <- FALSE }
        
        # if even.spacings then we have even spacings and can just insert rows based on equal spacings 
        if(even.spacings){
          
          lats.we.have <- as.numeric(rownames(input.matrix))
          lats.we.want <- seq(lats.we.have[1], lats.we.have[length(lats.we.have)], by = spacings[1])
          need.to.add <- lats.we.want[which(!lats.we.want %in% lats.we.have)]
          for(newrow in need.to.add){
            input.matrix <- insertRows(input.matrix, newrow, fill.value = NA)
          }
        }
        # else need to do something rather more clever
        else {
          
          # calculate the mean spacing
          mean.spacing <- mean(spacings)

          # check each the difference between each spacing and the mean is less that 0.1
          okay <- TRUE
          for(spacing in spacings) {
                if(abs((spacing-mean.spacing)/mean.spacing) > 0.1) okay <- FALSE
          }
          
          
          if(!okay) stop("TROUBLE WITH UNEVENLY SPACED LATITUDES WHEN GAP-FILLING A MATRIX")
        } # end if even.spacings
        
      } # end if differences are not all the same
      
    } # end if lats not provided
    
  } # end if gap.fill
  
  
  # ...and return
  return(input.matrix)
  
}



#' Select a subset of a gridlist
#' 
#' Crop a gridlist (taken as a file in the form of a two-columned table to a smaller subset provided by an Extent object
#' 
#' @param gridlist.file A character string giving the location of the original gridlist
#' @param subset.extent  A raster Extent object specifying the geograpical sub-domain required
#' @param file.name A character string specifying a path to write the new gridlist (can be ignored to write no file)
#' @param header Logical, whether or not the original file has a header
#' @param offset A two-member numeric vector specifying the longitude-latitude offset from the given coordinates 
#' to the gridcell centre, ie. should be c(0.25,0.25) for old LPJ-GUESS gridlists, and c(0.0,0.0) for most other more sensible scenarios
#' 
#' @return The new gridlist as a data.frame   
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export

subsetGridlist <- function(gridlist.file, subset.extent, file.name = NULL, header = TRUE, offset = c(0.25, 0.25)){
  
  
  gridlist <- read.table(gridlist.file, header = header)
  names(gridlist )  <- c("Lon", "Lat")
  
  print(head(gridlist))
  
  gridlist$Lon <- gridlist$Lon + offset[1]
  gridlist$Lat <- gridlist$Lat + offset[2]
 
  gridlist <- gridlist[which(gridlist$Lon <= subset.extent@xmax 
                     & gridlist$Lon >= subset.extent@xmin 
                     & gridlist$Lat <= subset.extent@ymax 
                     & gridlist$Lat >= subset.extent@ymin), ]
  
  
  gridlist$Lon <- gridlist$Lon - offset[1]
  gridlist$Lat <- gridlist$Lat - offset[2]
  
  if(!is.null(file.name)) write.table(gridlist, file.name, quote = FALSE, row.names = FALSE)
  
  return(gridlist)
  
}

############### SUB ANNUAL PERIODS

#' @rdname Period-class
all.periods <- list(Jan = new("Period",
                              id = "Jan",
                              name = "January",
                              abbreviation = "Jan",
                              index = 1,
                              padded.index = "01",
                              contains = "Jan",
                              days = 31,
                              days.leap = 31,
                              col = fields::tim.colors(12)[1]),
                    Feb = new("Period",
                              id = "Feb",
                              name = "February",
                              abbreviation = "Feb",
                              index = 2,
                              padded.index = "02",
                              contains = "Feb",
                              days = 28,
                              days.leap = 29,
                              col = fields::tim.colors(12)[2]),
                    Mar = new("Period",
                              id = "Mar",
                              name = "March",
                              abbreviation = "Mar",
                              index = 3,
                              padded.index = "03",
                              contains = "Mar",
                              days = 31,
                              days.leap = 31,
                              col = fields::tim.colors(12)[3]),
                    Apr = new("Period",
                              id = "Apr",
                              name = "April",
                              abbreviation = "Apr",
                              index = 4,
                              padded.index = "04",
                              contains = "Apr",
                              days = 30,
                              days.leap = 30,
                              col = fields::tim.colors(12)[4]),
                    May = new("Period",
                              id = "May",
                              name = "May",
                              abbreviation = "May",
                              index = 5,
                              padded.index = "05",
                              contains = "May",
                              days = 31,
                              days.leap = 31,
                              col = fields::tim.colors(12)[5]),
                    Jun = new("Period",
                              id = "Jun",
                              name = "June",
                              abbreviation = "Jun",
                              index = 6,
                              padded.index = "06",
                              contains = "Jun",
                              days = 30,
                              days.leap = 30,
                              col = fields::tim.colors(12)[6]),
                    Jul = new("Period",
                              id ="Jul",
                              name = "July",
                              abbreviation = "Jul",
                              index = 7,
                              padded.index = "07",
                              contains = "Jul",
                              days = 31,
                              days.leap = 31,
                              col = fields::tim.colors(12)[7]),
                    Aug = new("Period",
                              id = "Aug",
                              name = "August",
                              abbreviation = "Aug",
                              index = 8,
                              padded.index = "08",
                              contains = "Aug",
                              days = 31,
                              days.leap = 31,
                              col = fields::tim.colors(12)[8]),
                    Sep = new("Period",
                              id = "Sep",
                              name = "September",
                              abbreviation = "Sep",
                              index = 9,
                              padded.index = "09",
                              contains = "Sep",
                              days = 30,
                              days.leap = 30,
                              col = fields::tim.colors(12)[9]),
                    Oct = new("Period",
                              id = "Oct",
                              name = "October",
                              abbreviation = "Oct",
                              index = 10,
                              padded.index = "10",
                              contains = "Oct",
                              days = 31,
                              days.leap = 31,
                              col = fields::tim.colors(12)[10]),
                    Nov = new("Period",
                              id = "Nov",
                              name = "November",
                              abbreviation = "Nov",
                              index = 11,
                              padded.index = "11",
                              contains = "Nov",
                              days = 30,
                              days.leap = 30,
                              col = fields::tim.colors(12)[11]),
                    Dec = new("Period",
                              id = "Dec",
                              name = "December",
                              abbreviation = "Dec",
                              index = 12,
                              padded.index = "12",
                              contains = "Dec",
                              days = 31,
                              days.leap = 31,
                              col = "maroon4"),
                    DJF = new("Period",
                              id = "DJF",
                              name = "Winter",
                              abbreviation = "DJF",
                              index = c(12,1,2),
                              padded.index = c("12", "01", "02"),
                              contains = c("Dec", "Jan", "Feb"),
                              days = 60,
                              days.leap = 61,
                              col = fields::tim.colors(4)[1]),
                    MAM = new("Period",
                              id = "MAM",
                              name = "Spring",
                              abbreviation = "MAM",
                              index = c(3,4,5),
                              padded.index = c("03", "04", "05"),
                              contains = c("Mar", "Apr", "May"),
                              days = 62,
                              days.leap = 62,
                              col = fields::tim.colors(4)[2]),
                    JJA = new("Period",
                              id = "JJA",
                              name = "Summer",
                              abbreviation = "JJA",
                              index = c(6,7,8),
                              padded.index = c("07", "08", "09"),
                              contains = c("Jun", "Jul", "Aug"),
                              days = 62,
                              days.leap = 62,
                              col = fields::tim.colors(4)[3]),          
                    SON = new("Period",
                              id = "SON",
                              name = "Autumn",
                              abbreviation = "SON",
                              index = c(9,10,11),
                              padded.index = c("09", "10", "11"),
                              contains = c("Sep", "Oct", "Nov"),
                              days = 61,
                              days.leap = 61,
                              col = fields::tim.colors(4)[4]),
                    Annual = new("Period",
                                 id = "Annual",
                                 name = "Annual",
                                 abbreviation = "Ann",
                                 index = seq(1,12,1),
                                 padded.index = "Annual",
                                 contains = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug","Sep", "Oct", "Nov", "Dec"), 
                                 days = 365,
                                 days.leap = 366,
                                 col = "black"
                    )
                    
)

# subsets of periods
#' @rdname Period-class
#' @format List of\code{Period} objects
periods <- all.periods

#' @rdname Period-class
months <- all.periods[1:12]

#' @rdname Period-class
seasons <- all.periods[13:16]

#' @rdname Period-class
annual <- all.periods[17]



########### MEMORY MANAGEMENT ########### 

#' Improved list of objects
#' 
#' Because rasters and \code{ModelObject}s and so can be quite large, this function is useful for checking how large are the objects you have stored in memory.  
#'  
#' @param pos ???
#' @param pattern Character, to match only certain objects
#' @param orderby Character, can be "Type", "Size", "PrettySize", Rows" or "Columns" 
#' @param decreasing Logical, whether to list the objects in decreasing size
#' @param head Logical, if true just print the first \code{n} objects
#' @param n Integer, how many objects to list if selecting just the head
#' @keywords internal
#' Credit: Taken from: http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session

lsos <- function (pos = 1, pattern, order.by = "Size",
                  decreasing=TRUE, head=TRUE, n=10) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

########### HANDY PROCESSING FUNCTIONS ########### 


#' Safe division
#' 
#' Function to divide two number but return 0 if the denominator is 0
#' 
#' @param x numerator
#' @param y denominator
#' 
#' Handy little thing.
#' @export
#' 
#' 
"%/0%" <- function(x,y) ifelse(y==0,0,base::"/"(x,y))



########### COLOUR SCHEMES AND PRETTY PLOTTING FUNCTIONS AND PARAMETERS ########### 


#' Helpful palettes for vegetation maps
#'
#' \describe{
#' \item{veg.palette}{A nice white-green-brown color scheme.  Good for vegetation cover}
#' \item{lai.palette}{Default colour scheme for LAI}
#' \item{cmass.palette}{Default colour scheme for biomass}
#' \item{difference.palette}{Colour scheme for differences. Symmetrical, centred on white}
#' \item{fire.palette}{Colour scheme for measures of fire activity like fire return time and area burned (from more-to-less fire)}
#' \item{reversed.fire.palette}{Colour scheme for measures of fire activity like fire return time and area burned (from less-to-more fire)}
#' \item{reversed.tim.colors}{The fields::tim.colors scheme reversed to go from red (hot/dry) to blue (cold/wet)}
#' }
#' @param n Number of colour shades required
#' @name veg.palettes
NULL

#' @rdname veg.palettes
veg.palette <- grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black"))

#' @rdname veg.palettes
lai.palette <- grDevices::colorRampPalette(c("blue", "lightskyblue1", "palevioletred", "khaki1", "yellowgreen", "forestgreen", "saddlebrown","black" )) #this is a function which returns a list of colours

#' @rdname veg.palettes
cmass.palette <- grDevices::colorRampPalette(c("lemonchiffon","peru", "forestgreen", "dodgerblue4", "orchid4", "hotpink", "red4"))

#' @rdname veg.palettes
difference.palette <- function(n){
  
  # Make palette based on RColorBrewer "RdBu" palette
  rdbu <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
  cols <- grDevices::colorRampPalette(rdbu)
  return(cols(n))  

  #grDevices::colorRampPalette(c("green", "blue", "white", "red", "yellow")) #this is a function which returns a list of colours

}

#' @rdname veg.palettes
fire.palette <- grDevices::colorRampPalette(c("red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3"))

#' @rdname veg.palettes
reversed.fire.palette <- grDevices::colorRampPalette(rev(c("red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3")))

#' @rdname veg.palettes
reversed.tim.colors = function(n) rev(fields::tim.colors(n))


########### CONVERSION OF ABOVE-GROUND BIOMASS TO TOTAL CARBON ####################
#' Calculate total Carbon given above ground biomass.
#'
#' Equation from Baccini et al . 2012. 
#' Used when reading the original Baccini et al. 2012 and Avitabile et al. 2015 dataset
#' 
#' @param AGB ABive ground biomass (carbon)
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
AGBtoTotalCarbon <- function(AGB){
  
  BGB <- 0.489 * AGB^0.89
  total.carbon <- (AGB + BGB) / 2
  return(total.carbon)
  
}




#' Continental extents
#'
#' These were just defined by the author for studying different regions of the world.  Maybe also be handy for other people.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
standard.continental.extents <- list(Global = new("SpatialExtent", id = "Global", name = "Global", extent = extent(-180, 180, -90, 90)),
                                     Africa = new("SpatialExtent", id = "Africa", name = "Africa", extent =  extent(-20, 55, -30, 36)),
                                     Europe = new("SpatialExtent", id = "Europe", name = "Europe", extent =  extent(-30, 40, 36, 70)),
                                     Asia = new("SpatialExtent", id = "Asia", name = "Asia", extent =  extent(40, 180, -10, 80)),
                                     NorthAmerica = new("SpatialExtent", id = "NorthAmerica", name = "North America", extent =  extent(-170, -70, 25, 75)),
                                     SouthAmerica = new("SpatialExtent", id = "SouthAmerica", name = "South America", extent = extent(-180, -50, -60, 25)),
                                     Australia = new("SpatialExtent", id = "Australia", name = "Australia", extent = extent(110, 160, -45 ,10)),
                                     Mediterranean = new("SpatialExtent", id = "Med", name = "Mediterranean", extent = extent(10, 40, 28 ,48)),
                                     CentralAsia = new("SpatialExtent", id = "CentralAsia", name = "Central Asia", extent = extent(25, 140, 40, 55)),
                                     SouthEastAsia = new("SpatialExtent", id = "SouthEastAsia", name = "South East Asia", extent = extent(90, 140, 10, 40)),
                                     CentralNorthAmerica = new("SpatialExtent", id = "CentralNorthAmerica", name = "Central North America", extent = extent(-110, -85, 30, 50)),
                                     Boreal = new("SpatialExtent", id = "Boreal", name = "Boreal", extent = extent(-180, 180, 60, 90)),
                                     NHAfrica = new("SpatialExtent", id = "NHAfrica", name = "Northern Hemisphere Africa", extent = extent(-20, 50, 0, 25)),
                                     SHAfrica = new("SpatialExtent", id = "SHAfrica", name = "Southern Hemisphere Africa", extent = extent(5, 50, -30, 0))
                                     
)




## define for older R versions, where this function does not exist.
## Just copied from a recent R version
if (!exists("OlsonNames")) {
  OlsonNames <- function()
  {
    if (.Platform$OS.type == "windows")
      tzdir <- Sys.getenv("TZDIR", file.path(R.home("share"),
                                             "zoneinfo"))
    else {
      tzdirs <- c(Sys.getenv("TZDIR"), file.path(R.home("share"),
                                                 "zoneinfo"), "/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
                  "/usr/lib/zoneinfo", "/usr/local/etc/zoneinfo", "/etc/zoneinfo",
                  "/usr/etc/zoneinfo")
      tzdirs <- tzdirs[file.exists(tzdirs)]
      if (!length(tzdirs)) {
        warning("no Olson database found")
        return(character())
      }
      else tzdir <- tzdirs[1]
    }
    x <- list.files(tzdir, recursive = TRUE)
    grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", x, value = TRUE)
  }
}

