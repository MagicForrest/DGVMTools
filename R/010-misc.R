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
  if (all(!is.finite(c(a@xmin, b@xmin, a@xmax, b@xmax, 
                       a@ymin, b@ymin, a@ymax, b@ymax))))
    return(TRUE)
  if (a@xmin==b@xmin && a@xmax==b@xmax && 
      a@ymin==b@ymin && a@ymax==b@ymax)
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
    if (object@objects[[n]]@spatial.aggregate.method) {
      temporal[[n]]=list(name=n,
                         description=object@objects[[n]]@quant@name,
                         units=object@objects[[n]]@quant@units,
                         colnames=colnames(object@objects[[n]]@data))
    } else if (object@objects[[n]]@temporal.aggregate.method) {
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
#' @param subset.extent  A raster::Extent or a SpatialExtent object specifying the geograpical sub-domain required
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
  
  
  gridlist <- utils::read.table(gridlist.file, header = header)
  names(gridlist )  <- c("Lon", "Lat")
  
  print(utils::head(gridlist))
  
  gridlist$Lon <- gridlist$Lon + offset[1]
  gridlist$Lat <- gridlist$Lat + offset[2]
 
  gridlist <- gridlist[which(gridlist$Lon <= subset.extent@xmax 
                     & gridlist$Lon >= subset.extent@xmin 
                     & gridlist$Lat <= subset.extent@ymax 
                     & gridlist$Lat >= subset.extent@ymin), ]
  
  
  gridlist$Lon <- gridlist$Lon - offset[1]
  gridlist$Lat <- gridlist$Lat - offset[2]
  
  if(!is.null(file.name)) utils::write.table(gridlist, file.name, quote = FALSE, row.names = FALSE)
  
  return(gridlist)
  
}





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
  utils::capture.output(print(utils::object.size(x), units = "auto")) })
  obj.size <- napply(names, utils::object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- utils::head(out, n)
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

