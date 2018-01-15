#' Convert a Field, DataObject or data.table to a matrix
#' 
#' This function is useful for making a matrix of data (particularly for writing to disk as a netCDF file) whilst maintain the original longitudes and latitudes.  
#' The longitudes and latitudes are stored as the column and row names respectively. 
#' This is necessary because making a Raster objects converts unevenly spaced coordinates to evenly spaced ones.
#' 
#' @param input The input data, either as a Field, DataObject or data.table
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
  if(is.Field(input)) input <- input@data
  
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

