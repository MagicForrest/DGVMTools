
#' Make linear fit equation string
#' 
#' Makes a string (form: y = ax + b, r^2 = r^2) for putting on plots from a linear model (lm)
#' 
#' @param linear.model An object of class lm, should have been made with a simple \code{y ~ x} formula
#' 
#' @details
#' Make sure the model is \code{y ~ x} or this function doesn't really make sense 
#'
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A character string

lm_eqn <- function(linear.model) {
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(stats::coef(linear.model)[1], digits = 2), 
                        b = format(stats::coef(linear.model)[2], digits = 2), 
                        r2 = format(summary(linear.model)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


#' Calculate Normalised Mean Error
#' 
#' Calculates NME between two datasets (represented as two equally sized numeric vectors) 
#' 
#' @param vector1 A numeric vector of data
#' @param vector2 A numeric vector of data (same size as vector2)
#' 
#' @details  No check currently done on vector lengths
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A numeric
calcNME <- function(vector2, vector1) {
  return( sum(abs(vector2 - vector1), na.rm=TRUE) / sum(abs(vector2 - mean(vector2)), na.rm=TRUE)) 
}


#' Calculate Nash-Sutcliff Model Efficiency
#' 
#' Calculates Nash-Sutcliff Model Efficiency between two datasets (represented as two equally sized numeric vectors) 
#' 
#' @param vector1 A numeric vector of data
#' @param vector2 A numeric vector of data (same size as vector2)
#' 
#' @details  No check currently done on vector lengths. See:
#' Nash, J. E. and J. V. Sutcliffe (1970), River flow forecasting through conceptual models part I - A discussion of principles, Jounral of Hydrology, 10 (3), 282-290.
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A numeric
calcNashSutcliffe <- function(vector2, vector1) {
  return( 1 -  (sum((vector2 - vector1)^2, na.rm=TRUE) / length(vector2)) / stats::var(vector2) )
}



#' Compare continuous data
#' 
#' Compares two datasets of continuous data. Specifically calculates and returns a Statistics object (which contains many metrics) given numeric two vectors of equal size 
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containg the data to be compared
#' @param layers1 A character string giving the first layer to compare (should be a column in x)
#' @param layers2 A character string giving the second layer to compare (should be a column in x)
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{compareLayers()}
#' @param verbose A logical, if TRUE print out all the metric scores
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
continuousComparison <- function(x, layers1, layers2, additional, verbose = TRUE){
  
  # check the layers are present
  if(!layers1 %in% layers(x)) stop("Argument layers1 is not a column in x")
  if(!layers2 %in% layers(x)) stop("Argument layers2 is not a column in x")
  
  ###  STANDARD PACKAGE BENCHMARKS WHICH CAN RUN SIMPLY ON TWO VECTORS
  
  # Preamble - extract vectors and remove NAs from both vectors 
  vector1 <- x[[layers1]]
  vector2 <- x[[layers2]]
  
  # first remove where there are NAs in vector1
  vector2 <- vector2[!is.na(vector1)]
  vector1 <- vector1[!is.na(vector1)]
  # now for vector2
  vector1 <- vector1[!is.na(vector2)]
  vector2 <- vector2[!is.na(vector2)]
  
  difference.vector <- vector1 - vector2
  
  # ME and NME 
  ME <- mean(abs(vector2 - vector1))
  NME <- calcNME(vector2, vector1)
  
  # MSE, RMSE, NMSE
  MSE <- mean(difference.vector^2, na.rm=TRUE)
  NMSE <- MSE / mean((vector2 - mean(vector2))^2)
  RMSE <- MSE^0.5
  
  # r2_eff - model efficiency
  r2_eff <- calcNashSutcliffe(vector2, vector1)
  
  # Pearson product moment correlation coefficient, and then R^2
  r <- stats::cor(vector1, vector2, method = "pearson")
  r2 <- r^2
  
  
  stats <- list("ME" = ME, 
                "NME" = NME,
                "NMSE" = NMSE,
                "RMSE" = RMSE,
                "r2" = r2, 
                "r2_eff" = r2_eff,
                "r" = r)
  
  
  ##### HERE DO CUSTOM BENCHMARKS
  if(length(additional) > 0) {
    for(counter in 1:length(additional)) {
      stats[[names(additional)[counter]]] <- additional[[counter]](x, layers1, layers2) 
    }
  }
  
  
  if(verbose) {
    
    print(paste("+++ Stats for", paste(layers1, sep = ","), "vs",  paste(layers2, sep = ","),  "+++", sep = " "))
    for(counter in 1:length(stats)) {
      
      stat.val <- stats[[counter]]
      stat.name <- names(stats)[counter]
      
      if(length(stat.val) == 1) {
        
        # also give a little more info for the standard metrics (ie their full name) before printing
        if(stat.name == "ME") stat.name <- "ME (Mean Error)"
        else if(stat.name == "NME") stat.name <- "NME (Normalised Mean Error)"
        else if(stat.name == "NMSE") stat.name <- "NMSE (Normalised Mean Square Error)"
        else if(stat.name == "RMSE") stat.name <- "RMSE (Root Mean Squared Error)"
        else if(stat.name == "r2_eff") stat.name <- "r2_eff (Nash-Sutcliffe Model Efficiency)"
        else if(stat.name == "r2") stat.name <- "r2 (Coefficient of Determination)"
        else if(stat.name == "r") stat.name <- "r (Pearson's PMCC)"
        print(paste(stat.name,  "=", round(stat.val, 4), sep = " "))
        
      }
      else {
        
        # here print each sub value of the metric
        print(paste0(stat.name, ":"))
        for(counter2 in 1:length(stat.val)) {
          sub.stat.val <- stat.val[[counter2]]
          sub.stat.name <- names(stat.val)[[counter2]]
          print(paste("  ", sub.stat.name,  "=", round(sub.stat.val, 4), sep = " "))
        }
        
      }
    }
  }
  
  return(stats)
  
}

#' Compare relative proportions data
#' 
#' Compares two datasets of relative proportions of multiple classes (sum of classes equals one at each point) where the total for each  data where the totally value for each. 
#' Specifically calculates and returns a list with the Manhattan Metric (MM) and Square Chord Distance (SCD).
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containg the data to be compared
#' @param layers1 A vector of character strings giving the layers from the first dataset to compare (should be columns in x and sum to 1 or 100)
#' @param layers1 A vector of character strings giving the layers from the second dataset to compare (should be columns in x and sum to 1 or 100)
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{compareLayers()}
#' @param verbose A logical, if TRUE print out all the metric scores
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
proportionsComparison <- function(x, layers1, layers2, additional, verbose = TRUE){
  
  # check the layers are present
  if(!sum(layers1 %in% names(x)) == length(layers1)) stop("Some of argument layers1 are not a column in x")
  if(!sum(layers2 %in% names(x)) == length(layers2)) stop("Some of argument layers2 are not a column in x")
  
  dt1 <- x[, layers1]
  dt2 <- x[, layers2]
  
  # check the incoming data.tables are the same size
  if(ncol(dt1) != ncol(dt2)) stop("Trying to compare proportions (Manhattan Metric and Square Chord Distance) with different number of components")
  
  # check the incoming data.tables are the same size
  if(nrow(dt1) != nrow(dt2)) stop("Trying to compare proportions (Manhattan Metric and Square Chord Distance) with different number of rows")
  
  # quick fix, divide by 100 if max > 1.01
  dt1.sum <- rowSums(dt1)
  dt2.sum <- rowSums(dt2)
  if(max(dt1.sum) > 1.01 || max(dt2.sum) > 1.01){
    dt1 <- dt1/100
    dt2 <- dt2/100
  }
  
  # calculate Manhattan Metric and Squared Chord Distance
  MM <- 0
  SCD <- 0
  for(layer.index in 1:ncol(dt1)){
    
    
    # for Manhattan Metric
    difference.vector <- abs(dt1[[layer.index]] - dt2[[layer.index]])
    MM <- MM + sum(difference.vector)
    
    # for Square Chord Distance
    difference.vector <- ((dt1[[layer.index]])^0.5 - (dt2[[layer.index]])^0.5)^2
    SCD <- SCD + sum(difference.vector)
    
  }
  
  MM <- MM/nrow(dt1)
  SCD <- SCD/nrow(dt1)
  
  
  stats <- list("MM" = MM, 
                "SCD" = SCD
  )
  
  ##### HERE DO CUSTOM BENCHMARKS
  if(length(additional) > 0) {
    for(counter in 1:length(additional)) {
      stats[[names(additional)[counter]]] <- additional[[counter]](x, layers1, layers2) 
    }
  }
  
 
  if(verbose) {
    
    print(paste("+++ Stats for", paste(layers1, sep = ","), "vs",  paste(layers2, sep = ","),  "+++", sep = " "))
    for(counter in 1:length(stats)) {
      
      stat.val <- stats[[counter]]
      stat.name <- names(stats)[counter]
      
      if(length(stat.val) == 1) {
        
        # also give a little more info for the standard metrics (ie their full name) before printing
        if(stat.name == "MM") stat.name <- "MM (Manhattan Metric)"
        else if(stat.name == "SCD") stat.name <- "SCD (Square Chord Distance)"
        print(paste(stat.name,  "=", round(stat.val, 4), sep = " "))
        
      }
      else {
        
        # here print each sub value of the metric
        print(paste0(stat.name, ":"))
        for(counter2 in 1:length(stat.val)) {
          sub.stat.val <- stat.val[[counter2]]
          sub.stat.name <- names(stat.val)[[counter2]]
          print(paste("  ", sub.stat.name,  "=", round(sub.stat.val, 4), sep = " "))
        }
        
      }
    }
  }
  
  
  return(stats)
  
}

#' Comparison between two datasets of categorical variables
#' 
#' Calculates a Statistics object (which contains the Cohen's Kappa scores) given data.table of with two layers of categorical data (eg. biomes, land cover classes).
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containg the data to be compared
#' @param layers1 A character string giving the first layer to compare (should be a column in x)
#' @param layers2 A character string giving the second layer to compare (should be a column in x)
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{compareLayers()}
#' @param verbose A logical, if TRUE print out all the Kappa scores
#' 
#' Note that there are many other slots in a Statistics object which will not be filled in the resulting object because they are for continuous as opposed to categorical data
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
categoricalComparison<- function(x, layers1, layers2, additional, verbose = TRUE){
  
  
  dataset1 = dataset2 = code = NULL
  
  # check the layers are present
  if(!layers1 %in% layers(x)) stop("Argument layers1 is not a column in x")
  if(!layers2 %in% layers(x)) stop("Argument layers2 is not a column in x")
  ###  STANDARD PACKAGE BENCHMARKS WHICH CAN RUN SIMPLY ON TWO VECTORS
  
  # Preamble - extract vectors and remove NAs from both vectors 
  vector1 <- x[[layers1]]
  vector2 <- x[[layers2]]
  
  # first remove where there are NAs in vector1
  vector2 <- vector2[!is.na(vector1)]
  vector1 <- vector1[!is.na(vector1)]
  # now for vector2
  vector1 <- vector1[!is.na(vector2)]
  vector2 <- vector2[!is.na(vector2)]
  
  # make a factor with all possibilities (ie from both vector)
  all.factors <- factor(append(as.character(vector1), as.character(vector2)))
  
  # as numerics
  all.factors.numerics <- as.numeric(all.factors)
  
  # mangle that into a data.table
  dt.local <- data.table(x = all.factors.numerics[1:(length(all.factors.numerics)/2)], y = all.factors.numerics[((length(all.factors.numerics)/2)+1):length(all.factors.numerics)])
  
  # labels 
  labels <- as.numeric(unique(all.factors))
  names(labels) <- as.character(unique(all.factors))
  
  # list of unique classes
  both.vector <- as.numeric(as.character(append(vector1, vector2)))
  unique.classes <- unique(all.factors.numerics)
  unique.classes <- sort(unique.classes)
  unique.class.ids <- seq(unique.classes[1], unique.classes[length(unique.classes)])
  
  
  
  # assign each gridcell a code based on the classifications and get the frequency table       
  setnames(dt.local, c("dataset1", "dataset2"))
  dt.local[, code := 1000*dataset1 + dataset2]
  freq.table <- as.data.frame(table(dt.local[,code]))
  
  # make the empty kappa matrix
  kappa.matrix <- matrix(data = 0, nrow = length(unique.class.ids), ncol = length(unique.class.ids))
  
  # loop through all the entries of the matrix for the kappa calculation
  for(counter.Y in 1:length(unique.class.ids)){
    for(counter.X in 1:length(unique.class.ids)){
      
      # find the element in the freq table corresponding the position in the matrix
      position.code = 1000*counter.X + counter.Y
      position.row <- which(freq.table$Var1 == position.code)
      
      # if it exists in the table stick it in the matric
      if(length(position.row > 0)){ kappa.matrix[counter.Y, counter.X] <-  freq.table$Freq[position.row]   }
      
    }
  }
  
  # now we have the kappa matrix, calculate the Cohen's Kappa
  # handy sums
  col.totals <- colSums(kappa.matrix)
  row.totals <- rowSums(kappa.matrix)
  ngridcells <- sum(colSums(kappa.matrix))
  
  # total agreement
  total.agreement <- sum(diag(kappa.matrix))
  
  # chance agreement
  chance.agreement <- 0
  for(counter in 1:length(unique.class.ids)){
    chance.agreement <- chance.agreement + (col.totals[counter] * row.totals[counter] / ngridcells)
  }
  
  #finally kappa
  kappa <- (total.agreement - chance.agreement) / (ngridcells - chance.agreement)
  
  if(is.null(labels)) labels <- paste("Class", 1:length(unique.class.ids), " ")
  
  # also calculate the per class agreement (This is ripped from TH Kappa script, I don't understand it but it gives identical results)
  per.class.kappa <- c()
  for(counter in 1:length(unique.class.ids)){
    a  <- kappa.matrix[counter, counter] / ngridcells
    p1 <- row.totals[counter] / ngridcells
    p2 <- col.totals[counter] / ngridcells
    q1 <- 1.0-p1
    q2 <- 1.0-p2
    b  <- p1-a
    c  <- p2-a
    d  <- q1-c
    #per.class.kappa <- append(per.class.kappa, (a - p1*p2)/( (p1+p2)/2.0 -p1*p2))
    per.class.kappa[[names(labels[which(labels == counter)])]] <- (a - p1*p2)/( (p1+p2)/2.0 -p1*p2)
    
  }
  
  stats = list( "Kappa" = kappa, 
                "Individual Kappas" = per.class.kappa)
  
  
  ##### HERE DO CUSTOM BENCHMARKS
  if(length(additional) > 0) {
    for(counter in 1:length(additional)) {
      stats[[names(additional)[counter]]] <- additional[[counter]](x, layers1, layers2) 
    }
  }
  
  
  if(verbose) {
    
    print(paste("+++ Stats for", paste(layers1, sep = ","), "vs",  paste(layers2, sep = ","),  "+++", sep = " "))
    for(counter in 1:length(stats)) {
      
      stat.val <- stats[[counter]]
      stat.name <- names(stats)[counter]
      
      if(length(stat.val) == 1) {
       
        # also give a little more info for the standard metrics (ie their full name) before printing
        if(stat.name == "Kappa") stat.name <- "Kappa (Overall Cohen's Kappa)"
        print(paste(stat.name,  "=", round(stat.val, 4), sep = " "))
        
      }
      else {
        
        # here print each sub value of the metric
        print(paste0(stat.name, ":"))
        for(counter2 in 1:length(stat.val)) {
          sub.stat.val <- stat.val[[counter2]]
          sub.stat.name <- names(stat.val)[[counter2]]
          print(paste("  ", sub.stat.name,  "=", round(sub.stat.val, 4), sep = " "))
        }
        
      }
    }
  }
  
  
  return(stats)
  
  
}
