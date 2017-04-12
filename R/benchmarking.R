
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
                   list(a = format(coef(linear.model)[1], digits = 2), 
                        b = format(coef(linear.model)[2], digits = 2), 
                        r2 = format(summary(linear.model)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


calcNME <- function(vector2, vector1) {
  return( sum(abs(vector2 - vector1), na.rm=TRUE) / sum(abs(vector2 - mean(vector2)), na.rm=TRUE)) 
}

calcNashSutcliffe <- function(vector2, vector1) {
  return( 1 -  (sum((vector2 - vector1)^2, na.rm=TRUE) / length(vector2)) / stats::var(vector2) )
}

calcR2 <- function(vector2, vector1) {
  return( sum( (vector1 - mean(vector1)) * (vector2 - mean(vector2)) )^2 / (sum( (vector1 - mean(vector1))^2 ) * sum( (vector2 - mean(vector2)) ^2)) )
}


continuousComparison <- function(vector1, vector2, name1, name2, verbose = TRUE){
  
  # Preamble - remove NAs from both vectors 
  
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

  # R2 - coefficient of determination
  R2 <- calcR2(vector2, vector1)
  
  # R2eff - model efficiency
  R2.eff <- calcNashSutcliffe(vector2, vector1)
  
  # Pearson product moment correlation coefficient
  P.cor <- cor(vector1, vector2, method = "pearson")
  
  if(verbose) {
    print(paste("+++ Stats for", name1, "vs",  name2,  "+++", sep = " "))
    print(paste("Mean Error (ME) = ", round(ME, 4)))
    print(paste("Normalised Mean Error (NME) = ", round(NME, 4)))
    print(paste("Mean Squared Error (MSE) = ", round(MSE, 4)))
    print(paste("Normalised Mean Squared Error (NMSE) = ", round(NMSE, 4)))
    print(paste("Root Mean Squared Error (RMSE) = ", round(RMSE, 4)))
    print(paste("Coefficient of Determiantion (R^2) = ", round(R2, 4)))
    print(paste("Nash-Sutcliffe Model Efficiency (R^2_eff) = ", round(R2.eff, 4)))
    print(paste("Pearson's PMCC (r) = ", round(P.cor, 4)))
  }
  
  stats <- new("SpatialComparison",
               id = paste(name1, "vs",  name2,  sep = "."),
               R2 = R2, 
               R2.eff = R2.eff,
               P.cor = P.cor,
               ME = ME, 
               NME = NME,
               NMSE = NMSE,
               RMSE = RMSE
  )
  
  
  return(stats)
  
}


proportionsComparison <- function(dt1, dt2, name1, name2, verbose = TRUE){
  
  # check the incoming data.tables are the same size
  if(ncol(dt1) != ncol(dt1)) stop("Trying to compare proportions (Manhattan Metric and Square Chord Distance) with different number of components")
  
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
  
  if(verbose) {
    print(paste("+++ Stats for", name1, "vs",  name2,  "+++", sep = " "))
    print(paste("Manhattan Metric (MM) = ", round(MM, 4)))
    print(paste("Squared Chord Distance (NME) = ", round(SCD, 4)))
  }
  
  stats <- new("SpatialComparison",
               id = paste(name1, "vs",  name2,  sep = "."),
               MM = MM, 
               SCD = SCD
  )
  
  
  return(stats)
  
}

#' Kappa comparison between two biome rasters
#' 
#' Calculates a SpatialComparison object (which contains the Cohen's Kappa scores) given a stack containing two maps of categorical data (eg. biomes, land cover classes).
#' 
#' @param dt A two-columned data.table containing the biomes (or other categorical data) represented as integer codes
#' @param x Character string giving the column name of the first of the datasets to be compared
#' @param y Character string giving the column name of the second of the datasets to be compared
#' @param id A character string to identify this comparison, typically a combination the id of the biome scheme and the id of the vegetation model run.
#' @param labels A vector of character strings to describe the categories over which Kappa is compared (typically a list of biomes)
#' following the order of the integer codes used in the data (see \code{stack} argument)
#' @param verbose A logical, if TRUE print out all the Kappa scores
#' 
#' Note that there are many other slots in a SpatialComparison object which will not be filled in the resulting object because they are for continuous as opposed to categorical data
#' 
#' @return A spatial comparison object
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import raster
#' @export    
#' stats <- categoricalComparison(vector1 = vector1, vector2 = vector2, name1 = info1@name, name2 = info2@name, verbose = verbose)

categoricalComparison<- function(vector1,
                                 vector2, 
                                 name1, 
                                 name2, 
                                 verbose = TRUE){
  
  
  dataset1 = dataset2 = code = NULL
  
  # Old south order new nothern horizon
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
    
    if(verbose) print(paste(names(labels[which(labels == counter)]), round(per.class.kappa[counter], 3), sep = " "))
  }
  
  if(verbose) print(paste("Overall Kappa", round(kappa, 3), sep = " "))
  
  
  return(new("SpatialComparison",
             id = paste(name1, "vs",  name2,  sep = "."),
             Kappa = kappa, 
             individual.Kappas = per.class.kappa)
  )
  
  
}
