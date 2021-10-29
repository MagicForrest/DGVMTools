
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
#' @param obs A numeric vector of observed values
#' @param mod A numeric vector of modelled values (same size as obs)
#' @param area A numeric vector of the areas by which to weight the values (same size as obs) 
#' 
#' @details  No check currently done on vector lengths
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A numeric
calcNME <- function(mod, obs, area) {
  
  if(missing(area) || is.null(area))   return( sum(abs(mod - obs), na.rm=TRUE) / sum(abs(obs - mean(obs)), na.rm=TRUE)) 
  else {
    return( sum(abs(mod - obs) * area, na.rm=TRUE) / sum(abs(obs - mean(obs)) * area, na.rm=TRUE) ) 
  }
}


#' Calculate Normalised Mean Square Error
#' 
#' Calculates NMSE between two datasets (represented as two equally sized numeric vectors) 
#' 
#' @param mod A numeric vector of observed values
#' @param obs A numeric vector of modelled values (same size as mod)
#' @param area A numeric vector of the areas by which to weight the values (same size as obs) 
#' 
#' @details  No check currently done on vector lengths
#' 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @return A numeric
calcNMSE <- function(mod, obs, area) {
  
  if(missing(area) || is.null(area)) return( sum((mod - obs)^2, na.rm=TRUE) / sum((obs - mean(obs))^2 , na.rm=TRUE) ) 
  else return( sum((mod - obs)^2  * area, na.rm=TRUE) / sum((obs - mean(obs))^2 * area, na.rm=TRUE)) 
  
}



#' Compare continuous data
#' 
#' Compares two datasets of continuous data. Specifically calculates and returns a Statistics object (which contains many metrics) given numeric two vectors of equal size 
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containg the data to be compared
#' @param layers1 A character string giving the first layer to compare (should be a column in x).  For the normalised metrics, this is the *modelled* values.
#' @param layers2 A character string giving the second layer to compare (should be a column in x).  For the normalised metrics, this is the *observed* values.
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{compareLayers()}
#' @param verbose A logical, if TRUE print out all the metric scores
#' @param area A logical, if true weight the metrics by gridcell area
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
continuousComparison <- function(x, layers1, layers2, additional, verbose = TRUE, area = TRUE){
  
  # check the layers are present
  if(!layers1 %in% layers(x)) stop("Argument layers1 is not a column in x")
  if(!layers2 %in% layers(x)) stop("Argument layers2 is not a column in x")
  
  # add the area if selected
  if(area) {
    
    x.dims <- getDimInfo(x)
    
    if(!"Lon" %in% x.dims ||  !"Lat" %in% x.dims) {
      warning("Comparison stats will not be weighted because Lon/Lat not present")
      area.vec <- NULL
    } 
    else {
      x <- addArea(x, unit = "km^2")
      area.vec <- x[["Area"]]
    }
    
    
  }
  else area.vec <- NULL
  
  
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
  
  
  #### KELLEY ET AL 2013 METRICS
  
  # Unnormalised metrics:  ME, MSE and RSME
  if(is.null(area.vec)) {
    ME <- mean(abs(vector1 - vector2))
    MSE <- mean((vector1 - vector2)^2, na.rm=TRUE)
  }
  else {
    ME <- sum(abs(vector1 - vector2) * area.vec, na.rm=TRUE) / sum(area.vec)
    MSE <- sum((vector1 - vector2)^2 * area.vec, na.rm=TRUE) / sum(area.vec)
  }
  RMSE <- MSE^0.5
  
  # Normalised metrics: NME and NMSE (step 1)
  NME <- calcNME(mod = vector1, obs = vector2, area = area.vec)
  NMSE <- calcNMSE(mod = vector1, obs = vector2, area = area.vec)
  
  # and step 2 for NME and NMSE
  vector1_step2 <- vector1 - mean(vector1)
  vector2_step2 <- vector2 - mean(vector2)
  NME_2 <- calcNME(mod = vector1_step2, obs = vector2_step2, area = area.vec)
  NMSE_2 <- calcNMSE(mod = vector1_step2, obs = vector2_step2, area = area.vec)
  
  # and step 3 for NME and NMSE
  vector1_step3_NME <- vector1_step2 / sum(abs(vector1_step2 - mean(vector1_step2)))/ length(vector1_step2)
  vector2_step3_NME <- vector2_step2 / sum(abs(vector2_step2 - mean(vector2_step2)))/ length(vector2_step2)
  NME_3 <- calcNME(mod = vector1_step3_NME, obs = vector2_step3_NME, area = area.vec)
  
  vector1_step3_NMSE <- vector1_step2 / stats::var(vector1_step2)
  vector2_step3_NMSE <- vector2_step2 / stats::var(vector2_step2)
  NMSE_3 <- calcNMSE(mod = vector1_step3_NMSE, obs = vector2_step3_NMSE, area = area.vec)
  
  
  #### MORE 'STANDARD' METRICS MORE BASED ON LINEAR REGRESSION AND NOT FOCUSSED ON MODEL-OBSERVATION COMPARISON
  
  if(!is.null(area.vec)) {
    message("NOTE: metrics r, r2, m, and c are NOT weighted by grdicell area, the other metrics are.")
    warning("NOTE: metrics r, r2, m, and c are NOT weighted by grdicell area, the other metrics are.")
    
  }
  
  # r2_eff - Nash-Sutcliffe model efficiency (actually is focussed on model-obs Comparisons)
  r2_eff <- 1 - NMSE
  
  # Pearson product moment correlation coefficient, and then R^2
  # MF: not the best thing in my opinion
  r <- stats::cor(vector1, vector2, method = "pearson", )
  r2 <- r^2
  
  # calculate a simple linear regression 
  simple.regression <- stats::lm(formula = mod ~ obs, data = data.frame("mod" = vector1, "obs" = vector2))
  c <- stats::coef(simple.regression)[1]
  m <- stats::coef(simple.regression)[2]
  
  
  stats <- list("ME" = ME, 
                "NME" = NME,
                "NMSE" = NMSE,
                "RMSE" = RMSE,
                "NME_2" = NME_2,
                "NMSE_2" = NMSE_2,
                "NME_3" = NME_3,
                "NMSE_3" = NMSE_3,
                "r2_eff" = r2_eff,
                "r" = r,
                "r2" = r2, 
                "m" = m,
                "c" = c
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
        if(stat.name == "ME") stat.name <- "ME (Mean Error)"
        else if(stat.name == "NME") stat.name <- "NME (Normalised Mean Error)"
        else if(stat.name == "NMSE") stat.name <- "NMSE (Normalised Mean Square Error)"
        else if(stat.name == "RMSE") stat.name <- "RMSE (Root Mean Squared Error)"
        else if(stat.name == "NME_2") stat.name <- "NME_2 (NME with mean removed)"
        else if(stat.name == "NMSE_2") stat.name <- "NMSE_2 (NMSE with mean removed)"
        else if(stat.name == "NME_3") stat.name <- "NME_3 (NME with mean and variance removed)"
        else if(stat.name == "NMSE_3") stat.name <- "NMSE_3 (NMSE with mean and variance removed)"
        else if(stat.name == "r2_eff") stat.name <- "r2_eff (Nash-Sutcliffe Model Efficiency)"
        else if(stat.name == "r2") stat.name <- "r2 (Coefficient of Determination)"
        else if(stat.name == "r") stat.name <- "r (Pearson's PMCC)"
        else if(stat.name == "m") stat.name <- "m (gradient of linear fit mod = m * obs + c)"
        else if(stat.name == "c") stat.name <- "c (intercept of linear fit mod = m * obs + c)"
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
#' @param area A logical, if true weight the metrics by gridcell area (not implemented)
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
proportionsComparison <- function(x, layers1, layers2, additional, verbose = TRUE, area = TRUE){
  
  if(area) {
    message("Gridcell area weighting not currently implemented for proportionsComparison")
    warning("Gridcell area weighting not currently implemented for proportionsComparison")
  }
  
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
#' @param area A logical, if true weight the metrics by gridcell area (not currently implemented)
#' 
#' Note that there are many other slots in a Statistics object which will not be filled in the resulting object because they are for continuous as opposed to categorical data
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
categoricalComparison<- function(x, layers1, layers2, additional, verbose = TRUE,  area = TRUE){
  
  if(area) {
    message("Gridcell area weighting not currently implemented for categoricalComparison")
    warning("Gridcell area weighting not currently implemented for categoricalComparison")
  }
  
  
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
#' Compare seasonal data
#' 
#' Compares two datasets of monthly continuous data. Specifically calculates and returns a Statistics object (which contains many metrics) given numeric two vectors of equal size 
#' 
#' @param x A data.table containing the spatial-temporal-annual columns and two columns containg the data to be compared
#' @param layers1 A character string giving the first layer to compare (should be a column in x).  For the normalised metrics, this is the *modelled* values.
#' @param layers2 A character string giving the second layer to compare (should be a column in x).  For the normalised metrics, this is the *observed* values.
#' @param additional A list of functions define additions metrics, see the custom.metrics argument of \code{compareLayers()}
#' @param verbose A logical, if TRUE print out all the metric scores
#' 
#' @return A named list of metric statistics
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @export    
seasonalComparison <- function(x, layers1, layers2, additional, verbose = TRUE, area = TRUE){
  
  if(area) {
    message("Gridcell area weighting not currently implemented for seasonalComparison")
    warning("Gridcell area weighting not currently implemented for seasonalComparison")
  }
  
  
  C_1 = C_2 = L_x_1 = L_x_2 = L_y_1 = L_y_2 = Lat = Lon = Month = P_1 = P_2 = Sigma_x_1 = Sigma_x_2 = Theta_t = NULL
  
  # get the non-monthy dimensions
  all.dims <- getDimInfo(x)
  if(!"Month" %in% all.dims) stop ("Something went wrong, seasonalComparison() was called with data  with no 'Month' dimension")
  non.month.dims <- all.dims[which(!all.dims == "Month")]
  
  #### FIRST CALCULATE THE SEASONAL CONCENTRATION AND PHASE
  ##   Equations numbers refer to Kelley et al 2013 Biogeosciences
  
  #  calculate the theta_t, L_x and L_y
  theta_t <- 2 * pi * (1:12 -1) /12  # equation (5)
  x[, Theta_t := theta_t[Month]] # apply equation (5)
  x[, L_x_1 := get(layers1) * cos(Theta_t) ] # equation (6) without summation
  x[, L_y_1 := get(layers1) * sin(Theta_t) ] # equation (6) without summation
  x[, L_x_2 := get(layers2) * cos(Theta_t) ] # equation (6) without summation
  x[, L_y_2 := get(layers2) * sin(Theta_t) ] # equation (6) without summation
  
  # now sum over all months - the summations from equation (6), but also make the denominator from equation(7)
  x.summed <-  x[, j=list(L_x_1 = sum(L_x_1), L_y_1 = sum(L_y_1), L_x_2 = sum(L_x_2), L_y_2 = sum(L_y_2), Sigma_x_1 = sum(get(layers1)), Sigma_x_2 = sum(get(layers2))), by=non.month.dims]
  
  # remove where either Sigma == 0
  x.summed <- x.summed[ Sigma_x_1 != 0 & Sigma_x_2 != 0]
  
  # concentration and phase equations (7) and (8)
  x.summed[, C_1 := (L_x_1^2 + L_y_1^2) ^0.5 / Sigma_x_1]
  x.summed[, C_2 := (L_x_2^2 + L_y_2^2) ^0.5 / Sigma_x_2]
  x.summed[, P_1 := atan2(L_x_1,L_y_1)]
  x.summed[, P_2 := atan2(L_x_2,L_y_2)]
  
  
  #### KELLEY ET AL 2013 METRICS
  
  #### CONCENTRATION
  # code essentially ripped from continuousComparison above
  
  # Preamble - extract vectors and remove NAs from both vectors 
  vector1 <- x.summed[["C_1"]]
  vector2 <- x.summed[["C_2"]]
  
  # first remove where there are NAs in vector1
  vector2 <- vector2[!is.na(vector1)]
  vector1 <- vector1[!is.na(vector1)]
  # now for vector2
  vector1 <- vector1[!is.na(vector2)]
  vector2 <- vector2[!is.na(vector2)]
  
  # Normalised metrics: NME and NMSE (step 1)
  NME <- calcNME(mod = vector1, obs = vector2)
  NMSE <- calcNMSE(mod = vector1, obs = vector2)
  
  # and step 2 for NME and NMSE
  vector1_step2 <- vector1 - mean(vector1)
  vector2_step2 <- vector2 - mean(vector2)
  NME_2 <- calcNME(mod = vector1_step2, obs = vector2_step2)
  NMSE_2 <- calcNMSE(mod = vector1_step2, obs = vector2_step2)
  
  # and step 3 for NME and NMSE
  vector1_step3_NME <- vector1_step2 / sum(abs(vector1_step2 - mean(vector1_step2)))/ length(vector1_step2)
  vector2_step3_NME <- vector2_step2 / sum(abs(vector2_step2 - mean(vector2_step2)))/ length(vector2_step2)
  NME_3 <- calcNME(mod = vector1_step3_NME, obs = vector2_step3_NME)
  
  vector1_step3_NMSE <- vector1_step2 / stats::var(vector1_step2)
  vector2_step3_NMSE <- vector2_step2 / stats::var(vector2_step2)
  NMSE_3 <- calcNMSE(mod = vector1_step3_NMSE, obs = vector2_step3_NMSE)
  
  #### PHASE
  # Preamble - extract vectors and remove NAs from both vectors 
  phase1 <- x.summed[["P_1"]]
  phase2 <- x.summed[["P_2"]]
  
  # first remove where there are NAs in phase1
  phase2 <- phase2[!is.na(phase1)]
  phase1 <- phase1[!is.na(phase1)]
  # now for phase2
  phase1 <- phase1[!is.na(phase2)]
  phase2 <- phase2[!is.na(phase2)]
  
  MPD <- (1/pi) * sum(acos (cos(phase1 - phase2))) / length(phase1)
  
  
  #### COMPILE STATS
  stats <- list("NME_conc" = NME,
                "NMSE_conc" = NMSE,
                "NME_conc_2" = NME_2,
                "NMSE_conc_2" = NMSE_2,
                "NME_conc_3" = NME_3,
                "NMSE_conc_3" = NMSE_3,
                "MPD" = MPD
  )
  
  ##### HERE DO CUSTOM METRICS
  if(length(additional) > 0) {
    for(counter in 1:length(additional)) {
      stats[[names(additional)[counter]]] <- additional[[counter]](x, layers1, layers2) 
    }
  }
  
  
  #### PRINT METRICS
  if(verbose) {
    
    print(paste("+++ Stats for", paste(layers1, sep = ","), "vs",  paste(layers2, sep = ","),  "+++", sep = " "))
    for(counter in 1:length(stats)) {
      
      stat.val <- stats[[counter]]
      stat.name <- names(stats)[counter]
      
      if(length(stat.val) == 1) {
        
        # also give a little more info for the standard metrics (ie their full name) before printing
        if(stat.name == "NME_conc") stat.name <- "NME (Normalised Mean Error) of seasonal concentration"
        else if(stat.name == "NMSE_conc") stat.name <- "NMSE (Normalised Mean Square Error) of seasonal concentration"
        else if(stat.name == "NME_conc_2") stat.name <- "NME_2 (NME with mean removed) of seasonal concentration"
        else if(stat.name == "NMSE_conc_2") stat.name <- "NMSE_2 (NMSE with mean removed) of seasonal concentration"
        else if(stat.name == "NME_conc_3") stat.name <- "NME_3 (NME with mean and variance removed) of seasonal concentration"
        else if(stat.name == "NMSE_conc_3") stat.name <- "NMSE_3 (NMSE with mean and variance removed) of seasonal concentration"
        else if(stat.name == "MPD") stat.name <- "MPD (Mean Phase Difference)"
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
  
  # remove intermediate columns before returning
  x.summed[, c("L_x_1", "L_y_1", "L_x_2", "L_y_2", "Sigma_x_1", "Sigma_x_2") := NULL]
  
  # transfer the angular phases back to months for nice plotting ie. map 0 -> January and +/-pi to July
  
  correctPhase<- function(phase) {
    return(ifelse(phase < 0, phase + 12, phase))
  }
  x.summed[, P_1 :=  correctPhase(12 * P_1 / (2 * pi)), ]
  x.summed[, P_2 :=  correctPhase(12 * P_2 / (2 * pi)), ]
  
  
  return(list(dt = x.summed, stats = stats))
  
}
