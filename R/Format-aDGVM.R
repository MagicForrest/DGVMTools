#!/usr/bin/Rscript

############################################################################################################################
############################ FUNCTIONS TO HANDLE aDGVM2 FILES ##############################################################
############################################################################################################################

#' Get a Field for aDGVM
#' 
#' An internal function that reads data from an aDGVM2 run. It actually calls one of two other functions depending on the type of quantity specified.   
#' 
#' @param run A \code{source} containing the meta-data about the aDGVM2 run
#' @param quant A string the define what quantity from the aDGVM2 run to extract
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @param adgvm.scheme A number that defines if pop-files (=1) or trait-files (=2) are used.
#' @param adgvm.daily A logical, set to true to read daily data (only for \code{adgvm.scheme=1} and if daily data are provided in pop file)
#' @return A list containing firstly the data.table containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @seealso \code{\link{getQuantity_aDGVM_Scheme1}, \link{getQuantity_aDGVM_Scheme2}}
getField_aDGVM <- function(source,
                           quant,
                           target.STAInfo,
                           verbose,
                           adgvm.scheme,
                           adgvm.daily) {
  
  # first check that ncdf4 netCDF package is installed
  if (! requireNamespace("ncdf4", quietly = TRUE))  stop("Please install ncdf4 R package and, if necessary the netCDF libraries, on your system to read aDGVM data.")

  if(missing(adgvm.scheme)) adgvm.scheme <- 1
  if(missing(adgvm.daily)) adgvm.daily <- FALSE
  
  if("aDGVM" %in% quant@format | "Standard" %in% quant@format) {
    if(adgvm.scheme == 1) data.list <- getQuantity_aDGVM_Scheme1(source, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, quant, adgvm.daily)
    if(adgvm.scheme == 2) data.list <-(getQuantity_aDGVM_Scheme2(source, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, quant))
#    if(adgvm.scheme == 2) this.dt <- data.table(getQuantity_aDGVM_Scheme2(source, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, quant))
  }
  #else if(quant@format == "Standard") {
  #  stop("Standard quantities nor currently defined for aDGVM")
  #}
  else {
    stop(paste("Quantity", quant@id, "doesn't seem to be defined for aDGVM"))
  }
  
  actual.sta.info = new("STAInfo")
  
  return(data.list)
  
}



##########################################################################
##########################################################################
##########################################################################

#' Read aDGVM2 quantities from pop file
#' 
#' An internal function to read quantities from aDGVM2 pop files. Quantities are provided for trees, C4 grasses and C3 grasses. Trait files are not opened
#' 
#' @param run A \code{source} containing the meta-data about the aDGVM2 run
#' @param first.year First year of data to read
#' @param last.year Last year of data to read
#' @param variable A character string specifying which variable/quantity to get, can be "agb“, "aGPP_std“, "basalarea“, "bgb“, "canopyheight_std“, "LAI_std“, meanheight“, "nind“, "pind“, "vegC_std“, "vegcover_std"
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}, Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @seealso \code{\link{getQuantity_aDGVM_Scheme2}}
getQuantity_aDGVM_Scheme1 <- function(run, variable, first.year, last.year, adgvm.daily)
{
  # To stop NOTES
  Day = Lat = Lon = Month = Time = Year = NULL
  
  fname <- file.path(run@dir, paste("pop_", run@id,".nc", sep=""))
  
  # STAInfo object to summarise the spatial-temporal-annual dimensions of the data
  actual.sta.info <- new("STAInfo")
  
  cat( "Convert", fname, "\n" )
  d <- ncdf4::nc_open(fname)
  
  xx <- ncdf4::ncvar_get(d,"lon")
  yy <- ncdf4::ncvar_get(d,"lat")
  tt <- ncdf4::ncvar_get(d,"time")
  len_tt <- length(tt)

  # select the years we want
  # note that here we are assuming monthing data, so set meta-data to monthly
  # also assume that all years are in the days
  if(adgvm.daily) {
    actual.sta.info@subannual.resolution <- "Day"
    actual.sta.info@subannual.original <- "Day"
    time.steps.per.year <- 365
  }
  else {
    actual.sta.info@subannual.resolution <- "Month"
    actual.sta.info@subannual.original <- "Month"
    time.steps.per.year <- 12
  }
  actual.sta.info@first.year <- run@year.offset
  actual.sta.info@last.year <- run@year.offset + length(tt)/ time.steps.per.year - 1
  
  
  if(length(first.year) == 0) first.year <- actual.sta.info@first.year
  if(length(last.year) == 0) last.year <- actual.sta.info@last.year

  start.point <- ((first.year-actual.sta.info@first.year) * time.steps.per.year) + 1 
  n.years <- last.year - first.year +1
  time.count <-  time.steps.per.year * n.years
  end.point <- start.point + time.count - 1

  dim.names <- list(xx, yy, start.point:end.point)
  
  nc.start.vec.tr <- c(  1, 1, 1, start.point )
  nc.start.vec.g4 <- c(  1, 1, 2, start.point )
  nc.start.vec.g3 <- c(  1, 1, 3, start.point )
  nc.count.vec    <- c( -1,-1, 1, time.count )
  
  # number of years used to calculate fire return interval  
  years_fire <- 20
  
  #if(variable@id == "firefreq" | variable@id == "burntfraction_std" ){
    
  # tmp <- ncdf4::ncvar_get( d, "firecount", start=c( x,y,len_tt-(years_fire*12)+1 ), count=c( 1,1,years_fire*12 ) )
  # fir <- matrix( tmp, ncol=12, byrow=T )[,12]
  # tmp.fi <- min( sum(fir)/length(fir), 1)

  # z <- max(t_seq)
  #         #            Lon      Lat    Year            Tr      C4G     C3G     Total
  # out.vec <- c( xx[x],  yy[y], which(t_seq == z), tmp.fi, tmp.fi, tmp.fi, tmp.fi )
  # out.all <- rbind( out.all, out.vec )
  # }
  
  if(variable@id == "vegcover_std"){
    tmp.tr <- ncdf4::ncvar_get( d, "SumCanopyArea0", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncdf4::ncvar_get( d, "SumCanopyArea0", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncdf4::ncvar_get( d, "SumCanopyArea0", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.g4  <-       tmp.g4/10000*100  # *100 to convert to %
    tmp.g3  <-       tmp.g3/10000*100  # *100 to convert to %
    tmp.tr  <- pmin( tmp.tr/20000*100, 100-(tmp.g4+tmp.g3))  # *100 to convert to %
  }
  
  if(variable@id == "basalarea"){
    tmp.tr <- ncdf4::ncvar_get( d, "SumBasalArea", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncdf4::ncvar_get( d, "SumBasalArea", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncdf4::ncvar_get( d, "SumBasalArea", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "canopyheight_std"){
    tmp.tr <- ncdf4::ncvar_get( d, "MeanHeight", start=nc.start.vec.tr, count=nc.count.vec )
    message(paste("Warning: Canopy height currently equal to mean height!"))
    tmp.g3 <- tmp.tr   # dummy to avoid error message
    tmp.g4 <- tmp.tr   # dummy to avoid error message
  }
  
  if(variable@id == "aGPP_std"){
    tmp.tr <- ncdf4::ncvar_get( d, "MeanCGain",  start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncdf4::ncvar_get( d, "MeanCGain",  start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncdf4::ncvar_get( d, "MeanCGain",  start=nc.start.vec.g3, count=nc.count.vec )
    ind.tr <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.tr, count=nc.count.vec )
    ind.g4 <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.g4, count=nc.count.vec )
    ind.g3 <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr*30.42*ind.tr/2  # 30.42 to scale from monthly to annual, ind. for all plants, 2 for kg->C
    tmp.g4 <- tmp.g4*30.42*ind.g4/2  # 30.42 to scale from monthly to annual, ind. for all plants, 2 for kg->C
    tmp.g3 <- tmp.g3*30.42*ind.g3/2  # 30.42 to scale from monthly to annual, ind. for all plants, 2 for kg->C
  }
  
  if(variable@id == "LAI_std"){
    tmp.tr <- ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.tr, count=nc.count.vec )*ncdf4::ncvar_get( d, "meanSla", start=nc.start.vec.tr, count=nc.count.vec )/10 # division by 10 to scale with stand area and convert t/ha to kg
    tmp.g4 <- ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.g4, count=nc.count.vec )*ncdf4::ncvar_get( d, "meanSla", start=nc.start.vec.g4, count=nc.count.vec )/10 # division by 10 to scale with stand area and convert t/ha to kg
    tmp.g3 <- ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.g3, count=nc.count.vec )*ncdf4::ncvar_get( d, "meanSla", start=nc.start.vec.g3, count=nc.count.vec )/10 # division by 10 to scale with stand area and convert t/ha to kg
    
#    tmp.tr <- ncdf4::ncvar_get( d, "MeanLai", start=nc.start.vec.tr, count=nc.count.vec )
#    tmp.g4 <- ncdf4::ncvar_get( d, "MeanLai", start=nc.start.vec.g4, count=nc.count.vec )
#    tmp.g3 <- ncdf4::ncvar_get( d, "MeanLai", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "meanheight"){
    tmp.tr <- ncdf4::ncvar_get( d, "MeanHeight", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncdf4::ncvar_get( d, "MeanHeight", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncdf4::ncvar_get( d, "MeanHeight", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "pind"){
    tmp.tr <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.g3, count=nc.count.vec )
    
    ind_tot <- tmp.tr+tmp.g4+tmp.g3
    tmp.tr  <- tmp.tr/ind_tot
    tmp.g4  <- tmp.g4/ind_tot
    tmp.g3  <- tmp.g3/ind_tot
  }
  
  if(variable@id == "nind"){
    tmp.tr <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncdf4::ncvar_get( d, "nind_alive", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "agb"){
    tmp.tr <-          ncdf4::ncvar_get( d, "SumBBark", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncdf4::ncvar_get( d, "SumBWood", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <-          ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <-          ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr/20
    tmp.g4 <- tmp.g4/2
    tmp.g3 <- tmp.g3/2
  }
  
  if(variable@id == "bgb"){
    tmp.tr <-          ncdf4::ncvar_get( d, "SumBRoot", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <-          ncdf4::ncvar_get( d, "SumBRoot", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <-          ncdf4::ncvar_get( d, "SumBRoot", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr/20
    tmp.g4 <- tmp.g4/2
    tmp.g3 <- tmp.g3/2
  }
  
  if(variable@id == "vegC_std"){
    tmp.tr <-          ncdf4::ncvar_get( d, "SumBBark", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncdf4::ncvar_get( d, "SumBWood", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncdf4::ncvar_get( d, "SumBStor", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncdf4::ncvar_get( d, "SumBRoot", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncdf4::ncvar_get( d, "SumBRepr", start=nc.start.vec.tr, count=nc.count.vec )
    
    tmp.g4 <-          ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g4 <- tmp.g4 + ncdf4::ncvar_get( d, "SumBRoot", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g4 <- tmp.g4 + ncdf4::ncvar_get( d, "SumBStor", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g4 <- tmp.g4 + ncdf4::ncvar_get( d, "SumBRepr", start=nc.start.vec.g4, count=nc.count.vec )
    
    tmp.g3 <-          ncdf4::ncvar_get( d, "SumBLeaf", start=nc.start.vec.g3, count=nc.count.vec )
    tmp.g3 <- tmp.g3 + ncdf4::ncvar_get( d, "SumBRoot", start=nc.start.vec.g3, count=nc.count.vec )
    tmp.g3 <- tmp.g3 + ncdf4::ncvar_get( d, "SumBStor", start=nc.start.vec.g3, count=nc.count.vec )
    tmp.g3 <- tmp.g3 + ncdf4::ncvar_get( d, "SumBRepr", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr/20
    tmp.g4 <- tmp.g4/2
    tmp.g3 <- tmp.g3/2
  }

  # If simulations were done for single study sites only, the tmp.XX variables
  # are only one-dimensional (time) and not tree-dimensional (x, y, time).
  # Therefore, variables need to be converted into 3d arrays.
  if (length(dim(tmp.tr))==1) {
    tmp.tr   <- array( tmp.tr, c(length(xx), length(yy), length(start.point:end.point) ) )
    tmp.g4   <- array( tmp.g4, c(length(xx), length(yy), length(start.point:end.point) ) )
    tmp.g3   <- array( tmp.g3, c(length(xx), length(yy), length(start.point:end.point) ) )
  }

  dimnames(tmp.tr) <- dim.names
  dimnames(tmp.g4) <- dim.names
  dimnames(tmp.g3) <- dim.names

  # prepare data.table from the slice (array) for each PFT
  tr.dt <- as.data.table(melt(tmp.tr))
  names(tr.dt) <- c("Lon", "Lat", "Time", "Tr")
  setkey(tr.dt, Lon, Lat, Time)
  
  g3.dt <- as.data.table(melt(tmp.g3))
  names(g3.dt) <- c("Lon", "Lat", "Time", "C3G")
  setkey(g3.dt, Lon, Lat, Time)
  
  g4.dt <- as.data.table(melt(tmp.g4))
  names(g4.dt) <- c("Lon", "Lat", "Time", "C4G")
  setkey(g4.dt, Lon, Lat, Time)
  
  # merge the data tables for each PFT into one data.table
  out.all <- tr.dt[g3.dt[g4.dt]]
  
  # sort out the time dimension
  # NOTE: something special is happening here. The ':=' operator is changing the data.table in place.
  # this means that you don't need to do a re-assignment using '<-' 
  out.all[, Year := floor((Time-1)/time.steps.per.year) + run@year.offset ] # - 1]

  if(adgvm.daily) {
    out.all[, Day := ((Time-1) %% time.steps.per.year) + 1]
  }
  else {
    out.all[, Month := ((Time-1) %% time.steps.per.year) + 1]
  }
  out.all[, Time := NULL]
  
  out.all = stats::na.omit(out.all)
  print(out.all)

  # Now that we have the data we can set a spatial.extent
  actual.sta.info@spatial.extent <- extent(out.all)
  
  
  return(list(dt = out.all, 
              sta.info = actual.sta.info))
}




#' Read aDGVM2 quantities from trait file
#' 
#' An internal function to read quantities from aDGVM2 trait files. Quantities are currently provided for raingreen trees, evergreen trees, raingreen shrubs, evergreen shrubs, C4 grasses and C3 grasses. Pop files are not opened.
#' 
#' @param run A \code{source} containing the meta-data about the aDGVM2 run
#' @param first.year First year of data to read
#' @param last.year Last year of data to read
#' @param variable A character string specifying which variable/quantity to get, can be "agb“, "aGPP_std“, "basalarea“, "bgb“, "canopyheight_std“, "LAI_std“, "nind“, "meanheight“, "pind“, "vegC_std“, "vegcover_std"
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}, Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
#' @seealso \code{\link{getQuantity_aDGVM_Scheme1}}
getQuantity_aDGVM_Scheme2 <- function(run,variable, first.year, last.year)
{
  # To stop NOTES
  Day = Lat = Lon = Month = Time = Year = NULL

  fname <- file.path(run@dir, paste("trait_", run@id,".nc", sep=""))

  # STAInfo object to summarise the spatial-temporal-annual dimensions of the data
  actual.sta.info <- new("STAInfo")
  
  cat( "Convert", fname, "\n" )
  d <- ncdf4::nc_open(fname)
  
  xx <- ncdf4::ncvar_get(d,"lon")
  yy <- ncdf4::ncvar_get(d,"lat")
  tt <- ncdf4::ncvar_get(d,"time")
  len_tt <- length(tt)
  max_pop_size <- length(ncdf4::ncvar_get(d,"individual"))          # maximum population size
  
  timestep <- tt[2]-tt[1] # time step in file

  actual.sta.info@first.year <- run@year.offset
  actual.sta.info@last.year <- run@year.offset + (length(tt)-1)*timestep 
  
  if(length(first.year) == 0) first.year <- actual.sta.info@first.year
  if(length(last.year) == 0) last.year <- actual.sta.info@last.year
  
  actual.sta.info@subannual.resolution <- "Decade"
  
  start.point <- ((first.year-actual.sta.info@first.year) / timestep) + 1
  n.years <- (last.year - first.year)/timestep + 1
  time.count <-  n.years
  end.point <- start.point + time.count - 1
  
  dim.names <- list(xx, yy, start.point:end.point)

  nc.start.vec <- c(  1,  1,            1, start.point )
  nc.count.vec <- c( -1, -1, max_pop_size, time.count )

  tmp.te   <- array( NA, c(length(xx), length(yy), length(start.point:end.point) ) )
  tmp.td   <- array( NA, c(length(xx), length(yy), length(start.point:end.point) ) )
  tmp.se   <- array( NA, c(length(xx), length(yy), length(start.point:end.point) ) )
  tmp.sd   <- array( NA, c(length(xx), length(yy), length(start.point:end.point) ) )
  tmp.g4   <- array( NA, c(length(xx), length(yy), length(start.point:end.point) ) )
  tmp.g3   <- array( NA, c(length(xx), length(yy), length(start.point:end.point) ) )

  for ( x in 1:length(xx) )
  {
    cat( "Progress:", round(x/length(xx)*100,1), "%                                     \r")
    for ( y in 1:length(yy) )
    {
      for ( z in start.point:end.point )
      {
        alive       <- ncdf4::ncvar_get( d, "alive",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        vegtp       <- ncdf4::ncvar_get( d, "VegType",   start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        pheno       <- ncdf4::ncvar_get( d, "Evergreen", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        stems       <- ncdf4::ncvar_get( d, "StemCount", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        
        ind.alive <- which( alive==1 )
        ind.te    <- which( alive==1 & vegtp==0 & pheno==1 & stems<=2.5 )   # indices of evergreen trees in trait data
        ind.td    <- which( alive==1 & vegtp==0 & pheno==0 & stems<=2.5 )   # indices of deciduous trees in trait data
        ind.se    <- which( alive==1 & vegtp==0 & pheno==1 & stems> 2.5 )   # indices of evergreen shrubs in trait data
        ind.sd    <- which( alive==1 & vegtp==0 & pheno==0 & stems> 2.5 )   # indices of deciduous shrubs in trait data
        ind.g4    <- which( alive==1 & vegtp==1 )                           # indices of grasses in trait data
        ind.g3    <- which( alive==1 & vegtp==2 )                           # indices of grasses in trait data
        
        if(variable@id == "agb"){
          if (length(ind.alive)>0) {
            bm.leaf.all <- ncdf4::ncvar_get( d, "BLeaf",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.wood.all <- ncdf4::ncvar_get( d, "BWood",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.bark.all <- ncdf4::ncvar_get( d, "BBark",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            
            tmp.te1 <- sum(bm.wood.all[ind.te]+bm.bark.all[ind.te])
            tmp.td1 <- sum(bm.wood.all[ind.td]+bm.bark.all[ind.td])
            tmp.se1 <- sum(bm.wood.all[ind.se]+bm.bark.all[ind.se])
            tmp.sd1 <- sum(bm.wood.all[ind.sd]+bm.bark.all[ind.sd])
            tmp.g41 <- sum(bm.leaf.all[ind.g4])
            tmp.g31 <- sum(bm.leaf.all[ind.g3])
          
            tmp.te[x,y,z-start.point+1] <- tmp.te1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.td[x,y,z-start.point+1] <- tmp.td1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.se[x,y,z-start.point+1] <- tmp.se1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.sd[x,y,z-start.point+1] <- tmp.sd1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.g4[x,y,z-start.point+1] <- tmp.g41*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
            tmp.g3[x,y,z-start.point+1] <- tmp.g31*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          }
        }
        
        
        if(variable@id == "bgb"){
          if (length(ind.alive)>0) {
            bm.root.all <- ncdf4::ncvar_get( d, "BRoot",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            
            tmp.te1 <- sum(bm.root.all[ind.te])
            tmp.td1 <- sum(bm.root.all[ind.td])
            tmp.se1 <- sum(bm.root.all[ind.se])
            tmp.sd1 <- sum(bm.root.all[ind.sd])
            tmp.g41 <- sum(bm.root.all[ind.g4])
            tmp.g31 <- sum(bm.root.all[ind.g3])
            
            tmp.te[x,y,z-start.point+1] <- tmp.te1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.td[x,y,z-start.point+1] <- tmp.td1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.se[x,y,z-start.point+1] <- tmp.se1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.sd[x,y,z-start.point+1] <- tmp.sd1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.g4[x,y,z-start.point+1] <- tmp.g41*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
            tmp.g3[x,y,z-start.point+1] <- tmp.g31*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          }
        }
        
        if(variable@id == "vegC_std"){
          if (length(ind.alive)>0) {
            bm.leaf <- ncdf4::ncvar_get( d, "BLeaf",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.wood <- ncdf4::ncvar_get( d, "BWood",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.bark <- ncdf4::ncvar_get( d, "BBark",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.repr <- ncdf4::ncvar_get( d, "BRepr",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.stor <- ncdf4::ncvar_get( d, "BStor",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            bm.root <- ncdf4::ncvar_get( d, "BRoot",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          
            tmp.te1 <- sum(bm.leaf[ind.te]+bm.root[ind.te]+bm.stor[ind.te]+bm.repr[ind.te]+bm.wood[ind.te]+bm.bark[ind.te])
            tmp.td1 <- sum(bm.leaf[ind.td]+bm.root[ind.td]+bm.stor[ind.td]+bm.repr[ind.td]+bm.wood[ind.td]+bm.bark[ind.td])
            tmp.se1 <- sum(bm.leaf[ind.se]+bm.root[ind.se]+bm.stor[ind.se]+bm.repr[ind.se]+bm.wood[ind.se]+bm.bark[ind.se])
            tmp.sd1 <- sum(bm.leaf[ind.sd]+bm.root[ind.sd]+bm.stor[ind.sd]+bm.repr[ind.sd]+bm.wood[ind.sd]+bm.bark[ind.sd])
            tmp.g41 <- sum(bm.leaf[ind.g4]+bm.root[ind.g4]+bm.stor[ind.g4]+bm.repr[ind.g4])
            tmp.g31 <- sum(bm.leaf[ind.g3]+bm.root[ind.g3]+bm.stor[ind.g3]+bm.repr[ind.g3])
          
            tmp.te[x,y,z-start.point+1] <- tmp.te1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.td[x,y,z-start.point+1] <- tmp.td1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.se[x,y,z-start.point+1] <- tmp.se1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.sd[x,y,z-start.point+1] <- tmp.sd1*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
            tmp.g4[x,y,z-start.point+1] <- tmp.g41*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
            tmp.g3[x,y,z-start.point+1] <- tmp.g31*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          }
        }
        
        if(variable@id == "LAI_std"){
          if (length(ind.alive)>0) {
            tmp.blf  <- ncdf4::ncvar_get( d, "BLeaf", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            tmp.sla  <- ncdf4::ncvar_get( d, "Sla", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            
            tmp.te[x,y,z-start.point+1]   <- sum(tmp.blf[ind.te]*tmp.sla[ind.te])/10000
            tmp.td[x,y,z-start.point+1]   <- sum(tmp.blf[ind.td]*tmp.sla[ind.td])/10000
            tmp.se[x,y,z-start.point+1]   <- sum(tmp.blf[ind.se]*tmp.sla[ind.se])/10000
            tmp.sd[x,y,z-start.point+1]   <- sum(tmp.blf[ind.sd]*tmp.sla[ind.sd])/10000
            tmp.g4[x,y,z-start.point+1]   <- sum(tmp.blf[ind.g4]*tmp.sla[ind.g4])/10000
            tmp.g3[x,y,z-start.point+1]   <- sum(tmp.blf[ind.g3]*tmp.sla[ind.g3])/10000
            
#            tmp.all  <- ncdf4::ncvar_get( d, "Lai", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
#            tmp.te[x,y,z-start.point+1]   <- mean(tmp.all[ind.te])*(length(ind.te)/length(alive))
#            tmp.td[x,y,z-start.point+1]   <- mean(tmp.all[ind.td])*(length(ind.td)/length(alive))
#            tmp.se[x,y,z-start.point+1]   <- mean(tmp.all[ind.se])*(length(ind.se)/length(alive))
#            tmp.sd[x,y,z-start.point+1]   <- mean(tmp.all[ind.sd])*(length(ind.sd)/length(alive))
#            tmp.g4[x,y,z-start.point+1]   <- mean(tmp.all[ind.g4])*(length(ind.g4)/length(alive))
#            tmp.g3[x,y,z-start.point+1]   <- mean(tmp.all[ind.g3])*(length(ind.g3)/length(alive))
          }
        }
        
        if(variable@id == "basalarea"){
          if (length(ind.alive)>0) {
            tmp.all  <- ncdf4::ncvar_get( d, "StemDiamTot", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            tmp.te[x,y,z-start.point+1]   <- sum(tmp.all[ind.te])
            tmp.td[x,y,z-start.point+1]   <- sum(tmp.all[ind.td])
            tmp.se[x,y,z-start.point+1]   <- sum(tmp.all[ind.se])
            tmp.sd[x,y,z-start.point+1]   <- sum(tmp.all[ind.sd])
            tmp.g4[x,y,z-start.point+1]   <- sum(tmp.all[ind.g4])
            tmp.g3[x,y,z-start.point+1]   <- sum(tmp.all[ind.g3])
          }
        }
        
        if(variable@id == "nind"){
          if (length(ind.alive)>0) {
            tmp.te[x,y,z-start.point+1]   <- length(ind.te)
            tmp.td[x,y,z-start.point+1]   <- length(ind.td)
            tmp.se[x,y,z-start.point+1]   <- length(ind.se)
            tmp.sd[x,y,z-start.point+1]   <- length(ind.sd)
            tmp.g4[x,y,z-start.point+1]   <- length(ind.g4)
            tmp.g3[x,y,z-start.point+1]   <- length(ind.g3)
          }
        }
        
        if(variable@id == "meanheight"){
          if (length(ind.alive)>0) {
            tmp.all  <- ncdf4::ncvar_get( d, "Height", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            tmp.te[x,y,z-start.point+1]   <- mean(tmp.all[ind.te])
            tmp.td[x,y,z-start.point+1]   <- mean(tmp.all[ind.td])
            tmp.se[x,y,z-start.point+1]   <- mean(tmp.all[ind.se])
            tmp.sd[x,y,z-start.point+1]   <- mean(tmp.all[ind.sd])
            tmp.g4[x,y,z-start.point+1]   <- mean(tmp.all[ind.g4])
            tmp.g3[x,y,z-start.point+1]   <- mean(tmp.all[ind.g3])
          }
        }
        
        if(variable@id == "canopyheight_std"){
          if (length(ind.alive)>0) {
            tmp.all  <- ncdf4::ncvar_get( d, "Height", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            tmp.te[x,y,z-start.point+1]   <- stats::quantile(tmp.all[ind.te],0.95)
            tmp.td[x,y,z-start.point+1]   <- stats::quantile(tmp.all[ind.td],0.95)
            tmp.se[x,y,z-start.point+1]   <- stats::quantile(tmp.all[ind.se],0.95)
            tmp.sd[x,y,z-start.point+1]   <- stats::quantile(tmp.all[ind.sd],0.95)
            tmp.g4[x,y,z-start.point+1]   <- stats::quantile(tmp.all[ind.g4],0.95)
            tmp.g3[x,y,z-start.point+1]   <- stats::quantile(tmp.all[ind.g3],0.95)
          }
        }
        
        if(variable@id == "pind"){
          if (length(ind.alive)>0) {
            tmp.te1   <- length(ind.te)
            tmp.td1   <- length(ind.td)
            tmp.se1   <- length(ind.se)
            tmp.sd1   <- length(ind.sd)
            tmp.g41   <- length(ind.g4)
            tmp.g31   <- length(ind.g3)
          
            ind.tot  <- tmp.te1+tmp.td1+tmp.se1+tmp.sd1+tmp.g41+tmp.g31
            tmp.te[x,y,z-start.point+1]   <- tmp.te1/ind.tot
            tmp.td[x,y,z-start.point+1]   <- tmp.td1/ind.tot
            tmp.se[x,y,z-start.point+1]   <- tmp.se1/ind.tot
            tmp.sd[x,y,z-start.point+1]   <- tmp.sd1/ind.tot
            tmp.g4[x,y,z-start.point+1]   <- tmp.g41/ind.tot
            tmp.g3[x,y,z-start.point+1]   <- tmp.g31/ind.tot
          }
        }
        
        if(variable@id == "vegcover_std"){
          if (length(ind.alive)>0) {
            tmp.all  <- ncdf4::ncvar_get( d, "CrownArea", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
            tmp.te1   <- sum(tmp.all[ind.te])/10000
            tmp.td1   <- sum(tmp.all[ind.td])/10000
            tmp.se1   <- sum(tmp.all[ind.se])/10000
            tmp.sd1   <- sum(tmp.all[ind.sd])/10000
            tmp.g41   <- sum(tmp.all[ind.g4])/10000
            tmp.g31   <- sum(tmp.all[ind.g3])/10000
            
            tmp.cov   <- tmp.te1+tmp.td1+tmp.se1+tmp.sd1+tmp.g41+tmp.g31
            if ( tmp.cov<1 ) tmp.cov <- 1

            tmp.te[x,y,z-start.point+1]   <- tmp.te1/tmp.cov*100  # *100 to convert to %
            tmp.td[x,y,z-start.point+1]   <- tmp.td1/tmp.cov*100  # *100 to convert to %
            tmp.se[x,y,z-start.point+1]   <- tmp.se1/tmp.cov*100  # *100 to convert to %
            tmp.sd[x,y,z-start.point+1]   <- tmp.sd1/tmp.cov*100  # *100 to convert to %
            tmp.g4[x,y,z-start.point+1]   <- tmp.g41/tmp.cov*100  # *100 to convert to %
            tmp.g3[x,y,z-start.point+1]   <- tmp.g31/tmp.cov*100  # *100 to convert to %
          }
        }
        
        if(variable@id == "aGPP_std"){
          message(paste(variable@id, "not available in scheme, all values set to zero."))
          if (length(ind.alive)>0) {
            tmp.te[x,y,z-start.point+1]   <- 0
            tmp.td[x,y,z-start.point+1]   <- 0
            tmp.se[x,y,z-start.point+1]   <- 0
            tmp.sd[x,y,z-start.point+1]   <- 0
            tmp.g4[x,y,z-start.point+1]   <- 0
            tmp.g3[x,y,z-start.point+1]   <- 0
          }
        }
        
        #if(variable@id == "firefreq" | variable@id == "burntfraction_std" ){
        #  message(paste(variable@id, "not available in scheme, all values set to zero."))
        #  if (length(ind.alive)>0) {
        #    tmp.te[x,y,z-start.point+1]   <- 0
        #    tmp.td[x,y,z-start.point+1]   <- 0
        #    tmp.se[x,y,z-start.point+1]   <- 0
        #    tmp.sd[x,y,z-start.point+1]   <- 0
        #    tmp.g4[x,y,z-start.point+1]   <- 0
        #    tmp.g3[x,y,z-start.point+1]   <- 0
        #  }
        #}
        
      }
    }
  }
  cat( "                                                       \n\n")

  dimnames(tmp.te) <- dim.names
  dimnames(tmp.td) <- dim.names
  dimnames(tmp.se) <- dim.names
  dimnames(tmp.sd) <- dim.names
  dimnames(tmp.g4) <- dim.names
  dimnames(tmp.g3) <- dim.names
  
  # prepare data.table from the slice (array) for each PFT
  te.dt <- as.data.table(melt(tmp.te))
  names(te.dt) <- c("Lon", "Lat", "Time", "TrBE")
  setkey(te.dt, Lon, Lat, Time)
  
  td.dt <- as.data.table(melt(tmp.td))
  names(td.dt) <- c("Lon", "Lat", "Time", "TrBR")
  setkey(td.dt, Lon, Lat, Time)
  
  se.dt <- as.data.table(melt(tmp.se))
  names(se.dt) <- c("Lon", "Lat", "Time", "TrBES")
  setkey(se.dt, Lon, Lat, Time)
  
  sd.dt <- as.data.table(melt(tmp.sd))
  names(sd.dt) <- c("Lon", "Lat", "Time", "TrBRS")
  setkey(sd.dt, Lon, Lat, Time)
  
  g3.dt <- as.data.table(melt(tmp.g3))
  names(g3.dt) <- c("Lon", "Lat", "Time", "C3G")
  setkey(g3.dt, Lon, Lat, Time)
  
  g4.dt <- as.data.table(melt(tmp.g4))
  names(g4.dt) <- c("Lon", "Lat", "Time", "C4G")
  setkey(g4.dt, Lon, Lat, Time)
  
  # merge the data tables for each PFT into one data.table
  out.all <- se.dt[sd.dt[te.dt[td.dt[g3.dt[g4.dt]]]]]
  
  # sort out the time dimension
  # NOTE: something special is happening here. The ':=' operator is changing the data.table in place.
  # this means that you don't need to do a re-assignment using '<-' 
  out.all[, Year := Time*timestep + run@year.offset - timestep ]
  
#  if(adgvm.daily) {
#    out.all[, Day := ((Time-1) %% time.steps.per.year) + 1]
#  }
#  else {
  out.all[, Month := 1]
#  }
  out.all[, Time := NULL]
  out.all[, Day := NULL]
  
  out.all = stats::na.omit(out.all)
  print(out.all)
  
  # Now that we have the data we can set a spatial.extent
  actual.sta.info@spatial.extent <- extent(out.all)
  
  return(list(dt = out.all, 
              sta.info = actual.sta.info))
}

#' Detemine PFTs present in an aDGVM2; currently only returns a warning.
#' 
#' @param x  A Source objects describing a DGVMData source
#' @param variables Some variable to look for to detremine the PFTs present in the run.  Not the function automatically searches:
#'  "lai", "cmass", "dens" and "fpc".  If they are not in your output you should define another per-PFT variable here.  Currently ignored.
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal

determinePFTs_aDGVM <- function(x, variables) {
  
  warning("Need aDGVMers to write this function! For now I am returning the source@format@default.set argument directly.")
  return(x@format@default.pfts)
  
}


#' List aDGVM quantities avialable
#'
#' Simply lists all quantitied aDGVM  output variables 
#' 
#' @param source A path to a directory on the file system containing some .out files
#' @return A list of all the .out files present, with the ".out" removed. 
#' 
#' Needs to be implemented by an aDGVMer
#' 
#' @keywords internal
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}


availableQuantities_aDGVM <- function(source){
  
  warning("Needs to be implemented by an aDGVMer.")
  
  quantities.present <- list()
  return(quantities.present)
  
}


###############################################
########### aDGVM PFTS ########################
###############################################



#' @format An S4 class object with the slots as defined below.
#' @rdname PFT-class
#' @keywords datasets
aDGVM.PFTs <- list(
  
  new("PFT",
            id = "C3G",
            name = "Boreal/Temperate Grass",
            growth.form = "Grass",
            leaf.form = "Broadleaved",
            phenology = "GrassPhenology",
            climate.zone = "NA",
            colour = "lightgoldenrod1",
            shade.tolerance = "None"
  ),
  
  new("PFT",
            id = "C4G",
            name = "Tropical Grass",
            growth.form = "Grass",
            leaf.form = "Broadleaved",
            phenology = "GrassPhenology",
            climate.zone = "NA",
            colour = "sienna2",
            shade.tolerance = "None"
  ),
  
  new("PFT",
          id = "Tr",
          name = "Tropical Tree",
          growth.form = "Tree",
          leaf.form = "Broadleaved",
          phenology = "Mixed",
          climate.zone = "Tropical",
          colour = "palevioletred",
          shade.tolerance = "None"
  ),
  
  new("PFT",
             id = "TrBE",
             name = "Tropical Broadleaved Evergreen Tree",
             growth.form = "Tree",
             leaf.form = "Broadleaved",
             phenology = "Evergreen",
             climate.zone = "Tropical",
             colour = "orchid4",
             shade.tolerance = "None"
  ),
  
  new("PFT",
             id = "TrBR",
             name = "Tropical Broadleaved Raingreen Tree",
             growth.form = "Tree",
             leaf.form = "Broadleaved",
             phenology = "Raingreen",
             climate.zone = "Tropical",
             colour = "palevioletred",
             shade.tolerance = "None"
  ),
  
  new("PFT",
              id = "TrBES",
              name = "Tropical Broadleaved Evergreen Shrub",
              growth.form = "Shrub",
              leaf.form = "Broadleaved",
              phenology = "Evergreen",
              climate.zone = "Tropical",
              colour = "orchid4",
              shade.tolerance = "None"
  ),
  
  new("PFT",
              id = "TrBRS",
              name = "Tropical Broadleaved Raingreen Shrub",
              growth.form = "Shrub",
              leaf.form = "Broadleaved",
              phenology = "Raingreen",
              climate.zone = "Tropical",
              colour = "palevioletred",
              shade.tolerance = "None"
  )
  
  
)



#####################################################
########### aDGVM QUANTITIES ########################
#####################################################


#' @format The \code{Quantity} class is an S4 class with the slots defined below
#' @rdname Quantity-class
#' @keywords datasets
#' @include colour-palettes.R
#' 
aDGVM.quantities <- list(
  new("Quantity",
      id = "agb",
      name = "Above Ground Biomass",
      units = "kgC/m^2",
      colours = viridis::viridis,
      format = c("aDGVM")),
  
  new("Quantity",
      id = "bgb",
      name = "Below Ground Biomass",
      units = "kgC/m^2",
      colours = viridis::viridis,
      format = c("aDGVM")),
  
  new("Quantity",
      id = "meanheight",
      name = "Mean Canopy Height",
      units = "m",
      colours = fields::tim.colors,
      format = c("aDGVM")),
  
  new("Quantity",
      id = "basalarea",
      name = "Basal Area",
      units = "m^2/ha",
      colours = fields::tim.colors,
      format = c("aDGVM")),
  
  new("Quantity",
      id = "nind",
      name = "Number of individuals",
      units = "plants",
      colours = veg.palette,
      format = c("aDGVM")),
  
  new("Quantity",
      id = "pind",
      name = "Fraction of individuals",
      units = "",
      colours = veg.palette,
      format = c("aDGVM"))
  
  #new("Quantity",
  #    id = "firefreq",
  #    name = "Fire Frequency",
  #    units = "",
  #    colours = reversed.fire.palette,
  #    format = c("aDGVM"))
  
  
)


################################################################
########### aDGVM FORMAT ########################
################################################################

#' @description \code{aDGVM} - a Format for reading aDGVM2 model output
#' 
#' @format A \code{Quantity} object is an S4 class.
#' @aliases Format-class
#' @rdname Format-class
#' @keywords datasets
#' @include colour-palettes.R
#' @export
#' 
aDGVM <- new("Format",
             
             # UNIQUE ID
             id = "aDGVM",
             
             # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
             determinePFTs = determinePFTs_aDGVM,
             
             # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
             availableQuantities = availableQuantities_aDGVM,
             
             # FUNCTION TO READ A FIELD 
             getField = getField_aDGVM,
             
             # DEFAULT GLOBAL PFTS  
             default.pfts = aDGVM.PFTs,
             
             # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
             quantities = aDGVM.quantities
             
)
