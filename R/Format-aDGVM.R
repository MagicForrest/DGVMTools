############################################################################################################################
############################ FUNCTIONS TO HANDLE LPJ-GUESS FILES ###########################################################
############################################################################################################################

#' Get a Field for aDGVM
#' 
#' An internal function that reads data from an aDGVM run.  It actually call one of three other functions depending on the type of quantity specified.   
#' 
#' @param run A \code{Source} containing the meta-data about the LPJ-GUESS run
#' @param variable A string the define what output file from the LPJ-GUESS run to open, for example "anpp" opens and read the "anpp.out" file 
#' @param first.year The first year (as a numeric) of the data to be return
#' @param last.year The last year (as a numeric) of the data to be return
#' @param verbose A logical, set to true to give progress/debug information
#' @return A list containing firstly the data.tabel containing the data, and secondly the STA.info 
#' @author Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @keywords internal
getField_aDGVM <- function(source,
                           quant,
                           target.STAInfo,
                           verbose,
                           adgvm.scheme,
                           adgvm.daily) {
  
  if(missing(adgvm.scheme)) adgvm.scheme <- 1
  if(missing(adgvm.daily)) adgvm.daily <- FALSE
  
  if("aDGVM" %in% quant@format | "Standard" %in% quant@format) {
    if(adgvm.scheme == 1) data.list <- getQuantity_aDGVM_Scheme1(source, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, quant, adgvm.daily)
    if(adgvm.scheme == 2) this.dt <- data.table(getQuantity_aDGVM_Scheme2(source, first.year = target.STAInfo@first.year, last.year = target.STAInfo@last.year, quant))
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


# ----------------------------------------------------
# Convert aDGVM2 netCDF output into LPJ ASCII format.
#
# The scripts generate yearly or monthly data.
# Currently, there are two different schemes to
# classify vegetation into PFTs.
# 
# Todo:
#  * Include C3 grasses
#  * Include more classification schemes
#    (e.g. schrubs, fire or drought tolerance, ...)
#  * Include more variables (e.g. NPP, height, ...)
#  * Modify yearly output variables, currently the
#    scripts writes data from Jan 31st (for scheme 1
#    and data from Jan 1st (for scheme 2). I think
#    we should use maximum values of each year.
#  * Monthly output for scheme 2 (this requires
#    changes in aDGVM2 output routines)
# ----------------------------------------------------



# ----------------------------------------------------
# Convert aDGVM2 results into LPJ output format.
# The output file includes lon, lat, year and yearly
# values of state variables for different PFTs.
# Each state variable is in a seperate file.
#
# Vegetation classification scheme 1:
#  * Trees
#  * C4 grasses
#
# Currently there are two files for
#  * Aboveground biomas
#  * Lai
#
# The script reads pop files
#
# ----------------------------------------------------
#
#' Simple classification of yearly output for aDGVM 
#' 
#' Original from Simon Scheiter, a modified version has been produced by M. Forrest to fit into the DGVM framework and is probably now
#' redundant.  Classifies only as tree or grass based on only the pop file, doesn't open the trait file.
#' 
#' @param runid Character string identifying the run
#' @param fire Character string denoting if fire was on or off
#' @param directory Character string specifying the run directory
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}
#' @import ncdf4
#' @keywords internal
#' @seealso \code{getQuantity_aDGVM_Scheme1}
convertYearlyScheme1 <- function( runid, fire, directory )
{
  fname <- paste(directory, "/pop_", runid, "_", fire, ".nc", sep="" )
  cat( "Convert", fname, "\n" )
  d <- nc_open(fname)
  
  xx <- ncvar_get(d,"lon")
  yy <- ncvar_get(d,"lat")
  tt <- ncvar_get(d,"time")
  len_tt <- length(tt)
  
  # process only the last nyears years of the simulation run
  nyears <- 20
  t_seq <- seq( len_tt-(nyears*12), len_tt, by=12 )
  
  cnames <- c( "Lon", "Lat", "Year", "Tr", "C4G", "Total")
  
  out.abm <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.abm) <- cnames
  
  out.lai <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.lai) <- cnames
  
  for ( x in 1:length(xx) )
  {
    for ( y in 1:length(yy) )
    {
      for ( z in t_seq )
      {
        tmp.tr <-          ncvar_get( d, "SumBBark", start=c( x,y,2,z ), count=c( 1,1,1,1 ) )
        tmp.tr <- tmp.tr + ncvar_get( d, "SumBWood", start=c( x,y,2,z ), count=c( 1,1,1,1 ) )
        tmp.gr <-          ncvar_get( d, "SumBLeaf", start=c( x,y,3,z ), count=c( 1,1,1,1 ) )
        
        #            Lon      Lat    Year            Tr      C4G     Total
        out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.tr, tmp.gr, tmp.tr+tmp.gr )
        out.abm <- rbind( out.abm, out.vec )
        
        tmp.tr <- ncvar_get( d, "MeanLai", start=c( x,y,2,z ), count=c( 1,1,1,1 ) )
        tmp.gr <- ncvar_get( d, "MeanLai", start=c( x,y,3,z ), count=c( 1,1,1,1 ) )
        
        #            Lon      Lat    Year            Tr      C4G     Total
        out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.tr, tmp.gr, tmp.tr+tmp.gr )
        out.lai <- rbind( out.lai, out.vec )
      }
    }
  }
  print(out.lai)
  
  utils::write.table( out.abm, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_yearly_agbm", sep=""), row.names=F, quote=F )
  utils::write.table( out.lai, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_yearly_lai",  sep=""), row.names=F, quote=F )
  
  
  
  return(out.lai)
  
}


# ----------------------------------------------------
# Convert aDGVM2 results into LPJ output format.
# The output file includes lon, lat, year and yearly
# values of state variables for different PFTs.
# Each state variable is in a seperate file.
#
# Vegetation classification scheme 2:
#  * Deciduous trees
#  * Evergreen trees
#  * C4 grasses
#
# Currently there are two files for
#  * Aboveground biomas
#
# The script reads trait files
# ----------------------------------------------------
#' Slightly more complex classification of yearly output for aDGVM output
#' 
#' 
#' Original from Simon Scheiter, a modified version has been produced by M. Forrest to fit into the DGVM framework and is probably now
#' redundant.  Classifies as evergreen tree, deciduous tree or grass based on the pop file and the trait file.
#' 
#' @param runid Character string identifying the run
#' @param fire Character string denoting if fire was on or off
#' @param directory Character string specifying the run directory
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}
#' @import ncdf4
#' @keywords internal
#' @seealso \code{getQuantity_aDGVM_Scheme2}
#' 
convertYearlyScheme2 <- function( runid, fire, directory )
{
  fname <- paste( "~/", directory, "/trait_", runid, "_", fire, ".nc", sep="" )
  cat( "Convert", fname, "\n" )
  d <- nc_open(fname)
  
  xx <- ncvar_get(d,"lon")
  yy <- ncvar_get(d,"lat")
  tt <- ncvar_get(d,"time")
  len_tt <- length(tt)
  
  # process only the last nyears years of the simulation run
  nyears <- 20
  t_seq <- (len_tt-nyears+1):len_tt
  
  max_pop_size <- 1600          # maximum population size
  biomass_conversion <- 1/1000  # convert from kg/ha to t/ha
  
  cnames <- c( "Lon", "Lat", "Year", "TrBE", "TrBR", "C4G", "Total")
  
  out.abm <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.abm) <- cnames
  
  for ( x in 1:length(xx) )
  {
    cat( "x=", x, "of", length(xx), "\n")
    for ( y in 1:length(yy) )
    {
      for ( z in t_seq )
      {
        alive       <- ncvar_get( d, "alive",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        vegtp       <- ncvar_get( d, "VegType",   start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        pheno       <- ncvar_get( d, "Evergreen", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        bm_leaf_all <- ncvar_get( d, "BLeaf",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        bm_wood_all <- ncvar_get( d, "BWood",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        bm_bark_all <- ncvar_get( d, "BBark",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        
        ind_te <- which( alive==1 & vegtp==0 & pheno==1 )   # indices of evergreen trees in trait data
        ind_td <- which( alive==1 & vegtp==0 & pheno==0 )   # indices of deciduous trees in trait data
        ind_gr <- which( alive==1 & vegtp==1 )              # indices of grasses in trait data
        
        bm_ag_te <- sum(bm_wood_all[ind_te]+bm_bark_all[ind_te])*biomass_conversion  # calculate biomass of evergreen trees and convert to t/ha
        bm_ag_td <- sum(bm_wood_all[ind_td]+bm_bark_all[ind_td])*biomass_conversion  # calculate biomass of deciduous trees and convert to t/ha
        bm_ag_gr <- sum(bm_leaf_all[ind_gr])*biomass_conversion  # calculate biomass of grasses and convert to t/ha
        
        #            Lon      Lat    Year            TrBE      TrBR      C4G       Total
        out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), bm_ag_te, bm_ag_td, bm_ag_gr, bm_ag_te+bm_ag_td+bm_ag_gr )
        out.abm <- rbind( out.abm, out.vec )
      }
    }
  }
  
  utils::write.table( out.abm, file=paste("aDGVM2_", runid, "_", fire, "_scheme2_yearly_agbm", sep=""), row.names=F, quote=F )
  
}


# ----------------------------------------------------
# Convert aDGVM2 results into LPJ output format.
# The output file includes lon, lat, year and monthly
# values of state variables for different PFTs.
# Each PFT and eachs tate variable is in a seperate
# file.
#
# Vegetation classification scheme 1:
#  * Trees
#  * C4 grasses
#
# Currently there are four files for
#  * Aboveground biomas trees
#  * Aboveground biomas grasses
#  * LAI trees
#  * LAI grasses
#
# The script reads pop files
#
# ----------------------------------------------------
#' Simple classification of monthly output for aDGVM 
#' 
#' Original from Simon Scheiter, ***THERE IS CURRENTLY NO INTERGATED VERSION OF THIS WHICH FITS INTO DGVM TOOLS*
#' Someone needs to do that...    
#' Classifies only as tree or grass based on only the pop file, doesn't open the trait file.
#' 
#' @param runid Character string identifying the run
#' @param fire Character string denoting if fire was on or off
#' @param directory Character string specifying the run directory
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}
#' @import ncdf4
#' @keywords internal
#' @seealso \code{getQuantity_aDGVM_Scheme1}
convertMonthlyScheme1 <- function( runid, fire, directory )
{
  fname <- paste( "~/", directory, "/pop_", runid, "_", fire, ".nc", sep="" )
  cat( "Convert", fname, "\n" )
  d <- nc_open(fname)
  
  xx <- ncvar_get(d,"lon")
  yy <- ncvar_get(d,"lat")
  tt <- ncvar_get(d,"time")
  len_tt <- length(tt)
  
  # process only the last nyears years of the simulation run
  nyears <- 20
  t_seq <- seq( len_tt-(nyears*12)+1, len_tt )
  
  tstart <- min(t_seq)
  tend   <- max(t_seq)
  tlen   <- length(t_seq)
  
  year_seq <- unique(floor(tt[t_seq]))
  
  cnames <- c( "Lon", "Lat", "Year", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" )
  
  out.abm.tr <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.abm.tr) <- cnames
  
  out.abm.gr <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.abm.gr) <- cnames
  
  out.lai.tr <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.lai.tr) <- cnames
  
  out.lai.gr <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.lai.gr) <- cnames
  
  for ( x in 1:length(xx) )
  {
    for ( y in 1:length(yy) )
    {
      # aboveground biomass
      tmp.tr <-          ncvar_get( d, "SumBBark", start=c( x,y,2,tstart ), count=c( 1,1,1,tlen ) )
      tmp.tr <- tmp.tr + ncvar_get( d, "SumBWood", start=c( x,y,2,tstart ), count=c( 1,1,1,tlen ) )
      tmp.gr <-          ncvar_get( d, "SumBLeaf", start=c( x,y,3,tstart ), count=c( 1,1,1,tlen ) )
      #                    Lon                 Lat                 Year         ... monthly data ...
      out.vec    <- cbind( rep(xx[x], nyears), rep(yy[y], nyears), year_seq, matrix( tmp.tr, ncol=12, byrow=T ) )
      out.abm.tr <- rbind( out.abm.tr, out.vec )
      
      #                    Lon                 Lat                 Year         ... monthly data ...
      out.vec    <- cbind( rep(xx[x], nyears), rep(yy[y], nyears), year_seq, matrix( tmp.gr, ncol=12, byrow=T ) )
      out.abm.gr <- rbind( out.abm.gr, out.vec )
      
      # lai
      tmp.tr <- ncvar_get( d, "MeanLai", start=c( x,y,2,tstart ), count=c( 1,1,1,tlen ) )
      tmp.gr <- ncvar_get( d, "MeanLai", start=c( x,y,3,tstart ), count=c( 1,1,1,tlen ) )
      #                    Lon                 Lat                 Year         ... monthly data ...
      out.vec    <- cbind( rep(xx[x], nyears), rep(yy[y], nyears), year_seq, matrix( tmp.tr, ncol=12, byrow=T ) )
      out.lai.tr <- rbind( out.lai.tr, out.vec )
      
      #                    Lon                 Lat                 Year         ... monthly data ...
      out.vec    <- cbind( rep(xx[x], nyears), rep(yy[y], nyears), year_seq, matrix( tmp.gr, ncol=12, byrow=T ) )
      out.lai.gr <- rbind( out.lai.gr, out.vec )
    }
  }
  
  utils::write.table( out.abm.tr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_Tr_agbm", sep=""), row.names=F, quote=F )
  utils::write.table( out.lai.tr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_Tr_lai",  sep=""), row.names=F, quote=F )
  
  utils::write.table( out.abm.gr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_C4G_agbm", sep=""), row.names=F, quote=F )
  utils::write.table( out.lai.gr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_C4G_lai",  sep=""), row.names=F, quote=F )
  
}




#' Simple classification of yearly output for aDGVM
#' 
#' Original from Simon Scheiter, this version modified by M. Forrest to fit into the DGVM framework. 
#' Simple scheme, classifies only as tree, C4 grass or C3 grass based on only the pop file, doesn't open the trait file.
#' 
#' @param run A \code{Source}
#' @param first.year First year of data to read (not currently used)
#' @param last.year Last year of data to read (not currently used)
#' @param variable A character string specifying which variable to get, can be "agb","nind","meanheight","basalarea","pind","firefreq","vegcover_std","vegC_std","LAI_std","aGPP_std","canopyheight_std","burntfraction_std"
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}, Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ncdf4
#' @keywords internal
#' @seealso \code{getQuantity_aDGVM_Scheme2}
getQuantity_aDGVM_Scheme1 <- function(run, variable, first.year, last.year, adgvm.daily)
{
  # To stop NOTES
  Day = Lat = Lon = Month = Time = Year = NULL
  
  fname <- file.path(run@dir, paste("pop_", run@id,".nc", sep=""))
  
  # STAInfo object to summarise the spatial-temporal-annual dimensions of the data
  actual.sta.info <- new("STAInfo")
  
  cat( "Convert", fname, "\n" )
  d <- nc_open(fname)
  
  xx <- ncvar_get(d,"lon")
  yy <- ncvar_get(d,"lat")
  tt <- ncvar_get(d,"time")
  len_tt <- length(tt)
  #print(tt)
  #print(length(tt))
  
  
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
  print(c(first.year,last.year))
  
  start.point <- ((first.year-actual.sta.info@first.year) * time.steps.per.year) + 1 
  n.years <- last.year - first.year +1
  time.count <-  time.steps.per.year * n.years
  end.point <- start.point + time.count - 1
  
  print(c(start.point, end.point))
  
  dim.names <- list(xx, yy, start.point:end.point)
    
  nc.start.vec.tr <- c(  1, 1,1,start.point )
  nc.start.vec.g4 <- c(  1, 1,2,start.point )
  nc.start.vec.g3 <- c(  1, 1,3,start.point )
  nc.count.vec    <- c( -1,-1,1,time.count )
  
  # number of years used to calculate fire return interval  
  years_fire <- 20
  
  #if(variable@id == "firefreq" | variable@id == "burntfraction_std" ){
    
   # tmp <- ncvar_get( d, "firecount", start=c( x,y,len_tt-(years_fire*12)+1 ), count=c( 1,1,years_fire*12 ) )
  #  fir <- matrix( tmp, ncol=12, byrow=T )[,12]
 #   tmp.fi <- min( sum(fir)/length(fir), 1)

#    z <- max(t_seq)
  #         #            Lon      Lat    Year            Tr      C4G     C3G     Total
  #         out.vec <- c( xx[x],  yy[y], which(t_seq == z), tmp.fi, tmp.fi, tmp.fi, tmp.fi )
    #         out.all <- rbind( out.all, out.vec )
  #}
  
  if(variable@id == "vegcover_std"){
    tmp.tr <- ncvar_get( d, "SumCanopyArea0", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "SumCanopyArea0", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "SumCanopyArea0", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.g4  <-       tmp.g4/10000*100  # *100 to convert to %
    tmp.g3  <-       tmp.g3/10000*100  # *100 to convert to %
    tmp.tr  <- pmin( tmp.tr/20000*100, 100-(tmp.g4+tmp.g3))  # *100 to convert to %
  }
  
  if(variable@id == "basalarea"){
    tmp.tr <- ncvar_get( d, "SumBasalArea", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "SumBasalArea", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "SumBasalArea", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "canopyheight_std"){
    tmp.tr <- ncvar_get( d, "MeanHeight", start=nc.start.vec.tr, count=nc.count.vec )
    message(paste("Warning: Canopy height currently equal to mean height!"))
    tmp.g3 <- tmp.tr   # dummy to avoid error message
    tmp.g4 <- tmp.tr   # dummy to avoid error message
  }
  
  if(variable@id == "aGPP_std"){
    tmp.tr <- ncvar_get( d, "MeanCGain",  start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "MeanCGain",  start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "MeanCGain",  start=nc.start.vec.g3, count=nc.count.vec )
    ind.tr <- ncvar_get( d, "nind_alive", start=nc.start.vec.tr, count=nc.count.vec )
    ind.g4 <- ncvar_get( d, "nind_alive", start=nc.start.vec.g4, count=nc.count.vec )
    ind.g3 <- ncvar_get( d, "nind_alive", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr*30.42*ind.tr/2  # 30.42 to scale from monthly to annual, ind. for all plants, 2 for kg->C
    tmp.g4 <- tmp.g4*30.42*ind.g4/2  # 30.42 to scale from monthly to annual, ind. for all plants, 2 for kg->C
    tmp.g3 <- tmp.g3*30.42*ind.g3/2  # 30.42 to scale from monthly to annual, ind. for all plants, 2 for kg->C
  }
  
  if(variable@id == "LAI_std" | variable@id == "lai"){
    tmp.tr <- ncvar_get( d, "MeanLai", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "MeanLai", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "MeanLai", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "meanheight"){
    tmp.tr <- ncvar_get( d, "MeanHeight", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "MeanHeight", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "MeanHeight", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "pind"){
    tmp.tr <- ncvar_get( d, "nind_alive", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "nind_alive", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "nind_alive", start=nc.start.vec.g3, count=nc.count.vec )
    
    ind_tot <- tmp.tr+tmp.g4+tmp.g3
    tmp.tr  <- tmp.tr/ind_tot
    tmp.g4  <- tmp.g4/ind_tot
    tmp.g3  <- tmp.g3/ind_tot
  }
  
  if(variable@id == "nind"){
    tmp.tr <- ncvar_get( d, "nind_alive", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <- ncvar_get( d, "nind_alive", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <- ncvar_get( d, "nind_alive", start=nc.start.vec.g3, count=nc.count.vec )
  }
  
  if(variable@id == "agb"){
    tmp.tr <-          ncvar_get( d, "SumBBark", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncvar_get( d, "SumBWood", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.g4 <-          ncvar_get( d, "SumBLeaf", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g3 <-          ncvar_get( d, "SumBLeaf", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr/20
    tmp.g4 <- tmp.g4/2
    tmp.g3 <- tmp.g3/2
  }
  
  if(variable@id == "vegC_std"){
    tmp.tr <-          ncvar_get( d, "SumBBark", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncvar_get( d, "SumBWood", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncvar_get( d, "SumBLeaf", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncvar_get( d, "SumBStor", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncvar_get( d, "SumBRoot", start=nc.start.vec.tr, count=nc.count.vec )
    tmp.tr <- tmp.tr + ncvar_get( d, "SumBRepr", start=nc.start.vec.tr, count=nc.count.vec )
    
    tmp.g4 <-          ncvar_get( d, "SumBLeaf", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g4 <- tmp.g4 + ncvar_get( d, "SumBRoot", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g4 <- tmp.g4 + ncvar_get( d, "SumBStor", start=nc.start.vec.g4, count=nc.count.vec )
    tmp.g4 <- tmp.g4 + ncvar_get( d, "SumBRepr", start=nc.start.vec.g4, count=nc.count.vec )
    
    tmp.g3 <-          ncvar_get( d, "SumBLeaf", start=nc.start.vec.g3, count=nc.count.vec )
    tmp.g3 <- tmp.g3 + ncvar_get( d, "SumBRoot", start=nc.start.vec.g3, count=nc.count.vec )
    tmp.g3 <- tmp.g3 + ncvar_get( d, "SumBStor", start=nc.start.vec.g3, count=nc.count.vec )
    tmp.g3 <- tmp.g3 + ncvar_get( d, "SumBRepr", start=nc.start.vec.g3, count=nc.count.vec )
    
    tmp.tr <- tmp.tr/20
    tmp.g4 <- tmp.g4/2
    tmp.g3 <- tmp.g3/2
  }
  
  dimnames(tmp.tr) <- dim.names
  dimnames(tmp.g4) <- dim.names
  dimnames(tmp.g3) <- dim.names

  # prepare data.table from the slice (array) for each PFT
  tr.dt <- as.data.table(melt(tmp.tr))
  names(tr.dt) <- c("Lon", "Lat", "Time", "Tree")
  setkey(tr.dt, Lon, Lat, Time)
  
  g3.dt <- as.data.table(melt(tmp.g3))
  names(g3.dt) <- c("Lon", "Lat", "Time", "C3G")
  setkey(g3.dt, Lon, Lat, Time)
  
  g4.dt <- as.data.table(melt(tmp.g3))
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
  
  print(out.all)


  # Now that we have the data we can set a spatial.extent
  actual.sta.info@spatial.extent <- extent(out.all)
  
  
  return(list(dt = out.all, 
              sta.info = actual.sta.info))
}

#' Slightty more complex classification of yearly output for aDGVM
#' 
#' Original from Simon Scheiter, this version modified by M. Forrest to fit into the DGVM framework. 
#' Classifies as evergreen tree, deciduous tree, evergreen shrub, deciduous shrub, C3 grass, C4 grass.
#' 
#' @param run A \code{Source}
#' @param first.year First year of data to read (not currently used)
#' @param last.year Last year of data to read (not currently used)
#' @param variable A character string specifying which variable to get, only "agb" currently supported
#'
#' @author Simon Scheiter \email{simon.scheiter@@senckenberg.de}, Matthew Forrest \email{matthew.forrest@@senckenberg.de}
#' @import ncdf4
#' @keywords internal
#' @seealso \code{getQuantity_aDGVM_Scheme1}
getQuantity_aDGVM_Scheme2 <- function(run,variable, first.year, last.year)
{
  fname <- file.path(run@dir, paste("trait_", run@id,".nc", sep=""))
  
  cat( "Convert", fname, "\n" )
  d <- nc_open(fname)
  
  xx <- ncvar_get(d,"lon")
  yy <- ncvar_get(d,"lat")
  tt <- ncvar_get(d,"time")
  len_tt <- length(tt)
  # process only the last nyears years of the simulation run
  nyears <- 1
  t_seq <- (len_tt-nyears+1):len_tt
  
  max_pop_size <- length(ncvar_get(d,"individual"))          # maximum population size
  
  cnames  <- c( "Lon", "Lat", "Year", "TrBE", "TrBR", "TrBES", "TrBRS", "C4G", "C3G", "Total")
  out.all <- matrix(0, ncol=length(cnames), nrow=0)
  colnames(out.all) <- cnames
  
  for ( x in 1:length(xx) )
  {
    cat( "Progress:", round(x/length(xx)*100,1), "%                                     \r")
    for ( y in 1:length(yy) )
    {
      for ( z in t_seq )
      {
        alive       <- ncvar_get( d, "alive",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        vegtp       <- ncvar_get( d, "VegType",   start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        pheno       <- ncvar_get( d, "Evergreen", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        stems       <- ncvar_get( d, "StemCount", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
        
        ind.alive <- which( alive==1 )
        ind.te    <- which( alive==1 & vegtp==0 & pheno==1 & stems<=2.5 )   # indices of evergreen trees in trait data
        ind.td    <- which( alive==1 & vegtp==0 & pheno==0 & stems<=2.5 )   # indices of deciduous trees in trait data
        ind.se    <- which( alive==1 & vegtp==0 & pheno==1 & stems> 2.5 )   # indices of evergreen shrubs in trait data
        ind.sd    <- which( alive==1 & vegtp==0 & pheno==0 & stems> 2.5 )   # indices of deciduous shrubs in trait data
        ind.g4    <- which( alive==1 & vegtp==1 )                           # indices of grasses in trait data
        ind.g3    <- which( alive==1 & vegtp==2 )                           # indices of grasses in trait data
        
        if(variable@id == "agb"){
          bm.leaf.all <- ncvar_get( d, "BLeaf",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.wood.all <- ncvar_get( d, "BWood",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.bark.all <- ncvar_get( d, "BBark",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          tmp.te <- sum(bm.wood.all[ind.te]+bm.bark.all[ind.te])
          tmp.td <- sum(bm.wood.all[ind.td]+bm.bark.all[ind.td])
          tmp.se <- sum(bm.wood.all[ind.se]+bm.bark.all[ind.se])
          tmp.sd <- sum(bm.wood.all[ind.sd]+bm.bark.all[ind.sd])
          tmp.g4 <- sum(bm.leaf.all[ind.g4])
          tmp.g3 <- sum(bm.leaf.all[ind.g3])
          
          tmp.te <- tmp.te*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.td <- tmp.td*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.se <- tmp.se*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.sd <- tmp.sd*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.g4 <- tmp.g4*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          tmp.g3 <- tmp.g3*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        if(variable@id == "vegC_std"){
          bm.leaf <- ncvar_get( d, "BLeaf",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.wood <- ncvar_get( d, "BWood",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.bark <- ncvar_get( d, "BBark",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.repr <- ncvar_get( d, "BRepr",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.stor <- ncvar_get( d, "BStor",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          bm.root <- ncvar_get( d, "BRoot",     start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          
          tmp.te <- sum(bm.leaf[ind.te]+bm.root[ind.te]+bm.stor[ind.te]+bm.repr[ind.te]+bm.wood[ind.te]+bm.bark[ind.te])
          tmp.td <- sum(bm.leaf[ind.td]+bm.root[ind.td]+bm.stor[ind.td]+bm.repr[ind.td]+bm.wood[ind.td]+bm.bark[ind.td])
          tmp.se <- sum(bm.leaf[ind.se]+bm.root[ind.se]+bm.stor[ind.se]+bm.repr[ind.se]+bm.wood[ind.se]+bm.bark[ind.se])
          tmp.sd <- sum(bm.leaf[ind.sd]+bm.root[ind.sd]+bm.stor[ind.sd]+bm.repr[ind.sd]+bm.wood[ind.sd]+bm.bark[ind.sd])
          tmp.g4 <- sum(bm.leaf[ind.g4]+bm.root[ind.g4]+bm.stor[ind.g4]+bm.repr[ind.g4])
          tmp.g3 <- sum(bm.leaf[ind.g3]+bm.root[ind.g3]+bm.stor[ind.g3]+bm.repr[ind.g3])
          
          tmp.te <- tmp.te*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.td <- tmp.td*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.se <- tmp.se*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.sd <- tmp.sd*1/1000*(1/20)  # convert from kg/ha to kgC/m^2
          tmp.g4 <- tmp.g4*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          tmp.g3 <- tmp.g3*1/1000*(1/2)   # convert from kg/ha to kgC/m^2
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        
        if(variable@id == "LAI_std"){
          tmp.all  <- ncvar_get( d, "Lai", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          tmp.te   <- mean(tmp.all[ind.te])*(length(ind.te)/length(alive))
          tmp.td   <- mean(tmp.all[ind.td])*(length(ind.td)/length(alive))
          tmp.se   <- mean(tmp.all[ind.se])*(length(ind.se)/length(alive))
          tmp.sd   <- mean(tmp.all[ind.sd])*(length(ind.sd)/length(alive))
          tmp.g4   <- mean(tmp.all[ind.g4])*(length(ind.g4)/length(alive))
          tmp.g3   <- mean(tmp.all[ind.g3])*(length(ind.g3)/length(alive))
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        if(variable@id == "basalarea"){
          tmp.all  <- ncvar_get( d, "StemDiamTot", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          tmp.te   <- sum(tmp.all[ind.te])
          tmp.td   <- sum(tmp.all[ind.td])
          tmp.se   <- sum(tmp.all[ind.se])
          tmp.sd   <- sum(tmp.all[ind.sd])
          tmp.g4   <- sum(tmp.all[ind.g4])
          tmp.g3   <- sum(tmp.all[ind.g3])
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        if(variable@id == "nind"){
          tmp.te   <- length(ind.te)
          tmp.td   <- length(ind.td)
          tmp.se   <- length(ind.se)
          tmp.sd   <- length(ind.sd)
          tmp.g4   <- length(ind.g4)
          tmp.g3   <- length(ind.g3)
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        if(variable@id == "meanheight" | variable@id == "canopyheight_std"){
          tmp.all  <- ncvar_get( d, "Height", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          tmp.te   <- mean(tmp.all[ind.te])
          tmp.td   <- mean(tmp.all[ind.td])
          tmp.se   <- mean(tmp.all[ind.se])
          tmp.sd   <- mean(tmp.all[ind.sd])
          tmp.g4   <- mean(tmp.all[ind.g4])
          tmp.g3   <- mean(tmp.all[ind.g3])
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        if(variable@id == "pind"){
          tmp.te   <- length(ind.te)
          tmp.td   <- length(ind.td)
          tmp.se   <- length(ind.se)
          tmp.sd   <- length(ind.sd)
          tmp.g4   <- length(ind.g4)
          tmp.g3   <- length(ind.g3)
          
          ind.tot  <- tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3
          tmp.te   <- tmp.te/ind.tot
          tmp.td   <- tmp.td/ind.tot
          tmp.se   <- tmp.se/ind.tot
          tmp.sd   <- tmp.sd/ind.tot
          tmp.g4   <- tmp.g4/ind.tot
          tmp.g3   <- tmp.g3/ind.tot
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        if(variable@id == "vegcover_std"){
          tmp.all  <- ncvar_get( d, "CrownArea", start=c( x,y,1,z ), count=c( 1,1,max_pop_size,1) )
          tmp.te   <- sum(tmp.all[ind.te])/20000
          tmp.td   <- sum(tmp.all[ind.td])/20000
          tmp.se   <- sum(tmp.all[ind.se])/20000
          tmp.sd   <- sum(tmp.all[ind.sd])/20000
          tmp.g4   <- sum(tmp.all[ind.g4])/10000
          tmp.g3   <- sum(tmp.all[ind.g3])/10000
          
          #ind.tot  <- tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3
          tmp.te   <- tmp.te*100  # *100 to convert to %
          tmp.td   <- tmp.td*100  # *100 to convert to %
          tmp.se   <- tmp.se*100  # *100 to convert to %
          tmp.sd   <- tmp.sd*100  # *100 to convert to %
          tmp.g4   <- tmp.g4*100  # *100 to convert to %
          tmp.g3   <- tmp.g3*100  # *100 to convert to %
          
          #            Lon      Lat    Year            TrBE    TrBR    TrBES   TrBRS   C4G     C3G     Total
          out.vec <- c( xx[x],  yy[y], ceiling(tt[z]), tmp.te, tmp.td, tmp.se, tmp.sd, tmp.g4, tmp.g3, tmp.te+tmp.td+tmp.se+tmp.sd+tmp.g4+tmp.g3 )
          out.all <- rbind( out.all, out.vec )
        }
        
        
        if(variable@id == "aGPP_std"){
          message(paste(variable@id, "not available in scheme"))
          out.all <- rbind( out.all, rep(0,length(cnames)) )
          return(out.all)
        }
        
        if(variable@id == "firefreq" | variable@id == "burntfraction_std" ){
          out.all <- rbind( out.all, rep(0,length(cnames)) )
          return(out.all)
        }
        
      }
    }
  }
  cat( "\n")
  
  return(out.all)
  
}

#' Detemine PFTs present in an aDGVM  
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


determineQuantities_aDGVM <- function(source){
  
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
  
  C3G = new("PFT",
            id = "C3G",
            name = "Boreal/Temperate Grass",
            growth.form = "Grass",
            leaf.form = "Broadleaved",
            phenology = "GrassPhenology",
            climate.zone = "NA",
            colour = "lightgoldenrod1",
            shade.tolerance = "None"
  ),
  
  C4G = new("PFT",
            id = "C4G",
            name = "Tropical Grass",
            growth.form = "Grass",
            leaf.form = "Broadleaved",
            phenology = "GrassPhenology",
            climate.zone = "NA",
            colour = "sienna2",
            shade.tolerance = "None"
  ),
  
  Tr= new("PFT",
          id = "Tr",
          name = "Tropical Tree",
          growth.form = "Tree",
          leaf.form = "Broadleaved",
          phenology = "Evergreen",
          climate.zone = "Tropical",
          colour = "palevioletred",
          shade.tolerance = "None"
  ),
  
  TrBE = new("PFT",
             id = "TrBE",
             name = "Tropical Broadleaved Evergreen Tree",
             growth.form = "Tree",
             leaf.form = "Broadleaved",
             phenology = "Evergreen",
             climate.zone = "Tropical",
             colour = "orchid4",
             shade.tolerance = "None"
  ),
  
  TrBR = new("PFT",
             id = "TrBR",
             name = "Tropical Broadleaved Raingreen Tree",
             growth.form = "Tree",
             leaf.form = "Broadleaved",
             phenology = "Raingreen",
             climate.zone = "Tropical",
             colour = "palevioletred",
             shade.tolerance = "None"
  ),
  
  TrBES = new("PFT",
              id = "TrBE",
              name = "Tropical Broadleaved Evergreen Shrub",
              growth.form = "Shrub",
              leaf.form = "Broadleaved",
              phenology = "Evergreen",
              climate.zone = "Tropical",
              colour = "orchid4",
              shade.tolerance = "None"
  ),
  
  TrBRS = new("PFT",
              id = "TrBR",
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
#' 
#' 
aDGVM.quantities <- list(
  
  
  new("Quantity",
      id = "agb",
      name = "Above Ground Biomass",
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
      format = c("aDGVM")),
  
  new("Quantity",
      id = "firefreq",
      name = "Fire Frequency",
      units = "",
      colours = reversed.fire.palette,
      format = c("aDGVM"))
  
  
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
#' 
#' 
aDGVM <- new("Format",
             
             # UNIQUE ID
             id = "aDGVM",
             
             # FUNCTION TO LIST ALL PFTS APPEARING IN A RUN
             determinePFTs = determinePFTs_aDGVM,
             
             # FUNCTION TO LIST ALL QUANTIES AVAILABLE IN A RUN
             determineQuantities = determineQuantities_aDGVM,
             
             # FUNCTION TO READ A FIELD 
             getField = getField_aDGVM,
             
             # DEFAULT GLOBAL PFTS  
             default.pfts = aDGVM.PFTs,
             
             # QUANTITIES THAT CAN BE PULLED DIRECTLY FROM LPJ-GUESS RUNS  
             quantities = aDGVM.quantities
             
)
