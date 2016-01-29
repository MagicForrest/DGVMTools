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
  
	write.table( out.abm, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_yearly_agbm", sep=""), row.names=F, quote=F )
	write.table( out.lai, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_yearly_lai",  sep=""), row.names=F, quote=F )

  
  
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

	write.table( out.abm, file=paste("aDGVM2_", runid, "_", fire, "_scheme2_yearly_agbm", sep=""), row.names=F, quote=F )

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

	write.table( out.abm.tr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_Tr_agbm", sep=""), row.names=F, quote=F )
	write.table( out.lai.tr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_Tr_lai",  sep=""), row.names=F, quote=F )

	write.table( out.abm.gr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_C4G_agbm", sep=""), row.names=F, quote=F )
	write.table( out.lai.gr, file=paste("aDGVM2_", runid, "_", fire, "_scheme1_monthly_C4G_lai",  sep=""), row.names=F, quote=F )

}




  ################################# GET TIME-AVERAGED DATA #########################################

getVegQuantity_aDGVM_Scheme1 <- function(run, period, variable)
{
  fname <- file.path(run@run.dir, paste("pop_", run@id,".nc", sep=""))

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
  
  write.table(out.abm, file=file.path(run@run.dir, paste("aDGVM2_", run@id, "_scheme1_yearly_agbm", sep="")), row.names=F, quote=F )
  write.table(out.lai, file=file.path(run@run.dir, paste("aDGVM2_", run@id, "_scheme1_yearly_lai",  sep="")), row.names=F, quote=F )
  
  
  if(variable@id == "lai"){
    return(out.lai)
  }
  else if(variable@id == "agb"){
    return(out.abm)
  }
  
  
}


getVegQuantity_aDGVM_Scheme2 <- function(run, period, variable)
{
  fname <- file.path(run@run.dir, paste("trait_", run@id,".nc", sep=""))
  
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
  
  print(out.abm)
  
  if(variable@id == "lai"){
    fail("LAI not yet defined for aDGVM scheme 2")
  }
  else if(variable@id == "agb"){
    return(out.abm)
  }
  
}



# ----------------------------------------------------

#convertYearlyScheme1(  44, 0, "convert_to_LPJ_format" )
#convertYearlyScheme2(  44, 0, "convert_to_LPJ_format" )

#convertMonthlyScheme1( 44, 0, "convert_to_LPJ_format" )



