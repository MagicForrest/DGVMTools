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
#' @include classes.R
#' @name veg.palettes
NULL

#' @rdname veg.palettes
#' @export
veg.palette <- grDevices::colorRampPalette(c("white", "darkolivegreen1", "darkolivegreen4", "saddlebrown", "black"))

#' @rdname veg.palettes
#' @export
lai.palette <- grDevices::colorRampPalette(c("blue", "lightskyblue1", "palevioletred", "khaki1", "yellowgreen", "forestgreen", "saddlebrown","black" )) #this is a function which returns a list of colours

#' @rdname veg.palettes
#' @export
cmass.palette <- grDevices::colorRampPalette(c("lemonchiffon","peru", "forestgreen", "dodgerblue4", "orchid4", "hotpink", "red4"))

#' @rdname veg.palettes
#' @export
difference.palette <- function(n){
  
  # Make palette based on RColorBrewer "RdBu" palette
  rdbu <- rev(RColorBrewer::brewer.pal(11, "RdBu"))
  cols <- grDevices::colorRampPalette(rdbu)
  return(cols(n))  
  
  #grDevices::colorRampPalette(c("green", "blue", "white", "red", "yellow")) #this is a function which returns a list of colours
  
}

#' @rdname veg.palettes
#' @export
fire.palette <- grDevices::colorRampPalette(c("red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3"))

#' @rdname veg.palettes
#' @export
reversed.fire.palette <- grDevices::colorRampPalette(rev(c("red4", "red","orange","yellow", "olivedrab2", "chartreuse3", "chartreuse4", "skyblue", "blue", "blue3")))

#' @rdname veg.palettes
#' @export
reversed.tim.colors = function(n) rev(fields::tim.colors(n))

#' @rdname veg.palettes
#' @export
reversed.viridis = function(n) rev(viridis::viridis(n))

#' @rdname veg.palettes
#' @export
reversed.magma = function(n) rev(viridis::magma(n))

#' @rdname veg.palettes
#' @export
reversed.inferno = function(n) rev(viridis::inferno(n))

#' @rdname veg.palettes
#' @export
reversed.plasma = function(n) rev(viridis::plasma(n))