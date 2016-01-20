############## RECYCLING BIN and SCRATCH PAD #####################
# 
# Here keep keep code and functions whci are obselete but why may still be useful
#!/usr/bin/Rscript

######################################
###
###         ©©©©©©©©©©©©©
###       ©©©©©©©©©©©©©©©©©
###      ©©©             ©©©
###     ©©©   ©©©©©©©©    ©©©
###    ©©©   ©©       ©©   ©©©
###   ©©©   ©©         ©©   ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©               ©©  ©©©
###   ©©©   ©©         ©©   ©©©
###    ©©©   ©©       ©©   ©©©
###     ©©©    ©©©©©©©    ©©© 
###      ©©©             ©©©   
###       ©©©©©©©©©©©©©©©©© 
###         ©©©©©©©©©©©©©  
###
###  COPYLEFT:  ALL RIGHTS REVERSED
###
#####################################




### LAI PFT PLOTTING MINI-PACKAGE ########################### 

# Here define the the colours and limits
forrest.nlevel = 50
forrest.palette = colorRampPalette(c("blue", "lightskyblue1", "palevioletred", "khaki1", "yellowgreen", "forestgreen", "saddlebrown","black" )) #this is a function which returns a list of colours
forrest.colours = forrest.palette(forrest.nlevel)
forrest.zlim = c(log(0.06),log(8))
forrest.ticks<- c(0.06, 0.125, 0.25, 0.5, 1, 2,4,8)

lai.diff.nlevel = 40
lai.diff.palette = colorRampPalette(c("green", "blue", "white", "red", "yellow")) #this is a function which returns a list of colours
lai.diff.colours = lai.diff.palette(lai.diff.nlevel)
lai.diff.zlim = c(-1,1)
lai.diff.ticks<- c(-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1.0)

################################################################
###### CMASS PFT PLOTTING MINI-PACKAGE #########################
################################################################

Biomass.abs.colours.palette = colorRampPalette(c("lemonchiffon","peru", "forestgreen", "dodgerblue4", "orchid4", "hotpink", "red4"))
Biomass.abs.nlevel = 20
Biomass.abs.colours = Biomass.abs.colours.palette(Biomass.abs.nlevel)
Biomass.abs.zlim = c(log(0.044194174),log(45.254833996))
Biomass.abs.ticks<- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32)



Biomass.diff.colours.palette = colorRampPalette(c("green","blue","white","red", "yellow")) #this is a function which returns a list of colours
Biomass.small.diff.colours.palette = colorRampPalette(c("blue","white","red")) #this is a function which returns a list of colours
Biomass.diff.nlevel = 21
Biomass.small.diff.colours = Biomass.small.diff.colours.palette(Biomass.diff.nlevel)
Biomass.diff.colours = Biomass.diff.colours.palette(Biomass.diff.nlevel)
Biomass.diff.zlim = c(-21,21)
Biomass.diff.ticks =  seq(-20,20,5)


Biomass.percdiff.colours.palette = colorRampPalette(c("orchid4","blue","turquoise2","white","yellow", "orange","red4")) #this is a function which returns a list of colours
Biomass.percdiff.nlevel = 21
Biomass.percdiff.colours = Biomass.percdiff.colours.palette(Biomass.percdiff.nlevel)
Biomass.percdiff.zlim = c(-350,350)


### WATER CONTENT PLOTTING MINI-PACKAGE ###########################

# Here define the the colours and limits
wcont.nlevel = 20
wcont.colours = tim.colors(wcont.nlevel)[wcont.nlevel:1]
wcont.zlim = c(0,1)
wcont.ticks =  seq(0,1,0.1)

wcont.diff.nlevel = 20
wcont.diff.palette = colorRampPalette(c("orchid4","blue","turquoise2","white","yellow", "orange","red4"))
wcont.diff.colours = wcont.diff.palette(wcont.diff.nlevel)[wcont.diff.nlevel:1]
wcont.diff.zlim = c(-1,1)
wcont.diff.ticks =  seq(-1,1,0.1)




### FIRE INTENSITY PLOTTING MINI-PACKAGE ###########################

# Here define the the colours and limits
fire.intens.nlevel = 25
fire.intens.colours = tim.colors(fire.intens.nlevel)
fire.intens.zlim = c(0,50)
fire.intens.ticks =  seq(0,50,2)

### FIRE REISDENCE time PLOTTING MINI-PACKAGE ###########################

# Here define the the colours and limits
residence.time.nlevel = 20
residence.time.colours = tim.colors(residence.time.nlevel)
residence.time.zlim = c(0,20)
residence.time.ticks =  seq(0,20,2)

### FIRE REISDENCE time PLOTTING MINI-PACKAGE ###########################

# Here define the the colours and limits
fuel1hr.nlevel = 50
fuel1hr.colours = tim.colors(fuel1hr.nlevel)
fuel1hr.zlim = c(0,1000)
fuel1hr.ticks =  seq(0,1000,100)

### GLOBFIRM ###########################

# Here define the the colours and limits
firert.diff.nlevel = 20
firert.diff.colours = tim.colors(firert.diff.nlevel)
firert.diff.zlim = c(-1000,1000)
firert.diff.ticks =  seq(-1000,1000,100)


buildColumListToAverage <- function(input.dt){
  
  # make a list of the columns to be average 
  # we are assuming all except "Lon", "Lat" and "year"
  cols.to.average <- "list("
  for(name in names(input.dt)){
    if(name != "Lon" & name != "Lat" & name != "Year"){
      cols.to.average <- paste0(cols.to.average, name, "=mean(", name, ")", sep = "")
      if(name != tail(names(input.dt), n=1)){
        cols.to.average <- paste0(cols.to.average, ",", sep = "")
      }
    }
    if(name == tail(names(input.dt), n=1)){
      cols.to.average <- paste0(cols.to.average, ")", sep = "")
    }
  }
  cols.to.average <- parse(text = cols.to.average)
}
