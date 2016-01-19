

source("/home/forrest/Tools/RVCTools/v2.8/rvc-tools.R")


list.of.subregions = list(
  "central.africa" = extent(-20,60,-20,20),
  "eastern.south.america" = extent(-55, -35,-25,0),
  "mediterranean" = extent(-10,40,35,45) ,
  "north.australia" = extent(110,155,-25,-10),
  "central.north.ameria" = extent(-115,-85,30,50),
  "central.eurasia" = extent(55, 115, 40, 55)
)

selected.gridlist <- data.frame()

for(region in list.of.subregions){
  
  print(names(region))
  
  selected.gridlist <- rbind(selected.gridlist, subsetGridlist(region) )
  
  
}

write.table(selected.gridlist, "/home/forrest/AuxiliaryData/Gridlists/gridlist_FireZones.txt", quote = FALSE, row.names = FALSE)