

########## STANDARD LPJ-GUESS HALF DEGREE GRIDLIST
lpj.HD.gridlist <- file.path(gridlist.dir, "gridlist_global_0.5deg.txt")


# read gridlist and make 
makeGridlistMask <- function(gridlist.file = lpj.HD.gridlist, offset = c(0.25, 0.25)){
  
  gridlist <-read.table(gridlist.file, header = FALSE)
  setnames(gridlist, "V1", "Lon")
  setnames(gridlist, "V2", "Lat")
  
  gridlist$Lon <- gridlist$Lon + offset[1]
  gridlist$Lat <- gridlist$Lat + offset[2]
  
  gridlist$Present <- 1
  
  gridlist.points <- SpatialPoints(gridlist[,c("Lon", "Lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
  gridlist.pixels <- SpatialPixels(gridlist.points)
  gridlist.spdf <- SpatialPixelsDataFrame(gridlist.pixels, data.frame(gridlist[,c("Present")])) 
  
  gridlist.raster <- raster(gridlist.spdf)
  
  return(gridlist.raster)
  
}


subsetGridlist <- function(subset.extent, file.name = NULL, gridlist.file = lpj.HD.gridlist, offset = c(0.25, 0.25)){
  
  gridlist <-read.table(gridlist.file, header = FALSE)
  setnames(gridlist, "V1", "Lon")
  setnames(gridlist, "V2", "Lat")
  
  gridlist$Lon <- gridlist$Lon + offset[1]
  gridlist$Lat <- gridlist$Lat + offset[2]
  
  gridlist$Present <- 1
  
  gridlist.points <- SpatialPoints(gridlist[,c("Lon", "Lat")], proj4string = CRS("+proj=longlat +datum=WGS84"))
  gridlist.pixels <- SpatialPixels(gridlist.points)
  gridlist.spdf <- SpatialPixelsDataFrame(gridlist.pixels, data.frame(gridlist[,c("Present")])) 
  gridlist.raster <- raster(gridlist.spdf)
  
  cropped.gridlist.raster <- crop(gridlist.raster, subset.extent)
  cropped.gridlist.spdf <- as(cropped.gridlist.raster, "SpatialPixelsDataFrame")
  cropped.gridlist.spdf <- na.omit(cropped.gridlist.spdf)
  cropped.gridlist <- data.frame(cropped.gridlist.spdf)
  cropped.gridlist[1] <- NULL
  names(cropped.gridlist) <- c("Lon", "Lat")
  
  cropped.gridlist$Lon <- cropped.gridlist$Lon - offset[1]
  cropped.gridlist$Lat <- cropped.gridlist$Lat - offset[2]
  
  if(!is.null(file.name)) write.table(cropped.gridlist, file.name, quote = FALSE, row.names = FALSE)
  
  return(cropped.gridlist)
  
}



########### CONSTANTY STRINGS AND VECTORS ######################

##### Conversion factors 
kmsq_to_ha <- 100

months.2digits <- c(paste("0", 1:9, sep = ""), paste(10:12))
months.padded <- months.2digits 

months.plus.annual = list("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December","Annual")
seasons = list("Winter", "Spring", "Summer", "Autumn")
seasons.string = list("DJF", "MAM", "JJA", "SON")
all.times <-c(unlist(months),unlist(seasons.string), "Annual")
season.numbers = list(c(12,1,2), c(3,4,5), c(6,7,8), c(9,10,11))
season.abbreviations = list("DJF", "MAM", "JJA", "SON")

#make lists of the number of days in the month
#and the cumulative days up to a new month
days.in.month =list(31,28,31,30,31,30,31,31,30,31,30,31)
days.in.month.leap.year=list(31,28,31,30,31,30,31,31,30,31,30,31)
cum.days.in.month= list(31,59,90,120,151,181,212,243,273,304,334,365)
cum.days.in.month.leap.year= list(31,60,91,121,152,182,213,244,274,305,335,366)


##### Period - class to hold the metadata about a month, seasonal or annual period
months <- list(January = new("Period",
                             name = "January",
                             abbreviation = "Jan",
                             index = 1,
                             padded.index = "01",
                             days = 31,
                             days.leap = 31),
               February = new("Period",
                              name = "February",
                              abbreviation = "Feb",
                              index = 2,
                              padded.index = "02",
                              days = 28,
                              days.leap = 29),
               March = new("Period",
                           name = "March",
                           abbreviation = "Mar",
                           index = 3,
                           padded.index = "03",
                           days = 31,
                           days.leap = 31),
               April = new("Period",
                           name = "April",
                           abbreviation = "Apr",
                           index = 4,
                           padded.index = "04",
                           days = 30,
                           days.leap = 30),
               May = new("Period",
                         name = "May",
                         abbreviation = "May",
                         index = 5,
                         padded.index = "05",
                         days = 31,
                         days.leap = 31),
               Jun = new("Period",
                         name = "June",
                         abbreviation = "Jun",
                         index = 6,
                         padded.index = "06",
                         days = 30,
                         days.leap = 30),
               July = new("Period",
                          name = "July",
                          abbreviation = "Jul",
                          index = 7,
                          padded.index = "07",
                          days = 31,
                          days.leap = 31),
               August = new("Period",
                            name = "August",
                            abbreviation = "Aug",
                            index = 8,
                            padded.index = "08",
                            days = 31,
                            days.leap = 31),
               September = new("Period",
                               name = "September",
                               abbreviation = "Sep",
                               index = 9,
                               padded.index = "09",
                               days = 30,
                               days.leap = 30),
               October = new("Period",
                             name = "October",
                             abbreviation = "Oct",
                             index = 10,
                             padded.index = "10",
                             days = 31,
                             days.leap = 31),
               November = new("Period",
                              name = "November",
                              abbreviation = "Nov",
                              index = 11,
                              padded.index = "11",
                              days = 30,
                              days.leap = 30),
               December = new("Period",
                              name = "December",
                              abbreviation = "Dec",
                              index = 12,
                              padded.index = "12",
                              days = 31,
                              days.leap = 31)
)

seasons <- list(Winter = new("Period",
                             name = "Winter",
                             abbreviation = "DJF",
                             index = c(12,1,2),
                             padded.index = c("12", "01", "02"),
                             days = 60,
                             days.leap = 61),
                Spring = new("Period",
                             name = "Spring",
                             abbreviation = "MAM",
                             index = c(3,4,5),
                             padded.index = c("03", "04", "05"),
                             days = 62,
                             days.leap = 62),
                Summer = new("Period",
                             name = "Summer",
                             abbreviation = "JJA",
                             index = c(6,7,8),
                             padded.index = c("07", "08", "09"),
                             days = 62,
                             days.leap = 62),          
                Autumn = new("Period",
                             name = "Autumn",
                             abbreviation = "SON",
                             index = c(9,10,11),
                             padded.index = c("09", "10", "11"),
                             days = 61,
                             days.leap = 61)
)


annual <- new("Period",
              name = "Annual",
              abbreviation = "Ann",
              index = seq(1,12,1),
              padded.index = "Annual",
              days = 365,
              days.leap = 366
)

all.periods <- append(months, append(seasons, annual))


########### MEMORY MANAGEMENT ########### 

## Credit: Taken from: http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto")) })
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}


########### WORKAROUND TO USE DATA.TABLE FREAD() FUNCTION FOR VARIABLE WHITESPACE SEPARATORS ########### 


#credit: see here http://stackoverflow.com/questions/22229109/r-data-table-fread-command-how-to-read-large-files-with-irregular-separators

# Function to count rows with command wc for read.table() optimization.
fileRowsCount <- function(file){
  if(file.exists(file)){
    sysCmd <- paste("wc -l", file)
    rowCount <- system(sysCmd, intern=T)
    rowCount <- sub('^\\s', '', rowCount)
    as.numeric(
      strsplit(rowCount, '\\s')[[1]][1]
    )
  }
}

#credit: see here http://stackoverflow.com/questions/22229109/r-data-table-fread-command-how-to-read-large-files-with-irregular-separators

# function awkFread : first awk, then fread. Argument : colNums = selection of columns. 
awkFread<-function(file, colNums, ...){
  if(is.vector(colNums)){
    tmpPath<-tempfile(pattern='tmp',fileext='.txt')
    colGen<-paste0("$",colNums,"\",\"", collapse=",")
    colGen<-substr(colGen,1,nchar(colGen)-3)
    cmdAwk<-paste("awk '{print",colGen,"}'", file, '>', tmpPath)
    try(system(cmdAwk))
    DT<-fread(tmpPath,...)
    # MF Addition, remove spaces frome column names
    setnames(DT, old=names(DT), new=gsub("\\s","", names(DT)))
    try(system(paste('rm', tmpPath)))
    return(DT)
  }
}

########### HANDY PROCESSING FUNCTIONS ########### 


# Function to divide two number but return 0 if the denominator is 0
"%/0%" <- function(x,y) ifelse(y==0,0,base::"/"(x,y))



########### COLOUR SCHEMES AND PRETTY PLOTTING FUNCTIONS AND PARAMETERS ########### 

# a sort of dry/warm to cool/wet colour scheme 
rev.tim.colors = function(x)rev(tim.colors(x))

########### CONVERSION OF ABOVE-GROUND BIOMASS TO TOTAL CARBON ####################
### Used when reading the original Baccini et al. 2012 and Avitabile et al. 2015 dataset
### Reference Baccini et al. 2012

AGBtoTotalCarbon <- function(AGB){
  
  BGB <- 0.489 * AGB^0.89
  total.carbon <- (AGB + BGB) / 2
  return(total.carbon)
  
}

standard.continental.extents <- list(Global = new("SpatialExtent", id = "Global", name = "Global", extent = extent(-180, 180, -90, 90)),
                                     Africa = new("SpatialExtent", id = "Africa", name = "Africa", extent =  extent(-20, 55, -30, 36)),
                                     Europe = new("SpatialExtent", id = "Europe", name = "Europe", extent =  extent(-30, 40, 36, 70)),
                                     Asia = new("SpatialExtent", id = "Asia", name = "Asia", extent =  extent(40, 180, -10, 80)),
                                     NorthAmerica = new("SpatialExtent", id = "NorthAmerica", name = "North America", extent =  extent(-170, -70, 25, 75)),
                                     SouthAmerica = new("SpatialExtent", id = "SouthAmerica", name = "South America", extent = extent(-180, -50, -60, 25)),
                                     Australia = new("SpatialExtent", id = "Australia", name = "Australia", extent = extent(110, 160, -45 ,10)),
                                     Mediterranean = new("SpatialExtent", id = "Med", name = "Mediterranean", extent = extent(10, 40, 28 ,48)),
                                     CentralAsia = new("SpatialExtent", id = "CentralAsia", name = "Central Asia", extent = extent(25, 140, 40, 55)),
                                     SouthEastAsia = new("SpatialExtent", id = "SouthEastAsia", name = "South East Asia", extent = extent(90, 140, 10, 40)),
                                     CentralNorthAmerica = new("SpatialExtent", id = "CentralNorthAmerica", name = "Central North America", extent = extent(-110, -85, 30, 50)),
                                     Boreal = new("SpatialExtent", id = "Boreal", name = "Boreal", extent = extent(-180, 180, 60, 90)),
                                     NHAfrica = new("SpatialExtent", id = "NHAfrica", name = "Northern Hemisphere Africa", extent = extent(-20, 50, 0, 25)),
                                     SHAfrica = new("SpatialExtent", id = "SHAfrica", name = "Southern Hemisphere Africa", extent = extent(5, 50, -30, 0))
                                     
)

## define for older R versions, where this function does not exist.
## Just copied from a recent R version
if (!exists("OlsonNames")) {
  OlsonNames <- function()
  {
    if (.Platform$OS.type == "windows")
      tzdir <- Sys.getenv("TZDIR", file.path(R.home("share"),
                                             "zoneinfo"))
    else {
      tzdirs <- c(Sys.getenv("TZDIR"), file.path(R.home("share"),
                                                 "zoneinfo"), "/usr/share/zoneinfo", "/usr/share/lib/zoneinfo",
                  "/usr/lib/zoneinfo", "/usr/local/etc/zoneinfo", "/etc/zoneinfo",
                  "/usr/etc/zoneinfo")
      tzdirs <- tzdirs[file.exists(tzdirs)]
      if (!length(tzdirs)) {
        warning("no Olson database found")
        return(character())
      }
      else tzdir <- tzdirs[1]
    }
    x <- list.files(tzdir, recursive = TRUE)
    grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZ]", x, value = TRUE)
  }
}
