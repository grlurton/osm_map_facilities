library(maptools)
library(plyr)
library(RCurl)
library(XML)
library(shapefiles)

orgUnits <- read.csv('ExtractOrgUnitsRaw.csv')
MetaData <- read.csv('MetadataUnitsRaw.csv')

LGA <- subset(MetaData , UnitLevel == 3)

LGAData <- subset(orgUnits , UnitName %in% LGA$UnitName)

##Return GPS coordinates as a string
getGPS <- function(url){
  Page<-getURL(url,userpwd="grlurton:Glurton29",
               ssl.verifypeer = FALSE , httpauth = 1L)
  ParsedPage <- xmlParse(Page)
  root <- xmlRoot(ParsedPage)
  unitcoordinates <- xmlValue(root[["coordinates"]])
  unitcoordinates <- substr(unitcoordinates , 4 , nchar(unitcoordinates) - 3)
  unitcoordinates
}

## Returns the GPS coordinates in tabular format
tabGPS <- function(string_coordinates){
  string_coordinates <- substr(string_coordinates , 2 , nchar(string_coordinates) - 1)
  tabCoord <- data.frame(unlist(strsplit(string_coordinates , "],[" , fixed = TRUE)))
  splitcoord <- unlist(strsplit(as.character(tabCoord[,1]) , "," , fixed = TRUE))
  coord <- data.frame(long = as.numeric(splitcoord[2*(1:(length(splitcoord)/2))-1]) ,
                      lat = as.numeric(splitcoord[2*(1:(length(splitcoord)/2))]) )
  if (!is.na(coord$long) & !is.na(coord$lat)){
    if (coord$long[1] != coord$long[nrow(coord)]) coord$long <- NA
  }
  coord
}

Coordinates <- ddply(LGAData , .(UnitName) ,
                     function(x) {print(x$UnitName)
                                  tabGPS(getGPS(paste(x$UnitAdress , "xml" , sep = '.')))} ,
                     .progress = 'text')
Coordinates <- subset(Coordinates , !is.na(long) & !is.na(lat) )

##Creating data frame to be included in the shapefile
ShapeData <- data.frame(UnitName = unique(Coordinates$UnitName) , country = "Nigeria")

Shapefile <- convert.to.shapefile(Coordinates, ShapeData, "UnitName", 5)
write.shapefile(Shapefile, "LGAMap", arcgis=T)

Shapefile <- readShapePoly("LGAMap.shp")

plot(Shapefile , col = "grey")
