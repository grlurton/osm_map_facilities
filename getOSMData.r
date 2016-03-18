library(osmar)
library(maptools)
library(reshape2)

## Getting Bounding Box for Nigeria
NigeriaShp <-  readShapePoly('J://Project/phc/nga/dhis/LGAMap.shp')
Nigeriabbox <- bbox(NigeriaShp)

## Extracting all node type Data in OSM
url <- paste("http://www.overpass-api.de/api/xapi?" , 'node' , '[bbox=' ,
             Nigeriabbox[1,1], ',' ,Nigeriabbox[2,1], ',' , Nigeriabbox[1,2], ',' , Nigeriabbox[2,2] ,
             '][name=*]' ,
             sep = '')

response <- getURL(url)       
resp <- xmlParse(response)
osmardata <- as_osmar(resp)
shapefile <- as_sp(osmardata, what="points")


##Crop to the size of Nigeria

NigeriaShp@proj4string <- shapefile@proj4string

osm_data <- shapefile[!is.na(over(shapefile , NigeriaShp)$UnitName) ,]

plot(shapefile)
plot(osm_data , add = T , col = 'red')







## Reshaping a few names
tt <- osmardata$nodes$tags
tt$k <- as.character(tt$k)
tt$k <- gsub(':' , '_'  ,tt$k)
tt$k <- gsub('/' , '_'  ,tt$k)
tt$k <- gsub(' ' , '_'  ,tt$k)
tt$k <- gsub('__' , '_'  ,tt$k)
length(unique(tt$k))

reshTags <- dcast(tt , id ~ k)

colnames(reshTags)[1] <- colnames(shapefile@data)[1] <- 'idtoMatch'

shapefile@data <- merge(shapefile@data , reshTags , by = 'idtoMatch' ,
                        sort = FALSE)













plot(NigeriaShp)
plot(shapefile , add = TRUE)

plot(NigeriaShp)
plot(shapefile , col = factor(shapefile@data$amenity) , add = TRUE)

t <- function(data){
  for(i in 1:ncol(data)){
    if(is.character(data[,i]) + is.numeric(data[,i]) +
         is.factor(data[,i]) < 1){
      data[,i] <- as.character(data[,i])
    }
  }
  data
}

shapefile@data <- t(shapefile@data)

writePointsShape(osm_data, "OSMDataNigeria")



rm(list = ls()[ls() != c('NigeriaShp' , 'osm_data')])

