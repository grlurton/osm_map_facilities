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
par(mfrow = c(2,2))
plot(shapefile)

##Crop to the size of Nigeria

NigeriaShp@proj4string <- shapefile@proj4string

osm_data <- shapefile[!is.na(over(shapefile , NigeriaShp)$UnitName) ,]
plot(osm_data)

## Reshaping a few names
tt <- osmardata$nodes$tags
tt$k <- as.character(tt$k)
tt$v <- as.character(tt$v)

library(plyr)
n_values <- ddply(tt , .(k) , nrow)


## taking out indicators 
tt <- tt[!(tt$k %in% c("GNS:id" , "gns:dsg" , "GNS:dsg_code" , "is_in:country" , "GNS:dsg_string" , "fixme" , 
                       'wikipedia' ,
                       n_values$k[n_values$V1 < 7])) & 
                        substr(tt$k , 1, 5 ) != 'name:' & substr(tt$k , 1, 14) != 'official_name:' &
           !(tt$v %in% c('Cameroon','Chad','Benin')), ]
tt$k <- gsub(':' , '_'  ,tt$k)
tt$k <- gsub('/' , '_'  ,tt$k)
tt$k <- gsub(' ' , '_'  ,tt$k)
tt$k <- gsub('__' , '_'  ,tt$k)

reshTags <- dcast(tt , id ~ k ,  value.var = 'v')

colnames(reshTags)[1] <- colnames(osm_data@data)[1] <- 'idtoMatch'

osm_data@data <- merge(osm_data@data , reshTags , by = 'idtoMatch' ,
                        sort = FALSE , all.y = FALSE )



t <- function(data){
  for(i in 1:ncol(data)){
    if(is.character(data[,i]) + is.numeric(data[,i]) +
         is.factor(data[,i]) < 1){
      data[,i] <- as.character(data[,i])
    }
  }
  data
}

osm_data@data <- t(osm_data@data)

writePointsShape(osm_data, "data/OSMDataNigeria")


rm(list = ls()[!(ls() %in% c('NigeriaShp' , 'osm_data'))])
