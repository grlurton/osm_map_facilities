library(osmar)
library(maptools)
library(reshape2)
library(plyr)

## Getting Bounding Box for Nigeria
NigeriaShp <-  readShapePoly('C://Users/grlurton/Desktop/result_zip_shp/NIGERIA_LGA.shp')
Nigeriabbox <- bbox(NigeriaShp)

## Extracting all node type Data in OSM
url <- paste("http://www.overpass-api.de/api/xapi?" , '*' , '[bbox=' ,
             Nigeriabbox[1,1], ',' ,Nigeriabbox[2,1], ',' , Nigeriabbox[1,2], ',' , Nigeriabbox[2,2] ,
             '][name=*]' ,
             sep = '')

response <- getURL(url)       
resp <- xmlParse(response)
osmardata <- as_osmar(resp)

points <- as_sp(osmardata, what="points")
lines <- as_sp(osmardata, what="lines")

##Crop to the size of Nigeria
NigeriaShp@proj4string <- points@proj4string

osm_points <- points[!is.na(over(points , NigeriaShp)$lganame) ,]
osm_lines <- lines[!is.na(over(lines , NigeriaShp)$lganame) ,]


## Reshaping a few names
get_tags <- function(OsmarTags){
  OsmarTags$k <- as.character(OsmarTags$k)
  OsmarTags$v <- as.character(OsmarTags$v)
  return(OsmarTags)
}

drop_rare_tags <- function(OsmarTags , n = 10){
  tags <- get_tags(OsmarTags)
  content_tags <- ddply(tags , .(k) , nrow)
  to_keep <- content_tags$k[content_tags$V1 > n]
  OsmarTags <- OsmarTags[OsmarTags$k %in% to_keep ,]
  return(OsmarTags)
}

drop_useless_tags <- function(OsmarTags , useless_tags){
  OsmarTags <- OsmarTags[!(OsmarTags$k %in% useless_tags ),]
  return(OsmarTags)
}


rename_tags <- function(OsmarTags ,  patterns_remove){
  tags <- OsmarTags
  for(pattern in patterns_remove){
    tags$k <- gsub(pattern , '_'  , tags$k)
  }
  tags$k <- gsub('__' , '_' , tags$k)
  OsmarTags <- tags
  return(OsmarTags)
}


useless_tags <- c("GNS:id" , "gns:dsg" , "GNS:dsg_code" , "GNS:dsg_string" ,
                  "is_in:country" , "fixme" , 'wikipedia' ,'name_ru' , 'name_uk' , 'northing' , 'NORTHING')

patterns_remove <- c(':' , '/'  , ' ')


osmar_point_tags <- osmardata[['nodes']]$tags[osmardata[['nodes']]$tags$id %in% osm_points$id ,]


Osmar_cleaned <- drop_rare_tags(osmar_point_tags)
Osmar_cleaned <- drop_useless_tags(Osmar_cleaned , useless_tags)
Osmar_cleaned <- rename_tags(Osmar_cleaned , patterns_remove)


tags_in_shp <- function(osmar_cleaned , osm_shape){
  reshTags <- dcast(osmar_cleaned , id ~ k ,  value.var = 'v')
  
  colnames(reshTags)[1] <- colnames(osm_shape@data)[1] <- 'idtoMatch'
  
  data_keep <- merge(osm_shape@data , reshTags , by = 'idtoMatch' ,
                           sort = FALSE , all.y = FALSE)
  
  osm_shape <- osm_shape[osm_shape$idtoMatch %in% data_keep$idtoMatch ,]
  osm_shape@data <- data_keep
  
  return(osm_shape)
}




tags_to_character <- function(osm_shape){
    for(i in 1:ncol(osm_shape)){
      if(is.character(osm_shape[,i]) + is.numeric(osm_shape[,i]) +
          is.factor(osm_shape[,i]) < 1){
        osm_shape[,i] <- as.character(osm_shape[,i])
      }
    }
    return(osm_shape)
}

osm_points2 <- tags_in_shp(Osmar_cleaned , osm_points)
osm_points2@data <- tags_to_character(osm_points2@data)
osm_points2 <- osm_points2[!is.na(osm_points2$name) , ]



writePointsShape(osm_points2, "data/OSMDataNigeria")

rm(list = ls()[!(ls() %in% c('NigeriaShp' , 'osm_data'))])
