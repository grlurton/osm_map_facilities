library(maptools)
library(plyr)

DHISFacilities <- read.csv('J://Project/phc/nga/dhis/HierarchyData.csv')

##Prepare osm names to be used in the matching May be a bit more standardization here !!

health_projects@data$name <- gsub('\\{|\\}' , '-' , health_projects@data$name)



## Match the exact names of facilities

validation_set <- data.frame(osm_ID = character() , osm_name = character() , dhis_ID = character() , dhis_name = character(), LGA = character() , state = character()  , ward = character())
for (LGA in NigeriaShp@data$UnitName){
  print(LGA)
  osm_name <- mm <-  NA
  facilities <- DHISFacilities$Level5[DHISFacilities$Level3 == LGA]
  facs_osm <- over(NigeriaShp[NigeriaShp$UnitName == LGA , ] , health_projects  , returnList = TRUE)
  lga_facilities <- health_projects[health_projects$idtoMatch %in% facs_osm[[1]]$idtoMatch , ]
  if(nrow(lga_facilities@data) > 0){
    for(i in seq(nrow(lga_facilities@data))){
      out <- data.frame(osm_ID = character() , osm_name = character() , dhis_ID = character() , dhis_name = character(), LGA = character() , state = character() , ward = character())
      if (substr(LGA , 1 ,2) %in% c('ba' , 'bo','kn')){
        osm_name <- lga_facilities$name[i]
        }
      if (substr(LGA , 1 ,2) %in% c('ed')){
        osm_name <- lga_facilities$primary_na[i]
      }
      if (!is.na(osm_name)){ 
        mm <- grep(osm_name, as.character(facilities) , ignore.case = FALSE , value = TRUE)
      }
      if (length(mm) == 1  & !is.na(osm_name)){
        print(paste('     ' , mm))
        out <- data.frame(osm_ID = lga_facilities$idtoMatch[i]  , osm_name = osm_name ,
                          dhis_ID = DHISFacilities$Level5ID[DHISFacilities$Level5 == mm] ,
                          dhis_name = mm , 
                          LGA = DHISFacilities$Level3[DHISFacilities$Level5 == mm] ,
                          state = DHISFacilities$Level2[DHISFacilities$Level5 == mm] ,
                          ward = DHISFacilities$Level2[DHISFacilities$Level5 == mm] )
      }
      validation_set <- rbind(validation_set , out)
    }
  }
}

ValidationData <- health_projects[health_projects$idtoMatch %in% validation_set$osm_ID ,]
ValidationData@data <- merge(ValidationData@data , validation_set , by.x = 'idtoMatch' ,
                             by.y = 'osm_ID' , sort = F)


plot(NigeriaShp)
plot(ValidationData , col = factor(ValidationData@data$state) , add = TRUE)


## Taking out multi matches
doublons <- ddply(validation_set , .(dhis_name) , nrow)
doublons <- as.character(doublons$dhis_name[doublons$V1 > 1])

validation_set <- subset(validation_set , !(dhis_name %in% doublons))

plot(NigeriaShp)
plot(ValidationData , col = factor(ValidationData@data$state) , add = TRUE)

writePointsShape(ValidationData, "data/ValidationSet")

rm(out , doublons , facs_osm , i , LGA , lga_facilities , mm , osm_name)
