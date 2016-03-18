library(maptools)
library(plyr)

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  c(as.character(subset(aa , V1 == 1)[,1]))
}

DHISLGA <- readShapePoly("J://Project/phc/nga/dhis/LGAMap.shp")
DHISFacilities <- read.csv('J://Project/phc/nga/dhis/UnitsWithData.csv')

##Voir comment obtenir osm data directement
osmNigeria <- readShapePoints("data/OSMDataNigeria.shp")

#####################################gi
#####Retrieve Usable OSM layer#######
#####################################

##Prepare osm names to be used in the matching

osmNigeria@data$name <- gsub('\\{|\\}' , '-' , osmNigeria@data$name)

ehealthdata <- osmNigeria[osmNigeria$source == 'ehealthafrica.org' &
                            osmNigeria$amenity == 'hospital' &
                            !is.na(osmNigeria$source) & !is.na(osmNigeria$amenity),]

######Refaire match for eHealth data

DHISFacilities <- merge(DHISFacilities , AvailableHierarchy , by.x = 'x' , 
                        by.y = 'Level5ID')
colnames(DHISFacilities)[1] <- 'UnitID'




##Match eHealth with datasets from DHIS
##Sratify by LGA !!!

matchEhealthALL <- data.frame(ehealth = character() , data = character() , unitId = character())
for(i in seq(nrow(ehealthdata))){
  out <- data.frame(ehealth = character() , data = character(), unitId = character())
  mm <- grep(ehealthdata$name[i] , as.character(DHISFacilities$Level5) ,
             ignore.case = FALSE , value = TRUE)
  print(mm)
  if (length(mm) == 1){
    out <- data.frame(ehealth = ehealthdata$name[i] , data = mm , 
                      unitId = DHISFacilities$UnitID[DHISFacilities$Level5 == mm])
    matchEhealthALL <- rbind(matchEhealthALL , out)
  }
}

## Only keep matches with facilities in Kano region 
## and that are matched on osm localities

dfValidCorresp <- subset(matchEhealthALL , substr(data , 1 ,2) == 'kn')
dfValidCorresp <- subset(dfValidCorresp ,
                         data %in% UniqueMatch(dfValidCorresp , dfValidCorresp$data))
dfValidCorresp <- subset(dfValidCorresp , 
                         ehealth %in% UniqueMatch(dfValidCorresp , dfValidCorresp$ehealth))


ValidationData <- ehealthdata[ehealthdata$name %in% dfValidCorresp$ehealth ,]

ValidationData@data <- merge(ValidationData@data , dfValidCorresp , by.x = 'name' ,
                             by.y = 'ehealth' , sort = F)


ValidationData <- ValidationData[ValidationData$]

###################
######Data from SDI
###################


SDIData <- read.csv("J://temp/phc/facilities.csv" , stringsAsFactors = FALSE)
matchedSDI <- read.csv("matchedFacilities.csv" , stringsAsFactors = FALSE)

SDIData <- subset(SDIData , facility_name %in% matchedSDI$SDIFacility)

##get LGA of SDI facilities
SDIData <- merge(SDIData , matchedSDI , by.x = 'facility_name' , by.y = 'SDIFacility')
length(unique(SDIData$UnitId))

SDIData5 <- merge(SDIData , AvailableHierarchy , by.x = 'UnitId' , by.y = 'Level5ID')
SDIData6 <- merge(SDIData , AvailableHierarchy , by.x = 'UnitId' , by.y = 'Level6ID')

SDIData <- rbind(SDIData5 , SDIData6)

nn <- nrow(SDIData)
SDIData <- data.frame(SDIname = SDIData$facility_name ,
                      dhisName = SDIData$DHISFacility ,
                      dhisId = SDIData$UnitId ,
                      lat = as.numeric(
                        unlist(strsplit(as.character(SDIData$coordinates) , ','))[2*seq(1:nn/2)-1]) ,
                      long = as.numeric(
                        unlist(strsplit(as.character(SDIData$coordinates) , ','))[2*seq(1:nn/2)]) ,
                      lga = SDIData$Level3)

SDIData <- subset(SDIData ,!is.na(long) & !is.na(lat) )

SDILGAcrop <- data.frame(name = character() ,
                         UnitName = character() ,
                         country = character())

tot <- 0
SDIData$lga <- as.character(SDIData$lga)
for (LGA in unique(SDIData$lga)){
  print(LGA)
  LGABorder <- DHISLGA[DHISLGA$UnitName == LGA ,]
  SDIinLGA <- SDIData[SDIData$lga == LGA , ]
  print(nrow(SDIinLGA))
  coordinates(SDIinLGA) = ~long+lat
  inLGA <- over(SDIinLGA , LGABorder)
  out <- cbind(SDIinLGA@data , inLGA)
  print(nrow(out))
  tot <- tot + nrow(out)
  print(tot)
  SDILGAcrop <- rbind(SDILGAcrop , out)
}

SDIDataShp <- subset(SDILGAcrop , !is.na(UnitName))
SDIDataShp <- merge(SDIDataShp , SDIData , by = 'SDIname')


coordinates(SDIDataShp) = ~long+lat
coordinates(SDIData)= ~long+lat


par(mfrow = c(1,2))
plot(DHISLGA)
plot(SDIData , add = T , col = 'blue')
plot(ehealthdata , add = T , col = 'red')
title(main = 'Validation Set')


plot(DHISLGA)
plot(SDIDataShp , add = T , col = 'blue')
plot(ehealthdata , add = T , col = 'red')
title(main = 'Validation Set')

##merge the two validation data sets

ValidationData@data <- subset(ValidationData@data , 
                              select = c(unitId , name , data) )

SDIDataShp@data <- subset(SDIDataShp@data , 
                          select = c(dhisId.x , SDIname , dhisName.x) )


colnames(SDIDataShp@data) <- colnames(ValidationData@data) <- c("UnitID" , "ValidationName" ,
                                                            'DHISName')

SDIDataShp@data$Source <- 'SDI'
ValidationData@data$Source <- 'eHealth'

ValidationSet <- spRbind(SDIDataShp , ValidationData)

writePointsShape(ValidationSet, "ValidationSet")