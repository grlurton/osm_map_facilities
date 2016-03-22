library(maptools)
library(stringr)
library(rgeos)
library(osmar)
library(plyr)
library(raster)
library(ggplot2)

setwd('J://Project/phc/nga/dhis')



#orgUnits <- read.csv('ExtractOrgUnitsRaw.csv')
OrgUnitsHierarchy <- read.csv('HierarchyData.csv')
DHISLGA <- readShapePoly("LGAMap.shp")
Locations <- read.csv('table_location.csv')
Facilities <- Locations$location_id[Locations$location_level == 'Facility']

#Select hierarchy of LGA Available in DHIS as of now
AvailableHierarchy <- subset(OrgUnitsHierarchy , #Level3 %in% DHISLGA$UnitName &
                               Level5ID %in% Facilities)

##Voir comment obtenir osm data directement
osmData <- readShapePoints("OSMDataNigeria.shp")

##Crop data to fit the zones we have in DHIS
osmCrop <- over(osmData , DHISLGA)

## Merge to have LGA in osm data
osmData@data <- cbind(osmData@data , DHISLGA = osmCrop$UnitName)

##Drop points with no name
osmNigeria <- osmData[!is.na(osmData$DHISLGA) ,]

rm(osmData , osmCrop , OrgUnitsHierarchy)

#####################################
#####Retrieve Usable OSM layer#######
#####################################




##Prepare osm names to be used in the matching

osmNigeria@data$name <- gsub('\\{|\\}' , '-' , osmNigeria@data$name)



###Matching function
MatchSimple <- function(DhisData , osmData){
  out <- data.frame(index = character() , place = character() , 
                    match = character() , long = character() , lat = character())
  names <- as.character(osmData$name)
  nosm <- nrow(osmData@coords)
  facilities <- as.character(DhisData$Level5)
  facilitiesID <- as.character(DhisData$Level5ID)
  wards <- as.character(DhisData$Level4)
  wardsID <- as.character(DhisData$Level4ID)
  if (nosm >0){
    for(i in 1:length(names)){
      match <- grep(x = facilities , pattern = names[i] , ignore.case = TRUE)
      facility <- facilities[match]
      facilityID <- facilitiesID[match]
      ward <- wards[match]
      wardID <- wardsID[match]
      nmatch <- length(facility)
      if (nmatch > 0){
        outWRK <- data.frame(place = rep(names[i] , nmatch) , 
                             facility = facility ,
                             facilityID = facilityID ,
                             long = osmData@coords[i] ,
                             lat = osmData@coords[nosm + i] ,
                             ward = ward ,
                             wardID = wardID
        )
        out <- rbind(out , outWRK)
      }
    }
  }
  out
}

MatchOver <- function(hierarch , osmDataCropped){
  MatchsCoords <- data.frame(index = character() , place = character() ,
                             facility = character() , facilityID = character() ,
                             ward = character() , wardID = character() ,
                             long = character() , lat = character() ,
                             lga = character()
                             )
  LGAIndex <- 1
  for(LGAIndex in 1:length(unique(hierarch$Level3))){
    LGA <- unique(as.character(hierarch$Level3))[LGAIndex]
    print(paste(LGAIndex , LGA , sep = ' - '))
    if (sum(as.character(osmDataCropped$DHISLGA) == LGA) >= 1){
      osmWRK<- osmDataCropped[as.character(osmDataCropped$DHISLGA) == LGA ,]
      hierarchWRK <- subset(hierarch , Level3 == LGA)
      out <- MatchSimple(hierarchWRK , osmWRK)
      if (nrow(out) > 0){
        out$lga <- LGA
        MatchsCoords <- rbind(MatchsCoords , out)
      }
    }
      LGAIndex <- LGAIndex+1
  }
    coordinates(MatchsCoords) = ~long+lat
    print(paste('Number of Matches :' , nrow(MatchsCoords) , sep = ' '))
    MatchsCoords
}

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  c(as.character(subset(aa , V1 == 1)[,1]))
}

##Strategy 1 - match simply on units considered places by OSM 
osmStrategy1 <- osmNigeria[!is.na(osmNigeria$place) ,]
osmStrategy1@data$name <- paste(' ' , osmStrategy1@data$name , ' ' , sep = '')
osmStrategy1@data$name <- gsub('  ' , ' ' , osmStrategy1@data$name)
MatchStrat1 <- MatchOver(AvailableHierarchy , osmStrategy1)
MatchStrat1 <- MatchStrat1[MatchStrat1$facilityID %in% 
                             UniqueMatch(MatchStrat1@data , MatchStrat1@data$facilityID),
                           ]

##Strategy 2 - match simply on all units in OSM 
osmStrategy2 <- osmNigeria
osmStrategy2@data$name <- paste(' ' , osmStrategy2@data$name , ' ' , sep = '')
osmStrategy2@data$name <- gsub('  ' , ' ' , osmStrategy2@data$name)
MatchStrat2 <- MatchOver(AvailableHierarchy , osmStrategy2)
MatchStrat2 <- MatchStrat2[MatchStrat2$facilityID %in% 
                             UniqueMatch(MatchStrat2@data , MatchStrat2@data$facilityID),
                           ]

##Strategy 3 - simplify osm names by taking out natural feature indication
osmStrategy3 <- osmNigeria
NatFeatures <- 'River|Hill|Forest Reserve|Native Area'
osmStrategy3@data$name <- gsub(NatFeatures , '' , osmStrategy3@data$name)
osmStrategy3@data$name <- paste(' ' , osmStrategy3@data$name , ' ' , sep = '')
osmStrategy3@data$name <- gsub('  ' , ' ' , osmStrategy3@data$name)
MatchStrat3 <- MatchOver(AvailableHierarchy , osmStrategy3)
MatchStrat3 <- MatchStrat3[MatchStrat3$facilityID %in% 
                             UniqueMatch(MatchStrat3@data , MatchStrat3@data$facilityID),
                           ]

##Strategy 4 - From Strategy 4 - if name has multiple match, in a < 10 km2 zone, put
## in the middle
osmStrategy4 <- osmNigeria
NatFeatures <- 'River|Hill|Forest Reserve|Native Area'
osmStrategy4@data$name <- gsub(NatFeatures , '' , osmStrategy4@data$name)
osmStrategy4@data$name <- paste(' ' , osmStrategy4@data$name , ' ' , sep = '')
osmStrategy4@data$name <- gsub('  ' , ' ' , osmStrategy4@data$name)
MatchStrat4 <- MatchOver(AvailableHierarchy , osmStrategy4)
MatchStrat4Multip <- MatchStrat4[!(MatchStrat4$facilityID %in% 
                                 UniqueMatch(MatchStrat4@data , 
                                             MatchStrat4@data$facilityID)),
                               ] 

GetCentroids <- function(data){
  Centroids <- data.frame(facility = character(), facilityID = character() ,
                          place = character() ,
                          centroidlat = numeric(), centroidlong = numeric() ,
                          lat = numeric() , long = numeric() ,
                          ward = character() , wardID = character())
  for (place in unique(data$place)){
    MatchesforPlace <- data[data$place == place , ]
    if (nrow(MatchesforPlace@data) > 1){
      for (facilityID in unique(MatchesforPlace$facilityID)){
        MatchFacPlace <- MatchesforPlace[MatchesforPlace$facilityID == facilityID , ]
        if(nrow(MatchesforPlace@data) > 1){
          t <- pointDistance(MatchesforPlace ,
                             lonlat = TRUE, allpairs=FALSE) /1000
          if (max(t , na.rm = TRUE) < 10){
            nmatches <- nrow(MatchesforPlace@data)
            facility <- unique(MatchesforPlace$facility[MatchesforPlace$facilityID == 
                                                               facilityID])
            ward     <- unique(MatchesforPlace$ward[MatchesforPlace$facilityID == 
                                                          facilityID])
            wardID     <- unique(MatchesforPlace$wardID[MatchesforPlace$facilityID == 
                                                      facilityID])
            centroid <- gCentroid(MatchesforPlace)
            out <- data.frame(facilityID = rep(facilityID , nmatches),
                              facility = rep(facility , nmatches) ,
                              place = rep(place , nmatches) ,
                              centroidlat = rep(centroid@coords[,1] , nmatches) ,
                              centroidlong = rep(centroid@coords[,2] , nmatches) ,
                              lat = MatchesforPlace@coords[,1] ,
                              long = MatchesforPlace@coords[,2] ,
                              ward = rep(ward , nmatches) ,
                              wardID = rep(wardID , nmatches)                              
                              )
          }
          Centroids <- rbind(Centroids , out)
        }
      }
    }
  }
  Centroids
}

Centroids <- GetCentroids(MatchStrat4Multip)

CentroidsB <- Centroids
CentroidsC <- Centroids

coordinates(CentroidsB) =~ lat+long
coordinates(CentroidsC) =~ centroidlat + centroidlong

par(mfrow = c(1,2))
plot(DHISLGA)
plot(CentroidsB , add = TRUE , col = CentroidsB$facility)
plot(DHISLGA)
plot(CentroidsC , add = TRUE , col = CentroidsC$facility)

Centroids <- subset(Centroids , select = c(facility , place , 
                                        centroidlat , centroidlong))

Centroids <- Centroids[!(duplicated(Centroids)) ,]
Centroids <- Centroids[Centroids$facility %in% 
                         UniqueMatch(Centroids , Centroids$facility) ,]
Centroids$LGA <- ''
coordinates(Centroids) =~ centroidlat + centroidlong



##Strategy 5 - Try to match with non point geometries

##Strategy 6 - If multiple facilities have been found in a ward, attribute those in
## the same wards to variations in the convex zone


##Strategy Combine - combining first 3 strategies incrementally
osmStrategyC1 <- osmNigeria[!is.na(osmNigeria$place) ,]
osmStrategyC1@data$name <- paste(' ' , osmStrategyC1@data$name , ' ' , sep = '')
osmStrategyC1@data$name <- gsub('  ' , ' ' , osmStrategyC1@data$name)
MatchStratC1 <- MatchOver(AvailableHierarchy , osmStrategyC1)
MatchStratC1 <- MatchStratC1[MatchStratC1$facilityID %in% 
                             UniqueMatch(MatchStratC1@data , MatchStratC1@data$facilityID),
                             ]
MatchStratC1@data$MatchingStage <- 'Stage 1'


##Step 2

HierarchyStep2 <- subset(AvailableHierarchy , !(Level5ID %in% MatchStratC1$facilityID))
osmStrategyC2 <- osmNigeria
osmStrategyC2@data$name <- paste(' ' , osmStrategyC2@data$name , ' ' , sep = '')
osmStrategyC2@data$name <- gsub('  ' , ' ' , osmStrategyC2@data$name)
MatchStratC2 <- MatchOver(HierarchyStep2 , osmStrategyC2)
MatchStratC2 <- MatchStratC2[MatchStratC2$facilityID %in% 
                             UniqueMatch(MatchStratC2@data , MatchStratC2@data$facilityID), 
                             ]
MatchStratC2@data$MatchingStage <- 'Stage 2'                           
MatchStratC2 <- spRbind(MatchStratC2 , MatchStratC1)

##Step 3

HierarchyStep3 <- subset(AvailableHierarchy , !(Level5ID %in% MatchStratC2$facilityID))
osmStrategyC3 <- osmNigeria
osmStrategyC3@data$name <- gsub(NatFeatures , '' , osmStrategyC3@data$name)
osmStrategyC3@data$name <- paste(' ' , osmStrategyC3@data$name , ' ' , sep = '')
osmStrategyC3@data$name <- gsub('  ' , ' ' , osmStrategyC3@data$name)
MatchStratC3 <- MatchOver(HierarchyStep3 , osmStrategyC3)
MatchStratC3 <- MatchStratC3[MatchStratC3$facilityID %in% 
                               UniqueMatch(MatchStratC3@data , MatchStratC3@data$facilityID), 
                             ]
MatchStratC3@data$MatchingStage <- 'Stage 3'                           
MatchStratC3 <- spRbind(MatchStratC3 , MatchStratC2)

##Step 4

HierarchyStep4 <- subset(AvailableHierarchy , !(Level5 %in% MatchStratC3$facilityID))
osmStrategyC4 <- osmNigeria
osmStrategyC4@data$name <- gsub(NatFeatures , '' , osmStrategyC4@data$name)
osmStrategyC4@data$name <- paste(' ' , osmStrategyC4@data$name , ' ' , sep = '')
osmStrategyC4@data$name <- gsub('  ' , ' ' , osmStrategyC4@data$name)
MatchStratC4Multip <- MatchOver(HierarchyStep4 , osmStrategyC4)
MatchStratC4Multip <- MatchStratC4Multip[!(MatchStratC4Multip$facilityID %in% 
                                     UniqueMatch(MatchStratC4Multip@data , 
                                                 MatchStratC4Multip@data$facilityID)),
                                 ]

Centroids <- GetCentroids(MatchStratC4Multip)

Centroids <- subset(Centroids , select = c(facilityID , place , facility , ward , wardID ,
                                           centroidlat , centroidlong))

Centroids <- Centroids[!(duplicated(Centroids)) ,]
Centroids <- Centroids[Centroids$facilityID %in% 
                         UniqueMatch(Centroids , Centroids$facilityID) ,]
Centroids$LGA <- ''
coordinates(Centroids) =~ centroidlat + centroidlong
Centroids@data$MatchingStage <- 'Stage 4'
colnames(Centroids@data) <- c('facilityID' , 'place' , 'facility' ,
                              'ward' , 'wardID' , 'lga' , 'MatchingStage')
                      
MatchStratC4 <- spRbind(Centroids , MatchStratC3)


##Strategy 5 - Try to match with non point geometries

##Strategy 6 - If multiple facilities have been found in a ward, attribute those in
## the same wards to variations in the convex zone

##Function to get the convex hull of a ward

GetWardsConvexHull <- function(Data , WardID){
  data <- Data[Data@data$wardID == WardID ,]
  if (nrow(data@data) > 1){
    out <- gConvexHull(data)
    out
  }
}

##Function that should get all convex hull for all wards
##pb = some are just lines and pb in mering

WardsCH <- function(data){
  wardsIds <- unique(data$wardID)
  out <- GetWardsConvexHull(data , wardsIds[1])
#  out@data$wardID <- wardsIds[1]
  for (ID in wardsIds[-1]){
    print(ID)
    wards <- GetWardsConvexHull(data , ID)
#    wards@data$wardID <- ID
  }
  if (!is.null(wards)){
    out <- spRbind(out , wards)
  }
  out
}

#test <- WardsCH(MatchStratC4)

#plot(test)

GetWardsCentroid <- function(Data){
  out <- data.frame(centroidlat = numeric(), centroidlong = numeric() , 
                    wardID = character())
  wardsIds <- unique(Data$wardID)
  for (ward in wardsIds){
    print(ward)
    xx <- data.frame(wardID = ward)
    wardsCentroid <- gCentroid(Data[Data@data$wardID == ward,])
    xx$centroidlat <- wardsCentroid@coords[,1]
    xx$centroidlong <- wardsCentroid@coords[,2]
    out <- rbind(out , xx)
  }
  out
}

WardsCentroids <- GetWardsCentroid(MatchStratC4)

HierarchyStep5 <- subset(AvailableHierarchy , !(Level5 %in% MatchStratC4$facilityID))
NonMatchedFacilities <- HierarchyStep4[!(HierarchyStep4$Level5ID%in% MatchStratC4$facilityID) ,]

FacilitiesToWardCentroid <- function(facilities , wardsCentroids){
  out <- data.frame(facility = character(), facilityID = character() ,
                    centroidlat = numeric(), centroidlong = numeric() , 
                    wardID = character())
  for (facilityID in facilities$Level5ID){
    print(facilityID)
    facility <- facilities$Level5[facilities$Level5ID == facilityID]
    wardID <- facilities$Level4ID[facilities$Level5ID == facilityID]
    wardsCentroids$wardID <- as.character(wardsCentroids$wardID )
    centroidlat <- wardsCentroids$centroidlat[wardsCentroids$wardID == wardID]
    centroidlong <- wardsCentroids$centroidlong[wardsCentroids$wardID == wardID]
    if (length(centroidlat) > 0){
      xx <- data.frame(facility , facilityID , wardID , centroidlat , centroidlong)
      out <- rbind(out , xx)
    }
  }
  out
}

Match5 <- FacilitiesToWardCentroid(NonMatchedFacilities , WardsCentroids)
coordinates(Match5) <- ~centroidlat+ centroidlong

colnames(Match5@data)
colnames(MatchStratC4@data)

Match5$place <- Match5$ward <- Match5$lga <- ''

Match5$MatchingStage <- 'Stage 5'


MatchStratC5 <- spRbind(MatchStratC4 , Match5)

nrow(MatchStratC5@data)

rm(osmStrategy1 , osmStrategy2 , osmStrategy3 , osmStrategy4 , osmStrategyC1 ,
   osmStrategy42 , osmStrategyC3)


##################################################
########## Validate Matching #####################
##################################################

ValidationSet <- readShapePoints('ValidationSet.shp')

Validation <- function(TestedSet , ValidationSet){
  ValidData <- ValidationSet@data
  ValidData$UnitID <- as.character(ValidData$UnitID)
  TestedSet <- TestedSet[TestedSet@data$facilityID %in% ValidData$UnitID ,]
  
  TestedCoords <- data.frame(facilityID = TestedSet$facilityID ,
                             latData = TestedSet@coords[,1],
                             longData = TestedSet@coords[,2])
  TestedCoords <- merge(TestedCoords , ValidData , by.x = 'facilityID' , by.y  = 'UnitID')
  
  
  ValidCoords <- data.frame(facilityID = ValidationSet$UnitID ,
                            Source = ValidationSet$Source ,
                            lateHealth = ValidationSet@coords[,1],
                            longeHealth = ValidationSet@coords[,2])
  Compare <- merge(ValidCoords , TestedCoords , by = 'facilityID') 
  dist <- pointDistance(cbind(Compare$lateHealth , Compare$longeHealth), 
                        cbind(Compare$latData , Compare$longData), 
                        lonlat = TRUE, allpairs=FALSE) /1000
  out <- cbind(Compare , dist)
  out
}

CompareSet1 <- Validation(MatchStratC1 , ValidationSet)
CompareSet2 <- Validation(MatchStratC2 , ValidationSet)
CompareSet3 <- Validation(MatchStratC3 , ValidationSet)
CompareSet4 <- Validation(MatchStratC4 , ValidationSet)
CompareSet5 <- Validation(MatchStratC5 , ValidationSet)


ValidationStatistics <- function(ValidationOutput){
  min5 <- sum(ValidationOutput$dist <5)/length(ValidationOutput$dist)
  meanDist <- mean(ValidationOutput$dist)
  data.frame(min5 , meanDist) 
}


##Should review plotting
plotResults <- function(data , State){
  dataPlot <- subset(data , substr(data$match , 1 ,2) == State)
  coordinates(dataPlot) =~ long+lat
  plot(dataPlot)
  plot(DHISLGA , col = "grey" , add = TRUE)
  plot(DHISLGA[substr(DHISLGA$UnitName , 1 ,2) == State ,] , 
       col = "white" , add = TRUE)
  plot(dataPlot , add = TRUE , col = 'red')
  dataPlot <- subset(data , substr(data$match , 1 ,2) == State)
  coordinates(dataPlot) =~ latData + longData
  plot(dataPlot, add = TRUE)
  segments(dataPlot$lateHealth , dataPlot$longeHealth ,
           dataPlot$latData , dataPlot$longData , col = 'orange' , lwd = 2)
}

plotGroupped <- function(data){
  par(mfrow = c(2,4))
  for(State in unique(substr(data$match , 1 ,2))){
    plotResults(data , State)
  }
}

DiganosticElements <- function(data){
  stat <- ddply(data , .(Source.x) , ValidationStatistics)
  print(stat)
  plotGroupped(data)
}

DiganosticElements(CompareSet1)
DiganosticElements(CompareSet2)
DiganosticElements(CompareSet3)
DiganosticElements(CompareSet4)
DiganosticElements(CompareSet5)

par(mfrow = c(1,1))
plot(MatchStratC5 , col = as.factor(MatchStratC5$MatchingStage))
legend('left' , legend = sort(unique(as.factor(MatchStratC5$MatchingStage))) , 
       col = 1:5 , pch = 3 ,
       cex = 0.7 , text.width = 1)

WardKano <- AvailableHierarchy$Level3ID[AvailableHierarchy$Level2 == 'kn Kano State']

DataToPlot <- MatchStratC5[MatchStratC5$wardID %in% WardKano & !is.na(MatchStratC5$wardID ),]

plot(DataToPlot , col = as.factor(DataToPlot$MatchingStage))

writePointsShape(MatchStratC5, "FacilitiesGPS")
