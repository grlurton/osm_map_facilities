library(maptools)
library(stringr)
library(plyr)
library(ggplot2)
library(raster)
library(rgeos)
library(reshape2)



rm(list = ls()[!(ls() %in%  c('osm_data'  , 'NigeriaShp' ,'DhisFacilities'))])

#####################################
#####Retrieve Usable OSM layer#######
#####################################

##Prepare osm names to be used in the matching
osm_data@data$name <- gsub('\\{|\\}' , '-' , osm_data@data$name)

ValidationSet <- readShapePoints('data/ValidationSet.shp')

osm_to_match <- osm_data[!(osm_data$idtoMatch %in% ValidationSet$idtoMatch) , ]

osm_data$source <- as.character(osm_data$source)
osm_data$source[grep(pattern = 'ehealth' , x = tolower(osm_data$source))] <- 'ehealth'


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
      print(length(match))
      if (length(match) == 0){
        facilities2 <- gsub('Clinic' , 'Centre' , facilities)
        match <- grep(facilities2, names[i] , ignore.case = FALSE , value = TRUE)
      }
      if (length(match) == 0){
        facilities2 <- gsub('Centre' , 'Clinic' , facilities)
        match <- grep(facilities2, names[i] , ignore.case = FALSE , value = TRUE)
      }
      if (length(match) == 0){
        facilities2 <- gsub('Health Post' , 'Dispensary' , facilities)
        match <- grep(facilities2, names[i] , ignore.case = FALSE , value = TRUE)
      }
      if (length(match) == 0){
        facilities2 <- gsub('Dispensary' , 'Health Post' ,  facilities)
        match <- grep(facilities2, names[i] , ignore.case = FALSE , value = TRUE)
      }
      print(paste('       ' , length(match)))
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
  for(LGAIndex in 1:length(unique(NigeriaShp$UnitName))){
    LGA <- unique(as.character(NigeriaShp$UnitName))[LGAIndex]
    print(paste(LGAIndex , LGA , sep = ' - '))
    osmWRK<- over(NigeriaShp[NigeriaShp$UnitName == LGA , ] , osmDataCropped  , returnList = TRUE)
    osmWRK <- osmDataCropped[osmDataCropped$idtoMatch %in% osmWRK[[1]]$idtoMatch , ]
    hierarchWRK <- subset(hierarch , Level3 == LGA)
    out <- MatchSimple(hierarchWRK , osmWRK)
    if (nrow(out) > 0){
      out$lga <- LGA
      MatchsCoords <- rbind(MatchsCoords , out)
      }
    }
    coordinates(MatchsCoords) = ~long+lat
    print(paste('Number of Matches :' , nrow(MatchsCoords) , sep = ' '))
    MatchsCoords
}

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  c(as.character(subset(aa , V1 == 1)[,1]))
}

##Strategy 1 - match simply on all units in OSM 
osmStrategy2 <- osm_to_match
osmStrategy2@data$name <- paste(' ' , osmStrategy2@data$name , ' ' , sep = '')
osmStrategy2@data$name <- gsub('  ' , ' ' , osmStrategy2@data$name)
MatchStrat2 <- MatchOver(DhisFacilities , osmStrategy2)
MatchStrat2 <- MatchStrat2[MatchStrat2$facilityID %in% 
                             UniqueMatch(MatchStrat2@data , MatchStrat2@data$facilityID),
                           ]

##Strategy 2 - simplify osm names by taking out natural feature indication
osmStrategy3 <- osm_to_match
NatFeatures <- 'River|Hill|Forest Reserve|Native Area'
osmStrategy3@data$name <- gsub(NatFeatures , '' , osmStrategy3@data$name)
osmStrategy3@data$name <- paste(' ' , osmStrategy3@data$name , ' ' , sep = '')
osmStrategy3@data$name <- gsub('  ' , ' ' , osmStrategy3@data$name)
MatchStrat3 <- MatchOver(DhisFacilities , osmStrategy3)
MatchStrat3 <- MatchStrat3[MatchStrat3$facilityID %in% 
                             UniqueMatch(MatchStrat3@data , MatchStrat3@data$facilityID),
                           ]

##Strategy 3 - From Strategy 4 - if name has multiple match, in a < 10 km2 zone, put
## in the middle
osmStrategy4 <- osm_to_match
NatFeatures <- 'River|Hill|Forest Reserve|Native Area'
osmStrategy4@data$name <- gsub(NatFeatures , '' , osmStrategy4@data$name)
osmStrategy4@data$name <- paste(' ' , osmStrategy4@data$name , ' ' , sep = '')
osmStrategy4@data$name <- gsub('  ' , ' ' , osmStrategy4@data$name)
MatchStrat4 <- MatchOver(DhisFacilities , osmStrategy4)
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
    print(nrow(MatchesforPlace@data))
    if (nrow(MatchesforPlace@data) > 1){
      for (facilityID in unique(MatchesforPlace$facilityID)){
        MatchFacPlace <- MatchesforPlace[MatchesforPlace$facilityID == facilityID , ]
        if(nrow(MatchesforPlace@data) > 1){
          t <- pointDistance(MatchesforPlace ,
                             lonlat = TRUE, allpairs=FALSE) /1000
          
          if (max(t , na.rm = TRUE) < 10){
            nmatches <- nrow(MatchesforPlace@data)
            print(max(t , na.rm = TRUE))
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
            Centroids <- rbind(Centroids , out)
          }
          
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
plot(NigeriaShp)
plot(CentroidsB , add = TRUE , col = CentroidsB$facility)
plot(NigeriaShp)
plot(CentroidsC , add = TRUE , col = CentroidsC$facility)

Centroids <- subset(Centroids , select = c(facility , place , 
                                        centroidlat , centroidlong))

Centroids <- Centroids[!(duplicated(Centroids)) ,]
Centroids <- Centroids[Centroids$facility %in% 
                         UniqueMatch(Centroids , Centroids$facility) ,]
Centroids$LGA <- ''
coordinates(Centroids) =~ centroidlat + centroidlong


##Strategy 5 - Try to match with non point geometries



####################################################################################
################ Cumulative matching using different approaches  ###################
####################################################################################

##Step 2

HierarchyStep2 <- DhisFacilities
osmStrategyC2 <- osm_to_match
osmStrategyC2@data$name <- paste(' ' , osmStrategyC2@data$name , ' ' , sep = '')
osmStrategyC2@data$name <- gsub('  ' , ' ' , osmStrategyC2@data$name)
MatchStratC2 <- MatchOver(HierarchyStep2 , osmStrategyC2)
MatchStratC2 <- MatchStratC2[MatchStratC2$facilityID %in% 
                             UniqueMatch(MatchStratC2@data , MatchStratC2@data$facilityID), 
                             ]
MatchStratC2@data$MatchingStage <- 'Stage 2'                           
MatchStratC2 <- spRbind(MatchStratC2 , MatchStratC1)

##Step 3

HierarchyStep3 <- subset(DhisFacilities , !(Level5ID %in% MatchStratC2$facilityID))
osmStrategyC3 <- osm_to_match
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

HierarchyStep4 <- subset(DhisFacilities , !(Level5 %in% MatchStratC3$facilityID))
osmStrategyC4 <- osm_to_match
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

WardsCH <- function(data){
  i <- 1
  empty <- 'oui'
  wardsIds <- unique(data$wardID)
  for (ID in wardsIds){
    print(ID)
    wards <- GetWardsConvexHull(data , ID)
    if (!is.null(wards) & class(wards)[1] == "SpatialPolygons"){
      wards@polygons[[1]]@ID <- ID
      #print(wards@data)
      if(empty == 'non'){
        out <- spRbind(out , wards)
      }
      if(empty == 'oui'){
        wards
        out <- wards
        empty <- 'non'
      }
    }
  }
  out
}

a <- WardsCH(MatchStratC5)
plot(a , col = factor(a@plotOrder))

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

HierarchyStep5 <- subset(DhisFacilities , !(Level5 %in% MatchStratC4$facilityID))
NonMatchedFacilities <- HierarchyStep4[!(HierarchyStep4$Level5ID%in% MatchStratC4$facilityID) ,]


## Attention rendre les facilities uniques avant de tourner
FacilitiesToWardCentroid <- function(facilities , wardsCentroids){
  out <- data.frame(facility = character(), facilityID = character() ,
                    centroidlat = numeric(), centroidlong = numeric() , 
                    wardID = character())
  for (facilityID in facilities$Level5ID){
    print(facilityID)
    facility <- unique(facilities$Level5[facilities$Level5ID == facilityID])
    wardID <- facilities$Level4ID[facilities$Level5ID == facilityID]
    if (length(wardID) == 1){
      wardsCentroids$wardID <- as.character(wardsCentroids$wardID )
      centroidlat <- wardsCentroids$centroidlat[wardsCentroids$wardID == wardID]
      centroidlong <- wardsCentroids$centroidlong[wardsCentroids$wardID == wardID]
      if (length(centroidlat) > 0){
        xx <- data.frame(facility , facilityID , wardID , centroidlat , centroidlong)
        out <- rbind(out , xx)
      }
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


##################################################
########## Validate Matching #####################
##################################################

Validation <- function(TestedSet , ValidationSet){
  ValidData <- ValidationSet@data
  ValidData$dhis_ID <- as.character(ValidData$dhis_ID)
  TestedSet <- TestedSet[TestedSet@data$facilityID %in% ValidData$dhis_ID ,]
  
  TestedCoords <- data.frame(facilityID = TestedSet$facilityID ,
                             latData = TestedSet@coords[,1],
                             longData = TestedSet@coords[,2])
  
  TestedCoords <- merge(TestedCoords , ValidData , by.x = 'facilityID' , by.y  = 'dhis_ID')
  
  ValidCoords <- data.frame(facilityID = ValidationSet$dhis_ID ,
                            Source = ValidationSet$source ,
                            lateHealth = ValidationSet@coords[,1],
                            longeHealth = ValidationSet@coords[,2])
  
  Compare <- merge(ValidCoords , TestedCoords , by = 'facilityID') 
  dist <- pointDistance(cbind(Compare$lateHealth , Compare$longeHealth), 
                        cbind(Compare$latData , Compare$longData), 
                        lonlat = TRUE, allpairs=FALSE) /1000
  out <- cbind(Compare , dist)
  out
}

CompareSet2 <- Validation(MatchStratC2 , ValidationSet)
CompareSet3 <- Validation(MatchStratC3 , ValidationSet)
CompareSet4 <- Validation(MatchStratC4 , ValidationSet)
CompareSet5 <- Validation(MatchStratC5 , ValidationSet)


ValidationStatistics <- function(ValidationOutput){
  n_facilities <- length(ValidationOutput$dist)
  min5 <- sum(ValidationOutput$dist <5)/length(ValidationOutput$dist)
  medianDist <- median(ValidationOutput$dist)
  data.frame(n_facilities , min5 , medianDist) 
}


##Should review plotting
plotResults <- function(data , State){
  dataPlot <- subset(data , substr(data$state , 1 ,2) == State)
  coordinates(dataPlot) =~ lon+lat
  plot(dataPlot)
  plot(NigeriaShp , col = "grey" , add = TRUE)
  plot(NigeriaShp[substr(NigeriaShp$UnitName , 1 ,2) == State ,] , 
       col = "white" , add = TRUE)
  plot(dataPlot , add = TRUE , col = 'red')
  dataPlot <- subset(data , substr(data$state , 1 ,2) == State)
  coordinates(dataPlot) =~ latData + longData
  plot(dataPlot, add = TRUE)
  segments(dataPlot$lateHealth , dataPlot$longeHealth ,
           dataPlot$latData , dataPlot$longData , col = 'orange' , lwd = 2)
}

plotGroupped <- function(data){
  par(mfrow = c(2,2))
  for(State in unique(substr(data$state , 1 ,2))){
    plotResults(data , State)
  }
}

DiganosticElements <- function(data){
  stat <- ddply(data , .(Source) , ValidationStatistics)
  print(stat)
  plotGroupped(data)
}

DiganosticElements(CompareSet2)
DiganosticElements(CompareSet3)
DiganosticElements(CompareSet4)
DiganosticElements(CompareSet5)

par(mfrow = c(1,1))
plot(MatchStratC5 , col = as.factor(MatchStratC5$MatchingStage))
legend('left' , legend = sort(unique(as.factor(MatchStratC5$MatchingStage))) , 
       col = 1:5 , pch = 3 ,
       cex = 0.7 , text.width = 1)

WardKano <- DhisFacilities$Level4ID[DhisFacilities$Level2 == 'kn Kano State']

DataToPlot <- MatchStratC5[MatchStratC5$wardID %in% WardKano  & !is.na(MatchStratC5$wardID ),]

plot(DataToPlot , col = as.factor(DataToPlot$MatchingStage))
legend('left' , legend = sort(unique(as.factor(DataToPlot$MatchingStage))) , 
       col = 1:5 , pch = 3 ,
       cex = 0.7 , text.width = 1)

writePointsShape(MatchStratC5, "FacilitiesGPS")
