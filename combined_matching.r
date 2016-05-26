library(maptools)
library(stringr)
library(plyr)
library(ggplot2)
library(raster)
library(rgeos)
library(reshape2)

##Prepare osm names to be used in the matching
ValidationData <- readShapePoints('data/ValidationSet.shp')

osm_to_match <- osm_data[!(osm_data$idtoMatch %in% ValidationData$idtoMatch) & !is.na(osm_data$name), ]

###Matching function
MatchSimple <- function(DhisData , osmData , osm_name){
  out <- data.frame()
  
  names <- osmData@data[,osm_name]
  nosm <- nrow(osmData@coords)
  
  facilities <- as.character(DhisData$Level5_cleaned)
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
                             osmID = osmData@data$idtoMatch[i] ,
                             long = osmData@coords[i] ,
                             lat = osmData@coords[nosm + i] ,
                             ward = ward ,
                             wardID = wardID ,
                             state = as.character(DhisData$Level2[match])
                             )
        out <- rbind(out , outWRK)
      }
    }
  }
  out
}

MatchOver <- function(hierarch , osmDataCropped , osm_name){
  MatchsCoords <- data.frame()
  for(LGAIndex in 1:length(unique(NigeriaShp$lga_name_matched))){
    LGA <- unique(as.character(NigeriaShp$lga_name_matched))[LGAIndex]
    #print(paste(LGAIndex , LGA , sep = ' - '))
    osmWRK<- over(NigeriaShp[NigeriaShp$lga_name_matched == LGA , ] , osmDataCropped  , returnList = TRUE)
    osmWRK <- osmDataCropped[osmDataCropped$idtoMatch %in% osmWRK[[1]]$idtoMatch , ]
    hierarchWRK <- subset(hierarch , Level3 == LGA)
    out <- MatchSimple(hierarchWRK , osmWRK , osm_name)
    if (nrow(out) > 0){
      out$lga <- LGA
      MatchsCoords <- rbind(MatchsCoords , out)
    }
  else{
    print(paste(LGAIndex , LGA , sep = ' - '))
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

GetCentroids <- function(data){
  Centroids <- data.frame()
  
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
            
            state <- unique(MatchesforPlace$state[MatchesforPlace$facilityID == 
                                                    facilityID]) 
            
            lga <- unique(MatchesforPlace$lga[MatchesforPlace$facilityID == 
                                                    facilityID]) 
            
            osmID <- unique(MatchesforPlace$osmID[MatchesforPlace$facilityID == 
                                                facilityID]) 
            
            out <- data.frame(facilityID = rep(facilityID , nmatches),
                              facility = rep(facility , nmatches) ,
                              place = rep(place , nmatches) ,
                              centroidlat = rep(centroid@coords[,1] , nmatches) ,
                              centroidlong = rep(centroid@coords[,2] , nmatches) ,
                              lat = MatchesforPlace@coords[,1] ,
                              long = MatchesforPlace@coords[,2] ,
                              ward = rep(ward , nmatches) ,
                              wardID = rep(wardID , nmatches) ,
                              state = rep(state , nmatches) ,
                              lga = rep(lga , nmatches) ,
                              osmID = rep(osmID , nmatches)
            )
            Centroids <- rbind(Centroids , out)
          }
          
        }
      }
    }
  }
  Centroids
}



####################################################################################
################ Cumulative matching using different approaches  ###################
####################################################################################

NatFeatures <- 'Bus Stop|International School|Police Station|Filling Station|Grammar School|Comprehensive School|Comprehensive Health Center|Hills|River|Hill|Forest Reserve|Native Area|Water Works|Station|Market|Dispensary|Primary Health Centre|Health Facility|Quarters|Hospital|Hill|Forest Reserve|Dam'

##Step 1

osm_to_match@data$name_1 <- paste(' ' , osm_to_match@data$name , ' ' , sep = '')
osm_to_match@data$name_1 <- gsub('  ' , ' ' , osm_to_match@data$name)
MatchStrat1 <- MatchOver(DHISFacilities , osm_to_match , 'name_1')
MatchStrat1 <- MatchStrat1[MatchStrat1$facilityID %in% 
                               UniqueMatch(MatchStrat1@data , MatchStrat1@data$facilityID), 
                             ]
MatchStrat1@data$MatchingStage <- 'Stage 1'                           

##Step 2 => pb : pourquoi on match beaucoup moins maintenant

## Garder nom original pour decrire comment on match

Hierarchy_Step2 <- subset(DHISFacilities , !(Level5ID %in% MatchStrat1$facilityID))
osm_to_match@data$name_2 <- gsub(NatFeatures , '' , osm_to_match@data$name)
osm_to_match@data$name_2 <- paste(' ' , osm_to_match@data$name_2 , ' ' , sep = '')
osm_to_match@data$name_2 <- gsub('  ' , ' ' , osm_to_match@data$name_2)
MatchStrat2 <- MatchOver(Hierarchy_Step2 , osm_to_match , "name_2")
MatchStrat2 <- MatchStrat2[MatchStrat2$facilityID %in% 
                             UniqueMatch(MatchStrat2@data , MatchStrat2@data$facilityID), 
                             ]
MatchStrat2@data$MatchingStage <- 'Stage 2'         

Match_complete <- spRbind(MatchStrat2 , MatchStrat1)


##Step 3

Hierarchy_Step3 <- subset(DHISFacilities , !(Level5ID %in% MatchStrat1$facilityID |
                                              Level5ID %in% MatchStrat2$facilityID ))
osm_to_match@data$name_3 <- gsub(NatFeatures , '' , osm_to_match@data$name)
osm_to_match@data$name_3 <- paste(' ' , osm_to_match@data$name_3 , ' ' , sep = '')
osm_to_match@data$name_3 <- gsub('  ' , ' ' , osm_to_match@data$name_3)
MatchMultip <- MatchOver(Hierarchy_Step3 , osm_to_match , 'name_3')


## Getting centroids for multiple matches
Centroids <- GetCentroids(MatchMultip)
Centroids <- subset(Centroids , select = c(facilityID , facility , place , ward , wardID , state , lga ,
                                           centroidlat , centroidlong))
Centroids$osmID <- 'centroid'
Centroids <- Centroids[!duplicated(Centroids) , ]
Centroids <- Centroids[Centroids$facilityID %in% 
                         UniqueMatch(Centroids , Centroids$facilityID) ,]
coordinates(Centroids) =~ centroidlat + centroidlong
Centroids@data$MatchingStage <- 'Stage 3'

MatchStrat3 <- Centroids

Match_complete <- spRbind(Match_complete , MatchStrat3)


##Strategy 4 - If multiple facilities have been found in a ward, attribute those in
## the same wards to variations in the convex zone

##Function to get the convex hull of a ward

GetWardsCentroid <- function(Data){
  out <- data.frame()
  
  wardsIds <- unique(Data$wardID)
  
  for (ward in wardsIds){
    print(ward)
    
    xx <- data.frame(wardID = ward)
    wardsCentroid <- gCentroid(Data[Data@data$wardID == ward,])
    xx$centroidlat <- wardsCentroid@coords[,1]
    xx$centroidlong <- wardsCentroid@coords[,2]
    
    out <- rbind(out, xx)
  }
  out
}

WardsCentroids <- GetWardsCentroid(Match_complete)

Hierarchy_Step4 <- subset(DHISFacilities , !(Level5ID %in% c(as.character(MatchStrat3$facilityID) ,
                                                              as.character(MatchStrat2$facilityID),
                                                              as.character(MatchStrat1$facilityID))))

## Attention rendre les facilities uniques avant de tourner
FacilitiesToWardCentroid <- function(facilities , wardsCentroids){
  out <- data.frame()
  
  levIDs <- unique(facilities$Level5ID)
  
  i <- 0
  n <- length(levIDs)
  toPrint <- 0
  
  for (facilityID in levIDs){
    facility <- facilities$Level5[facilities$Level5ID == facilityID]
    wardID <- facilities$Level4ID[facilities$Level5ID == facilityID]
    ward <- facilities$Level4[facilities$Level5ID == facilityID]
    state <- facilities$Level2[facilities$Level5ID == facilityID]
    lga <- facilities$Level3[facilities$Level5ID == facilityID]
    if (length(wardID) == 1){
      wardsCentroids$wardID <- as.character(wardsCentroids$wardID )
      centroidlat <- wardsCentroids$centroidlat[wardsCentroids$wardID == wardID]
      centroidlong <- wardsCentroids$centroidlong[wardsCentroids$wardID == wardID]
      if (length(centroidlat) > 0){
        xx <- data.frame(facility , facilityID , wardID , centroidlat , centroidlong , ward , state , lga)
        out <- rbind(out , xx)
      }
    }
    perc <- round(100*i/n , 0)
    if (perc != toPrint){
      toPrint <- perc
      print(paste0(toPrint , "%"))
    }
    i <- i+1
  }
  out
}

MatchStrat4 <- FacilitiesToWardCentroid(Hierarchy_Step4 , WardsCentroids)
coordinates(MatchStrat4) <- ~centroidlat+ centroidlong

MatchStrat4$place <- MatchStrat4$osmID <- 'centroid'

MatchStrat4$MatchingStage <- 'Stage 4'
MatchStrat4@data <- subset(MatchStrat4@data , select = c("facility" , "facilityID" , "wardID","lga","ward","place","MatchingStage" , "state" ,
                                                         "osmID"))


Match_complete <- spRbind(Match_complete , MatchStrat4)

##################################################
########## Validate Matching #####################
##################################################

Validation <- function(TestedSet , ValidationData){
  ValidData <- ValidationData@data
  ValidData$dhis_ID <- as.character(ValidData$dhis_ID)
  TestedSet <- TestedSet[TestedSet@data$facilityID %in% ValidData$dhis_ID ,]
  
  TestedCoords <- data.frame(facilityID = TestedSet$facilityID ,
                             latData = TestedSet@coords[,1],
                             longData = TestedSet@coords[,2] ,
                             matching_step = TestedSet$MatchingStage)
  
  TestedCoords <- merge(TestedCoords , ValidData , by.x = 'facilityID' , by.y  = 'dhis_ID')
  
  ValidCoords <- data.frame(facilityID = ValidationData$dhis_ID ,
                            Source = ValidationData$source ,
                            lateHealth = ValidationData@coords[,1],
                            longeHealth = ValidationData@coords[,2])
  
  Compare <- merge(ValidCoords , TestedCoords , by = 'facilityID') 
  dist <- pointDistance(cbind(Compare$lateHealth , Compare$longeHealth), 
                        cbind(Compare$latData , Compare$longData), 
                        lonlat = TRUE, allpairs=FALSE) /1000
  out <- cbind(Compare , dist)
  out
}

CompareSet1 <- Validation(MatchStrat1 , ValidationData)
CompareSet2 <- Validation(MatchStrat2 , ValidationData)
CompareSet3 <- Validation(MatchStrat3 , ValidationData)
CompareSet4 <- Validation(MatchStrat4 , ValidationData)

CompareSetComplete <- Validation(Match_complete , ValidationData)


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
  plot(NigeriaShp[substr(NigeriaShp$lga_name_matched , 1 ,2) == State ,] , 
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

DiganosticElements(CompareSet1)
DiganosticElements(CompareSet2)
DiganosticElements(CompareSet3)
DiganosticElements(CompareSet4)

DiganosticElements(CompareSetComplete)

writePointsShape(Match_complete, "FacilitiesGPS")
