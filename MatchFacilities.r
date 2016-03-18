library(stringr)
library(plyr)

setwd('J://Project/phc/nga/dhis')

DataUnits <- as.character(read.csv('UnitsWithData.csv')$x)
ReportUnits <- unique(as.character(read.csv('OrgUnitxDataSets.csv')$as.character.orgUnits))

SDIFacilities <- read.csv('J://temp/phc/facilities.csv')
MetaData <- read.csv(file = 'MetadataUnitsRaw.csv')
HierarchyData <- read.csv('HierarchyData.csv')
StateMatched <- read.csv('LocationsExport.csv')

UnitsToMatch <- unique(c(DataUnits , ReportUnits))

FacilitiesToMatch <- unique(subset(MetaData , UnitId %in% UnitsToMatch & UnitLevel > 4)$UnitId)


dhisInput <- subset(MetaData , UnitId %in% FacilitiesToMatch , select = c(UnitName, UnitId))
dhisInput <- dhisInput[!duplicated(dhisInput) , ]

dhisInput <- merge(dhisInput , HierarchyData , by.x = 'UnitId' , by.y = 'Level5ID' , 
                   all.x = TRUE , all.y = FALSE)
multipleParent <- unique(as.character(dhisInput$UnitId))[table(as.character(dhisInput$UnitId)) > 1]
dhisInput <- subset(dhisInput , !(UnitId %in% multipleParent))
dhisInput <- subset(dhisInput , select = c(UnitName , UnitId , Level2))

dhisInput <- merge(dhisInput , HierarchyData , by.x = 'UnitId' , by.y = 'ToMatch' , 
                   all.x = TRUE , all.y = FALSE)

dhisInput$Level2.x[is.na(dhisInput$Level2.x)] <- dhisInput$Level2.y[is.na(dhisInput$Level2.x)]
  
dhisInput <- subset(dhisInput , select = c(UnitName , UnitId , Level2.x))
colnames(dhisInput) <- c('facility' , 'facilityID' , 'State')

############################
########FACILITIES##########
############################

########
##Format SDI facility names
########

SDIFacility <- str_trim(unique(SDIFacilities$facility_name))
SDIFacility <- gsub('PHCC|P\\.H\\.C\\.C' , 'PRIMARY HEALTH CARE CENTRE' ,SDIFacility)
SDIFacility <- gsub('PHC|P\\.H\\.C' , 'PRIMARY HEALTH CENTRE' ,SDIFacility)
SDIFacility <- gsub('BHC|B\\.H\\.C' , 'BASIC HEALTH CENTRE' ,SDIFacility)
SDIFacility <- gsub('C\\.H\\.C|CHC' , 'COMMUNITY HEALTH CENTRE' ,SDIFacility)
SDIFacility <- gsub('H\\/P|HP | HP' , 'HEALTH POST' ,SDIFacility)
SDIFacility <- gsub('H\\/C|H\\.C| HC|HC ' , 'HEALTH CENTER' ,SDIFacility)
SDIFacility <- gsub('M\\.C\\.H|MCH' , ' MODEL HEALTH CENTRE' ,SDIFacility)
SDIFacility <- gsub('HEALTH CENTE |HAELTH CENTRE' , 'HEALTH CENTER' ,SDIFacility)
SDIFacility <- gsub('PRIMARY HELTH CENTR', 'PRIMARY HEALTH CENTRE' ,SDIFacility)
SDIFacility <- gsub('GEN HOSPITAL|GENARAL HOSPITAL', 'GENERAL HOSPITAL' ,SDIFacility)
SDIFacility <- gsub('PRY HEALTH CARE CLNIC', 'PRIMARY HEALTH CLINIC' ,SDIFacility) 

SDIFacility <- gsub(' CENTER' , ' CENTRE' ,SDIFacility)
SDIFacility <- gsub('  ' , ' ' ,SDIFacility)

InverseName <- function(x , FacType){
  n <- nchar(FacType)
  sub <- substr(x  , 1 , n) == FacType
  x[sub] <- paste(substr(x[sub]  , n + 2 , nchar(x[sub])) , FacType , sep = ' ')
  x
}

SDIFacility <- InverseName(SDIFacility , 'MODEL HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'PRIMARY HEALTH CARE CLINIC')
SDIFacility <- InverseName(SDIFacility , 'PRIMARY HEALTH CLINIC')
SDIFacility <- InverseName(SDIFacility , 'PRIMARY HEALTH CARE CENTRE')
SDIFacility <- InverseName(SDIFacility , 'PRIMARY HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'BASIC HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'COMMUNITY HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'HEALTH POST')
SDIFacility <- InverseName(SDIFacility , 'HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'REFERRAL HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'GENERAL HOSPITAL')
SDIFacility <- InverseName(SDIFacility , 'COMPREHENSIVE HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'BASIC HEALTH CLINIC')
SDIFacility <- InverseName(SDIFacility , 'COMMUNITY HEALTH CLINIC')
SDIFacility <- InverseName(SDIFacility , 'RURAL HOSPITAL')
SDIFacility <- InverseName(SDIFacility , 'PRIMARY HEALTH CARE')
SDIFacility <- InverseName(SDIFacility , 'MODEL PRIMARY HEALTH CENTRE')
SDIFacility <- InverseName(SDIFacility , 'MODEL HEALTH CLINIC')
SDIFacility <- InverseName(SDIFacility , 'PRIMARY HEALTH CARE CENTRE')
SDIFacility <- InverseName(SDIFacility , 'HEALTH CLINIC')


SDIToMatch <- cbind(SDIFacilities , SDIFacility)

SDIToMatch$SDIFacility <- gsub('  ' , ' ' , SDIToMatch$SDIFacility)
SDIToMatch$SDIFacility <- str_trim(SDIToMatch$SDIFacility)
SDIToMatch$SDIFacility <- paste(' ' , SDIToMatch$SDIFacility , sep = '')

SDIDic <- data.frame(original_name = SDIToMatch$facility_name , 
                     simplified_name = SDIToMatch$SDIFacility)

write.csv(SDIDic , file = 'SDIDico.csv')

########
##Format DHIS facility names
########

dhisInput$facility <- str_trim(dhisInput$facility)
dhisInput$facility <- gsub(' CENTER' , ' CENTRE ' ,dhisInput$facility)
dhisInput <- subset(dhisInput , !(facility %in% c(",,", ",." , "..","..." )))



##First Step : Match, by LGA roughly on the complete names

matchFacility <- function(SDIData , DHISData){
  DHISFacility <-as.character(DHISData$facility)
  SDIFacility <- as.character(SDIData$SDIFacility)
  matchFacility <- data.frame(SDIFacility = character() , DHISFacility = character() , UnitId  = character())
  for(i in seq(length(SDIFacility))){
    print(paste(i , SDIFacility[i] , sep = ' - '))
    mm <- grep(SDIFacility[i] , DHISFacility , ignore.case = TRUE , value = TRUE)
    if (length(mm) == 1){
      out <- data.frame(SDIFacility = SDIData$facility_name[i] , 
                        DHISFacility = as.character(DHISData$facility[DHISData$facility ==mm]) , 
                        UnitId = as.character(DHISData$facilityID[DHISData$facility == mm]))
      matchFacility <- rbind(matchFacility , out)
    }
  }
  if (nrow(matchFacility) > 1){
    matchFacility <- matchFacility[matchFacility$SDIFacility %in%
                                     UniqueMatch(matchFacility , matchFacility$SDIFacility) , ]
    matchFacility <- matchFacility[matchFacility$DHISFacility %in%
                                     UniqueMatch(matchFacility , matchFacility$DHISFacility) , ]
  }
  matchFacility
}

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  if(nrow(aa) > 1){
    c(as.character(subset(aa , V1 == 1)[,1]))
  }  
}


Step1Out <- ddply(SDIToMatch , .(geography) , 
                  function(x){
                    print(unique(x$geography))
                    DHISState <- StateMatched$DHISName[as.character(StateMatched$IDBName) == 
                                                          unique(as.character(x$geography)) &
                                                         as.character(StateMatched$Level) ==
                                                         'State']
                    DHISState <- as.character(DHISState)
                    dhisInput$State <- as.character(dhisInput$State)
                    dhisin <- subset(dhisInput , State == DHISState)
                    matchFacility(x  , dhisin)
                  })

Step1NotMatched <- subset(SDIToMatch, !(SDIToMatch$facility_name %in% Step1Out$SDIFacility))
Step1DHISNotMatched <- subset(dhisInput , !(dhisInput$facility %in% Step1Out$DHISFacility))

##Step 2 : using only locality name

facType <- c('BASIC HEALTH CENTRE' , 'REFERRAL HEALTH CENTRE' , 'GENERAL HOSPITAL' ,
             'COMMUNITY HEALTH CENTRE' , 'HEALTH POST' , 'MODEL HEALTH CENTRE' , 
             'PRIMARY HEALTH CARE CLINIC' , 'PRIMARY HEALTH CLINIC' , 'COMPREHENSIVE HEALTH CENTRE',
             'PRIMARY HEALTH CARE CENTRE' , 'PRIMARY HEALTH CENTRE' , 'RURAL HOSPITAL' ,
             'DISPENSARY' , 'MATERNITY' , 'HEALTH CLINIC' , 'HEALTH CENTRE' , 'PRIMARY HEALTH CARE',
             'HEALTH CARE CENTRE' , 'HEALTH FACILITY' , 'POLY CLINIC' , 'COMMUNITY HOSPITAL A' ,
             'CLINIC' , 'PRIMARY' , 'HEALTH SERVICE CENTRE' , 'COMPREHENSIVE' , 'COMMUNITY','M.D.G',
             'BASIC|\\(|\\)','SPECIALIST HOSPITAL','MEDICAL CENTRE','HEALTH CENTE',
             '/ HEALYH CARE CENTRE',' HP',' HC')

Step1NotMatched$SDIFacilitySave <- Step1NotMatched$SDIFacility
Step1NotMatched$SDIFacility <-   gsub(pattern = paste(facType , collapse ='|') , '' , 
                                      Step1NotMatched$SDIFacility)

Step1NotMatched$SDIFacility <- gsub('  ' , ' ' , Step1NotMatched$SDIFacility)
Step1NotMatched$SDIFacility <- str_trim(Step1NotMatched$SDIFacility)
Step1NotMatched$SDIFacility <- paste(' ' , Step1NotMatched$SDIFacility , ' ' , sep='')

Step2Out <- ddply(Step1NotMatched , .(geography) , 
                  function(x){
                    print(unique(x$geography))
                    DHISState <- StateMatched$DHISName[as.character(StateMatched$IDBName) == 
                                                         unique(as.character(x$geography)) &
                                                         as.character(StateMatched$Level) ==
                                                         'State']
                    DHISState <- unique(as.character(DHISState))
                    print(DHISState)
                    Step1DHISNotMatched$State <- as.character(Step1DHISNotMatched$State)
                    dhisin <- subset(Step1DHISNotMatched , State == DHISState)
                    matchFacility(x  , dhisin)
                  })

Step2NotMatched <-subset(Step1NotMatched , !(SDIToMatch$facility_name %in% Step2Out$SDIFacility))
Step2DHISNotMatched <- subset(Step1DHISNotMatched , 
                              !(Step1DHISNotMatched$facility %in% Step2Out$DHISFacility))


###Finally matched interactively the remaining facilities

InteractiveMatch <- function(SDIData , DHISFacility){
  SDIFacility <- SDIData$SDIFacility
  matchFacility <- data.frame(SDIFacility = character() , DHISFacility = character())
  for(i in seq(length(SDIFacility))){
    mm <- grep(SDIFacility[i] , DHISFacility , ignore.case = TRUE , value = TRUE)
    Nmatches <- length(mm)
    if (Nmatches > 0){
      possibles <- SDIData$SDIFacilitySave[SDIData$SDIFacility == SDIFacility[i]]
      print(paste(i , 'Pattern' , SDIFacility[i] , sep = ' - '))  
      print(paste('Equivalents in this pattern : ' , possibles , sep = ' - '))
      print(paste('To Compare ' , SDIData$SDIFacilitySave[i] , sep = " - "))
      print(paste('Type ' , SDIData$fac_type[i] , sep = " - "))
      print(paste(mm))
      choose <- readline('Which one ? ')
      out <- data.frame(SDIFacility = SDIData$facility_name[i] , DHISFacility = mm[as.numeric(choose)])
      matchFacility <- rbind(matchFacility , out)
    }
  }
  matchFacility
}

Step3Out <- ddply(Step2NotMatched , .(geography) , 
                  function(x){
                    print(unique(x$geography))
                    DHISState <- StateMatched$DHISName[as.character(StateMatched$IDBName) == 
                                                         unique(as.character(x$geography)) &
                                                         as.character(StateMatched$Level) ==
                                                         'State']
                    DHISState <- as.character(DHISState)
                    dhisin <- subset(Step2DHISNotMatched , State == DHISState)
                    InteractiveMatch(x  , dhisin$facility)
                  })

Step3Out <- subset(Step3Out , !is.na(DHISFacility))
Step3Out <- Step3Out[Step3Out$DHISFacility %in% UniqueMatch(Step3Out , Step3Out$DHISFacility) , ]
Step3Out <- merge(Step3Out , dhisInput , by.x = 'DHISFacility' , by.y = 'facility')
Step3Out <- subset(Step3Out , select = c(DHISFacility , geography , SDIFacility , facilityID))
colnames(Step3Out)[4] <- 'UnitId'

MatchedFacilities <- rbind(Step1Out , Step2Out , Step3Out)

write.csv(MatchedFacilities , 'matchedFacilities.csv' , row.names = FALSE)
