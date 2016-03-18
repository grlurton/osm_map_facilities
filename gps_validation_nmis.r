library(maptools)
library(stringr)
library(plyr)

setwd('J://Project/phc/nga/dhis')

validation_data <- read.csv('c://Users/grlurton/Documents/NigeriaHMIS/data/Health_Mopup_and_Baseline_NMIS_Facility.csv' , as.is = TRUE)

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

dhisInput <- merge(dhisInput , HierarchyData , by.x = 'UnitId' , by.y = 'toMatch' , 
                   all.x = TRUE , all.y = FALSE)

dhisInput$Level2.x[is.na(dhisInput$Level2.x)] <- dhisInput$Level2.y[is.na(dhisInput$Level2.x)]

dhisInput <- subset(dhisInput , select = c(UnitName , UnitId , Level2.x))
colnames(dhisInput) <- c('facility' , 'facilityID' , 'State')



########
##Format DHIS facility names
########

dhisInput$facility <- str_trim(dhisInput$facility)
dhisInput$facility <- gsub(' CENTER' , ' CENTRE ' ,dhisInput$facility)
dhisInput <- subset(dhisInput , !(facility %in% c(",,", ",." , "..","..." )))
dhisInput$facility <- tolower(dhisInput$facility)

#######
##Format NMIS names
#######

validation_data$facility_name <- tolower(validation_data$facility_name)

tab_facility_name <- ddply(validation_data , .(facility_name) , nrow)

facility_louche <- subset(tab_facility_name , V1 > 2)

validation_data <- subset(validation_data , !(facility_name %in% facility_louche$facility_name))


