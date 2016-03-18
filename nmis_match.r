setwd('J://Project/dhis/nigeria/')

data_nmis <- 
  read.csv("../../phc/nga/nmis/Health_Mopup_and_Baseline_NMIS_Facility.csv")

data_nmis$id <- seq(1:nrow(data_nmis))

#library(maptools)

#coordinates(data_nmis) = ~ longitude + latitude

length(unique(data_nmis@data$facility_id))

length(unique(data_nmis@data$facility_name))
## Lots of redudency... probably names not properly entered -> will have to hand match some


##Launching DHIS Data

library(stringr)
library(plyr)

MetaData      <- read.csv(file = '../../phc/nga/dhis/MetadataUnitsRaw.csv')
HierarchyData <- read.csv('../../phc/nga/dhis/HierarchyData.csv')
StateMatched  <- read.csv('../../phc/nga/dhis/LocationsExport.csv')
data  <- read.csv('../../phc/nga/dhis/Data.csv')
data_elements  <- read.csv('../../phc/nga/dhis/')

dhisInput <- subset(MetaData , UnitLevel > 4 , select = c(UnitName, UnitId))
dhisInput <- dhisInput[!duplicated(dhisInput) , ]


HierarchyData <- HierarchyData[,-1]
HierarchyData <-  HierarchyData[!duplicated(HierarchyData) , ]

dhisInput <- merge(dhisInput , HierarchyData , by.x = 'UnitId' , by.y = 'Level5ID' , 
                   all.x = FALSE , all.y = FALSE)

multipleParent <- 
  unique(as.character(dhisInput$UnitId))[table(as.character(dhisInput$UnitId)) > 1]
dhisInput <- subset(dhisInput , !(UnitId %in% multipleParent))
dhisInput <- subset(dhisInput , select = c(UnitName , UnitId , Level2))

colnames(dhisInput) <- c('facility' , 'facilityID' , 'State')

## In first step, the goal is to change take out the indicator of health facility type out of the names of the facilities 

## Second step will be to link different appelations of health facilities to see what they are like

## Third step will to match with other data source solely on the name

#### STANDARDIZE FACILITY NAMES ###



names <- data_nmis$facility_name
names <- tolower(str_trim(names))
names <- gsub('\\.|,' , '' , names)
names <- gsub('&' , 'and' , names)
names <- gsub('  ' , ' ' , names)
names<- paste(' ',names,' ' , sep = '')

## Solve abbreviations
names <- gsub('phc|p hc|p h c' , 
              'primary health care center' , names)

names <- gsub('hosp ' , 'hospital' , names)
names <- gsub('gen hospital' , 'general hospital' , names)
names <- gsub('disp ' , 'dispensary ' , names)
names <- gsub('hp' , 'health post' , names)
names <- gsub('hc|h/c' , 'health center' , names)
names <- gsub('bhc|b h c|bhealth center' , 'basic health center' , names)
names <- gsub('mch|m c h' , 'mother and child health' , names)
names <- gsub('healthcare' , 'health care' , names)


interactive_change <- function(element , relevant , type_facility){
  print(paste('What about' , element , '?' , sep = ' '))
  print('Original Names :')
  print(relevant$name[relevant$list_element == element])
  change <- ''
  while(!(change  %in% c('ko' , 'ctt' , 'ctn'))){
    change <- readline(paste('change to' , 
                             type_facility , '(ctt)' ,
                             'change to new type (ctn), rewrite all (ra) or keep original (ko)'))
  }
  if(change == 'ctt'){
    relevant$new_name[relevant$list_element == element] <- 
      gsub(element , type_facility , relevant$name[relevant$list_element == element])
    }
  if(change == 'ctn'){
    change_to <- readline('Enter new value :')
    relevant$new_name[relevant$list_element == element] <- 
      gsub(element , change_to , relevant$name[relevant$list_element == element])
    }
  if(change == 'ra'){
    new_name <- readline('Enter new facility name :')
    relevant$new_name[relevant$list_element == element] <- new_name
    }
  relevant
}

delist <- function(list_element){
  data.frame(name = paste(list_element , collapse = ' ') , list_element)
}

correct_partial <- function(names_tab , type_facility , max_dist){
  distances <- adist(names_tab$list_element , type_facility)
  relevant <- subset(names_tab , distances <= max_dist & distances > 0)
  variants <- as.character(unique(relevant$list_element))
  relevant$new_name
  for(i in 1:length(variants)){
    element <- variants[i]
    relevant <- interactive_change(element , relevant , type_facility)
  }
  relevant
}

run_partial_correction <- function(names , test_words){
  listNames <- str_split(names , ' ')
  names_tab <- ldply(listNames , delist)
  correction_table <- data.frame(names = character(), new_name = character(), list_element= character())
  for(i in 1:length(test_words)){
    to_test <- test_words[i]
    print(to_test)
    distance <- readline('What distance from this word do you allow ?')
    out <- correct_partial(names_tab , to_test , distance)
    correction_table <- rbind(correction_table , out)
  }
  correction_table <- correction_table[!duplicated(correction_table) , ]
  correction_table
}

test_words <- c('health' , 'care')

run_partial_correction(names , test_words)

#, 'center' , 'hospital' , 'dispensary' , 'clinic' , )



test <- correct_partial(names , 'dispensary' , 4)
test2 <- test[!duplicated(test) , ]


facilities <- merge(facilities , test2 , all.x = TRUE , all.y = FALSE , by.x = "names" ,
                    by.y = 'name')

facilities$name_short[!is.na(facilities$new_name)] <- 
  facilities$new_name[!is.na(facilities$new_name)]

health <- correct_partial(names , 'health' , 2)


## CHANGER LA SEQUENCE : 
##    0. Analyse syntaxique
##    1. changements simples
##    2. faire les mots approximatifs un a un => pas faire le split a chaque fois...
##    2b. faire les matchs approximatifs sur les combinaisons standards
##    3. grep sur combinaisons standards
##    4. table
##    5. grep sur mots simples

