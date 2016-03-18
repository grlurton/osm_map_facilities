library(stringr)
library(plyr)

setwd('J://Project/phc/nga/dhis')

idbFacil <- read.csv("J://temp/phc/facilities.csv")
idbGeog <- read.csv("J://temp/phc/phc_location_names.csv")
DhisHierar <- read.csv('HierarchyData.csv')
orgUnitsDhis <- read.csv('ExtractOrgUnitsRaw.csv')

idbStates <- as.character(unique(idbGeog$state))
dhisStates <- as.character(unique(DhisHierar$Level2))


idbGeog$STate_lga <- paste(idbGeog$state , idbGeog$lga , sep = '_')

idbStates[idbStates == 'Akwa Ibom'] <- 'Akwa-Ibom'

matchState <- data.frame(IDBState = character() , DHISState = character())
for(i in seq(length(idbStates))){
  mm <- grep(idbStates[i] , dhisStates , 
             ignore.case = TRUE , value = TRUE)
  if (length(mm) > 0){
    out <- data.frame(IDBState = idbStates[i] , DHISState = mm)
  }
  matchState <- rbind(matchState , out)
}

StateMatched <- merge(matchState , orgUnitsDhis , by.x = 'DHISState' , by.y = 'UnitName' , 
                      all.y = F)
StateMatched <- subset(StateMatched , select = c('IDBState' , 'DHISState' , 'UnitId'))

StateMatched$Parent <- 'Nigeria National'
StateMatched$ParentId <- 's5DPBsdoE8b'

StateMatched <- StateMatched[!duplicated(StateMatched) , ]
colnames(StateMatched) <- c('IDBName' , 'DHISName' , 'UnitId' , 'ParentName' , 'ParentId')
StateMatched$Level <- 'State'


###MATCH LGAS

idbLGA <- unique(idbGeog$lga)
idbLGA <- paste(' ' , idbLGA , ' ' , sep = '')
dhisLGAs <- unique(DhisHierar$Level3)
matchLGA <- data.frame(IDBLGA = character() , DHISLGA = character() , LGALong = character())
for(i in seq(length(idbLGA))){
  mm <- grep(idbLGA[i] , dhisLGAs , 
             ignore.case = TRUE , value = TRUE)
  if (length(mm) > 0){
    out <- data.frame(IDBLGA = idbLGA[i] , DHISLGA = mm , LGALong = )
    matchLGA <- rbind(matchLGA , out)
  }
}

##Check Doulbon

UniqueMatch <- function(data , indexVar){
  aa <- ddply(data , .(indexVar) , function(x) nrow(x))
  if(nrow(aa) > 1){
    c(as.character(subset(aa , V1 == 1)[,1]))
  }  
}

matchLGA <- matchLGA[matchLGA$IDBLGA %in% UniqueMatch(matchLGA , matchLGA$IDBLGA) , ]
matchLGA <- matchLGA[matchLGA$DHISLGA %in% UniqueMatch(matchLGA , matchLGA$DHISLGA) , ]

matchLGA$IDBLGA <- as.character(matchLGA$IDBLGA)
matchLGA$DHISLGA <- as.character(matchLGA$DHISLGA)

missing <- data.frame(IDBLGA = sort(as.character(idbLGA[!(idbLGA %in% matchLGA$IDBLGA)])) ,
                      DHISLGA = NA)

matchLGA <- rbind(matchLGA , missing)


matchLGA$DHISLGA[matchLGA$IDBLGA ==" Aboh Mbaise " ] <-"im Aboh-Mbaise Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ahiazu Mbaise " ] <-"im Ahiazu-Mbaise Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Aiyekire (Gbonyin) " ] <-"ek Aiyekire (Gbonyin) Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ajeromi-Ifelodun " ] <-"la Ajeromi/Ifelodun Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Akoko Edo " ] <-"ed Akoko-Edo Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Akoko North-East " ] <-"on Akoko North East Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Akoko North-West " ] <-"on Akoko North West Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Akoko South-East " ] <-"on Akoko South East Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Akoko South-West " ] <-"on Akoko South West Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Akuku-Toru " ] <-"ri Akuku Toru Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Amuwo-Odofin " ] <-"la Amuwo Odofin Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Arewa-Dandi " ] <-"ke Arewa Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Atakumosa East " ] <-"os Atakunmosa East Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Atakumosa West " ] <-"os Atakunmosa West Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ayedire " ] <-"os Aiyedire Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Biriniwa " ] <-"jg Birniwa Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Birnin Magaji/Kiyaw " ] <-"za Birnin Magaji Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Birnin-Gwari " ] <-"kd Birnin Gwari Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Damban " ] <-"ba Dambam Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Dambatta " ] <-"kn Danbatta Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Dange Shuni " ] <-"so Dange-Shuni Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Degama " ] <-"ri Degema Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ehime Mbano " ] <-"im Ehime-Mbano Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Esan North East " ] <-"ed Esan North-East Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Esan South East " ] <-"ed Esan South-East Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ese-Odo " ] <-"on Ese Odo Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ezinihitte Mbaise " ] <-"im Ezinihitte-Mbaise Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Garum Malam " ] <-"kn Garum Mallam Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ibeju-Lekki " ] <-"la Ibeju Lekki Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ifako-Ijaye " ] <-"la Ifako/Ijaye Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" IleOluji/Okeigbo " ] <-"on Ile-Oluji-Okeigbo Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Kiri Kasama " ] <-"jg Kiri Kasamma Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Koji " ] <-"ko Kogi Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Mbaitoli " ] <-"im Mbatoli Local Government Area"	
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Nassaraw " ] <-"kn Nassarawa Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Obgba/Egbema/Ndoni " ] <-"ri Ogba/Egbema/Ndoni Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Odo-0tin " ] <-"os Odo-Otin Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ogori/Mangongo " ] <-"ko Ogori/Magongo Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Orunda South " ] <-"an Orumba South Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Oshodi-Isolo " ] <-"la Oshodi/Isolo Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Oturkpo " ] <-"be Otukpo Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ovia North East " ] <-"ed Ovia North-East Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ovia South West " ] <-"ed Ovia South-West Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Sabon-Gari " ] <-"kd Sabon Gari Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Sule Tankarkar " ] <-"jg Sule-Tankarkar Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Urue Offong Oruko " ] <-"ak Urue Offong/Oruko Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Wasagu/Danko " ] <-"ke Danko/Wasagu Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Yenegoa " ] <-"by Yenagoa Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Zangon-Kataf " ] <-"kd Zangon Kataf Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ado " ] <-"be Ado Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ado-Odo/Ota " ] <-"og Ado Odo/Ota Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Eket " ] <-"ak Eket Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ekiti " ] <-"kw Ekiti Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ibadan North " ] <-"oy Ibadan North Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ijebu North " ] <-"og Ijebu North Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Ika " ] <-"ak Ika Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Kaura " ] <-"kd Kaura Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Nasarawa " ] <-"na Nasarawa Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Nasarawa Egon " ] <-"na Nasarawa Eggon Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Obi  Ngwa " ] <-"ab Obi Nwga Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Tofa " ] <-"kn Tofa Local Government Area"
matchLGA$DHISLGA[matchLGA$IDBLGA ==" Warri South " ] <-"de Warri South Local Government Area"

matchLGA$IDBLGA <- str_trim(matchLGA$IDBLGA)

LGAMatched <- merge(idbGeog , matchLGA , all = TRUE , by.x = 'lga' , by.y = 'IDBLGA')

LGAMatched$DHISLGA[LGAMatched$lga == 'Bassa' &LGAMatched$state ==  "Plateau"] <- 
  'pl Bassa Local Government Area'
LGAMatched$DHISLGA[LGAMatched$lga == 'Bassa' &LGAMatched$state ==  "Kogi"] <- 
  'ko Bassa Local Government Area'

LGAMatched$DHISLGA[LGAMatched$lga == 'Surulere' &LGAMatched$state ==  "Oyo"] <- 
  'oy Surulere Local Government Area'
LGAMatched$DHISLGA[LGAMatched$lga == 'Surulere' &LGAMatched$state ==  "Lagos"] <- 
  'la Surulere Local Government Area'

LGAMatched$DHISLGA[LGAMatched$lga == 'Obi' &LGAMatched$state ==  "Benue"] <- 
  'be Obi Local Government Area'
LGAMatched$DHISLGA[LGAMatched$lga == 'Obi' &LGAMatched$state ==  "Nasarawa"] <- 
  'na Obi Local Government Area'

LGAMatched$DHISLGA[LGAMatched$lga == 'Ifelodun' &LGAMatched$state ==  "Kwara"] <- 
  'kw Ifelodun Local Government Area'
LGAMatched$DHISLGA[LGAMatched$lga == 'Ifelodun' &LGAMatched$state ==  "Osun"] <- 
  'os Ifelodun Local Government Area'

LGAMatched$DHISLGA[LGAMatched$lga == 'Irepodun' &LGAMatched$state ==  "Kwara"] <- 
  'kw Irepodun Local Government Area'
LGAMatched$DHISLGA[LGAMatched$lga == 'Irepodun' &LGAMatched$state ==  "Osun"] <- 
  'os Irepodun Local Government Area'


###Getting the unitIDs

LGAMatched <- merge(LGAMatched , orgUnitsDhis , by.x = 'DHISLGA' , by.y = 'UnitName' , all.y = F)
LGAMatched <- subset(LGAMatched , state != "Water body" ,
                     select = c('lga' , 'DHISLGA' , 'UnitId' , 'ParentName'))
colnames(LGAMatched) <- c('IDBName' , 'DHISName' , 'UnitId' , 'ParentName') 

subStateMerge <- subset(StateMatched , selec = c(DHISName , UnitId))
colnames(subStateMerge) <- c('DHISName' , 'ParentId')

LGAMatched <- merge(LGAMatched , subStateMerge , by.x = 'ParentName' , by.y = 'DHISName')
LGAMatched$Level <- 'LGA'


rm(missing , out , i , idbLGA , mm , idbStates , dhisLGAs , dhisStates , matchState , 
   matchLGA , subStateMerge)


##Match Parents
LgaStatesFull <- rbind(StateMatched , LGAMatched)

write.csv(LgaStatesFull , 'LocationsExport.csv' ,  row.names = FALSE)

length(unique(LgaStatesFull$UnitId))