DHISFacilities <- read.csv('J://Project/phc/nga/dhis/HierarchyData.csv')
osm_data <-  readShapePoints('data/OSMDataNigeria.shp')
##Prepare osm names to be used in the matching May be a bit more standardization here !!

osm_data$source <- tolower(as.character(osm_data$source))
osm_data$source[grep(pattern = 'ehealth' , x = osm_data$source)] <- 'ehealth'

health_projects <- osm_data[grepl(x = osm_data$source , pattern = 'ehealth|nmis|pepfar') , ]

health_projects@data$name <- gsub('\\{|\\}' , '-' , health_projects@data$name)
health_projects@data$name <- gsub('Center' , 'Centre'  , health_projects@data$name)

DHISFacilities$Level5 <- gsub('PHC|primary health caree|Primary Centre|Primary HealthCentre' , 'Primary Health Centre' , DHISFacilities$Level5 , ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('Center|Clinjc|Clininc|Cljnic|Cinic|Clinc|Centrte|Cenntre' , 'Centre' , DHISFacilities$Level5, ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('H/C| HC' , 'Health Centre' , DHISFacilities$Level5, ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('Dipsensary|Dispensarry|Dispnsary|Dispensory|Dispenary|Dispesanry|Dispemsary|Dispensry|Disppensary' , 'Dispensary' , DHISFacilities$Level5, ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('Facilility' , 'Facility' , DHISFacilities$Level5, ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('Maternity. Home|Mat. Home|Mathome' , 'Maternity Home' , DHISFacilities$Level5, ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('Hospita/Maternity' , 'Hospital and Maternity' ,  DHISFacilities$Level5, ignore.case = TRUE)
DHISFacilities$Level5 <- gsub('Heath|healthl|Healh|Helth' , 'Health' ,  DHISFacilities$Level5, ignore.case = TRUE)

DHISFacilities$Level5 <- gsub('  ' , ' ' , DHISFacilities$Level5)


drop <- c("z." , "Unknown" , "os z", "os." , "os.", "os." ,"os.","os..","os..","os..","os.." , "os .","os ." , "os .","os .","os ..","os"  , ",,", ",.",",.", "..",".." ,
          ".."   ,"..",".."  ,".."  ,  ".." ,".."   , ".." ,"..."   )

DHISFacilities <- subset(DHISFacilities , !(Level5 %in% drop))

facilities_patterns <- c('Primary Health Centre' , 'General Hospital' , 'Medical Centre' , 'Medical Clinic' , 'Health Centre' , 'Specialist Hospital' , 'Specialist Teaching Hospital' , 
                         'Health Post' , 'Nursing and Maternity Home' , 'Clinic and Maternity' , 'Dispensary' , 'Health Facility' , 
                         ' Referral Hospital' , 'Psychiatric Hospital' , 'Nursing Centre' , 'Nursing Home' , 'Maternal and Child Health Centre' , 'Maternity Home' , 'Army Hospital' ,
                         'Hospital and Maternity' , 'Convalescent Home' , 'Maternal and Child Health Clinic' , 'Primary Health Clinic' , 'Basic Health Clinic' , 'Primary Health Care' ,
                         'Community Hospital' , 'Medical and Diagnostic Centre' , 'Specialist Clinic' , 'Child and Maternity Centre' , 'Health Clinic' , 'Mobile Clinic' ,
                         'Women and Children Welfare Clinic' , 'Family Support Clinic' ,'Orphans and Less Previllage Clinic' , 'Orphan and Less Previlage Clinic' , 'Women and Child Welfare Clinic' ,
                         'Private Clinic' , 'Orpan and Less Privelege Clinic' ,  ' Maternal Child Health clinic' , 'Outreach Unit' , 'Cottage Hospital' , 'Military Hospital' ,
                         'Teaching Hospital' , 'Chest Hospital' , 'Eye Hospital' , 'Mission Hospital' , '(Specialist) Hospital' , 'Medical Laboratory' , 'Rehabilitation Centre' ,
                         'clinic' , 'Muslim Hospital' ,  'Maternity Centre' , 'Catholic Hospital' , 'MCHC' , 'MCH' , 'Memorial Hospital' , 'Maternal Child Health ' , 
                         'Maternal and Child Health' , 'Maternity and Child Welfare ' , 'Tuberculusis Leprosy' , 'Tuberculusisi and Leprocy' , 'Referal Hospital' , 'Tuberculusis And Leprosy' ,
                         'Optical Centre' , 'Tuberculosis and Leprosy Control Unit' , 'Model Hospital' , 'Optical Centre' , 'Mother And Child Hospital' , 'Medical Laboratories' ,
                         'Mem. Hosp.' , 'Immunisation Centre' , 'Primary Health Health ' , 'Convalscent Home' , 'Ophn And Less Previllige' , 'Maternity and Child Health' , 'Maternal Child Health' ,
                         'Health Office' , 'Mortuary Service' , 'Family Health Unit' , 'Optical Service' ,'Martanal and Chirld Health' , 'Modern Maternity' , 'Maternal & Child Health' , 
                         'Maternal&Child Health' , 
                      
                         
                         'hospital')
fac_patt <- paste(facilities_patterns , collapse = '|')

grep(x = DHISFacilities$Level5 , pattern = fac_patt , ignore.case = TRUE , value = TRUE , invert = TRUE)

grep(x = DHISFacilities$Level5 , 'Lamido Borno Maternal Child Health' , value=TRUE)

rem_pattern <- gsub(x = DHISFacilities$Level5 , pattern = fac_patt , replacement = "" , ignore.case = TRUE)
