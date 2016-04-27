library(osmar)
library(maptools)
library(reshape2)

DHISFacilities <- read.csv('J://Project/phc/nga/dhis/HierarchyData.csv')

DHISFacilities <- unique(subset(DHISFacilities , select = -c(X , toMatch , Level6ID)))

uniqueID <- ddply(DHISFacilities , .(Level5ID) , nrow)
uniqueID <- subset(uniqueID , V1 == 1)

DHISFacilities <- subset(DHISFacilities , Level5ID %in% uniqueID$Level5ID )

uniqueName <- ddply(DHISFacilities , .(Level5 , Level4 , Level3) , nrow)
uniqueName <- subset(uniqueName , V1 == 1)

DHISFacilities <- subset(DHISFacilities , Level5 %in% uniqueName$Level5 )

osm_data <-  readShapePoints('data/OSMDataNigeria.shp')
##Prepare osm names to be used in the matching May be a bit more standardization here !!

osm_data$source <- tolower(as.character(osm_data$source))
osm_data$source[grep(pattern = 'ehealth' , x = osm_data$source)] <- 'ehealth'

health_projects <- osm_data[grepl(x = osm_data$source , pattern = 'ehealth|nmis|pepfar') , ]

#### Correction of typos and others.

# Adding extra space at the end of string to differentiate abbreviations 



phc <- list('Primary Health Centre' , c('PHC' , 'primary health caree' , 'Primary Centre' , 'Primary HealthCentre' , 'primary health vare centre' ,'primary healyh care' ,
                                        'P H C', 'primary health centr\\s' , 'Primary Health Health' , 'Primary Health Cemtre' , 'Primary Health Entre'))
centre <- list('Centre' , c('Center' ,'Centrte' , 'Cenntre' , 'Cente', 'Centre' ,  'Crntre' , 'Centrre', 'Cetre' , 'Clenter'))
clinic <- list('Clinic' , c( 'Clinjc' , 'Clininc' , 'Cljnic' , 'Cinic' , 'Clinc' , 'Clnic'  , 'ckinic' , 'Klinic'  , 'Cliinic'))
hc <- list('Health Centre' , c('H/C' , 'HC' ))
dispensary <- list('Dispensary' , c('Dipsensary' , 'Dispensarry' , 'Dispnsary' , 'Dispensory' , 'Dispenary' , 'Dispesanry' , 'Dispemsary' , 'Dispensry' , 'Disppensary' , 'Dispensery' , 
                                    'Dispensaty' , 'Hospensary' , 'Dispesary' , 'Disp\\s' , 'Dspensary'))
facility <- list('Facility' , c('Facilility' , 'Facilty'))
mat_home <- list('Maternity Home' , c('Maternity Home' , 'Mat Home' , 'Mathome'))
hosp_maternity <- list('Hospital and Maternity' , c('Hospita/Maternity'))
health <- list('Health' , c('Heath' , 'healthl' , 'Healh' , 'Helth' , 'Heaith' , 'Heatlh' , 'Healt\\s' ,'Heallth' ,'Healtj','Haelth') )
maternity <- list('Maternity' , c('Matenity','Materntiy','Meternity' , 'Matternity' , 'Mayernity' , 'Matarnity'))
hospital <- list('hospital' , c('Hopital' , 'hsoital' , 'Hosptial' , 'Hoapital' , 'Hospitak','Hosp\\s' , 'Hosipital' , 'Hospi\\s' , 'Hosptal'))
health_post <- list('Health Post' , c('Health-Post' , 'HealthPost' , 'HP\\s' ,'Halth Post', '\\s ealth Post'))
med_centre <- list('Medical Centre' , c('Medical Centre' , 'Med Centre','Midical Centre','Mediacl Centre',  'Medcal Centre', 'Medica Centre' ))
tub_lep <- list('Tuberculosis and Leprosy' , c('Tuberculusis Leprosy' , 'Tuberculusisi and Leprocy' , 'Tuberculusis And Leprosy' ,'TBL', 'Tuberculosis Leprosy' ))
mat_child_health <- list( 'Maternal and Child Health' , c('Maternity and Child Health' , 'Maternal Child Health' ,'MCH' ,'Maternal & Child Health' ,'Martanal and Chirld Health' ,
                                                          'Maternal&Child Health'))
conv_home <- list('Convalescence Home' , c('Convalascent Home' ,'conv home' ))
mdg <- list('')


facilities_patterns <- c('Primary Health Centre' ,  'General Hospital' , 'Medical Centre', 'Medical Clinic' , 'Health Centre' , 'Specialist Hospital' , 'Specialist Teaching Hospital' , 
                         'Nursing and Maternity Home' , 'Clinic and Maternity' , 'Dispensary' , 'Health Facility' , 'Health Post' ,
                         ' Referral Hospital' , 'Psychiatric Hospital' , 'Nursing Centre' , 'Nursing Home' , 'Maternal and Child Health Centre' , 'Maternity Home' , 'Army Hospital' ,
                         'Hospital and Maternity' , 'Convalescent Home' , 'Maternal and Child Health Clinic' , 'Primary Health Clinic' , 'Basic Health Clinic' , 'Primary Health Care' ,
                         'Community Hospital' , 'Medical and Diagnostic Centre' , 'Specialist Clinic' , 'Child and Maternity Centre' , 'Health Clinic' , 'Mobile Clinic' ,
                         'Women and Children Welfare Clinic' , 'Family Support Clinic' ,'Orphans and Less Previllage Clinic' , 'Orphan and Less Previlage Clinic' , 'Women and Child Welfare Clinic' ,
                         'Private Clinic' , 'Orpan and Less Privelege Clinic' ,  ' Maternal Child Health clinic' , 'Outreach Unit' , 'Cottage Hospital' , 'Military Hospital' ,
                         'Teaching Hospital' , 'Chest Hospital' , 'Eye Hospital' , 'Mission Hospital' , 'Medical Laboratory' , 'Rehabilitation Centre' ,
                         'clinic' , 'Muslim Hospital' ,  'Maternity Centre' , 'Catholic Hospital' , 'MCHC' , 'Memorial Hospital' ,
                          'Maternity and Child Welfare ' , 'Referal Hospital' , 
                         'Optical Centre' , 'Tuberculosis and Leprosy Control Unit' , 'Model Hospital' , 'Optical Centre' , 'Mother And Child Hospital' , 'Medical Laboratories' ,
                         'Mem. Hosp.' , 'Immunisation Centre'  , 'Convalscent Home' , 'Ophn And Less Previllige' , 
                         'Health Office' , 'Mortuary Service' , 'Family Health Unit' , 'Optical Service'  , 'Modern Maternity' ,  
                         'Maternity Child' , 'National Programe On Immunization' , 'Community Health Model' , ' General Hopital' , 'Dispensar' , 'Diagnostic Laboratory',
                         'Conv. Home' , 'health care centre' , 'Convalescent Centre' , 'Health Community Centre' , 'Basic Health Care' , 'Dental Centre' , 'Tuberculosis and Leprosy Referral Centre' ,
                         'M C H C'  , 'HMC' , 'Medicare Centre' ,  
                         'Maternity And Child Wellfare Centre' , 'Health care' , 'Medical Diagonstic Centre' , 'health care' , 'Care Home' , 'Maternity and Centre' , 'Medical Services' ,
                         'Ultrasound Diagnostic Centre' , 'Maternity and Child welfare' , 'Maternity and Child Wellfare' , 'Nursing Care Centre' , 'Radiological Services' , 'Medical Service' ,
                         'Medical Care' , 'Medical Reception Station' , 'Diagnostics Centre' ,'Welfare Centre' , 'Medical Consultants' , 'Malaria Laboratory' , 'Dental Laboratory' ,
                         'Maternity. Centre' , 'Model Laboratory' , 'Dental Unit' ,'Medical. Laboratory,' ,'Medical Consultancy' , 'Medical Diagnosis Laboratory' ,
                         'Basic Health Community' , ' Health Services', 'Specialist Home', 'Materntiy Home' , 'Mat. Centre' , 'MDGs' , 'Renal Dialysis Centre' ,
                         'Convalescent and Maternity' , 'Maternity and Dispensary' , 'Millenium Develoment Goal' , 'Unknown Ward' , 'Surgical and Medical Home' ,'Scanning Centre,' ,
                         'Maternity House,' , 'Medical. Laboratory. Services' , 'Medical and Scan and Diagnostic Centre' , 'Health Foundation' , 'Family Support Programme Centre' ,
                         'Millennium Development Goal' , 'Aged and Widow Centre' , 'National Paediatric Centre' , 'Medical Foundation' , 'Home Care' ,
                         'Maternity and Home' , 'Medical Store' , 'Mutual Health Plan' , ' Model Maternity'  , 'Care and Maternity' ,
                         'Specialist Centre' , 'Total Care' , 'Medicare' , 'Referral Centre' , 'University Health Service'  , 'comprehensive Centre' , 'Mediplex',
                         'CSO'  , 'Health Cent re'  , 'chemist' , 'Maternal and Child Hopspital' ,
                         'Family Support Progmme' , 'Eye Centre' , 'Urology Centre' , 'Comprehensive Health entre' , 'Eye Centre and Optic Service' ,
                         'Support Group' , 'Infirmary', 'Laboratory' , 'Distric Health Unit' , 'NPI Unit' , #National Program on Immunization 
                         'Epidemiology Unit' , 'Federal Model Centre' , 'Model' , 'Specialist' , 'Ward Health System' , 'ward' , 'Convalescent' , 'Foundation' , 
                         'Consulting Room' , 'Technical Centre' , 'Medical Consult' , ' Medical Lab\\s' , 'Medical Art Centre' , 'Medical Limited' ,'Medical' ,
                         'Convalscent Centre' , 'Millennium Developement Goal Health' , 'Infant Welfare' ,  'Outreach Centre' , 'School of Nursing' , 'Trauma Centre' , 'Paediatrics Centre' ,
                         'Children Centre' ,'X-ray Centre' , 'LGA Secretariat NPI Office' ,'Local Government Action C Aids' ,'City Council' ,
                         'Government hospital' , 'Private Hospital' , 'Dental Maxillo-Facial Hospital' , 'Comprehensive Hospital' , ' Primary Health ' ,
                         'Maternity' , 'hospital' , 'leprosy' )#Town , EN PARENTHESE

typos_patterns <- list(phc , centre , hc , clinic ,  dispensary , facility , mat_home , hosp_maternity , health , maternity , hospital , health_post , med_centre)

drop <- c("z. " , "Unknown " , "os z ", "os. " , "os.. ","os . ","os .. ","os " , ",, ", ",. ",".. " ,"... " , "ak Unknown ",
          "an " , "os B " ,"Xxx " , "None "  , "T ")


correct_typos <- function(data , typos_patterns){
  var_2 <- as.character(data)

  var_2 <- paste0(var_2 , ' ')
  var_2 <- gsub('\\{|\\}' , '-' , var_2)
  var_2 <- gsub('\\(|\\)' , '-' , var_2)
  var_2 <- gsub('\\.' , '' , var_2)
  var_2 <- gsub(',' , '' , var_2)

  for (i in seq(1,length(typos_patterns))){
    pattern <- typos_patterns[[i]][[2]]
    pattern <- paste(pattern , collapse = '|')
    var_2 <- gsub(pattern = pattern, typos_patterns[[i]][[1]] , var_2 , ignore.case = TRUE)
  }
  var_2
}


health_projects@data$unif_name <- as.character(health_projects@data$name)
health_projects@data$unif_name[health_projects@data$unif_name == 'edo'] <- as.character(health_projects@data$primary_na[health_projects@data$unif_name == 'edo'])

DHISFacilities$Level5_cleaned <- correct_typos(DHISFacilities$Level5 , typos_patterns)
health_projects@data$name_cleaned <- correct_typos(health_projects@data$unif_name , typos_patterns)

osm_data$source <- as.character(osm_data$source)
osm_data$source[grep(pattern = 'ehealth' , x = tolower(osm_data$source))] <- 'ehealth'

