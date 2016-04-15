library(osmar)
library(maptools)
library(reshape2)

DHISFacilities <- read.csv('J://Project/phc/nga/dhis/HierarchyData.csv')
osm_data <-  readShapePoints('data/OSMDataNigeria.shp')
##Prepare osm names to be used in the matching May be a bit more standardization here !!

osm_data$source <- tolower(as.character(osm_data$source))
osm_data$source[grep(pattern = 'ehealth' , x = osm_data$source)] <- 'ehealth'

health_projects <- osm_data[grepl(x = osm_data$source , pattern = 'ehealth|nmis|pepfar') , ]

health_projects@data$name <- gsub('\\{|\\}' , '-' , health_projects@data$name)
health_projects@data$name <- gsub('.' , '' , health_projects@data$name)
health_projects@data$name <- gsub(',' , '' , health_projects@data$name)


#### Correction of typos and others.

# Adding extra space at the end of string to differentiate abbreviations 

DHISFacilities$Level5 <- paste0(DHISFacilities$Level5 , ' ')

phc <- list('Primary Health Centre' , c('PHC' , 'primary health caree' , 'Primary Centre' , 'Primary HealthCentre' , 'primary health vare centre' ,'primary healyh care' ,
                                        'Primary HealthCentre'))
centre <- list('Centre' , c('Center' , 'Clinjc' , 'Clininc' , 'Cljnic' , 'Cinic' , 'Clinc' , 'Centrte' , 'Cenntre' , 'Cente', 'Centre' , 'Clnic' , 'Crntre' , 'Centrre', 'Cetre' , 
                            'Cliniepicetou','ckinic'))
hc <- list('Health Centre' , c('H/C' , 'HC' ))
dispensary <- list('Dispensary' , c('Dipsensary' , 'Dispensarry' , 'Dispnsary' , 'Dispensory' , 'Dispenary' , 'Dispesanry' , 'Dispemsary' , 'Dispensry' , 'Disppensary' , 'Dispensery' , 
                                    'Dispensaty' , 'Hospensary' , 'Dispesary' , 'Disp\\s'))
facility <- list('Facility' , c('Facilility' , 'Facilty'))
mat_home <- list('Maternity Home' , c('Maternity. Home' , 'Mat. Home' , 'Mathome'))
hosp_maternity <- list('Hospital and Maternity' , c('Hospita/Maternity'))
health <- list('Health' , c('Heath' , 'healthl' , 'Healh' , 'Helth' , 'Heaith' , 'Heatlh' , 'Healt\\s' ,'Heallth' ,'Healtj','Haelth') )
maternity <- list('Maternity' , c('Matenity','Materntiy'))
hospital <- list('hospital' , c('Hopital' , 'hsoital' , 'Hosptial' , 'Hoapital' , 'Hospitak','Hosp\\s' , 'Hosipital'))
health_post <- list('Health Post' , c('Health-Post' , 'HealthPost' , 'HP\\s' ,'Halth Post'))
med_centre <- list('Medical Centre' , c('Medical. Centre' , 'Med. Centre'))


typos_patterns <- list(phc , centre , hc , dispensary , facility , mat_home , hosp_maternity , health , maternity , hospital , health_post , med_centre)

DHISFacilities$Level5Typoed <- DHISFacilities$Level5

for (i in seq(1,length(typos_patterns))){
  pattern <- typos_patterns[[i]][[2]]
  pattern <- paste(pattern , collapse = '|')
  DHISFacilities$Level5Typoed <- gsub(pattern = pattern, typos_patterns[[i]][[1]] , DHISFacilities$Level5Typoed , ignore.case = TRUE)
}




### Normalization

# Taking out Extra Spaces

DHISFacilities$Level5Typoed <- gsub('  ' , ' ' , DHISFacilities$Level5Typoed)

# Dropping uninformative names
drop <- c("z. " , "Unknown " , "os z ", "os. " , "os.. ","os . ","os .. ","os " , ",, ", ",. ",".. " ,"... " , "ak Unknown ","an " , "os B "  )

DHISFacilities <- subset(DHISFacilities , !(Level5Typoed %in% drop))

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
                         'Maternal&Child Health' , 'Maternity Child' , 'National Programe On Immunization' , 'Community Health Model' , ' General Hopital' , 'Dispensar' , 'Diagnostic Laboratory',
                         'Conv. Home' , 'health care centre' , 'Convalescent Centre' , 'Health Community Centre' , 'Basic Health Care' , 'Dental Centre' , 'Tuberculosis and Leprosy Referral Centre' ,
                         'M C H C' , 'Medical Hosptal' , 'Tuberculosis Leprosy' , 'HMC' , 'Medicare Centre' , 'Convalescence Home' , 'Tuberculosis and Leprosy' ,
                         'Maternity And Child Wellfare Centre' , 'Health care' , 'Medical Diagonstic Centre' , 'health care' , 'Care Home' , 'Maternity and Centre' , 'Medical Services' ,
                         'Ultrasound Diagnostic Centre' , 'Maternity and Child welfare' , 'Maternity and Child Wellfare' , 'Nursing Care Centre' , 'Radiological Services' , 'Medical Service' ,
                         'Medical Care' , 'Medical Reception Station' , 'Diagnostics Centre' ,'Welfare Centre' , 'Medical Consultants' , 'Malaria Laboratory' , 'Dental Laboratory' ,
                         'Maternity. Centre' , 'Model Laboratory' , 'Dental Unit' ,'Medical. Laboratory,' ,'TBL' ,'Medical Consultancy' , 'Medical Diagnosis Laboratory' ,
                         'Basic Health Community' , ' Health Services', 'Specialist Home', 'Materntiy Home' ,'conv home' , 'Mat. Centre' , 'MDGs' , 'Renal Dialysis Centre' ,
                         'Convalescent and Maternity' , 'Maternity and Dspensary' , 'Millenium Develoment Goal' , 'Unknown Ward' , 'Surgical and Medical Home' ,'Scanning Centre,' ,
                         'Maternity House,' , 'Medical. Laboratory. Services' , 'Medical and Scan and Diagnostic Centre' , 'Health Foundation' , 'Family Support Programme Centre' ,
                         'maternity house' , 'Millennium Development Goal' , 'Medica Centre' , 'Aged and Widow Centre' , 'National Paediatric Centre' , 'Medical Foundation' , 'Home Care' ,
                         'Maternity and Home' , 'Medical Store' , 'Mutual Health Plan' , 'Cliinic' , ' Model Maternity' , 'Convalascent Home' , 'Care and Maternity' ,
                         'Specialist Centre' , 'Total Care' , 'Medicare' , 'Referral Centre' , 'University Health Service' , 'primary health centr' , 'comprehensive Centre' , 'Mediplex',
                         'CSO' , '\\s ealth Post' , 'Health Cent re' , 'Klinic Centre' , 'Medcal Centre' , 'chemist' , 'Primary Health Cemtre' , 'Maternal and Child Hopspital' ,
                         'Primary HealthCentre' , 'Family Support Progmme' , 'Eye Centre' , 'Urology Centre' , 'Comprehensive Health entre' , 'Health Clenter' , 'Eye Centre and Optic Service' ,
                         'Matarnity' , 'Support Group' , 'Infirmary', 'Laboratory' , 'Distric Health Unit' , 'NPI Unit' , #National Program on Immunization 
                         'Epidemiology Unit' , 'Federal Model Centre' , 'Model' , 'Specialist' , 'Ward Health System' , 'ward' , 'Convalescent' , 'Foundation' , 
                         'Primary Health Entre' , 'Consulting Room' , 'Technical Centre' , 'Medical Consult' , ' Medical Lab\\s' , 'Medical Art Centre' , 'Medical Limited' ,'Medical' ,
                         'Convalscent Centre' , 'Millennium Developement Goal Health' , 'Infant Welfare' ,  'Outreach Centre' , 'School of Nursing' , 'Trauma Centre' , 'Paediatrics Centre' ,
                         'Children Centre' ,'X-ray Centre' , 'LGA Secretariat NPI Office' ,'Local Government Action C Aids' ,'City Council' ,
                         'Government hospital' , 'Private Hospital' , 'Dental Maxillo-Facial Hospital' , 'Comprehensive Hospital' ,
                         'Maternity' , 'hospital'  )#Town , EN PARENTHESE
fac_patt <- paste(facilities_patterns , collapse = '|')

o <- grep(x = DHISFacilities$Level5Typoed , pattern = fac_patt , ignore.case = TRUE , value = TRUE , invert = TRUE)

grep(x =  DHISFacilities$Level5Typoed , 'National Program On Immunization' , value=TRUE , ignore.case = TRUE)
