
### PURPOSE:  The purpose of this project is to predict Sepsis Non-POA ###
### AUTHOR:   Jet Wang ###
### DATE:     June 1, 2017 ###

# Get sepsis ICD-10 Diagnosis codes
# CMS.file <- read.table("U:/Sepsis NonPOA/Files/Appendix_A.1_v5_2a.txt",as.is = TRUE, header = T, sep="\t", quote="")
# head(CMS.file)
# table(CMS.file$Table.Name)
# sepsis_I10 <- CMS.file$ICD.10.Code[CMS.file$Table.Name=="Severe Sepsis and Septic Shock (SEP)"]
# head(sepsis_I10)


# Use package dplyr to run descriptive statistics
#install.packages("dplyr")
#install.packages("data.table")
#install.packages('sqldf')
#install.packages('gmodels')

library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(sqldf)
library(gmodels)

setwd("C:/RData") #set work library



# Build RODBC Connection
library(RODBC)
PW <- read.csv('C:/PW/PW.txt',as.is=TRUE,header = T,sep="\t")
ch <- odbcConnect("CIS-Parkland",uid=PW[1,1],pwd=PW[1,2],believeNRows=F)

# Read in sepsis data
# sqlcode <- "select * from PIE_SYC_SEPSIS_ICD10;"
# sepsis <- sqlQuery(ch,sqlcode)


## Read in diag data
  sqlcode_diag <- "select * from PIE_SYC_SEPSIS_DIAG;"
  diagnosis <- sqlQuery(ch,sqlcode_diag)
  save(diagnosis,file = "diagnosis.RData")
  
## Read in antibiotics data
 sqlcode_antibio <- "select * from PIE_SYC_SEPSIS_ANTIBIO;"
 antibio <- sqlQuery(ch,sqlcode_antibio) 
 #table(antibio$MED_ROUTE) #INJECTION and INTRAVENOUS only
 antibio1 <- antibio%>%
   arrange(HSP_ACCOUNT_ID, ORDER_DTM)%>%
   distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)%>%
   select(HSP_ACCOUNT_ID, ORDER_DTM, MED_ROUTE) #select the first order date&time
 save(antibio,file = "antibio.RData")

## Read in Inpatient indicator
 sqlcode_inp <- "select * from PIE_SYC_NONPOA_SEPSIS_INPATIENT;"
 inp <- sqlQuery(ch,sqlcode_inp) 
 inp <- inp%>%
   filter(INPATIENT == 1)%>%
   distinct(HSP_ACCOUNT_ID, .keep_all = FALSE)#Select inpatients only
 save(inp,file = "inp.RData") 
  
 ## Read in Treatment Team
 sqlcode_TREAT <- "select * from PIE_SYC_NONPOA_SEPSIS_TREATMENT;"
 treatment <- sqlQuery(ch,sqlcode_TREAT)  
 save(treatment,file = "treatment.RData") 
 
## Read in Department ID Details
 sqlcode_DID <- "select * from PIE_SYC_NONPOA_SEPSIS_DID;"
 DID_details <- sqlQuery(ch,sqlcode_DID) 
 DID_details <- DID_details%>%
  arrange(HSP_ACCOUNT_ID, HOSP_ADMSN_TIME, HOSP_DISCH_TIME)
 save(DID_details,file = "DID_details.RData") 
 
## Get DID information
 DID <- DID_details%>%distinct(HSP_ACCOUNT_ID, DEPARTMENT_ID)
 
## Load all saved data
 load("diagnosis.RData")
 load("antibio.RData")
 load("inp.RData") 
 load("DID.RData") 
 load("treatment.RData") 
 
## Import the treatment team list provided by Shelley
 tteam_include_list <- read.csv(file='U:/Sepsis NonPOA/Files/from Shelley/TREATMENT_TEAM_INCLUDE.csv', header=TRUE)[,'TeamName']
 
## Import the department ID list provided by Shelley
 DID_exclude_list <- read.csv(file='U:/Sepsis NonPOA/Files/from Shelley/DEPARTMENT_ID_EXCLUDE.csv', header=TRUE)[,'DEPARTMENT_ID']
 





all_inpatients <- inner_join(diagnosis, inp, by = 'HSP_ACCOUNT_ID', copy = FALSE)

inpatient_number<-all_inpatients%>%distinct(HSP_ACCOUNT_ID, .keep_all=F)%>%
  filter('2014-01-01 00:00:00' <= DIS_DTM & DIS_DTM < '2015-01-01 00:00:00' )


## Select Intermal Medicine HSP_ACCOUNT_ID
all_IM_ID <- treatment%>%
  filter(toupper(TREATMENT_NAME) %in% tteam_include_list | toupper(TREATMENT_TITLE) %in% tteam_include_list | toupper(TREATMENT_ABBR) %in% tteam_include_list)%>%
  arrange(HSP_ACCOUNT_ID)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=TRUE)
all_IM_ID1 <- 
  left_join(all_IM_ID, DID, by='HSP_ACCOUNT_ID', copy=F)%>%
  filter(!DEPARTMENT_ID %in% DID_exclude_list )%>%
  arrange(HSP_ACCOUNT_ID)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all=FALSE)%>%
  mutate(IM=1)


#test1 <- treatment_new%>%
#  distinct(TREATMENT_NAME, TREATMENT_TITLE, TREATMENT_ABBR)

all_inpatients_new <- 
  left_join(all_inpatients, all_IM_ID1, by = 'HSP_ACCOUNT_ID', copy = FALSE)%>%
  arrange(HSP_ACCOUNT_ID, LINE)


# save(sepsis,file = "sepsis.RData")

# Load data from C Drive
#load("sepsis.RData")

# The complete ICD-10 code list to define sepsis cases, based on CMS definition
sepsis_ICD10_code <- 
  c('A02.1', 'A22.7', 'A26.7', 'A32.7', 'A40.0', 'A40.1', 'A40.3', 'A40.8', 'A40.9', 'A41.01', 'A41.02', 'A41.1', 'A41.2', 'A41.3', 'A41.4', 'A41.50', 'A41.51', 'A41.52', 
    'A41.53', 'A41.59', 'A41.81', 'A41.89', 'A41.9', 'A42.7', 'A54.86','B37.7','R65.20', 'R65.21')

SEPSIS <- 
  all_inpatients_new %>%
  mutate(is_sepsis = case_when(ICD10 %in%  sepsis_ICD10_code ~ 1, TRUE ~ 0),
          ICD_Period = case_when(DIS_DTM >= "2015-10-01 00:00:00" ~ 'I10', TRUE ~ 'I9'),
          POAX = case_when(POA == '1' ~ 1, TRUE ~ 0),
          TYPE = case_when(POAX == 0 & is_sepsis == 1  ~ 1,
                           POAX == 0 & is_sepsis == 0  ~ 2,
                           TRUE ~ 9),
         POA_STATUS = case_when(is_sepsis == 1 & POAX == 1   ~ 'Sepsis POA',
                              is_sepsis == 1 & POAX == 0   ~ 'Sepsis Non-POA',
                              TRUE ~ 'No Sepsis'),         
         AGE = as.period(interval(start = DOB, end = ADM_DTM))$year
         #LOS = as.period(new_interval(start = ADM_DTM, end = DIS_DTM))$day
)%>% 
filter(DIS_DTM < '2017-07-01 00:00:00' )#Set the cutoff date as 2017-06-30

#table(SEPSIS$AGE)
#table(SEPSIS$ICD_Period)  
  


# Check the distribution of sepsis POA status by ICD periods
check_POA <- SEPSIS %>%
  filter(POA_STATUS %in% c('Sepsis POA','Sepsis Non-POA'))%>%
  arrange(HSP_ACCOUNT_ID, desc(POA_STATUS))%>%
  select(HSP_ACCOUNT_ID, POA_STATUS, ICD_Period)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all = TRUE)
CrossTable(check_POA$POA_STATUS, check_POA$ICD_Period, prop.r=TRUE, prop.chisq=FALSE, chisq = TRUE, format = 'SAS')
  
# Check the distribution of sepsis POA status by ICD periods, INTERNAL MEDICINE ONLY
check_POA_IM <- SEPSIS %>%
  filter(POA_STATUS %in% c('Sepsis POA','Sepsis Non-POA') & IM==1 )%>%
  arrange(HSP_ACCOUNT_ID, desc(POA_STATUS))%>%
  select(HSP_ACCOUNT_ID, POA_STATUS, ICD_Period)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all = TRUE)
CrossTable(check_POA_IM$POA_STATUS, check_POA_IM$ICD_Period, prop.r=TRUE, prop.chisq=FALSE, chisq = TRUE, format = 'SAS')




# Find out what encounters have POA Sepsis and remove them from the modeling process
POA_sepsis <- distinct(filter(SEPSIS,POAX == 1 & is_sepsis == 1),HSP_ACCOUNT_ID, .keep_all = FALSE)


# Select only one record for each hospital account ID
SEPSIS_TRAINING <- SEPSIS %>%
  filter(!HSP_ACCOUNT_ID %in% POA_sepsis$HSP_ACCOUNT_ID)%>%
  mutate(Cases = case_when(TYPE==1 ~ 'YES', TYPE %in% c(2,9) ~ 'NO'))%>%
  arrange(HSP_ACCOUNT_ID, TYPE)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all = TRUE)
# Check the distribution of sepsis POA status by ICD periods
CrossTable(SEPSIS_TRAINING$Cases, SEPSIS_TRAINING$ICD_Period, prop.r=TRUE, prop.chisq=FALSE, chisq = TRUE, format = 'SAS')


# Select only one record for each hospital account ID, INTERNAL MEDICINE ONLY
SEPSIS_TRAINING_IM <- SEPSIS %>%
  filter(!HSP_ACCOUNT_ID %in% POA_sepsis$HSP_ACCOUNT_ID & IM==1 )%>%
  mutate(Cases = case_when(TYPE==1 ~ 'YES', TYPE %in% c(2,9) ~ 'NO'))%>%
  arrange(HSP_ACCOUNT_ID, TYPE)%>%
  distinct(HSP_ACCOUNT_ID, .keep_all = TRUE)
# Check the distribution of sepsis POA status by ICD periods
CrossTable(SEPSIS_TRAINING_IM$Cases, SEPSIS_TRAINING_IM$ICD_Period, prop.r=TRUE, prop.chisq=FALSE, chisq = TRUE, format = 'SAS')


# Select only Non-POA sepsis cases
NonPOA_sepsis <- SEPSIS_TRAINING%>%
  filter(Cases == 'YES')


# Merge Non-POA sepsis cases with antibiotics history
SEPSIS_ANTIBIO <- left_join(NonPOA_sepsis, antibio1, by = "HSP_ACCOUNT_ID", copy = FALSE, suffix = c(".x", ".y"))

### Calculate time from admission to antibiotics and LOS, INTERNAL MEDICINE ONLY
SEPSIS_ANTIBIO_IM <- SEPSIS_ANTIBIO%>%
  filter(IM==1)%>%
  mutate(adm_to_antib = as.numeric(difftime(ORDER_DTM, ADM_DTM, units = "days") ),
         LOS = as.numeric(difftime(DIS_DTM, ADM_DTM, units = "days") ) )%>%
  select(HSP_ACCOUNT_ID, ADM_DTM, ORDER_DTM, DIS_DTM, adm_to_antib, LOS, MED_ROUTE)

CrossTable(SEPSIS_ANTIBIO_IM$MED_ROUTE, prop.r=TRUE, prop.chisq=FALSE, chisq = FALSE, format = 'SAS', missing.include = F)

summary(SEPSIS_ANTIBIO_IM$adm_to_antib)
gm_mean(SEPSIS_ANTIBIO_IM$adm_to_antib)
ggplot(SEPSIS_ANTIBIO_IM, aes(adm_to_antib)) +  geom_histogram(fill='blue', binwidth = 30)
ggplot(subset(SEPSIS_ANTIBIO_IM, adm_to_antib<15), aes(adm_to_antib)) +  geom_histogram(fill='blue')

summary(sepsis1_IM$LOS)
gm_mean(sepsis1_IM$LOS)
ggplot(sepsis1_IM, aes(LOS)) +  geom_histogram(fill='blue')
ggplot(subset(sepsis1_IM, LOS<100), aes(LOS)) +  geom_histogram(fill='blue')


table(sepsis1_IM$MED_ROUTE)





hist(sepsis1$LOS)




# sepsis2 <- sepsis1 %>%
#   group_by(PAT_ID, HSP_ACCOUNT_ID)%>%
#   mutate(antibio_dtm = min(ORDER_INST), ADM_DTM = min(ADM_DTM), DIS_DTM = max(DIS_DTM),rank=row_number())%>%
#   arrange(PAT_ID,HSP_ACCOUNT_ID,PAT_ENC_CSN_ID)
#   #filter(rank==1)


#& SERVICE_LINE %in% c(123,124)



sepsis2 <- sepsis2 %>%
  group_by(HSP_ACCOUNT_ID)%>%
  mutate(rank=row_number(TYPE))%>%
  arrange(HSP_ACCOUNT_ID,rank)


sepsis_final <- sepsis2[which(sepsis2$rank==1 & (sepsis2$SERVICE_LINE == 123 | sepsis2$SERVICE_LINE == 134)),]

table(sepsis_final$is_sepsis)

hist(sepsis_final$LOS)



























