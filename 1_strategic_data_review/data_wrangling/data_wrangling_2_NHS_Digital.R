library(dplyr)
library(tidyr)
library(lubridate)


# CLINICAL TREATMENTS AND CHARGES ----

Clinical_Charges_Post18$ICB<- case_when(Clinical_Charges_Post18$SUB_ICB_CODE %in% c("15E") ~"BIRMINGHAM AND SOLIHULL",
                                        Clinical_Charges_Post18$SUB_ICB_CODE %in% c("18C", "05J", "05T", "06D","05F") ~ "HEREFORDSHIRE AND WORCESTERSHIRE",
                                        Clinical_Charges_Post18$SUB_ICB_CODE %in% c("M2L0M", "05X", "05N")~ "SHROPSHIRE, TELFORD AND WREKIN", 
                                        Clinical_Charges_Post18$SUB_ICB_CODE %in% c("D2P2L", "05Y", "06A", "05C", "05L") ~"BLACK COUNTRY",
                                        Clinical_Charges_Post18$SUB_ICB_CODE %in% c("B2M3M","05A","05H","05R")~"COVENTRY AND WARWICKSHIRE",
                                        Clinical_Charges_Post18$SUB_ICB_CODE %in% c("04Y","05D","05G","05Q","05V","05W") ~"STAFFORDSHIRE AND STOKE-ON-TRENT" )


# Recoding years 
Clinical_Charges_Post18<-Clinical_Charges_Post18%>%
  mutate(FINANCIAL_YR=recode(END_DATE_QUARTER, '2018-19'='2019', '2019-20'='2020','2020-21'='2021', '2021-22'='2022'))%>%
  mutate(FINANCIAL_YR=format(as.Date(FINANCIAL_YR, format="%Y"),"%Y" ))

# Correcting date format for Sept 2020
Clinical_Charges_Post18$END_DATE_QUARTER[Clinical_Charges_Post18$END_DATE_QUARTER=='30-Sept-20']<-'30-Sep-20'


## Charges ----
Charges<-Clinical_Charges_Post18%>%
  filter(MEASURE=="Charge" & DENTAL_TREATMENT_BAND!="Free") %>%
  group_by(FINANCIAL_YR, ICB, SUB_ICB_CODE, SUB_ICB_ONS_CODE, DENTAL_TREATMENT_BAND)%>%
  summarize(VALUE=sum(VALUE), .groups='drop')


## Clinical treatments ----
Treatment<-Clinical_Charges_Post18%>%
  filter(MEASURE!="Charge") %>%
  mutate(END_DATE_QUARTER=as.Date(END_DATE_QUARTER, format="%d-%b-%y"))%>%
  rename(DATE = END_DATE_QUARTER)%>%
  group_by(DATE, ICB, SUB_ICB_CODE,  SUB_ICB_ONS_CODE, PATIENT_TYPE, DENTAL_TREATMENT_BAND, MEASURE)%>%
  summarize(VALUE=sum(VALUE), .groups='drop')

# WORKFORCE ----

# Summarising Workforce data for WM ICB's, Midlands and England

Workforce$Region[Workforce$Parent_Code_1=="ENG"]<-"England" 
Workforce$Region[Workforce$Parent_Code_1=="Y60"]<-"Midlands"

Workforce$ICB<- case_when(Workforce$Org_Code %in% c("15E") ~"BIRMINGHAM AND SOLIHULL",
                          Workforce$Org_Code %in% c("18C", "05J", "05T", "06D","05F") ~ "HEREFORDSHIRE AND WORCESTERSHIRE",
                          Workforce$Org_Code %in% c("M2L0M", "05X", "05N")~ "SHROPSHIRE, TELFORD AND WREKIN", 
                          Workforce$Org_Code %in% c("D2P2L", "05Y", "06A", "05C", "05L") ~"BLACK COUNTRY",
                          Workforce$Org_Code %in% c("B2M3M","05A","05H","05R")~"COVENTRY AND WARWICKSHIRE",
                          Workforce$Org_Code %in% c("04Y","05D","05G","05Q","05V","05W") ~"STAFFORDSHIRE AND STOKE-ON-TRENT" )

# Workforce Summary
Workforce_Summary<-Workforce %>%
  rename(Sub_ICB_Code = Org_Code)%>%
  dplyr::select(-Year, -Geography, -Parent_Code_1)


#ACTIVITY ----

Activity_Post18$ICB<- case_when(Activity_Post18$SUB_ICB_CODE %in% c("15E") ~"BIRMINGHAM AND SOLIHULL",
                                Activity_Post18$SUB_ICB_CODE %in% c("18C", "05J", "05T", "06D","05F") ~ "HEREFORDSHIRE AND WORCESTERSHIRE",
                                Activity_Post18$SUB_ICB_CODE %in% c("M2L0M", "05X", "05N")~ "SHROPSHIRE, TELFORD AND WREKIN", 
                                Activity_Post18$SUB_ICB_CODE %in% c("D2P2L", "05Y", "06A", "05C", "05L") ~"BLACK COUNTRY",
                                Activity_Post18$SUB_ICB_CODE %in% c("B2M3M","05A","05H","05R")~"COVENTRY AND WARWICKSHIRE",
                                Activity_Post18$SUB_ICB_CODE %in% c("04Y","05D","05G","05Q","05V","05W") ~"STAFFORDSHIRE AND STOKE-ON-TRENT" )


#Correcting date format  
Activity_Post18$ACTIVITY_END_DATE[Activity_Post18$ACTIVITY_END_DATE=='30-Sept-21']<-'30-Sep-21'
Activity_Post18$ACTIVITY_END_DATE[Activity_Post18$ACTIVITY_END_DATE=='30-June-20']<-'30-Jun-20'
Activity_Post18$ACTIVITY_END_DATE[Activity_Post18$ACTIVITY_END_DATE=='30-June-21']<-'30-Jun-21'

Activity_Post18<-Activity_Post18 %>%
  mutate(ACTIVITY_END_DATE=as.Date(ACTIVITY_END_DATE, format="%d-%b-%y"))%>%
  rename(DATE=ACTIVITY_END_DATE)

Activity_ICB<-Activity_Post18 %>%
  group_by(DATE, SUB_ICB_CODE, SUB_ICB_ONS_CODE, ICB, LA_CODE, PATIENT_TYPE, DENTAL_TREATMENT_BAND) %>%
  summarise(COT=sum(COT),UDA=sum(UDA), .groups='drop')

Activity_Practice_Level<-Activity_Post18%>%
  dplyr::select(-SUB_ICB_NAME, -REGION_CODE, -REGION_NAME, -REGION_ONS_CODE)


# ATTENDANCE ----

#Summarising attendance data for WM ICB's and Midlands.Number of adults seen in the last 2 yrs, and children within the last 1 yr, taken at the end of the financial yr. 

Attendance_Post18$ICB<- case_when(Attendance_Post18$SUB_ICB_CODE %in% c("15E") ~"BIRMINGHAM AND SOLIHULL",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("18C", "05J", "05T", "06D","05F") ~ "HEREFORDSHIRE AND WORCESTERSHIRE",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("M2L0M", "05X", "05N")~ "SHROPSHIRE, TELFORD AND WREKIN", 
                                  Attendance_Post18$SUB_ICB_CODE %in% c("D2P2L", "05Y", "06A", "05C", "05L") ~"BLACK COUNTRY",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("B2M3M","05A","05H","05R")~"COVENTRY AND WARWICKSHIRE",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("04Y","05D","05G","05Q","05V","05W") ~"STAFFORDSHIRE AND STOKE-ON-TRENT" )

Attendance_ICB<-Attendance_Post18 %>%
  filter(!is.na(ICB)) %>%
  group_by(DATE, SUB_ICB_CODE, SUB_ICB_ONS_CODE, ICB, PATIENT_TYPE, AGE_BAND)%>%
  summarise(SEEN=sum(PATIENTS_SEEN),POP=sum(POPULATION), .groups='drop')%>%
  mutate(PERCENT_SEEN=round((SEEN/POP)*100,2))

Attendance_Practice_Level<-Attendance_Post18%>%
  filter(!is.na(PRACTICE_CODE))%>%
  dplyr::select(-GEOG_TYPE, -SUB_ICB_NAME, -REGION_CODE, -REGION_NAME, -REGION_ONS_CODE, -POPULATION)

# ADDING POPULATION DATA AT ICB LEVEL----

#Summarising the population data from the Attendance sheet to use for the other datasets
Population_data<-Attendance_ICB%>%
  mutate(DATE2=quarter(DATE, with_year = TRUE, fiscal_start = 4)) %>%
  separate(DATE2, c('FINANCIAL_YR', 'QUARTER'))%>%
  group_by(DATE, ICB, QUARTER, FINANCIAL_YR, PATIENT_TYPE)%>%
  summarise(POP=sum(POP),.groups='drop')%>%
  filter(!is.na(ICB))

Population_data_Total<- Population_data%>%
  filter(QUARTER==4)%>%
  group_by(FINANCIAL_YR, ICB)%>%
  summarise(POP=sum(POP),.groups='drop')


# Calculating Charges per 1,000 
Charges<- left_join(Charges, Population_data_Total, by = c("FINANCIAL_YR","ICB")) %>%
  mutate(ChargePer1000=round((VALUE/POP)*1000,2))

# Calculating Treatments per 1,000
Treatment<- left_join(Treatment, Population_data, by = c("DATE", "PATIENT_TYPE", "ICB")) %>%
  select(-c('FINANCIAL_YR', 'QUARTER'))%>%
  mutate(TreatmentPer1000=round((VALUE/POP)*1000,2))

# Calculating activity per 1,000 
Activity_ICB<- Activity_ICB %>%
  mutate(GROUP=recode(PATIENT_TYPE, "Paying adult"= "Adult", "Non-paying adult"= "Adult"))

Activity_ICB <- left_join(Activity_ICB, Population_data, by = c("DATE", "ICB", "GROUP"="PATIENT_TYPE")) %>%
  select(-c('FINANCIAL_YR', 'QUARTER'))%>%
  mutate(COTPer1000=round((COT/POP)*1000,2)) %>%
  mutate(UDAPer1000=round((UDA/POP)*1000,2)) 

# Calculating workforce per 100,000
Workforce_Summary <- left_join(Workforce_Summary, Population_data_Total, by = c("Financial_yr"="FINANCIAL_YR", "ICB")) %>%
  mutate(WorkforcePer100000=round((Dentist_Count/POP)*100000,2)) 


write.csv(Charges, '1_strategic_data_review/data/NHS_digital_Charges.csv')
write.csv(Treatment, '1_strategic_data_review/data/NHS_digital_Treatment.csv')

write.csv(Workforce_Summary, '1_strategic_data_review/data/NHS_digital_Workforce_Summary.csv')     

write.csv(Activity_ICB, '1_strategic_data_review/data/NHS_digital_Activity_ICB.csv')  
write.csv(Activity_Practice_Level, '1_strategic_data_review/data/NHS_digital_Activity_Practice_Level.csv')     

write.csv(Attendance_ICB, '1_strategic_data_review/data/NHS_digital_Attendance_ICB.csv')
write.csv(Attendance_Practice_Level, '1_strategic_data_review/data/NHS_digital_Attendance_Practice_Level.csv')