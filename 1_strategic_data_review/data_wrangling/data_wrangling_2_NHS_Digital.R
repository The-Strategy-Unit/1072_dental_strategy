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

Clinical_Charges_Post18<- Clinical_Charges_Post18 %>%
  filter(LA_CODE!="Unallocated" |SUB_ICB_CODE!="Unallocated")%>%
  filter(LA_CODE!="UNA" |SUB_ICB_CODE!="UNA")

## Charges ----
Charges<-Clinical_Charges_Post18%>%
  filter(MEASURE=="Charge" & DENTAL_TREATMENT_BAND!="Free") 

## Clinical treatments ----
Treatment<-Clinical_Charges_Post18%>%
  filter(MEASURE!="Charge") %>%
  dplyr::select(-FINANCIAL_YR)



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
  mutate_all(na_if,"")


#ACTIVITY ----

Activity_Post18$ICB<- case_when(Activity_Post18$SUB_ICB_CODE %in% c("15E") ~"BIRMINGHAM AND SOLIHULL",
                                Activity_Post18$SUB_ICB_CODE %in% c("18C", "05J", "05T", "06D","05F") ~ "HEREFORDSHIRE AND WORCESTERSHIRE",
                                Activity_Post18$SUB_ICB_CODE %in% c("M2L0M", "05X", "05N")~ "SHROPSHIRE, TELFORD AND WREKIN", 
                                Activity_Post18$SUB_ICB_CODE %in% c("D2P2L", "05Y", "06A", "05C", "05L") ~"BLACK COUNTRY",
                                Activity_Post18$SUB_ICB_CODE %in% c("B2M3M","05A","05H","05R")~"COVENTRY AND WARWICKSHIRE",
                                Activity_Post18$SUB_ICB_CODE %in% c("04Y","05D","05G","05Q","05V","05W") ~"STAFFORDSHIRE AND STOKE-ON-TRENT" )

Activity_ICB<-Activity_Post18 %>%
  group_by(DATE, SUB_ICB_CODE, SUB_ICB_ONS_CODE, ICB, LA_CODE, LA_NAME, REGION_NAME, PATIENT_TYPE, DENTAL_TREATMENT_BAND) %>%
  summarise(COT=sum(COT),UDA=sum(UDA), .groups='drop')%>%
  filter(SUB_ICB_CODE!="Unallocated")

# Practice level - selected Midlands only due to file size
Activity_Practice_Level<-Activity_Post18%>%
  filter(REGION_NAME=="Midlands")%>%
  dplyr::select(-SUB_ICB_NAME, -REGION_CODE, -REGION_NAME, -REGION_ONS_CODE)


# ATTENDANCE ----

#Number of adults seen in the last 2 yrs, and children within the last 1 yr, taken at the end of the financial yr. 

Attendance_Post18$ICB<- case_when(Attendance_Post18$SUB_ICB_CODE %in% c("15E") ~"BIRMINGHAM AND SOLIHULL",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("18C", "05J", "05T", "06D","05F") ~ "HEREFORDSHIRE AND WORCESTERSHIRE",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("M2L0M", "05X", "05N")~ "SHROPSHIRE, TELFORD AND WREKIN", 
                                  Attendance_Post18$SUB_ICB_CODE %in% c("D2P2L", "05Y", "06A", "05C", "05L") ~"BLACK COUNTRY",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("B2M3M","05A","05H","05R")~"COVENTRY AND WARWICKSHIRE",
                                  Attendance_Post18$SUB_ICB_CODE %in% c("04Y","05D","05G","05Q","05V","05W") ~"STAFFORDSHIRE AND STOKE-ON-TRENT" )

Attendance_ICB<-Attendance_Post18 %>%
  filter(GEOG_TYPE=="LA" | GEOG_TYPE=="CCG"| GEOG_TYPE=="SUB_ICB") %>%
  group_by(DATE, GEOG_TYPE, SUB_ICB_CODE, SUB_ICB_ONS_CODE, ICB, LA_CODE, LA_NAME, PATIENT_TYPE, AGE_BAND)%>%
  summarise(SEEN=sum(PATIENTS_SEEN),POP=sum(POPULATION), .groups='drop')%>%
  mutate(PERCENT_SEEN=round((SEEN/POP)*100,2))%>%
  filter(LA_CODE!="Unallocated" | SUB_ICB_CODE!="Unallocated")

# Practice level - selected Midlands only due to file size
Attendance_Practice_Level<-Attendance_Post18%>%
  filter(!is.na(PRACTICE_CODE))%>%
  filter(REGION_NAME=="Midlands")%>%
  dplyr::select(-GEOG_TYPE, -SUB_ICB_NAME,-REGION_CODE, -REGION_NAME,  -REGION_ONS_CODE, -POPULATION)

# ADDING POPULATION DATA AT ICB LEVEL----

#Summarising the population data from the Attendance sheet to use for the other datasets
Population_data<-Attendance_ICB %>%
  group_by(DATE, SUB_ICB_CODE, LA_CODE, PATIENT_TYPE)%>%
  summarise(POP=sum(POP),.groups='drop')

Population_data_Total<- Population_data%>%
  mutate(DATE2=quarter(DATE, with_year = TRUE, fiscal_start = 4)) %>%
  separate(DATE2, c('FINANCIAL_YR', 'QUARTER'))%>%
  filter(QUARTER==4)%>%
  group_by(FINANCIAL_YR, SUB_ICB_CODE, LA_CODE)%>%
  summarise(POP=sum(POP),.groups='drop')


# Adding population data to charges 
Charges<- left_join(Charges, Population_data_Total, by = c("FINANCIAL_YR","SUB_ICB_CODE", "LA_CODE")) 

# Adding population data to Treatments
Treatment<- left_join(Treatment, Population_data, by = c("DATE", "SUB_ICB_CODE", "LA_CODE", "PATIENT_TYPE")) 

Treatment_ICB<-Treatment%>%
  filter(GEOG_TYPE=="CCG"| GEOG_TYPE=="SUB_ICB")

Treatment_LA<-Treatment%>%
  filter(GEOG_TYPE=="LA")


# Calculating activity per 1,000 
Activity_ICB<- Activity_ICB %>%
  mutate(GROUP=recode(PATIENT_TYPE, "Paying adult"= "Adult", "Non-paying adult"= "Adult"))

# Adding population to workforce data (NB some new Sub ICB codes are used for older workforce data so the population data doesn't match up, should be ok once grouped by ICB)
Workforce_Summary <- left_join(Workforce_Summary, Population_data_Total, by = c("Financial_yr"="FINANCIAL_YR", "Sub_ICB_Code"= "SUB_ICB_CODE")) %>%
 

write.csv(Population_data, '1_strategic_data_review/data/NHS_digital_Population_data.csv')

write.csv(Charges, '1_strategic_data_review/data/NHS_digital_Charges.csv')
write.csv(Treatment_ICB, '1_strategic_data_review/data/NHS_digital_Treatment_ICB.csv')
write.csv(Treatment_LA, '1_strategic_data_review/data/NHS_digital_Treatment_LA.csv')

write.csv(Workforce_Summary, '1_strategic_data_review/data/NHS_digital_Workforce_Summary.csv')     

write.csv(Activity_ICB, '1_strategic_data_review/data/NHS_digital_Activity_ICB.csv')  
write.csv(Activity_Practice_Level, '1_strategic_data_review/data/NHS_digital_Activity_Practice_Level.csv')     

write.csv(Attendance_ICB, '1_strategic_data_review/data/NHS_digital_Attendance_ICB.csv')
write.csv(Attendance_Practice_Level, '1_strategic_data_review/data/NHS_digital_Attendance_Practice_Level.csv')