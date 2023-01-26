library(RCurl)
library(dplyr)
library(tidyr)
library(parsedate)

# Pulling and wrangling Dental Statistics data from NHS Digital

#Workforce

#Dental Workforce
download<- getURL("https://files.digital.nhs.uk/C6/B187FA/NHS%20Dental%20Statistics%20for%20England%202021-22%20Annex%203_Workforce%20csv.csv")
Workforce<-read.csv(text=download)

#Dentist Leavers and Joiners
download<- getURL("https://files.digital.nhs.uk/3A/364BAB/NHS%20Dental%20Statistics%20for%20England%202021-22%20Annex%203_Workforce%20Joiners%20Leavers%20csv.csv")
LeaversJoiners<-read.csv(text=download)

# Function to format workforce data
Format<-function(data){
  data%>%
    filter(Parent_Code_1=="ENG"|Parent_Code_1=="Y60")%>%
    filter(Year=="2015-16"|Year=="2016-17"|Year=="2017-18"|Year=="2018-19"|Year=="2019-20"|Year=="2020-21"|Year=="2021-22") 
}

Workforce<-Format(Workforce)
LeaversJoiners<-Format(LeaversJoiners)  

Workforce<-Workforce%>%
  mutate(Group="Workforce", .after = Org_Code)

names(LeaversJoiners)=names(Workforce)

Workforce<-rbind(Workforce, LeaversJoiners)%>%
  mutate(Financial_yr=recode(Year, '2015-16'='2016', '2016-17'='2017','2017-18'='2018', '2018-19'='2019', '2019-20'='2020','2020-21'='2021', '2021-22'='2022'))%>%
  mutate(Financial_yr=format(as.Date(Financial_yr, format="%Y"),"%Y" ))


#Dental Attendances
url<- "https://files.digital.nhs.uk/F5/E2CB64/nhs-dent-stat-eng-21-22-anx3-ps-prac.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Attendance22<-read.csv("nhs-dent-stat-eng-jan-jun-22-anx3-ps-prac.csv")
Attendance21b<-read.csv("nhs-dent-stat-eng-jul-dec-21-anx3-ps-prac.csv")

url<- "https://files.digital.nhs.uk/66/6B392F/nhs-dent-stat-eng-20-21-anx3-ps-prac.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Attendance21<-read.csv("nhs-dent-stat-eng-jan-jun-21-anx3-ps-prac.csv")
Attendance20b<-read.csv("nhs-dent-stat-eng-jul-dec-20-anx3-ps-prac.csv")


url<- "https://files.digital.nhs.uk/25/741A44/nhs-dent-stat-eng-19-20-anx3-ps-prac.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Attendance20<-read.csv("nhs-dent-stat-eng-jan-jun-20-anx3-ps-prac.csv")
Attendance19b<-read.csv("nhs-dent-stat-eng-jul-dec-19-anx3-ps-prac.csv")

url<- "https://files.digital.nhs.uk/5B/4707CE/nhs-dent-stat-eng-jul18-jun19-anx3-ps-prac.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Attendance19<-read.csv("nhs-dent-stat-eng-jan-jun-19-anx3-ps-prac.csv")
Attendance18b<-read.csv("nhs-dent-stat-eng-jul-dec-18-anx3-ps-prac.csv")

download<- getURL("https://files.digital.nhs.uk/13/90F010/nhs-dent-stat-eng-17-18-anx3-ps.csv")
Attendance18<-read.csv(text=download)
download<- getURL("https://files.digital.nhs.uk/18/C17EC5/nhs-dent-stat-eng-17-18-anx3-child-ps.csv")
Attendance18c<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publication/5/o/nhs-dent-stat-eng-16-17-anx4-ps.csv")
Attendance17<-read.csv(text=download)
download<- getURL("https://files.digital.nhs.uk/publication/5/p/nhs-dent-stat-eng-16-17-anx4-child-ps.csv")
Attendance17c<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21701/nhs-dent-stat-eng-15-16-anx4-ps-v2.csv")
Attendance16<-read.csv(text=download)


#Post 2018
Attendance19<-Attendance19%>%
  select(-REGION_LO_CODE, -REGION_LO_ONS_CODE,-REGION_LO_NAME)

Attendance18b<-Attendance18b%>%
  select(-REGION_LO_CODE, -REGION_LO_ONS_CODE,-REGION_LO_NAME)

names(Attendance21)<-names(Attendance22)
names(Attendance20)<-names(Attendance22)
names(Attendance20b)<-names(Attendance22)
names(Attendance19)<-names(Attendance22)
names(Attendance19b)<-names(Attendance22)
names(Attendance18b)<-names(Attendance22)


Attendance_Post18<-rbind(Attendance22, Attendance21b, Attendance21, Attendance20b, Attendance20, Attendance19b, Attendance19, Attendance18b) %>%
  filter(REGION_CODE=="Y60") %>%
  mutate(PSEEN_END_DATE=parse_date(PSEEN_END_DATE))

rm(Attendance22, Attendance21b, Attendance21, Attendance20b, Attendance20, Attendance19b, Attendance19, Attendance18b)


#Attendance pre 2018 (NB Childrens data changed in June 2016 to seen in the last 12 months rather than seen in the last 24 months)

names(Attendance18c)<-names(Attendance17c)  
Attendance_2017_2018_Childages<-rbind(Attendance18c, Attendance17c)

names(Attendance16)  <-names(Attendance17)
names(Attendance18)  <-names(Attendance17)
Attendance_Pre18<-rbind(Attendance18, Attendance17, Attendance16) %>%
  mutate(AGE=NA, .after = PARENT_CODE2)

names(Attendance_2017_2018_Childages)<-names(Attendance_Pre18)

Attendance_Pre18<-rbind(Attendance_Pre18,Attendance_2017_2018_Childages)


Attendance_Pre18$PATIENT_SEEN_END_DATE[Attendance_Pre18$PATIENT_SEEN_END_DATE=="30APR2017"]<-"30-Apr-17"
Attendance_Pre18$PATIENT_SEEN_END_DATE[Attendance_Pre18$PATIENT_SEEN_END_DATE=="30JUN2017"]<-"30-Jun-17"
Attendance_Pre18$PATIENT_SEEN_END_DATE[Attendance_Pre18$PATIENT_SEEN_END_DATE=="31MAY2017"]<-"31-May-17"

Attendance_Pre18<-Attendance_Pre18%>%
  mutate(COUNT=as.numeric(COUNT))%>%
  filter(PARENT_CODE1=="Q76"|PARENT_CODE1=="Q77"|PARENT_CODE1=="Q78")%>%
  spread(key=PATIENT_TYPE, value=COUNT)%>%
  dplyr::select(-TOTAL, -TOTAL_POPULATION) %>%
  rename(Adult=ADULT_POPULATION, Child=CHILD_POPULATION) %>%
  gather("AGE_GROUP", "POPULATION", Adult, Child) %>%
  rename(Adult=ADULT_24_months, Child=CHILD_12_months) %>%
  gather("AGE_GROUP2", "PATIENTS_SEEN", Adult, Child, CHILD_PSEEN) %>%
  filter(!is.na(PATIENTS_SEEN) & !is.na(POPULATION)) %>%
  mutate(AGE_GROUP2=ifelse(!is.na(AGE), "Child", AGE_GROUP2)) %>%
  filter(AGE_GROUP==AGE_GROUP2)%>%
  mutate(PATIENT_SEEN_END_DATE=as.Date(parse_date(PATIENT_SEEN_END_DATE)))%>%
  filter(!is.na(AGE) | (PATIENT_SEEN_END_DATE<'2017-01-01'& AGE_GROUP=="Child")|AGE_GROUP=="Adult")%>%
  dplyr::select(-AGE_GROUP2)


rm(Attendance18c, Attendance17c,Attendance18, Attendance17, Attendance16,Attendance_2017_2018_Childages )



#Dental Charges
download<- getURL("https://files.digital.nhs.uk/07/44A030/nhs-dent-stat-eng-21-22-anx3-clin-char.csv")
Charges22<-read.csv(text=download)

url<- "https://files.digital.nhs.uk/6C/E6E6B7/nhs-dent-stat-eng-20-21-anx3-clin-char.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Charges21<-read.csv("nhs-dent-stat-eng-20-21-anx3-clin-char.csv")

download<- getURL("https://files.digital.nhs.uk/C1/8EC9A4/nhs-dent-stat-eng-19-20-anx3-clin-char.csv")
Charges20<-read.csv(text=download)

url<- "https://files.digital.nhs.uk/32/B0F7BA/nhs-dent-stat-eng-18-19-anx3-clin-char.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Charges19<-read.csv("nhs-dent-stat-eng-18-19-anx3-clin-char.csv")

download<- getURL("https://files.digital.nhs.uk/6E/719F0E/nhs-dent-stat-eng-17-18-anx3-clin.csv")
Charges18a<-read.csv(text=download)
download<- getURL("https://files.digital.nhs.uk/C7/11B2F5/nhs-dent-stat-eng-17-18-anx3-pc.csv")
Charges18b<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publication/5/7/nhs-dent-stat-eng-16-17-anx4-clin.csv")
Charges17a<-read.csv(text=download)
download<- getURL("https://files.digital.nhs.uk/publication/5/3/nhs-dent-stat-eng-16-17-anx4-pc.csv")
Charges17b<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21701/nhs-dent-stat-eng-15-16-anx4-clin.csv")
Charges16a<-read.csv(text=download)
download<- getURL("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21701/nhs-dent-stat-eng-15-16-anx4-pc.csv")
Charges16b<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18129/nhs-dent-stat-eng-14-15-anx4-clin.csv")
Charges15a<-read.csv(text=download)
download<- getURL("https://files.digital.nhs.uk/publicationimport/pub18xxx/pub18129/nhs-dent-stat-eng-14-15-anx4-pc.csv")
Charges15b<-read.csv(text=download)

#Post 2018 Charges/Clinical Treatment data (Quarterly data with Charges and number of treatments included in one sheet)
Charges19<-Charges19%>%
  select(-REGION_LO_CODE, -REGION_LO_ONS_CODE,-REGION_LO_NAME)

names(Charges21) <- names(Charges22)
names(Charges20) <- names(Charges22)
names(Charges19) <- names(Charges22)

Clinical_Charges_Post18<-rbind(Charges22, Charges21, Charges20, Charges19)

Clinical_Charges_Post18<- Clinical_Charges_Post18%>%
  filter(REGION_CODE=="Y60")

rm(Charges22, Charges21, Charges20, Charges19)



#Pre 2018 Charges/Clinical Treatment data (Yearly and the charges and treatments are separate sheets that need merging as Post 18) 

#Clinical treatment
Charges15a <-Charges15a %>%
  gather(key = "CLINICAL_TREATMENT", value = "COUNT", -Year,-Quarter,-Geography,-Org_Code,-Org_Name,-Parent_Code1,-Parent_Code2,-PatientType, -TreatmentBand)%>%
  select(-Org_Name)
Charges15a <-     Charges15a[, c(1, 3, 2, 4, 5,6,7,8,9,10)]

names(Charges15a) <- names(Charges17a)
names(Charges18a) <- names(Charges17a)

ClinicalTreatment_Pre18<-rbind(Charges18a, Charges17a, Charges16a, Charges15a)

ClinicalTreatment_Pre18<- ClinicalTreatment_Pre18%>%
  filter(PARENT_CODE1=="Q76"|PARENT_CODE1=="Q77"|PARENT_CODE1=="Q78")%>%
  rename(VALUE=COUNT)%>%
  rename(MEASURE=CLINICAL_TREATMENT)

#Charges
Charges15b <-Charges15b %>%
  gather(key = "Treatment_Band", value = "Total_Charge", -Year,-Geography,-Org_Code,-Org_Name,-Parent_Code1,-Parent_Code2)%>%
  select(-Org_Name)

names(Charges18b) <- names(Charges17b)
names(Charges16b) <- names(Charges17b)
names(Charges15b) <- names(Charges17b)

Charges_Pre18<-rbind(Charges18b, Charges17b, Charges16b, Charges15b)

Charges_Pre18<-  Charges_Pre18%>%
  filter(PARENT_CODE1=="Q76"|PARENT_CODE1=="Q77"|PARENT_CODE1=="Q78")%>%
  mutate(QUARTER=NA, .after = YEAR)%>%
  mutate(PATIENT_TYPE=NA, .after = PARENT_CODE2)%>%
  mutate(MEASURE=TREATMENT_BAND, .after = TREATMENT_BAND)%>%
  rename(VALUE=TOTAL_CHARGE)%>%
  filter(TREATMENT_BAND!="Free_Charge")

Charges_Pre18$TREATMENT_BAND[Charges_Pre18$TREATMENT_BAND=="Band1_Charge"]<-"Band 1"
Charges_Pre18$TREATMENT_BAND[Charges_Pre18$TREATMENT_BAND=="Band2_Charge"]<-"Band 2"
Charges_Pre18$TREATMENT_BAND[Charges_Pre18$TREATMENT_BAND=="Band3_Charge"]<-"Band 3"
Charges_Pre18$TREATMENT_BAND[Charges_Pre18$TREATMENT_BAND=="Urgent_Charge"]<-"Urgent/Occasional"
Charges_Pre18$TREATMENT_BAND[Charges_Pre18$TREATMENT_BAND=="NonBanded_Charge"|Charges_Pre18$TREATMENT_BAND=="Total_Charge"]<-NA


Clinical_Charges_Pre18<-rbind(Charges_Pre18, ClinicalTreatment_Pre18)


rm(Charges18a, Charges17a, Charges16a, Charges15a,Charges18b, Charges17b, Charges16b, Charges15b, Charges_Pre18, ClinicalTreatment_Pre18)

#Dental Activity
download<- getURL("https://files.digital.nhs.uk/EF/9430DB/nhs-dent-stat-eng-21-22-anx3-act.csv")
Activity22<-read.csv(text=download)

url<- "https://files.digital.nhs.uk/AF/46A6E9/nhs-dent-stat-eng-20-21-anx3-act.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Activity21<-read.csv("nhs-dent-stat-eng-20-21-anx3-act.csv")

url<- "https://files.digital.nhs.uk/11/8D54E3/nhs-dent-stat-eng-19-20-anx3-act.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Activity20<-read.csv("nhs-dent-stat-eng-19-20-anx3-act.csv")

url<- "https://files.digital.nhs.uk/ED/A40690/nhs-dent-stat-eng-18-19-anx3-act.zip"
test<-download.file(url, "zippeddata.zip")
unzip("zippeddata.zip")
Activity19<-read.csv("nhs-dent-stat-eng-18-19-anx3-act.csv")

download<- getURL("https://files.digital.nhs.uk/90/2FCEF8/nhs-dent-stat-eng-17-18-anx3-act.csv")
Activity18<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publication/5/a/nhs-dent-stat-eng-16-17-anx4-act.csv")
Activity17<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/publicationimport/pub21xxx/pub21701/nhs-dent-stat-eng-15-16-anx4-act.csv")
Activity16<-read.csv(text=download)

download<- getURL("https://files.digital.nhs.uk/2A/C54F73/nhs-dent-stat-eng-14-15-anx4-act_v2.csv")
Activity15<-read.csv(text=download)

# Post 2018 activity data  
Activity19<-Activity19%>%
  select(-REGION_LO_CODE, -REGION_LO_ONS_CODE,-REGION_LO_NAME)

names(Activity21) <- names(Activity22)
names(Activity20) <- names(Activity22)
names(Activity19) <- names(Activity22)

Activity_Post18<-rbind(Activity22, Activity21, Activity20, Activity19)

Activity_Post18<-Activity_Post18 %>%
  filter(REGION_CODE=="Y60")

rm(Activity22, Activity21, Activity20, Activity19)

# Pre 2018 activity data
Activity15<-Activity15 %>%
  select(-Org_Name)
Activity15 <- Activity15[, c(1, 2, 4, 5,6,7,8, 3,9,10)]

Activity16<-Activity16 %>%
  spread(Activity_Type, Count)
Activity17<-Activity17 %>%
  spread(Activity_Type, Count)
Activity18<-Activity18 %>%
  filter(Activity_Type=="COT"|Activity_Type=="UDA")%>%
  spread(Activity_Type, Count)

names(Activity17) <- names(Activity15)
names(Activity18) <- names(Activity15)

Activity_Pre18<- rbind(Activity15, Activity16, Activity17, Activity18)

Activity_Pre18<-Activity_Pre18 %>%
  filter(Parent_Code1=="Q76"|Parent_Code1=="Q77"|Parent_Code1=="Q78")

rm(Activity15, Activity16, Activity17, Activity18)

#To export as csv files

#write.csv(Activity_Post18, 'data/NHSDigital_Activity_Post18.csv')
#write.csv(Activity_Pre18, 'data/NHSDigital_Activity_Pre18.csv')

#write.csv(Attendance_Post18, 'data/NHSDigital_Attendance_Post18.csv')
#write.csv(Attendance_Pre18, 'data/NHSDigital_Attendance_Pre18.csv')

#write.csv(Clinical_Charges_Post18, 'data/NHSDigital_Clinical_Charges_Post18.csv')
#write.csv(Clinical_Charges_Pre18, 'data/NHSDigital_Clinical_Charges_Pre18.csv')

#write.csv(Workforce, 'data/NHSDigital_Workforce.csv')
#write.csv(LeaversJoiners, 'data/NHSDigital_LeaversJoiners.csv')