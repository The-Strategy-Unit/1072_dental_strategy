# Data collection 

library(tidyverse)
library(janitor)
library(fingertipsR)

"%notin%" <- Negate("%in%")

# Resources:
## https://fingertips.phe.org.uk/documents/fingertips_api_guide.pdf
## https://rstudio-pubs-static.s3.amazonaws.com/274982_54e369ab8e5c4702a99acb15c37cae88.html


# OHID PHE Fingertips data ----

# Access dental data 

## Fingertips reference
fingertip_profiles <- 
  profiles()

fingertips_indicators <-
  indicators()

fingertips_areatypes <- area_types()

## Find and access dental indicators
dental_search_terms <-
  c("Oral", "oral", "Dental", "dental", "Dentistry", "dentistry","Teeh", "teeth","DMFT", "dmft")

fingertip_profiles %>% 
  filter(str_detect(DomainName, paste(dental_search_terms, collapse = "|")))

fingertips_indicators %>% 
  filter(str_detect(IndicatorName, paste(dental_search_terms, collapse = "|"))) 


#dental_indicator_ids <-
#  fingertips_indicators %>% 
#  filter(str_detect(IndicatorName, paste(dental_search_terms, collapse = "|"))) %>% 
#  select(IndicatorID) %>% 
#  distinct() %>% 
#  as.list()
#
#Indicators_dental <-
#  purrr::pmap_dfr(
#  .l = list(IndicatorID = dental_indicator_ids,
#            AreaTypeID = "All"),
#  .f = fingertipsR::fingertips_data)


# Access specific indicators 
## Dental caries (0-5 years) (UA's & PCN/ICB/GP)
indicator_93493 <- fingertips_data(IndicatorID = 93493, AreaTypeID = "All") %>% as_tibble()
indicator_93479 <- fingertips_data(IndicatorID = 93479, AreaTypeID = "All") %>% as_tibble()
## Domain: oral health
### 3 year olds
indicator_92499 <- fingertips_data(IndicatorID = 92499, AreaTypeID = "All") %>% as_tibble() 
indicator_92500 <- fingertips_data(IndicatorID = 92500, AreaTypeID = "All") %>% as_tibble() 
indicator_92501 <- fingertips_data(IndicatorID = 92501, AreaTypeID = "All") %>% as_tibble()
### 5 year olds
indicator_92504 <- fingertips_data(IndicatorID = 92504, AreaTypeID = "All") %>% as_tibble()
indicator_93563 <- fingertips_data(IndicatorID = 93563, AreaTypeID = "All") %>% as_tibble()
## Service access (18+)
indicator_92785 <- fingertips_data(IndicatorID = 92785, AreaTypeID = "All") %>% as_tibble()
## Oral cancer
indicator_92953 <- fingertips_data(IndicatorID = 92953, AreaTypeID = "All") %>% as_tibble()
# Didn't work - but got elsewhere
#indicator_1206  <- fingertips_data(IndicatorID = 1206,  AreaTypeID = "All") %>% as_tibble()
#indicator_90820 <- fingertips_data(IndicatorID = 90820, AreaTypeID = "All") %>% as_tibble()
# Didn't work - excluded
#indicator_2501  <- fingertips_data(IndicatorID = 2501,  AreaTypeID = "All") %>% as_tibble()
#indicator_2512  <- fingertips_data(IndicatorID = 2512,  AreaTypeID = "All") %>% as_tibble()
#indicator_92441 <- fingertips_data(IndicatorID = 92441, AreaTypeID = "All") %>% as_tibble()

# Where specific indicators are inaccessible - access domains and isolate indicators
domain_oral_health <-
  fingertips_data(DomainID = 1938133263, AreaTypeID = "All") %>% 
  as_tibble()

domain_Smoking_ill_health <-
  fingertips_data(DomainID = 1938132888, AreaTypeID = "All") %>% 
  as_tibble()

indicator_1206 <-
  domain_Smoking_ill_health %>% 
  filter(IndicatorID == 1206) 

domain_early_years_high_impact <-
  fingertips_data(DomainID = 1938133223, AreaTypeID = "All") %>% 
  as_tibble()

indicator_90820 <- 
  domain_early_years_high_impact %>% 
  filter(IndicatorID == 90820) 


#domain_outcomes_spot <-
#  fingertips_data(DomainID = 1938133284, AreaTypeID = 102) %>% 
#  as_tibble()
#
#indicator_2501 <-
#  domain_outcomes_spot %>% 
#  filter(IndicatorID == 2501)
#
#indicator_2512 <-
#  domain_outcomes_spot %>% 
#  filter(IndicatorID == 2512)
#
#
#domain_outcomes_pholio_2 <-
#  fingertips_data(DomainID = 1938133299, AreaTypeID = "All") %>% 
#  as_tibble()
#
#indicator_92441 <-
#  domain_outcomes_pholio_2 %>% 
#  filter(IndicatorID == 92441)


# Combine indicators and clean environment
indicators_dental <-
  indicator_93493 %>% 
  union_all(indicator_93479) %>% 
  union_all(indicator_92499) %>% 
  union_all(indicator_92500) %>% 
  union_all(indicator_92501) %>% 
  union_all(indicator_92504) %>% 
  union_all(indicator_93563) %>% 
  union_all(indicator_92785) %>% 
  union_all(indicator_92953) %>% 
  union_all(indicator_1206 )  %>% 
  union_all(indicator_90820)  %>% 
  clean_names()
  
rm(
  indicator_93493,
  indicator_93479,
  indicator_92499,
  indicator_92500,
  indicator_92501,
  indicator_92504,
  indicator_93563,
  indicator_92785,
  indicator_92953,
  indicator_1206,
  indicator_90820,
  
  domain_oral_health,
  domain_Smoking_ill_health,
  domain_early_years_high_impact
  )

write_csv(indicators_dental, "data/fingertips_indicators.csv")






