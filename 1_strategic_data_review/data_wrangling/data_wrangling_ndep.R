#### To load and compile the NDEP survey data...
# load packages
library(readxl) #reading xls files
library(janitor) #cleaning column names
library(plyr) #knitting csv's
library(dplyr) #creating and changing variables
library(tidyr)

# turn off silly engineering notation for large/tiny numbers
options(scipen = 999)

#### load and standardise 2013 3 years files ####
ndep_3_2013_ltla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_3Yr_2013_Results.xlsx"
                        , sheet = 'Lower Tier', skip = 5, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  filter(!row_number() %in% c(337:340)) %>% #remove rows with notes and eng duplicate
  clean_names() %>% #lowercase and underscore etc...
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  select(-c(19)) %>% #remove empty column
  mutate(period = 2013, child_age= 3, pop_year = 2012
         , area_type = case_when(
           lower_tier_la_code == "Eng" ~ "nation"
           , lower_tier_la_code %in% c("A","B","D","E","F","G","H","J","K") ~ "region"
           , TRUE ~ "ltla")
         , examined_pop_percent = examined/x3_year_old_population_mid_2012*100
         # whole bunch of blank fields included in future surveys
         , drawn_sample = NA, examined_sample_percent = NA, examined_consent_percent = NA
         , dmft_percent_0 = NA, 'dt_mean_dmft_>0' = NA, 'mt_mean_dmft_>0' = NA, 'mt_percent_dmft_>0' = NA, 'ft_mean_dmft_>0' = NA
         , caries_incisor_percent = NA, code_3t_percent = NA, care_index_percent = NA, extraction_index_percent = NA, pufa_percent = NA
         , dmft_percent_0_lower = NA, dmft_percent_0_upper = NA, 'dt_mean_dmft_>0_lower' = NA, 'dt_mean_dmft_>0_upper' = NA
         , 'mt_mean_dmft_>0_lower' = NA, 'mt_mean_dmft_>0_upper' = NA
         , 'mt_percent_dmft_>0_lower' = NA, 'mt_percent_dmft_>0_upper' = NA, 'ft_mean_dmft_>0_lower' = NA, 'ft_mean_dmft_>0_upper' = NA
         , 'caries_incisor_percent_lower' = NA, 'caries_incisor_percent_upper' = NA, 'code_3t_percent_lower' = NA
         , 'code_3t_percent_upper' = NA, 'care_index_percent_lower' = NA, 'care_index_percent_upper' = NA, 'extraction_index_percent_lower' = NA
         , 'extraction_index_percent_upper' = NA, 'pufa_percent_lower' = NA, 'pufa_percent_upper' = NA
  # Fields with inexplicable text values of 'N/A'!!?!??!
         , percent_with_substantial_plaque = na_if(percent_with_substantial_plaque, "N/A")
         , percent_with_sepsis = na_if(percent_with_sepsis, "N/A")) %>%
  # Change above fields to numerical
  mutate_at(c('percent_with_substantial_plaque','percent_with_sepsis'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(area_code = lower_tier_la_code, area_name = lower_tier_la_name, pop_mid = x3_year_old_population_mid_2012
         , dmft_mean = mean_d3mft_including_incisors, dt_mean = mean_d3t, mt_mean = mean_mt_including_incisors, ft_mean = mean_ft
         , 'dmft_percent_>0' = percent_d3mft_0_including_incisors, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0_including_incisors
         , dt_percent = percent_d3t_0, 'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_percent_>0' = percent_mt_0_including_incisors
         , 'mt_mean_mt_>0' = mean_mt_percent_mt_0_including_incisors
         , plaque_subst_percent = percent_with_substantial_plaque, sepsis_percent = percent_with_sepsis
         , caries_ecc_percent = percent_with_early_childhood_caries_ecc
         , dmft_mean_lower = lower_d3mft_including_incisors, dmft_mean_upper = upper_d3mft_including_incisors
         , dt_mean_lower = lower_d3t, dt_mean_upper = upper_d3t, mt_mean_lower = lower_mt_including_incisors, mt_mean_upper = upper_mt_including_incisors
         , ft_mean_lower = lower_ft, ft_mean_upper = upper_ft
         , 'dmft_percent_>0_lower' = lower_percent_d3mft_0_including_incisors, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_including_incisors
         , 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean_including_incisors, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean_including_incisors
         , 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean
         , dt_percent_lower = lower_percent_d3t_0, dt_percent_upper = upper_percent_d3t_0
         , 'mt_mean_mt_>0_lower' = lower_mt_0_mean_including_incisors, 'mt_mean_mt_>0_upper' = upper_mt_0_mean_including_incisors
         , 'mt_percent_>0_lower' = lower_percent_mt_0_including_incisors, 'mt_percent_>0_upper' = upper_percent_mt_0_including_incisors
         , plaque_subst_percent_lower = lower_percent_with_substantial_plaque, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         , sepsis_percent_lower = lower_percent_with_sepsis, sepsis_percent_upper = upper_percent_with_sepsis
         , caries_ecc_percent_lower = lower_percent_with_early_childhood_caries_ecc, caries_ecc_percent_upper = upper_percent_with_early_childhood_caries_ecc
  ) %>%
select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
       ,care_index_percent,care_index_percent_lower,care_index_percent_upper
       ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
       ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
       ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
       ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
       ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
       ,dt_percent,dt_percent_lower,dt_percent_upper
       ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
       ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
       ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
       ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
       ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
       ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
       ,pufa_percent,pufa_percent_lower,pufa_percent_upper
       ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2013 3 years UTLA file  
ndep_3_2013_utla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_3Yr_2013_Results.xlsx"
                          , sheet = 'Upper Tier', skip = 5, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  filter(!row_number() %in% c(1,154:166)) %>% #remove rows with notes and eng/region duplicates
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(period = 2013, child_age = 3, pop_year = 2012
         , examined_pop_percent = examined/x3_year_old_population_mid_2012*100
         , area_type = "utla"
         # whole bunch of blank fields included in future surveys
         , drawn_sample = NA, examined_sample_percent = NA, examined_consent_percent = NA
         , dmft_percent_0 = NA, 'dt_mean_dmft_>0' = NA, 'mt_mean_dmft_>0' = NA, 'mt_percent_dmft_>0' = NA, 'ft_mean_dmft_>0' = NA
         , caries_incisor_percent = NA, code_3t_percent = NA, care_index_percent = NA, extraction_index_percent = NA, pufa_percent = NA
         , dmft_percent_0_lower = NA, dmft_percent_0_upper = NA, 'dt_mean_dmft_>0_lower' = NA, 'dt_mean_dmft_>0_upper' = NA
         , 'mt_mean_dmft_>0_lower' = NA, 'mt_mean_dmft_>0_upper' = NA
         , 'mt_percent_dmft_>0_lower' = NA, 'mt_percent_dmft_>0_upper' = NA, 'ft_mean_dmft_>0_lower' = NA, 'ft_mean_dmft_>0_upper' = NA
         , 'caries_incisor_percent_lower' = NA, 'caries_incisor_percent_upper' = NA, 'code_3t_percent_lower' = NA
         , 'code_3t_percent_upper' = NA, 'care_index_percent_lower' = NA, 'care_index_percent_upper' = NA, 'extraction_index_percent_lower' = NA
         , 'extraction_index_percent_upper' = NA, 'pufa_percent_lower' = NA, 'pufa_percent_upper' = NA
         # Fields with inexplicable text values of 'N/A'!!?!??!
         , percent_with_substantial_plaque = na_if(percent_with_substantial_plaque, "N/A")
         , percent_with_sepsis = na_if(percent_with_sepsis, "N/A")) %>%
  # Change above fields to numerical
  mutate_at(c('percent_with_substantial_plaque','percent_with_sepsis'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(area_code = upper_tier_la_code, area_name = upper_tier_la_name, pop_mid = x3_year_old_population_mid_2012
         , dmft_mean = mean_d3mft_including_incisors, dt_mean = mean_d3t, mt_mean = mean_mt_including_incisors, ft_mean = mean_ft
         , 'dmft_percent_>0' = percent_d3mft_0_including_incisors, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0_including_incisors
         , dt_percent = percent_d3t_0, 'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_percent_>0' = percent_mt_0_including_incisors
         , 'mt_mean_mt_>0' = mean_mt_percent_mt_0_including_incisors
         , plaque_subst_percent = percent_with_substantial_plaque, sepsis_percent = percent_with_sepsis
         , caries_ecc_percent = percent_with_early_childhood_caries_ecc
         , dmft_mean_lower = lower_d3mft_including_incisors, dmft_mean_upper = upper_d3mft_including_incisors
         , dt_mean_lower = lower_d3t, dt_mean_upper = upper_d3t, mt_mean_lower = lower_mt_including_incisors, mt_mean_upper = upper_mt_including_incisors
         , ft_mean_lower = lower_ft, ft_mean_upper = upper_ft
         , 'dmft_percent_>0_lower' = lower_percent_d3mft_0_including_incisors, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_including_incisors
         , 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean_including_incisors, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean_including_incisors
         , 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean
         , dt_percent_lower = lower_percent_d3t_0, dt_percent_upper = upper_percent_d3t_0
         , 'mt_mean_mt_>0_lower' = lower_mt_0_mean_including_incisors, 'mt_mean_mt_>0_upper' = upper_mt_0_mean_including_incisors
         , 'mt_percent_>0_lower' = lower_percent_mt_0_including_incisors, 'mt_percent_>0_upper' = upper_percent_mt_0_including_incisors
         , plaque_subst_percent_lower = lower_percent_with_substantial_plaque, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         , sepsis_percent_lower = lower_percent_with_sepsis, sepsis_percent_upper = upper_percent_with_sepsis
         , caries_ecc_percent_lower = lower_percent_with_early_childhood_caries_ecc, caries_ecc_percent_upper = upper_percent_with_early_childhood_caries_ecc
  ) %>%
  filter(area_type == "utla") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

ndep_3_2013 <- rbind(ndep_3_2013_ltla, ndep_3_2013_utla)
rm(ndep_3_2013_ltla, ndep_3_2013_utla) # remove component df's

#### load and standardise 2020 3 years files ####
ndep_3_2020_ltla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_3Yr_2020_Results.xlsx"
                               , sheet = '3yr Lower Tier LAs', range = "A7:BP341", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(28)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(period = 2020, child_age = 3, pop_year = 2019
         , area_type = case_when(
           substr(lower_tier_la_code,1,3) == "E92" ~ "nation"
           , substr(lower_tier_la_code,1,3) == "E12" ~ "region"
           , TRUE ~ "ltla")
         ,drawn_sample=NA,examined_sample_percent=NA,dmft_percent_0=NA,sepsis_percent=NA,caries_ecc_percent=NA
         ,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA,sepsis_percent_lower=NA,sepsis_percent_upper=NA
         ,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA
         # Fields with inexplicable text values of 'N/A'!!?!??!
         , approximate_percent_of_consent_sought_examined2 = na_if(approximate_percent_of_consent_sought_examined2, "Unavailable")
         , percent_with_pufa = na_if(percent_with_pufa, "N/A")
         , lower_percent_with_pufa = na_if(lower_percent_with_pufa, "N/A")
         , upper_percent_with_pufa = na_if(upper_percent_with_pufa, "N/A")) %>%
  # Change above fields to numerical
  mutate_at(c('approximate_percent_of_consent_sought_examined2','percent_with_pufa','lower_percent_with_pufa','upper_percent_with_pufa'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(area_code = lower_tier_la_code, area_name = lower_tier_la_name
         ,pop_mid = x3_year_old_population_mid_2019, examined_pop_percent = percent_of_population_examined1,examined_consent_percent =	approximate_percent_of_consent_sought_examined2
         ,dmft_mean	= mean_d3mft_incl_incisors,dt_mean = mean_d3t,mt_mean = mean_mt_incl_incisors,ft_mean	= mean_ft
         ,'dmft_percent_>0' =	percent_d3mft_0_incl_incisors,'dmft_mean_dmft_percent_>0'	= mean_d3mft_incl_incisors_d3mft_0_incl_incisors
         ,dt_percent = percent_d3t_0,'dt_mean_dmft_>0' =	mean_d3t_d3mft_0_incl_incisors,'dt_mean_dt_>0' =	mean_d3t_d3t_0
         ,'mt_percent_>0'	= percent_mt_0_incl_incisors,'mt_mean_mt_>0' = mean_mt_incl_incisors_mt_0_incl_incisors
         ,'mt_mean_dmft_>0' =	mean_mt_incl_incisors_d3mft_0_incl_incisors,'mt_percent_dmft_>0' = percent_mt_0_incl_incisors_d3mft_0_incl_incisors
         ,'ft_mean_dmft_>0' =	mean_ft_d3mft_0_incl_incisors,plaque_subst_percent = percent_with_substantial_amounts_of_plaque
         ,caries_incisor_percent = percent_with_incisor_caries,code_3t_percent = percent_with_code_3_t
         ,care_index_percent = care_index_percent_incl_incisors_ft_d3mft_incl_i,extraction_index_percent = extraction_index_percent_incl_incisors_mt_incl_i_d3mft_incl_i
         ,pufa_percent = percent_with_pufa,dmft_mean_lower = lower_mean_d3mft_incl_incisors,dmft_mean_upper = upper_mean_d3mft_incl_incisors
         ,dt_mean_lower = lower_mean_d3t,dt_mean_upper = upper_mean_d3t,mt_mean_lower = lower_mean_mt_incl_incisors
         ,mt_mean_upper = upper_mean_mt_incl_incisors,ft_mean_lower = lower_mean_ft,ft_mean_upper = upper_mean_ft
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_incl_incisors,'dmft_percent_>0_upper'	= upper_percent_d3mft_0_incl_incisors
         ,'dmft_mean_dmft_percent_>0_lower'	= lower_mean_d3mft_incl_incisors_d3mft_0_incl_incisors,'dmft_mean_dmft_percent_>0_upper'	= upper_mean_d3mft_incl_incisors_d3mft_0_incl_incisors
         ,'dt_mean_dt_>0_lower'	= lower_mean_d3t_d3t_0,'dt_mean_dt_>0_upper'= upper_mean_d3t_d3t_0
         ,'dt_mean_dmft_>0_lower'	= lower_mean_d3t_d3mft_0_incl_incisors,'dt_mean_dmft_>0_upper' = upper_mean_d3t_d3mft_0_incl_incisors
         ,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,'mt_mean_mt_>0_lower' = lower_mean_mt_incl_incisors_mt_0_incl_incisors,'mt_mean_mt_>0_upper' = upper_mean_mt_incl_incisors_mt_0_incl_incisors
         ,'mt_mean_dmft_>0_lower' = lower_mean_mt_incl_incisors_d3mft_0_incl_incisors,'mt_mean_dmft_>0_upper' = upper_mean_mt_incl_incisors_d3mft_0_incl_incisors
         ,'mt_percent_>0_lower' = lower_percent_mt_0_incl_incisors,'mt_percent_>0_upper' = upper_percent_mt_0_incl_incisors
         ,'mt_percent_dmft_>0_lower' = lower_percent_mt_0_incl_incisors_d3mft_0_incl_incisors,'mt_percent_dmft_>0_upper' = upper_percent_mt_0_incl_incisors_d3mft_0_incl_incisors
         ,'ft_mean_dmft_>0_lower' = lower_mean_ft_d3mft_0_incl_incisors,'ft_mean_dmft_>0_upper' = upper_mean_ft_d3mft_0_incl_incisors
         ,plaque_subst_percent_lower = lower_percent_with_substantial_amounts_of_plaque,plaque_subst_percent_upper = upper_percent_with_substantial_amounts_of_plaque
         ,caries_incisor_percent_lower = lower_percent_with_incisor_caries,caries_incisor_percent_upper = upper_percent_with_incisor_caries
         ,code_3t_percent_lower = lower_percent_code_3t,code_3t_percent_upper = upper_percent_code_3t
         ,care_index_percent_lower = lower_care_index_percent_incl_incisors,care_index_percent_upper = upper_care_index_percent_incl_incisors
         ,extraction_index_percent_lower = lower_extraction_index_percent_incl_incisors,extraction_index_percent_upper = upper_extraction_index_percent_incl_incisors
         ,pufa_percent_lower = lower_percent_with_pufa,pufa_percent_upper = upper_percent_with_pufa
    ) %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2020 3 year old UTLA file
ndep_3_2020_utla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_3Yr_2020_Results.xlsx"
                               , sheet = '3yr Upper Tier LAs', range = "A7:BP168", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  filter(!row_number() %in% c(1)) %>% #remove row with eng duplicate
  select(-c(28)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(period = 2020, child_age = 3, pop_year = 2019
         , area_type = "utla"
         ,drawn_sample=NA,examined_sample_percent=NA,dmft_percent_0=NA,sepsis_percent=NA,caries_ecc_percent=NA
         ,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA,sepsis_percent_lower=NA,sepsis_percent_upper=NA
         ,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA
         # Fields with inexplicable text values of 'N/A'!!?!??!
         , approximate_percent_of_consent_sought_examined2 = na_if(approximate_percent_of_consent_sought_examined2, "Unavailable")
         , percent_with_pufa = na_if(percent_with_pufa, "N/A")
         , lower_percent_with_pufa = na_if(lower_percent_with_pufa, "N/A")
         , upper_percent_with_pufa = na_if(upper_percent_with_pufa, "N/A")) %>%
  # Change above fields to numerical
  mutate_at(c('approximate_percent_of_consent_sought_examined2','percent_with_pufa','lower_percent_with_pufa','upper_percent_with_pufa'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(area_code = upper_tier_la_code, area_name = upper_tier_la_name
         ,pop_mid = x3_year_old_population_mid_2019, examined_pop_percent = percent_of_population_examined1,examined_consent_percent =	approximate_percent_of_consent_sought_examined2
         ,dmft_mean	= mean_d3mft_incl_incisors,dt_mean = mean_d3t,mt_mean = mean_mt_incl_incisors,ft_mean	= mean_ft
         ,'dmft_percent_>0' =	percent_d3mft_0_incl_incisors,'dmft_mean_dmft_percent_>0'	= mean_d3mft_incl_incisors_d3mft_0_incl_incisors
         ,dt_percent = percent_d3t_0,'dt_mean_dmft_>0' =	mean_d3t_d3mft_0_incl_incisors,'dt_mean_dt_>0' =	mean_d3t_d3t_0
         ,'mt_percent_>0'	= percent_mt_0_incl_incisors,'mt_mean_mt_>0' = mean_mt_incl_incisors_mt_0_incl_incisors
         ,'mt_mean_dmft_>0' =	mean_mt_incl_incisors_d3mft_0_incl_incisors,'mt_percent_dmft_>0' = percent_mt_0_incl_incisors_d3mft_0_incl_incisors
         ,'ft_mean_dmft_>0' =	mean_ft_d3mft_0_incl_incisors,plaque_subst_percent = percent_with_substantial_amounts_of_plaque
         ,caries_incisor_percent = percent_with_incisor_caries,code_3t_percent = percent_with_code_3_t
         ,care_index_percent = care_index_percent_incl_incisors_ft_d3mft_incl_i,extraction_index_percent = extraction_index_percent_incl_incisors_mt_incl_i_d3mft_incl_i
         ,pufa_percent = percent_with_pufa,dmft_mean_lower = lower_mean_d3mft_incl_incisors,dmft_mean_upper = upper_mean_d3mft_incl_incisors
         ,dt_mean_lower = lower_mean_d3t,dt_mean_upper = upper_mean_d3t,mt_mean_lower = lower_mean_mt_incl_incisors
         ,mt_mean_upper = upper_mean_mt_incl_incisors,ft_mean_lower = lower_mean_ft,ft_mean_upper = upper_mean_ft
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_incl_incisors,'dmft_percent_>0_upper'	= upper_percent_d3mft_0_incl_incisors
         ,'dmft_mean_dmft_percent_>0_lower'	= lower_mean_d3mft_incl_incisors_d3mft_0_incl_incisors,'dmft_mean_dmft_percent_>0_upper'	= upper_mean_d3mft_incl_incisors_d3mft_0_incl_incisors
         ,'dt_mean_dt_>0_lower'	= lower_mean_d3t_d3t_0,'dt_mean_dt_>0_upper'= upper_mean_d3t_d3t_0
         ,'dt_mean_dmft_>0_lower'	= lower_mean_d3t_d3mft_0_incl_incisors,'dt_mean_dmft_>0_upper' = upper_mean_d3t_d3mft_0_incl_incisors
         ,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,'mt_mean_mt_>0_lower' = lower_mean_mt_incl_incisors_mt_0_incl_incisors,'mt_mean_mt_>0_upper' = upper_mean_mt_incl_incisors_mt_0_incl_incisors
         ,'mt_mean_dmft_>0_lower' = lower_mean_mt_incl_incisors_d3mft_0_incl_incisors,'mt_mean_dmft_>0_upper' = upper_mean_mt_incl_incisors_d3mft_0_incl_incisors
         ,'mt_percent_>0_lower' = lower_percent_mt_0_incl_incisors,'mt_percent_>0_upper' = upper_percent_mt_0_incl_incisors
         ,'mt_percent_dmft_>0_lower' = lower_percent_mt_0_incl_incisors_d3mft_0_incl_incisors,'mt_percent_dmft_>0_upper' = upper_percent_mt_0_incl_incisors_d3mft_0_incl_incisors
         ,'ft_mean_dmft_>0_lower' = lower_mean_ft_d3mft_0_incl_incisors,'ft_mean_dmft_>0_upper' = upper_mean_ft_d3mft_0_incl_incisors
         ,plaque_subst_percent_lower = lower_percent_with_substantial_amounts_of_plaque,plaque_subst_percent_upper = upper_percent_with_substantial_amounts_of_plaque
         ,caries_incisor_percent_lower = lower_percent_with_incisor_caries,caries_incisor_percent_upper = upper_percent_with_incisor_caries
         ,code_3t_percent_lower = lower_percent_code_3t,code_3t_percent_upper = upper_percent_code_3t
         ,care_index_percent_lower = lower_care_index_percent_incl_incisors,care_index_percent_upper = upper_care_index_percent_incl_incisors
         ,extraction_index_percent_lower = lower_extraction_index_percent_incl_incisors,extraction_index_percent_upper = upper_extraction_index_percent_incl_incisors
         ,pufa_percent_lower = lower_percent_with_pufa,pufa_percent_upper = upper_percent_with_pufa
  ) %>%
  filter(area_type == "utla") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2020 3 year old CCG file
ndep_3_2020_ccg <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_3Yr_2020_Results.xlsx"
                               , sheet = '3yr Clinical Commissioning Grps', range = "A7:BO331", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  filter(!row_number() %in% c(1)) %>% #remove row with eng duplicate
  select(-c(27)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(nhs_regional_local_office, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(period = 2020, child_age = 3, pop_year = 2019
         , area_type = "ccg"
         ,drawn_sample=NA,examined_sample_percent=NA,examined_consent_percent=NA
         ,dmft_percent_0=NA,sepsis_percent=NA,caries_ecc_percent=NA
         ,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA,sepsis_percent_lower=NA,sepsis_percent_upper=NA
         ,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA
         # Fields with inexplicable text values of 'N/A'!!?!??!
         , percent_with_pufa = na_if(percent_with_pufa, "N/A")
         , lower_percent_with_pufa = na_if(lower_percent_with_pufa, "N/A")
         , upper_percent_with_pufa = na_if(upper_percent_with_pufa, "N/A")) %>%
  # Change above fields to numerical
  mutate_at(c('percent_with_pufa','lower_percent_with_pufa','upper_percent_with_pufa'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(area_code = clinical_commissioning_group_code, area_name = clinical_commissioning_group_name, region = nhs_regional_local_office
         ,pop_mid = x3_year_old_population_mid_2019, examined_pop_percent = percent_of_population_examined
         ,dmft_mean	= mean_d3mft_incl_incisors,dt_mean = mean_d3t,mt_mean = mean_mt_incl_incisors,ft_mean	= mean_ft
         ,'dmft_percent_>0' =	percent_d3mft_0_incl_incisors,'dmft_mean_dmft_percent_>0'	= mean_d3mft_incl_incisors_d3mft_0_incl_incisors
         ,dt_percent = percent_d3t_0,'dt_mean_dmft_>0' =	mean_d3t_d3mft_0_incl_incisors,'dt_mean_dt_>0' =	mean_d3t_d3t_0
         ,'mt_percent_>0'	= percent_mt_0_incl_incisors,'mt_mean_mt_>0' = mean_mt_incl_incisors_mt_0_incl_incisors
         ,'mt_mean_dmft_>0' =	mean_mt_incl_incisors_d3mft_0_incl_incisors,'mt_percent_dmft_>0' = percent_mt_0_incl_incisors_d3mft_0_incl_incisors
         ,'ft_mean_dmft_>0' =	mean_ft_d3mft_0_incl_incisors,plaque_subst_percent = percent_with_substantial_amounts_of_plaque
         ,caries_incisor_percent = percent_with_incisor_caries,code_3t_percent = percent_with_code_3_t
         ,care_index_percent = care_index_percent_incl_incisors_ft_d3mft_incl_i,extraction_index_percent = extraction_index_percent_incl_incisors_mt_incl_i_d3mft_incl_i
         ,pufa_percent = percent_with_pufa,dmft_mean_lower = lower_mean_d3mft_incl_incisors,dmft_mean_upper = upper_mean_d3mft_incl_incisors
         ,dt_mean_lower = lower_mean_d3t,dt_mean_upper = upper_mean_d3t,mt_mean_lower = lower_mean_mt_incl_incisors
         ,mt_mean_upper = upper_mean_mt_incl_incisors,ft_mean_lower = lower_mean_ft,ft_mean_upper = upper_mean_ft
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_incl_incisors,'dmft_percent_>0_upper'	= upper_percent_d3mft_0_incl_incisors
         ,'dmft_mean_dmft_percent_>0_lower'	= lower_mean_d3mft_incl_incisors_d3mft_0_incl_incisors,'dmft_mean_dmft_percent_>0_upper'	= upper_mean_d3mft_incl_incisors_d3mft_0_incl_incisors
         ,'dt_mean_dt_>0_lower'	= lower_mean_d3t_d3t_0,'dt_mean_dt_>0_upper'= upper_mean_d3t_d3t_0
         ,'dt_mean_dmft_>0_lower'	= lower_mean_d3t_d3mft_0_incl_incisors,'dt_mean_dmft_>0_upper' = upper_mean_d3t_d3mft_0_incl_incisors
         ,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,'mt_mean_mt_>0_lower' = lower_mean_mt_incl_incisors_mt_0_incl_incisors,'mt_mean_mt_>0_upper' = upper_mean_mt_incl_incisors_mt_0_incl_incisors
         ,'mt_mean_dmft_>0_lower' = lower_mean_mt_incl_incisors_d3mft_0_incl_incisors,'mt_mean_dmft_>0_upper' = upper_mean_mt_incl_incisors_d3mft_0_incl_incisors
         ,'mt_percent_>0_lower' = lower_percent_mt_0_incl_incisors,'mt_percent_>0_upper' = upper_percent_mt_0_incl_incisors
         ,'mt_percent_dmft_>0_lower' = lower_percent_mt_0_incl_incisors_d3mft_0_incl_incisors,'mt_percent_dmft_>0_upper' = upper_percent_mt_0_incl_incisors_d3mft_0_incl_incisors
         ,'ft_mean_dmft_>0_lower' = lower_mean_ft_d3mft_0_incl_incisors,'ft_mean_dmft_>0_upper' = upper_mean_ft_d3mft_0_incl_incisors
         ,plaque_subst_percent_lower = lower_percent_with_substantial_amounts_of_plaque,plaque_subst_percent_upper = upper_percent_with_substantial_amounts_of_plaque
         ,caries_incisor_percent_lower = lower_percent_with_incisor_caries,caries_incisor_percent_upper = upper_percent_with_incisor_caries
         ,code_3t_percent_lower = lower_percent_code_3t,code_3t_percent_upper = upper_percent_code_3t
         ,care_index_percent_lower = lower_care_index_percent_incl_incisors,care_index_percent_upper = upper_care_index_percent_incl_incisors
         ,extraction_index_percent_lower = lower_extraction_index_percent_incl_incisors,extraction_index_percent_upper = upper_extraction_index_percent_incl_incisors
         ,pufa_percent_lower = lower_percent_with_pufa,pufa_percent_upper = upper_percent_with_pufa
  ) %>%
  filter(area_type == "ccg") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

ndep_3_2020 <- rbind(ndep_3_2020_ltla, ndep_3_2020_utla, ndep_3_2020_ccg)
rm(ndep_3_2020_ltla, ndep_3_2020_utla, ndep_3_2020_ccg) # remove component df's

#### load and standardise 2015 5 years files ####
ndep_5_2015_ltla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2015_Results.xlsx"
                               , sheet = 'Lower Tier LAs', range = "A6:AZ350", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(22)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
        period = 2015, child_age = 5, pop_year = 2014
         , area_type = case_when(
           substr(lower_tier_la_code,1,3) == "E92" ~ "nation"
           , substr(lower_tier_la_code,1,3) == "E12" ~ "region"
           , TRUE ~ "ltla")
         ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,'dt_mean_dmft_>0'=NA,'mt_mean_dmft_>0'=NA
        , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA,code_3t_percent=NA,extraction_index_percent=NA
        ,pufa_percent=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA,'mt_mean_dmft_>0_lower'=NA
        ,'mt_mean_dmft_>0_upper'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
        ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,code_3t_percent_lower=NA
        ,code_3t_percent_upper=NA,extraction_index_percent_lower=NA,extraction_index_percent_upper=NA
        ,pufa_percent_lower=NA,pufa_percent_upper=NA
         # Fields with inexplicable text values of 'N/A' or * !!?!??!
         , percent_of_sample_examined_unavailable = na_if(percent_of_sample_examined_unavailable, "*")) %>%
  # Change above fields to numerical
  mutate_at(c('percent_of_sample_examined_unavailable'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2014',care_index_percent_lower = lower_care_index_percent
         ,dmft_mean_lower = lower_d3mft, 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean
         ,dt_mean_lower = lower_d3t, 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, ft_mean_lower = lower_ft
         ,mt_mean_lower = lower_mt, 'mt_mean_mt_>0_lower' = lower_mt_0_mean, dmft_percent_0_lower = lower_percent_d3mft_0
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_2, dt_percent_lower = lower_percent_d3t_0, 'mt_percent_>0_lower' = lower_percent_mt_0
         ,sepsis_percent_lower = lower_percent_with_sepsis, caries_incisor_percent_lower = lower_percent_with_incisor_caries
         ,plaque_subst_percent_lower = lower_percent_with_substantial_plaque, area_code = lower_tier_la_code, area_name = lower_tier_la_name
         ,dmft_mean = mean_d3mft, dt_mean = mean_d3t, ft_mean = mean_ft, mt_mean = mean_mt, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_mean_mt_>0' = mean_mt_percent_mt_0, dmft_percent_0 = percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0_2, dt_percent = percent_d3t_0, 'mt_percent_>0' = percent_mt_0
         ,examined_sample_percent = percent_of_sample_examined_unavailable, caries_incisor_percent = percent_with_incisor_caries
         ,sepsis_percent = percent_with_sepsis, plaque_subst_percent = percent_with_substantial_plaque
         ,care_index_percent_upper = upper_care_index_percent, dmft_mean_upper = upper_d3mft, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean
         ,dt_mean_upper = upper_d3t, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean, ft_mean_upper = upper_ft, mt_mean_upper = upper_mt
         ,'mt_mean_mt_>0_upper' = upper_mt_0_mean, dmft_percent_0_upper = upper_percent_d3mft_0, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_2
         ,dt_percent_upper = upper_percent_d3t_0, 'mt_percent_>0_upper' = upper_percent_mt_0, sepsis_percent_upper = upper_percent_with_sepsis
         ,caries_incisor_percent_upper = upper_percent_with_incisor_caries, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
  ) %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2017 5 year old UTLA file
ndep_5_2015_utla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2015_Results.xlsx"
                               , sheet = 'Upper Tier LAs', range = "A6:AZ166", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(22)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2015, child_age = 5, pop_year = 2014
    , area_type = case_when(
      substr(upper_tier_la_code,1,3) == "E92" ~ "nation"
      , substr(upper_tier_la_code,1,3) == "E12" ~ "region"
      , TRUE ~ "utla")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,'dt_mean_dmft_>0'=NA,'mt_mean_dmft_>0'=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA,code_3t_percent=NA,extraction_index_percent=NA
    ,pufa_percent=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA,'mt_mean_dmft_>0_lower'=NA
    ,'mt_mean_dmft_>0_upper'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,code_3t_percent_lower=NA
    ,code_3t_percent_upper=NA,extraction_index_percent_lower=NA,extraction_index_percent_upper=NA
    ,pufa_percent_lower=NA,pufa_percent_upper=NA) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2014',care_index_percent_lower = lower_care_index_percent
         ,dmft_mean_lower = lower_d3mft, 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean
         ,dt_mean_lower = lower_d3t, 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, ft_mean_lower = lower_ft
         ,mt_mean_lower = lower_mt, 'mt_mean_mt_>0_lower' = lower_mt_0_mean, dmft_percent_0_lower = lower_percent_d3mft_0
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_2, dt_percent_lower = lower_percent_d3t_0, 'mt_percent_>0_lower' = lower_percent_mt_0
         ,sepsis_percent_lower = lower_percent_with_sepsis, caries_incisor_percent_lower = lower_percent_with_incisor_caries
         ,plaque_subst_percent_lower = lower_percent_with_substantial_plaque, area_code = upper_tier_la_code, area_name = upper_tier_la_name
         ,dmft_mean = mean_d3mft, dt_mean = mean_d3t, ft_mean = mean_ft, mt_mean = mean_mt, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_mean_mt_>0' = mean_mt_percent_mt_0, dmft_percent_0 = percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0_2, dt_percent = percent_d3t_0, 'mt_percent_>0' = percent_mt_0
         ,examined_sample_percent = percent_of_sample_examined, caries_incisor_percent = percent_with_incisor_caries
         ,sepsis_percent = percent_with_sepsis, plaque_subst_percent = percent_with_substantial_plaque
         ,care_index_percent_upper = upper_care_index_percent, dmft_mean_upper = upper_d3mft, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean
         ,dt_mean_upper = upper_d3t, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean, ft_mean_upper = upper_ft, mt_mean_upper = upper_mt
         ,'mt_mean_mt_>0_upper' = upper_mt_0_mean, dmft_percent_0_upper = upper_percent_d3mft_0, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_2
         ,dt_percent_upper = upper_percent_d3t_0, 'mt_percent_>0_upper' = upper_percent_mt_0, sepsis_percent_upper = upper_percent_with_sepsis
         ,caries_incisor_percent_upper = upper_percent_with_incisor_caries, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
  ) %>%
  filter(area_type == "utla") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2020 3 year old CCG file
ndep_5_2015_ccg <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2015_Results.xlsx"
                               , sheet = 'CCGs', range = "A6:AY225", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(21)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2015, child_age = 5, pop_year = 2014, examined_sample_percent=NA
    , area_type = case_when(
      substr(ccg_code,1,3) == "E92" ~ "nation"
      , substr(ccg_code,1,3) == "E12" ~ "region"
      , TRUE ~ "ccg")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,'dt_mean_dmft_>0'=NA,'mt_mean_dmft_>0'=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA,code_3t_percent=NA,extraction_index_percent=NA
    ,pufa_percent=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA,'mt_mean_dmft_>0_lower'=NA
    ,'mt_mean_dmft_>0_upper'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,code_3t_percent_lower=NA
    ,code_3t_percent_upper=NA,extraction_index_percent_lower=NA,extraction_index_percent_upper=NA
    ,pufa_percent_lower=NA,pufa_percent_upper=NA) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2014',care_index_percent_lower = lower_care_index_percent
         ,dmft_mean_lower = lower_d3mft, 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean
         ,dt_mean_lower = lower_d3t, 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, ft_mean_lower = lower_ft
         ,mt_mean_lower = lower_mt, 'mt_mean_mt_>0_lower' = lower_mt_0_mean, dmft_percent_0_lower = lower_percent_d3mft_0
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_2, dt_percent_lower = lower_percent_d3t_0, 'mt_percent_>0_lower' = lower_percent_mt_0
         ,sepsis_percent_lower = lower_percent_with_sepsis, caries_incisor_percent_lower = lower_percent_with_incisor_caries
         ,plaque_subst_percent_lower = lower_percent_with_substantial_plaque, area_code = ccg_code, area_name = ccg_name
         ,dmft_mean = mean_d3mft, dt_mean = mean_d3t, ft_mean = mean_ft, mt_mean = mean_mt, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_mean_mt_>0' = mean_mt_percent_mt_0, dmft_percent_0 = percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0_2, dt_percent = percent_d3t_0, 'mt_percent_>0' = percent_mt_0, caries_incisor_percent = percent_with_incisor_caries
         ,sepsis_percent = percent_with_sepsis, plaque_subst_percent = percent_with_substantial_plaque
         ,care_index_percent_upper = upper_care_index_percent, dmft_mean_upper = upper_d3mft, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean
         ,dt_mean_upper = upper_d3t, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean, ft_mean_upper = upper_ft, mt_mean_upper = upper_mt
         ,'mt_mean_mt_>0_upper' = upper_mt_0_mean, dmft_percent_0_upper = upper_percent_d3mft_0, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_2
         ,dt_percent_upper = upper_percent_d3t_0, 'mt_percent_>0_upper' = upper_percent_mt_0, sepsis_percent_upper = upper_percent_with_sepsis
         ,caries_incisor_percent_upper = upper_percent_with_incisor_caries, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
  ) %>%
  filter(area_type == "ccg") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

ndep_5_2015 <- rbind(ndep_5_2015_ltla, ndep_5_2015_utla, ndep_5_2015_ccg)
rm(ndep_5_2015_ltla, ndep_5_2015_utla, ndep_5_2015_ccg) # remove component df's

#### load and standardise 2017 5 years files ####
ndep_5_2017_ltla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2017_Results.xlsx"
                               , sheet = 'Lower Tier LAs', range = "A6:BC350", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(23)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2017, child_age = 5, pop_year = 2016
    , area_type = case_when(
      substr(lower_tier_la_code,1,3) == "E92" ~ "nation"
      , substr(lower_tier_la_code,1,3) == "E12" ~ "region"
      , TRUE ~ "ltla")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,'dt_mean_dmft_>0'=NA,'mt_mean_dmft_>0'=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA,extraction_index_percent=NA
    ,pufa_percent=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA,'mt_mean_dmft_>0_lower'=NA
    ,'mt_mean_dmft_>0_upper'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,extraction_index_percent_lower=NA
    ,extraction_index_percent_upper=NA,pufa_percent_lower=NA,pufa_percent_upper=NA
    # Fields with inexplicable text values of 'N/A' or * !!?!??!
    , percent_of_sample_examined_unavailable = na_if(percent_of_sample_examined_unavailable, "*")) %>%
  # Change above fields to numerical
  mutate_at(c('percent_of_sample_examined_unavailable'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2016',care_index_percent_lower = lower_care_index_percent
         ,dmft_mean_lower = lower_d3mft, 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean
         ,dt_mean_lower = lower_d3t, 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, ft_mean_lower = lower_ft
         ,mt_mean_lower = lower_mt, 'mt_mean_mt_>0_lower' = lower_mt_0_mean, dmft_percent_0_lower = lower_percent_d3mft_0
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_2, dt_percent_lower = lower_percent_d3t_0, 'mt_percent_>0_lower' = lower_percent_mt_0
         ,sepsis_percent_lower = lower_percent_with_sepsis, caries_incisor_percent_lower = lower_percent_with_incisor_caries
         ,plaque_subst_percent_lower = lower_percent_with_substantial_plaque, area_code = lower_tier_la_code, area_name = lower_tier_la_name
         ,dmft_mean = mean_d3mft, dt_mean = mean_d3t, ft_mean = mean_ft, mt_mean = mean_mt, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_mean_mt_>0' = mean_mt_percent_mt_0, dmft_percent_0 = percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0_2, dt_percent = percent_d3t_0, 'mt_percent_>0' = percent_mt_0
         ,examined_sample_percent = percent_of_sample_examined_unavailable, caries_incisor_percent = percent_with_incisor_caries
         ,sepsis_percent = percent_with_sepsis, plaque_subst_percent = percent_with_substantial_plaque
         ,care_index_percent_upper = upper_care_index_percent, dmft_mean_upper = upper_d3mft, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean
         ,dt_mean_upper = upper_d3t, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean, ft_mean_upper = upper_ft, mt_mean_upper = upper_mt
         ,'mt_mean_mt_>0_upper' = upper_mt_0_mean, dmft_percent_0_upper = upper_percent_d3mft_0, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_2
         ,dt_percent_upper = upper_percent_d3t_0, 'mt_percent_>0_upper' = upper_percent_mt_0, sepsis_percent_upper = upper_percent_with_sepsis
         ,caries_incisor_percent_upper = upper_percent_with_incisor_caries, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         ,code_3t_percent = percent_with_code_3_t,code_3t_percent_lower = lower_percent_with_code_3_t,code_3t_percent_upper = upper_percent_with_code_3_t
  ) %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2017 5 year old UTLA file
ndep_5_2017_utla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2017_Results.xlsx"
                               , sheet = 'Upper Tier LAs', range = "A6:BC166", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(23)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2017, child_age = 5, pop_year = 2016
    , area_type = case_when(
      substr(upper_tier_la_code,1,3) == "E92" ~ "nation"
      , substr(upper_tier_la_code,1,3) == "E12" ~ "region"
      , TRUE ~ "utla")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,'dt_mean_dmft_>0'=NA,'mt_mean_dmft_>0'=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA,extraction_index_percent=NA
    ,pufa_percent=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA,'mt_mean_dmft_>0_lower'=NA
    ,'mt_mean_dmft_>0_upper'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,extraction_index_percent_lower=NA
    ,extraction_index_percent_upper=NA,pufa_percent_lower=NA,pufa_percent_upper=NA
    # Fields with inexplicable text values of 'N/A' or * !!?!??!
    , percent_of_sample_examined_unavailable = na_if(percent_of_sample_examined_unavailable, "*")) %>%
  # Change above fields to numerical
  mutate_at(c('percent_of_sample_examined_unavailable'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2016',care_index_percent_lower = lower_care_index_percent
         ,dmft_mean_lower = lower_d3mft, 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean
         ,dt_mean_lower = lower_d3t, 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, ft_mean_lower = lower_ft
         ,mt_mean_lower = lower_mt, 'mt_mean_mt_>0_lower' = lower_mt_0_mean, dmft_percent_0_lower = lower_percent_d3mft_0
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_2, dt_percent_lower = lower_percent_d3t_0, 'mt_percent_>0_lower' = lower_percent_mt_0
         ,sepsis_percent_lower = lower_percent_with_sepsis, caries_incisor_percent_lower = lower_percent_with_incisor_caries
         ,plaque_subst_percent_lower = lower_percent_with_substantial_plaque, area_code = upper_tier_la_code, area_name = upper_tier_la_name
         ,dmft_mean = mean_d3mft, dt_mean = mean_d3t, ft_mean = mean_ft, mt_mean = mean_mt, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_mean_mt_>0' = mean_mt_percent_mt_0, dmft_percent_0 = percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0_2, dt_percent = percent_d3t_0, 'mt_percent_>0' = percent_mt_0
         ,examined_sample_percent = percent_of_sample_examined_unavailable, caries_incisor_percent = percent_with_incisor_caries
         ,sepsis_percent = percent_with_sepsis, plaque_subst_percent = percent_with_substantial_plaque
         ,care_index_percent_upper = upper_care_index_percent, dmft_mean_upper = upper_d3mft, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean
         ,dt_mean_upper = upper_d3t, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean, ft_mean_upper = upper_ft, mt_mean_upper = upper_mt
         ,'mt_mean_mt_>0_upper' = upper_mt_0_mean, dmft_percent_0_upper = upper_percent_d3mft_0, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_2
         ,dt_percent_upper = upper_percent_d3t_0, 'mt_percent_>0_upper' = upper_percent_mt_0, sepsis_percent_upper = upper_percent_with_sepsis
         ,caries_incisor_percent_upper = upper_percent_with_incisor_caries, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         ,code_3t_percent = percent_with_code_3_t,code_3t_percent_lower = lower_percent_with_code_3_t,code_3t_percent_upper = upper_percent_with_code_3_t
  ) %>%
  filter(area_type == "utla") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2020 3 year old CCG file
ndep_5_2017_ccg <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2017_Results.xlsx"
                               , sheet = 'CCGs', range = "A6:BB223", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(22)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2017, child_age = 5, pop_year = 2016
    , area_type = case_when(
      substr(ccg_code,1,3) == "E92" ~ "nation"
      , substr(ccg_code,1,3) == "E12" ~ "region"
      , TRUE ~ "ccg")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,'dt_mean_dmft_>0'=NA,'mt_mean_dmft_>0'=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA,extraction_index_percent=NA
    ,pufa_percent=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA,'mt_mean_dmft_>0_lower'=NA
    ,'mt_mean_dmft_>0_upper'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,extraction_index_percent_lower=NA
    ,extraction_index_percent_upper=NA,pufa_percent_lower=NA,pufa_percent_upper=NA,examined_sample_percent=NA) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2016',care_index_percent_lower = lower_care_index_percent
         ,dmft_mean_lower = lower_d3mft, 'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_0_mean
         ,dt_mean_lower = lower_d3t, 'dt_mean_dt_>0_lower' = lower_d3t_0_mean, ft_mean_lower = lower_ft
         ,mt_mean_lower = lower_mt, 'mt_mean_mt_>0_lower' = lower_mt_0_mean, dmft_percent_0_lower = lower_percent_d3mft_0
         ,'dmft_percent_>0_lower' = lower_percent_d3mft_0_2, dt_percent_lower = lower_percent_d3t_0, 'mt_percent_>0_lower' = lower_percent_mt_0
         ,sepsis_percent_lower = lower_percent_with_sepsis, caries_incisor_percent_lower = lower_percent_with_incisor_caries
         ,plaque_subst_percent_lower = lower_percent_with_substantial_plaque, area_code = ccg_code, area_name = ccg_name
         ,dmft_mean = mean_d3mft, dt_mean = mean_d3t, ft_mean = mean_ft, mt_mean = mean_mt, 'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0, 'mt_mean_mt_>0' = mean_mt_percent_mt_0, dmft_percent_0 = percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0_2, dt_percent = percent_d3t_0, 'mt_percent_>0' = percent_mt_0
         , caries_incisor_percent = percent_with_incisor_caries
         ,sepsis_percent = percent_with_sepsis, plaque_subst_percent = percent_with_substantial_plaque
         ,care_index_percent_upper = upper_care_index_percent, dmft_mean_upper = upper_d3mft, 'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_0_mean
         ,dt_mean_upper = upper_d3t, 'dt_mean_dt_>0_upper' = upper_d3t_0_mean, ft_mean_upper = upper_ft, mt_mean_upper = upper_mt
         ,'mt_mean_mt_>0_upper' = upper_mt_0_mean, dmft_percent_0_upper = upper_percent_d3mft_0, 'dmft_percent_>0_upper' = upper_percent_d3mft_0_2
         ,dt_percent_upper = upper_percent_d3t_0, 'mt_percent_>0_upper' = upper_percent_mt_0, sepsis_percent_upper = upper_percent_with_sepsis
         ,caries_incisor_percent_upper = upper_percent_with_incisor_caries, plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         ,code_3t_percent = percent_with_code_3_t,code_3t_percent_lower = lower_percent_with_code_3_t,code_3t_percent_upper = upper_percent_with_code_3_t
  ) %>%
  filter(area_type == "ccg") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

ndep_5_2017 <- rbind(ndep_5_2017_ltla, ndep_5_2017_utla, ndep_5_2017_ccg)
rm(ndep_5_2017_ltla, ndep_5_2017_utla, ndep_5_2017_ccg) # remove component df's

#### load and standardise 2019 5 years files ####
ndep_5_2019_ltla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2019_Results.xlsx"
                               , sheet = '5yr Lower Tier LAs', range = "A6:BI341", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(25)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2019, child_age = 5, pop_year = 2018
    , area_type = case_when(
      substr(lower_tier_la_code,1,3) == "E92" ~ "nation"
      , substr(lower_tier_la_code,1,3) == "E12" ~ "region"
      , TRUE ~ "ltla")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA
    ,dmft_percent_0=NA,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA
    ,pufa_percent=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA
    ,pufa_percent_lower=NA,pufa_percent_upper=NA) %>%
  # Change above fields to numerical
  mutate_at(c('approximate_percent_of_sample_examined'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2018', area_code = lower_tier_la_code, area_name = lower_tier_la_name
         ,examined_sample_percent = approximate_percent_of_sample_examined
         ,care_index_percent = care_index_percent_ft_d3mft, care_index_percent_lower = lower_care_index_percent, care_index_percent_upper = upper_care_index_percent
         ,caries_incisor_percent = percent_with_incisor_caries, caries_incisor_percent_lower = lower_percent_with_incisor_caries, caries_incisor_percent_upper = upper_percent_with_incisor_caries
         ,code_3t_percent = percent_with_code_3_t, code_3t_percent_lower = lower_percent_with_code_3_t, code_3t_percent_upper = upper_percent_with_code_3_t
         ,dmft_mean = mean_d3mft, dmft_mean_lower = lower_mean_d3mft, dmft_mean_upper = upper_mean_d3mft
         ,'dmft_mean_dmft_percent_>0' = mean_d3mft_d3mft_0, 'dmft_mean_dmft_percent_>0_lower' = lower_mean_d3mft_d3mft_0, 'dmft_mean_dmft_percent_>0_upper' = upper_mean_d3mft_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0,'dmft_percent_>0_lower' = lower_percent_d3mft_0,'dmft_percent_>0_upper' = upper_percent_d3mft_0
         ,dt_mean = mean_d3t,dt_mean_lower = lower_mean_d3t,dt_mean_upper = upper_mean_d3t
         ,'dt_mean_dmft_>0' = mean_d3t_d3mft_0,'dt_mean_dmft_>0_lower' = lower_mean_d3t_d3mft_0,'dt_mean_dmft_>0_upper' = upper_mean_d3t_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_d3t_0,'dt_mean_dt_>0_lower' = lower_mean_d3t_d3t_0,'dt_mean_dt_>0_upper' = upper_mean_d3t_d3t_0
         ,dt_percent = percent_d3t_0,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,extraction_index_percent = extraction_index_percent_mt_d3mft,extraction_index_percent_lower = lower_extraction_index_percent,extraction_index_percent_upper = upper_extraction_index_percent
         ,ft_mean = mean_ft,ft_mean_lower = lower_mean_ft,ft_mean_upper = upper_mean_ft
         ,mt_mean = mean_mt,mt_mean_lower = lower_mean_mt,mt_mean_upper = upper_mean_mt
         ,'mt_mean_dmft_>0' = mean_mt_d3mft_0,'mt_mean_dmft_>0_lower' = lower_mean_mt_d3mft_0,'mt_mean_dmft_>0_upper' = upper_mean_mt_d3mft_0
         ,'mt_mean_mt_>0' = mean_mt_mt_0,'mt_mean_mt_>0_lower' = lower_mean_mt_mt_0,'mt_mean_mt_>0_upper' = upper_mean_mt_mt_0
         ,'mt_percent_>0' = percent_mt_0,'mt_percent_>0_lower' = lower_percent_mt_0,'mt_percent_>0_upper' = upper_percent_mt_0
         ,plaque_subst_percent = percent_with_substantial_plaque,plaque_subst_percent_lower = lower_percent_with_substantial_plaque,plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         ,sepsis_percent = percent_with_sepsis,sepsis_percent_lower = lower_percent_with_sepsis,sepsis_percent_upper = upper_percent_with_sepsis
         
  ) %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2017 5 year old UTLA file
ndep_5_2019_utla <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2019_Results.xlsx"
                               , sheet = '5yr Upper Tier LAs', range = "A6:BI165", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(25)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(region, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2019, child_age = 5, pop_year = 2018
    , area_type = case_when(
      substr(upper_tier_la_code,1,3) == "E92" ~ "nation"
      , substr(upper_tier_la_code,1,3) == "E12" ~ "region"
      , TRUE ~ "utla")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA
    ,dmft_percent_0=NA,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA
    ,pufa_percent=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA
    ,pufa_percent_lower=NA,pufa_percent_upper=NA) %>%
  # Change above fields to numerical
  mutate_at(c('approximate_percent_of_sample_examined'), as.numeric) %>%
  # rename remaining fields to help standardise across surveys
  rename(pop_mid = 'x5_year_old_population_mid_2018', area_code = upper_tier_la_code, area_name = upper_tier_la_name
         ,examined_sample_percent = approximate_percent_of_sample_examined
         ,care_index_percent = care_index_percent_ft_d3mft, care_index_percent_lower = lower_care_index_percent, care_index_percent_upper = upper_care_index_percent
         ,caries_incisor_percent = percent_with_incisor_caries, caries_incisor_percent_lower = lower_percent_with_incisor_caries, caries_incisor_percent_upper = upper_percent_with_incisor_caries
         ,code_3t_percent = percent_with_code_3_t, code_3t_percent_lower = lower_percent_with_code_3_t, code_3t_percent_upper = upper_percent_with_code_3_t
         ,dmft_mean = mean_d3mft, dmft_mean_lower = lower_mean_d3mft, dmft_mean_upper = upper_mean_d3mft
         ,'dmft_mean_dmft_percent_>0' = mean_d3mft_d3mft_0, 'dmft_mean_dmft_percent_>0_lower' = lower_mean_d3mft_d3mft_0, 'dmft_mean_dmft_percent_>0_upper' = upper_mean_d3mft_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0,'dmft_percent_>0_lower' = lower_percent_d3mft_0,'dmft_percent_>0_upper' = upper_percent_d3mft_0
         ,dt_mean = mean_d3t,dt_mean_lower = lower_mean_d3t,dt_mean_upper = upper_mean_d3t
         ,'dt_mean_dmft_>0' = mean_d3t_d3mft_0,'dt_mean_dmft_>0_lower' = lower_mean_d3t_d3mft_0,'dt_mean_dmft_>0_upper' = upper_mean_d3t_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_d3t_0,'dt_mean_dt_>0_lower' = lower_mean_d3t_d3t_0,'dt_mean_dt_>0_upper' = upper_mean_d3t_d3t_0
         ,dt_percent = percent_d3t_0,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,extraction_index_percent = extraction_index_percent_mt_d3mft,extraction_index_percent_lower = lower_extraction_index_percent,extraction_index_percent_upper = upper_extraction_index_percent
         ,ft_mean = mean_ft,ft_mean_lower = lower_mean_ft,ft_mean_upper = upper_mean_ft
         ,mt_mean = mean_mt,mt_mean_lower = lower_mean_mt,mt_mean_upper = upper_mean_mt
         ,'mt_mean_dmft_>0' = mean_mt_d3mft_0,'mt_mean_dmft_>0_lower' = lower_mean_mt_d3mft_0,'mt_mean_dmft_>0_upper' = upper_mean_mt_d3mft_0
         ,'mt_mean_mt_>0' = mean_mt_mt_0,'mt_mean_mt_>0_lower' = lower_mean_mt_mt_0,'mt_mean_mt_>0_upper' = upper_mean_mt_mt_0
         ,'mt_percent_>0' = percent_mt_0,'mt_percent_>0_lower' = lower_percent_mt_0,'mt_percent_>0_upper' = upper_percent_mt_0
         ,plaque_subst_percent = percent_with_substantial_plaque,plaque_subst_percent_lower = lower_percent_with_substantial_plaque,plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         ,sepsis_percent = percent_with_sepsis,sepsis_percent_lower = lower_percent_with_sepsis,sepsis_percent_upper = upper_percent_with_sepsis
         
  ) %>%
  filter(area_type == "utla") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

# load and standardise 2019 5 year old CCG file
ndep_5_2019_ccg <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_5Yr_2019_Results.xlsx"
                               , sheet = '5yr Clinical Commissioning Grps', range = "A6:BH212", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(24)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(nhs_england_regional_local_office, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2019, child_age = 5, pop_year = 2018
    , area_type = case_when(
      substr(clinical_commissioning_group_code,1,3) == "E92" ~ "nation"
      , substr(clinical_commissioning_group_code,1,3) == "E39" ~ "region"
      , TRUE ~ "ccg")
    ,drawn_sample=NA,examined_pop_percent=NA,examined_consent_percent=NA,examined_sample_percent=NA
    , 'mt_percent_dmft_>0'=NA,'ft_mean_dmft_>0'=NA,caries_ecc_percent=NA
    ,dmft_percent_0=NA,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA
    ,pufa_percent=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA,'ft_mean_dmft_>0_lower'=NA
    ,'ft_mean_dmft_>0_upper'=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA
    ,pufa_percent_lower=NA,pufa_percent_upper=NA) %>%
  # rename remaining fields to help standardise across surveys
  rename(region = nhs_england_regional_local_office
         ,pop_mid = 'x5_year_old_population_mid_2018', area_code = clinical_commissioning_group_code, area_name = clinical_commissioning_group_name
         ,care_index_percent = care_index_percent_ft_d3mft, care_index_percent_lower = lower_care_index_percent, care_index_percent_upper = upper_care_index_percent
         ,caries_incisor_percent = percent_with_incisor_caries, caries_incisor_percent_lower = lower_percent_with_incisor_caries, caries_incisor_percent_upper = upper_percent_with_incisor_caries
         ,code_3t_percent = percent_with_code_3_t, code_3t_percent_lower = lower_percent_with_code_3_t, code_3t_percent_upper = upper_percent_with_code_3_t
         ,dmft_mean = mean_d3mft, dmft_mean_lower = lower_mean_d3mft, dmft_mean_upper = upper_mean_d3mft
         ,'dmft_mean_dmft_percent_>0' = mean_d3mft_d3mft_0, 'dmft_mean_dmft_percent_>0_lower' = lower_mean_d3mft_d3mft_0, 'dmft_mean_dmft_percent_>0_upper' = upper_mean_d3mft_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0,'dmft_percent_>0_lower' = lower_percent_d3mft_0,'dmft_percent_>0_upper' = upper_percent_d3mft_0
         ,dt_mean = mean_d3t,dt_mean_lower = lower_mean_d3t,dt_mean_upper = upper_mean_d3t
         ,'dt_mean_dmft_>0' = mean_d3t_d3mft_0,'dt_mean_dmft_>0_lower' = lower_mean_d3t_d3mft_0,'dt_mean_dmft_>0_upper' = upper_mean_d3t_d3mft_0
         ,'dt_mean_dt_>0' = mean_d3t_d3t_0,'dt_mean_dt_>0_lower' = lower_mean_d3t_d3t_0,'dt_mean_dt_>0_upper' = upper_mean_d3t_d3t_0
         ,dt_percent = percent_d3t_0,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,extraction_index_percent = extraction_index_percent_mt_d3mft,extraction_index_percent_lower = lower_extraction_index_percent,extraction_index_percent_upper = upper_extraction_index_percent
         ,ft_mean = mean_ft,ft_mean_lower = lower_mean_ft,ft_mean_upper = upper_mean_ft
         ,mt_mean = mean_mt,mt_mean_lower = lower_mean_mt,mt_mean_upper = upper_mean_mt
         ,'mt_mean_dmft_>0' = mean_mt_d3mft_0,'mt_mean_dmft_>0_lower' = lower_mean_mt_d3mft_0,'mt_mean_dmft_>0_upper' = upper_mean_mt_d3mft_0
         ,'mt_mean_mt_>0' = mean_mt_mt_0,'mt_mean_mt_>0_lower' = lower_mean_mt_mt_0,'mt_mean_mt_>0_upper' = upper_mean_mt_mt_0
         ,'mt_percent_>0' = percent_mt_0,'mt_percent_>0_lower' = lower_percent_mt_0,'mt_percent_>0_upper' = upper_percent_mt_0
         ,plaque_subst_percent = percent_with_substantial_plaque,plaque_subst_percent_lower = lower_percent_with_substantial_plaque,plaque_subst_percent_upper = upper_percent_with_substantial_plaque
         ,sepsis_percent = percent_with_sepsis,sepsis_percent_lower = lower_percent_with_sepsis,sepsis_percent_upper = upper_percent_with_sepsis
         
  ) %>%
  filter(area_type == "ccg") %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

ndep_5_2019 <- rbind(ndep_5_2019_ltla, ndep_5_2019_utla, ndep_5_2019_ccg)
rm(ndep_5_2019_ltla, ndep_5_2019_utla, ndep_5_2019_ccg) # remove component df's

#### load and standardise 2009 12 years files ####
ndep_12_2009 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_survey_12Yr_2009_Results_LA.xlsx"
                               , sheet = 'LA', range = "A5:AI351", col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  select(-c(17)) %>% #remove empty column
  clean_names() %>% #lowercase and underscore
  fill(gor, .direction = "down") %>% # fill empty region values from top cell in group
  mutate(#fields to generate or null
    period = 2009, child_age = 12, pop_year = 2008
    , area_type = case_when(
      la_code == "Eng" ~ "nation"
      , la_code %in% c("A","B","D","E","F","G","H","J","K") ~ "region"
      , TRUE ~ "ltla")
    ,examined_pop_percent=NA,examined_consent_percent=NA
    ,caries_ecc_percent=NA,caries_ecc_percent_lower=NA,caries_ecc_percent_upper=NA,caries_incisor_percent=NA,caries_incisor_percent_lower=NA,caries_incisor_percent_upper=NA
    ,code_3t_percent=NA,code_3t_percent_lower=NA,code_3t_percent_upper=NA
    ,dmft_percent_0=NA,dmft_percent_0_lower=NA,dmft_percent_0_upper=NA
    ,'dt_mean_dmft_>0'=NA,'dt_mean_dmft_>0_lower'=NA,'dt_mean_dmft_>0_upper'=NA
    ,extraction_index_percent=NA,extraction_index_percent_lower=NA,extraction_index_percent_upper=NA
    ,'ft_mean_dmft_>0'=NA,'ft_mean_dmft_>0_lower'=NA,'ft_mean_dmft_>0_upper'=NA
    ,'mt_mean_dmft_>0'=NA,'mt_mean_dmft_>0_lower'=NA,'mt_mean_dmft_>0_upper'=NA
    ,'mt_mean_mt_>0'=NA,'mt_mean_mt_>0_lower'=NA,'mt_mean_mt_>0_upper'=NA
    ,'mt_percent_>0'=NA,'mt_percent_>0_lower'=NA,'mt_percent_>0_upper'=NA
    ,'mt_percent_dmft_>0'=NA,'mt_percent_dmft_>0_lower'=NA,'mt_percent_dmft_>0_upper'=NA
    ,plaque_subst_percent=NA,plaque_subst_percent_lower=NA,plaque_subst_percent_upper=NA
    ,pufa_percent=NA,pufa_percent_lower=NA,pufa_percent_upper=NA
    ,sepsis_percent=NA,sepsis_percent_lower=NA,sepsis_percent_upper=NA
  ) %>%
  # rename remaining fields to help standardise across surveys
  rename(region = gor, area_code = la_code ,area_name = la_name,pop_mid = x12_year_old_population_mid_2008
         ,examined_sample_percent = percent_examined
         ,care_index_percent_lower = lower_care_index_percent,care_index_percent_upper = upper_care_index_percent
         ,dmft_mean = mean_d3mft,dmft_mean_lower = lower_d3mft,dmft_mean_upper = upper_d3mft
         ,'dmft_mean_dmft_percent_>0' = mean_d3mft_percent_d3mft_0,'dmft_mean_dmft_percent_>0_lower' = lower_d3mft_percent_d3mft_0,'dmft_mean_dmft_percent_>0_upper' = upper_d3mft_percent_d3mft_0
         ,'dmft_percent_>0' = percent_d3mft_0,'dmft_percent_>0_lower' = lower_percent_d3mft_0,'dmft_percent_>0_upper' = upper_percent_d3mft_0
         ,dt_mean = mean_d3t,dt_mean_lower = lower_d3t,dt_mean_upper = upper_d3t
         ,'dt_mean_dt_>0' = mean_d3t_percent_d3t_0,'dt_mean_dt_>0_lower' = lower_d3t_percent_d3t_0,'dt_mean_dt_>0_upper' = upper_d3t_percent_d3t_0
         ,dt_percent = percent_d3t_0,dt_percent_lower = lower_percent_d3t_0,dt_percent_upper = upper_percent_d3t_0
         ,ft_mean = mean_ft,ft_mean_lower = lower_ft,ft_mean_upper = upper_ft
         ,mt_mean = mean_mt,mt_mean_lower = lower_mt,mt_mean_upper = upper_mt
      ) %>%
  select(period,child_age,region,area_code,area_name,area_type,pop_mid,pop_year,drawn_sample,examined,examined_pop_percent,examined_sample_percent,examined_consent_percent
         ,care_index_percent,care_index_percent_lower,care_index_percent_upper
         ,caries_ecc_percent,caries_ecc_percent_lower,caries_ecc_percent_upper,caries_incisor_percent,caries_incisor_percent_lower,caries_incisor_percent_upper
         ,code_3t_percent,code_3t_percent_lower,code_3t_percent_upper
         ,dmft_mean,dmft_mean_lower,dmft_mean_upper,'dmft_mean_dmft_percent_>0','dmft_mean_dmft_percent_>0_lower','dmft_mean_dmft_percent_>0_upper'
         ,'dmft_percent_>0','dmft_percent_>0_lower','dmft_percent_>0_upper',dmft_percent_0,dmft_percent_0_lower,dmft_percent_0_upper
         ,dt_mean,dt_mean_lower,dt_mean_upper,'dt_mean_dmft_>0','dt_mean_dmft_>0_lower','dt_mean_dmft_>0_upper','dt_mean_dt_>0','dt_mean_dt_>0_lower','dt_mean_dt_>0_upper'
         ,dt_percent,dt_percent_lower,dt_percent_upper
         ,extraction_index_percent,extraction_index_percent_lower,extraction_index_percent_upper
         ,ft_mean,ft_mean_lower,ft_mean_upper,'ft_mean_dmft_>0','ft_mean_dmft_>0_lower','ft_mean_dmft_>0_upper'
         ,mt_mean,mt_mean_lower,mt_mean_upper,'mt_mean_dmft_>0','mt_mean_dmft_>0_lower','mt_mean_dmft_>0_upper'
         ,'mt_mean_mt_>0','mt_mean_mt_>0_lower','mt_mean_mt_>0_upper'
         ,'mt_percent_>0','mt_percent_>0_lower','mt_percent_>0_upper','mt_percent_dmft_>0','mt_percent_dmft_>0_lower','mt_percent_dmft_>0_upper'
         ,plaque_subst_percent,plaque_subst_percent_lower,plaque_subst_percent_upper
         ,pufa_percent,pufa_percent_lower,pufa_percent_upper
         ,sepsis_percent,sepsis_percent_lower,sepsis_percent_upper)

#### join all together to one df and write csv####

rbind(ndep_3_2013, ndep_3_2020, ndep_5_2015, ndep_5_2017, ndep_5_2019,ndep_12_2009) %>%
  write.csv(file="C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/ndep_collated.csv")
  
rm(ndep_3_2013, ndep_3_2020, ndep_5_2015, ndep_5_2017, ndep_5_2019,ndep_12_2009) # remove any remaining component df's