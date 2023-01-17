#### To load and compile the GPPS dental survey data...
# load packages
library(readxl) #reading xls files
library(janitor) #cleaning column names
library(plyr) #knitting csv's
library(dplyr) #creating and changing variables
library(tidyr)

# turn off silly engineering notation for large/tiny numbers
options(scipen = 999)

# load ccg to ICB lookup
ccg_to_icb <- read.csv(file = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/reference/CCG_ICB_overlap_all_trunc_v2.csv"
                       , header = TRUE)

# load old icb to new icb codes lookup
icb_to_icb <- read.csv(file = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/reference/ICBold_to_ICBnew.csv"
                       , header = TRUE)

#### Load all the GPPS data ####
# load latest file (as template code)
gpps_2022 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2022.xlsx"
                        , sheet = 'Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ics_code_ons,1,3) == "E40" ~ "Region"
    , substr(ics_code_ons,1,3) == "E54" ~ "ICB"
    , substr(ics_code_ons,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2022
         , q3_no = (no_no_appointments_were_available + no_the_dentist_was_not_taking_new_patients + no_for_another_reason)
         , q5_no_teeth = NA, q5_not_time = NA, q5_switch = NA) %>%
  rename(area_code_ons = ics_code_ons, area_code_NHS = ics_code, area_region = region_name, area_name = ics_region_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = never_tried_to_get_an_nhs_dental_appointment_14
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_noapps = no_no_appointments_were_available
         , q3_nopats = no_the_dentist_was_not_taking_new_patients, q3_other = no_for_another_reason, q3_notrem = cant_remember_33
         
         , q4_resp = total_responses_39, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_50, q5_not_need = i_havent_needed_to_visit_a_dentist, q5_not_like = i_dont_like_going_to_the_dentist
         , q5_not_think = i_didnt_think_i_could_get_an_nhs_dentist, q5_waiting = im_on_a_waiting_list_for_an_nhs_dentist
         , q5_private = i_prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

# and historic files too
gpps_2021 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2021.xlsx"
                        , sheet = 'Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2021
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2020 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2020.xlsx"
                        , sheet = 'Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2020
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2019 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2019.xlsx"
                        , sheet = 'Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2019
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2018 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2018.xlsx"
                        , sheet = 'Question Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2018
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2017 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2017.xlsx"
                        , sheet = 'Question Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2017
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2016 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2016.xlsx"
                        , sheet = 'Question responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2016
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2015 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2015.xls"
                        , sheet = 'Question Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_commissioning_region_or_ccg_code,1,3) == "E40" ~ "Region"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E54" ~ "ICB"
    , substr(ons_commissioning_region_or_ccg_code,1,3) == "E38" ~ "CCG"
    , TRUE ~ "England")) %>%
  mutate(period = 2015
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  rename(area_code_ons = ons_commissioning_region_or_ccg_code, area_code_NHS = commissioning_region_or_ccg_code, area_region = commissioning_region_name, area_name = commissioning_region_or_ccg_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_8, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_21, q2_yes = yes_22, q2_no = no_23, q2_notrem = cant_remember_24
         
         , q3_resp = total_responses_28, q3_yes = yes_29, q3_no = no_30, q3_notrem = cant_remember_31
         
         , q4_resp = total_responses_35, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_46, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other)

gpps_2014 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2014.xls"
                        , sheet = 'Question Responses ', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_region_or_area_team_code,1,3) == "E40" ~ "Region"
    , substr(ons_region_or_area_team_code,1,3) == "E54" ~ "ICB"
    , substr(ons_region_or_area_team_code,1,3) == "E38" ~ "CCG"
    , substr(ons_region_or_area_team_code,1,3) == "E39" ~ "AreaTeam"
    , TRUE ~ "England")) %>%
  mutate(period = 2014
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  mutate(area_region = NA) %>%
  rename(area_code_ons = ons_region_or_area_team_code, area_code_NHS = region_or_area_team_code, area_name = region_or_area_team_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_7, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_20, q2_yes = yes_21, q2_no = no_22, q2_notrem = cant_remember_23
         
         , q3_resp = total_responses_27, q3_yes = yes_28, q3_no = no_29, q3_notrem = cant_remember_30
         
         , q4_resp = total_responses_34, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_45, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other) %>%
  filter(!row_number() %in% c(31,32)) #remove rows with notes

gpps_2013 <- read_excel(path = "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_raw/GP-Patient-Survey-Dental-Results-January-to-March-2013.xls"
                        , sheet = 'Question Responses', skip = 3, col_names = TRUE) %>%
  filter_all(any_vars(!is.na(.))) %>% #remove empty rows
  clean_names() %>% #lowercase and underscore
  mutate(area_type = case_when(
    substr(ons_sha_or_pct_code,1,3) == "E40" ~ "Region"
    , substr(ons_sha_or_pct_code,1,3) == "E54" ~ "ICB"
    , substr(ons_sha_or_pct_code,1,3) == "E38" ~ "CCG"
    , substr(ons_sha_or_pct_code,1,3) == "E39" ~ "AreaTeam"
    , substr(ons_sha_or_pct_code,1,3) == "E18" ~ "SHA"
    , substr(ons_sha_or_pct_code,1,3) %in% c("E16","E17") ~ "PCT"
    , TRUE ~ "England")) %>%
  mutate(period = 2013
         , q3_noapps = NA, q3_nopats = NA, q3_other = NA) %>%
  mutate(area_region = NA) %>%
  rename(area_code_ons = ons_sha_or_pct_code, area_code_NHS = pct_or_sha_code, area_name = sha_or_pct_name
         , forms_dist = total_survey_forms_distributed, forms_resp = total_completed_forms_received
         , q1_resp = total_responses_7, "q1_<3" = in_the_last_3_months, q1_3to6 = between_3_and_6_months_ago, q1_6to12 = between_6_months_and_a_year_ago
         , q1_12to24 = between_1_and_2_years_ago, "q1_>24" = more_than_2_years_ago, q1_never = have_never_tried_to_get_an_nhs_dental_appointment
         
         , q2_resp = total_responses_20, q2_yes = yes_21, q2_no = no_22, q2_notrem = cant_remember_23
         
         , q3_resp = total_responses_27, q3_yes = yes_28, q3_no = no_29, q3_notrem = cant_remember_30
         
         , q4_resp = total_responses_34, q4_very_gd = very_good, q4_fairly_gd = fairly_good
         , q4_neither = neither_good_nor_poor, q4_fairly_pr = fairly_poor, q4_very_pr = very_poor
         
         , q5_resp = total_responses_45, q5_not_need = havent_needed_to_visit_a_dentist, q5_no_teeth = no_longer_have_any_natural_teeth
         , q5_not_time = havent_had_time_to_visit_a_dentist, q5_not_like = dont_like_going_to_the_dentist
         , q5_not_think = didnt_think_could_get_an_nhs_dentist, q5_waiting = on_a_waiting_list_for_an_nhs_dentist
         , q5_switch = stayed_with_dentist_when_they_changed_from_nhs_to_private
         , q5_private = prefer_to_go_to_a_private_dentist, q5_too_exp = nhs_dental_care_is_too_expensive, q5_other = another_reason) %>%
  
  select(period, area_code_ons, area_code_NHS, area_region, area_name, area_type
         , forms_dist, forms_resp
         , q1_resp, "q1_<3" , q1_3to6 , q1_6to12 , q1_12to24, "q1_>24", q1_never
         , q2_resp, q2_yes, q2_no, q2_notrem
         , q3_resp, q3_yes, q3_no, q3_noapps, q3_nopats, q3_other, q3_notrem
         , q4_resp, q4_very_gd, q4_fairly_gd, q4_neither, q4_fairly_pr, q4_very_pr
         , q5_resp, q5_not_need, q5_no_teeth, q5_not_time, q5_not_like, q5_not_think, q5_waiting, q5_switch, q5_private, q5_too_exp, q5_other) %>%
  filter(!row_number() %in% c(163,164)) #remove rows with notes

gpps_all <- rbind(gpps_2013, gpps_2014, gpps_2015, gpps_2016, gpps_2017, gpps_2018, gpps_2019, gpps_2020, gpps_2021, gpps_2022)


# join to GPPS data frame and apportion
gpps_all <- gpps_all %>%
  # add existing ICB codes and names to joined fields
  full_join(select(ccg_to_icb, ccg_code, ICB22CD, ICB22NM, appt_area, appt_pop)
            , by = c("area_code_ons" = "ccg_code"), na_matches = "never") %>%
  # additional join to turn 2022 deprecated codes into current ICB codes and names #
  full_join(select(icb_to_icb, icb_code_a, icb_code_b, icb_name_b)
            , by = c("area_code_ons" = "icb_code_a"), na_matches = "never") %>%
  mutate(ICB22CD = case_when(
    area_type == "ICB" ~ icb_code_b
    , TRUE ~ ICB22CD)
    ,ICB22NM = case_when(
      area_type == "ICB" ~ icb_name_b
      , TRUE ~ ICB22NM)) %>%
  # add 1 value to existing ICB data (i.e. 2022 onwards) and other area types
  mutate(appt_area = case_when(
    area_type == "CCG" ~ appt_area
    , TRUE ~ 1)
    , appt_pop = case_when(
      area_type == "CCG" ~ appt_pop
      , TRUE ~ 1)) %>%
  mutate(across(c(7:43), function(x) x*appt_pop)) %>%
  select(-c(icb_code_b, icb_name_b))

# remove component df's - redundant
rm(gpps_2013, gpps_2014, gpps_2015, gpps_2016, gpps_2017, gpps_2018, gpps_2019, gpps_2020, gpps_2021, gpps_2022)

# write output file
write.csv(gpps_all, "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_all.csv", row.names=TRUE)

# ICB aggregates 2015 onwards
gpps_icb <- gpps_all %>%
  filter(period >= 2015, !is.na(ICB22CD)) %>% #only periods with CCG/ICB data
  select(-c(area_code_ons, area_code_NHS, area_region, area_name, area_type, appt_area, appt_pop)) %>% #remove unwanted columns
  group_by(period, ICB22CD, ICB22NM) %>%
  summarise_all(sum)

# write output file
write.csv(gpps_icb, "C:/Users/Andrew.Hood/OneDrive - Midlands and Lancashire CSU/Working Projects/1072 Dental Analysis/1072_dental_strategy/1_strategic_data_review/data/gpps_ICB.csv", row.names=TRUE)

 
  
