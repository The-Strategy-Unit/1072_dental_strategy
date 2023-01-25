# Wrangle and merge NHSE data set variables 

library(tidyverse)
library(janitor)
library(readxl)

# Import raw data
unique_patients_icb        <- read_excel("1_strategic_data_review/data/nhse_dataset_raw_tables.xlsx", sheet = "unique_patients_icb") %>% clean_names()
urgent_treatment_forms_icb <- read_excel("1_strategic_data_review/data/nhse_dataset_raw_tables.xlsx", sheet = "urgent_forms_icb") %>% clean_names()
uda_delivered_icb          <- read_excel("1_strategic_data_review/data/nhse_dataset_raw_tables.xlsx", sheet = "uda_delivered_icb") %>% clean_names()
uoa_delivered_icb          <- read_excel("1_strategic_data_review/data/nhse_dataset_raw_tables.xlsx", sheet = "uoa_delivered_icb") %>% clean_names()


# Combine 
combined <-
  uda_delivered_icb %>% 
  select(1:4) %>% 
  left_join(uoa_delivered_icb %>% 
              select(1:4), by = c("schedule_month", "region_name", "commissioner_name") ) %>% 
  left_join(urgent_treatment_forms_icb %>% 
              select(-financial_year),
            by = c("schedule_month" = "month", "region_name", "commissioner_name")) %>% 
  left_join(unique_patients_icb %>% 
              rename(schedule_month = x12_month_period_end), 
            by = c("schedule_month", "region_name", "commissioner_name"))


# Clean environment
rm(unique_patients_icb,       
   urgent_treatment_forms_icb,
   uda_delivered_icb,         
   uoa_delivered_icb
   )

write_csv(combined, "1_strategic_data_review/data/nhse_dataset_indicators_icb.csv")

# Explore temporal coverage
combined %>% 
  pivot_longer(cols = c(-schedule_month, -region_name, -commissioner_name)) %>% 
  
  ggplot(aes(x = schedule_month, y = value, fill = name)) +
  geom_col() +
  facet_wrap(~name, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = c(0.8,0.2))





