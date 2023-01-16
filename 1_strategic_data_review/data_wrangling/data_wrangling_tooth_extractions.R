# Import and organise dental extract data 

library(tidyverse)
library(janitor)
library(readxl)


# 202021
extraction_2021 <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2018_to_2021.xlsx", sheet = "W11", range = "A8:H316") %>% clean_names()
la_pop_estimates_2021 <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2018_to_2021.xlsx", sheet = "W11", range = "J8:Q316") %>% clean_names()

#201920
extraction_1920 <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2018_to_2021.xlsx", sheet = "W13", range = "A8:H316") %>% clean_names()
la_pop_estimates_1920 <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2018_to_2021.xlsx", sheet = "W13", range = "J8:Q316") %>% clean_names()

# 201819
extraction_1819 <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2018_to_2021.xlsx", sheet = "W14", range = "A8:H316") %>% clean_names()
la_pop_estimates_1819 <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2018_to_2021.xlsx", sheet = "W14", range = "J8:Q316") %>% clean_names()

# 201718
extraction_1718_all <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2016_to_2020.xlsx", sheet = "LA of residence 2017-18", range = "A4:S327") %>% clean_names()
extraction_1718 <- extraction_1718_all %>% select(1:3, 4:8) 
la_pop_estimates_1718 <- extraction_1718_all %>% select(1:3, 10:14) 

rm(extraction_1718_all)

# 201617
extraction_1617_all <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2016_to_2020.xlsx", sheet = "LA of residence 2016-17", range = "A4:S327") %>% clean_names()
extraction_1617 <- extraction_1617_all %>% select(1:3, 4:8) 
la_pop_estimates_1617 <- extraction_1617_all %>% select(1:3, 10:14) 

rm(extraction_1617_all)

# 201516
extraction_1516_all <- read_excel("1_strategic_data_review/data/Hospital_teeth_extractions__0-19Y_2016_to_2020.xlsx", sheet = "LA of residence 2015-16", range = "A4:S327") %>% clean_names()
extraction_1516 <- extraction_1516_all %>% select(1:3, 4:8) 
la_pop_estimates_1516 <- extraction_1516_all %>% select(1:3, 10:14) 

rm(extraction_1516_all)


# Lengthen and combine extraction count, pop estimate and rate per 100,000
# 2020-21
extraction_2021_long <-
  extraction_2021 %>%
  mutate(age_0_5yrs   = as.numeric(age_0_5yrs  ),
         age_6_10yrs  = as.numeric(age_6_10yrs ),
         age_11_14yrs = as.numeric(age_11_14yrs),
         age_15_19yrs = as.numeric(age_15_19yrs),
         total_0_19yrs = as.numeric(total_0_19yrs)
         ) %>% 
  pivot_longer(cols = c(contains("age_"), total_0_19yrs), values_to = "extractions") %>% 
  left_join(la_pop_estimates_2021 %>% 
              pivot_longer(cols = c(contains("age_"), total_0_19yrs), values_to = "population_est"),
            by = c("region", "la_code", "la_name", "name")) %>% 
  mutate(year = 2021)

rm(extraction_2021,
   la_pop_estimates_2021
   )

# 2019-20
extraction_1920_long <-
  extraction_1920 %>%
  mutate(age_0_5yrs   = as.numeric(age_0_5yrs  ),
         age_6_10yrs  = as.numeric(age_6_10yrs ),
         age_11_14yrs = as.numeric(age_11_14yrs),
         age_15_19yrs = as.numeric(age_15_19yrs)
         ) %>% 
  pivot_longer(cols = c(contains("age_"), total_0_19yrs), values_to = "extractions") %>% 
  left_join(la_pop_estimates_1920 %>% 
             pivot_longer(cols = c(contains("age_"), total_0_19yrs), values_to = "population_est"),
            by = c("region", "la_code", "la_name", "name")) %>% 
  mutate(year = 1920)

rm(extraction_1920,
   la_pop_estimates_1920
   )

# 2018-19
extraction_1819_long <-
  extraction_1819 %>%
  mutate(age_0_5yrs   = as.numeric(age_0_5yrs  ),
         age_6_10yrs  = as.numeric(age_6_10yrs ),
         age_11_14yrs = as.numeric(age_11_14yrs),
         age_15_19yrs = as.numeric(age_15_19yrs),
         total_0_19yrs = as.numeric(total_0_19yrs)
         ) %>% 
  pivot_longer(cols = c(contains("age_"), total_0_19yrs), values_to = "extractions") %>% 
  left_join(la_pop_estimates_1819 %>% 
              mutate(age_0_5yrs   = as.numeric(age_0_5yrs  ),
                     age_6_10yrs  = as.numeric(age_6_10yrs ),
                     age_11_14yrs = as.numeric(age_11_14yrs),
                     age_15_19yrs = as.numeric(age_15_19yrs),
                     total_0_19yrs = as.numeric(total_0_19yrs)
                     ) %>%
              pivot_longer(cols = c(contains("age_"), total_0_19yrs), values_to = "population_est"),
            by = c("region", "la_code", "la_name", "name")) %>% 
  mutate(year = 1819)

rm(extraction_1819,
   la_pop_estimates_1819
   )

# 2017-18
extraction_1718_long <-
  extraction_1718 %>%
  mutate(age_0_5yrs_4 = as.numeric(age_0_5yrs_4),
         age_6_10yrs_5 = as.numeric(age_6_10yrs_5),
         age_11_14yrs_6 = as.numeric(age_11_14yrs_6),
         age_15_19yrs_7 = as.numeric(age_15_19yrs_7)
         ) %>% 
  pivot_longer(cols = c(contains("age_"), total_0_19yrs_8), values_to = "extractions") %>% 
  mutate(name = str_sub(name, end=-3)) %>%
  left_join(la_pop_estimates_1718 %>% 
              pivot_longer(cols = c(contains("age_"), total_0_19yrs_14), values_to = "population_est") %>% 
              mutate(name = str_sub(name, end=-4)),
            by = c("region", "la_code", "la_name", "name")) %>% 
  mutate(year = 1718)

rm(extraction_1718,
   la_pop_estimates_1718
   )

# 2016-17
extraction_1617_long <-
  extraction_1617 %>%
  mutate(age_0_5yrs_4 = as.numeric(age_0_5yrs_4),
         age_6_10yrs_5 = as.numeric(age_6_10yrs_5),
         age_11_14yrs_6 = as.numeric(age_11_14yrs_6),
         age_15_19yrs_7 = as.numeric(age_15_19yrs_7)
         ) %>% 
  pivot_longer(cols = c(contains("age_"), total_0_19yrs_8), values_to = "extractions") %>% 
  mutate(name = str_sub(name, end=-3)) %>%
  left_join(la_pop_estimates_1617 %>% 
              pivot_longer(cols = c(contains("age_"), total_0_19yrs_14), values_to = "population_est") %>% 
              mutate(name = str_sub(name, end=-4)),
            by = c("region", "la_code", "la_name", "name")) %>% 
  mutate(year = 1617)

rm(extraction_1617,
   la_pop_estimates_1617
   )

# 2015-16
extraction_1516_long <-
  extraction_1516 %>%
  mutate(age_0_5yrs_4 = as.numeric(age_0_5yrs_4),
         age_6_10yrs_5 = as.numeric(age_6_10yrs_5),
         age_11_14yrs_6 = as.numeric(age_11_14yrs_6),
         age_15_19yrs_7 = as.numeric(age_15_19yrs_7)
         ) %>% 
  pivot_longer(cols = c(contains("age_"), total_0_19yrs_8), values_to = "extractions") %>% 
  mutate(name = str_sub(name, end=-3)) %>%
  left_join(la_pop_estimates_1516 %>% 
              pivot_longer(cols = c(contains("age_"), total_0_19yrs_14), values_to = "population_est") %>% 
              mutate(name = str_sub(name, end=-4)),
            by = c("region", "la_code", "la_name", "name")) %>% 
  mutate(year = 1516)

rm(extraction_1516,
   la_pop_estimates_1516
   )

# Union all extractions (long)
extractions_comb <-
  extraction_2021_long %>% 
  union_all(extraction_1920_long) %>%   
  union_all(extraction_1819_long) %>%   
  union_all(extraction_1718_long) %>%   
  union_all(extraction_1617_long) %>%   
  union_all(extraction_1516_long) %>% 
  filter(la_code != "E92000001") %>%  # Remove England region rows 
  mutate(rate_per_100000 = extractions/population_est * 100000) 

rm(extraction_2021_long,
   extraction_1920_long,
   extraction_1819_long,
   extraction_1718_long,
   extraction_1617_long,
   extraction_1516_long
   )

# Write csv
write_csv(extractions_comb, "1_strategic_data_review/data/dental_extractions_la_1516_2021.csv") 
