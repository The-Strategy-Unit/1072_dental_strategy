# Dental mapping exercise

library(tidyverse)
library(janitor)
library(sf)
library(readxl)
library(leaflet)

# https://rpubs.com/mattdray/basic-leaflet-maps 




# Read in contract data and combine
dental_contracts_BC  <- read_excel("Dental contracts/Black Country ICS PC dental list October 2022.xlsx"          , skip = 4) %>% clean_names()
dental_contracts_BS  <- read_excel("Dental contracts/Birmingham and Solihull ICS PC dental list October 2022.xlsx", skip = 4) %>% clean_names()
dental_contracts_CW  <- read_excel("Dental contracts/Coventry and Warwick ICS PC dental list October 2022.xlsx"   , skip = 4) %>% clean_names()
dental_contracts_HW  <- read_excel("Dental contracts/Hereford and Worcs ICS PC dental list October 2022.xlsx"     , skip = 4) %>% clean_names()
dental_contracts_STW <- read_excel("Dental contracts/Shropshire TW ICS PC dental list October 2022.xlsx"          , skip = 4) %>% clean_names()
dental_contracts_SHR <- read_excel("Dental contracts/Staffordshire ICS PC dental list October 2022..xlsx"          , skip = 4) %>% clean_names()

feilds <- c("contract_information", "post_code", "icb_code", "uda_annual_contracted")

dental_contracts_comb <-
  dental_contracts_BC %>% 
  select(feilds) %>% 
  union_all(dental_contracts_BS %>% select(feilds)) %>% 
  union_all(dental_contracts_CW %>% select(feilds)) %>% 
  union_all(dental_contracts_HW %>% select(feilds)) %>% 
  union_all(dental_contracts_STW %>% select(feilds)) %>% 
  union_all(dental_contracts_SHR %>% select(feilds)) 

rm(
  "dental_contracts_BC",
  "dental_contracts_BS",
  "dental_contracts_CW",
  "dental_contracts_HW",
  "dental_contracts_STW",
  "dental_contracts_SHR",
  feilds
  )
  
postcode_lookup <- 
  read_csv("reference_data/postcode_point_lookup.csv") %>% 
  clean_names()

# Postcodes to point locations
dental_locations <-
  dental_contracts_comb %>% 
  mutate(post_code_2 = str_replace_all(post_code, " ", "")) %>% 
  left_join(postcode_lookup %>% select(postcode, eastings, northings), by = c("post_code" = "postcode")) %>% # left join on postcode with spaces
  left_join(postcode_lookup %>% select(postcode, eastings, northings) , by = c("post_code_2" = "postcode")) %>%  # left join on postcode without spaces
  mutate(eastings = case_when(!is.na(eastings.x) ~ eastings.x, TRUE ~ eastings.y),
         northings = case_when(!is.na(northings.x) ~ northings.x, TRUE ~ northings.y)) %>% # select where not NA
  select(-contains(c(".x", ".y"))) 

# List to shape file
dental_location_map <-
  dental_locations %>% 
  drop_na(eastings) %>% 
  st_as_sf(coords = c("eastings", "northings"), crs = 27700) %>% # as shape file
  st_transform(4326) # geometry from eastings/northings to lng/lat

dental_location_leaflet <-
  dental_location_map%>% 
  as_tibble() %>% 
  mutate(geometry_chr = as.character(geometry)) %>% 
  mutate(longitude  = as.numeric(str_sub(geometry_chr,  3, 19)),
         latitude = as.numeric(str_sub(geometry_chr, 22, 37))
         )

# Sub ICS shp file 
sub_ics_shp <- 
  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/SICBL_JUL_2022_EN_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  clean_names() %>% 
  mutate(sicb_code_2 = sub('.*-', '', sicbl22nm)) # Isolate 3 character sub-icb code

midlands_sub_icbs <- 
  c("18C","15E","15M","71E","03W","04C","04V","04Y","05D","05G","05Q","05V","05W","M2L0M","78H","02Q","52R","D2P2L","B2M3M")

sub_ics_midlands_shp <-
  sub_ics_shp %>% 
  filter(str_detect(sicb_code_2, paste(midlands_sub_icbs, collapse = "|")))
  

# Leaflet
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = sub_ics_midlands_shp ,
              weight = 1,  
              opacity = 1,  
              color = "black", 
              fillColor = "#5881c1",
              label = ~sicbl22nm  
              ) %>% 
  addMarkers(data = dental_location_leaflet, 
             lng = ~longitude, lat = ~latitude,
             clusterOptions = markerClusterOptions()
             )



