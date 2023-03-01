# Dental mapping exercise

library(tidyverse)
library(janitor)
library(sf)
library(readxl)
library(leaflet)

# https://rpubs.com/mattdray/basic-leaflet-maps 

# Locations ----
# Read in contract data and combine
dental_contracts_BC  <- read_excel("1_strategic_data_review/data/Dental contracts/Black Country ICS PC dental list October 2022.xlsx"          , skip = 4) %>% clean_names()
#dental_contracts_BS  <- read_excel("1_strategic_data_review/data/Dental contracts/Birmingham and Solihull ICS PC dental list October 2022.xlsx", skip = 4) %>% clean_names()
dental_contracts_BS  <- read_excel("1_strategic_data_review/data/Dental contracts/BS ICS PC dental list.xlsx", skip = 4) %>% clean_names()
dental_contracts_CW  <- read_excel("1_strategic_data_review/data/Dental contracts/Coventry and Warwick ICS PC dental list October 2022.xlsx"   , skip = 4) %>% clean_names()
dental_contracts_HW  <- read_excel("1_strategic_data_review/data/Dental contracts/Hereford and Worcs ICS PC dental list October 2022.xlsx"     , skip = 4) %>% clean_names()
dental_contracts_STW <- read_excel("1_strategic_data_review/data/Dental contracts/Shropshire TW ICS PC dental list October 2022.xlsx"          , skip = 4) %>% clean_names()
dental_contracts_SHR <- read_excel("1_strategic_data_review/data/Dental contracts/Staffordshire ICS PC dental list October 2022..xlsx"         , skip = 4) %>% clean_names()

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
  read_csv("1_strategic_data_review/data/reference/postcode_point_lookup.csv") %>% 
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
  dental_location_map %>% 
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
             #clusterOptions = markerClusterOptions()
             )


# Dental practice deprivation profiles ----

# https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-intersection-transformative-join.html 
# https://r-spatial.github.io/sf/ 

lsoa_shp <- 
  read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_2021_EW_BSC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") %>% 
  st_transform(crs = 27700) %>% 
  select(LSOA21CD, geometry) %>% 
  mutate(lsoa_geometry = geometry) %>% 
  mutate(lsoa_area = st_area(geometry))

# Read in LSOA IMD
lsoa_imd <- 
  read_csv("1_strategic_data_review/data/reference/LSOA_IMD_2019.csv") %>% 
  select(lsoa11cd, IMDRank, IMDDecil)

# lsoa lookup
lsoa_ccg <- read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/LSOA_(2011)_CCG(20)_STP(20).csv")
ccg_nhsr <- read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/CCG_STP_NHSER_April_2020.csv")

midlands_lsoas <-
  lsoa_ccg %>%
  select(2,4) %>% 
  left_join(ccg_nhsr %>% 
              select(2, 10), by = "CCG20CD") %>% 
  filter(NHSER20NM == "Midlands")

west_midlands_lsoas <-
  lsoa_ccg %>%
  select(2,4) %>% 
  left_join(ccg_nhsr %>% 
              select(2, 5, 10), 
            by = "CCG20CD") %>% 
  filter(NHSER20NM == "Midlands") %>% 
  filter(STP20CD %in% c("E54000018",
                        "E54000010",
                        "E54000016",
                        "E54000011",
                        "E54000017",
                        "E54000019"))

# Test on single practice
single_practice_location <-
  dental_location_leaflet %>%
  select(1,2,6) %>% 
  filter(contract_information %in% c(1001760000)) %>% 
  mutate(point_geometry = geometry, 
         buffer = st_buffer(geometry, 1000)) %>% 
  mutate(buffer_geometry = buffer) %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700)

all_practice_locations <-
  dental_location_leaflet %>% 
  select(1,2,6) %>% 
  mutate(point_geometry = geometry, 
         buffer = st_buffer(geometry, 1000)) %>% 
  mutate(buffer_geometry = buffer) %>% 
  st_as_sf() %>% 
  st_transform(crs = 27700)

# Check buffer
single_practice_location %>% 
  ggplot() +
  geom_sf(aes(geometry = buffer), fill = NA) +
  geom_sf(size = 4, shape = 22, fill = "darkred") 

# Visualise LSOA, practice location and buffer
st_join(lsoa_shp, single_practice_location, 
        # default join is st_intersects - single returns 
        left = FALSE #Excludes LSOA's with no point contained 
        ) %>%  
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = NA, colour = "blue") +
  geom_sf(aes(geometry = point_geometry)) +
  geom_sf(aes(geometry = buffer), fill = NA, colour = "red")

# Change join - capture all LSOA's within buffer
st_join(lsoa_shp, 
        single_practice_location %>% 
          as_tibble() %>% 
          select(-geometry, -point_geometry) %>% 
          st_as_sf() %>% 
          st_transform(crs = 27700),
        join = st_intersection,
        left = FALSE,
        largest = TRUE
        ) %>% 
  # Pull back in point location
  st_join(single_practice_location, join = st_intersects) %>% 
  mutate(area = st_area(buffer_geometry.x)) %>% 
  
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = NA, colour = "blue") +
  geom_sf(aes(geometry = point_geometry)) +
  geom_sf(aes(geometry = buffer), fill = NA, colour = "red")

# Exclude areas of intersected LSOA's that are outside of buffer
single_practice_intersected_lsoas <-
  st_intersection(lsoa_shp, 
                  single_practice_location %>% 
                    as_data_frame() %>% 
                    select(-geometry, -point_geometry) %>% 
                    st_as_sf() %>% 
                    st_transform(crs = 27700)
                  ) %>% 
  mutate(intersection_area = st_area(geometry)) %>% 
  as_data_frame() %>% 
  mutate(intersection_proportion = as.numeric(intersection_area)/lsoa_area) 

# Check st_intersection join
single_practice_intersected_lsoas %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

# Pull in IMD of lsoa, weight by amount of intersection and summarise practice by weighted IMD of surrounding LSOA's
single_practice_intersected_lsoas %>% 
  as_data_frame() %>%
  left_join(lsoa_imd, by = c("LSOA21CD" = "lsoa11cd")) %>% 
  mutate(imd_decile_weighted = IMDDecil * intersection_proportion) %>% 
  
  group_by(contract_information) %>% 
  mutate(n = n()) %>% 
  summarise(imd_decile = sum(imd_decile_weighted)/n) %>% 
  distinct()

# Broaden to all practices
all_intersected_lsoas <-
  st_intersection(lsoa_shp, 
                  all_practice_locations %>% 
                    as_data_frame() %>% 
                    select(-geometry, -point_geometry) %>% 
                    st_as_sf() %>% 
                    st_transform(crs = 27700)
                  ) %>% 
  mutate(intersection_area = st_area(geometry)) %>% 
  as_data_frame() %>% 
  mutate(intersection_proportion = as.numeric(intersection_area)/lsoa_area) 

# Check st_intersection join
all_intersected_lsoas %>% 
  filter(contract_information %in% c(
    "1001620000",
    "1001640000",
    "1001760000",
    "1002670000",
    "1003980000",
    "1004000000",
    "1004040000",
    "1004050000",
    "1004590000",
    "1004630000" )) %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

all_intersected_lsoas %>% 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

# Pull in IMD of lsoa, weight by amount of intersection and summarise practice by weighted IMD of surrounding LSOA's
practice_weighted_imd <-
  all_intersected_lsoas %>% 
  as_data_frame() %>%
  left_join(lsoa_imd, by = c("LSOA21CD" = "lsoa11cd")) %>% 
  mutate(imd_decile_weighted = IMDDecil * intersection_proportion) %>% 
  group_by(contract_information) %>% 
  mutate(n = n()) %>% 
  summarise(imd_decile = sum(imd_decile_weighted, na.rm = T)/n) %>% 
  distinct() %>% 
  mutate(imd_2 = as.numeric(imd_decile)) 

# Check distribution of weighted IMD
practice_weighted_imd %>%
  ggplot(aes(x = imd_2, after_stat(count))) +
  geom_density()

# Plot IMD for reference  
lsoa_shp %>% 
  filter(LSOA21CD %in% west_midlands_lsoas$LSOA11CD) %>% 
  left_join(lsoa_imd, by = c("LSOA21CD" = "lsoa11cd")) %>%
  mutate(IMD_character = as.character(IMDDecil)) %>% 
  mutate(IMD_character = factor(IMD_character,
                                levels = c("1","2","3","4","5","6","7","8","9","10"))) %>% 

  ggplot() +
  geom_sf(aes(fill = IMD_character), colour = NA) +
  geom_sf(data = all_practice_locations) +
  scale_fill_brewer(palette="Spectral")


# Nearest dental practice for each lsoa ---- 
# All lsoas and the nearest practices
lsoa_shp %>% 
  filter(LSOA21CD %in% west_midlands_lsoas$LSOA11CD) %>% 
  select(1,2) %>% 
  st_join(all_practice_locations %>% 
            select(1,3,4,5), 
          join = st_nearest_feature
          )

# LSOA's with no practice associated 
lsoa_outside_buffer <-
  lsoa_shp %>% 
  filter(LSOA21CD %in% west_midlands_lsoas$LSOA11CD) %>% #3,396
  #select(1,2) %>% 
  left_join(all_intersected_lsoas %>% #10,037
              as_data_frame() %>%
              select(LSOA21CD, contract_information, intersection_proportion)
            ,
            by = "LSOA21CD"
            ) %>% #10,180
  mutate(fill_case = case_when(is.na(contract_information) ~ "no", TRUE ~ "yes")) 

lsoa_outside_buffer %>%
  ggplot() +
  geom_sf(aes(fill = fill_case), colour = NA) +
  geom_sf(data = all_practice_locations)



# Isolate lsoa's with no practices
a <-
  lsoa_shp %>% 
  filter(LSOA21CD %in% lsoa_outside_buffer$LSOA21CD[lsoa_outside_buffer$fill_case == "no"]) %>% #725 lsoa's
  #select(1,2) %>% 
  st_join(all_practice_locations %>% 
            select(1,3,4,5), 
          join = st_nearest_feature
          ) %>% 
  left_join(lsoa_imd, by = c("LSOA21CD" = "lsoa11cd"))


# Join to buffers 
b <-
  all_intersected_lsoas %>% 
  as_data_frame() %>%
  left_join(lsoa_imd, by = c("LSOA21CD" = "lsoa11cd")) 


# Link to IMD - group and summarise
c <-
  a %>% 
  union_all(b) %>% 
  as_data_frame() %>%
  select(LSOA21CD, contract_information, IMDDecil, intersection_proportion) %>% 
  mutate(intersection_proportion = as.numeric(intersection_proportion)) %>% 
  mutate(intersection_proportion = 
           case_when(is.na(intersection_proportion) ~ 1, 
                     TRUE ~ intersection_proportion
                     )
         ) %>% # where intersection is na (i.e. not in any buffer) - weight = 1, all population go to closest dental practice
  mutate(imd_decile_weighted = IMDDecil * intersection_proportion) %>%  
  group_by(contract_information) %>% 
  mutate(n = n()) %>% 
  summarise(imd_decile = sum(imd_decile_weighted, na.rm = T)/n) %>% 
  distinct()  


# Visualise practices weighted-deprivation scores 
library(patchwork)
(practice_weighted_imd %>%
    ggplot(aes(x = imd_2, after_stat(count))) +
    geom_density() +
    ylim(0,300) +
    xlim(0,7) +
    labs(title = "")
  ) +
  (c %>%
     ggplot(aes(x = imd_decile, after_stat(count))) +
     geom_density() +
     ylim(0,300) +
     xlim(0,7)
  )













  





