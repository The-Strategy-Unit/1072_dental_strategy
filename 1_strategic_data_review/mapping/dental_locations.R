# Dental mapping exercise

library(tidyverse)
library(janitor)
library(sf)
library(readxl)
library(leaflet)

# https://rpubs.com/mattdray/basic-leaflet-maps 

# Locations ----
dental_services <- 
  read_csv("1_strategic_data_review/data/reference/dental_services_202303.csv") |> 
  clean_names() |> 
  select(code, name, type, icb_name, easting, northing, nhs_private) |> 
  mutate(code = case_when(type == "Dental Access Centre" ~ paste0("DAC_", row_number()), TRUE ~ code)) |>
  mutate(nhs_private = case_when(nhs_private == "#N/A" & type == "Dental Access Centre" ~ "DAC", TRUE ~ nhs_private))

dental_services_shp <-
  dental_services |> 
  st_as_sf(coords = c("easting", "northing"), crs = 27700) |> # as shape file
  st_transform(4326)

dental_location_leaflet <-
  dental_services_shp |> 
  as_tibble() |> 
  mutate(geometry_chr = as.character(geometry)) |> 
  mutate(longitude  = as.numeric(str_sub(geometry_chr,  3, 19)),
         latitude = as.numeric(str_sub(geometry_chr, 22, 37))
         )

# Sub ICS shp file 
sub_ics_shp <- 
  st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/SICBL_JUL_2022_EN_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  clean_names() |> 
  mutate(sicb_code_2 = sub('.*-', '', sicbl22nm)) # Isolate 3 character sub-icb code

midlands_sub_icbs <- 
  c("18C","15E","15M","71E","03W","04C","04V","04Y","05D","05G","05Q","05V","05W","M2L0M","78H","02Q","52R","D2P2L","B2M3M")

sub_ics_midlands_shp <-
  sub_ics_shp |> 
  filter(str_detect(sicb_code_2, paste(midlands_sub_icbs, collapse = "|")))
  
# Leaflet
leaflet() |> 
  addTiles() |> 
  addPolygons(data = sub_ics_midlands_shp ,
              weight = 1,  
              opacity = 1,  
              color = "black", 
              fillColor = "#5881c1",
              label = ~sicbl22nm  
              ) |> 
  addMarkers(data = dental_location_leaflet, 
             lng = ~longitude, lat = ~latitude,
             #clusterOptions = markerClusterOptions()
             )

# Dental practice deprivation profiles ----

# https://tmieno2.github.io/R-as-GIS-for-Economists/spatial-intersection-transformative-join.html 
# https://r-spatial.github.io/sf/ 

lsoa_shp <- 
  #read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_Dec_2011_Boundaries_Generalised_Clipped_BGC_EW_V3_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_2011_Boundaries_Super_Generalised_Clipped_BSC_EW_V4/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_transform(crs = 27700) |> 
  select(2, geometry) |> 
  mutate(lsoa_geometry = geometry) |> 
  mutate(lsoa_area = st_area(geometry))

# Read in LSOA IMD
lsoa_imd <- 
  read_csv("1_strategic_data_review/data/reference/LSOA_IMD_2019.csv") |> 
  select(lsoa11cd, IMDRank, IMDDecil)

# lsoa lookup
lsoa_ccg <- read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/LSOA_(2011)_CCG(20)_STP(20).csv")
ccg_nhsr <- read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/CCG_STP_NHSER_April_2020.csv")

midlands_lsoas <-
  lsoa_ccg |>
  select(2,4) |> 
  left_join(ccg_nhsr |> 
              select(2, 10), by = "CCG20CD") |> 
  filter(NHSER20NM == "Midlands")

west_midlands_lsoas <-
  lsoa_ccg |>
  select(2,4) |> 
  left_join(ccg_nhsr |> 
              select(2, 5, 10), 
            by = "CCG20CD") |> 
  filter(NHSER20NM == "Midlands") |> 
  filter(STP20CD %in% c("E54000018",
                        "E54000010",
                        "E54000016",
                        "E54000011",
                        "E54000017",
                        "E54000019"))

# Test on single practice
single_practice_location <-
  dental_services_shp |> 
  filter(code == "V82510") |> 
  mutate(point_geometry = geometry, 
         buffer = st_buffer(geometry, 1000)) |> 
  mutate(buffer_geometry = buffer) |> 
  st_transform(crs = 27700)

all_practice_locations <-
  dental_services_shp |> 
  mutate(point_geometry = geometry, 
         buffer = st_buffer(geometry, 1000)) |> 
  mutate(buffer_geometry = buffer) |> 
  st_transform(crs = 27700)

# Check buffer
single_practice_location |> 
  ggplot() +
  geom_sf(aes(geometry = buffer), fill = NA) +
  geom_sf(size = 4, shape = 22, fill = "darkred") 

# Visualise LSOA, practice location and buffer
st_join(lsoa_shp, single_practice_location, 
        # default join is st_intersects - single returns 
        left = FALSE #Excludes LSOA's with no point contained 
        ) |>  
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = NA, colour = "blue") +
  geom_sf(aes(geometry = point_geometry)) +
  geom_sf(aes(geometry = buffer), fill = NA, colour = "red")

# Change join - capture all LSOA's within buffer
st_join(lsoa_shp, 
        single_practice_location |> 
          as_tibble() |> 
          select(-geometry, -point_geometry) |> 
          st_as_sf() |> 
          st_transform(crs = 27700),
        join = st_intersection,
        left = FALSE,
        largest = TRUE
        ) |> 
  # Pull back in point location
  st_join(single_practice_location, join = st_intersects) |> 
  mutate(area = st_area(buffer_geometry.x)) |> 
  
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = NA, colour = "blue") +
  geom_sf(aes(geometry = point_geometry)) +
  geom_sf(aes(geometry = buffer), fill = NA, colour = "red")

# Exclude areas of intersected LSOA's that are outside of buffer
single_practice_intersected_lsoas <-
  st_intersection(lsoa_shp, 
                  single_practice_location |> 
                    as_tibble() |> 
                    select(-geometry, -point_geometry) |> 
                    st_as_sf() |> 
                    st_transform(crs = 27700)
                  ) |> 
  mutate(intersection_area = st_area(geometry)) |> 
  as_tibble() |> 
  mutate(intersection_proportion = as.numeric(intersection_area)/lsoa_area) 

# Check st_intersection join
single_practice_intersected_lsoas |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

# Pull in IMD of lsoa, weight by amount of intersection and summarise practice by weighted IMD of surrounding LSOA's
single_practice_intersected_lsoas |> 
  as_data_frame() |>
  left_join(lsoa_imd, by = c("LSOA11CD" = "lsoa11cd")) |> 
  mutate(imd_decile_weighted = IMDDecil * intersection_proportion) |> 

  group_by(code) |> 
  mutate(n = n()) |> 
  summarise(imd_decile = sum(imd_decile_weighted)/n) |> 
  distinct()

# Broaden to all practices
all_intersected_lsoas <-
  st_intersection(lsoa_shp, 
                  all_practice_locations |> 
                    filter(type != "Dental Access Centre") |> 
                    as_data_frame() |> 
                    select(-geometry, -point_geometry) |> 
                    st_as_sf() |> 
                    st_transform(crs = 27700)
                  ) |> 
  mutate(intersection_area = st_area(geometry)) |> 
  as_data_frame() |> 
  mutate(intersection_proportion = as.numeric(intersection_area)/lsoa_area) 

# Check st_intersection join
all_intersected_lsoas |> 
  filter(code %in% c(
    "V82510",
    "V04832",
    "V04913",
    "V04896",
    "V28583",
    "V04866",
    "V04874",
    "V04361",
    "V28584",
    "V04838" )) |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

all_intersected_lsoas |> 
  ggplot() + 
  geom_sf(aes(geometry = geometry))

# Pull in IMD of lsoa, weight by amount of intersection and summarise practice by weighted IMD of surrounding LSOA's
practice_weighted_imd <-
  all_intersected_lsoas |> 
  as_data_frame() |>
  left_join(lsoa_imd, by = c("LSOA11CD" = "lsoa11cd")) |> 
  mutate(imd_decile_weighted = IMDDecil * intersection_proportion) |> 
  group_by(code) |> 
  mutate(n = n()) |> 
  summarise(imd_decile = sum(imd_decile_weighted, na.rm = T)/n) |> 
  distinct() |> 
  mutate(imd_2 = as.numeric(imd_decile)) |> 
  ungroup()

# Check distribution of weighted IMD
practice_weighted_imd |>
  ggplot(aes(x = imd_2, after_stat(count))) +
  geom_density()

# Plot IMD for reference  
lsoa_shp |> 
  filter(LSOA11CD %in% west_midlands_lsoas$LSOA11CD) |> 
  left_join(lsoa_imd, by = c("LSOA11CD" = "lsoa11cd")) |>
  mutate(IMD_character = as.character(IMDDecil)) |> 
  mutate(IMD_character = factor(IMD_character,
                                levels = c("1","2","3","4","5","6","7","8","9","10"))) |> 

  ggplot() +
  geom_sf(aes(fill = IMD_character), colour = NA) +
  geom_sf(data = all_practice_locations) +
  scale_fill_brewer(palette="Spectral") +
  labs(fill = "IMD Decile")

# Update: Nearest dental practice for each lsoa ---- 
# All lsoas and the nearest practices
lsoa_shp |> 
  filter(LSOA11CD %in% west_midlands_lsoas$LSOA11CD) |> 
  select(1,2) |> 
  st_join(all_practice_locations |> 
            select(1,3,4,5), 
          join = st_nearest_feature
          )

# LSOA's with no practice associated 
lsoa_outside_buffer <-
  lsoa_shp |> 
  filter(LSOA11CD %in% west_midlands_lsoas$LSOA11CD) |> #3,487
  left_join(all_intersected_lsoas |> #11,713
              as_data_frame(),
            by = "LSOA11CD"
            ) |> #12,170
  mutate(fill_case = case_when(is.na(code) ~ "no", TRUE ~ "yes")) #12,170

lsoa_outside_buffer |>
  ggplot() +
  geom_sf(aes(fill = fill_case), colour = NA) +
  geom_sf(data = all_practice_locations) +
  labs(
    fill = 
      "LSOA captured
in buffer")

# Import lsoa population estimates 
lsoa_pop_20 <- 
  read_excel("1_strategic_data_review/data/reference/sape23dt2mid2020lsoasyoaestimatesunformatted.xlsx", 
                          sheet = "Mid-2020 Persons", skip = 4) |> 
  select(1,7) |> 
  clean_names() |> 
  rename(pop_20 = all_ages)

# Isolate lsoa's with no practices (469 lsoa's)
a <-
  lsoa_shp |> 
  filter(LSOA11CD %in% lsoa_outside_buffer$LSOA11CD[lsoa_outside_buffer$fill_case == "no"]) |> 
  #select(1,2) |> 
  st_join(all_practice_locations |> 
            select(1,3,4,5) |> 
            filter(type != "Dental Access Centre"), 
          join = st_nearest_feature
          ) |> 
  left_join(lsoa_imd, by = c("LSOA11CD" = "lsoa11cd"))

# Join to buffers 
b <-
  all_intersected_lsoas |> 
  select(-buffer_geometry, -name) |> 
  as_data_frame() |>
  left_join(lsoa_imd, by = c("LSOA11CD" = "lsoa11cd")) 

# Calculate the number of registrations at each practice by deprivation decile
c <-
  a |> 
  bind_rows(b) |> 
  as_data_frame() |>
  select(LSOA11CD, code, type, icb_name, nhs_private, IMDDecil, intersection_proportion) |> 
  left_join(lsoa_pop_20, by = c("LSOA11CD" = "lsoa_code")) |> 
  mutate(intersection_proportion = as.numeric(intersection_proportion)) |> 
  mutate(intersection_proportion = 
           case_when(is.na(intersection_proportion) ~ 1, 
                     TRUE ~ intersection_proportion
                     )
         ) |> # where intersection is na (i.e. not in any buffer) - weight = 1, all population go to closest dental practice
  mutate(weighted_pop = pop_20 * intersection_proportion) |> 
  group_by(code, IMDDecil) |> 
  summarise(practice_pop = sum(weighted_pop)) |> 
  ungroup()

# Calculate average deprivation by weighted population for each practice
d <-
  c |> 
  group_by(code) |> 
  mutate(pop_proportion = practice_pop / sum(practice_pop)) |>
  mutate(weighted_deprivation = IMDDecil * pop_proportion) |>
  summarise(avg_deprivation = sum(weighted_deprivation)) #905 practices 


# Visualise practices weighted-deprivation scores 
library(patchwork)
(practice_weighted_imd |>
    ggplot(aes(x = imd_2, after_stat(count))) +
    geom_density() +
    ylim(0,400) +
    xlim(0,10) +
    labs(title = "")
  ) +
  (d |>
     ggplot(aes(x = avg_deprivation, after_stat(count))) +
     geom_density() +
     ylim(0,400) +
     xlim(0,10) +
     labs(title = "")
  )


# Write csv listing dental practices with weighted deprivation ----
write_csv(dental_services |>
            left_join(d, by = "code"),
          "1_strategic_data_review/data/reference/dental_services_202303_weighted_deprivation.csv"
          )


# Plot position of practices by nhs vs private - for reference
lsoa_shp |> 
  filter(LSOA11CD %in% west_midlands_lsoas$LSOA11CD) |> 
 
  ggplot() +
  geom_sf(fill = NA, colour = "grey") +
  geom_sf(data = all_practice_locations |> 
            filter(nhs_private != "#N/A") |> 
            mutate(nhs_private = case_when(nhs_private == "D" ~ "1. NHS Dentist",
                                           nhs_private == "P" ~ "2. Private Dentist",
                                           nhs_private == "DAC" ~ "3. DAC")), 
          aes(colour = nhs_private, size = nhs_private)) +
  #theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()
        ) +
  labs(title = "Geographic distribution of dental practice by type", 
       subtitle = "West Midlands | 2023",
       caption = "Note: DAC refers to Dental Access Centre",
       color = "Practice type", 
       size = "Practice type")











  





