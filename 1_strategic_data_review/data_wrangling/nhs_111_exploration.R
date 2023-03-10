# NHS 111 data exploration ----
library(tidyverse)
library(janitor)
library(lubridate)
library(scales)


# Set SU theme ####
SU_colours <- c (
  `orange`                     = grDevices::rgb(248,191,7, maxColorValue = 255),# "#f9bf07",
  `charcoal`                   = grDevices::rgb(44,40,37, maxColorValue = 255),# "#2c2825",
  `slate`                      = grDevices::rgb(104,111,115, maxColorValue = 255), # "#686f73",
  `blue`                       = grDevices::rgb(88,29,193, maxColorValue = 255), # "#5881c1",
  `red`                        = grDevices::rgb(236,101,85, maxColorValue = 255), # "#ec6555",
  #additional accent colours from word doc template
  `yellow`                     = grDevices::rgb(252,229,155, maxColorValue = 255),
  `grey`                       = grDevices::rgb(163,168,172, maxColorValue = 255),
  `white`                      = grDevices::rgb(255,255,255, maxColorValue = 255),
  #light and dark ends from colour theme in word doc
  `light orange`               = grDevices::rgb(253,242,205, maxColorValue = 255),
  `dark orange`                = grDevices::rgb(124,95,3, maxColorValue = 255),
  `light charcoal`             = grDevices::rgb(235,233,231, maxColorValue = 255),
  `dark charcoal`              = 	"#000000",#black
  `light slate`                = grDevices::rgb(224,226,227, maxColorValue = 255),
  `dark slate`                 = grDevices::rgb(51,55,57, maxColorValue = 255),
  `light blue`                 = grDevices::rgb(221,229,242, maxColorValue = 255),
  `dark blue`                  = grDevices::rgb(38,61,102, maxColorValue = 255),
  `light red`                  = grDevices::rgb(251,224,220, maxColorValue = 255),
  `dark red`                   = grDevices::rgb(144,29,16, maxColorValue = 255),
  `light yellow`               = grDevices::rgb(254,249,235, maxColorValue = 255),
  `dark yellow`                = grDevices::rgb(197,152,5, maxColorValue = 255),
  `light grey`                 = grDevices::rgb(236,237,238, maxColorValue = 255),
  `dark grey`                  = grDevices::rgb(79,84,88, maxColorValue = 255),
  `light white`                = grDevices::rgb(242,242,242, maxColorValue = 255),
  `dark white`                 = grDevices::rgb(127,127,127, maxColorValue = 255),
  `red2`                       = grDevices::rgb(215,25,28, maxColorValue = 255),
  `orange2`                    = grDevices::rgb(253,174,97, maxColorValue = 255),
  `yellow2`                    = grDevices::rgb(255,255,191, maxColorValue = 255),
  `green2`                     = grDevices::rgb(171,221,164, maxColorValue = 255),
  `blue2`                      = grDevices::rgb(43,131,186, maxColorValue = 255) #"#2b83ba"
)

SU_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (SU_colours)
  
  SU_colours[cols]
}

SU_palettes <- list(
  `main` = SU_cols("orange","charcoal","slate","blue","red"),
  `oranges` = SU_cols("light orange","orange","dark orange"),
  `slates` = SU_cols("light slate","slate","dark slate"),
  `mixed` = SU_cols("dark red","orange","yellow","light blue","slate"),
  `oj_coal` = SU_cols("yellow","orange","red","dark red","dark charcoal"),
  `oj_red` = SU_cols("yellow","orange","red","dark red"),
  `white_oj_coal` = SU_cols("white","yellow","orange","red","dark red","dark charcoal"),#added since shared
  `lyellow_oj_coal` = SU_cols("light yellow","orange","red","dark red","dark charcoal"),#added since shared
  `wy_oj_coal` = SU_cols("white","light yellow","yellow","orange","red","dark red","charcoal","dark charcoal"),
  `red_coal` = SU_cols("red","dark red","charcoal","dark charcoal"),
  `blue_yellow_red` = SU_cols("red2","orange2","yellow2","green2","blue2"),
  `red_yellow_blue` = SU_cols("blue2","green2","yellow2","orange2","red2")
)


SU_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- SU_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


scale_color_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_SU <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- SU_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("SU_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}  

theme_SU <-   function (base_size){
  theme_minimal(
    #base_family = "Segoe UI", 
    base_size=12
  ) %+replace% 
    theme(axis.title = element_text(size=11, face="bold",colour=SU_cols("charcoal")),
          plot.title = element_text(hjust=0,face="bold",size=12,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.subtitle = element_text(hjust=0,face="italic",size=10,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")),
          plot.caption = element_text(hjust = 0,face="italic",size=9,colour=SU_cols("slate"),margin=margin(b=4,unit="pt")),
          legend.text = element_text(size=10,colour=SU_cols("charcoal")),
          legend.title = element_text(face="bold",size=11,colour=SU_cols("charcoal"),margin=margin(b=4,unit="pt")))
}

theme_set(theme_SU())

# Read in raw data ----

# Look up(s)
sympton_group_lookup <- 
  read_csv("1_strategic_data_review/data/reference/nhs_111_sympton_group_lookup.csv") |> 
  clean_names()

# Data
## Data includes Midlands commissioners & Data filtered on symptom code
nhs_111_raw <- 
  read_csv("1_strategic_data_review/data/NHS_111_raw/NHS_111_data_midlands.csv") |> 
  clean_names() |> 
  mutate(month = as.Date(paste0(str_sub(call_connect_date, 1, 7),"-01"))) |> 
  filter(stp_code %in% c(
    "QHL",      # NHS BIRMINGHAM AND SOLIHULL INTEGRATED CARE BOARD              
    "QOC",      # NHS SHROPSHIRE, TELFORD AND WREKIN INTEGRATED CARE BOARD       
    "QUA",      # NHS BLACK COUNTRY INTEGRATED CARE BOARD                        
    "QWU",      # NHS COVENTRY AND WARWICKSHIRE INTEGRATED CARE BOARD            
    "QNC",      # NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD     
    "QGH"       # NHS HEREFORDSHIRE AND WORCESTERSHIRE INTEGRATED CARE BOARD   
    )) |> 
  left_join(sympton_group_lookup, by = c("symptom_group" = "sg_sd"))





# Time series ----
# April 2015 - Jan 2022

# By ICB
nhs_111_raw |> 
  group_by(month, stp_name) |> 
  summarise(call_count = n_distinct(nhse_guid)) |> 
  filter(month > "2017-01-01") |> 
  ggplot(aes(x = month, y = call_count)) +
  geom_point(fill = "#2c2825", alpha = 0.5) +
  geom_smooth(method = 'loess', span = 0.2, colour = "#f9bf07") +
  facet_wrap(~str_wrap(stp_name, 30)) +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text =  element_text(face = "bold")
        ) +
  labs(x = "Month",
       y = "Monthly call count",
       title = "NHS 111 calls for dental symptoms",
       subtitle = "West Midlands ICB | 2017-22 ")

# By sympton group
nhs_111_raw |> 
  group_by(month, sg_description) |> 
  #summarise(call_count = n())  |> 
  summarise(call_count = n_distinct(nhse_guid)) |> 
  filter(month > "2017-01-01") |> 
  ggplot(aes(x = month, y = call_count)) +
  geom_point(fill = "#2c2825", alpha = 0.5) +
  geom_smooth(method = 'loess', span = 0.2, colour = "#f9bf07") +
  facet_wrap(~str_wrap(sg_description, 30), scales = "free"
             ) +
  scale_y_continuous(labels = comma, oob = squish) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text =  element_text(face = "bold")
        ) +
  labs(x = "Month",
       y = "Monthly call count",
       title = "NHS 111 calls for dental symptoms by sympton group",
       subtitle = "West Midlands | 2017-22 ")


# By LSOA

# lsoa lookup
library(sf)
lsoa_shp <- 
  read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA_2011_Boundaries_Super_Generalised_Clipped_BSC_EW_V4/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_transform(crs = 27700) |> 
  select(2, geometry) |> 
  clean_names()

lsoa_ccg <- read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/LSOA_(2011)_CCG(20)_STP(20).csv")
ccg_nhsr <- read_csv("C:/Users/alexander.lawless/OneDrive - Midlands and Lancashire CSU/Data_management/CCG_STP_NHSER_April_2020.csv")

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
  
# Map calls by LSOA
lsoa_shp |> 
  filter(lsoa11cd %in% west_midlands_lsoas$LSOA11CD) |> 
  left_join(
    nhs_111_raw |> 
      group_by(dmic_lsoa_2011) |> 
      summarise(call_count = n_distinct(nhse_guid)) |> 
      filter(dmic_lsoa_2011 != "NULL") |> 
      mutate(call_count_range = 
               case_when(call_count < 25 ~ "<25",
                         call_count >= 25 & call_count < 50 ~ "25-49",
                         call_count >= 50 & call_count < 75 ~ "50-74",
                         call_count >= 75 & call_count < 100 ~ "75-99",
                         call_count >= 100 & call_count < 125 ~ "100-124",
                         call_count >= 125 & call_count < 150 ~ "125-149",
                         call_count >= 150 & call_count < 175 ~ "150-174",
                         call_count >= 175 & call_count < 200 ~ "175-199",
                         call_count >= 200 ~ ">200"
               )) |> 
      mutate(call_count_range = 
               factor(call_count_range, 
                      levels = c(
                        "<25",
                        "25-49",
                        "50-74",
                        "75-99",
                        "100-124",
                        "125-149",
                        "150-174",
                        "175-199",
                        ">200"
                      ))),
    by = c("lsoa11cd" = "dmic_lsoa_2011")
    ) |>
  
  ggplot() +
  geom_sf(aes(fill = call_count_range), colour = NA) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()
        ) +
  labs(x = "", y = "",
       title = "NHS 111 calls for dental symptoms by LSOA",
       subtitle = "West Midlands commissioners | 2015-22",
       fill = "Call count")


# Descriptive table
nhs_111_raw |> 
  mutate(year = year(month)) |> 
  group_by(year) |> 
  summarise(Calls = n_distinct(nhse_guid), 
            Individuals = n_distinct(der_pseudo_number)
            ) |> 
  pivot_longer(cols = -year) |> 
  
  ggplot(aes(x = year, y = value, fill = name)) +
  geom_col() +
  facet_wrap(~name) +
  scale_y_continuous(labels = comma) +
  scale_fill_SU() +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "none"
        ) +
  labs(x = "Year", y = "",
       title = "NHS 111 annual call activity",
       subtitle = "West Midlands | 2015-22")

# # A tibble: 8 × 4
# year  rows calls individuals
# <dbl> <int> <int>       <int>
# 1  2015   110   110          79
# 2  2016  5682  5682        4886
# 3  2017 31419 31419       26444
# 4  2018 24760 24760       21296
# 5  2019 21802 21802       18209
# 6  2020 67063 67063       53372
# 7  2021 38561 38561       31041
# 8  2022  1806  1806        1528



  





