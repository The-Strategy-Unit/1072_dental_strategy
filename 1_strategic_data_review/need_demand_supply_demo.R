# Demonstrate need and demand analysis 
library(tidyverse)
library(janitor)
library(readxl)

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

# Read in data ----
ndep_adults_2018 <- 
  read_excel("1_strategic_data_review/data/ndep_raw/NDEP_for_England_OH_Survey_Adults_in_Practice_2018_Results.xlsx", 
             sheet = "AiP Regions Upper Tier LAs", 
             skip = 4) |> 
  clean_names() |> 
  select(-x20, -x43) |> 
  filter(!is.na(upper_tier_la_code)) |> 
  mutate(upper_tier_la_name = 
           case_when(
             upper_tier_la_name ==  "Nottinghamshire (No data for Ashfield, Bassetlaw, Rushcliffe)" ~ "Nottinghamshire",
             upper_tier_la_name == "Essex (No data for Basildon, Brentwood, Castle Point, Colchester, Harlow, Rochford, Uttlesford)" ~ "Essex",
             upper_tier_la_name == "Norfolk (No data for Great Yarmouth)" ~ "Norfolk",                   
             upper_tier_la_name == "Suffolk (No data for Forest Heath, Waveney)" ~ "Suffolk",                     
             upper_tier_la_name == "Cumbria (No data for Eden)" ~ "Cumbria",                     
             upper_tier_la_name == "Devon (No data for East Devon, Mid Devon, South Hams, Torridge, West Devon)" ~ "Devon",                    
             upper_tier_la_name == "Worcestershire (No data for Redditch)" ~ "Worcestershire",
             TRUE ~ upper_tier_la_name
             )
         ) 


# Compare need and supply of care - any treatment needs vs provision of clinical examinations ----
any_treatment_needs <-
  ndep_adults_2018 |> 
  select(1:3, percent_with_any_treatment_need) |> 
  left_join(
    ndep_adults_2018 |> 
      select(2, contains("number_of_complete_clinical_examinations")),
    by = "upper_tier_la_code"
    ) |> 
  distinct()

# Visualise 
any_treatment_needs |>
  filter(area_type != "Regions",
         area_type != "Country") |> 
  mutate(fill_case = case_when(area_type != "West Midlands" ~ "Other LA", TRUE ~ area_type)) |> 
  drop_na(percent_with_any_treatment_need) |> 
  ggplot(aes(y = reorder(upper_tier_la_name, percent_with_any_treatment_need), 
             x = percent_with_any_treatment_need, 
             fill = fill_case)) +
  geom_col() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(x = "Percent with any treatment need", 
       y = "Upper tier local authority",
       title = "Need: Percent of population with any dental treatment need",
       subtitle = "National Dental Epidemiology Programme (NDEP) data | 16 years and older | 2018",
       caption = "Note: Missing local authorities did not participate in survey",
       fill = ""
       )
  
  

# NDEP ICB summary
ndep_icb_summary <- 
  read_csv("1_strategic_data_review/data/ndep_icb_summary.csv") |> 
  pivot_longer(cols = c(-area_code, -area_name))

ndep_icb_summary_england <-
  ndep_icb_summary |>  
  filter(area_name == "England")
  
# Visualise percentages
ndep_icb_summary |>  
  filter(area_name != "England") |> 
  left_join(ndep_icb_summary_england |> 
              select(3,4) |> 
              rename(england_value = value), 
            by = "name") |> 
  
  #ggplot(aes(x = value, y = name)) +
  ggplot(aes(x = value, y = area_name )) +
  geom_col(fill = "#f9bf07", width = 0.5) +
  #facet_grid(~str_wrap(area_name, 20)) +
  facet_grid(~str_wrap(name,20)) +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        )



ndep_icb_summary |>  
  filter(area_name != "England") |> 
  left_join(ndep_icb_summary_england |> 
              select(3,4) |> 
              rename(england_value = value), 
            by = "name") |> 
  filter(name == "3-year olds with dental caries") |> 
  
  ggplot(aes(y = area_name, x = value)) +
  geom_col()
  


# z-score
ndep_icb_summary_zscore <-
  ndep_icb_summary |>
  filter(area_name != "England",
         area_name != "Midlands") |> 
  group_by(name) |> 
  mutate(z_score = (value-mean(value, na.rm = T))/sd(value, na.rm = T)) |> 
  mutate(direction = case_when(z_score <  0 ~ "Below avg", 
                               z_score >= 0 ~ "Above avg"
                               )) |> 
  drop_na(z_score) |> 
  ungroup() |> 
  mutate(area_name = str_remove_all(area_name, " Integrated Care System"))


ndep_icb_summary_zscore |> 
  ggplot(aes(x = z_score, y = name,20, colour = direction)) +
  geom_vline(xintercept = 0) +
  geom_segment(aes(y = name, yend = name, x = 0, xend = z_score)) +
  geom_point(size = 5) +
  facet_grid(~str_wrap(area_name, 20)) +
  scale_color_manual(values = c("#ec6555", "#f9bf07")) + 
  #scale_color_SU() +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom"
        ) +
  labs(x = "Z-score", y = "Indicator", 
       title = "Key dental health indicators by ICB", 
       subtitle = "National Dental Epidemiology Programme (NDEP) data | 3-12 years ",
       colour = ""
       )

ndep_icb_summary_zscore


# Read in supply (activity) data ---- 

NHS_digital_Activity_ICB <- 
  read_csv("1_strategic_data_review/data/NHS_digital_Activity_ICB.csv") |> 
  clean_names() |> 
  select(-x1)

NHS_digital_Population_data <- 
  read_csv("1_strategic_data_review/data/NHS_digital_Population_data.csv") |> 
  clean_names() |> 
  select(-x1)

icb_lookup <-
  tribble(
    ~Sub_ICB_ODS_Code, ~Sub_ICB_Name, ~ICB_Code, ~ICB_name,
    
    "18C",   "NHS HEREFORDSHIRE AND WORCESTERSHIRE ICB - 18C","QGH", "NHS HEREFORDSHIRE AND WORCESTERSHIRE INTEGRATED CARE BOARD",
    "15E",   "NHS BIRMINGHAM AND SOLIHULL ICB - 15E","QHL", "NHS BIRMINGHAM AND SOLIHULL INTEGRATED CARE BOARD",
    "15M",   "NHS DERBY AND DERBYSHIRE ICB - 15M","QJ2", "NHS DERBY AND DERBYSHIRE INTEGRATED CARE BOARD",
    "71E",   "NHS LINCOLNSHIRE ICB - 71E","QJM", "NHS LINCOLNSHIRE INTEGRATED CARE BOARD",
    "03W",   "NHS LEICESTER, LEICESTERSHIRE AND RUTLAND ICB - 03W","QK1", "NHS LEICESTER, LEICESTERSHIRE AND RUTLAND INTEGRATED CARE BOARD",
    "04C",   "NHS LEICESTER, LEICESTERSHIRE AND RUTLAND ICB - 04C","QK1", "NHS LEICESTER, LEICESTERSHIRE AND RUTLAND INTEGRATED CARE BOARD",
    "04V",   "NHS LEICESTER, LEICESTERSHIRE AND RUTLAND ICB - 04V","QK1", "NHS LEICESTER, LEICESTERSHIRE AND RUTLAND INTEGRATED CARE BOARD",
    "04Y",   "NHS STAFFORDSHIRE AND STOKE-ON-TRENT ICB - 04Y","QNC", "NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD",
    "05D",   "NHS STAFFORDSHIRE AND STOKE-ON-TRENT ICB - 05D","QNC", "NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD",
    "05G",   "NHS STAFFORDSHIRE AND STOKE-ON-TRENT ICB - 05G","QNC", "NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD",
    "05Q",   "NHS STAFFORDSHIRE AND STOKE-ON-TRENT ICB - 05Q","QNC", "NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD",
    "05V",   "NHS STAFFORDSHIRE AND STOKE-ON-TRENT ICB - 05V","QNC", "NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD",
    "05W",   "NHS STAFFORDSHIRE AND STOKE-ON-TRENT ICB - 05W","QNC", "NHS STAFFORDSHIRE AND STOKE-ON-TRENT INTEGRATED CARE BOARD",
    "M2L0M", "NHS SHROPSHIRE, TELFORD AND WREKIN ICB - M2L0M","QOC", "NHS SHROPSHIRE, TELFORD AND WREKIN INTEGRATED CARE BOARD",
    "78H",   "NHS NORTHAMPTONSHIRE ICB - 78H","QPM", "NHS NORTHAMPTONSHIRE INTEGRATED CARE BOARD",
    "02Q",   "NHS NOTTINGHAM AND NOTTINGHAMSHIRE ICB - 02Q","QT1", "NHS NOTTINGHAM AND NOTTINGHAMSHIRE INTEGRATED CARE BOARD",
    "52R",   "NHS NOTTINGHAM AND NOTTINGHAMSHIRE ICB - 52R","QT1", "NHS NOTTINGHAM AND NOTTINGHAMSHIRE INTEGRATED CARE BOARD",
    "D2P2L", "NHS BLACK COUNTRY ICB - D2P2L","QUA", "NHS BLACK COUNTRY INTEGRATED CARE BOARD",
    "B2M3M", "NHS COVENTRY AND WARWICKSHIRE ICB - B2M3M","QWU", "NHS COVENTRY AND WARWICKSHIRE INTEGRATED CARE BOARD"
  ) |> 
  clean_names()

west_midlands_icb_codes <- c("QGH", "QHL", "QNC", "QUA", "QWU", "QOC")
 
midlands_icb_delivery <-
  NHS_digital_Activity_ICB |> 
  filter(str_sub(date, 1,4) == 2021) |>
  filter(patient_type == "Child") |>
  
  left_join(icb_lookup, by = c("sub_icb_code" = "sub_icb_ods_code")) |> 
  drop_na(icb_name) |> 
  group_by(sub_icb_code, icb_name) |> 
  summarise(cot = sum(cot, na.rm = T),
            uda = sum(uda, na.rm = T)) |> 
  left_join(NHS_digital_Population_data |> 
              filter(patient_type == "Child", 
                     date == "2021-03-31"), 
            by = c("sub_icb_code")) |> 
  group_by(icb_name) |> 
  summarise(cot = sum(cot, na.rm = T),
            uda = sum(uda, na.rm = T),
            pop = sum(pop, na.rm = T)
            ) |>
  mutate(uda_rate = uda/pop * 1000) |> 
  mutate(z_score = (uda_rate-mean(uda_rate, na.rm = T))/sd(uda_rate, na.rm = T)) |> 
  mutate(direction = case_when(z_score <  0 ~ "Below avg", 
                               z_score >= 0 ~ "Above avg"
                               ))

# Visualise supply rate
midlands_icb_delivery |> 
  filter(icb_name %in% icb_lookup$icb_name[icb_lookup$icb_code %in% west_midlands_icb_codes]) |> 
  mutate(icb_label = str_remove_all(icb_name, "INTEGRATED CARE BOARD")) |> 
  mutate(icb_label = str_remove_all(icb_label, "NHS ")) |> 
  
  ggplot(aes(y = reorder(icb_label, uda_rate), x = uda_rate)) +
  geom_col(width = 0.5, fill = "#f9bf07") +
  labs(y = "ICB", x = "UDA rate per 1000",
       title = "Delivery of dental units in the child population by ICB",
       subtitle = "West Midlands ICB's | NHS Digital 2021")

# Visualise supply z-score
midlands_icb_delivery |> 
  filter(icb_name %in% icb_lookup$icb_name[icb_lookup$icb_code %in% west_midlands_icb_codes]) |> 
  mutate(icb_label = str_remove_all(icb_name, "INTEGRATED CARE BOARD")) |> 
  mutate(icb_label = str_remove_all(icb_label, "NHS ")) |> 
  
  ggplot(aes(y = reorder(icb_label, z_score), x = z_score, fill = direction)) +
  geom_col(width = 0.5) +
  scale_fill_SU() +
  labs(y = "ICB", x = "Z score", fill = "",
       title = "Delivery of dental units in the child population by ICB",
       subtitle = "West Midlands ICB's | NHS Digital 2021")


# Combine need and supply z-score 

# Need
ndep_icb_summary_zscore |> 
  mutate(area_name = str_remove_all(area_name, " Integrated Care System")) |> 
  group_by(name) |> 
  mutate(area_id = row_number()) |> 
  left_join(
    # Supply
    midlands_icb_delivery |> 
      filter(icb_name %in% icb_lookup$icb_name[icb_lookup$icb_code %in% west_midlands_icb_codes]) |>
      mutate(area_id = row_number()),
    by = "area_id"
    ) |> 
  
  filter(str_detect(name, "3-year")) |> 
  
  ggplot(aes(x = z_score.x, y = z_score.y, colour = area_name)) +
  geom_point(size = 6) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(~name) +
  scale_color_SU() +
  theme(strip.background = element_rect(fill = NA, colour = "grey"),
        strip.text = element_text(face = "bold")
        ) +
  labs(x = "Need: z-score", y = "Supply: z-score", 
       color = "ICB",
       title = "Comparing the need for and supply of dental services in West Midlands ICB's",
       subtitle = "Sources: Need - NDEP, Supply - NHS Digital")






# Map ----
library(sf)
la_18_shp <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2018_Ultra_Generalised_Boundaries_GB_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

la_18_shp |> 
  select(2, 3, geometry) |> 
  left_join(any_treatment_needs, by = c("lad18cd" = "upper_tier_la_code")) |> 
  filter(!is.na(upper_tier_la_name)) |> 
  
  ggplot() +
  geom_sf(aes(fill = percent_with_any_treatment_need))





# 3 or 5 year olds example 
fingertips_indicators <- read_csv("1_strategic_data_review/data/fingertips_indicators.csv")

fingertips_indicators |> 
  select(indicator_id, indicator_name) |> 
  distinct()

# # A tibble: 12 × 2
# indicator_id indicator_name                                                               
# <dbl> <chr>                                                                        
# 1        93493 Hospital admissions for dental caries (0-5 years)                            
# 2           NA NA                                                                           
# 3        93479 Hospital admissions for dental caries (0-5 years)                            
# 4        92499 Incisor caries prevalence in three year olds                                 
# 5        92500 Percentage of three year olds with experience of visually obvious tooth decay
# 6        92501 dmft in three year olds                                                      
# 7        92504 dmft (decayed, missing or filled teeth) in five year olds                    
# 8        93563 Percentage of 5 year olds with experience of visually obvious dental decay   
# 9        92785 Access to NHS dental services - successfully obtained a dental appointment   
# 10        92953 Mortality rate from oral cancer                                              
# 11         1206 Oral cancer registrations                                                    
# 12        90820 Children with one or more decayed,  missing or filled teeth 


fingertips_indicators |> 
  filter(indicator_id == "92500") |> 
  
  ggplot(aes(x = value, y = reorder(area_name, value))) +
  geom_col() +
  facet_wrap(~timeperiod)




