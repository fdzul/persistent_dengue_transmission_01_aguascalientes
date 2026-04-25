

# Step 1. load the geocoded dataset ####
#load("~/Library/CloudStorage/Dropbox/hotspots_2025/8.RData/denmex.RData")
load("~/Library/CloudStorage/Dropbox/hotspots_2025/8.RData/denmex_chains.RData")

# Step 2. load the function ####
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/1.2.persisten_dengue_transmission_2026/persistent_dengue_transmission_01_aguascalientes/mp_transmission_chains.R")
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/1.2.persisten_dengue_transmission_2026/persistent_dengue_transmission_01_aguascalientes/mp_dengue_cases.R")
# Step 3. make the maps of transmission chains ####
library(mapgl)
library(fishualize)

mp_transmission_chains(data = xy,
                       yrs = c(2016:2022),
                       locality = "Aguascalientes",
                       cve_edo = "01")
mp_transmission_chains(data = xy,
                       yrs = c(2023),
                       locality = "Aguascalientes",
                       cve_edo = "01")
mp_transmission_chains(data = xy,
                       yrs = c(2024),
                       locality = "Aguascalientes",
                       cve_edo = "01")
mp_transmission_chains(data = xy,
                       yrs = c(2025),
                       locality = "Aguascalientes",
                       cve_edo = "01")
mp_transmission_chains(data = xy,
                       yrs = c(2023:2025),
                       locality = "Aguascalientes",
                       cve_edo = "01")

# Step 3. make the maps of dengue cases ####
mp_dengue_cases(data = xy,
                yrs = c(2016:2022),
                locality = "Aguascalientes",
                cve_edo = "01")
mp_dengue_cases(data = xy,
                yrs = 2023,
                locality = "Aguascalientes",
                cve_edo = "01")

mp_dengue_cases(data = xy,
                yrs = 2024,
                locality = "Aguascalientes",
                cve_edo = "01")
mp_dengue_cases(data = xy,
                yrs = 2025,
                locality = "Aguascalientes",
                cve_edo = "01")

mp_dengue_cases(data = xy,
                yrs = c(2023:2025),
                locality = "Aguascalientes",
                cve_edo = "01")






###########
data <- data |>
    dplyr::filter(ANO %in% c(yrs)) |>
    dplyr::mutate(week = lubridate::epiweek(onset)) |>
    dplyr::mutate(week_factor = dplyr::case_when(week <= 10 ~ "1-10",
                                                 week > 10 & week <= 20 ~ "11-20",
                                                 week > 20 & week <= 25 ~ "21-25",
                                                 week > 25 & week <= 30 ~ "26-30",
                                                 week > 30 & week <= 35 ~ "31-35",
                                                 week > 35 & week <= 40 ~ "36-40",
                                                 week > 40 & week <= 45 ~ "41-45",
                                                 week > 45 & week <= 53 ~ "46-53")) |>
    dplyr::mutate(week_factor = factor(week_factor,
                                       levels = c("1-10",
                                                  "11-20",
                                                  "21-25",
                                                  "26-30",
                                                  "31-35",
                                                  "36-40",
                                                  "41-45",
                                                  "46-53"),
                                       ordered = TRUE)) 
# Step 2. 
loc <- rgeomex::extract_locality(cve_edo = "01",
                                 locality = "Aguascalientes")
data <- data[loc, ] 
#

library(mapgl)
library(sf)
#######
#pal_colores <- c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4",
#                 "#1d91c0", "#225ea8", "#253494", "#081d58")

pal_colores <- fishualize::fish(n = 8,
                                option = "Scarus_hoefleri",
                                direction = -1)

# week_num como character
data$week_num <- as.character(as.integer(data$week_factor))

maplibre(style  = carto_style("positron"),
         bounds = st_bbox(loc)) |>
    add_source("area", data = loc) |>
    add_line_layer(id         = "ciudad-borde",
                   source     = "area",
                   line_color = "#444444",
                   line_width = 1) |>
    add_circle_layer(id     = "casos-puntos",
                     source = data,
                     circle_color = match_expr(column  = "week_num",
                                               values  = as.character(1:8),
                                               stops   = pal_colores),
                     circle_stroke_color = "white",
                     circle_stroke_width = 1,
                     circle_radius       = 6,
                     tooltip             = "week_factor") |>
    add_categorical_legend(legend_title = "Semana  Epidemiológica",
                           values       = c("1-10","11-20","21-25","26-30",
                                            "31-35","36-40","41-45","46-53"),
                           colors       = pal_colores,
                           patch_shape  = "circle") |>
    add_fullscreen_control(position = "top-left") |> 
    add_navigation_control() |>
    add_globe_control() |>
    add_scale_control()

######

