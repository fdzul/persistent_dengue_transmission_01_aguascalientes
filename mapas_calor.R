

# Step 1. load the dataset ####
load("~/Library/CloudStorage/Dropbox/hotspots_2025/8.RData/denmex_chains.RData")

# Step 2. load the function ####
source("~/Library/CloudStorage/Dropbox/r_developments/r_dashboards/netlify/1.2.persisten_dengue_transmission_2026/persistent_dengue_transmission_01_aguascalientes/mp_heatmap.R")
library(sf)
library(mapgl)

# 2023-2025
mp_heatmap(geocoded_dataset = xy,
           yrs = c(2023:2025),
           cve_edo = "01",
           locality = "Aguascalientes")

# 2023
mp_heatmap(geocoded_dataset = xy,
           yrs = 2025,
           cve_edo = "01",
           locality = "Aguascalientes")

# 2024
mp_heatmap(geocoded_dataset = xy,
           yrs = 2024,
           cve_edo = "01",
           locality = "Aguascalientes")
# 2023
mp_heatmap(geocoded_dataset = xy,
           yrs = 2023,
           cve_edo = "01",
           locality = "Aguascalientes")

# 2016-2022
mp_heatmap(geocoded_dataset = xy,
           yrs = c(2016:2022),
           cve_edo = "01",
           locality = "Aguascalientes")
#####################

geocoded_dataset <- xy
yrs = 2023
cve_edo <- "01"
locality = "Aguascalientes"

# Step 1. transform dataset #####
z <- geocoded_dataset 

# Step 2. extract the locality ####
loc <- rgeomex::extract_locality(cve_edo = cve_edo,
                                 locality = locality)

# Step 3. extract the geocoded cases of merida ####
z <- z[loc, ]  |>
    dplyr::mutate(x = long,
                  y = lat) |>
    #sf::st_drop_geometry() |>
    dplyr::filter(ANO %in% c(yrs))

#########

mapgl::maplibre_view(loc,
                     color = NA,
                     #style = carto_style(style_name = "voyager")
                     style = mapgl::carto_style(style_name = "positron")) |>
    mapgl::add_source("area",  data = loc) |>
    mapgl::add_source("casos", data = z) |>
    # Límites de la ciudad
    add_fill_layer(id           = "ciudad-fill",
                   source       = "area",
                   fill_color   = "transparent",
                   fill_opacity = 0) |>
    mapgl::add_line_layer(id             = "ciudad-borde",
                          source         = "area",
                          line_color     = "#444444",
                          #line_dasharray = c(1, 1)
                          line_width     = 1) |>
    mapgl::add_heatmap_layer(id = "dengue_cases",
                             source = "casos",
                             heatmap_weight = mapgl::interpolate(column = "mag",
                                                                 values = c(0, 6),
                                                                 stops = c(0, 1)),
                             heatmap_intensity = mapgl::interpolate(property = "zoom",
                                                                    values = c(0, 9),
                                                                    stops = c(1, 3)),
                             heatmap_color = mapgl::interpolate(property = "heatmap-density",
                                                                values = seq(from = 0, to = 1, by = 0.2),
                                                                stops = c('rgba(33,102,172,0)', 
                                                                          'rgb(103,169,207)',
                                                                          'rgb(209,229,240)', 
                                                                          'rgb(253,219,199)',
                                                                          'rgb(239,138,98)', 
                                                                          'rgb(178,24,43)')),
                             heatmap_opacity = 0.7) |>
    mapgl::dd_fullscreen_control(position = "top-left") |> 
    mapgl::add_navigation_control() |>
    mapgl::add_globe_control() |>
    mapgl::add_scale_control()



