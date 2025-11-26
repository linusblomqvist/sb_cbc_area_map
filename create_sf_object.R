# Create sf object
library(sf)
library(tidyverse)

hotspots <- readRDS("data/hotspots.rds")

south_sb <- read_sf("data/south_sb.kml", layer = "CBC South SB Areas") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("South SB", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

# temp
south_sb <- south_sb %>%
  mutate(Description = "")

mountains <- read_sf("data/mountains.kml", layer = "Mountains") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Circle = "Santa Barbara") %>%
  mutate(Name = ifelse(Name == "Mountains--Kummel's home and surroundings", "Mountains--K's home and surroundings", Name))

north_sb <- read_sf("data/north_sb.kml", layer = "Polygons") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("North SB", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

north_goleta <- read_sf("data/north_goleta.kml", layer = "North Goleta.kml") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("North Goleta", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara") %>%
  mutate(Name = ifelse(Name == "North Goleta--Tucker's Grove", "North Goleta--Tucker's Grove and San Antonio Canyon Park", Name))

montecito <- read_sf("data/montecito.kml", layer = "CBC Montecito Areas") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("Montecito", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

south_goleta <- read_sf("data/south_goleta.kml", layer = "south_goleta_polygons") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("South Goleta", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

sb_cbc_areas <- bind_rows(south_sb, north_sb, mountains, north_goleta,
                          south_goleta, montecito) %>%
  st_transform(crs = 3857) %>%
  st_simplify(dTolerance = 10)

sb_cbc_areas <- st_transform(sb_cbc_areas, crs = 4326)

# Example: Extract coordinates from sf_data
extract_coordinates <- function(sf_object) {
  
  sf_object %>%
    rowwise() %>%
    mutate(
      coords_formatted = {
        # Extract coordinates for the geometry
        coords <- sf::st_coordinates(geometry)
        
        # Format each vertex as "lat,lon"
        coords_strings <- apply(coords, 1, function(x) {
          # x[1] = lon (X), x[2] = lat (Y)
          paste0(x[2], ",", x[1])
        })
        
        # Concatenate vertices with a single space between them
        # e.g. "34.3900,-119.7204 34.4086,-119.6812 ..."
        paste(coords_strings, collapse = " ")
      }
    ) %>%
    ungroup() %>%
    dplyr::select(Name, coords_formatted)
}

# Apply the function
sb_cbc_areas <- bind_cols(sb_cbc_areas, extract_coordinates(sb_cbc_areas) %>%
                            st_drop_geometry() %>%
                            select(coords_formatted))

sb_cbc_areas_with_hotspots <- st_join(sb_cbc_areas, hotspots %>% select(code, hotspot_name))

hyperlink_data <- sb_cbc_areas_with_hotspots %>%
  filter(!is.na(hotspot_name)) %>%  # Exclude polygons with no associated hotspots
  group_by(Name) %>%  # Group by polygon geometry
  summarize(
    hyperlinks = paste0(
      "<a href='https://birdinghotspots.org/hotspot/", 
      code, 
      "'>", 
      hotspot_name, 
      "</a>"
    ) %>%
      paste(collapse = "<br>")  # Combine multiple hyperlinks with line breaks
  )

sb_cbc_areas <- sb_cbc_areas %>%
  left_join(st_drop_geometry(hyperlink_data), by = "Name")

# saveRDS(sb_cbc_areas, "output/sb_cbc_areas.rds")
