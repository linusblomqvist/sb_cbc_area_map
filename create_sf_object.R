# Create sf object
library(sf)
library(tidyverse)

south_sb <- read_sf("south_sb.kml", layer = "CBC South SB Areas") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("South SB", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

# temp
south_sb <- south_sb %>%
  mutate(Description = "")

mountains <- read_sf("mountains.kml", layer = "Mountains") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Circle = "Santa Barbara") %>%
  mutate(Name = ifelse(Name == "Mountains--Kummel's home and surroundings", "Mountains--K's home and surroundings", Name))

north_sb <- read_sf("north_sb.kml", layer = "Polygons") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("North SB", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

north_goleta <- read_sf("north_goleta.kml", layer = "North Goleta.kml") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("North Goleta", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara") %>%
  mutate(Name = ifelse(Name == "North Goleta--Tucker's Grove", "North Goleta--Tucker's Grove and San Antonio Canyon Park", Name))

montecito <- read_sf("montecito.kml", layer = "CBC Montecito Areas") %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_make_valid() %>%
  mutate(Name = str_c("Montecito", Name, sep = "--")) %>%
  mutate(Circle = "Santa Barbara")

south_goleta <- read_sf("south_goleta.kml", layer = "south_goleta_polygons") %>%
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
  
  # Loop over each feature
  result <- sf_object %>%
    rowwise() %>%
    mutate(
      coords_formatted = {
        # Extract coordinates for the geometry
        coords <- st_coordinates(geometry)
        
        # Format as "lat,lon"
        coords_strings <- apply(coords, 1, function(x) {
          paste0(x[2], "%2C%20", x[1])  # Lat (Y), Lon (X)
        })
        
        # Concatenate with " - " separator
        paste(coords_strings, collapse = "%20-%20")
      }
    ) %>%
    select(Name, coords_formatted)  # Keep only name and formatted coordinates
  
  return(result)
}

# Apply the function
sb_cbc_areas <- bind_cols(sb_cbc_areas, extract_coordinates(sb_cbc_areas) %>%
                            st_drop_geometry() %>%
                            select(coords_formatted))

sb_cbc_areas <- sb_cbc_areas %>%
  mutate(birdview_url = str_c("https://s3.us-west-1.amazonaws.com/membot.com/BirdViewSBC.html?v=2&species=%E2%9A%A1%20%2B%20Ross%27s%20Goose%20%2B%20Greater%20White-fronted%20Goose%20%2B%20Wood%20Duck%20%2B%20Blue-winged%20Teal%20%2B%20Eurasian%20Wigeon%20%2B%20Greater%20Scaup%20%2B%20Common%20Merganser%20%2B%20Mountain%20Quail%20%2B%20Greater%20Roadrunner%20%2B%20Costa%27s%20Hummingbird%20%2B%20Virginia%20Rail%20%2B%20Wilson%27s%20Snipe%20%2B%20Iceland%20Gull%20%2B%20Elegant%20Tern%20%2B%20American%20Bittern%20%2B%20Yellow-crowned%20Night%20Heron%20%2B%20Northern%20Pygmy-Owl%20%2B%20Burrowing%20Owl%20%2B%20Northern%20Saw-whet%20Owl%20%2B%20Yellow-bellied%20Sapsucker%20%2B%20Red-naped%20Sapsucker%20%2B%20Mountain%20Chickadee%20%2B%20Horned%20Lark%20%2B%20Tree%20Swallow%20%2B%20Violet-green%20Swallow%20%2B%20Northern%20Rough-winged%20Swallow%20%2B%20Barn%20Swallow%20%2B%20Golden-crowned%20Kinglet%20%2B%20Phainopepla%20%2B%20Brown%20Creeper%20%2B%20Rock%20Wren%20%2B%20Pacific%20Wren%20%2B%20Mountain%20Bluebird%20%2B%20Townsend%27s%20Solitaire%20%2B%20Varied%20Thrush%20%2B%20Cassin%27s%20Finch%20%2B%20Red%20Crossbill%20%2B%20Pine%20Siskin%20%2B%20Lawrence%27s%20Goldfinch%20%2B%20Lark%20Sparrow%20%2B%20Chipping%20Sparrow%20%2B%20White-throated%20Sparrow%20%2B%20Vesper%20Sparrow%20%2B%20Orchard%20Oriole%20%2B%20Hooded%20Oriole%20%2B%20Bullock%27s%20Oriole%20%2B%20Baltimore%20Oriole%20%2B%20Tricolored%20Blackbird%20%2B%20Nashville%20Warbler%20%2B%20Yellow%20Warbler%20only%20%2B%20Black-throated%20Gray%20Warbler%20%2B%20Hermit%20Warbler%20%2B%20Wilson%27s%20Warbler%20%2B%20Summer%20Tanager%20%2B%20Western%20Tanager%20%2B%20Rose-breasted%20Grosbeak%20%2B%20Black-headed%20Grosbeak&photosOnly=0&taxaLevel=full&rarity=&date=2024-Dec%20-%202025-Jan&loc=&hotspotsOnly=0&latlon=",
                              coords_formatted,
                              "&birder=&source=checklists&groupMode=checklists&listby=taxon", # &lat=34.4636&lon=-119.7696&zoom=9.9731
                              sep = "")) %>%
  mutate(birdview_dec_jan = str_replace_all(
    birdview_url, 
    "date=2024-Dec%20-%202025-Jan", 
    "date=Dec-Jan"
  ))

hotspots <- read.csv("hotspots_sba.csv", header = FALSE)

hotspots <- hotspots %>%
  set_names(c("code", "country", "state", "county", "lat", "lon", "hotspot_name", 
                 "updated", "number"))

hotspots <- st_as_sf(hotspots, coords = c("lon", "lat"), crs = 4326)

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

saveRDS(sb_cbc_areas, "sb_cbc_areas.rds")
