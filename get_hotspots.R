library(tidyverse)
library(auk)

ebd <- read_ebd("data/ebd_US-CA-083_relOct-2025/ebd_US-CA-083_relOct-2025.txt")

hotspots <- ebd %>%
  filter(locality_type == "H") %>%
  distinct(locality_id, .keep_all = TRUE) %>%
  select(locality, locality_id, latitude, longitude) %>%
  set_names(c("hotspot_name", "code", "lat", "lon")) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

saveRDS(hotspots, "data/hotspots.rds")
