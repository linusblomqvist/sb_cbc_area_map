library(tidyverse)
library(sf)

sf_data <- readRDS("output/sb_cbc_areas.rds")
hotspots <- readRDS("data/hotspots.rds")

# Center point
lat <- 34.451200
lon <- -119.762700

# Radius in meters (15 miles diameter → 7.5 mile radius)
radius_m <- 7.5 * 1609.34  # ~12070.05 m

# Point in WGS84 (same CRS as sf_data)
pt_wgs <- st_sfc(st_point(c(lon, lat)), crs = 4326)

# Project to a metric CRS (good for California)
pt_proj <- st_transform(pt_wgs, 3310)  # NAD83 / California Albers

# Buffer by 7.5 miles in meters
circle_proj <- st_buffer(pt_proj, dist = radius_m)

# Transform back to match sf_data's CRS (WGS84)
circle <- st_transform(circle_proj, st_crs(sf_data))

# Optionally make it a proper sf object with an attribute
circle_sf <- st_sf(name = "15-mile circle", geometry = circle)

# 1) Make sure everything is in the same CRS -------------------------
hotspots_aligned <- st_transform(hotspots, st_crs(circle_sf))
sf_data_aligned  <- st_transform(sf_data,  st_crs(circle_sf))

# 2) Logical vector: which hotspots are inside the circle? -----------
inside_circle <- st_within(hotspots_aligned, circle_sf, sparse = FALSE)[, 1]
# 'sparse = FALSE' gives a logical matrix; [,1] since there is one circle

# 3) Logical vector: which hotspots are in ANY sf_data polygon? ------
# st_intersects is good here; sparse = TRUE by default
in_polygons_list <- st_intersects(hotspots_aligned, sf_data_aligned)

# TRUE if the hotspot intersects at least one polygon
in_any_polygon <- lengths(in_polygons_list) > 0

# 4) Filter: in circle AND NOT in polygons ---------------------------
hotspots_target <- hotspots_aligned[ inside_circle & !in_any_polygon, ]

# hotspots_target is now the sf object you want
hotspots_target$hotspot_name
