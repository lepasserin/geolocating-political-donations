# Purpose: Clean and Validate Raw FED Shapefile such that it Functions as a Lookup Table.
# Author: Benedict Cummins-Mburu
# Last Updated: 25 Jun 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - centroid coordinates are saved in latitude/longitude degrees (st = 4326).
# - centroid coordinates are saved at a spatial resolution of ~ 1km (5 decimal places).

# ------- Setup --------
library(tidyverse)
library(sf)
FED_shapefile_raw <- st_read(
  "data/raw_data/lfed000b21a_e/lfed000b21a_e.shp"
)
COORDINATE_SYSTEM <- 4326

# ------- Cleaning --------

FED_shapefile_clean <- FED_shapefile_raw %>%
  mutate(
    FEDENAME = case_when(
      FEDENAME ==
        "Beauport--Côte-de-Beaupré--Île d\u0092Orléans--Charlevoix" ~ "Beauport--Côte-de-Beaupré--Île d’Orléans--Charlevoix",
      TRUE ~ FEDENAME
    )
  ) %>%
  rename(name = FEDENAME) %>%
  rename(area = LANDAREA) %>%
  select(FEDUID, PRUID, name, area, geometry) %>%
  mutate(
    centroid = st_centroid(st_transform(
      st_make_valid(geometry),
      COORDINATE_SYSTEM
    )), # use lat, lon instead of metres
    centroid_lat = st_coordinates(centroid)[, 2],
    centroid_lon = st_coordinates(centroid)[, 1]
  ) %>%
  select(-centroid)

# ------- Validation --------

FED_shapefile_test <- st_drop_geometry(FED_shapefile_clean)

if (all(complete.cases(FED_shapefile_test))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: FED lookup has missing values.")
}

if (nrow(FED_shapefile_clean) == 338) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: FED lookup has the wrong number of rows.")
}

if (length(unique(FED_shapefile_clean$FEDUID)) == nrow(FED_shapefile_clean)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: FED lookup IDs are not unique.")
}

if (length(unique(FED_shapefile_clean$name)) == nrow(FED_shapefile_clean)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: FED lookup names are not unique.")
}

if (length(unique(FED_shapefile_clean$PRUID)) == 13) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: FED lookup has the wrong number of PRUIDs")
}

if (all(FED_shapefile_clean$area > 0.0)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some FEDs have negative or zero areas.")
}

# ------- Write to RDS --------
saveRDS(
  FED_shapefile_clean,
  "data/clean_data/FED_lookup.rds"
)
