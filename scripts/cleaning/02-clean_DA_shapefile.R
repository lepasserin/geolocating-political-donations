# Purpose: Clean and Validate Raw DA Shapefile such that it Functions as a Lookup Table.
# Author: Benedict Cummins-Mburu
# Last Updated: 1 Jul 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - centroid coordinates are saved in latitude/longitude degrees (st = 4326).
# - centroid coordinates are saved at a spatial resolution of ~ 1km (5 decimal places ; not enough to differentiate metropolitan DAs, but enough to provide a good distance measurement).

# ------- Setup --------
library(tidyverse)
library(sf)
DA_shapefile_raw <- st_read(
  "data/raw_data/lda000b21a_e/lda_000b21a_e.shp"
)
COORDINATE_SYSTEM <- 4326

# ------- Cleaning --------

DA_shapefile_clean <- DA_shapefile_raw %>%
  rename(area = LANDAREA) %>%
  select(DAUID, PRUID, area, geometry) %>%
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

DA_shapefile_test <- st_drop_geometry(DA_shapefile_clean)

if (all(complete.cases(DA_shapefile_test))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: DA lookup has missing values.")
}

if (nrow(DA_shapefile_test) == 57932) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: DA lookup has the wrong number of rows.")
}

if (length(unique(DA_shapefile_clean$DAUID)) == nrow(DA_shapefile_clean)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: DA lookup IDs are not unique.")
}

if (length(unique(DA_shapefile_clean$PRUID)) == 13) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: DA lookup has the wrong number of PRUIDs")
}

if (all(DA_shapefile_clean$area >= 0.0)) {
  # a few cases (6) have 0 area, ignoring at this stage.
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some DAs have negative areas.")
}


# ------- Write to RDS --------
saveRDS(
  DA_shapefile_clean,
  "data/clean_data/DA_lookup.rds"
)
