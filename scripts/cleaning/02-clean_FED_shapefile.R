# Purpose: Clean Raw FED Shapefile.
# Author: Benedict Cummins-Mburu
# Last Updated: 27 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------- Setup --------
library(tidyverse)
library(sf)
library(arrow)
PCFRF_2022 <- read_parquet("data/clean_data/clean_PCFRF_2022.parquet")
FED_shapefile_raw <- st_read(
  "data/raw_data/lfed000b21a_e/lfed000b21a_e.shp"
)

# ------- Cleaning --------

fed_province <- PCFRF_2022 %>%
  distinct(FEDUID, PROVINCE)


FED_shapefile_clean <- FED_shapefile_raw %>%
  mutate(
    FEDENAME = case_when(
      FEDENAME ==
        "Beauport--Côte-de-Beaupré--Île d\u0092Orléans--Charlevoix" ~ "Beauport--Côte-de-Beaupré--Île d’Orléans--Charlevoix",
      TRUE ~ FEDENAME
    )
  ) %>%
  rename(FED = FEDENAME) %>%
  select(FED, FEDUID, LANDAREA, geometry) %>%
  left_join(fed_province, by = "FEDUID")

# ------- Validation --------

if (nrow(FED_shapefile_clean) == 338) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: FED shapefile has the wrong number of rows.")
}

# ------- Write to RDS --------
saveRDS(
  FED_shapefile_clean,
  "data/clean_data/clean_FED_shapefile.rds"
)
