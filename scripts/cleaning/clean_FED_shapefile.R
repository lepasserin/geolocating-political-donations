# Purpose: Generate a Sample of the data to show to Chris Cochrane.
# Author: Benedict Cummins-Mburu
# Last Updated: 27 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------- Setup --------
library(tidyverse)
library(sf)
FED_shapefile_raw <- st_read(
  "data/conversion_files/lfed000b21a_e/lfed000b21a_e.shp"
)

# ------- Cleaning --------

FED_shapefile_clean <- FED_shapefile_raw %>%
  mutate(
    FEDENAME = case_when(
      FEDENAME ==
        "Beauport--Côte-de-Beaupré--Île d\u0092Orléans--Charlevoix" ~ "Beauport--Côte-de-Beaupré--Île d’Orléans--Charlevoix",
      TRUE ~ FEDENAME
    )
  )

# ------- Write to RDS --------
saveRDS(
  FED_shapefile_clean,
  "data/conversion_files/lfed000b21a_e/lfed000b21a_e.rds"
)
