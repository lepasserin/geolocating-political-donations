# Purpose: Query Census data through the CensusMappper API to get socio-demographic data for each DA, and for each FED. Seperate datafiles are saved for each.
# Author: Benedict Cummins-Mburu
# Last Updated: 17 June 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup --------
library(tidyverse)
library(cancensus)
WHICH_CENSUS <- "CA21"
MINIMUM_SPATIAL_GRAIN <- "DA"
if (Sys.getenv("CM_API_KEY") == "") {
  message("Error: CensusMapper API key is unavailable.")
}
FED_shapefile <- readRDS("data/clean_data/clean_FED_shapefile.rds") # names: FED , geometry , PROVINCE (for validation)
VARIABLES_OF_INTEREST <- c()

# --- Get Census Data for All DAs ---

province_region_ids <- list_census_regions(WHICH_CENSUS) %>%
  filter(level == "PR") %>%
  pull(region)


the <- list_census_vectors(WHICH_CENSUS)
thing <- list_census_regions("CA21")
length(unique(list_census_regions("CA21")$level))


toronto_dbs <- get_census(
  dataset = WHICH_CENSUS,
  regions = list(CMA = "35535"),
  level = MINIMUM_SPATIAL_GRAIN, # <-- This is where you specify DA
  geo_format = "sf" # Optional: returns spatial boundaries if needed
)

# ----- Save to File  -----

# COLUMNS: DA, PR, FED, geometry, population,
