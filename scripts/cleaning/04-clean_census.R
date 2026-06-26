# Purpose: Query the CensusMappper API to get desired census data for each DA. Saves data to file.
# Author: Benedict Cummins-Mburu
# Last Updated: 25 June 2026
# STATUS: TODO (NEXT: SCAFFOLDED)
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup --------
library(tidyverse)
library(arrow)
library(cancensus)
DA_lookup <- read_parquet("data/clean_data/DA_lookup.parquet")
if (Sys.getenv("CM_API_KEY") == "") {
  message("Error: CensusMapper API key is unavailable.")
}

# ------ Constants ------
WHICH_CENSUS <- "CA21"
SPATIAL_GRAIN <- "DA"
VALID_DAs <- unique(DA_lookup$DAUID)
VARIABLES_OF_INTEREST <- c(
  "v_CA21_386" # mean age (comined male and female)
)
PRUIDs <- c(
  "10",
  "11",
  "12",
  "13",
  "24",
  "35",
  "46",
  "47",
  "48",
  "59",
  "60",
  "61",
  "62"
)
# DIAGNOSTIC: confirm that all province IDs are available for query
# province_region_ids <- list_census_regions(WHICH_CENSUS) %>%
#   filter(level == "PR") %>%
#   pull(region)
# setequal(province_region_ids, PRUIDs)
# DIAGNOSTIC: confirm that all variables are available for query
# vectors <- list_census_vectors(WHICH_CENSUS) %>%
#   pull(vector)
# all(VARIABLES_OF_INTEREST %in% vectors)

# ------- Helpers -------

get_province_DA_data <- function(pruid) {
  get_census(
    dataset = WHICH_CENSUS,
    regions = list(PR = as.numeric(pruid)),
    vectors = VARIABLES_OF_INTEREST,
    level = SPATIAL_GRAIN,
    geo_format = NA,
    quiet = FALSE
  )
}

this <- get_province_DA_data("35")

# ------ Query API -------

# census_data_raw <- map(PRUIDs, get_province_DA_data) %>%
#   bind_rows()

# ----- Validation ------

queried_DAs <- unique(census_data_raw$DAUID)

if (all(VALID_DAs %in% queried_DAs)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Some DAs in DA_lookup are missing from the queried data."
  )
}

if (all(queried_DAs %in% VALID_DAs)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Some DAs in DA_lookup are missing from the queried data."
  )
}

#TODO: validate that all DAs pulled exist in the PCCF

# ----- Save to File  -----

# COLUMNS: DAUID, population, households, VARIABLES_OF_INTEREST
