# Purpose: Clean and Validate the 2021 Geographic Attribute File (GAF) into a Dissemination Area (DA) lookup table. Saves dataset to file.
# Author: Benedict Cummins-Mburu
# Last Updated: 25 Jun 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - 1.6% of DAs mapped to two or more FEDs. These were aggressively removed. Does not impact darta pipeline, just validation.

# ------- Setup --------
library(tidyverse)
library(data.table)
library(arrow)
GAF_2021 <- data.table::fread("data/raw_data/raw_GAF_2021.csv")
FED_lookup <- readRDS("data/clean_data/FED_lookup.rds")

# ------ Cleaning -------

clean_GAF <- GAF_2021 %>%
  transmute(
    DAUID = as.character(DAUID_ADIDU),
    FEDUID = as.character(FEDUID_CEFIDU),
    PRUID = as.character(PRUID_PRIDU)
  ) %>%
  distinct(DAUID, .keep_all = TRUE)

# ----- Validation ------

clean_GAF_test_1 <- GAF_2021 %>%
  transmute(
    DAUID = as.character(DAUID_ADIDU),
    FEDUID = as.character(FEDUID_CEFIDU),
    PRUID = as.character(PRUID_PRIDU)
  ) %>%
  distinct(DAUID, FEDUID, PRUID)

clean_GAF_test_2 <- GAF_2021 %>%
  transmute(
    DAUID = as.character(DAUID_ADIDU),
    FEDUID = as.character(FEDUID_CEFIDU)
  ) %>%
  distinct(DAUID, FEDUID)

if (nrow(clean_GAF_test_1) == nrow(clean_GAF_test_2)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: The same FED or DA maps to multiple PRs.")
}

if (
  ((nrow(clean_GAF_test_1) - nrow(clean_GAF)) / nrow(clean_GAF_test_1)) < 0.016
) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Over 1.6% of DAs mapped to two or more FEDs.")
}

if (length(unique(clean_GAF$DAUID)) == nrow(clean_GAF)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: DAUIDs are not unique.")
}

if (all(str_detect(clean_GAF$DAUID, "^\\d{8}$"))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some DA IDs are malformatted.")
}

if (length(unique(clean_GAF$FEDUID)) == 338) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: There are a wrong number of FEDUIDs.")
}

if (setequal(unique(clean_GAF$FEDUID), FED_lookup$FEDUID)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: There are wrong FEDUIDs.")
}

# ------- Save --------
write_parquet(clean_GAF, "data/clean_data/DA_lookup.parquet")
