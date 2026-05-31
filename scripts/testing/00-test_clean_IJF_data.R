# Purpose: Validating Structure of
# Author: Benedict Cummins-Mburu
# Last Updated: 30 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ---- Setup --------
library(tidyverse)
focal_data <- clean_data_09

# ----- Constants -----

EXPECTED_COLUMN_NAMES <- c(
  "electoral_event",
  "political_entity",
  "political_party",
  "recipient",
  "donor_location",
  "electoral_district",
  "donation_date",
  "donation_year",
  "donor_full_name",
  "amount_monetary",
  "amount_non_monetary",
  "donor_postal_code"
)
EXPECTED_POLITICAL_ENTITIES <- c()
EXPECTED_POLITICAL_PARTIES <- c()
EXPECTED_DATE_RANGE <- c(
  lubridate::ymd("2004-01-01"),
  lubridate::ymd("2026-05-05")
)

# ------ Testing -------

# 1. Testing Column Structure
if (ncol(focal_data) == length(EXPECTED_COLUMN_NAMES)) {
  message("Test Passed: Correct number of columns.")
} else {
  stop("Test Failed: Wrong number of columns.")
}
if (setequal(names(focal_data), EXPECTED_COLUMN_NAMES)) {
  message("Test Passed: Columns are named correctly.")
} else {
  stop("Test Failed: Column names are wrong.")
}

# 2. Testing Bounds and Categories (also checks for NAs)
if (
  setequal(unique(focal_data$political_entitity), EXPECTED_POLITICAL_ENTITIES)
) {
  message("Test Passed: Categories of `political_entity` are expected.")
} else {
  stop("Test Failed: Invalid `political_entity`.")
}
if (setequal(unique(focal_data$political_party), EXPECTED_POLITICAL_PARTIES)) {
  message("Test Passed: Categories of `political_entity` are expected.")
} else {
  stop("Test Failed: Invalid `political_entity`.")
}

# - check the date range
# - test postal code structure

# 3. Testing That NAs are correctly distributed
if (
  setequal(
    unique(focal_data[is.na(focal_data$electoral_district), ]$political_entity),
    c("Leadership contestants", "Registered parties")
  )
) {
  message(
    "Test Passed: All donations with NA `electoral_district` are issued to Leadership Contestants or Registered parties."
  )
} else {
  stop(
    "Error: Entities other than Leadership Contestants and Registered Parties have NA `electoral_district`"
  )
}
if (
  setequal(
    unique(
      focal_data[!is.na(focal_data$electoral_district), ]$political_entity
    ),
    c("Nomination contestants", "Candidates", "Registered associations")
  )
) {
  message(
    "Test Passed: All donations with an `electoral_district` are issued to Nomination contestants, Candidates, or Registered associations"
  )
} else {
  stop(
    "Error: Entities other than Leadership Contestants and Registered Parties have NA `electoral_district`"
  )
}

# 4. Testing Intra-Column Consistency
# - check that year and date are consistent
