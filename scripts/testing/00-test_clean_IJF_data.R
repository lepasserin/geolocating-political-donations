# Purpose: Validating Structure of
# Author: Benedict Cummins-Mburu
# Last Updated: 30 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ---- Setup --------
library(tidyverse)
library(arrow)
focal_data <- read_csv("data/clean_data/clean_data_IJF.parquet")

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

EXPECTED_POLITICAL_ENTITIES <- c(
  # n = 5
  "Leadership contestants",
  "Nomination contestants",
  "Candidates",
  "Registered parties",
  "Registered associations"
)

EXPECTED_POLITICAL_PARTIES <- c(
  # n = 37
  "Liberal Party of Canada",
  "New Democratic Party",
  "Bloc Québécois",
  "Conservative Party of Canada",
  "Green Party of Canada",
  "Christian Heritage Party of Canada",
  "Canadian Action Party",
  "Independent",
  "Progressive Canadian Party",
  "Libertarian Party of Canada",
  "Marijuana Party",
  "Marxist-Leninist Party of Canada",
  "First Peoples National Party of Canada",
  "Western Block Party",
  "United Party of Canada",
  "Parti Rhinocéros Party",
  "Pirate Party of Canada",
  "People's Political Power Party of Canada",
  "Animal Protection Party of Canada",
  "Newfoundland and Labrador First Party",
  "Communist Party of Canada",
  "People's Party of Canada",
  "Party for Accountability, Competency and Transparency",
  "Forces et Démocratie",
  "Canada Party",
  "Seniors Party of Canada",
  "National Advancement Party of Canada",
  "Alliance of the North",
  "National Citizens Alliance of Canada",
  "Canadian Nationalist Party",
  "Direct Democracy Party of Canada",
  "Parti pour l'Indépendance du Québec",
  "Maverick Party",
  "Free Party Canada",
  "Centrist Party of Canada",
  "Veterans Coalition Party of Canada",
  "Canadian Future Party"
)
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
