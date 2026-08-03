# Purpose: Extract selected census variables from the 2021 Census Profile ZIP at the DA level.
# Author: Benedict Cummins-Mburu
# Last Updated: 27 June 2026
# STATUS: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup --------
library(tidyverse)
library(arrow)
ZIP_PATH <- "data/raw_data/98-401-X2021.zip"

VARIABLE_LOOKUP <- tribble(
  ~CHARACTERISTIC_ID , ~vector                  ,
    39L              , "avg_age"                , # Average age of the population
   243L              , "median_hh_income"       , # Median total income of household in 2020 ($)
  1998L              , "pop_15plus"             , # Total - Highest certificatation pop 15+ (education denominator)
  2001L              , "postsec_count"          , # Postsecondary certificate, diploma or degree
  1683L              , "pop_private_households" , # Total - Visible minority for the population in private households (visible minority denominator)
  1684L              , "vismin_total"           , # Total visible minority population
  1685L              , "vismin_south_asian"     , # South Asian
  1686L              , "vismin_chinese"         , # Chinese
  1687L              , "vismin_black"           , # Black
  1688L              , "vismin_filipino"        , # Filipino
  1689L              , "vismin_arab"            , # Arab
  1690L              , "vismin_latin_american"  , # Latin American
  1691L              , "vismin_southeast_asian" , # Southeast Asian
  1692L              , "vismin_west_asian"      , # West Asian
  1693L              , "vismin_korean"          , # Korean
  1694L              , "vismin_japanese"        , # Japanese
     1L              , "population"             , # Population, 2021
     6L              , "density_per_sqkm"       , # Population density per square kilometre
     7L              , "land_area_sqkm" # Land area in square kilometres
)

VARIABLE_IDS <- VARIABLE_LOOKUP$CHARACTERISTIC_ID

DATA_FILES <- c(
  "98-401-X2021006_English_CSV_data_Atlantic.csv",
  "98-401-X2021006_English_CSV_data_BritishColumbia.csv",
  "98-401-X2021006_English_CSV_data_Ontario.csv",
  "98-401-X2021006_English_CSV_data_Prairies.csv",
  "98-401-X2021006_English_CSV_data_Quebec.csv",
  "98-401-X2021006_English_CSV_data_Territories.csv"
)

# ------ Helpers ------

read_province_file <- function(filename) {
  message(sprintf("Reading: %s", filename))
  con <- unz(ZIP_PATH, filename)
  df <- read_csv(
    con,
    col_types = cols(
      ALT_GEO_CODE = col_character(),
      GEO_LEVEL = col_character(),
      CHARACTERISTIC_ID = col_integer(),
      C1_COUNT_TOTAL = col_double(),
      .default = col_skip()
    )
  ) %>%
    filter(
      GEO_LEVEL == "Dissemination area",
      CHARACTERISTIC_ID %in% VARIABLE_IDS
    )
  message(sprintf("  -> %d rows retained", nrow(df)))
  df
}

# ------ Execution ------

# 1. Read each region file one-at-a-time (returns in long format).
census_output <- map(DATA_FILES, read_province_file) %>%
  bind_rows()

# 2. Pivot so each DA is a single row with one column per characteristic.
census_pivoted <- census_output %>%
  left_join(VARIABLE_LOOKUP, by = "CHARACTERISTIC_ID") %>%
  select(DAUID = ALT_GEO_CODE, vector, value = C1_COUNT_TOTAL) %>%
  pivot_wider(names_from = vector, values_from = value)

# ------ Validate ------

EXPECTED_COLUMNS <- VARIABLE_LOOKUP$vector

if (nrow(census_pivoted) > 50000) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: returned dataframe obviously does not have enough rows."
  )
}

if (all(EXPECTED_COLUMNS %in% names(census_wide))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: some variables missing from output.")
}

# ------ Save ------

write_parquet(census_pivoted, OUT_PATH)
message(sprintf(
  "Saved to %s --- END OF SCRIPT.",
  "data/processed_data/census_uncompressed_2021.parquet"
))
