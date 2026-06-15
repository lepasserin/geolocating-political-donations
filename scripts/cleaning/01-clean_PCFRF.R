# Purpose: Format Statistics Canada's PCFRF TXT File into a 1:1 Mapping, then Save as a Parquet.
# Author: Benedict Cummins-Mburu
# Last Updated: 2 June 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------- Setup --------
library(tidyverse)
library(arrow)
path_to_raw_PCFRF <- "data/raw_data/raw_PCFRF_2022.txt"

# ----- Constants ------

PROVINCE_CODES <- c(
  "10" = "Newfoundland and Labrador",
  "11" = "Prince Edward Island",
  "12" = "Nova Scotia",
  "13" = "New Brunswick",
  "24" = "Quebec",
  "35" = "Ontario",
  "46" = "Manitoba",
  "47" = "Saskatchewan",
  "48" = "Alberta",
  "59" = "British Columbia",
  "60" = "Yukon",
  "61" = "Northwest Territories",
  "62" = "Nunavut"
)

# ------- Parse TXT and Establish 1:1 Mapping --------

# Define the exact character positions for the columns
col_positions <- fwf_positions(
  start = c(1, 7, 12),
  end = c(6, 11, 67),
  col_names = c("postal_code", "fed_uid", "fed_name")
)

# Read TXT file
fed_mapping <- read_fwf(
  file = path_to_raw_PCFRF,
  col_positions = col_positions,
  col_types = cols(.default = col_character()),
  trim_ws = TRUE
)

# Reduce to a  1:1 mapping
final_mapping_df1 <- fed_mapping %>%
  mutate(
    FED = fed_name,
    FEDUID = fed_uid,
    PC = postal_code,
    PROVINCE = PROVINCE_CODES[str_sub(FEDUID, 1, 2)]
  ) %>%
  select(FED, PC, FEDUID, PROVINCE) %>%
  distinct(PC, .keep_all = TRUE)

# Switch from Latin-1 to UTF-8

final_mapping_df <- final_mapping_df1 %>%
  mutate(FED = iconv(FED, from = "Windows-1252", to = "UTF-8"))

# ------- Validation --------

if (all(!is.na(final_mapping_df$PROVINCE))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some FEDs could not be mapped to Provinces.")
}


# ------- Save as a CSV --------

write_parquet(
  final_mapping_df,
  "data/clean_data/clean_PCFRF_2022.parquet"
)
