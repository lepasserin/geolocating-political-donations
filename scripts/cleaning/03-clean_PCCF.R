# Purpose: Format Statistics Canada's Base PCCF TAB File into a hierarchical one-to-many spatial lookup table.
# Author: Benedict Cummins-Mburu
# Last Updated: 24 June 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------- Setup --------
library(tidyverse)
library(data.table)
PCCF_2024 <- data.table::fread("data/raw_data/raw_PCCF_2024.tab")
GAF_2021 <- data.table::fread("data/raw_data/raw_GAF_2021.csv")
FED_shapefile <- readRDS("data/clean_data/clean_FED_shapefile.rds")

# ------ Cleaning -------

# 1. Establish PC to DA 1:1 mapping
clean_PCCF_01 <- PCCF_2024 %>%
  filter(SLI == 1)

if (length(unique(clean_PCCF_01$PC)) == nrow(clean_PCCF_01)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Filtering by SLI = 1 did not result in unique PCs.")
}

if (length(unique(clean_PCCF_01$PC)) == length(unique(PCCF_2024$PC))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Filtering by SLI = 1 removed some PCs.")
}

if (all(str_detect(clean_PCCF_01$PC, "^[A-Z]\\d[A-Z]\\d[A-Z]\\d$"))) {
  message("Validation Passed.")
} else {
  message("Validation Failed: Some PCs are malformatted.")
}

# 2. Establish DA to FED 1:1 mapping

# ----- Constants ------

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
