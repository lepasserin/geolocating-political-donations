# Purpose: Format Statistics Canada's Base PCCF TAB File into a hierarchical spatial lookup table.
# Author: Benedict Cummins-Mburu
# Last Updated: 25 Jun 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - 35% of entries dropped during SLI step.
# - 0.7% of postal codes dropped since did not refer to a valid DA or FED.
# - 0.5% of postal codes dropped resolving the DA to FED issue. Wanted to avoid reassigning PCs, so dropped them instead.

# ------- Setup --------
library(tidyverse)
library(data.table)
library(arrow)
PCCF_2024 <- data.table::fread("data/raw_data/raw_PCCF_2024.tab")
DA_lookup <- readRDS("data/analysis_data/DA_lookup.rds")
FED_lookup <- readRDS("data/analysis_data/FED_lookup.rds")

# ----- Constants ------

VALID_PRs <- c(
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
VALID_DAs <- as.character(DA_lookup$DAUID)
VALID_FEDs <- as.character(FED_shapefile$FEDUID)
DONATIONS_DATA_POSTAL_CODES <- unique(donations_data_pc$donor_postal_code)

# ------ Helpers -------

misalignment_exists <- function(df, from, to) {
  counts <- df %>%
    distinct(.data[[from]], .data[[to]]) %>%
    count(.data[[from]], name = "n_targets")
  return(any(counts$n_targets > 1))
}

modal_DA_to_FED_map <- function(df) {
  df %>%
    count(DAUID, FEDUID, name = "n") %>%
    group_by(DAUID) %>%
    arrange(desc(n), FEDUID, .by_group = TRUE) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(DAUID, FEDUID_mode = FEDUID)
}

# ------ Cleaning ------

clean_PCCF_01 <- PCCF_2024 %>%
  rename(
    FEDUID = FED13uid,
    DAUID = DAuid,
    PRUID = PR
  ) %>%
  filter(SLI == 1) %>%
  select(PC, DAUID, FEDUID, PRUID)

clean_PCCF_02 <- clean_PCCF_01 %>%
  filter(DAUID != 0)

clean_PCCF_03 <- clean_PCCF_02 %>%
  filter(FEDUID != 0)

clean_PCCF_04 <- clean_PCCF_03 %>%
  filter(DAUID %in% VALID_DAs)

clean_PCCF_05 <- clean_PCCF_04 %>%
  filter(FEDUID %in% VALID_FEDs)

clean_PCCF_06 <- clean_PCCF_05 %>%
  filter(PRUID %in% VALID_PRs)

da_fed_mode <- modal_DA_to_FED_map(clean_PCCF_06)
clean_PCCF_07 <- clean_PCCF_06 %>%
  left_join(da_fed_mode, by = "DAUID") %>%
  filter(FEDUID == FEDUID_mode) %>%
  select(PC, DAUID, FEDUID, PRUID)

clean_PCCF <- clean_PCCF_07

# ------ Validation -----

# 1. Validate the Integrity and Uniqueness of the PCs
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

# 2. Assess the Loss of Missing DAs, FEDs, or PRs
if (all(clean_PCCF_01$PRUID %in% VALID_PRs)) {
  message("Validation Passed.")
} else {
  message("Validation Failed: Some PRs are missing.")
}
if (
  ((nrow(clean_PCCF_01) - nrow(clean_PCCF_03)) / nrow(clean_PCCF_01)) < 0.007
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Post-SLI data rows are missing over 0.7% of DAs or FEDs."
  )
}

# 3. Verify the Validity of the remaining PRs, DAs, and FEDs
if (nrow(clean_PCCF_03) == nrow(clean_PCCF_06)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: One of DAs, FEDs, or PRs in the lookup table is invalid."
  )
}

# 4. Assess the Loss from the Restrictive Mapping Steps
if (!misalignment_exists(clean_PCCF_06, "PC", "PRUID")) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some PCs map to multiple provinces.")
}
if (!misalignment_exists(clean_PCCF_06, "DAUID", "PRUID")) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some DAs map to multiple provinces.")
}
if (!misalignment_exists(clean_PCCF_06, "FEDUID", "PRUID")) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some FEDs map to multiple provinces.")
}
if (
  ((nrow(clean_PCCF_06) - nrow(clean_PCCF_07)) / nrow(clean_PCCF_01)) < 0.005
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Post-SLI data rows are missing over 0.7% of DAs or FEDs."
  )
}
if (!misalignment_exists(clean_PCCF_07, "DAUID", "FEDUID")) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some DAs still map to multiple FEDs.")
}

# -------- Save --------
# END.
rows_discarded_total <- nrow(clean_PCCF_01) - nrow(clean_PCCF)
perc_rows_discarded <- round(
  rows_discarded_total / nrow(clean_PCCF_01) * 100,
  5
) # 1.083% of postal codes were lost ; 35% were lost through SLI step.

write_parquet(
  clean_PCCF,
  "data/analysis_data/PCCF_lookup.parquet"
)
message("Parquet saved successfully --- END OF SCRIPT.")
