# Purpose: Clean and validate `census_uncompressed_2021`. Save resulting dataframe to file.
# Author: Benedict Cummins-Mburu
# Last Updated: 29 June 2026
# STATUS: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup -------
library(tidyverse)
library(arrow)
census_uncompressed <- read_parquet(
  "data/processed_data/census_uncompressed_2021.parquet"
)
PCCF_lookup <- read_parquet("data/analysis_data/PCCF_lookup.parquet")

# ------ Constants ------
VALID_DAs <- unique(PCCF_lookup$DAUID)
QUARTER_SAMPLE_VARIABLES_DEP <- c(
  "vismin_total",
  "vismin_south_asian",
  "vismin_chinese",
  "vismin_black",
  "vismin_filipino",
  "vismin_arab",
  "vismin_latin_american",
  "vismin_southeast_asian",
  "vismin_west_asian",
  "vismin_korean",
  "vismin_japanese",
  "pop_15plus",
  "postsec_count"
)
ETHNICITY_COUNT_VARS <- c(
  "vismin_total",
  "vismin_south_asian",
  "vismin_chinese",
  "vismin_black",
  "vismin_filipino",
  "vismin_arab",
  "vismin_latin_american",
  "vismin_southeast_asian",
  "vismin_west_asian",
  "vismin_korean",
  "vismin_japanese"
)
EDUCATION_COUNT_VARS <- c("postsec_count")
ALWAYS_PRESENT_COLS <- c(
  "DAUID",
  "population",
  "density_per_sqkm",
  "land_area_sqkm"
)
DEPENDENT_COLS <- setdiff(names(census_uncompressed), ALWAYS_PRESENT_COLS)

# ------ Cleaning -------

# 1. Chonfirm that all DAs present in the PCCF are also present in the Census. Also confirm uniqueness.
if (all(VALID_DAs %in% unique(census_uncompressed$DAUID))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Some DAs in the PCCF lookup cannot be found in the census output."
  )
}
if (length(unique(census_uncompressed$DAUID)) == nrow(census_uncompressed)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: DAs in census output are not unique.")
}

# 2. Remove DAs in the Census that do not appear in the PCCF.
invalid_DAs <- census_uncompressed %>% # 8761 cases
  filter(!(DAUID %in% VALID_DAs))

clean_census_01 <- census_uncompressed %>%
  filter(DAUID %in% VALID_DAs)

if (nrow(clean_census_01) == length(unique(PCCF_lookup$DAUID))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: DAs in Census and PCCF do not form a bijectiv mapping."
  )
}

# ---- Loss Accounting -----

# 0. Proportion of rows missing at least one entry.
incomplete_rows <- clean_census_01[!complete.cases(clean_census_01), ] # 1582 cases
prop_incomplete_any <- nrow(incomplete_rows) / nrow(clean_census_01)
message(sprintf(
  "Rows missing at least one value: %d (%.2f%%).",
  nrow(incomplete_rows),
  prop_incomplete_any * 100
))

# 1. Proportion of rows missing everything.
NA_pop <- clean_census_01 %>% # 26 cases
  filter(is.na(population)) %>%
  select(-c(land_area_sqkm, DAUID))
if (all(is.na(NA_pop))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some rows with NA `population` have non-NA values in other variables."
  )
}

# 2. non-population variables are missing.
fully_missing_cases <- clean_census_01[
  rowSums(!is.na(clean_census_01[, DEPENDENT_COLS])) == 0,
]

# 3. 25% (long-form) variables missing.
NA_pop_private_households <- clean_census_01 %>%
  filter(is.na(pop_private_households))
if (all(is.na(NA_pop_private_households[, QUARTER_SAMPLE_VARIABLES_DEP]))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some rows with NA `pop_private_households` have a non-NA 25% variable."
  )
}

clean_census_flagged <- clean_census_01 %>%
  mutate(
    flag_missing_population = is.na(population),
    flag_mostly_missing = rowSums(!is.na(across(all_of(DEPENDENT_COLS)))) == 0,
    flag_missing_25pct = is.na(pop_private_households)
  )

# ------ Validate Hierarchy ------

# Confirm the stages are strictly nested:
#   missing_population  =>  mostly_missing  =>  missing_25pct
hierarchy_ok_1 <- with(
  clean_census_flagged,
  all(!flag_missing_population | flag_mostly_missing)
)
hierarchy_ok_2 <- with(
  clean_census_flagged,
  all(!flag_mostly_missing | flag_missing_25pct)
)

if (hierarchy_ok_1 && hierarchy_ok_2) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: missingness stages are not strictly hierarchical."
  )
}

clean_census_02 <- clean_census_flagged %>%
  mutate(
    missing_stage = case_when(
      flag_missing_population ~ "missing_population",
      flag_mostly_missing ~ "mostly_missing",
      flag_missing_25pct ~ "missing_25pct",
      is.na(median_hh_income) ~ "missing_income_only",
      TRUE ~ "complete"
    ),
    missing_stage = factor(
      missing_stage,
      levels = c(
        "complete",
        "missing_income_only",
        "missing_25pct",
        "mostly_missing",
        "missing_population"
      )
    )
  ) %>%
  select(-c(flag_missing_population, flag_mostly_missing, flag_missing_25pct))

# Validate that the only remaining incompleteness among otherwise-complete rows
# is household income (i.e. "missing_income_only" is correctly the residual).
income_only_rows <- clean_census_02 %>%
  filter(missing_stage == "missing_income_only")
non_income_cols <- setdiff(names(clean_census_01), "median_hh_income")
if (all(complete.cases(income_only_rows[, non_income_cols]))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some `missing_income_only` rows are missing a variable other than median_hh_income."
  )
}

# ---- Convert Counts to Proportions ----

denominators_valid <- clean_census_02 %>%
  filter(
    (!is.na(pop_private_households) & pop_private_households <= 0) |
      (!is.na(pop_15plus) & pop_15plus <= 0)
  )
if (nrow(denominators_valid) == 0) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some denominator variables (pop_private_households or pop_15plus) are zero or negative."
  )
}

ethnicity_exceeds <- clean_census_02 %>%
  filter(if_any(
    all_of(ETHNICITY_COUNT_VARS),
    ~ !is.na(.x) & !is.na(pop_private_households) & .x > pop_private_households
  ))
if (nrow(ethnicity_exceeds) == 0) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some ethnicity counts exceed pop_private_households."
  )
}

minority_group_vars <- setdiff(ETHNICITY_COUNT_VARS, "vismin_total")
GROUP_TOTAL_TOLERANCE <- 10
group_exceeds_total <- clean_census_02 %>%
  filter(if_any(
    all_of(minority_group_vars),
    ~ !is.na(.x) &
      !is.na(vismin_total) &
      .x > (vismin_total + GROUP_TOTAL_TOLERANCE)
  ))
if (nrow(group_exceeds_total) == 0) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some visible minority group counts exceed vismin_total by more than 10."
  )
}

education_exceeds <- clean_census_02 %>%
  filter(
    !is.na(postsec_count) & !is.na(pop_15plus) & postsec_count > pop_15plus
  )
if (nrow(education_exceeds) == 0) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: postsec_count exceeds pop_15plus.")
}

clean_census_03 <- clean_census_02 %>%
  mutate(
    across(all_of(ETHNICITY_COUNT_VARS), ~ .x / pop_private_households),
    across(all_of(EDUCATION_COUNT_VARS), ~ .x / pop_15plus)
  ) %>%
  rename(postsec_prop = postsec_count)

PROPORTION_VARS <- c(ETHNICITY_COUNT_VARS, "postsec_prop")
proportions_out_of_range <- clean_census_03 %>%
  filter(if_any(
    all_of(PROPORTION_VARS),
    ~ !is.na(.x) & (.x < 0 | .x > 1)
  ))
if (nrow(proportions_out_of_range) == 0) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: some computed proportions fall outside [0, 1].")
}

# ------ Remove Redundant Denominators ------

clean_census_04 <- clean_census_03 %>%
  select(-c(pop_private_households, pop_15plus))

# END.
clean_census <- clean_census_04

# ------ Save to File ------

write_parquet(
  clean_census,
  "data/analysis_data/census_lookup.parquet"
)
message("Parquet saved successfully --- END OF SCRIPT.")
