# Purpose: Clean and Validate a Dataset of Candidate-bound Donations, Augmented with Candidate, Race, and Census Characteristics.
# Author: Benedict Cummins-Mburu
# Last Updated: 22 Nov 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------ Setup -------
library(tidyverse)
library(haven)
library(arrow)
library(sf)

# 0. Load in lookup datasets.
FED_lookup <- readRDS("data/analysis_data/FED_lookup.rds") %>%
  st_drop_geometry() %>%
  mutate(FEDUID = as.character(FEDUID))
PCCF_lookup <- read_parquet("data/analysis_data/PCCF_lookup.parquet") %>%
  mutate(FEDUID = as.character(FEDUID))

# 1. Load in `donations_data`, but only candidate-bound donations.
donations_data <- read_parquet("data/analysis_data/donations_data.parquet") %>%
  filter(political_entity == "Candidates") %>%
  select(
    -c(
      political_entity,
      recipient_name,
      donor_province,
      is_local,
      is_general_election_period,
      general_election_period
    )
  ) %>%
  rename(candidate_party = political_party) %>%
  mutate(
    election_year = case_when(
      electoral_event == "44th general election" ~ 2021,
      electoral_event == "43rd general election" ~ 2019,
      electoral_event == "42nd general election" ~ 2015
    )
  ) %>%
  select(-electoral_event)

# 2. Load in datasets used to augment `donations_data`.
# 2.1. Candidate demography.
candidate_demography <- read_dta("data/raw_data/candidate_demography.dta") %>%
  mutate(across(where(is.labelled), ~ as.character(as_factor(.x)))) %>%
  transmute(
    election_year = Year,
    destination_district_ID = as.character(DistrictID),
    destination_district_name = DistrictName,
    candidate_party = Party,
    candidate_gender = Gender,
    candidate_ethnicity = Race,
    candidate_incumbency = Incumbent,
    candidate_challenger = Challenger,
    candidate_prior_experience = OtherLevels,
    candidate_out_LGBTQ2S = OutLGBTQ2S,
    candidate_out_TNB = OutTNB,
    candidate_out_2S = Out2S,
    candidate_votes = Votes,
    candidate_vote_share = VoteShare,
    candidate_vote_share_lag = VoteShareLag,
    candidate_elected = Elected,
    candidate_win_margin = WinMargin,
    candidate_win_margin_lag = WinMarginLag
  )

# 2.2. Margins of victory.
election_results <- read_parquet("data/analysis_data/election_results.parquet")

# 2.3. District-level census data (`census_lookup_FED`).
census_lookup <- read_parquet("data/analysis_data/census_lookup.parquet") %>%
  mutate(DAUID = as.character(DAUID))
DAUID_to_FEDUID <- PCCF_lookup %>%
  distinct(DAUID, FEDUID) %>%
  transmute(DAUID = as.character(DAUID), FEDUID = as.character(FEDUID))
census_FED_summary <- census_lookup %>%
  filter(missing_stage == "complete") %>%
  left_join(DAUID_to_FEDUID, by = "DAUID") %>%
  group_by(FEDUID) %>%
  summarise(
    density_per_sqkm = sum(population) / sum(land_area_sqkm),
    avg_age = weighted.mean(avg_age, population),
    median_hh_income = weighted.mean(median_hh_income, population),
    postsec_prop = weighted.mean(postsec_prop, population),
    across(
      starts_with("vismin_"),
      ~ weighted.mean(.x, population)
    ),
    .groups = "drop"
  )

# 2.4. District region (same province-to-region grouping as `paper/paper_helpers.R`), joined onto `census_lookup_FED`.
REGION_LEVELS <- c(
  "Ontario",
  "Quebec",
  "British Columbia",
  "Prairies",
  "Atlantic Canada",
  "Territories"
)
REGION_BY_PRUID <- c(
  "10" = "Atlantic Canada",
  "11" = "Atlantic Canada",
  "12" = "Atlantic Canada",
  "13" = "Atlantic Canada",
  "24" = "Quebec",
  "35" = "Ontario",
  "46" = "Prairies",
  "47" = "Prairies",
  "48" = "Prairies",
  "59" = "British Columbia",
  "60" = "Territories",
  "61" = "Territories",
  "62" = "Territories"
)
census_lookup_FED <- FED_lookup %>%
  transmute(
    FEDUID,
    region = factor(
      unname(REGION_BY_PRUID[as.character(PRUID)]),
      levels = REGION_LEVELS
    )
  ) %>%
  left_join(census_FED_summary, by = "FEDUID", relationship = "one-to-one")

# ------ Cleaning & Validation -------

# 1. Confirm that all FEDs in `demographic_data` are in `FED_lookup`, AND that all FEDs are represented.
if (setequal(candidate_demography$destination_district_ID, FED_lookup$FEDUID)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# 2. Add `donor_district_ID` and `destination_district_ID` to `donations_data`.
if (all(donations_data$donor_district %in% FED_lookup$name)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
if (all(donations_data$recipient_district %in% FED_lookup$name)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
donations_data_02 <- donations_data %>%
  left_join(
    transmute(
      FED_lookup,
      donor_district = name,
      donor_district_ID = as.character(FEDUID)
    ),
    by = "donor_district",
    relationship = "many-to-one"
  ) %>%
  left_join(
    transmute(
      FED_lookup,
      recipient_district = name,
      destination_district_ID = as.character(FEDUID)
    ),
    by = "recipient_district",
    relationship = "many-to-one"
  )

# 3. Confirm that (party + district_name + election-year) uniquely identifying a row in `candidate_demography`. Then, assess missingness.
test_03 <- candidate_demography %>%
  distinct(destination_district_ID, candidate_party, election_year)
if (nrow(test_03) == nrow(candidate_demography)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
if (
  setequal(
    candidate_demography$candidate_party,
    c("Liberal", "Conservative", "New Democratic", "Bloc Quebecois", "Green")
  )
) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
candidate_demography_03 <- candidate_demography %>%
  mutate(
    candidate_party = case_when(
      candidate_party == "Conservative" ~ "Conservative Party of Canada",
      candidate_party == "Liberal" ~ "Liberal Party of Canada",
      candidate_party == "New Democratic" ~ "New Democratic Party",
      candidate_party == "Green" ~ "Green Party of Canada",
      candidate_party == "Bloc Quebecois" ~ "Bloc Québécois"
    )
  )
missingness_03 <- mean(
  !paste(
    donations_data_02$election_year,
    donations_data_02$destination_district_ID,
    donations_data_02$candidate_party
  ) %in%
    paste(
      candidate_demography_03$election_year,
      candidate_demography_03$destination_district_ID,
      candidate_demography_03$candidate_party
    )
)
if (missingness_03 < 0.071) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# 4. Add `candidate_ID` (formatted [Election Year]_FEDUID_[Party]) to `candidate_demograhy`, and remove unused candidate/race characteristics.
candidate_demography_04 <- candidate_demography_03 %>%
  mutate(
    candidate_ID = paste(
      election_year,
      destination_district_ID,
      candidate_party,
      sep = "_"
    )
  ) %>%
  select(
    -c(
      destination_district_name,
      candidate_votes,
      candidate_out_2S,
      candidate_out_TNB,
      candidate_win_margin,
      candidate_win_margin_lag
    )
  )

# 5. Reshape `election_results` to have margin of victory for each district-year pair.
margin_lag_2015 <- candidate_demography %>%
  filter(election_year == 2015) %>%
  group_by(FEDUID = destination_district_ID) %>%
  summarise(
    margin_of_victory_lag_2015 = if (all(is.na(candidate_win_margin_lag))) {
      NA_real_
    } else {
      max(candidate_win_margin_lag, na.rm = TRUE)
    },
    .groups = "drop"
  )
election_results_05 <- election_results %>%
  mutate(FEDUID = as.character(FEDUID)) %>%
  select(FEDUID, starts_with("margin_of_victory_4")) %>%
  pivot_longer(
    -FEDUID,
    names_to = "electoral_event",
    names_prefix = "margin_of_victory_",
    values_to = "margin_of_victory"
  ) %>%
  mutate(
    election_year = case_when(
      electoral_event == "44th general election" ~ 2021,
      electoral_event == "43rd general election" ~ 2019,
      electoral_event == "42nd general election" ~ 2015
    )
  ) %>%
  select(-electoral_event) %>%
  left_join(margin_lag_2015, by = "FEDUID", relationship = "many-to-one") %>%
  arrange(FEDUID, election_year) %>%
  group_by(FEDUID) %>%
  mutate(
    margin_of_victory_lag = if_else(
      election_year == 2015,
      margin_of_victory_lag_2015,
      lag(margin_of_victory)
    )
  ) %>%
  ungroup() %>%
  select(-margin_of_victory_lag_2015)
if (
  nrow(distinct(election_results_05, FEDUID, election_year)) ==
    nrow(election_results_05)
) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# 6. Confirm that every district-year pair in `donations_data` exists in `election_results`.
election_results_06 <- election_results_05 %>%
  filter(!is.na(margin_of_victory))
missingness_06 <- mean(
  !paste(
    donations_data_02$election_year,
    donations_data_02$destination_district_ID
  ) %in%
    paste(
      election_results_06$election_year,
      election_results_06$FEDUID
    )
)
if (missingness_06 == 0) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# ------ Donation Data Augmentation -------

# 1. Augment `donations_data` with target candidate demographics.
donations_data_a_01 <- donations_data_02 %>%
  left_join(
    candidate_demography_04,
    by = c(
      "election_year",
      "destination_district_ID",
      "candidate_party"
    ),
    relationship = "many-to-one"
  )

# 2. Augment `donations_data` with victory margins.
donations_data_a_02 <- donations_data_a_01 %>%
  left_join(
    election_results_06,
    by = c(
      "election_year",
      "destination_district_ID" = "FEDUID"
    ),
    relationship = "many-to-one"
  )

# 3. Augment `donations_data` with candidate riding's demogrpahics.
donations_data_a_03 <- donations_data_a_02 %>%
  left_join(
    census_lookup_FED,
    by = c(
      "destination_district_ID" = "FEDUID"
    ),
    relationship = "many-to-one"
  )

# ------ Save Data -------
write_parquet(
  donations_data_a_03,
  "data/analysis_data/candidate_model_data.parquet"
)
