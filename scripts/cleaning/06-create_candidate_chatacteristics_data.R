# Purpose: Clean and Validate a Dataset of Candidate-bound Donations, Augmented with Candidate Characteristics, OOD Distances, Competitiveness Measures, and Census Characteristics.
# Author: Benedict Cummins-Mburu
# Last Updated: 8 Nov 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------ Setup -------
library(tidyverse)
library(haven)
library(arrow)
library(sf)

# 1. Load in `donations_data`, but only candidate-bound donations.
donations_data <- read_parquet("data/analysis_data/donations_data.parquet") %>%
  filter(political_entity == "Candidates") %>%
  select(
    -c(
      political_entity,
      recipient_name,
      is_local,
      is_general_election_period,
      general_election_period
    )
  ) %>%
  rename(
    candidate_party = political_party,
    destination_district = recipient_district
  ) %>%
  mutate(
    election_year = case_when(
      electoral_event == "44th general election" ~ 2021,
      electoral_event == "43rd general election" ~ 2019,
      electoral_event == "42nd general election" ~ 2015
    )
  ) %>%
  select(-electoral_event)

# 2. Load in datasets used to augment `donations_data`.
candidate_demography <- read_dta("data/raw_data/candidate_demography.dta") %>%
  mutate(across(where(is.labelled), ~ as.character(as_factor(.x)))) %>%
  transmute(
    election_year = Year,
    destination_province = Province,
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
election_results <- read_parquet("data/analysis_data/election_results.parquet")
census_lookup <- read_parquet("data/analysis_data/census_lookup.parquet")

# 2. Load in lookup datasets.
FED_lookup <- st_drop_geometry(readRDS("data/analysis_data/FED_lookup.rds"))
DA_lookup <- st_drop_geometry(readRDS("data/analysis_data/DA_lookup.rds"))
PCCF_lookup <- read_parquet("data/analysis_data/PCCF_lookup.parquet")

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
if (all(donations_data$destination_district %in% FED_lookup$name)) {
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
      destination_district = name,
      destination_district_ID = as.character(FEDUID)
    ),
    by = "destination_district",
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
      candidate_out_TNB
    )
  )

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

# 2. Augment `donations_data` with target candidate demographics.

# 6. Augment `donations_data` with donor census characteristics (by donor DA).
donations_data_06 <- donations_data_05 %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area)) %>%
  left_join(
    census_lookup,
    by = c("donor_dissemination_area" = "DAUID"),
    relationship = "many-to-one"
  )

# 7. Augment `donations_data` with donor-recipient distance (`donation_distance`, in km).
# NOTE: same calculation as `scripts/other/01-distance_simulation.R` (haversine distance between donor DA centroid and recipient FED centroid; 0 if in-district).
da_to_fed <- PCCF_lookup %>%
  distinct(DAUID, FEDUID) %>%
  distinct(DAUID, .keep_all = TRUE) %>%
  transmute(DAUID = as.character(DAUID), donor_FEDUID = as.character(FEDUID))
da_centroids <- DA_lookup %>%
  transmute(
    DAUID = as.character(DAUID),
    donor_lat = as.numeric(centroid_lat),
    donor_lon = as.numeric(centroid_lon)
  )
fed_centroids <- FED_lookup %>%
  transmute(
    recipient_district_ID = FEDUID,
    fed_lat = as.numeric(centroid_lat),
    fed_lon = as.numeric(centroid_lon)
  )
donations_data_07 <- donations_data_06 %>%
  left_join(da_to_fed, by = c("donor_dissemination_area" = "DAUID")) %>%
  left_join(da_centroids, by = c("donor_dissemination_area" = "DAUID")) %>%
  left_join(fed_centroids, by = "recipient_district_ID") %>%
  mutate(
    dlat = (fed_lat - donor_lat) * pi / 180, # Vectorised haversine great-circle distance (in kilometres).
    dlon = (fed_lon - donor_lon) * pi / 180,
    hav = sin(dlat / 2)^2 +
      cos(donor_lat * pi / 180) * cos(fed_lat * pi / 180) * sin(dlon / 2)^2,
    donation_distance = if_else(
      donor_FEDUID == recipient_district_ID,
      0,
      6371 * 2 * atan2(sqrt(hav), sqrt(1 - hav))
    )
  ) %>%
  select(
    -c(donor_FEDUID, donor_lat, donor_lon, fed_lat, fed_lon, dlat, dlon, hav)
  )

# 8. Augment `donations_data` with competitiveness metrics.
district_margins <- election_results %>%
  select(
    recipient_district_ID = FEDUID,
    starts_with("margin_of_victory_4"),
    starts_with("winning_party_4")
  ) %>%
  pivot_longer(
    -recipient_district_ID,
    names_to = c(".value", "electoral_event"),
    names_pattern = "(margin_of_victory|winning_party)_(.*)"
  ) %>%
  rename(
    district_margin_of_victory = margin_of_victory,
    district_winning_party = winning_party
  )
donations_data_08 <- donations_data_07 %>%
  left_join(
    district_margins,
    by = c("electoral_event", "recipient_district_ID"),
    relationship = "many-to-one"
  )

# ------ Save Data -------
write_parquet(
  donations_data_08,
  "data/analysis_data/candidate_model_data.parquet"
)
