# Purpose: Clean and Validate a Dataset of Candidate-bound Donations During the 2015 and 2019 General Elections, Augmented with Candidate Characteristics, OOD Distances, Competitiveness Measures, and Census Characteristics.
# Author: Benedict Cummins-Mburu
# Last Updated: 8 Nov 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
#     - So we've got 93% of donations (91% of OODs) for which we know the ethnicity and competitiveness of their race (most important), plus gender, whether or not incumbent, occupation, past experience (may be useful as secondary result, or to verify the first result).

# ------ Setup -------
library(tidyverse)
library(arrow)
library(sf)

# 1. Load in `donations_data`
donations_data <- read_parquet("data/analysis_data/donations_data.parquet")

# 2. Load in datasets used to augment `donations_data`.
candidate_demographics <- read_csv(
  ("data/raw_data/candidate_demographics.csv"),
  col_names = c(
    "election_year",
    "destination_province",
    "destination_district_ID",
    "destination_district_name",
    "candidate_incumbency",
    "candidate_gender",
    "candidate_ethnicity", # 12 NA, 0 after cleaning.
    "candidate_occupation", # 1 NA, 1 after cleaning.
    "candidate_prior_experience", # 18 NA, 1 after cleaning.
    "candidate_party",
    "birth_year", # 37% missing, 42% after cleaning.
    "election_outcome",
    "margin_of_victory"
  )
) %>%
  select(-margin_of_victory, destination_district_name)
election_results <- read_parquet("data/analysis_data/election_results.parquet")

# 2. Load in validation datasets.
election_results <- read_parquet("data/analysis_data/election_results.parquet")
FED_lookup <- st_drop_geometry(readRDS("data/analysis_data/FED_lookup.rds"))
census_lookup <- read_parquet("data/analysis_data/census_lookup.parquet")
DA_lookup <- st_drop_geometry(readRDS("data/analysis_data/DA_lookup.rds"))
PCCF_lookup <- read_parquet("data/analysis_data/PCCF_lookup.parquet")

# ------ Cleaning & Validation -------

# 1. Scope `donations_data` and `demographic_data` so that both refer to: candidates in 2015/2019 elections.
donations_data_01 <- donations_data %>%
  filter(
    electoral_event %in% c("43rd general election", "42nd general election"),
    political_entity == "Candidates"
  )
demographic_data_01 <- demographic_data %>%
  filter(election_year %in% c(2015, 2019)) %>%
  filter(ethnicity != "Not known") # DECISION: Candidates with unknown ethnicities were dropped. Only increases the number of missing donations in the affinity analysis by 0.1%

# 2. Confirm that all FEDs in `demographic_data` are in `FED_lookup`, AND that all FEDs are represented.
if (setequal(demographic_data_01$recipient_district_ID, FED_lookup$FEDUID)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# 3. Add ID to `donations_data`
if (all(donations_data_01$recipient_district %in% FED_lookup$name)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
donations_data_03 <- donations_data_01 %>%
  left_join(
    select(
      FED_lookup,
      recipient_district = name,
      recipient_district_ID = FEDUID
    ),
    by = "recipient_district"
  )

# 4. Confirm that (party + district_name + election-year) uniquely identifying a row in `demographic_data`. Then, assess missingness.
test_04 <- demographic_data_01 %>%
  distinct(recipient_district_ID, party, election_year)
if (nrow(test_04) == nrow(demographic_data_01)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
demographic_data_04 <- demographic_data_01 %>%
  mutate(
    party = case_when(
      party == "Conservative" ~ "Conservative Party of Canada",
      party == "Liberal" ~ "Liberal Party of Canada",
      party == "NDP" ~ "New Democratic Party",
      party == "Green" ~ "Green Party of Canada",
      party == "Bloc" ~ "Bloc Québécois"
    )
  ) %>%
  mutate(
    recipient_district_ID = as.character(recipient_district_ID),
    electoral_event = if_else(
      election_year == 2015,
      "42nd general election",
      "43rd general election"
    )
  )
missingness_04 <- mean(
  !paste(
    donations_data_03$electoral_event,
    donations_data_03$recipient_district_ID,
    donations_data_03$political_party
  ) %in%
    paste(
      demographic_data_04$electoral_event,
      demographic_data_04$recipient_district_ID,
      demographic_data_04$party
    )
)
if (missingness_04 < 0.073) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}

# 5. Augment `donations_data` with candidate demographics.
donations_data_05 <- donations_data_03 %>%
  left_join(
    demographic_data_04,
    by = c(
      "electoral_event",
      "recipient_district_ID",
      "political_party" = "party"
    ),
    relationship = "many-to-one"
  ) %>%
  select(-margin_of_victory)

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
