# Purpose: Process `clean_data_IJF` into a dataset suitable for analysis. Saves resulting dataset to file.
# Author: Benedict Cummins-Mburu
# Last Updated: 9 July 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - Range restricted to donations dated within the 2013 representational order.
# - `donations_data_appendix` is full cleaned version.
# - `donations_data` is a subset of `donations_data_appendix`, where:
#         - donation is over \$200
#         - donor could be geolocated (location missing OR unmatched postal code)
#         - nomination contestants & by-elections are discarded

# Note: `donations_data` needs to be further subsetted in specific analysis files using `is_local` to get the study data.

# --------- Setup ----------
library(tidyverse)
library(arrow)
library(sf)
IJF_data <- read_parquet("data/clean_data/clean_data_IJF.parquet")
PCCF_lookup <- read_parquet("data/clean_data/PCCF_lookup.parquet") # to validate postal codes and recipient FED names
FED_lookup <- readRDS("data/clean_data/FED_lookup.rds") # to fetch valid FED names
FED_lookup <- FED_lookup %>%
  st_drop_geometry() %>%
  select(FEDUID, name)

# ------- Constants --------
VALID_DATE_RANGE <- c(
  lubridate::ymd("2015-08-02"),
  lubridate::ymd("2024-04-22")
)
VALID_FEDS <- unique(FED_lookup$name)
VALID_FED_ENTRIES <- c(VALID_FEDS, NA)
VALID_EVENTS <- c(
  "Unknown leadership contest",
  "Quarterly",
  "July 24, 2023 By-election",
  "June 19, 2023, By-elections",
  "December 12, 2022 By-election",
  "44th general election",
  "March 4, 2024, By-election",
  "Unknown nomination contest",
  "Annual",
  "December 3, 2018, By-election",
  "42nd general election",
  "February 25, 2019, By-elections",
  "April 3, 2017 By-elections",
  "October 23, 2017 By-elections",
  "December 11, 2017, By-elections",
  "June 18, 2018, By-election",
  "May 6, 2019 By-election",
  "October 24, 2016 By-election",
  "October 19, 2015 By-elections",
  "43rd general election",
  "October 26, 2020, By-elections"
)
VALID_LC_PARTY <- c(
  "Bloc Québécois",
  "Conservative Party of Canada",
  "Green Party of Canada",
  "Maverick Party",
  "New Democratic Party"
)
PR_LOOKUP <- data.frame(
  PRUID = c(
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
  ),
  province = c(
    "Newfoundland and Labrador",
    "Prince Edward Island",
    "Nova Scotia",
    "New Brunswick",
    "Quebec",
    "Ontario",
    "Manitoba",
    "Saskatchewan",
    "Alberta",
    "British Columbia",
    "Yukon",
    "Northwest Territories",
    "Nunavut"
  )
)

if (length(VALID_FEDS) == 338) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: The number of FEDs in `VALID_FEDS` is different than 338."
  )
}

# -------- Cleaning ---------

# 1. Subsetting and Column Renaming
created_donations_data_01 <- IJF_data %>%
  filter(
    donation_date >= VALID_DATE_RANGE[1],
    donation_date <= VALID_DATE_RANGE[2]
  ) %>%
  rename(
    recipient_district = electoral_district,
    donor_name = donor_full_name,
    recipient_name = recipient
  )

# 2. Standardizing Recipient Riding
created_donations_data_02 <- created_donations_data_01 %>%
  mutate(
    recipient_district = case_when(
      recipient_district ==
        "Beauport-Côte-de-Beaupré-Île d'Orléans-Charlevoix" ~ "Beauport--Côte-de-Beaupré--Île d’Orléans--Charlevoix",
      recipient_district ==
        "Leeds-Grenville-Thousand Islands and Rideau Lakes" ~ "Leeds--Grenville--Thousand Islands and Rideau Lakes",
      recipient_district == "" ~ NA,
      TRUE ~ recipient_district
    )
  )

# 3. Removing Invalid Recipient Ridings (n = 82)
created_donations_data_03 <- created_donations_data_02 %>%
  filter(recipient_district %in% VALID_FED_ENTRIES)

invalid_recipient_districts <- created_donations_data_02 %>%
  filter(!(recipient_district %in% VALID_FED_ENTRIES))

if (
  nrow(invalid_recipient_districts) / nrow(created_donations_data_02) < 0.00002
) {
  message("Validation Passed")
} else {
  stop(
    "Validation Error: a non-negligible proportion of rows were removed in Step 3."
  )
}
if (
  all(
    unique(created_donations_data_03$recipient_district) %in% VALID_FED_ENTRIES
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Some of the FED names in `created_donations_data` are invalid."
  )
}
if (
  nrow(invalid_recipient_districts) ==
    (nrow(created_donations_data_02) - nrow(created_donations_data_03))
) {
  message("Validation Passed")
} else {
  stop("Validation Error: Step 3 removed the wrong number of entries.")
}

# 4. Remove Invalid Events (n = 18)
invalid_events <- created_donations_data_03 %>%
  filter(!(electoral_event %in% VALID_EVENTS))

created_donations_data_04 <- created_donations_data_03 %>%
  filter(electoral_event %in% VALID_EVENTS)
if (nrow(invalid_events) / nrow(created_donations_data_03) < 0.00001) {
  message("Validation Passed")
} else {
  stop(
    "Validation Error: a non-negligible proportion of rows were removed in Step 4."
  )
}
if (
  nrow(invalid_events) ==
    (nrow(created_donations_data_03) - nrow(created_donations_data_04))
) {
  message("Validation Passed")
} else {
  stop("Validation Error: Step 4 removed the wrong number of entries.")
}

# DIAGNOSTICS:
# valid_entries_outside_daterange <- IJF_data %>%
#   filter(
#     donation_date < as.Date("2015-08-02") |
#       donation_date > as.Date("2024-04-22")
#   ) %>%
#   filter((electoral_event %in% VALID_EVENTS)) %>%
#   filter(
#     !(electoral_event %in%
#       c(
#         "Annual",
#         "Quarterly",
#         "Unknown leadership contest",
#         "Unknown nomination contest"
#       ))
#   )

# 5. Remove Invalid Leadership Events (n = 3 liberal events)

invalid_liberal_events <- created_donations_data_04 %>%
  filter(political_entity == "Leadership contestants") %>%
  filter(political_party == "Liberal Party of Canada")

invalid_events <- created_donations_data_04 %>%
  filter(political_entity == "Leadership contestants") %>%
  filter(!(political_party %in% VALID_LC_PARTY))

if (nrow(invalid_events) == nrow(invalid_liberal_events)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: some invalid events are non-Liberal.")
}

created_donations_data_05 <- created_donations_data_04 %>%
  filter(
    (political_entity != "Leadership contestants") |
      political_party %in% VALID_LC_PARTY
  )

if (nrow(invalid_liberal_events) / nrow(created_donations_data_04) < 0.000001) {
  message("Validation Passed")
} else {
  stop(
    "Validation Error: a non-negligible proportion of rows were removed in Step 5."
  )
}
if (
  nrow(invalid_events) ==
    (nrow(created_donations_data_04) - nrow(created_donations_data_05))
) {
  message("Validation Passed")
} else {
  stop("Validation Error: Step 5 removed the wrong number of entries.")
}

# DIAGNOSTIC
# diagnose_lc <- created_donations_data_04 %>%
#   filter(political_entity == "Leadership contestants") %>%
#   group_by(political_party) %>%
#   summarize(n = n())
# prop_nom <- created_donations_data_04 %>%
#   mutate(is_nom = (political_entity == "Nomination contestants")) %>%
#   group_by(is_nom) %>%
#   summarize(n = n(), sum = sum(amount_monetary + amount_non_monetary))

# 6. Group Events and Validate General Election Date Ranges

created_donations_data_06 <- created_donations_data_05 %>%
  mutate(electoral_event_OG = electoral_event) %>%
  mutate(
    electoral_event = case_when(
      electoral_event_OG %in% c("Quarterly", "Annual") ~ "Non-electoral",
      electoral_event_OG %in%
        c("Unknown leadership contest") ~ "Leadership contest",
      electoral_event_OG %in%
        c("Unknown nomination contest") ~ "Nomination contest",
      str_detect(electoral_event_OG, "general election") ~ electoral_event_OG,
      str_detect(electoral_event_OG, "By-election") ~ "By-election",
      TRUE ~ NA
    )
  ) %>%
  mutate(total_amount = amount_monetary + amount_non_monetary)

if (all(!is.na(created_donations_data_06$electoral_event))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some electoral events were mapped to NA.")
}
if (all(created_donations_data_06$total_amount > 0)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some total amounts are 0 or less.")
}

# NOTE: election date anomalies NOT removed

created_donations_data_06 <- created_donations_data_06 %>%
  mutate(
    general_election_period = case_when(
      donation_date >= as.Date("2015-08-02") &
        donation_date <= as.Date("2015-10-19") ~ "42nd general election",
      donation_date >= as.Date("2019-09-11") &
        donation_date <= as.Date("2019-10-21") ~ "43rd general election",
      donation_date >= as.Date("2021-08-15") &
        donation_date <= as.Date("2021-09-20") ~ "44th general election",
      TRUE ~ "None"
    )
  ) %>%
  mutate(is_general_election_period = general_election_period != "None")

# 7. Derive Entire Spatial Stack (DA, FED, and PR) from Postal Code using PCCF_lookup.
# NOTE: also used FED_lookup and PR_LOOKUP to match IDs to names (more usable).
# NOTE: also flags donations under \$200.
# NOTE: also flags donations to nomination contestants.

created_donations_data_07 <- created_donations_data_06 %>%
  left_join(PCCF_lookup, by = c("donor_postal_code" = "PC")) %>%
  mutate(
    FEDUID = as.character(FEDUID),
    PRUID = as.character(PRUID)
  ) %>%
  left_join(FED_lookup, by = "FEDUID") %>%
  left_join(PR_LOOKUP, by = "PRUID") %>%
  rename(
    donor_dissemination_area = DAUID,
    donor_district = name,
    donor_province = province
  ) %>%
  select(-c(PRUID, FEDUID))

# Flagging.
created_donations_data_07 <- created_donations_data_07 %>%
  mutate(
    is_under_two_hundred = ifelse(is_aggregated, TRUE, total_amount < 200)
  ) %>%
  mutate(
    is_nomination_contestant = political_entity == "Nomination contestants"
  ) %>%
  mutate(
    is_by_election = electoral_event == "By-election"
  )

if (
  all(
    unique(created_donations_data_07$donor_district) %in% VALID_FED_ENTRIES
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Some of the donor FED names are invalid."
  )
}

DA_exists <- created_donations_data_07 %>%
  filter(!is.na(donor_dissemination_area)) %>%
  select(donor_dissemination_area, donor_district, donor_province)
if (all(complete.cases(DA_exists))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: some rows with a DA have missing district or province."
  )
}

# 8. Subsetting `donations_data_appendix` to Create `donations_data`.
#     - 8.1: remove entries under $200.
#     - 8.2: remove donations where donor could not be geolocated.
#     - 8.3: remove donations to nomination contestants.
#     - 8.3: select for only necessary columns.
#     - 8.4: add `is_local` and `is_out_of_district` flags.
#     - 8.5: remove `is_local` cases where `total_amount` > 5000 (candidiates), or > 3450 (EDAs).
#     - 8.6: remove By-elections and donations to Candidates sent outside of general elections.

# Main Filtering Step
created_donations_data_08 <- created_donations_data_07 %>%
  filter(!is.na(donor_district)) %>%
  filter(!is_under_two_hundred) %>%
  filter(!is_nomination_contestant) %>%
  filter(!is_by_election) %>%
  mutate(
    is_out_of_district = case_when(
      is.na(recipient_district) ~ NA,
      TRUE ~ (donor_district != recipient_district)
    )
  )

# Additional Amount Filtering for Localized Entities (removes n = 8 entries)
created_donations_data_08 <- created_donations_data_08 %>%
  mutate(
    is_local = political_entity %in% c("Registered associations", "Candidates")
  ) %>%
  filter(
    !(is_local &
      (political_entity == "Registered associations") &
      (total_amount > 3450))
  ) %>% # removes ~3 entries only
  filter(
    !(is_local &
      (political_entity == "Candidates") &
      (total_amount > 5000))
  ) # removes ~5 entries only


if (all(!created_donations_data_08$is_aggregated)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: `is_aggregated` is non-constant somehow.")
}
if (all(!created_donations_data_08$is_under_two_hundred)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: `is_under_two_hundred` is non-constant somehow.")
}
if (all(!created_donations_data_08$is_nomination_contestant)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: `is_under_two_hundred` is non-constant somehow.")
}
created_donations_data_08 <- created_donations_data_08 %>%
  select(
    -c(
      is_by_election,
      is_nomination_contestant,
      is_under_two_hundred,
      is_aggregated,
      amount_monetary,
      amount_non_monetary,
      rid,
      electoral_event_OG,
      donor_location,
      donation_year,
      donor_postal_code
    )
  )
prop_ood_na <- mean(is.na(created_donations_data_08$is_out_of_district))
prop_national_entity <- mean(
  created_donations_data_08$political_entity %in%
    c("Leadership contestants", "Registered parties")
)
if (prop_ood_na == prop_national_entity) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Evidence that OOD NAs are not the same as National Entities."
  )
}

# END.
created_donations_data_appendix <- created_donations_data_07
created_donations_data <- created_donations_data_08

# ----- Write to Parquet -----

write_parquet(
  created_donations_data,
  "data/analysis_data/donations_data.parquet"
)
write_parquet(
  created_donations_data_appendix,
  "data/analysis_data/donations_data_full.parquet"
)
message("Parquet saved successfully --- END OF SCRIPT.")
