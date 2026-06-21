# Purpose: Restrict cleaned IJF data to the range encompassing all donations that occured while the 2013 representational order was in effect.
# Author: Benedict Cummins-Mburu
# Last Updated: 19 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(arrow)
IJF_data <- read_parquet("data/clean_data/clean_data_IJF.parquet")
PCFRF_2022 <- read_parquet("data/clean_data/clean_PCFRF_2022.parquet")

# ------- Constants --------
VALID_DATE_RANGE <- c(
  lubridate::ymd("2015-08-02"),
  lubridate::ymd("2024-04-22")
)
VALID_FEDS <- unique(PCFRF_2022$FED)
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
    donation_date >= as.Date("2015-08-02"), # calling of the 2015 general election
    donation_date <= as.Date("2024-04-22") # earliest possible date of effective change of FEDs
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
valid_entries_outside_daterange <- IJF_data %>%
  filter(
    donation_date < as.Date("2015-08-02") |
      donation_date > as.Date("2024-04-22")
  ) %>%
  filter((electoral_event %in% VALID_EVENTS)) %>%
  filter(
    !(electoral_event %in%
      c(
        "Annual",
        "Quarterly",
        "Unknown leadership contest",
        "Unknown nomination contest"
      ))
  )

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
      str_detect(electoral_event_OG, "general election") ~ "General election",
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

# 7. Derive Donor Ridings from Postal Code (END OF CLEANING FOR APPENDIX)

created_donations_data_07 <- created_donations_data_06 %>%
  left_join(PCFRF_2022, by = c("donor_postal_code" = "PC")) %>%
  rename(donor_district = FED) %>%
  select(-c(FEDUID, PROVINCE))

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

# DIAGNOSTICS:
# unmatched_prop <- created_donations_data_07 %>%
#   filter(!is.na(donor_postal_code)) %>%
#   group_by(is.na(donor_district)) %>%
#   summarize(n = n(), sum = sum(total_amount))

# 8. Filter out Missing Donor Districts (END OF CLEANING FOR MAIN ANALYSIS)

created_donations_data_08 <- created_donations_data_07 %>%
  filter(!is.na(donor_district)) %>%
  select(-c(electoral_event_OG)) %>%
  mutate(
    is_out_of_district = case_when(
      is.na(recipient_district) ~ NA,
      TRUE ~ (donor_district != recipient_district)
    )
  )

if (all(!created_donations_data_08$is_aggregated)) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: `is_aggregated` is non-constant somehow.")
}
created_donations_data_08 <- created_donations_data_08 %>%
  select(-c(is_aggregated, amount_monetary, amount_non_monetary, rid))
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

# DIAGNOSTICS:
# missing_donor_FEDs <- created_donations_data_07 %>%
#   group_by(is.na(donor_district)) %>%
#   summarize(n = n(), sum = sum(total_amount))

# END.
created_donations_data_appendix <- created_donations_data_07
created_donations_data <- created_donations_data_08

# ----- Write to Parquet -----

write_parquet(
  created_donations_data,
  "data/processed_data/donations_data.parquet"
)
write_parquet(
  created_donations_data_appendix,
  "data/processed_data/donations_data_appendix.parquet"
)
message("Parquet saved successfully --- END OF SCRIPT.")
