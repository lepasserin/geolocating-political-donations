# Purpose: General Ceaning Script for Raw IJF Data.
# Author: Benedict Cummins-Mburu
# Last Updated: 19 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - `raw_data_IJF_fix2026-06-17.csv` is accurate as of June 17, 2026.
# - First round of cleaning. More specialized tasks will draw on this dataset later in the pipeline.
# - Script contain some validation code to justify the renoval of certain entries or columns.
# - Final dataframe represents all Individual contributions between 2004 and 2026.

# --------- Setup ----------
library(tidyverse) # for data cleaning
library(data.table) # for speedy CSV reading
library(arrow) # to save result as parquet
raw_IJF_data <- fread(
  "data/raw_data/raw_data_IJF_fix2026-06-17.csv",
  data.table = FALSE
)

# --- Constants ----

VALID_DATE_RANGE <- c(
  lubridate::ymd("2004-01-01"),
  lubridate::ymd("2026-06-17")
)
VALID_DONOR_TYPE <- "Individuals"

VALID_POLITICAL_ENTITIES <- c(
  # n = 5
  "Leadership contestants",
  "Nomination contestants",
  "Candidates",
  "Registered parties",
  "Registered associations"
)

VALID_POLITICAL_PARTIES <- c(
  # n = 37
  "Liberal Party of Canada",
  "New Democratic Party",
  "Bloc Québécois",
  "Conservative Party of Canada",
  "Green Party of Canada",
  "Christian Heritage Party of Canada",
  "Canadian Action Party",
  "Independent",
  "Progressive Canadian Party",
  "Libertarian Party of Canada",
  "Marijuana Party",
  "Marxist-Leninist Party of Canada",
  "First Peoples National Party of Canada",
  "Western Block Party",
  "United Party of Canada",
  "Parti Rhinocéros Party",
  "Pirate Party of Canada",
  "People's Political Power Party of Canada",
  "Animal Protection Party of Canada",
  "Newfoundland and Labrador First Party",
  "Communist Party of Canada",
  "People's Party of Canada",
  "Party for Accountability, Competency and Transparency",
  "Forces et Démocratie",
  "Canada Party",
  "Seniors Party of Canada",
  "National Advancement Party of Canada",
  "Alliance of the North",
  "National Citizens Alliance of Canada",
  "Canadian Nationalist Party",
  "Direct Democracy Party of Canada",
  "Parti pour l'Indépendance du Québec",
  "Maverick Party",
  "Free Party Canada",
  "Centrist Party of Canada",
  "Veterans Coalition Party of Canada",
  "Canadian Future Party",
  "Reforge Party"
)

# -------- Cleaning --------

# 0. Perform ingestion de-duping (normally done in IJF pipeline, special case)
raw_IJF_data <- raw_IJF_data %>% distinct()

# 1. Remove Columns `s`, `added` (since irrelevant).

if (all(unique(raw_IJF_data$s) %in% c("fd"))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: column `s` is not constant 'fd', should not be removed."
  )
}
if (length(unique(raw_IJF_data$rid)) == nrow(raw_IJF_data)) {
  message("Validation Passed.")
} else {
  stop("Validation Error: column `rid` is not unique, should address this.")
}
clean_data_01 <- raw_IJF_data %>%
  select(-c(s, added))

# 2. Remove Column `year` (since `donation_year` is equivalent).

if (all(!is.na(clean_data_01$donation_year))) {
  message("Validation Passed.")
} else {
  stop("Validation Error: some entries in column `donation_year` are missing.")
}
if (all(!is.na(clean_data_01$year))) {
  message("Validation Passed.")
} else {
  stop("Validation Error: some entries in column `year` are missing.")
}
if (all(clean_data_01$year %% 1 == 0)) {
  message("Validation Passed.")
} else {
  stop("Validation Error: some entries in column `year` are not integers.")
}
if (all(clean_data_01$donation_year %% 1 == 0)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: some entries in column `donation_year` are not integers."
  )
}
if (all(clean_data_01$donation_year == clean_data_01$year)) {
  message("Validation Passed.")
} else {
  stop("Validation Error: columns `donation_year` and `year` are not the same.")
}
clean_data_02 <- clean_data_01 %>%
  select(-year)

# 3. Check that all dates are present.

if (all(!is.na(clean_data_02$donation_date))) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Some dates are missing.")
}
clean_data_03 <- clean_data_02

# 4. Check that date and year aggree + remove invalid dates.

check_4 <- clean_data_03[
  (year(clean_data_03$donation_date) != clean_data_03$donation_year),
]
if (nrow(check_4) == 0) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Unhandled misalignment between columns `donation_date` and `donation_year`."
  )
}
invalid_dates_03 <- clean_data_03 %>%
  filter(
    donation_date < VALID_DATE_RANGE[1] | donation_date > VALID_DATE_RANGE[2]
  )
invalid_date_count_03 <- nrow(invalid_dates_03)
if ((invalid_date_count_03 / nrow(clean_data_03)) < 0.00001) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: There are a non-trivial amount of invalid dates."
  )
}
clean_data_04 <- clean_data_03 %>%
  filter(
    donation_date >= VALID_DATE_RANGE[1],
    donation_date <= VALID_DATE_RANGE[2]
  )
if ((nrow(clean_data_03) - nrow(clean_data_04)) == invalid_date_count_03) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Cleaning Step 4 removed the wrong number of rows.")
}

# 5. Remove Invalid Donor Types (< 0.4% of the data at this stage ; vast majority are from 2006 - 2004 (NOT VALIDATED)).

invalid_types_04 <- clean_data_04 %>%
  filter(donor_type != VALID_DONOR_TYPE)
invalid_types_count_04 <- nrow(invalid_types_04)
if ((invalid_types_count_04 / nrow(clean_data_04)) < 0.004) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: There are a non-trivial amount of non-Individual donors."
  )
}
clean_data_05 <- clean_data_04 %>%
  filter(donor_type == VALID_DONOR_TYPE) %>%
  select(-donor_type)
if ((nrow(clean_data_04) - nrow(clean_data_05)) == invalid_types_count_04) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Cleaning Step 5 removed the wrong number of rows.")
}

# 6. Remove Column `amount` (since `amount_non_monetary` is equivalent).

if (all(clean_data_05$amount == clean_data_05$amount_non_monetary)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Columns `amount` and `amount_non_monetary` are not equivalent."
  )
}
clean_data_06 <- clean_data_05 %>%
  select(-amount)

# 7. Clean Column `political_entity` (simple handling of 18 poorly formatted entries)

clean_data_07 <- clean_data_06 %>%
  mutate(political_entity = str_remove(political_entity, "^[^A-Za-z]*"))
if (all(clean_data_07$political_entity %in% VALID_POLITICAL_ENTITIES)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Cleaning Step 7 did not de-duplicate column `political_entity`."
  )
}
invalid_entities_06 <- clean_data_06 %>%
  filter(!(political_entity %in% VALID_POLITICAL_ENTITIES))
invalid_entities_count_06 <- nrow(invalid_entities_06)
n_entities_changed_07 <- sum(
  clean_data_07$political_entity != clean_data_06$political_entity
)
if (n_entities_changed_07 > 0) {
  message("Validation Passed.")
} else {
  stop("Valiation Error: Cleaning Step 7 did nothing, investigate further.")
}
if (n_entities_changed_07 == invalid_entities_count_06) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Cleaning Step 7 changed the wrong number of rows.")
}

# 8. Clean Column `political_party` (join "Independent" and "No Affiliation ; join "United Party of Canada (UP)" and "United Party of Canada"). Also match slightly different recipient and party names.

clean_data_08 <- clean_data_07 %>%
  mutate(
    political_party = case_when(
      political_party ==
        "United Party of Canada (UP)" ~ "United Party of Canada",
      political_party == "No Affiliation" ~ "Independent",
      TRUE ~ political_party
    )
  ) %>%
  mutate(
    recipient = case_when(
      recipient == "United Party of Canada (UP)" ~ "United Party of Canada",
      recipient ==
        "Competency and Transparency Party for Accountability" ~ "Party for Accountability, Competency and Transparency",
      TRUE ~ political_party
    )
  )

registered_party_check <- clean_data_08 %>%
  filter(political_entity == "Registered parties") %>%
  filter(political_party != recipient)

if (nrow(registered_party_check) == 0) {
  message("Validation Passed.")
} else {
  stop(
    paste0(
      "Validation Failed: ",
      nrow(registered_party_check),
      " rows where political_party != recipient for Registered Parties."
    )
  )
}
if (all(clean_data_08$political_party %in% VALID_POLITICAL_PARTIES)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Cleaning Step 8 did not de-duplicate column `political_party`."
  )
}
invalid_entities_07 <- clean_data_07 %>%
  filter(!(political_party %in% VALID_POLITICAL_PARTIES))
invalid_entities_count_07 <- nrow(invalid_entities_07)
n_entities_changed_08 <- sum(
  clean_data_08$political_party != clean_data_07$political_party
)
if (n_entities_changed_08 > 0) {
  message("Validation Passed.")
} else {
  stop("Valiation Error: Cleaning Step 8 did nothing, investigate further.")
}
if (n_entities_changed_08 == invalid_entities_count_07) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Cleaning Step 8 changed the wrong number of rows.")
}

# 9. Filter For Strictly Positive Donation Amounts

if (
  all(is.double(
    clean_data_08$amount_monetary + clean_data_08$amount_non_monetary
  )) &&
    all(
      !is.na(
        clean_data_08$amount_monetary + clean_data_08$amount_non_monetary
      )
    )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Some summed contribution amounts are missing or not numbers."
  )
}
clean_data_09 <- clean_data_08 %>%
  filter(amount_monetary + amount_non_monetary > 0)

invalid_amounts_08 <- clean_data_08 %>%
  filter(amount_monetary + amount_non_monetary <= 0)
if ((nrow(invalid_amounts_08) / nrow(clean_data_08)) < 0.0002) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: A non-trivial amount of rows were removed in Cleaning Step 9."
  )
}
if (nrow(clean_data_09) == (nrow(clean_data_08) - nrow(invalid_amounts_08))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Wrong number of rows were removed in Cleaning Step 9."
  )
}

# 10. Filter for Present, Reasonably Long Donor and Recipient Names

if (all(!is.na(clean_data_09$donor_full_name))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some donor names are NA")
}
if (all(!is.na(clean_data_09$recipient))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some recipient names are NA")
}

clean_data_10 <- clean_data_09 %>%
  filter(
    nchar(trimws(donor_full_name)) >= 4 &
      nchar(trimws(recipient)) >= 4
  )

invalid_names_09 <- clean_data_09 %>%
  filter(
    nchar(trimws(donor_full_name)) < 4 |
      nchar(trimws(recipient)) < 4
  )

if ((nrow(invalid_names_09) / nrow(clean_data_09)) < 0.00001) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: A non-trivial amount of rows were removed in Cleaning Step 10."
  )
}
if (nrow(clean_data_10) == (nrow(clean_data_09) - nrow(invalid_names_09))) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Wrong number of rows were removed in Cleaning Step 10."
  )
}

# 11. Identify Aggregate Rows, then Remove Non-Aggregate Rows Above $25,000

clean_data_11 <- clean_data_10 %>%
  mutate(is_aggregated = str_detect(donor_full_name, "^Contribut")) %>%
  mutate(donor_location = ifelse(is_aggregated, NA, donor_location))
filter(is_aggregated | (amount_monetary + amount_non_monetary <= 25000))

if (
  ((nrow(clean_data_10) - nrow(clean_data_11)) / nrow(clean_data_10)) < 0.00001
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: A non-trivial amount of rows were removed in Cleaning Step 11."
  )
}

# 12. Validate and Clean `electoral_event`.

if (all(!is.na(clean_data_11$electoral_event))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some electoral events are NA.")
}
missing_event_entities <- clean_data_11 %>%
  filter(is.na(electoral_event) | electoral_event == "") %>%
  distinct(political_entity) %>%
  pull(political_entity)

if (
  setequal(
    missing_event_entities,
    c("Leadership contestants", "Nomination contestants")
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Unexpected political entities are missing `electoral_event`."
  )
}
event_lead_nom <- clean_data_11 %>%
  filter(
    political_entity %in% c("Leadership contestants", "Nomination contestants")
  ) %>%
  distinct(electoral_event) %>%
  pull(electoral_event)
if (
  setequal(
    event_lead_nom,
    c("")
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Unexpected political entities are missing `electoral_event`."
  )
}
clean_data_12 <- clean_data_11 %>%
  mutate(
    electoral_event = case_when(
      political_entity ==
        "Leadership contestants" ~ "Unknown leadership contest",
      political_entity ==
        "Nomination contestants" ~ "Unknown nomination contest",
      TRUE ~ electoral_event
    )
  )
if (all(!(clean_data_12$electoral_event == ""))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some electoral events are still empty.")
}
if (length(unique(clean_data_12$electoral_event)) < 50) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: number of unique entries in `electoral_event` is unreasonably large. "
  )
}

# 13. Validate `electoral_district`.

if (all(!is.na(clean_data_12$electoral_district))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some electoral district entries are NA. ")
}
missing_district_entities <- clean_data_12 %>%
  filter(electoral_district == "") %>%
  distinct(political_entity) %>%
  pull(political_entity)
if (
  setequal(
    missing_district_entities,
    c("Leadership contestants", "Registered parties")
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Unexpected political entities are missing `electoral_district`."
  )
}
district_lead_part <- clean_data_12 %>%
  filter(
    !political_entity %in% c("Leadership contestants", "Registered parties")
  ) %>%
  filter(electoral_district == "")
if (nrow(district_lead_part) == 0) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: Rows outside Leadership/Nomination contestants are missing `electoral_district`."
  )
}
if (length(unique(clean_data_12$electoral_district)) < 700) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Failed: number of unique entries in `electoral_district` is unreasonably large."
  )
}
clean_data_13 <- clean_data_12 %>%
  mutate(
    electoral_district = ifelse(
      electoral_district == "",
      NA,
      electoral_district
    )
  )

# 14. Clean `donor_location` and extract donor postal code (0.36% confirmed had no location data ; 0.45% could not extract postal code. So, we are at most missing 0.09% postal codes.)

if (all(!is.na(clean_data_13$donor_location))) {
  message("Validation Passed.")
} else {
  stop("Validation Failed: Some donor locations are entries are NA.")
}

clean_data_14 <- clean_data_13 %>%
  mutate(
    donor_postal_code = str_remove_all(donor_location, "[\\s-]"),
    donor_postal_code = str_to_upper(donor_postal_code),
    donor_postal_code = str_replace_all(
      donor_postal_code,
      c("O" = "0", "I" = "1", "!" = "1")
    ),
    donor_postal_code = str_extract(donor_postal_code, "([A-Z][0-9]){3}$")
  ) %>%
  mutate(
    donor_postal_code = ifelse(
      str_detect(donor_postal_code, "^[WZ]|[DFIOQU]"),
      NA,
      donor_postal_code
    )
  ) %>%
  select(-donor_location)


# DIAGNOSTICS:
# diagnoastic_data <- clean_data_14 %>% filter(!is_aggregated)
# match_rate <- 1 - mean(is.na(diagnoastic_data$donor_postal_code))
# diagnose_invalid_postal_codes <- clean_data_14 %>%
#   filter(!is_aggregated) %>%
#   group_by(is.na(donor_postal_code)) %>%
#   summarise(
#     n = n(),
#     sum = sum(amount_monetary + amount_non_monetary)
#   )
# invalid_postal_codes <- clean_data_14 %>%
#   filter(!is_aggregated & is.na(donor_postal_code)) %>%
#   summarize(n = n(), sum = sum(amount_monetary + amount_non_monetary))

# END.
cleaned_IJF_data <- clean_data_14
rows_discarded_total <- nrow(raw_IJF_data) - nrow(cleaned_IJF_data)
perc_rows_discarded <- round(rows_discarded_total / nrow(raw_IJF_data) * 100, 5)

# ------ Write to Parquet -------

message(paste0(
  "IJF raw data cleaning complete, discarded ",
  as.character(rows_discarded_total),
  " rows total (",
  perc_rows_discarded,
  "%), saving to Parquet."
))
write_parquet(
  cleaned_IJF_data,
  "data/clean_data/clean_data_IJF.parquet"
)
message("Parquet saved successfully --- END OF SCRIPT.")
