# Purpose: General Ceaning Script for Raw IJF Data.
# Author: Benedict Cummins-Mburu
# Last Updated: 2 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - `raw_data_IJF.csv` is accurate as of May 5, 2026. Will likely be updated, fixing errors.
# - First round of cleaning. More specialized data frames will use this later in the pipeline.
# - Script contain some validation code to justify the renoval of certain entries or columns.
# - Final dataframe represents all Individual contributions between 2004 and 2026.

# --------- Setup ----------
library(tidyverse) # for data cleaning
library(data.table) # for speedy CSV reading
library(arrow) # to save result as parquet
raw_IJF_data <- fread("data/raw_data/raw_data_IJF.csv", data.table = FALSE)

# --- Constants ----

VALID_DATE_RANGE <- c(
  lubridate::ymd("2004-01-01"),
  lubridate::ymd("2026-05-05")
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
  "Canadian Future Party"
)

# -------- Cleaning --------

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
  stop("Validation Error: some entries in column `year` are not integers.")
}
if (all(!is.na(clean_data_01$year))) {
  message("Validation Passed.")
} else {
  stop("Validation Error: some entries in column `year` are not integers.")
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

# 3. Remove NA Dates (since all < 2004, so irrelvant).

if (
  all(clean_data_02[is.na(clean_data_02$donation_date), ]$donation_year <= 2003)
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Some null dates have posted years strictly later than 2003."
  )
}
clean_data_03 <- clean_data_02 %>%
  filter(!is.na(donation_date))

# 4. Remove Invalid Dates (< 0.01% of the data at this stage)

Marian_Archibold_case_03 <- clean_data_03[
  (year(clean_data_03$donation_date) != clean_data_03$donation_year),
] # given the year is correct and date is invalid, assumed year = 2001 was correct. Since out of range, just removed row directly.
Marian_Archibold_rid_03 <- Marian_Archibold_case_03$rid
if (length(Marian_Archibold_rid_03) == 1) {
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
if ((invalid_date_count_03 / nrow(clean_data_03)) < 0.0001) {
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

this <- clean_data_06 %>% filter(amount_monetary + amount_non_monetary <= 0.0)
nrow(this)

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

# 8. Clean Column `political_party` (join "Independent" and "No Affiliation ; join "United Party of Canada (UP)" and "United Party of Canada")

clean_data_08 <- clean_data_07 %>%
  mutate(
    political_party = case_when(
      political_party ==
        "United Party of Canada (UP)" ~ "United Party of Canada",
      political_party == "No Affiliation" ~ "Independent",
      TRUE ~ political_party
    )
  )

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

# 9. Extract Donor Postal Code (0.36% confirmed had no location data ; 0.45% could not extract postal code. So, we are at most missing 0.09% postal codes.)

clean_data_09 <- clean_data_08 %>%
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
  ) # only 418 cases (NOT VALIDATED)
if (nrow(clean_data_09) == nrow(clean_data_08)) {
  message("Validation Passed.")
} else {
  stop(
    "validation Failed: Cleaning Step 9 changed the number of rows when it should not have."
  )
}

# DIAGNOSTIC: To better assess coverage, find true NAs
# true_missing_postal_codes <- clean_data_08 %>%
#   filter(str_detect(donor_location, "^[ ,\\.;]*$"))
# nrow(true_missing_postal_codes) / nrow(clean_data_08) * 100

# TODO: this code does not check if the postal code has ever been used. Download PCCF for this.

# TODO: Columns left to clean:
# - electoral_district (need to validate against location ; WE ARE ASSUMING THAT LOCATION IS CORRECT ALWAYS)
# - electoral_event (ask Martin about "0", "1", and "2")
# - recipient (ask Martin about recipient_ID)
# - donor_full_name (figure out later)
# - amount_monetary ; amount_non_monetary (deal with 0 and negative values)

# END.
cleaned_IJF_data <- clean_data_09
rows_discarded_total <- nrow(raw_IJF_data) - nrow(cleaned_IJF_data)
perc_rows_discarded <- round(rows_discarded_total / nrow(raw_IJF_data) * 100, 2)

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
