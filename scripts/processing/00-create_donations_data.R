# Purpose: Aggressively restrict IJF data to the range encompassing all donations that occured while the 2013 representational order was in effect.
# Author: Benedict Cummins-Mburu
# Last Updated: 4 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Note:
# At a later date, this will be meticulously redone, and removed rows will be examined and assessed for removal impact.

# --------- Setup ----------
library(tidyverse)
library(arrow)
IJF_data <- read_parquet("data/clean_data/clean_data_IJF.parquet")
PCFRF_2022 <- read_parquet("data/clean_data/clean_PCFRF_2022.parquet")

# ------- Constants --------

VALID_FEDS <- unique(PCFRF_2022$FED)
VALID_FED_ENTRIES <- c(VALID_FEDS, NA)
INVALID_GENERAL_ELECTION_PERIODS <- c(
  "39th general election",
  "40th general election",
  "41st general election",
  "45th general election"
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
# TODO: datewise subsetting to be discussed and thought about.
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

# 3. Removing Invalid Recipient Ridings (a negligible 87 entries are removed in this step.)
created_donations_data_03 <- created_donations_data_02 %>%
  filter(recipient_district %in% VALID_FED_ENTRIES)

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

# 4. Generate Donor Ridings from Postal Code (absent + unmatched PCs were removed, representing a loss of ~ 1.5% rows at this stage)
# TODO: review these invalid data more closely later.
created_donations_data_04 <- created_donations_data_03 %>%
  filter(!is.na(donor_postal_code)) %>% # removed 0.26% of rows
  left_join(PCFRF_2022, by = c("donor_postal_code" = "PC")) %>%
  rename(donor_district = FED) %>% # removed 1.1% of rows
  filter(!is.na(donor_district))

if (
  all(
    unique(created_donations_data_04$donor_district) %in% VALID_FEDS
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Some of the donor FED names are invalid."
  )
}

# 5. Removing Invalid Electoral Events (a negligible 12 entries are removed in this step.)
# TODO: rewrite this when talked with Martin about weird numeric entries to `electoral-event`
created_donations_data_05 <- created_donations_data_04 %>%
  filter(!(electoral_event %in% INVALID_GENERAL_ELECTION_PERIODS))

# 6. Create New Column for OOD Donations
created_donations_data_06 <- created_donations_data_05 %>%
  mutate(is_out_of_district = donor_district != recipient_district)

# 6. Validate Other Aspects of the Data

# Note: All donations where `recipient_district` is NA are issued to "Leadership contestants" or "Registered parties".
# Note: All donations where `recipient_district` is valid and present are issued to "Registered associations", "Candidates", or "Nomination Contestants"
# Note: Most (> 95%) of donations said to have been made during a general election had dates that reflected that. This justfies the use of `electoral_event` later in the pipeline.

if (
  all(
    unique(
      created_donations_data_05$political_entity[is.na(
        created_donations_data_05$recipient_district
      )]
    ) %in%
      c("Leadership contestants", "Registered parties")
  )
) {
  message(
    "Validation Passed."
  )
} else {
  stop(
    "Validation Error: Found records with NA districts assigned to localized political entities."
  )
}

if (
  all(
    unique(
      created_donations_data_05$political_entity[
        !is.na(created_donations_data_05$recipient_district)
      ]
    ) %in%
      c("Registered associations", "Candidates", "Nomination contestants")
  )
) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Found national-level entities (Parties/Leadership) mapped to specific riding districts."
  )
}


# END.
created_donations_data <- created_donations_data_06

# ----- Write to Parquet -----

write_parquet(
  created_donations_data,
  "data/processed_data/donations_data.parquet"
)
message("Parquet saved successfully --- END OF SCRIPT.")
