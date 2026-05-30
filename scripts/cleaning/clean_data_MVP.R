# Purpose: Generate a Sample of the data to show to Chris Cochrane.
# Author: Benedict Cummins-Mburu
# Last Updated: 26 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - `raw_data_MPV.csv` takes < 5 minutes to be read into R
# - `raw_data_MPV.csv` data was manually added to the repo. Will download from IJF server later.

# ------- Setup --------

library(tidyverse)
raw_data <- read_csv("data/raw_data/raw_data_MVP.csv")
clean_PCFRF <- read_csv(
  "data/conversion_files/cleaned_PCFRF_dataNatFED2013_082021.csv"
)

# ------- Donation Data Cleaning --------

# 0. Helper Vectors
election_cycles_considered <- c(
  "44th general election",
  "43rd general election",
  "42nd general election"
)
years_considered <- c(2021, 2019, 2015)

# 1. Scoping
cleaned_data1 <- raw_data %>%
  filter(
    donor_type == "Individuals", # only considering donations from individuals
    electoral_event %in% election_cycles_considered, # only considering election cycles b/w 2013-2023
  ) %>%
  mutate(
    YEAR = as.integer(year),
    ID = rid
  ) %>%
  filter(YEAR %in% years_considered) %>% # TODO: talk to Martin. Some dontions to candidates are not dated within the election year?
  select(
    ID,
    YEAR,
    donor_location,
    donor_full_name,
    amount_monetary,
    recipient,
    electoral_district
  )

# TODO: need a script that validates Recipient Names

# Note: this also restricts dataset to only donations aimed at candidates
# Note: columns `year` and `donation_year` are equal
# Note: columns `amount` and `amount_non_monetary` are equal (ASK MARTIN ABOUT THIS)
# Note: `donor_full_name:Contributions Of $200 Or Less` only exists when

# 2. Extracting Postal Code

cleaned_data2 <- cleaned_data1 %>%
  filter(donor_location != ", ,") %>% # 6.4% of rows do not have any donor location
  mutate(DONOR_POSTAL = str_extract(donor_location, "[^,]*$")) %>%
  mutate(DONOR_POSTAL = str_remove_all(DONOR_POSTAL, "\\s")) %>%
  mutate(DONOR_POSTAL = str_to_upper(DONOR_POSTAL)) %>%
  mutate(
    DONOR_POSTAL = str_replace_all(DONOR_POSTAL, c("O" = "0", "I" = "1"))
  ) %>% # Canadas bans O and I in PCs
  mutate(DONOR_POSTAL = str_replace_all(DONOR_POSTAL, "[`:-]", "")) %>%
  mutate(DONOR_POSTAL_VALID = str_detect(DONOR_POSTAL, "^([A-Z][0-9]){3}$")) # currently 1% invalid (n ~= 333)

# 3. Matching Postal Code To FED

cleaned_data <- cleaned_data2 %>%
  filter(DONOR_POSTAL_VALID) %>% # ignoring these for now
  left_join(clean_PCFRF, by = c("DONOR_POSTAL" = "PC")) %>% # currently 531 PCs (1% ; 387 unique) cannot be mapped onto FEDs by the PCFRF
  rename(
    DONOR_FED = FED,
    RECIPIENT_FED = electoral_district
  ) %>%
  filter(!is.na(DONOR_FED)) %>% # ignoring these for now
  mutate(
    # manually handle specific cases
    RECIPIENT_FED = case_when(
      RECIPIENT_FED ==
        "Beauport-Côte-de-Beaupré-Île d'Orléans-Charlevoix" ~ "Beauport--Côte-de-Beaupré--Île d’Orléans--Charlevoix",
      RECIPIENT_FED ==
        "Leeds-Grenville-Thousand Islands and Rideau Lakes" ~ "Leeds--Grenville--Thousand Islands and Rideau Lakes",
      TRUE ~ RECIPIENT_FED
    )
  )

# Note: I checked that FED names are now standardized between RECIPIENTs and DONORs

# ------- Write to CSV --------
write_csv(cleaned_data, "data/clean_data/cleaned_data_MVP.csv")
