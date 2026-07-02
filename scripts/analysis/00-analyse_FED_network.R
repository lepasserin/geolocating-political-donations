# Purpose: Build District-Level Donations Flow Network Dataset.
# Author: Benedict Cummins-Mburu
# Last Updated: 30 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# Model Structure:
# Unit: FED. need two datasets:
# - FED_attrtib

# ----- Setup -------
library(tidyverse)
library(arrow)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
FED_lookup <- readRDS("data/clean_data/FED_lookup.rds")
PCCF_lookup <- NA
census_lookup <- NA

# ----- Create Network Datasets -------

FED_attributes <- donations_data %>%
  group_by(donor_district) %>%
  summarise(
    n_donors = n(),
    sum = sum(total_amount),
    prop_ood = (sum(total_amount) * is_out_of_district) / sum(total_amount)
  )
