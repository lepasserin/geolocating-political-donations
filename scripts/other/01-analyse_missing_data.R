# Purpose: Carry out analyses and create visualizatons discussed in Appendix @sec-missing-data.
# Author: Benedict Cummins-Mburu
# Last Updated: 2 July 2026
# STATUS: TODO
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup --------
library(tidyverse)
library(arrow)
donations_data_appendix <- read_parquet(
  "data/processed_data/donations_data_appendix.parquet"
)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")

# 1. Get overall missing proportions (4 summary tables)
missing_total_all <- donations_data_appendix %>%
  group_by(is.na(donor_district)) %>%
  summarise(n = n(), sum = sum(total_amount))

missing_total_local <- donations_data_appendix %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  group_by(is.na(donor_district)) %>%
  summarise(n = n(), sum = sum(total_amount))

missing_aggregated_all <- donations_data_appendix %>%
  filter(is.na(donor_district)) %>%
  group_by(is_aggregated) %>%
  summarise(n = n(), sum = sum(total_amount))

missing_aggregated_local <- donations_data_appendix %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  filter(is.na(donor_district)) %>%
  group_by(is_aggregated) %>%
  summarise(n = n(), sum = sum(total_amount))

# 1. Descriptively Analyze Local Aggregated Data

local_aggregated_missing_data <- donations_data_appendix %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  filter(is.na(donor_district)) %>%
  filter(is_aggregated) %>%
  relocate(donation_date, political_entity, electoral_event_OG)

plot(table(local_aggregated_missing_data$donation_date))
