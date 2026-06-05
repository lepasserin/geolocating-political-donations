# Purpose: Provide a Glimpse of All Processed Data Used for the Data Overview.
# Author: Benedict Cummins-Mburu
# Last Updated: 5 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(lubridate)
library(arrow)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
# TODO: once you have more data tables, handle them here.

# ----- Create Tables ------

DONATIONS_DATA_FOCAL_COLUMNS <- c(
  "donor_name",
  "donor_district",
  "recipient_name",
  "recipient_district",
  "amount_monetary",
  "donation_date"
)

Table_1 <- donations_data %>%
  arrange(political_entity) %>% # shows Candidate recipients first, more interesting
  select(all_of(DONATIONS_DATA_FOCAL_COLUMNS)) %>%
  head(6)

# -- Save Visualizations ---
write_csv(Table_1, "other/tables/Table_1.csv")
