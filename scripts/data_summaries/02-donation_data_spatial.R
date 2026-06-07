# Purpose: Visualize `donations_data` Spatially through Figures and Tables.
# Author: Benedict Cummins-Mburu
# Last Updated: 5 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - This suite of plots should establish the existence of a "donor district"
# - This suite should show that OOD and ID donations originate from the same districts
# - This suite should determine where the recipients of donations are (for Local Entities)

# --------- Setup ----------
library(tidyverse)
library(lubridate)
library(arrow)
FED_donations_data <- read_parquet(
  "data/processed_data/FED_donations_data.parquet"
)

PARTIES_VISUALIZED <- c(
  "Conservative Party of Canada",
  "Liberal Party of Canada",
  "New Democratic Party",
  "Green Party of Canada"
)

FED_senders <- FED_donations_data %>%
  group_by(sending_district) %>%
  summarise(
    n_donations_sent = sum(n_donations_all_both),
    donation_amount_sent = sum(donation_amount_all_both)
  )


meann <- mean(FED_senders$donation_amount_sent)
mediann <- median(FED_senders$donation_amount_sent)
FED_senders %>%
  ggplot(aes(x = donation_amount_sent)) +
  geom_histogram() +
  theme_classic() +
  geom_vline(xintercept = meann, linetype = "dotted") +
  geom_vline(xintercept = mediann, linetype = "solid")
