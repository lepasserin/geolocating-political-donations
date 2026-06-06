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
FED_OOD_data <- read_parquet("data/processed_data/FED_donations_data.parquet")

PARTIES_VISUALIZED <- c(
  "Conservative Party of Canada",
  "Liberal Party of Canada",
  "New Democratic Party",
  "Green Party of Canada"
)
