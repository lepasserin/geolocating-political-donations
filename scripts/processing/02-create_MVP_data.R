# Purpose: Create an aggressively simplified MVP dataset to produce preliminary visualizations.
# Author: Benedict Cummins-Mburu
# Last Updated: 2 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - relies on `clean_data_IJF.parquet`

# --------- Setup ----------
library(tidyverse) # for data cleaning
library(arrow) # to save result as parquet
raw_IJF_data <- fread("data/raw_data/raw_data_IJF.csv", data.table = FALSE)
