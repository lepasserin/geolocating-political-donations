# Purpose: Query Census data through the CensusMappper API to get socio-demographic data for each DA, and for each FED. Seperate datafiles are saved for each.
# Author: Benedict Cummins-Mburu
# Last Updated: 17 June 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup --------
library(tidyverse)
library(cancensus)
CENSUS_DATAFILES <- c("CA21", "CA16")
Sys.getenv("CM_API_KEY") # make sure the CensusMapper key is accessible

# --- Save to Parquet ---
