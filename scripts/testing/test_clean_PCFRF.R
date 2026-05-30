# Purpose: Validate Structure of PCFRF 1:1 Mapping Dataframe
# Author: Benedict Cummins-Mburu
# Last Updated: 26 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Notes:
# - There are 4 total tests.
# - Test suite takes < 1 minute to run.

# ------- Setup --------
library(tidyverse)
clean_PCFRF <- read_csv(
  "data/conversion_files/cleaned_PCFRF_dataNatFED2013_082021.csv"
)

# ------- Testing --------

# Validate column structure
c1 <- setequal(names(clean_PCFRF), c("PC", "FED"))
c2 <- length(names(clean_PCFRF)) == 2
if (c1 && c2) {
  message("Test 1 Passed.")
} else {
  stop("Test Failed: Invalid Column Structure.")
}

# Check that there are exactly 338 FEDs
if (length(unique(clean_PCFRF$FED)) == 338) {
  message("Test 2 Passed.")
} else {
  stop("Test Failed: The number of unique FEDs is different from 338.")
}

# Check that all postal codes are unique
if (length(unique(clean_PCFRF$PC)) == nrow(clean_PCFRF)) {
  message("Test 3 Passed.")
} else {
  stop("Test Failed: There are some duplicate postal codes.")
}

# Check that all postal codes are well-formatted
for (postal_code in clean_PCFRF$PC) {
  if (!str_detect(postal_code, "^([A-Z][0-9]){3}$")) {
    stop("Test Failed: This postal code is poorly formatted")
  }
}
message("Test 4 Passed.")

# TODO: Check that FED names are real FED names
