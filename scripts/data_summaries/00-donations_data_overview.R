# Purpose: Provide a Glimpse of All Processed Data, Displayed the Data Overview.
# Author: Benedict Cummins-Mburu
# Last Updated: 5 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(lubridate)
library(arrow)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
FED_donations_data <- read_parquet(
  "data/processed_data/FED_donations_data.parquet"
)

# ----- Create Tables ------

DONATIONS_DATA_FOCAL_COLUMNS <- c(
  "donor_name",
  "donor_district",
  "recipient_name",
  "amount_monetary",
  "donation_date"
)

FED_DONATIONS_DATA_FOCAL_COLUMNS <- c(
  "sending_district",
  "receiving_district",
  "n_donations_all_both",
  "donation_amount_all_both"
)

Table_1 <- donations_data %>%
  select(all_of(DONATIONS_DATA_FOCAL_COLUMNS)) %>%
  head(4)

Table_2 <- FED_donations_data %>%
  select(all_of(FED_DONATIONS_DATA_FOCAL_COLUMNS)) %>%
  rename(
    n_donations = n_donations_all_both,
    donation_amount = donation_amount_all_both
  ) %>%
  head(4)


Table_3 <- data.frame(
  variable = c(
    "donor_name",
    "donor_district",
    "donation_date",
    "electoral_event",
    "amount_monetary",
    "recipient_name",
    "recipient_district",
    "political_entity",
    "political_party"
  )
) %>%
  mutate(
    example = c(
      "Alexandra Lulka",
      "Eglinton--Lawrence",
      "2015-10-18",
      "42nd general election",
      "563.3",
      "Joe Oliver",
      "Eglinton--Lawrence",
      "Candidates",
      "Conservative Party of Canada"
    )
  ) %>%
  mutate(
    description = c(
      "Donor full name.",
      "District associated with donor postal code.",
      "Date donation was received.",
      "Electoral event (if any) occuring during donation.",
      "The value of the donation, in CAD (summed monetary and non-monetary).",
      "Recipient full name.",
      "District of the recipient (if applicable).",
      "Entity type of the recipient.",
      "Recipient party affiliation (if any)."
    )
  )

# -------- Save ---------
write_csv(Table_1, "other/tables/Table_1_1.csv")
write_csv(Table_2, "other/tables/Table_1_2.csv")
write_csv(Table_3, "other/tables/Table_1_3.csv")
