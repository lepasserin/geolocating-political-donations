# Purpose: Build District-Level Donations Flow Network Dataset.
# Author: Benedict Cummins-Mburu
# Last Updated: 12 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(arrow)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
FED_shapefile <- readRDS("data/clean_data/clean_FED_shapefile.rds")
# TODO: refactor these changes into the cleaning script.
donations_data <- donations_data %>%
  mutate(
    electoral_event_clean = case_when(
      donation_date >= as.Date("2015-08-02") &
        donation_date <= as.Date("2015-10-19") ~ "General Election",
      donation_date >= as.Date("2019-09-11") &
        donation_date <= as.Date("2019-10-21") ~ "General Election",
      donation_date >= as.Date("2021-08-15") &
        donation_date <= as.Date("2021-09-20") ~ "General Election",
      TRUE ~ "Non-Election"
    ),
    total_amount = amount_monetary + amount_non_monetary
  ) %>%
  filter(total_amount <= 25000)

PCFRF_2022 <- read_parquet("data/clean_data/clean_PCFRF_2022.parquet")

# ---- Pre-Processing ------

LOCAL_POLITICAL_ENTITIES <- c("Registered associations", "Candidates")
VALID_FED_NAMES <- unique(PCFRF_2022$FED)

subsetted_data <- donations_data %>%
  filter(political_entity %in% LOCAL_POLITICAL_ENTITIES)

if (all(!is.na(subsetted_data$recipient_district))) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Some Local Political Entity FEDs are Missing.")
}
if (all(!is.na(subsetted_data$donor_district))) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Some Donor FEDs are Missing.")
}
if (all(unique(subsetted_data$recipient_district) %in% VALID_FED_NAMES)) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Some Recipient FEDs are Invalid.")
}
if (all(unique(subsetted_data$donor_district) %in% VALID_FED_NAMES)) {
  message("Validation Passed.")
} else {
  stop("Validation Error: Some Donor FEDs are Invalid.")
}

# -------- Execution --------

make_flow <- function(data, n_col, amt_col) {
  data %>%
    group_by(
      sending_district = donor_district,
      receiving_district = recipient_district
    ) %>%
    summarise(
      "{n_col}" := n(),
      "{amt_col}" := sum(total_amount, na.rm = TRUE),
      .groups = "drop"
    )
}

join_flow <- function(base, data, n_col, amt_col) {
  base %>%
    left_join(
      make_flow(data, n_col, amt_col),
      by = c("sending_district", "receiving_district")
    ) %>%
    mutate(across(all_of(c(n_col, amt_col)), ~ replace_na(.x, 0)))
}

# Base edge set: all local donations, pruned to edges with at least 1 donation
base_edges <- make_flow(
  subsetted_data,
  n_col = "n_donations_all_local",
  amt_col = "donation_amount_all_local"
) %>%
  filter(n_donations_all_local > 0)

# Slices
eda_all <- subsetted_data %>%
  filter(political_entity == "Registered associations")
eda_off <- eda_all %>% filter(electoral_event_clean == "Non-Election")
eda_on <- eda_all %>% filter(electoral_event_clean == "General Election")
candidates_on <- subsetted_data %>%
  filter(
    political_entity == "Candidates",
    electoral_event_clean == "General Election"
  )
both_on <- subsetted_data %>%
  filter(electoral_event_clean == "General Election")

# Join all 10 edge columns onto base
FED_donations_data_1 <- base_edges %>%
  join_flow(eda_all, "n_donations_EDA_all", "donation_amount_EDA_all") %>%
  join_flow(eda_off, "n_donations_EDA_off", "donation_amount_EDA_off") %>%
  join_flow(eda_on, "n_donations_EDA_on", "donation_amount_EDA_on") %>%
  join_flow(candidates_on, "n_donations_cand_on", "donation_amount_cand_on") %>%
  join_flow(both_on, "n_donations_both_on", "donation_amount_both_on")

# ---- Augment Links With Province -----

fed_to_province <- FED_shapefile %>%
  st_drop_geometry() %>%
  distinct(FED, PROVINCE)

# 1. Determine Province of Sender and Receiver Districts
FED_donations_data_2 <- FED_donations_data_1 %>%
  left_join(
    fed_to_province %>% rename(sending_province = PROVINCE),
    by = c("sending_district" = "FED")
  ) %>%
  left_join(
    fed_to_province %>% rename(receiving_province = PROVINCE),
    by = c("receiving_district" = "FED")
  )

# ---- Augment Links With Local Neighborhood -----

# 1. Unique links actually present in the donations data
edges <- FED_donations_data %>%
  distinct(sending_district, receiving_district)

# 2. Compute touching FED pairs across the FULL shapefile (no province pruning)
touch <- st_touches(FED_shapefile)

neighbor_pairs <- tibble(
  a = FED_shapefile$FED[rep(seq_along(touch), lengths(touch))],
  b = FED_shapefile$FED[unlist(touch)]
)

neighbor_keys <- paste(neighbor_pairs$a, neighbor_pairs$b, sep = "||")

# 3. Flag every FED that borders a FED in a different province (a "provincial edge")
fed_province <- FED_shapefile %>%
  st_drop_geometry() %>%
  distinct(FED, PROVINCE)

provincial_edge_feds <- neighbor_pairs %>%
  left_join(fed_province, by = c("a" = "FED")) %>%
  rename(prov_a = PROVINCE) %>%
  left_join(fed_province, by = c("b" = "FED")) %>%
  rename(prov_b = PROVINCE) %>%
  group_by(FED = a) %>%
  summarise(is_provincial_edge = any(prov_a != prov_b), .groups = "drop")

# 4. Mark each link's adjacency, then attach the provincial-edge flag for both ends
FED_neighbors_data <- edges %>%
  mutate(
    are_neighbors = paste(sending_district, receiving_district, sep = "||") %in%
      neighbor_keys
  ) %>%
  left_join(
    provincial_edge_feds %>%
      rename(sender_is_provincial_edge = is_provincial_edge),
    by = c("sending_district" = "FED")
  ) %>%
  left_join(
    provincial_edge_feds %>%
      rename(receiver_is_provincial_edge = is_provincial_edge),
    by = c("receiving_district" = "FED")
  ) %>%
  select(
    sending_district,
    receiving_district,
    are_neighbors,
    sender_is_provincial_edge,
    receiver_is_provincial_edge
  )

# 2. Join
FED_donations_data_3 <- FED_donations_data_2 %>%
  left_join(
    FED_neighbors_data,
    by = c("sending_district", "receiving_district")
  )

# ---- Write to Parquet -----
FED_donations_data <- FED_donations_data_3
write_parquet(
  FED_donations_data,
  "data/processed_data/FED_donations_data.parquet"
)
