# Purpose: Clean, validate, and consolidate 2015, 2019, and 2021 election results raw datasets. Saves to file.
# Author: Benedict Cummins-Mburu
# Last Updated: 30 July 2026
# STATUS: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------- Setup -------
library(tidyverse)
library(arrow)
library(sf)

FED_lookup <- readRDS("data/clean_data/FED_lookup.rds") %>%
  st_drop_geometry()

# ------- Pre-Processing -------

# Load in Electoral Results
results_2015_42 <- read_csv(
  "data/raw_data/election_results/2015_42_election_results.csv"
) %>%
  mutate(
    election_period = "42nd general election",
    FEDUID = `Electoral District Number/Numéro de circonscription`,
    candidate_name = `Candidate/Candidat`,
    votes_total = `Votes Obtained/Votes obtenus`,
    votes_prop = `Percentage of Votes Obtained /Pourcentage des votes obtenus`
  ) %>%
  select(c(election_period, FEDUID, candidate_name, votes_total, votes_prop))
results_2019_43 <- read_csv(
  "data/raw_data/election_results/2019_43_election_results.csv"
) %>%
  mutate(
    election_period = "43rd general election",
    FEDUID = `Electoral District Number/Numero de circonscription`,
    candidate_name = `Candidate/Candidat`,
    votes_total = `Votes Obtained/Votes obtenus`,
    votes_prop = `Percentage of Votes Obtained /Pourcentage des votes obtenus`
  ) %>%
  select(c(election_period, FEDUID, candidate_name, votes_total, votes_prop))
results_2021_44 <- read_csv(
  "data/raw_data/election_results/2021_44_election_results.csv"
) %>%
  mutate(
    election_period = "44th general election",
    FEDUID = `Electoral District Number/Numero de circonscription`,
    candidate_name = `Candidate/Candidat`,
    votes_total = `Votes Obtained/Votes obtenus`,
    votes_prop = `Percentage of Votes Obtained /Pourcentage des votes obtenus`
  ) %>%
  select(c(election_period, FEDUID, candidate_name, votes_total, votes_prop))

# Merge Together
results <- rbind(results_2015_42, results_2019_43, results_2021_44)

# Check that within each of the 3 election periods, every FED appears at least twice (i.e. there is always a runner-up).
candidate_counts <- results %>%
  count(election_period, FEDUID, name = "n_candidates")
if (
  n_distinct(candidate_counts$election_period) == 3 &&
    all(candidate_counts$n_candidates >= 2)
) {
  message(
    "Validation Passed."
  )
} else {
  stop(
    "Validation Failed: some FED-election period combinations have fewer than 2 candidates."
  )
}

# ------- Create FED to Margin Lookup -------

FED_to_margin <- results %>%
  mutate(FEDUID = as.character(FEDUID)) %>%
  group_by(FEDUID, election_period) %>%
  summarise(
    victory_prop = max(votes_prop),
    winning_candidate = candidate_name[which.max(votes_prop)],
    runnerup_prop = sort(votes_prop, decreasing = TRUE)[2],
    margin_of_victory_prop = abs(victory_prop - runnerup_prop),
    .groups = "drop"
  ) %>%
  mutate(
    winning_party = case_when(
      str_detect(winning_candidate, "Liberal") ~ "Liberal",
      str_detect(winning_candidate, "Conservative") ~ "Conservative",
      str_detect(winning_candidate, "NDP") ~ "NDP",
      str_detect(winning_candidate, "Bloc") ~ "Bloc Québécois",
      TRUE ~ "Other"
    )
  ) %>%
  select(FEDUID, election_period, margin_of_victory_prop, winning_party) %>%
  pivot_wider(
    names_from = election_period,
    values_from = c(margin_of_victory_prop, winning_party),
    names_glue = "{.value}_{election_period}"
  ) %>%
  rowwise() %>%
  mutate(
    margin_of_victory_avg = mean(
      c_across(starts_with("margin_of_victory_prop_")),
      na.rm = TRUE
    ),
    margin_of_victory_min = mean(
      c_across(starts_with("margin_of_victory_prop_")),
      na.rm = TRUE
    ),
    margin_of_victory_range = max(
      c_across(starts_with("margin_of_victory_prop_")),
      na.rm = TRUE
    ) -
      min(
        c_across(starts_with("margin_of_victory_prop_")),
        na.rm = TRUE
      ),
    margin_of_victory_sd = sd(
      c_across(starts_with("margin_of_victory_prop_")),
      na.rm = TRUE
    ),
    n_unique_winning_parties = n_distinct(
      c_across(starts_with("winning_party_")),
      na.rm = TRUE
    ),
    dominant_party = {
      parties <- c_across(starts_with("winning_party_"))
      parties <- parties[!is.na(parties)]
      names(sort(table(parties), decreasing = TRUE))[1]
    }
  ) %>%
  ungroup() %>%
  rename_with(
    ~ str_replace(., "margin_of_victory_prop_", "margin_of_victory_"),
    starts_with("margin_of_victory_prop_")
  ) %>%
  left_join(FED_lookup, by = "FEDUID") %>%
  select(-c(PRUID, area, centroid_lat, centroid_lon)) %>%
  rename(FED = name)

# ------ Save to File ------
write_parquet(FED_to_margin, "data/clean_data/election_results.parquet")

# Check: Relationship between party stability and average margin of victory.
stopifnot(all(FED_to_margin$n_unique_winning_parties %in% c(1, 2)))

margin_model <- glm(
  I(n_unique_winning_parties == 2) ~ margin_of_victory_avg,
  data = FED_to_margin,
  family = binomial(link = "logit")
)

# McFadden's pseudo-R²
1 - margin_model$deviance / margin_model$null.deviance

summary(margin_model)
