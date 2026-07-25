# Purpose: Constants, Helper Functions, and Model Datasets Sourced by `presentation.qmd`.
# Author: Benedict Cummins-Mburu
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ---- Setup ----

# Data Loading & Manipulation
library(tidyverse)
library(arrow)
library(here)
library(sf)

# Visualization and Analysis Tools
library(patchwork)
library(modelsummary)
library(tinytable)
library(png)
library(mgcv)
library(betareg)

localized_donations_data <- read_parquet(
  here::here("data/processed_data/donations_data.parquet")
) %>%
  filter(is_local) %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area))
census_lookup <- read_parquet(here::here(
  "data/clean_data/census_lookup.parquet"
)) %>%
  mutate(DAUID = as.character(DAUID))
FED_lookup <- readRDS(here::here("data/clean_data/FED_lookup.rds")) %>%
  st_drop_geometry() %>%
  mutate(FEDUID = as.character(FEDUID))
PCCF_lookup <- read_parquet(here::here(
  "data/clean_data/PCCF_lookup.parquet"
)) %>%
  mutate(FEDUID = as.character(FEDUID))
localized_donations_data_distances <- read_parquet(here::here(
  "data/other/localized_donations_data_distances.parquet"
))

# ---- Constants ----

ELECTION_PERIODS <- tibble(
  start = as.Date(c("2015-08-02", "2019-09-11", "2021-08-15")),
  end = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20"))
)
EFFECT_COLOURS <- c(
  "Positive" = "#1B7837",
  "Negative" = "#C62828",
  "Not significant" = "#999999"
)

# ---- Functions ----

# Point-and-whiskers coefficient plot for regression models, coloured by the
# sign and significance of each effect. Transparent background for slides.
coef_whisker_plot <- function(
  model,
  term_labels = NULL # so I can change the nasty raw labels
) {
  # handle glm() and betareg() separately.
  if (inherits(model, "betareg")) {
    tidied <- broom::tidy(model) %>%
      filter(component == "mean")
  } else {
    tidied <- broom::tidy(model)
  }

  tidied <- tidied %>%
    filter(!term %in% c("(Intercept)", "(phi)", "phi")) %>%
    mutate(
      conf.low = estimate - qnorm(0.975) * std.error,
      conf.high = estimate + qnorm(0.975) * std.error,
      effect = case_when(
        p.value >= 0.05 ~ "Not significant",
        estimate > 0 ~ "Positive",
        TRUE ~ "Negative"
      ),
      effect = factor(effect, levels = names(EFFECT_COLOURS)),
      term_label = if (is.null(term_labels)) {
        term
      } else {
        coalesce(term_labels[term], term)
      },
      term_label = fct_rev(fct_inorder(term_label))
    )

  ggplot(
    tidied,
    aes(x = estimate, y = term_label, colour = effect)
  ) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "grey40") +
    geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      orientation = "y",
      width = 0,
      linewidth = 0.6
    ) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = EFFECT_COLOURS, drop = FALSE) +
    labs(
      x = "Coefficient estimates and 95% confidence intervals",
      y = NULL
    ) +
    theme_classic() +
    theme(
      legend.position = "none",
      panel.grid.major.y = element_line(colour = "grey90"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      legend.background = element_rect(fill = "transparent", colour = NA)
    )
}

# ---- Datasets ----

# 1. Individual OOD Propensity Model Dataset.
individual_model_data <- localized_donations_data %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area)) %>%
  filter(is_general_election_period | political_entity != "Candidates") %>%
  mutate(
    political_party_cat = case_when(
      political_party == "Conservative Party of Canada" ~ "Conservative",
      political_party == "Liberal Party of Canada" ~ "Liberal",
      political_party == "New Democratic Party" ~ "N.D.P",
      TRUE ~ "Other"
    ),
    donor_region = case_when(
      donor_province == "Ontario" ~ "Ontario",
      donor_province == "Quebec" ~ "Quebec",
      donor_province == "British Columbia" ~ "British Columbia",
      donor_province %in% c("Alberta", "Saskatchewan", "Manitoba") ~ "Prairies",
      donor_province %in%
        c(
          "Nova Scotia",
          "New Brunswick",
          "Prince Edward Island",
          "Newfoundland and Labrador"
        ) ~ "Atlantic Canada",
      donor_province %in%
        c("Yukon", "Northwest Territories", "Nunavut") ~ "Territories"
    ),
    donor_parliament = case_when(
      donation_date <= as.Date("2015-10-19") ~ "41st Parliament",
      donation_date > as.Date("2015-10-19") &
        donation_date <= as.Date("2019-10-21") ~ "42nd Parliament",
      donation_date > as.Date("2019-10-21") &
        donation_date <= as.Date("2021-09-20") ~ "43rd Parliament",
      donation_date > as.Date("2021-09-20") ~ "44th Parliament",
    )
  ) %>%
  left_join(census_lookup, by = c("donor_dissemination_area" = "DAUID")) %>%
  mutate(
    political_entity = relevel(
      factor(political_entity),
      ref = "Registered associations"
    ),
    political_party_cat = relevel(
      factor(political_party_cat),
      ref = "Conservative"
    ),
    donor_region = relevel(factor(donor_region), ref = "Ontario"),
    density_per_sqkm_sqrt = sqrt(density_per_sqkm)
  ) %>%
  select(
    is_out_of_district,
    is_general_election_period,
    political_entity,
    political_party_cat,
    total_amount,
    donor_region,
    avg_age,
    median_hh_income,
    postsec_prop,
    starts_with("vismin_"),
    density_per_sqkm_sqrt,
    donor_parliament
  )

# 2. District OOD Propensity Model Dataset.
DAUID_to_FED_name <- PCCF_lookup %>%
  distinct(DAUID, FEDUID) %>%
  left_join(FED_lookup, by = "FEDUID") %>%
  transmute(DAUID = as.character(DAUID), FED_name = name)

census_lookup_FED <- census_lookup %>%
  filter(missing_stage == "complete") %>%
  left_join(DAUID_to_FED_name, by = "DAUID") %>%
  group_by(FED_name) %>%
  summarise(
    density_per_sqkm = sum(population) / sum(land_area_sqkm),
    avg_age = weighted.mean(avg_age, population),
    median_hh_income = weighted.mean(median_hh_income, population),
    postsec_prop = weighted.mean(postsec_prop, population),
    across(starts_with("vismin_"), ~ weighted.mean(.x, population)),
    .groups = "drop"
  )

FED_model_data <- localized_donations_data %>%
  group_by(donor_district) %>%
  summarise(
    n = n(),
    prop_ood = sum(total_amount * is_out_of_district) / sum(total_amount),
    total_amount = sum(total_amount),
    donor_province = first(donor_province),
    .groups = "drop"
  ) %>%
  mutate(
    donor_region = case_when(
      donor_province == "Ontario" ~ "Ontario",
      donor_province == "Quebec" ~ "Quebec",
      donor_province == "British Columbia" ~ "British Columbia",
      donor_province %in% c("Alberta", "Saskatchewan", "Manitoba") ~ "Prairies",
      donor_province %in%
        c(
          "Nova Scotia",
          "New Brunswick",
          "Prince Edward Island",
          "Newfoundland and Labrador"
        ) ~ "Atlantic Canada",
      donor_province %in%
        c("Yukon", "Northwest Territories", "Nunavut") ~ "Territories"
    )
  ) %>%
  left_join(census_lookup_FED, by = c("donor_district" = "FED_name")) %>%
  mutate(
    density_per_sqkm_sqrt = sqrt(density_per_sqkm),
    donor_region = relevel(factor(donor_region), ref = "Ontario")
  ) %>%
  select(
    prop_ood,
    density_per_sqkm_sqrt,
    total_amount,
    donor_region,
    avg_age,
    median_hh_income,
    postsec_prop,
    n,
    starts_with("vismin_")
  )
