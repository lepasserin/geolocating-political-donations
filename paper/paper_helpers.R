# Purpose: Constants, Visualization Helper Functions, and Modelling Datasets Sourced by `paper.qmd`.
# Author: Benedict Cummins-Mburu
# Last Updated: 30 July 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ---- Setup: Libraries -----

# Data Loading & Manipulation
library(tidyverse)
library(arrow)
library(here)
library(sf)

# Visualization Tools
library(patchwork)
library(modelsummary)
library(tinytable)
library(png)
library(mgcv)

# Analysis
library(betareg)

# ---- Setup: Data Sources For Main Paper ONLY -----

# Donations Datasets
donations_data_appendix <- read_parquet(here::here(
  "data/processed_data/donations_data_appendix.parquet"
))
donations_data <- read_parquet(here::here(
  "data/processed_data/donations_data.parquet"
))

# Additional Simulation Datasets
distance_hyp_test_results <- read_parquet(here::here(
  "data/other/distance_hyp_test_results.parquet"
))
localized_donations_data_distances <- read_parquet(here::here(
  "data/other/localized_donations_data_distances.parquet"
))
localized_donations_data_neighbours <- read_parquet(here::here(
  "data/other/localized_donations_data_neighbours.parquet"
))

# ---- Setup: Shared Data Sources -----

# Donations Dataset
localized_donations_data <- donations_data %>%
  filter(is_local) %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area))

# Correlaries
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
election_results <- read_parquet(here::here(
  "data/clean_data/election_results.parquet"
))

# ------ Constants -------

PARTIES_NAMED <- c(
  "Conservative Party of Canada",
  "Liberal Party of Canada",
  "New Democratic Party"
)
PARTY_COLOURS <- c(
  "Conservative Party of Canada" = "#0072B2",
  "Liberal Party of Canada" = "#D55E00",
  "New Democratic Party" = "#E69F00",
  "Other" = "#999999"
)
ELECTION_PERIODS <- tibble(
  start = as.Date(c("2015-08-02", "2019-09-11", "2021-08-15")),
  end = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20"))
)
DISTRICT_STATUS_COLOURS <- c(
  "In-district" = "#D55E00",
  "Out-of-district" = "#0072B2"
)
PROVINCE_CODES <- c(
  "Newfoundland and Labrador" = "NL",
  "Prince Edward Island" = "PE",
  "Nova Scotia" = "NS",
  "New Brunswick" = "NB",
  "Quebec" = "QC",
  "Ontario" = "ON",
  "Manitoba" = "MB",
  "Saskatchewan" = "SK",
  "Alberta" = "AB",
  "British Columbia" = "BC",
  "Yukon" = "YT",
  "Northwest Territories" = "NT",
  "Nunavut" = "NU"
)
EFFECT_COLOURS <- c(
  "Positive" = "#1B7837",
  "Negative" = "#C62828",
  "Not significant" = "#999999"
)
STUDY_COLOUR <- "#0072B2"
REGION_LEVELS <- c(
  "Ontario",
  "Quebec",
  "British Columbia",
  "Prairies",
  "Atlantic Canada",
  "Territories"
)
PROVINCE_TO_REGION <- c(
  "Ontario" = "Ontario",
  "Quebec" = "Quebec",
  "British Columbia" = "British Columbia",
  "Alberta" = "Prairies",
  "Saskatchewan" = "Prairies",
  "Manitoba" = "Prairies",
  "Nova Scotia" = "Atlantic Canada",
  "New Brunswick" = "Atlantic Canada",
  "Prince Edward Island" = "Atlantic Canada",
  "Newfoundland and Labrador" = "Atlantic Canada",
  "Yukon" = "Territories",
  "Northwest Territories" = "Territories",
  "Nunavut" = "Territories"
)
PROVINCE_BY_PRUID <- c(
  "10" = "NL",
  "11" = "PE",
  "12" = "NS",
  "13" = "NB",
  "24" = "QC",
  "35" = "ON",
  "46" = "MB",
  "47" = "SK",
  "48" = "AB",
  "59" = "BC",
  "60" = "YT",
  "61" = "NT",
  "62" = "NU"
)
REGION_BY_CODE <- setNames(
  unname(PROVINCE_TO_REGION[names(PROVINCE_CODES)]),
  unname(PROVINCE_CODES)
)
district_to_province <- FED_lookup %>%
  mutate(prov_code = PROVINCE_BY_PRUID[as.character(PRUID)]) %>%
  transmute(
    name,
    recipient_province = factor(prov_code, levels = PROVINCE_BY_PRUID),
    recipient_region = factor(REGION_BY_CODE[prov_code], levels = REGION_LEVELS)
  )

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


# ------ Helper Functions -------

# 1. Point-and-whiskers coefficient plot for regression models.
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

  # plot
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
      panel.grid.major.y = element_line(colour = "grey90")
    )
}

# 2. Helper for 3.
build_comparison_panel <- function(
  df,
  xvar,
  colours,
  xlabels = NULL,
  ref_status = "Study Data",
  order_by_prop = TRUE # FALSE keeps the existing factor order (e.g. time)
) {
  if (order_by_prop) {
    study <- df[df$data_status == ref_status, ]
    ordered_levels <- as.character(study[[xvar]])[order(-study$prop)]
    df[[xvar]] <- factor(
      df[[xvar]],
      levels = union(ordered_levels, levels(df[[xvar]]))
    )
  }

  p <- ggplot(df, aes(x = .data[[xvar]], y = prop, fill = data_status)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_y_continuous(labels = scales::label_percent()) +
    scale_fill_manual(values = colours, drop = FALSE) +
    labs(x = NULL, y = "Proportion of Total (Per Group)", fill = NULL) +
    theme_classic() +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      axis.title = element_text(face = "plain"),
      axis.text = element_text(face = "plain")
    )
  if (!is.null(xlabels)) {
    p <- p + scale_x_discrete(labels = xlabels)
  }
  p
}

# 3. Compare the marginal distributions of two datasets (weighted by contributed amount) across five categorical factors.
plot_missingness_comparison <- function(
  comparison_data_1,
  comparison_data_2,
  comparison_colour_1 = "#0072B2",
  comparison_label_1 = "Study Data",
  comparison_colour_2 = "#D55E00",
  comparison_label_2 = "Comparison Data"
) {
  colours <- c(comparison_colour_1, comparison_colour_2)
  names(colours) <- c(comparison_label_1, comparison_label_2)

  # Keep only the columns the panels use, so unused columns with mismatched
  # types across the two datasets don't break the bind.
  plot_cols <- c(
    "total_amount",
    "political_entity", # 2 cases
    "political_party", # 4 cases
    "recipient_district", # 13 cases
    "electoral_event", # candidates only (panel D)
    "donation_year" # district associations only (panel E)
  )
  plot_data <- bind_rows(
    comparison_data_1 %>%
      select(all_of(plot_cols)) %>%
      mutate(data_status = comparison_label_1),
    comparison_data_2 %>%
      select(all_of(plot_cols)) %>%
      mutate(data_status = comparison_label_2)
  ) %>%
    mutate(data_status = factor(data_status, levels = names(colours)))

  # A. Recipient type (political_entity)
  entity_data <- plot_data %>%
    mutate(
      entity = factor(
        recode(
          political_entity,
          "Candidates" = "Candidates",
          "Registered associations" = "District associations"
        ),
        levels = c("Candidates", "District associations")
      )
    ) %>%
    group_by(data_status, entity) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_entity <- build_comparison_panel(
    entity_data,
    "entity",
    colours,
    ref_status = comparison_label_1
  )

  # B. Recipient party (political_party)
  party_data <- plot_data %>%
    mutate(
      party = factor(
        if_else(political_party %in% PARTIES_NAMED, political_party, "Other"),
        levels = c(PARTIES_NAMED, "Other")
      )
    ) %>%
    group_by(data_status, party) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_party <- build_comparison_panel(
    party_data,
    "party",
    colours,
    xlabels = c(
      "Conservative Party of Canada" = "Conservative",
      "Liberal Party of Canada" = "Liberal",
      "New Democratic Party" = "NDP",
      "Other" = "Other"
    ),
    ref_status = comparison_label_1
  )

  # C. Recipient region (recipient province grouped into regions)
  region_data <- plot_data %>%
    left_join(district_to_province, by = c("recipient_district" = "name")) %>%
    filter(!is.na(recipient_region)) %>%
    group_by(data_status, recipient_region) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_region <- build_comparison_panel(
    region_data,
    "recipient_region",
    colours,
    xlabels = c(
      "Ontario" = "Ontario",
      "Quebec" = "Quebec",
      "British Columbia" = "B.C.",
      "Prairies" = "Prairies",
      "Atlantic Canada" = "Atlantic",
      "Territories" = "Terr."
    ),
    ref_status = comparison_label_1
  )

  # D. Election received (candidates only), across general election periods.
  election_data <- plot_data %>%
    filter(political_entity == "Candidates", !is.na(electoral_event)) %>%
    mutate(
      electoral_event = factor(
        as.character(electoral_event),
        levels = c(
          "42nd general election",
          "43rd general election",
          "44th general election"
        )
      )
    ) %>%
    group_by(data_status, electoral_event) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_election <- build_comparison_panel(
    election_data,
    "electoral_event",
    colours,
    xlabels = c(
      "42nd general election" = "42nd",
      "43rd general election" = "43rd",
      "44th general election" = "44th"
    ),
    ref_status = comparison_label_1,
    order_by_prop = FALSE
  )

  # E. Donation year (district associations only), across years.
  year_data <- plot_data %>%
    filter(
      political_entity == "Registered associations",
      !is.na(donation_year)
    ) %>%
    mutate(
      donation_year = factor(
        donation_year,
        levels = sort(unique(donation_year))
      )
    ) %>%
    group_by(data_status, donation_year) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_year <- build_comparison_panel(
    year_data,
    "donation_year",
    colours,
    ref_status = comparison_label_1,
    order_by_prop = FALSE
  )

  # Combine: 2x2 grid (A-D) on top, panel E spanning the bottom row.
  panel_title <- theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10)
  )
  p_entity <- p_entity + labs(title = "A. Recipient Type") + panel_title
  p_party <- p_party + labs(title = "B. Recipient Party") + panel_title
  p_region <- p_region + labs(title = "C. Recipient Region") + panel_title
  p_election <- p_election +
    labs(title = "D. General Election (Candidates)") +
    panel_title
  p_year <- p_year +
    labs(title = "E. Donation Year (District Associations)") +
    panel_title

  p_all <- (p_entity | p_party) /
    (p_region | p_election) /
    p_year +
    plot_layout(guides = "collect") &
    theme(legend.position = "top")
  p_all & theme(axis.title.y = element_blank())
}

# ------ Modelling Datasets -------

# We specify three model datasets:
#         - Individual (with everything needed for all variants)
#         - FED (aggregated both)
#         - FED (EDAs only)

# 1. Individual
individual_model_data <- localized_donations_data %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area)) %>%
  filter(is_general_election_period | political_entity != "Candidates") %>% # TODO: deal with this somewhere else later; 1179 entries.
  mutate(
    political_party_cat = case_when(
      political_party == "Conservative Party of Canada" ~ "Conservative",
      political_party == "Liberal Party of Canada" ~ "Liberal",
      political_party == "New Democratic Party" ~ "N.D.P",
      TRUE ~ "Other"
    )
  ) %>%
  left_join(
    district_to_province %>%
      rename(donor_region = recipient_region),
    by = c("donor_district" = "name")
  ) %>%
  left_join(election_results, by = c("donor_district" = "FED")) %>%
  mutate(
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
    general_election_period,
    political_entity,
    political_party_cat,
    total_amount,
    donor_province,
    donor_region,
    avg_age,
    median_hh_income,
    postsec_prop,
    starts_with("vismin_"),
    `margin_of_victory_42nd general election`,
    `margin_of_victory_43rd general election`,
    `margin_of_victory_44th general election`,
    margin_of_victory_avg,
    density_per_sqkm_sqrt,
    donor_parliament,
    donor_district
  )

# 2. District - Aggregated
FED_model_data <- localized_donations_data %>%
  group_by(donor_district) %>%
  summarise(
    n = n(),
    prop_ood = sum(total_amount * is_out_of_district) / sum(total_amount),
    total_amount = sum(total_amount),
    donor_province = first(donor_province),
    .groups = "drop"
  ) %>%
  left_join(
    district_to_province %>%
      rename(donor_region = recipient_region),
    by = c("donor_district" = "name")
  ) %>%
  left_join(election_results, by = c("donor_district" = "FED")) %>%
  left_join(census_lookup_FED, by = c("donor_district" = "FED_name")) %>%
  mutate(
    density_per_sqkm_sqrt = sqrt(density_per_sqkm),
    donor_region = relevel(factor(donor_region), ref = "Ontario")
  ) %>%
  select(
    donor_district,
    prop_ood,
    density_per_sqkm_sqrt,
    total_amount,
    donor_province,
    donor_region,
    avg_age,
    median_hh_income,
    postsec_prop,
    margin_of_victory_avg,
    n,
    starts_with("vismin_")
  )

# 3. District - EDA Only
FED_model_data_EDA <- localized_donations_data %>%
  filter(political_entity == "Registered associations") %>%
  group_by(donor_district) %>%
  summarise(
    n = n(),
    prop_ood = sum(total_amount * is_out_of_district) / sum(total_amount),
    total_amount = sum(total_amount),
    donor_province = first(donor_province),
    .groups = "drop"
  ) %>%
  left_join(
    district_to_province %>%
      rename(donor_region = recipient_region),
    by = c("donor_district" = "name")
  ) %>%
  left_join(election_results, by = c("donor_district" = "FED")) %>%
  left_join(census_lookup_FED, by = c("donor_district" = "FED_name")) %>%
  mutate(
    density_per_sqkm_sqrt = sqrt(density_per_sqkm),
    donor_region = relevel(factor(donor_region), ref = "Ontario")
  ) %>%
  select(
    donor_district,
    prop_ood,
    density_per_sqkm_sqrt,
    total_amount,
    donor_province,
    donor_region,
    avg_age,
    median_hh_income,
    postsec_prop,
    margin_of_victory_avg,
    n,
    starts_with("vismin_")
  )
