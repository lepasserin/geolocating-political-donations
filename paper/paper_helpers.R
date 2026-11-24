# Purpose: Constants, Visualization Helper Functions, and Modelling Datasets Sourced by `paper.qmd`.
# Author: Benedict Cummins-Mburu
# Last Updated: 22 Nov 2026
# Status: COMPLETE
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
library(kableExtra)
library(png)

# Analysis
library(mgcv)
library(spdep)
library(sandwich)
library(lmtest)

# ---- Setup: Data Sources For Main Paper ONLY -----

# Donations Datasets
donations_data_full <- read_parquet(here::here(
  "data/analysis_data/donations_data_full.parquet"
))
donations_data <- read_parquet(here::here(
  "data/analysis_data/donations_data.parquet"
))
candidate_model_data <- read_parquet(here::here(
  "data/analysis_data/candidate_model_data.parquet"
))

# Additional Simulation Datasets
distance_hyp_test_results <- read_parquet(here::here(
  "data/cached_data/distance_hyp_test_results.parquet"
))
localized_donations_data_distances <- read_parquet(here::here(
  "data/cached_data/localized_donations_data_distances.parquet"
))
localized_donations_data_neighbours <- read_parquet(here::here(
  "data/cached_data/localized_donations_data_neighbours.parquet"
))

# ---- Setup: Shared Data Sources -----

# Donations Dataset
localized_donations_data <- donations_data %>%
  filter(is_local) %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area))

# Correlaries
census_lookup <- read_parquet(here::here(
  "data/analysis_data/census_lookup.parquet"
)) %>%
  mutate(DAUID = as.character(DAUID))
FED_lookup <- readRDS(here::here("data/analysis_data/FED_lookup.rds")) %>%
  st_drop_geometry() %>%
  mutate(FEDUID = as.character(FEDUID))
PCCF_lookup <- read_parquet(here::here(
  "data/analysis_data/PCCF_lookup.parquet"
)) %>%
  mutate(FEDUID = as.character(FEDUID))
election_results <- read_parquet(here::here(
  "data/analysis_data/election_results.parquet"
))
FED_neighbours <- readRDS(here::here(
  "data/cached_data/FED_neighbour_edges.rds"
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
  term_labels = NULL, # so I can change the nasty raw labels
  vcov = NULL # optional covariance matrix (e.g. clustered) used for SEs, Wald CIs, and p-values
) {
  # handle a supplied vcov, betareg(), and glm() separately.
  if (!is.null(vcov)) {
    tidied <- broom::tidy(lmtest::coeftest(model, vcov = vcov))
  } else if (inherits(model, "betareg")) {
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

# 2. Results table for a basic logistic regression model.
logistic_results_table <- function(
  model,
  cluster,
  term_labels = NULL
) {
  if (
    !inherits(model, "glm") ||
      model$family$family != "binomial" ||
      model$family$link != "logit"
  ) {
    stop("`model` must be a glm() with family = binomial(link = \"logit\").")
  }

  # A. Row order: intercept, then each formula term; factors get their reference level first.
  coef_names <- names(coef(model))
  coef_assign <- attr(model.matrix(model), "assign")
  formula_terms <- attr(terms(model), "term.labels")
  rows <- intersect("(Intercept)", coef_names)
  for (k in seq_along(formula_terms)) {
    term <- formula_terms[k]
    if (term %in% names(model$xlevels)) {
      rows <- c(rows, paste0(term, model$xlevels[[term]][1]))
    }
    rows <- c(rows, coef_names[coef_assign == k])
  }

  # B. Unclustered and clustered inference (NA for reference levels).
  pick <- function(ct, col) {
    out <- setNames(rep(NA_real_, length(rows)), rows)
    hit <- intersect(rows, rownames(ct))
    out[hit] <- ct[hit, col]
    out
  }
  ct <- lmtest::coeftest(model)
  ct_cl <- lmtest::coeftest(
    model,
    vcov = sandwich::vcovCL(model, cluster = cluster, type = "HC1")
  )
  est <- pick(ct, "Estimate")
  se <- pick(ct, "Std. Error")
  p <- pick(ct, "Pr(>|z|)")
  se_cl <- pick(ct_cl, "Std. Error")
  p_cl <- pick(ct_cl, "Pr(>|z|)")
  or <- exp(est) # Odds ratios, with delta-method SEs (as `modelsummary(exponentiate = TRUE)` does).

  # C. Assemble.
  fmt_num <- function(x) ifelse(is.na(x), "-", sprintf("%.3f", x))
  # p-values: 3 decimals if >= 0.001, else their order of magnitude (<10^k), capped at <10^-16.
  fmt_p <- function(x) {
    out <- sprintf("%.3f", x)
    small <- !is.na(x) & x < 0.001
    # Text-mode superscripts (not math mode) so that bolding applies to these entries too.
    out[small] <- sprintf(
      "\\textless{}10\\textsuperscript{\\textminus{}%d}",
      -as.integer(ceiling(log10(pmax(x[small], 1e-16))))
    )
    out[!is.na(x) & x < 1e-16] <- "\\textless{}10\\textsuperscript{\\textminus{}16}"
    out[is.na(x)] <- "-"
    out
  }
  labels <- if (is.null(term_labels)) {
    rows
  } else {
    ifelse(is.na(term_labels[rows]), rows, term_labels[rows])
  }
  results <- tibble(
    " " = unname(labels),
    "Est." = fmt_num(est),
    "S.E." = fmt_num(se),
    "Cl. S.E." = fmt_num(se_cl),
    "Est." = fmt_num(or),
    "S.E." = fmt_num(or * se),
    "Cl. S.E." = fmt_num(or * se_cl),
    "p" = fmt_p(p),
    "Cl. p" = fmt_p(p_cl),
    .name_repair = "minimal" # Allow repeated column names across the Coefficient / Odds Ratio sections.
  )
  sig_rows <- which(!is.na(p_cl) & p_cl < 0.05)

  # D. Footer: fit statistics.
  fmt_big <- function(x, digits = 1) {
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }
  footer <- paste0(
    "Num.Obs. = ",
    fmt_big(nobs(model), 0),
    "; Log.Lik. = ",
    fmt_big(as.numeric(logLik(model))),
    "; Null Deviance = ",
    fmt_big(model$null.deviance),
    "; Residual Deviance = ",
    fmt_big(model$deviance),
    "; Residual df = ",
    fmt_big(model$df.residual, 0),
    "."
  )

  table <- tt(results, notes = footer) %>%
    group_tt(
      j = list("Coefficient" = 2:4, "Odds Ratio" = 5:7, "Significance" = 8:9)
    ) %>%
    style_tt(j = 1, align = "l") %>%
    style_tt(j = 2:9, align = "c") %>%
    style_tt(i = 0:nrow(results), fontsize = 0.85) %>%
    style_tt(i = "notes", fontsize = 0.7) %>%
    style_tt(i = -1:0, bold = TRUE)
  if (length(sig_rows) > 0) {
    table <- style_tt(table, i = sig_rows, j = 2:9, bold = TRUE)
  }
  table
}

# 2b. Results table for a quasibinomial (fractional logit) regression model, without SE clustering.
quasibinomial_results_table <- function(
  model,
  term_labels = NULL
) {
  if (
    !inherits(model, "glm") ||
      model$family$family != "quasibinomial" ||
      model$family$link != "logit"
  ) {
    stop("`model` must be a glm() with family = quasibinomial(link = \"logit\").")
  }

  # A. Row order: intercept, then each formula term; factors get their reference level first.
  coef_names <- names(coef(model))
  coef_assign <- attr(model.matrix(model), "assign")
  formula_terms <- attr(terms(model), "term.labels")
  rows <- intersect("(Intercept)", coef_names)
  for (k in seq_along(formula_terms)) {
    term <- formula_terms[k]
    if (term %in% names(model$xlevels)) {
      rows <- c(rows, paste0(term, model$xlevels[[term]][1]))
    }
    rows <- c(rows, coef_names[coef_assign == k])
  }

  # B. Inference (NA for reference levels); quasibinomial p-values use the t distribution.
  pick <- function(ct, col) {
    out <- setNames(rep(NA_real_, length(rows)), rows)
    hit <- intersect(rows, rownames(ct))
    out[hit] <- ct[hit, col]
    out
  }
  ct <- coef(summary(model))
  est <- pick(ct, "Estimate")
  se <- pick(ct, "Std. Error")
  p <- pick(ct, "Pr(>|t|)")
  or <- exp(est) # Odds ratios, with delta-method SEs.

  # C. Assemble.
  fmt_num <- function(x) ifelse(is.na(x), "-", sprintf("%.3f", x))
  # p-values: 3 decimals if >= 0.001, else their order of magnitude (<10^k), capped at <10^-16.
  fmt_p <- function(x) {
    out <- sprintf("%.3f", x)
    small <- !is.na(x) & x < 0.001
    # Text-mode superscripts (not math mode) so that bolding applies to these entries too.
    out[small] <- sprintf(
      "\\textless{}10\\textsuperscript{\\textminus{}%d}",
      -as.integer(ceiling(log10(pmax(x[small], 1e-16))))
    )
    out[!is.na(x) & x < 1e-16] <- "\\textless{}10\\textsuperscript{\\textminus{}16}"
    out[is.na(x)] <- "-"
    out
  }
  labels <- if (is.null(term_labels)) {
    rows
  } else {
    ifelse(is.na(term_labels[rows]), rows, term_labels[rows])
  }
  results <- tibble(
    " " = unname(labels),
    "Est." = fmt_num(est),
    "S.E." = fmt_num(se),
    "Est." = fmt_num(or),
    "S.E." = fmt_num(or * se),
    "p" = fmt_p(p),
    .name_repair = "minimal" # Allow repeated column names across the Coefficient / Odds Ratio sections.
  )
  sig_rows <- which(!is.na(p) & p < 0.05)

  # D. Footer: fit statistics (no log-likelihood under quasi-likelihood).
  fmt_big <- function(x, digits = 1) {
    formatC(x, format = "f", digits = digits, big.mark = ",")
  }
  footer <- paste0(
    "Num.Obs. = ",
    fmt_big(nobs(model), 0),
    "; Dispersion = ",
    fmt_big(summary(model)$dispersion, 3),
    "; Null Deviance = ",
    fmt_big(model$null.deviance),
    "; Residual Deviance = ",
    fmt_big(model$deviance),
    "; Residual df = ",
    fmt_big(model$df.residual, 0),
    "."
  )

  table <- tt(results, notes = footer) %>%
    group_tt(
      j = list("Coefficient" = 2:3, "Odds Ratio" = 4:5, "Significance" = 6)
    ) %>%
    style_tt(j = 1, align = "l") %>%
    style_tt(j = 2:6, align = "c") %>%
    style_tt(i = 0:nrow(results), fontsize = 0.85) %>%
    style_tt(i = "notes", fontsize = 0.7) %>%
    style_tt(i = -1:0, bold = TRUE)
  if (length(sig_rows) > 0) {
    table <- style_tt(table, i = sig_rows, j = 2:6, bold = TRUE)
  }
  table
}

# 3. Helper for 4.
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

# 4. Compare the marginal distributions of two datasets (weighted by contributed amount) across five categorical factors.
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

# We specify four model datasets:
#         - Main Models:
#             - Propensity
#             - Recipient
#         - Supplement S4 Models:
#             - Propensity (district-aggregated)
#             - Recipient  (candidate-aggregated)

# 1. Propensity
propensity_model_data <- localized_donations_data %>%
  mutate(donor_dissemination_area = as.character(donor_dissemination_area)) %>%
  filter(is_general_election_period | political_entity != "Candidates") %>% # TODO: deal with this somewhere else later; 1179 entries only.
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
    density_per_sqkm_sqrt = sqrt(density_per_sqkm),
    median_hh_income = median_hh_income / 1000,
    total_amount = total_amount / 1000
  ) %>%
  mutate(donor_province = relevel(factor(donor_province), ref = "Ontario")) %>%
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
    donor_district,
    donor_dissemination_area
  )
propensity_model_data <- propensity_model_data[
  complete.cases(propensity_model_data),
]

# 2. Recipient
recipient_model_data <- candidate_model_data %>%
  mutate(
    candidate_party = relevel(
      factor(candidate_party),
      ref = "Conservative Party of Canada"
    ),
    candidate_incumbency = relevel(
      factor(candidate_incumbency),
      ref = "Not an incumbent"
    ),
    candidate_gender = factor(
      if_else(candidate_gender == "Man", "Man", "Non-Man"),
      levels = c("Man", "Non-Man")
    ),
    candidate_ethnicity = factor(
      if_else(candidate_ethnicity == "White", "White", "Non-White"),
      levels = c("White", "Non-White")
    ),
    median_hh_income = median_hh_income / 1000,
    total_amount = total_amount / 1000
  )
recipient_model_data <- recipient_model_data[
  complete.cases(recipient_model_data),
]

# 3. Propensity (district-aggregated)
sharebased_propensity_data <- localized_donations_data %>%
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
    donor_region = relevel(factor(donor_region), ref = "Ontario"),
    median_hh_income = median_hh_income / 1000,
    total_amount = total_amount / 1000
  ) %>%
  mutate(donor_province = relevel(factor(donor_province), ref = "Ontario")) %>%
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

# 4. Recipient (candidate-aggregated)
sharebased_recipient_data <- recipient_model_data %>%
  group_by(
    candidate_ID,
    election_year,
    candidate_party,
    candidate_ethnicity,
    candidate_gender,
    candidate_out_LGBTQ2S,
    candidate_incumbency,
    candidate_prior_experience,
    region,
    recipient_district
  ) %>%
  summarise(
    n = n(),
    n_ood = sum(is_out_of_district),
    share_ood = sum(total_amount * is_out_of_district) / sum(total_amount),
    .groups = "drop"
  )
if (
  nrow(sharebased_recipient_data) ==
    n_distinct(sharebased_recipient_data$candidate_ID)
) {
  message("Validation Passed.")
} else {
  stop("Validation Failed.")
}
