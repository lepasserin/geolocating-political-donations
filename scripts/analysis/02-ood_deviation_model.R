# Purpose: Model the district-level deviation between the observed and simulated
#          out-of-district (OOD) donation rate as a function of the same
#          socio-demographic covariates used in the paper's district-level model.
#          The outcome is (observed OOD rate - simulated OOD rate) for each FED,
#          pooled across political entities (candidates and district
#          associations) into a single model.
# Author: Benedict Cummins-Mburu
# Last Updated: 16 July 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Note:
#   - The simulated OOD rates come from `01-distance_hyp_test.R`, stored in
#     `data/other/distance_hyp_test_results.parquet`.
#   - The outcome (observed - simulated) is modelled directly (no transformation)
#     with a Gaussian linear model, weighted by log(n), where n is the number of
#     donations underlying each district-entity's observed rate. This mirrors the
#     precision-by-log(n) assumption of `FED_model` in `paper/paper.qmd`.
#   - Model predictors and coefficient plotting are otherwise identical to the
#     district-level model (`FED_model`) in `paper/paper.qmd`.

# ----- Setup -------
library(tidyverse)
library(arrow)
library(here)

# `paper_helpers.R` provides:
#   - coef_whisker_plot()      (identical plotting to paper.qmd)
#   - EFFECT_COLOURS
#   - localized_donations_data (all localized donations)
#   - census_lookup_FED        (FED-level aggregated census covariates)
#   - FED_model_data           (district-level model dataset, entity-combined)
source(here::here("paper/paper_helpers.R"))

distance_hyp_test_results <- read_parquet(
  here::here("data/other/distance_hyp_test_results.parquet")
)

# ----- FED-level covariates (keyed by district) -------
# `FED_model_data` in paper_helpers.R drops the district key, so we rebuild the
# exact same covariate table while retaining `donor_district`, in order to join
# it onto the simulation results below. This carries over ALL the information in
# `FED_model_data`.
FED_covariates <- localized_donations_data %>%
  group_by(donor_district) %>%
  summarise(
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
    donor_district,
    density_per_sqkm_sqrt,
    total_amount,
    donor_region,
    avg_age,
    median_hh_income,
    postsec_prop,
    starts_with("vismin_")
  )

# ----- Augment simulation results with FED covariates + build outcome -------
# Outcome is the raw deviation (observed - simulated OOD rate). `n` (the
# entity-specific donation count from the simulation results) is retained as the
# regression weight; `total_amount` as a predictor comes from FED_model_data.
model_data <- distance_hyp_test_results %>%
  filter(!is.na(prop_ood_obs), !is.na(mean_prop_ood_sim)) %>%
  mutate(deviation = prop_ood_obs - mean_prop_ood_sim) %>%
  select(-total_amount) %>% # keep FED_model_data's total_amount predictor
  left_join(
    FED_covariates,
    by = c("donor_district_obs" = "donor_district")
  )

# ----- Fit a single pooled model -------
# Mean predictors are identical to `FED_model` in paper.qmd. Raw Gaussian outcome,
# weighted by log(n) (in place of the betareg `| log(n)` precision submodel).
deviation_model <- lm(
  deviation ~
    total_amount +
    donor_region +
    avg_age +
    median_hh_income +
    postsec_prop +
    vismin_total +
    density_per_sqkm_sqrt,
  data = model_data,
  weights = log(n)
)

print(summary(deviation_model))

# ----- Coefficient plot (identical to paper.qmd) -------
# Same term labels as the district-level model in paper.qmd.
FED_term_labels <- c(
  "total_amount" = "Amount",
  "donor_regionAtlantic Canada" = "Atlantic Canada",
  "donor_regionBritish Columbia" = "British Columbia",
  "donor_regionPrairies" = "Prairies",
  "donor_regionQuebec" = "Quebec",
  "donor_regionTerritories" = "Territories",
  "avg_age" = "Age",
  "median_hh_income" = "Income",
  "postsec_prop" = "Education",
  "vismin_total" = "Visible minority",
  "density_per_sqkm_sqrt" = "Density (sqrt)"
)

deviation_model_coefs <- coef_whisker_plot(
  deviation_model,
  term_labels = FED_term_labels
)

out_path <- here::here(
  "other/static_figures/Figure_ood_deviation_model_coefs.png"
)
ggsave(out_path, deviation_model_coefs, width = 6, height = 5, dpi = 300)
cat("\nSaved coefficient plot to:", out_path, "\n")
