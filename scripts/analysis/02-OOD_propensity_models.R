# Purpose: Model the propensity for out-of-district giving, at the individual (DA) and district (FED) level.
# Author: Benedict Cummins-Mburu
# Last Updated: 2 July 2026
# Status: PILOT
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Note:
# - Didn't do it literally at the individual entry level, because likely very high autocorrelation between donations made by the same person:

# ----- Setup -------
library(tidyverse)
library(arrow)
library(sf)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
FED_lookup <- readRDS("data/clean_data/FED_lookup.rds") %>% st_drop_geometry()
PCCF_lookup <- read_parquet("data/clean_data/PCCF_lookup.parquet")
census_lookup <- read_parquet("data/clean_data/census_lookup.parquet")

census_lookup <- census_lookup %>%
  mutate(DAUID = as.character(DAUID))
FED_lookup <- FED_lookup %>%
  mutate(FEDUID = as.character(FEDUID))
PCCF_lookup <- PCCF_lookup %>%
  mutate(FEDUID = as.character(FEDUID))
donations_data <- donations_data %>%
  mutate(
    donor_dissemination_area = as.character(donor_dissemination_area)
  )

# --- Individual-Level OOD-Propensity Model ---

# # 1. Subsetting and grouping.
# DA_local_donations_data <- donations_data %>%
#   filter(political_entity %in% c("Registered associations", "Candidates")) %>%
#   group_by(donor_dissemination_area) %>%
#   summarise(
#     n_donors = n(),
#     prop_ood = sum(total_amount * is_out_of_district) / sum(total_amount),
#     total_amount = sum(total_amount),
#     donor_district = donor_district[1]
#   ) %>%
#   left_join(census_lookup, by = c("donor_dissemination_area" = "DAUID"))

# # 2. Fitting model.
# DA_model_data <- DA_local_donations_data %>%
#   ungroup() %>%
#   filter(!is.na(prop_ood)) %>%
#   select(
#     -donor_dissemination_area,
#     -donor_district,
#     -total_amount,
#     -missing_stage,
#     -vismin_total,
#     -land_area_sqkm
#   )

# DA_model <- glm(
#   prop_ood ~ . - n_donors,
#   data = DA_model_data,
#   family = binomial(link = "logit"),
#   weights = n_donors
# )
# summary(DA_model)

# ------ District-Level OOD-Propensity Models ------

# 1. Create a version of `FED_attributes` Dataset.

# 1.1. Subsetting and grouping.
FED_local_donations_data <- donations_data %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  group_by(donor_district) %>%
  summarise(
    n_donors = n(),
    prop_ood = sum(total_amount * is_out_of_district) / sum(total_amount),
    total_amount = sum(total_amount)
  )

# 1.2. Create FED-level Census lookup.
DAUID_to_FED_name <- PCCF_lookup %>%
  distinct(DAUID, FEDUID) %>%
  left_join(FED_lookup, by = c("FEDUID")) %>%
  rename(FED_name = name) %>%
  select(DAUID, FED_name) %>%
  mutate(DAUID = as.character(DAUID))

census_lookup_FED <- census_lookup %>%
  filter(missing_stage == "complete") %>% # TODO: deal with this more carefully later.
  left_join(DAUID_to_FED_name, by = c("DAUID")) %>%
  group_by(FED_name) %>%
  summarise(
    total_population = sum(population),
    land_area_sqkm = sum(land_area_sqkm),
    density_per_sqkm = sum(population) / sum(land_area_sqkm),
    avg_age = weighted.mean(avg_age, population),
    median_hh_income = weighted.mean(median_hh_income, population),
    vismin_total = weighted.mean(vismin_total, population),
    vismin_south_asian = weighted.mean(vismin_south_asian, population),
    vismin_chinese = weighted.mean(vismin_chinese, population),
    vismin_black = weighted.mean(vismin_black, population),
    vismin_filipino = weighted.mean(vismin_filipino, population),
    vismin_arab = weighted.mean(vismin_arab, population),
    vismin_latin_american = weighted.mean(vismin_latin_american, population),
    vismin_southeast_asian = weighted.mean(vismin_southeast_asian, population),
    vismin_west_asian = weighted.mean(vismin_west_asian, population),
    vismin_korean = weighted.mean(vismin_korean, population),
    vismin_japanese = weighted.mean(vismin_japanese, population),
    postsec_prop = weighted.mean(postsec_prop, population)
  )
# 1.3. Join FED Census with `FED_local_donations_data`.
FED_model_data <- FED_local_donations_data %>%
  left_join(census_lookup_FED, by = c("donor_district" = "FED_name"))

# 2. Fit and Diagnose Models.

# 2.1. Determine model covariates.
FED_model_covariates <- c(
  "density_per_sqkm",
  "total_amount",
  "avg_age",
  "median_hh_income",
  "postsec_prop",
  "vismin_total"
)

# 2.2. Plot marginal distributions of responses under logit, to verify normality.
FED_model_data %>%
  ggplot(aes(x = log(prop_ood / (1 - prop_ood)))) +
  geom_histogram(binwidth = 0.4) +
  theme_classic()

FED_model_data %>%
  mutate(prop_ood_logit = log(prop_ood / (1 - prop_ood))) %>%
  ggplot(aes(sample = prop_ood_logit)) +
  stat_qq() +
  stat_qq_line() +
  theme_classic()

# 2.2. Plot pair scatterplots with covariates.
FED_model_data %>%
  ungroup() %>%
  mutate(prop_ood_logit = log(prop_ood / (1 - prop_ood))) %>%
  mutate(density_per_sqkm_sqrt = sqrt(density_per_sqkm)) %>%
  mutate(total_amount_log = log(total_amount)) %>%
  mutate(median_hh_income_log = log(median_hh_income)) %>%
  mutate(vismin_prop_logit = log(vismin_total / (1 - vismin_total))) %>%
  mutate(postsec_prop_logit = log(postsec_prop / (1 - postsec_prop))) %>%
  select(
    prop_ood_logit,
    density_per_sqkm_sqrt,
    total_amount_log,
    median_hh_income_log,
    avg_age,
    postsec_prop_logit,
    vismin_prop_logit
  ) %>%
  pairs()

# 2.3. Fit model.
FED_model <- lm(
  log(
    FED_model_data$prop_ood /
      (1 - FED_model_data$prop_ood)
  ) ~
    # sqrt(FED_model_data$density_per_sqkm) +
    FED_model_data$avg_age +
    log(FED_model_data$median_hh_income) +
    log(FED_model_data$total_amount) +
    log(
      FED_model_data$vismin_total /
        (1 - FED_model_data$vismin_total)
    ) +
    log(
      FED_model_data$postsec_prop /
        (1 - FED_model_data$postsec_prop)
    )
)
# DIAGNOSTICS:
# summary(FED_model)
# qqnorm(FED_model$residuals)
# qqline(FED_model$residuals)
# plot(
#   x = FED_model$residuals,
#   y = FED_model$fitted.values
# )

plot(
  y = log(FED_model_data$prop_ood / (1 - FED_model_data$prop_ood)),
  x = FED_model_data$avg_age
)

lm(
  log(
    FED_model_data$prop_ood / (1 - FED_model_data$prop_ood)
  ) ~ FED_model_data$avg_age
)

# ------ Save Models to File ------
saveRDS(FED_model, "other/fitted_models/full_FED_model_base.rds")
