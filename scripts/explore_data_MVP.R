# Purpose: Generate Summary Tables and Visualizations of MVP Data
# Author: Benedict Cummins-Mburu
# Last Updated: 27 May 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ------- Setup --------
library(tidyverse)
library(sf)
clean_MVP <- read_csv("data/clean_data/cleaned_data_MVP.csv")
clean_FED_shapefile <- readRDS(
  "data/conversion_files/lfed000b21a_e/lfed000b21a_e.rds"
)


# ------- Longitudinal Summary Table --------

# flag if a donation if out-of-district
focal_data <- clean_MVP %>%
  mutate(OUT_OF_DISTRICT = ifelse(DONOR_FED != RECIPIENT_FED, TRUE, FALSE))

# get the proportion, count,  that the average donor recieves
summary_statistics_long <- focal_data %>%
  group_by(YEAR) %>%
  summarize(
    n = n(),
    gross = sum(amount_monetary),
    n_ood = sum(OUT_OF_DISTRICT),
    prop_ood = n_ood / n
  ) %>%
  mutate(
    avg_daily_n = n / c(78, 40, 36),
    avg_daily_gross = gross / c(78, 40, 36)
  )

# ------- Longitudinal (Recipient/Candidate) Summary Table --------

summary_statistics_long_recipient <- focal_data %>%
  group_by(recipient, YEAR) %>%
  summarize(
    n = n(),
    gross = sum(amount_monetary),
    gross_ood = sum(amount_monetary * OUT_OF_DISTRICT),
    gross_ood_prop = gross_ood / gross,
  ) %>%
  group_by(YEAR) %>%
  summarize(
    n = n(),
    avg_gross = mean(gross),
    avg_gross_ood = mean(gross_ood),
    avg_gross_ood_prop = mean(gross_ood_prop, na.rm = TRUE)
  )


# ------- Spatial (Donor) Summary --------

summary_statistics_wide_full <- focal_data %>%
  group_by(DONOR_FED, YEAR) %>%
  summarize(n = n(), gross = sum(amount_monetary))

# # plot
# mapped_donations <- clean_FED_shapefile %>%
#   left_join(summary_statistics_wide_full, by = c("FEDENAME" = "DONOR_FED")) %>%
#   st_simplify(preserveTopology = TRUE, dTolerance = 1000)

# # 2. Plot the map of Canada
# ggplot(data = mapped_donations) +
#   # Renders the geography from the 'geometry' column
#   geom_sf(aes(fill = gross), color = "white", size = 0.05) +

#   # Formats the fill scale with a clean color scheme
#   scale_fill_viridis_c(
#     name = "Gross Contributions ($)",
#     labels = scales::dollar,
#     na.value = "grey90"
#   )
