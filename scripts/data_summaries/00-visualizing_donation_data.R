# Purpose: Visualize `donations_data` through Figures and Tables.
# Author: Benedict Cummins-Mburu
# Last Updated: 4 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(lubridate)
library(arrow)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")

national_donations <- donations_data %>%
  filter(
    political_entity %in% c("Leadership contestants", "Registered parties")
  )

local_donations <- donations_data %>%
  filter(
    political_entity %in%
      c("Candidates", "Nomination contestants", "Registered associations")
  )

if (nrow(local_donations) + nrow(national_donations) == nrow(donations_data)) {
  message("Validation Passed.")
} else {
  stop(
    "Validation Error: Some rows were lost in splitting data by national vs. local donations."
  )
}

# TODO: create generic TS plotting and summary-building functions later.

# ----- Create Tables ------

# -- Longitudinal Figures --

# 1. TS of national political donations (aggregated monthly ; in 100,000 CAD)
Figure_1_data <- national_donations %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  )
Figure_1_plot <- Figure_1_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, color = political_entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Federal Political Donations to National Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Total (in $100,000)",
    color = "Entity"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

# 2. TS of in-district local political donations (aggregated monthly ; in 100,000 CAD)
Figure_2_data <- local_donations %>%
  filter(!is_out_of_district) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    .groups = "drop",
    n_donors = n(),
  )
Figure_2_plot <- Figure_2_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, color = political_entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 31)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "In-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Total (in $100,000)",
    color = "Entity"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

# 3. TS of out-of-district local political donations (aggregated monthly ; in 100,000 CAD)
Figure_3_data <- local_donations %>%
  filter(is_out_of_district) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  )
Figure_3_plot <- Figure_3_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, color = political_entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 31)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Out-of-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Total (in $100,000)",
    color = "Entity Type"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )
# Obs Fake for 2. and 3.: systematic peaks in December of each year for Registered associations is likely not real.
# Obs Fake for 2. and 3.: lack of donations to registered associations is likely not real.
# Obs Real: During election cycles, the amount donated to Local political entities in and out of district is similar. Particularly among registered parties. However, off-cycle, contributions to registered parties are more likely to originate within their home district.
# Obs Real: Total monthly contributions to national parties have been steadily increasing over time, a trend observed between 20XX and 2015 by McMahon & Wolfe (2015). Also, the trend isn't dominated by in-election contributions. National party contributions also make up the majority (X%) of all contributions. By contrast, trends in donations to all other types of entities are dominated by alternation on and off of election cycles. Even to registered district associations, which don't "campain" like the others (not sure about this, need to flatten peaks first).

# 4. TS of Registered Party donations by Party (for only the big 4 or 5 Parties)

# -- Save Visualizations ---
saveRDS()
