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

# ----- Create Figures ------

# CONTRIBUTIONS BY RECIPIENT TYPE OVER TIME

# 1. TS of national political donations (aggregated monthly ; in 100,000 CAD)
Figure_1_data <- national_donations %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  )
Figure_1a_plot <- Figure_1_data %>%
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
Figure_1b_plot <- Figure_1_data %>%
  ggplot(aes(x = month, y = n_donors / 1e3, color = political_entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Federal Political Donations to National Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 1000 people)",
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
Figure_2a_plot <- Figure_2_data %>%
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
Figure_2b_plot <- Figure_2_data %>%
  ggplot(aes(x = month, y = n_donors / 1e2, color = political_entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 65)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "In-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 100 people)",
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
Figure_3a_plot <- Figure_3_data %>%
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
Figure_3b_plot <- Figure_3_data %>%
  ggplot(aes(x = month, y = n_donors / 1e2, color = political_entity)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 65)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Out-of-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 100 people)",
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

# CONTRIBUTIONS BY PARTY OVER TIME

# 4. TS of Registered Party donations by Party (for only the top 4 Parties)

Figure_4_data <- national_donations %>%
  filter(
    political_party %in%
      c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party",
        "Green Party of Canada"
      )
  ) %>%
  filter(political_entity == "Registered parties") %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_party) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  )
Figure_4a_plot <- Figure_4_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "National Party Donations to the Top 5 Political Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Entity Type"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )
Figure_4b_plot <- Figure_4_data %>%
  ggplot(aes(x = month, y = n_donors / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "National Party Donations to the Top 5 Political Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Entity Type"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )
# Obs Real: Bloc Quebecois donations were negligibly small and scarce, so not included on graph.

# 5. TS of all out-of-district-donations by Party (for only the big 4 Parties)

Figure_5_data <- local_donations %>%
  filter(is_out_of_district) %>%
  filter(
    political_party %in%
      c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party"
      )
  ) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_party) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  )
Figure_5a_plot <- Figure_5_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Out-of-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Party"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )
Figure_5b_plot <- Figure_5_data %>%
  ggplot(aes(x = month, y = n_donors / 1e3, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Out-of-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donor Count (in 1000 people)",
    fill = "Party"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )

# 6. TS of all in-district donations by Party (for only the top 4 Parties)

Figure_6_data <- local_donations %>%
  filter(!is_out_of_district) %>%
  filter(
    political_party %in%
      c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party"
      )
  ) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_party) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  )
Figure_6a_plot <- Figure_6_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "In-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Party"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )
Figure_6b_plot <- Figure_6_data %>%
  ggplot(aes(x = month, y = n_donors / 1e3, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "In-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donor Count (in 1000 people)",
    fill = "Party"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12)
  )
# Note: Green party removed here, since negligible compared to others. This tells us that Green party donations happen at the national level, not to any local entities, both in abd out of district.

# -- Save Visualizations ---
saveRDS(Figure_1a_plot, "other/figures/Figure_1a.rds")
saveRDS(Figure_1b_plot, "other/figures/Figure_1b.rds")
saveRDS(Figure_2a_plot, "other/figures/Figure_2a.rds")
saveRDS(Figure_2b_plot, "other/figures/Figure_2b.rds")
saveRDS(Figure_3a_plot, "other/figures/Figure_3a.rds")
saveRDS(Figure_3b_plot, "other/figures/Figure_3b.rds")
saveRDS(Figure_4a_plot, "other/figures/Figure_4a.rds")
saveRDS(Figure_4b_plot, "other/figures/Figure_4b.rds")
saveRDS(Figure_5a_plot, "other/figures/Figure_5a.rds")
saveRDS(Figure_5b_plot, "other/figures/Figure_5b.rds")
saveRDS(Figure_6a_plot, "other/figures/Figure_6a.rds")
saveRDS(Figure_6b_plot, "other/figures/Figure_6b.rds")
