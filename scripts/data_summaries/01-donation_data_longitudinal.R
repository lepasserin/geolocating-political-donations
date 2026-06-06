# Purpose: Visualize `donations_data` Longitudinally through Figures and Tables.
# Author: Benedict Cummins-Mburu
# Last Updated: 5 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(lubridate)
library(slider)
library(arrow)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")

# -- Subset Donations Data --

PARTIES_VISUALIZED <- c(
  "Conservative Party of Canada",
  "Liberal Party of Canada",
  "New Democratic Party",
  "Green Party of Canada"
)

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

# ---- Helper Functions ----

redistribute_december_ma <- function(
  data,
  stratifying_column = "political_party"
) {
  required_cols <- c("month", "total_donated", "n_donors", stratifying_column)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(paste(
      "The input data is missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  data %>%
    arrange(across(all_of(stratifying_column)), month) %>%

    # 1. Create a year column alongside the month number
    mutate(
      month_num = lubridate::month(month),
      year_num = lubridate::year(month)
    ) %>%

    # 2. GROUP BY PARTY AND YEAR (keeps annual calculations isolated)
    group_by(across(all_of(stratifying_column)), year_num) %>%
    mutate(
      # These calculations are now strictly local to that specific calendar year!
      true_dec_amount = sum(total_donated[month_num == 12], na.rm = TRUE),
      true_dec_count = sum(n_donors[month_num == 12], na.rm = TRUE),

      dec_increment_amount = true_dec_amount / 12,
      dec_increment_count = true_dec_count / 12,

      amt_no_dec = if_else(month_num == 12, NA_real_, total_donated),
      cnt_no_dec = if_else(month_num == 12, NA_real_, n_donors)
    ) %>%

    # 3. TEMPORARILY DROP YEAR GROUPING FOR THE MOVING AVERAGE
    # The MA smoother needs to see across year boundaries (e.g., Nov -> Dec -> Jan)
    group_by(across(all_of(stratifying_column))) %>%
    mutate(
      ma_amount = slider::slide_dbl(
        amt_no_dec,
        mean,
        na.rm = TRUE,
        .before = 1,
        .after = 1,
        .complete = FALSE
      ),
      ma_count = slider::slide_dbl(
        cnt_no_dec,
        mean,
        na.rm = TRUE,
        .before = 1,
        .after = 1,
        .complete = FALSE
      ),

      total_donated = if_else(month_num == 12, ma_amount, total_donated),
      n_donors = if_else(month_num == 12, ma_count, n_donors),

      # Add back the increments (which were calculated safely per year)
      total_donated = total_donated + dec_increment_amount,
      n_donors = n_donors + dec_increment_count
    ) %>%
    # Clean up and exit
    select(
      -month_num,
      -year_num,
      -amt_no_dec,
      -cnt_no_dec,
      -ma_amount,
      -ma_count,
      -true_dec_amount,
      -true_dec_count,
      -dec_increment_amount,
      -dec_increment_count
    ) %>%
    ungroup()
}

add_election_style <- function() {
  list(
    geom_vline(
      xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
      color = "darkgray",
      linetype = "dashed",
      linewidth = 0.7
    ),
    theme_classic(),
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.title.y = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12)
    ),
    scale_fill_manual(values = c("#377eb8", "#4daf4a", "#e41a1c", "#ff7f00")),
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    )
  )
}

# ----- Create Time-Series ------

# 1. TS of national political donations (aggregated monthly ; in 100,000 CAD)
Figure_1_data <- national_donations %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  redistribute_december_ma(stratifying_column = "political_entity")

Figure_1a_plot <- Figure_1_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_entity)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  labs(
    title = "Federal Political Donations to National Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Total (in $100,000)",
    fill = "Political Entity:"
  ) +
  add_election_style()

Figure_1b_plot <- Figure_1_data %>%
  ggplot(aes(x = month, y = n_donors / 1e3, fill = political_entity)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  labs(
    title = "Federal Political Donations to National Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 1000s of contributions)",
    fill = "Political Entity:"
  ) +
  add_election_style()

# 2. TS of in-district local political donations (aggregated monthly ; in 100,000 CAD)
Figure_2_data <- local_donations %>%
  filter(!is_out_of_district) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  redistribute_december_ma(stratifying_column = "political_entity")

Figure_2a_plot <- Figure_2_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_entity)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  labs(
    title = "In-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Total (in $100,000)",
    fill = "Political Entity:"
  ) +
  add_election_style()

Figure_2b_plot <- Figure_2_data %>%
  ggplot(aes(x = month, y = n_donors / 1e2, fill = political_entity)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  labs(
    title = "In-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Count (in 100s of contributions)",
    fill = "Political Entity:"
  ) +
  add_election_style()

# 3. TS of out-of-district local political donations (aggregated monthly ; in 100,000 CAD)
Figure_3_data <- local_donations %>%
  filter(is_out_of_district) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_entity) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  redistribute_december_ma(stratifying_column = "political_entity")

Figure_3a_plot <- Figure_3_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_entity)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  labs(
    title = "Out-of-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donation Total (in $100,000)",
    fill = "Political Entity:"
  ) +
  add_election_style()

Figure_3b_plot <- Figure_3_data %>%
  ggplot(aes(x = month, y = n_donors / 1e2, fill = political_entity)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  labs(
    title = "Out-of-District Federal Political Donations to Local Entities (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 100s of contributions)",
    fill = "Political Entity:"
  ) +
  add_election_style()

# Obs Fake for 2. and 3.: lack of donations to registered associations in 2019 is likely not real.
# Obs Real: During election cycles, the amount donated to Local political entities in and out of district is similar. Particularly among registered parties. However, off-cycle, contributions to registered parties are more likely to originate within their home district.
# Obs Real: Total monthly contributions to national parties have been steadily increasing over time, a trend observed between 20XX and 2015 by McMahon & Wolfe (2015). Also, the trend isn't dominated by in-election contributions. National party contributions also make up the majority (X%) of all contributions. By contrast, trends in donations to all other types of entities are dominated by alternation on and off of election cycles. Even to registered district associations, which don't "campain" like the others (not sure about this, need to flatten peaks first).

# 4. TS of Registered Party donations by Party (for only the top 4 Parties)

Figure_4_data <- national_donations %>%
  filter(political_party %in% PARTIES_VISUALIZED) %>%
  filter(political_entity == "Registered parties") %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_party) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_4a_plot <- Figure_4_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  labs(
    title = "National Party Donations to the Top 4 Political Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Political Party:"
  ) +
  add_election_style()

Figure_4b_plot <- Figure_4_data %>%
  ggplot(aes(x = month, y = n_donors / 1e3, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  labs(
    title = "National Party Donations to the Top 4 Political Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 1000s of contributions)",
    fill = "Political Party:"
  ) +
  add_election_style()

# Obs Real: Bloc Quebecois donations were negligibly small and scarce, so not included on graph. Also helped reduce visual clutter
# Obs Real: The rise in dnoation count AND amount in recent years can be primarily attributed to a large amount of additional contributions to the Conservatve national party.

# 5. TS of all out-of-district-donations by Party (for only the big 4 Parties)

Figure_5_data <- local_donations %>%
  filter(is_out_of_district) %>%
  filter(political_party %in% PARTIES_VISUALIZED) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_party) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_5a_plot <- Figure_5_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  labs(
    title = "Out-of-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Political Party: "
  ) +
  add_election_style()

Figure_5b_plot <- Figure_5_data %>%
  ggplot(aes(x = month, y = n_donors / 1e2, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  labs(
    title = "Out-of-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 1000s of contributions)",
    fill = "Political Party: "
  ) +
  add_election_style()

# 6. TS of all in-district donations by Party (for only the top 4 Parties)
Figure_6_data <- local_donations %>%
  filter(!is_out_of_district) %>%
  filter(political_party %in% PARTIES_VISUALIZED) %>%
  mutate(month = lubridate::floor_date(donation_date, "month")) %>%
  group_by(month, political_party) %>%
  summarise(
    total_donated = sum(amount_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_6a_plot <- Figure_6_data %>%
  ggplot(aes(x = month, y = total_donated / 1e5, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  labs(
    title = "In-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Cumulative Monthly Donations (in $100,000)",
    fill = "Political Party: "
  ) +
  add_election_style()

Figure_6b_plot <- Figure_6_data %>%
  ggplot(aes(x = month, y = n_donors / 1e2, fill = political_party)) +
  geom_area(color = "white", linewidth = 0.2, alpha = 0.8) +
  labs(
    title = "In-District Donations to Local Entities by Top 4 Parties (Oct 2015 - Apr 2024)",
    x = NULL,
    y = "Monthly Donor Count (in 100s of contributions)",
    fill = "Political Party: "
  ) +
  add_election_style()

# Note: Green party removed here, since negligible compared to others. This tells us that Green party donations happen at the national level, not to any local entities, both in abd out of district.
# TODO: tell Rohan that I see no point in plotting day-by-day time series of election cycles. I think the monthly TS plus the tables are enuogh to make a good point.

# -------- Create Tables --------

raw_combined <- donations_data %>%
  mutate(
    electoral_event_clean = case_when(
      # 42nd GE: Aug 2, 2015 to Oct 19, 2015
      donation_date >= as.Date("2015-08-02") &
        donation_date <= as.Date("2015-10-19") ~ "General Election",

      # 43rd GE: Sept 11, 2019 to Oct 21, 2019
      donation_date >= as.Date("2019-09-11") &
        donation_date <= as.Date("2019-10-21") ~ "General Election",

      # 44th GE: Aug 15, 2021 to Sept 20, 2021
      donation_date >= as.Date("2021-08-15") &
        donation_date <= as.Date("2021-09-20") ~ "General Election",

      # Otherwise
      TRUE ~ "Non-Election"
    )
  )

# Table 1: Summary stats of Off-Election Cycle Donations by political entity
Table_1 <- raw_combined %>%
  filter(electoral_event_clean == "Non-Election") %>%
  group_by(political_entity, donation_year) %>%
  summarise(
    yr_amt = sum(amount_monetary, na.rm = TRUE),
    yr_cnt = n(),
    yr_o_amt = sum(amount_monetary[is_out_of_district == TRUE], na.rm = TRUE),
    yr_o_cnt = sum(is_out_of_district == TRUE, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  summarise(
    avg_annual_total_amount = mean(yr_amt, na.rm = TRUE),
    avg_annual_total_count = mean(yr_cnt, na.rm = TRUE),
    prop_sum_ood = mean(yr_o_amt, na.rm = TRUE) / mean(yr_amt, na.rm = TRUE),
    prop_num_ood = mean(yr_o_cnt, na.rm = TRUE) / mean(yr_cnt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_annual_total_amount))

# Table 2: Summary stats of Election Cycle Donations by political entity
Table_2 <- raw_combined %>%
  filter(electoral_event_clean == "General Election") %>%
  group_by(political_entity) %>%
  summarise(
    total_cycle_amount = sum(amount_monetary, na.rm = TRUE),
    total_cycle_count = n(),
    prop_sum_ood = sum(
      amount_monetary[is_out_of_district == TRUE],
      na.rm = TRUE
    ) /
      total_cycle_amount,
    prop_num_ood = sum(is_out_of_district == TRUE, na.rm = TRUE) /
      total_cycle_count,
    .groups = "drop"
  ) %>%
  arrange(desc(total_cycle_amount))

# Table 3: Summary stats of Donations by political party
Table_3 <- raw_combined %>%
  group_by(political_party, donation_year) %>%
  filter(political_party %in% PARTIES_VISUALIZED) %>%
  summarise(
    yr_amt = sum(amount_monetary, na.rm = TRUE),
    yr_cnt = n(),
    yr_elec_amt = sum(
      amount_monetary[electoral_event_clean == "General Election"],
      na.rm = TRUE
    ),
    yr_elec_cnt = sum(
      electoral_event_clean == "General Election",
      na.rm = TRUE
    ),
    .groups = "drop_last"
  ) %>%
  summarise(
    avg_annual_amount = mean(yr_amt, na.rm = TRUE),
    prop_amount_election = mean(yr_elec_amt, na.rm = TRUE) /
      mean(yr_amt, na.rm = TRUE),
    avg_annual_count = mean(yr_cnt, na.rm = TRUE),
    prop_count_election = mean(yr_elec_cnt, na.rm = TRUE) /
      mean(yr_cnt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_annual_amount))

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
write_csv(Table_1, "other/tables/Table_2_1.csv")
write_csv(Table_2, "other/tables/Table_2_2.csv")
write_csv(Table_3, "other/tables/Table_2_3.csv")
