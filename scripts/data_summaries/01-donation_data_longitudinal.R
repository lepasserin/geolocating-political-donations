# Purpose: Visualize `donations_data` Longitudinally through Figures and Tables.
# Author: Benedict Cummins-Mburu
# Last Updated: 5 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(arrow)
library(cowplot)
library(slider)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
# TODO: refactor these changes into the cleaning script.
donations_data <- donations_data %>%
  mutate(
    electoral_event_clean = case_when(
      donation_date >= as.Date("2015-08-02") &
        donation_date <= as.Date("2015-10-19") ~ "General Election",
      donation_date >= as.Date("2019-09-11") &
        donation_date <= as.Date("2019-10-21") ~ "General Election",
      donation_date >= as.Date("2021-08-15") &
        donation_date <= as.Date("2021-09-20") ~ "General Election",
      TRUE ~ "Non-Election"
    ),
    total_amount = amount_monetary + amount_non_monetary
  ) %>%
  filter(total_amount <= 25000) # removing 2 errors ; SHOULD DO THIS IN THE CLEANING SCRIPT!!

# -- Constants & Helpers --
NATIONAL <- c("Registered parties", "Leadership contestants")
ENTITY_COLOURS <- c(
  "Registered parties" = "#0072B2",
  "Leadership contestants" = "#009E73",
  "Candidates" = "#E69F00",
  "Registered associations" = "#D55E00"
)
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
    mutate(
      month_num = lubridate::month(month),
      year_num = lubridate::year(month)
    ) %>%
    group_by(across(all_of(stratifying_column)), year_num) %>%
    mutate(
      true_dec_amount = sum(total_donated[month_num == 12], na.rm = TRUE),
      true_dec_count = sum(n_donors[month_num == 12], na.rm = TRUE),

      dec_increment_amount = true_dec_amount / 12,
      dec_increment_count = true_dec_count / 12,

      amt_no_dec = if_else(month_num == 12, NA_real_, total_donated),
      cnt_no_dec = if_else(month_num == 12, NA_real_, n_donors)
    ) %>%
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

      total_donated = total_donated + dec_increment_amount,
      n_donors = n_donors + dec_increment_count
    ) %>%
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


# ------ Figure 2.1 --------

plot_data <- donations_data %>%
  mutate(
    group = if_else(
      political_entity %in% NATIONAL,
      "National entity",
      "Local entity"
    ),
    group = factor(group, levels = c("National entity", "Local entity"))
  ) %>%
  filter(political_entity != "Nomination contestants") %>%
  group_by(electoral_event_clean, group, political_entity) %>%
  summarise(total_amount = sum(total_amount, na.rm = TRUE), .groups = "drop")

make_plot <- function(data, cycle_label, panel_title) {
  data %>%
    filter(electoral_event_clean == cycle_label) %>%
    ggplot(aes(x = group, y = total_amount, fill = political_entity)) +
    geom_col(position = position_stack()) +
    scale_y_continuous(
      labels = scales::dollar_format(scale = 1e-6, suffix = "M")
    ) +
    scale_fill_manual(values = ENTITY_COLOURS, drop = FALSE) +
    labs(x = "Entity Type", y = NULL, fill = NULL, title = panel_title) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
      axis.title.x = element_text(face = "plain"),
      axis.text = element_text(face = "plain")
    )
}
Figure_2_1_a <- make_plot(
  plot_data,
  "General Election",
  "A. During Election Cycles"
)
Figure_2_1_b <- make_plot(
  plot_data,
  "Non-Election",
  "B. Outside Election Cycles"
)
p1 <- Figure_2_1_a + theme(legend.position = "none")
p2 <- Figure_2_1_b + theme(legend.position = "none")
shared_legend <- get_legend(
  Figure_2_1_a +
    theme(
      legend.position = "top",
      legend.justification = "center",
      legend.key.size = unit(0.4, "cm"),
      legend.text = element_text(size = 8)
    )
)
Figure_2_1 <- plot_grid(
  shared_legend,
  plot_grid(p1, p2, nrow = 1),
  ncol = 1,
  rel_heights = c(0.1, 1)
)

# ------ Figure 2.2 --------

Figure_2_2_data <- donations_data %>%
  mutate(
    party_group = if_else(
      political_party %in% PARTIES_NAMED,
      political_party,
      "Other"
    ),
    party_group = factor(
      party_group,
      levels = c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party",
        "Other"
      )
    ),
    month = lubridate::floor_date(donation_date, "month")
  ) %>%
  group_by(month, party_group) %>%
  summarise(
    total_donated = sum(amount_monetary + amount_non_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  rename(political_party = party_group) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_2_2_v1 <- Figure_2_2_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, fill = political_party)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_fill_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

Figure_2_2_v2 <- Figure_2_2_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, color = political_party)) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_color_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(size = 8),
    axis.text = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

# ------ Figure 2.3 --------

Figure_2_3_data <- donations_data %>%
  filter(political_entity %in% c("Candidates", "Registered associations")) %>%
  mutate(
    party_group = if_else(
      political_party %in% PARTIES_NAMED,
      political_party,
      "Other"
    ),
    party_group = factor(
      party_group,
      levels = c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party",
        "Other"
      )
    ),
    month = lubridate::floor_date(donation_date, "month")
  ) %>%
  group_by(month, party_group) %>%
  summarise(
    total_donated = sum(amount_monetary + amount_non_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  rename(political_party = party_group) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_2_3_v1 <- Figure_2_3_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, fill = political_party)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_fill_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

Figure_2_3_v2 <- Figure_2_3_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, color = political_party)) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_color_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

# ------ Figure 2.4 (In-District: Candidates & EDAs) --------

Figure_2_4_data <- donations_data %>%
  filter(
    political_entity %in% c("Candidates", "Registered associations"),
    !is_out_of_district
  ) %>%
  mutate(
    party_group = if_else(
      political_party %in% PARTIES_NAMED,
      political_party,
      "Other"
    ),
    party_group = factor(
      party_group,
      levels = c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party",
        "Other"
      )
    ),
    month = lubridate::floor_date(donation_date, "month")
  ) %>%
  group_by(month, party_group) %>%
  summarise(
    total_donated = sum(amount_monetary + amount_non_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  rename(political_party = party_group) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_2_4_v1 <- Figure_2_4_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, fill = political_party)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_fill_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

Figure_2_4_v2 <- Figure_2_4_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, color = political_party)) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_color_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

# ------ Figure 2.5 (Out-of-District: Candidates & EDAs) --------

Figure_2_5_data <- donations_data %>%
  filter(
    political_entity %in% c("Candidates", "Registered associations"),
    is_out_of_district
  ) %>%
  mutate(
    party_group = if_else(
      political_party %in% PARTIES_NAMED,
      political_party,
      "Other"
    ),
    party_group = factor(
      party_group,
      levels = c(
        "Conservative Party of Canada",
        "Liberal Party of Canada",
        "New Democratic Party",
        "Other"
      )
    ),
    month = lubridate::floor_date(donation_date, "month")
  ) %>%
  group_by(month, party_group) %>%
  summarise(
    total_donated = sum(amount_monetary + amount_non_monetary, na.rm = TRUE),
    n_donors = n(),
    .groups = "drop"
  ) %>%
  rename(political_party = party_group) %>%
  redistribute_december_ma(stratifying_column = "political_party")

Figure_2_5_v1 <- Figure_2_5_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, fill = political_party)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.3) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_fill_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    fill = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

Figure_2_5_v2 <- Figure_2_5_data %>%
  ggplot(aes(x = month, y = total_donated / 1e6, color = political_party)) +
  geom_line(linewidth = 0.8) +
  geom_vline(
    xintercept = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    color = "darkgray",
    linetype = "dashed",
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = as.Date(c("2015-10-19", "2019-10-21", "2021-09-20")),
    y = Inf,
    label = c("Oct 19, 2015", "Oct 21, 2019", "Sep 20, 2021"),
    angle = 90,
    hjust = 1.1,
    vjust = -0.4,
    size = 2,
    color = "darkgray"
  ) +
  scale_color_manual(values = PARTY_COLOURS) +
  scale_x_date(
    date_breaks = "1 year",
    date_minor_breaks = "1 month",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Monthly Donation Total (in millions of dollars)",
    color = NULL
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y = element_text(face = "plain", size = 8),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 8),
    panel.grid.minor.x = element_line(color = "grey90", linewidth = 0.3),
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(5, "mm")
  )

# ------ Table 2.1 --------

Table_2_1 <- donations_data %>%
  filter(political_entity %in% c("Candidates", "Registered associations")) %>%
  mutate(
    entity_cycle = case_when(
      political_entity == "Candidates" &
        electoral_event_clean == "General Election" ~ "Candidates (in-cycle)",
      political_entity == "Candidates" &
        electoral_event_clean == "Non-Election" ~ "Candidates (out-of-cycle)",
      political_entity == "Registered associations" &
        electoral_event_clean == "General Election" ~ "EDAs (in-cycle)",
      political_entity == "Registered associations" &
        electoral_event_clean == "Non-Election" ~ "EDAs (out-of-cycle)"
    ),
    donation_year = lubridate::year(donation_date)
  ) %>%
  group_by(entity_cycle, donation_year) %>%
  summarise(
    yr_amt = sum(total_amount, na.rm = TRUE),
    yr_cnt = n(),
    yr_ood_amt = sum(total_amount[is_out_of_district], na.rm = TRUE),
    yr_ood_cnt = sum(is_out_of_district, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  summarise(
    `Mean Annual Sum` = round(mean(yr_amt)),
    `Mean Annual Count` = round(mean(yr_cnt)),
    `OOD % (Sum)` = round(mean(yr_ood_amt) / mean(yr_amt), 2),
    `OOD % (Count)` = round(mean(yr_ood_cnt) / mean(yr_cnt), 2),
    .groups = "drop"
  ) %>%
  rename(Entity = entity_cycle) %>%
  arrange(Entity)

# ---- Write to File ------
saveRDS(Figure_2_1, "other/figures/Figure_2_1.rds")
saveRDS(Figure_2_2_v1, "other/figures/Figure_2_2_v1.rds")
saveRDS(Figure_2_2_v2, "other/figures/Figure_2_2_v2.rds")
saveRDS(Figure_2_3_v1, "other/figures/Figure_2_3_v1.rds")
saveRDS(Figure_2_3_v2, "other/figures/Figure_2_3_v2.rds")
saveRDS(Figure_2_4_v1, "other/figures/Figure_2_4_v1.rds")
saveRDS(Figure_2_4_v2, "other/figures/Figure_2_4_v2.rds")
saveRDS(Figure_2_5_v1, "other/figures/Figure_2_5_v1.rds")
saveRDS(Figure_2_5_v2, "other/figures/Figure_2_5_v2.rds")
write_csv(Table_2_1, "other/tables/Table_2_1.csv")
