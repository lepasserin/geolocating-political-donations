# Purpose: Visualize spatial trends in `donations_data` at the district level.
# Author: Benedict Cummins-Mburu
# Last Updated: 12 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
library(tidyverse)
library(arrow)
library(cowplot)
library(ggrepel)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
FED_donations_data <- read_parquet(
  "data/processed_data/FED_donations_data.parquet"
)

LOCAL_ENTITIES <- c("Candidates", "Registered associations")
TOP_PARTIES <- c(
  "Conservative Party of Canada",
  "Liberal Party of Canada",
  "New Democratic Party",
  "Green Party of Canada"
)

# ----- Figure Wrappers -----

# Wrapper for Versions of Figure 3.1 and 3.2
make_ood_histograms <- function(data, edge_col = "donation_amount_all_local") {
  ood_edges <- data %>%
    filter(sending_district != receiving_district)

  sent <- ood_edges %>%
    group_by(district = sending_district) %>%
    summarise(value = sum(.data[[edge_col]], na.rm = TRUE), .groups = "drop")

  received <- ood_edges %>%
    group_by(district = receiving_district) %>%
    summarise(value = sum(.data[[edge_col]], na.rm = TRUE), .groups = "drop")

  make_panel <- function(df, panel_title) {
    mean_val <- mean(df$value, na.rm = TRUE)

    ggplot(df, aes(x = value)) +
      geom_histogram(
        aes(y = after_stat(count / sum(count))),
        fill = "grey60",
        color = "white",
        bins = 30
      ) +
      geom_vline(
        xintercept = mean_val,
        color = "#D55E00",
        linetype = "solid",
        linewidth = 0.8
      ) +
      annotate(
        "text",
        x = mean_val,
        y = Inf,
        label = scales::label_number(scale = 1e-3, accuracy = 1)(mean_val),
        hjust = -0.1,
        vjust = 1.5,
        size = 3,
        color = "#D55E00"
      ) +
      scale_x_continuous(
        labels = scales::label_number(scale = 1e-3, accuracy = 1)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(
        title = panel_title,
        x = "Contributed Amount (in 1000s of dollars)",
        y = NULL
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 10),
        text = element_text(size = 8)
      )
  }
  p1 <- make_panel(sent, "A. Distribution of Senders")
  p2 <- make_panel(received, "B. Distribution of Recipients")
  plot_grid(p1, p2, nrow = 1)
}

# Wrapper for Versions of Figure 3.3
make_sent_vs_received <- function(
  data,
  edge_col = "donation_amount_all_local"
) {
  ood_edges <- data %>% filter(sending_district != receiving_district)

  sent <- ood_edges %>%
    group_by(district = sending_district) %>%
    summarise(sent = sum(.data[[edge_col]], na.rm = TRUE), .groups = "drop")

  received <- ood_edges %>%
    group_by(district = receiving_district) %>%
    summarise(received = sum(.data[[edge_col]], na.rm = TRUE), .groups = "drop")

  sent_vs_received <- full_join(sent, received, by = "district") %>%
    replace_na(list(sent = 0, received = 0))

  to_label <- union(
    slice_max(sent_vs_received, sent, n = 3)$district,
    slice_max(sent_vs_received, received, n = 3)$district
  )
  sent_vs_received <- sent_vs_received %>%
    mutate(label = if_else(district %in% to_label, district, NA_character_))

  ggplot(sent_vs_received, aes(x = sent, y = received)) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "darkgray",
      linewidth = 0.7
    ) +
    geom_point(color = "grey40", size = 1.5, alpha = 0.7) +
    geom_text_repel(
      aes(label = label),
      na.rm = TRUE,
      size = 2.5,
      min.segment.length = 0,
      segment.color = "grey70"
    ) +
    scale_x_continuous(
      labels = scales::label_number(scale = 1e-3, accuracy = 1)
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale = 1e-3, accuracy = 1)
    ) +
    labs(
      x = "Amount Sent Out-of-District (thousands of dollars)",
      y = "Amount Received from OOD (thousands of dollars)"
    ) +
    theme_classic() +
    theme(axis.text = element_text(size = 8))
}

# Wrapper for Versions of Figure 3.4
make_self_vs_ood <- function(data, edge_col = "donation_amount_all_local") {
  ood_edges <- data %>% filter(sending_district != receiving_district)
  self_edges <- data %>% filter(sending_district == receiving_district)

  sent <- ood_edges %>%
    group_by(district = sending_district) %>%
    summarise(sent = sum(.data[[edge_col]], na.rm = TRUE), .groups = "drop")

  self <- self_edges %>%
    group_by(district = sending_district) %>%
    summarise(self = sum(.data[[edge_col]], na.rm = TRUE), .groups = "drop")

  self_vs_ood <- full_join(self, sent, by = "district") %>%
    replace_na(list(self = 0, sent = 0))

  to_label <- union(
    slice_max(self_vs_ood, self, n = 3)$district,
    slice_max(self_vs_ood, sent, n = 3)$district
  )
  self_vs_ood <- self_vs_ood %>%
    mutate(label = if_else(district %in% to_label, district, NA_character_))

  ggplot(self_vs_ood, aes(x = self, y = sent)) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "darkgray",
      linewidth = 0.7
    ) +
    geom_point(color = "grey40", size = 1.5, alpha = 0.7) +
    geom_text_repel(
      aes(label = label),
      na.rm = TRUE,
      size = 2.5,
      min.segment.length = 0,
      segment.color = "grey70"
    ) +
    scale_x_continuous(
      labels = scales::label_number(scale = 1e-3, accuracy = 1)
    ) +
    scale_y_continuous(
      labels = scales::label_number(scale = 1e-3, accuracy = 1)
    ) +
    labs(
      x = "Amount Donated In-District (thousands of dollars)",
      y = "Amount Donated Out-of-District (thousands of dollars)"
    ) +
    theme_classic() +
    theme(axis.text = element_text(size = 8))
}

fed_props <- function(data, metric, direction) {
  key <- if (direction == "Sent") "sending_district" else "receiving_district"
  data %>%
    mutate(amt = .data[[metric]]) %>%
    group_by(fed = .data[[key]]) %>%
    summarise(
      total = sum(amt, na.rm = TRUE),
      ood = sum(amt[sending_district != receiving_district], na.rm = TRUE),
      ood_oop = sum(
        amt[
          sending_district != receiving_district &
            sending_province != receiving_province
        ],
        na.rm = TRUE
      ),
      ood_nonadj = sum(
        amt[
          sending_district != receiving_district &
            !are_neighbors
        ],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    transmute(
      prop_ood = ood / total,
      prop_oop_of_ood = ood_oop / ood,
      prop_nonadj_of_ood = ood_nonadj / ood
    )
}
mean_prop <- function(data, metric, column, direction) {
  mean(fed_props(data, metric, direction)[[column]], na.rm = TRUE)
}
make_ood_breakdown_table <- function(
  data,
  edge_col = "donation_amount_all_local"
) {
  tibble(
    Statistic = c(
      "Mean Proportion of OODs from Out-of-Province",
      "Mean Proportion of OODs from Out-of-Province",
      "Mean Proportion of OODs from Non-Adjascent Districts",
      "Mean Proportion of OODs from Non-Adjascent Districts"
    ),
    Direction = c("Sent", "Received", "Sent", "Received"),
    Value = scales::percent(
      c(
        mean_prop(data, edge_col, "prop_oop_of_ood", "Sent"),
        mean_prop(data, edge_col, "prop_oop_of_ood", "Received"),
        mean_prop(data, edge_col, "prop_nonadj_of_ood", "Sent"),
        mean_prop(data, edge_col, "prop_nonadj_of_ood", "Received")
      ),
      accuracy = 0.1
    )
  )
}

party_ood_received <- function(donations_data, party, entity) {
  donations_data %>%
    filter(political_entity == entity, political_party == party) %>%
    group_by(recipient_district) %>%
    summarise(
      total = sum(total_amount),
      ood = sum(total_amount[is_out_of_district]),
      .groups = "drop"
    ) %>%
    mutate(prop_ood = ood / total) %>%
    summarise(m = mean(prop_ood, na.rm = TRUE)) %>%
    pull(m)
}

# --------- Figures ---------

Figure_3_1_v1 <- make_ood_histograms(
  FED_donations_data,
  edge_col = "donation_amount_EDA_all"
)
Figure_3_2_v1 <- make_ood_histograms(
  FED_donations_data,
  edge_col = "donation_amount_cand_on"
)

Figure_3_3_v1 <- make_sent_vs_received(
  FED_donations_data,
  edge_col = "donation_amount_EDA_off"
)
Figure_3_3_v2 <- make_sent_vs_received(
  FED_donations_data,
  edge_col = "donation_amount_cand_on"
)

Figure_3_4_v1 <- make_self_vs_ood(
  FED_donations_data,
  edge_col = "donation_amount_EDA_off"
)
Figure_3_4_v2 <- make_self_vs_ood(
  FED_donations_data,
  edge_col = "donation_amount_cand_on"
)

# ---------- Tables ---------

Table_3_1 <- tibble(
  Statistic = c(
    "Mean Proportion of Candidate-Bound Contributions Received from Out-of-District",
    "Mean Proportion of Candidate-Bound Contributions Sent Out-of-District",
    "Mean Proportion of EDA-Bound Contributions Received from Out-of-District",
    "Mean Proportion of EDA-Bound Contributions Sent Out-of-District"
  ),
  Value = scales::percent(
    c(
      mean_prop(
        FED_donations_data,
        "donation_amount_cand_on",
        "prop_ood",
        "Received"
      ),
      mean_prop(
        FED_donations_data,
        "donation_amount_cand_on",
        "prop_ood",
        "Sent"
      ),
      mean_prop(
        FED_donations_data,
        "donation_amount_EDA_all",
        "prop_ood",
        "Received"
      ),
      mean_prop(
        FED_donations_data,
        "donation_amount_EDA_all",
        "prop_ood",
        "Sent"
      )
    ),
    accuracy = 0.1
  )
)

Table_3_2 <- tidyr::crossing(
  Party = TOP_PARTIES,
  Entity = c("Candidate", "EDA")
) %>%
  mutate(
    political_entity = if_else(
      Entity == "Candidate",
      "Candidates",
      "Registered associations"
    ),
    `% Donations Received from OOD` = scales::percent(
      map2_dbl(
        Party,
        political_entity,
        ~ party_ood_received(donations_data, .x, .y)
      ),
      accuracy = 0.1
    )
  ) %>%
  select(-political_entity)

Table_3_3 <- make_ood_breakdown_table(
  FED_donations_data,
  edge_col = "donation_amount_EDA_all"
)

Table_3_4 <- make_ood_breakdown_table(
  FED_donations_data,
  edge_col = "donation_amount_cand_on"
)

# ------ Save to File -------
write_csv(Table_3_1, "other/tables/Table_3_1.csv")
write_csv(Table_3_2, "other/tables/Table_3_2.csv")
write_csv(Table_3_3, "other/tables/Table_3_3.csv")
write_csv(Table_3_4, "other/tables/Table_3_4.csv")
saveRDS(Figure_3_1_v1, "other/figures/Figure_3_1_v1.rds")
saveRDS(Figure_3_2_v1, "other/figures/Figure_3_2_v1.rds")
saveRDS(Figure_3_3_v1, "other/figures/Figure_3_3_v1.rds")
saveRDS(Figure_3_3_v2, "other/figures/Figure_3_3_v2.rds")
saveRDS(Figure_3_4_v1, "other/figures/Figure_3_4_v1.rds")
saveRDS(Figure_3_4_v2, "other/figures/Figure_3_4_v2.rds")
