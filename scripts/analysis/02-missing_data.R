# Purpose: Carry out analyses and create visualizatons discussed in Appendix @sec-missing-data.
# Author: Benedict Cummins-Mburu
# Last Updated: 5 July 2026
# STATUS: TODO
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# -------- Setup --------
library(tidyverse)
library(arrow)
library(sf)
library(patchwork)
donations_data_appendix <- read_parquet(
  "data/processed_data/donations_data_appendix.parquet"
)
FED_lookup <- readRDS("data/clean_data/FED_lookup.rds") %>%
  st_drop_geometry() %>%
  select(name, PRUID)

# -------- Report Overall Missingness --------

missing_total_all <- donations_data_appendix %>%
  group_by(is.na(donor_district)) %>%
  summarise(n = n(), sum = sum(total_amount))

missing_total_local <- donations_data_appendix %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  group_by(is.na(donor_district)) %>%
  summarise(n = n(), sum = sum(total_amount))

missing_aggregated_all <- donations_data_appendix %>%
  filter(is.na(donor_district)) %>%
  group_by(is_aggregated) %>%
  summarise(n = n(), sum = sum(total_amount))

missing_aggregated_local <- donations_data_appendix %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  filter(is.na(donor_district)) %>%
  group_by(is_aggregated) %>%
  summarise(n = n(), sum = sum(total_amount))

# -------- Comparative Distribution Plots --------

# 0. Constants
STUDY_COLOUR <- "#0072B2"
TOP_PARTIES <- c(
  "Conservative Party of Canada",
  "Liberal Party of Canada",
  "New Democratic Party"
)
LOCAL_ENTITIES <- c(
  "Candidates",
  "Registered associations"
)
PROVINCE_NAMES <- c(
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
district_to_province <- FED_lookup %>%
  mutate(
    recipient_province = factor(
      PROVINCE_NAMES[as.character(PRUID)],
      levels = PROVINCE_NAMES
    )
  )

# 1. Helper: Setup plot panel. Bars are ordered by "Study Data" proportion.
build_panel <- function(df, xvar, colours, xlabels = NULL) {
  study <- df[df$data_status == "Study Data", ]
  ordered_levels <- as.character(study[[xvar]])[order(-study$prop)]
  df[[xvar]] <- factor(
    df[[xvar]],
    levels = union(ordered_levels, levels(df[[xvar]]))
  )

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

donations_data_appendix <- donations_data_appendix %>%
  filter(is_aggregated | total_amount < 200)

donations_data_appendix <- donations_data_appendix %>%
  filter(is_aggregated | total_amount >= 200)


# 2. Plotting Function:
#      - compare the marginal distributions of a subset of unlocatable local donations against the geolocated study data.
#      - 5 factors considered: political-entity, political_party, electoral_event, general_election_period, recipient_province.
#      - aggregated = TRUE  -> anonymized aggregated contribution sums (red).
#      - aggregated = FALSE -> unlocated individual contributions      (orange).
plot_comparative_distributions <- function(aggregated = TRUE) {
  # 2.0. Configure depending on `aggregated`
  if (aggregated) {
    comparison_subset <- donations_data_appendix %>%
      filter(political_entity %in% LOCAL_ENTITIES) %>%
      filter(is_aggregated)
    comparison_label <- "Aggregated"
    comparison_colour <- "#D55E00"
  } else {
    comparison_subset <- donations_data_appendix %>%
      filter(political_entity %in% LOCAL_ENTITIES) %>%
      filter(!is_aggregated) %>%
      filter(is.na(donor_district))
    comparison_label <- "Unlocated Individual Data"
    comparison_colour <- "#E69F00"
  }
  colours <- c("Study Data" = STUDY_COLOUR)
  colours[comparison_label] <- comparison_colour

  study_subset <- donations_data_appendix %>%
    filter(political_entity %in% LOCAL_ENTITIES) %>%
    filter(!is.na(donor_district))

  comparison_data <- bind_rows(
    comparison_subset %>% mutate(data_status = comparison_label),
    study_subset %>% mutate(data_status = "Study Data")
  ) %>%
    mutate(data_status = factor(data_status, levels = names(colours)))
  # 2.1. Recipient type (political_entity)
  entity_data <- comparison_data %>%
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
  p_entity <- build_panel(entity_data, "entity", colours)

  # 2.2. Recipient party (political_party)
  party_data <- comparison_data %>%
    mutate(
      party = factor(
        if_else(political_party %in% TOP_PARTIES, political_party, "Other"),
        levels = c(TOP_PARTIES, "Other")
      )
    ) %>%
    group_by(data_status, party) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_party <- build_panel(
    party_data,
    "party",
    colours,
    xlabels = c(
      "Conservative Party of Canada" = "Conservative",
      "Liberal Party of Canada" = "Liberal",
      "New Democratic Party" = "NDP",
      "Other" = "Other"
    )
  )
  # 2.3. Electoral event (electoral_event)
  event_data <- comparison_data %>%
    filter(!is.na(electoral_event)) %>%
    mutate(
      event = factor(
        electoral_event,
        levels = c("General election", "By-election", "Non-electoral")
      )
    ) %>%
    group_by(data_status, event) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_event <- build_panel(
    event_data,
    "event",
    colours,
    xlabels = c(
      "General election" = "General",
      "By-election" = "By-election",
      "Non-electoral" = "Non-electoral"
    )
  )
  # 2.4. General elections (electoral_event_OG, within General elections + general_election_period)
  gen_election_data <- comparison_data %>%
    filter(electoral_event == "General election") %>%
    mutate(
      election = factor(
        electoral_event_OG,
        levels = c(
          "42nd general election",
          "43rd general election",
          "44th general election"
        )
      )
    ) %>%
    group_by(data_status, election) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_gen_election <- build_panel(
    gen_election_data,
    "election",
    colours,
    xlabels = c(
      "42nd general election" = "42nd",
      "43rd general election" = "43rd",
      "44th general election" = "44th"
    )
  )
  # 2.5. Recipient province (recipient_province, derived from recipient_district)
  province_data <- comparison_data %>%
    left_join(district_to_province, by = c("recipient_district" = "name")) %>%
    filter(!is.na(recipient_province)) %>%
    group_by(data_status, recipient_province) %>%
    summarise(
      amount = sum(total_amount, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    mutate(prop = amount / sum(amount)) %>%
    ungroup()
  p_province <- build_panel(province_data, "recipient_province", colours)

  # 2.6. Combine.
  panel_title <- theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10)
  )
  p_entity <- p_entity + labs(title = "A. Recipient Type") + panel_title
  p_party <- p_party + labs(title = "B. Recipient Party") + panel_title
  p_event <- p_event + labs(title = "C. Electoral Event") + panel_title
  p_gen_election <- p_gen_election +
    labs(title = "D. General Election") +
    panel_title
  p_province <- p_province + labs(title = "E. Recipient Province") + panel_title
  p_all <- (p_entity | p_party) /
    (p_event | p_gen_election) /
    (p_province) +
    plot_layout(guides = "collect") &
    theme(legend.position = "top")
  p_all & theme(axis.title.y = element_blank())
}

# 3. Build plots.
fig_aggregated <- plot_comparative_distributions(aggregated = TRUE)
fig_unlocated <- plot_comparative_distributions(aggregated = FALSE)

# 4. Save plots to File.
saveRDS(
  fig_aggregated,
  "other/appendix_figures/distribution_comparison_aggregated.rds"
)
saveRDS(
  fig_unlocated,
  "other/appendix_figures/distribution_comparison_unlocated.rds"
)
