# Purpose: Visualize `FED_OOD_data` as an Edge-Weighted, Directed Graph.
# Author: Benedict Cummins-Mburu
# Last Updated: 6 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# ---- Setup -----
library(tidyverse)
library(arrow)
library(igraph)
library(graphlayouts)
library(ggraph)
FED_graph_data <- read_parquet("data/processed_data/FED_donations_data.parquet")
PCFRF_2022 <- read_parquet("data/clean_data/clean_PCFRF_2022.parquet")

# ---- Parse province codes from FEDUID ----
# FEDUID format: first two digits are the StatsCan province/territory code
# e.g. 10xxx = NL, 11xxx = PEI, 12xxx = NS, 13xxx = NB, 24xxx = QC,
#      35xxx = ON, 46xxx = MB, 47xxx = SK, 48xxx = AB, 59xxx = BC,
#      60/61/62xxx = territories

PROVINCE_REGION_MAP <- c(
  "10" = "Maritimes", # Newfoundland & Labrador
  "11" = "Maritimes", # Prince Edward Island
  "12" = "Maritimes", # Nova Scotia
  "13" = "Maritimes", # New Brunswick
  "24" = "QC",
  "35" = "ON",
  "46" = "Prairies", # Manitoba
  "47" = "Prairies", # Saskatchewan
  "48" = "Prairies", # Alberta
  "59" = "BC",
  "60" = "Other", # Yukon
  "61" = "Other", # Northwest Territories
  "62" = "Other" # Nunavut
)

fed_to_region <- PCFRF_2022 %>%
  mutate(province_code = str_sub(as.character(FEDUID), 1, 2)) %>%
  mutate(region = PROVINCE_REGION_MAP[province_code]) %>%
  select(name = FED, region) %>%
  distinct()

# ---- Graph function ----

make_fed_graph <- function(
  data,
  weight_col,
  prune_quantile = 0.8,
  node_prune_quantile = 0.05
) {
  weight_sym <- sym(weight_col)

  # Build edge list: drop zero-weight, prune weakest edges
  edges <- data %>%
    select(sending_district, receiving_district, weight = !!weight_sym) %>%
    filter(weight > 0) %>%
    filter(weight >= quantile(weight, prune_quantile))

  # Node outflow strength
  node_strength <- edges %>%
    group_by(name = sending_district) %>%
    summarise(outflow = sum(weight), .groups = "drop")

  # Degree (in + out) for node pruning
  node_degree <- data.frame(
    name = c(edges$sending_district, edges$receiving_district)
  ) %>%
    count(name, name = "degree")

  # Label threshold: top 10% by outflow
  label_threshold <- quantile(node_strength$outflow, 0.90)

  # Build node table
  nodes <- data.frame(name = VALID_FED_NAMES) %>%
    left_join(node_strength, by = "name") %>%
    mutate(outflow = replace_na(outflow, 0)) %>%
    mutate(outflow_scaled = outflow) %>%
    left_join(node_degree, by = "name") %>%
    mutate(degree = replace_na(degree, 0)) %>%
    left_join(fed_to_region, by = "name") %>%
    mutate(region = replace_na(region, "Other")) %>%
    filter(degree > quantile(degree, node_prune_quantile))

  # Re-filter edges to pruned node set
  active_nodes <- nodes$name
  edges <- edges %>%
    filter(
      sending_district %in% active_nodes,
      receiving_district %in% active_nodes
    )

  g <- graph_from_data_frame(
    d = edges,
    directed = TRUE,
    vertices = nodes
  )
  components <- igraph::components(g)
  largest <- which.max(components$csize)
  g <- igraph::induced_subgraph(
    g,
    vids = V(g)[components$membership == largest]
  )

  # ForceAtlas2 layout — matching the JS settings closely
  # niter: equivalent to JS iterations = 300; 500 gives a bit more stability
  coords <- layout_with_stress(g) # warm start
  layout_coords <- layout_with_sparse_stress(
    g,
    pivots = 50, # number of pivot nodes — higher = more accurate, slower
    weights = "weight"
  )

  region_colours <- c(
    "Maritimes" = "#E69F00",
    "QC" = "#56B4E9",
    "ON" = "#009E73",
    "Prairies" = "#D55E00",
    "BC" = "#CC79A7",
    "Other" = "#999999"
  )

  ggraph(g, layout = "manual", x = layout_coords[, 1], y = layout_coords[, 2]) +
    geom_edge_link(
      aes(alpha = weight),
      arrow = arrow(length = unit(2, "mm"), type = "closed"),
      end_cap = circle(3, "mm"),
      colour = "#AAAAAA"
    ) +
    geom_node_point(aes(size = outflow_scaled, colour = region)) +
    scale_colour_manual(values = region_colours, name = "Region") +
    scale_size_continuous(
      range = c(1, 12), # increase upper bound since large values are now bigger
      name = "Outflow"
    ) +
    geom_node_text(
      aes(label = ifelse(outflow > label_threshold, name, "")),
      size = 1.2,
      repel = TRUE
    ) +
    scale_edge_alpha_continuous(range = c(0.05, 0.9), name = weight_col) +
    theme_graph(base_family = "sans") +
    labs(title = paste("Inter-district donation flows:", weight_col))
}

# ---- Usage ----
p <- make_fed_graph(FED_graph_data, "donation_amount_on_both")
ggsave("fed_graph.pdf", p, width = 24, height = 24, units = "in")
