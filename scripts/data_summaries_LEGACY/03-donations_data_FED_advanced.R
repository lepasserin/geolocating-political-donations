# Purpose: Visualize `FED_donations_data` as an inter-district donation flow network.
# Author: Benedict Cummins-Mburu
# Last Updated: 12 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# --------- Setup ----------
set.seed(416)
library(tidyverse)
library(sf)
library(arrow)
library(igraph)
library(graphlayouts)
library(ggraph)
FED_donations_data <- read_parquet(
  "data/processed_data/FED_donations_data.parquet"
)
FED_shapefile <- readRDS("data/clean_data/clean_FED_shapefile.rds")

province_shapes <- FED_shapefile %>%
  st_make_valid() %>%
  group_by(PROVINCE) %>%
  summarise(.groups = "drop")

# Province centroids as plain x/y
province_centroids <- province_shapes %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(PROVINCE, x, y)

# ----- Constants -----

PROVINCE_REGION_MAP <- c(
  "Newfoundland and Labrador" = "Maritimes",
  "Prince Edward Island" = "Maritimes",
  "Nova Scotia" = "Maritimes",
  "New Brunswick" = "Maritimes",
  "Quebec" = "QC",
  "Ontario" = "ON",
  "Manitoba" = "Prairies",
  "Saskatchewan" = "Prairies",
  "Alberta" = "Prairies",
  "British Columbia" = "BC",
  "Yukon" = "Other",
  "Northwest Territories" = "Other",
  "Nunavut" = "Other"
)

REGION_COLOURS <- c(
  "Maritimes" = "#E69F00",
  "QC" = "#56B4E9",
  "ON" = "#009E73",
  "Prairies" = "#D55E00",
  "BC" = "#CC79A7",
  "Other" = "#999999"
)
VALID_FED_NAMES <- unique(FED_shapefile$FED)

fed_to_region <- FED_shapefile %>%
  st_drop_geometry() %>%
  mutate(region = PROVINCE_REGION_MAP[PROVINCE]) %>%
  select(name = FED, region) %>%
  distinct()

# ----- Figure Wrapper -----

# Figure Wrapper for Network Graph
make_fed_graph <- function(
  data,
  edge_col,
  directed = TRUE,
  flux_quantile = 0.05, # stage 1: drop bottom % of nodes by total flux
  prune_quantile = 0.80 # stage 2: keep only edges at/above this weight quantile
) {
  # Build edge list: drop zero-weight edges
  raw_edges <- data %>%
    select(sending_district, receiving_district, weight = all_of(edge_col)) %>%
    filter(weight > 0)

  # If undirected, collapse each unordered pair into a single summed-flow edge
  if (!directed) {
    raw_edges <- raw_edges %>%
      mutate(
        a = pmin(sending_district, receiving_district),
        b = pmax(sending_district, receiving_district)
      ) %>%
      group_by(sending_district = a, receiving_district = b) %>%
      summarise(weight = sum(weight), .groups = "drop")
  }

  edges <- raw_edges # keep everything — no component filtering

  # Helper: total flux (inflow + outflow) per node, from the current edge set
  node_flux <- function(e) {
    bind_rows(
      e %>% select(name = sending_district, weight),
      e %>% select(name = receiving_district, weight)
    ) %>%
      group_by(name) %>%
      summarise(flux = sum(weight), .groups = "drop")
  }

  # ---- Stage 1: drop bottom `flux_quantile` of NODES by total flux ----
  flux <- node_flux(edges)
  keep <- flux %>% filter(flux > quantile(flux, flux_quantile)) %>% pull(name)
  edges <- edges %>%
    filter(sending_district %in% keep, receiving_district %in% keep)

  # ---- Stage 2: prune weakest EDGES (keep top by weight) ----
  edges <- edges %>% filter(weight >= quantile(weight, prune_quantile))

  # Node inflow/outflow on the final edge set — for sizing, labels, direction
  out_str <- edges %>%
    group_by(name = sending_district) %>%
    summarise(out = sum(weight), .groups = "drop")
  in_str <- edges %>%
    group_by(name = receiving_district) %>%
    summarise(inn = sum(weight), .groups = "drop")

  # Build node table from surviving districts only
  nodes <- data.frame(
    name = unique(c(edges$sending_district, edges$receiving_district))
  ) %>%
    left_join(out_str, by = "name") %>%
    left_join(in_str, by = "name") %>%
    mutate(
      out = replace_na(out, 0),
      inn = replace_na(inn, 0),
      flux = out + inn,
      net = out - inn,
      direction = if_else(net > 0, "Net giver", "Net receiver")
    ) %>%
    left_join(fed_to_region, by = "name") %>%
    mutate(region = replace_na(region, "Other"))

  label_threshold <- quantile(nodes$flux, 0.90)

  g <- graph_from_data_frame(d = edges, directed = directed, vertices = nodes)

  # Stage 3: drop any vertices that ended up with no edges
  g <- igraph::delete_vertices(g, igraph::V(g)[igraph::degree(g) == 0])

  # Stress layout — handles disconnected components (unlike sparse_stress)
  layout_coords <- layout_with_stress(g)

  # Directed edges get arrows; undirected edges don't
  edge_layer <- if (directed) {
    geom_edge_link(
      aes(alpha = weight),
      arrow = arrow(length = unit(1, "mm"), type = "closed"),
      end_cap = circle(1.5, "mm"),
      colour = "#AAAAAA"
    )
  } else {
    geom_edge_link(aes(alpha = weight), colour = "#AAAAAA")
  }

  ggraph(g, layout = "manual", x = layout_coords[, 1], y = layout_coords[, 2]) +
    edge_layer +
    geom_node_point(
      aes(size = flux, fill = region, colour = direction),
      shape = 21,
      stroke = 0.6
    ) +
    scale_fill_manual(values = REGION_COLOURS, name = "Region") +
    scale_colour_manual(
      values = c("Net giver" = "black", "Net receiver" = "transparent"),
      name = "Direction"
    ) +
    scale_size_continuous(range = c(0.5, 5), name = "Total flux") +
    geom_node_text(
      aes(label = ifelse(flux > label_threshold, name, "")),
      size = 2,
      fontface = "bold",
      repel = TRUE
    ) +
    scale_edge_alpha_continuous(range = c(0.05, 0.9), name = edge_col) +
    theme_graph(base_family = "sans") +
    theme(
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.size = unit(3, "mm"),
      legend.spacing.y = unit(1, "mm"),
      legend.box.spacing = unit(2, "mm")
    )
}

# Wrapper for Interdistrict Flows Map
make_province_map <- function(
  data,
  edge_col,
  directed = TRUE,
  curvature = 0.2,
  n_seg = 50
) {
  # 1. Interprovincial directed flows
  ip <- data %>%
    filter(sending_province != receiving_province) %>%
    group_by(sending_province, receiving_province) %>%
    summarise(
      weight = sum(.data[[edge_col]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(weight > 0)

  # 2. Node net flow + total strength (always from directed flows)
  out_flow <- ip %>%
    group_by(PROVINCE = sending_province) %>%
    summarise(out = sum(weight), .groups = "drop")
  in_flow <- ip %>%
    group_by(PROVINCE = receiving_province) %>%
    summarise(inn = sum(weight), .groups = "drop")

  nodes <- province_centroids %>%
    left_join(out_flow, by = "PROVINCE") %>%
    left_join(in_flow, by = "PROVINCE") %>%
    mutate(
      out = replace_na(out, 0),
      inn = replace_na(inn, 0),
      strength = out + inn,
      io_ratio = out / inn, # > 1 = net receiver, < 1 = net giver
      io_log = log(io_ratio) # symmetric around 0 (balanced)
    )

  # 3. Edge geometry
  if (directed) {
    # Quadratic Bézier per directed edge; control point offset perpendicular to
    # travel direction, so A->B and B->A curve to opposite sides.
    edge_points <- ip %>%
      left_join(
        province_centroids %>% rename(x0 = x, y0 = y),
        by = c("sending_province" = "PROVINCE")
      ) %>%
      left_join(
        province_centroids %>% rename(x1 = x, y1 = y),
        by = c("receiving_province" = "PROVINCE")
      ) %>%
      mutate(edge_id = row_number()) %>%
      rowwise() %>%
      mutate(
        pts = list({
          dx <- x1 - x0
          dy <- y1 - y0
          L <- sqrt(dx^2 + dy^2)
          px <- -dy / L
          py <- dx / L # left-of-direction normal
          cx <- (x0 + x1) / 2 + curvature * L * px # control point
          cy <- (y0 + y1) / 2 + curvature * L * py
          t <- seq(0, 1, length.out = n_seg)
          tibble(
            t = t,
            x = (1 - t)^2 * x0 + 2 * (1 - t) * t * cx + t^2 * x1,
            y = (1 - t)^2 * y0 + 2 * (1 - t) * t * cy + t^2 * y1
          )
        })
      ) %>%
      ungroup() %>%
      select(edge_id, weight, pts) %>%
      unnest(pts)
    edge_layer <- geom_path(
      data = edge_points,
      aes(
        x,
        y,
        group = edge_id,
        colour = t,
        linewidth = weight,
        alpha = weight
      ),
      lineend = "butt"
    )
  } else {
    # Undirected: collapse pairs, straight segments
    prov_edges <- ip %>%
      mutate(
        a = pmin(sending_province, receiving_province),
        b = pmax(sending_province, receiving_province)
      ) %>%
      group_by(sending_province = a, receiving_province = b) %>%
      summarise(weight = sum(weight), .groups = "drop") %>%
      left_join(
        province_centroids %>% rename(x0 = x, y0 = y),
        by = c("sending_province" = "PROVINCE")
      ) %>%
      left_join(
        province_centroids %>% rename(x1 = x, y1 = y),
        by = c("receiving_province" = "PROVINCE")
      )

    edge_layer <- geom_segment(
      data = prov_edges,
      aes(
        x = x0,
        y = y0,
        xend = x1,
        yend = y1,
        linewidth = weight,
        alpha = weight
      ),
      colour = "grey50"
    )
  }

  # 4. Assemble. Edges use the `colour` scale; nodes use `fill` (shape 21) so the
  #    two gradients don't collide.
  p <- ggplot() +
    geom_sf(data = province_shapes, fill = "grey95", colour = "white") +
    edge_layer +
    geom_point(
      data = nodes,
      aes(x, y, fill = io_log, size = strength),
      shape = 21,
      colour = "black",
      stroke = 0.3
    ) +
    geom_text(
      data = nodes,
      aes(x, y, label = PROVINCE),
      size = 2.5,
      vjust = -1.4
    ) +
    scale_linewidth_continuous(range = c(0.2, 2.5), name = edge_col) +
    scale_alpha_continuous(range = c(0.2, 0.9), guide = "none") +
    scale_size_continuous(
      range = c(2, 12),
      name = "Total interprovincial flow"
    ) +
    scale_fill_gradient2(
      low = "#D55E00",
      mid = "grey90",
      high = "#56B4E9",
      midpoint = 0,
      name = "Outflow : Inflow",
      breaks = log(c(0.5, 1, 2)),
      labels = c("0.5×", "1×", "2×")
    ) +
    theme_void()

  if (directed) {
    p <- p +
      scale_colour_gradient(
        low = "#56B4E9",
        high = "#D55E00",
        name = "Flow direction",
        breaks = c(0, 1),
        labels = c("Giving", "Receiving")
      )
  }
  p
}

# --------- Figures ---------

Figure_spatial_1_v1 <- make_fed_graph(
  FED_donations_data,
  edge_col = "donation_amount_EDA_all",
  directed = TRUE,
  flux_quantile = 0.8,
  prune_quantile = 0.4
)

Figure_spatial_1_v2 <- make_fed_graph(
  FED_donations_data,
  edge_col = "donation_amount_cand_on",
  directed = TRUE,
  flux_quantile = 0.85,
  prune_quantile = 0.15
)

Figure_spatial_2_v1 <- make_province_map(
  FED_donations_data,
  edge_col = "donation_amount_EDA_all",
  directed = TRUE
)

Figure_spatial_2_v2 <- make_province_map(
  FED_donations_data,
  edge_col = "donation_amount_cand_on",
  directed = TRUE
)


# ------ Save to File -------
ggsave(
  "other/figures/Figure_spatial_1_v1.pdf",
  Figure_spatial_1_v1,
  width = 7.5,
  height = 4.5,
  units = "in"
)
ggsave(
  "other/figures/Figure_spatial_1_v2.pdf",
  Figure_spatial_1_v2,
  width = 7.5,
  height = 4.5,
  units = "in"
)
ggsave(
  "other/figures/Figure_spatial_2_v1.pdf",
  Figure_spatial_2_v1,
  width = 12,
  height = 10,
  units = "in"
)
ggsave(
  "other/figures/Figure_spatial_2_v2.pdf",
  Figure_spatial_2_v2,
  width = 12,
  height = 10,
  units = "in"
)
