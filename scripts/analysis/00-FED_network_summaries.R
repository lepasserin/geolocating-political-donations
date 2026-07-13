# Purpose: Create Summaries of the Out-of-district funding Network.
# Author: Benedict Cummins-Mburu
# Last Updated: 30 Jun 2026
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Note:
#    - These summaries are separated from the main script because they take too long to run.

# ----- Setup -------
library(tidyverse)
library(arrow)
library(sf)
localized_donations_data <- read_parquet(
  "data/processed_data/donations_data.parquet"
) %>%
  filter(is_local)
FED_lookup_geo <- readRDS("data/clean_data/FED_lookup.rds")

# ----- Constants -------

PR_LOOKUP <- data.frame(
  PRUID = c(
    "10",
    "11",
    "12",
    "13",
    "24",
    "35",
    "46",
    "47",
    "48",
    "59",
    "60",
    "61",
    "62"
  ),
  province = c(
    "Newfoundland and Labrador",
    "Prince Edward Island",
    "Nova Scotia",
    "New Brunswick",
    "Quebec",
    "Ontario",
    "Manitoba",
    "Saskatchewan",
    "Alberta",
    "British Columbia",
    "Yukon",
    "Northwest Territories",
    "Nunavut"
  )
)

# ----- Helpers -------

# Used to persist the slow geometry steps.
cache_rds <- function(path, expr) {
  if (file.exists(path)) {
    return(readRDS(path))
  }
  obj <- expr
  saveRDS(obj, path)
  obj
}

# ----- Augment Localized Data -------

# 0. Get FED-to-province lookup by name.
FED_to_PR <- FED_lookup_geo %>%
  st_drop_geometry() %>%
  select(name, PRUID) %>%
  left_join(PR_LOOKUP, by = c("PRUID")) %>%
  select(-PRUID)

# 1. Flag if a donation was sent out-of-province.
augmented_localized_data <- localized_donations_data %>%
  left_join(FED_to_PR, by = c("recipient_district" = "name")) %>%
  rename(recipient_province = province) %>%
  mutate(is_out_of_province = recipient_province != donor_province)

# 2. Flag if a donation was sent to a neighbouring district.
neighbour_pairs <- cache_rds("data/other/FED_neighbour_edges.rds", {
  nb <- st_touches(FED_lookup_geo)
  tibble(
    district = FED_lookup_geo$name[rep(seq_along(nb), lengths(nb))],
    neighbour = FED_lookup_geo$name[unlist(nb)],
    are_neighbors = TRUE
  )
})
augmented_localized_data <- augmented_localized_data %>%
  left_join(
    neighbour_pairs,
    by = c("donor_district" = "district", "recipient_district" = "neighbour")
  ) %>%
  mutate(are_neighbors = replace_na(are_neighbors, FALSE))

# ----- Create Wrapper for Provincial Funding Flow Map -------

# 1. Create Base Provincial Network
province_network <- augmented_localized_data %>%
  group_by(
    sending_province = donor_province,
    receiving_province = recipient_province
  ) %>%
  summarise(
    n = n(),
    n_candidates = sum(political_entity == "Candidates"),
    n_EDAs = sum(political_entity == "Registered associations"),
    amount = sum(total_amount),
    amount_candidates = sum(total_amount[political_entity == "Candidates"]),
    amount_EDAs = sum(
      total_amount[political_entity == "Registered associations"]
    ),
    .groups = "drop"
  )

# 2. Pre-compute province geometries by dissolving the FED polygons.
province_shapes <- cache_rds("data/other/province_shapes.rds", {
  FED_lookup_geo %>%
    left_join(PR_LOOKUP, by = "PRUID") %>%
    group_by(province) %>%
    summarise(.groups = "drop")
})

province_centroids <- province_shapes %>%
  st_centroid() %>%
  mutate(x = st_coordinates(.)[, 1], y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(province, x, y)

# 3. Wrapper (for a chosen edge measure from `province_network`)
interprovincial_network <- function(
  edge,
  edge_label = edge, # nicer legend name for the edge measure
  network = province_network,
  directed = TRUE,
  top_n = 20,
  curvature = 0.2,
  n_seg = 50
) {
  # Interprovincial flows for the chosen edge measure
  ip <- network %>%
    filter(sending_province != receiving_province) %>%
    transmute(sending_province, receiving_province, weight = .data[[edge]]) %>%
    filter(weight > 0)

  # Node net flow + total strength, from the FULL interprovincial set so node
  # size/balance reflect all flows, not only the top_n edges that get drawn.
  out_flow <- ip %>%
    group_by(province = sending_province) %>%
    summarise(out = sum(weight), .groups = "drop")
  in_flow <- ip %>%
    group_by(province = receiving_province) %>%
    summarise(inn = sum(weight), .groups = "drop")

  nodes <- province_centroids %>%
    left_join(out_flow, by = "province") %>%
    left_join(in_flow, by = "province") %>%
    mutate(
      out = replace_na(out, 0),
      inn = replace_na(inn, 0),
      strength = out + inn,
      io_ratio = out / inn, # > 1 = net giver, < 1 = net receiver
      io_log = log(io_ratio) # symmetric around 0 (balanced)
    )

  if (directed) {
    # Keep the strongest edges only
    edges <- ip %>% slice_max(weight, n = top_n, with_ties = FALSE)

    # Quadratic Bézier per directed edge; control point offset perpendicular to
    # travel direction, so A->B and B->A curve to opposite sides.
    edge_points <- edges %>%
      left_join(
        province_centroids %>% rename(x0 = x, y0 = y),
        by = c("sending_province" = "province")
      ) %>%
      left_join(
        province_centroids %>% rename(x1 = x, y1 = y),
        by = c("receiving_province" = "province")
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
        linewidth = weight
      ),
      alpha = 0.8,
      lineend = "butt"
    )
  } else {
    # Undirected: collapse pairs, keep strongest, straight segments
    prov_edges <- ip %>%
      mutate(
        a = pmin(sending_province, receiving_province),
        b = pmax(sending_province, receiving_province)
      ) %>%
      group_by(sending_province = a, receiving_province = b) %>%
      summarise(weight = sum(weight), .groups = "drop") %>%
      slice_max(weight, n = top_n, with_ties = FALSE) %>%
      left_join(
        province_centroids %>% rename(x0 = x, y0 = y),
        by = c("sending_province" = "province")
      ) %>%
      left_join(
        province_centroids %>% rename(x1 = x, y1 = y),
        by = c("receiving_province" = "province")
      )
    edge_layer <- geom_segment(
      data = prov_edges,
      aes(
        x = x0,
        y = y0,
        xend = x1,
        yend = y1,
        linewidth = weight
      ),
      alpha = 0.8,
      colour = "grey50"
    )
  }

  # Assemble. Edges use the `colour` scale; nodes use `fill` (shape 21) so the
  # two gradients don't collide.
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
      aes(x, y, label = province),
      size = 2.5,
      vjust = -1.4
    ) +
    # Anchor at 0 so linewidth is proportional to weight (not rescaled)
    scale_linewidth_continuous(
      range = c(0, 2.5),
      limits = c(0, NA),
      labels = scales::label_number(scale = 1e-3),
      name = edge_label
    ) +
    scale_size_continuous(
      range = c(2, 12),
      name = "Total flux"
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
        guide = "none"
      )
  }
  p
}


# ----- Figures -------

Figure_province_flow_candidates <- interprovincial_network(
  "amount_candidates",
  edge_label = "Amount ($K)"
)
Figure_province_flow_EDAs <- interprovincial_network(
  "amount_EDAs",
  edge_label = "Amount ($K)"
)


# ----- Save -------
write_parquet(
  augmented_localized_data,
  "data/other/localized_donations_data_neighbours.parquet"
)
ggsave(
  "other/big_figures/Figure_province_flow_candidates.png",
  Figure_province_flow_candidates,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)
ggsave(
  "other/big_figures/Figure_province_flow_EDAs.png",
  Figure_province_flow_EDAs,
  width = 12,
  height = 10,
  units = "in",
  dpi = 300
)
