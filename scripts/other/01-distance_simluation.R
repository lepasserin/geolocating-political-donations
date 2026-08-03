# Purpose: Test hypothesis that the observed district-level out-of-district donation rates can be adequately explained by differences in spatial clustering.
# Author: Benedict Cummins-Mburu
# Last Updated: 11 July 2026
# Status: COMPLETE
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT
# Note:
#   - This analysis is separated from the main script because it takes too long to run.

# ----- Setup -------
set.seed(416)
library(tidyverse)
library(arrow)
library(sf)
localized_donations_data <- read_parquet(
  "data/analysis_data/donations_data.parquet"
) %>%
  filter(is_local)
DA_lookup_geo <- readRDS("data/analysis_data/DA_lookup.rds")
FED_lookup_geo <- readRDS("data/analysis_data/FED_lookup.rds")
PCCF_lookup <- read_parquet("data/analysis_data/PCCF_lookup.parquet")

FED_lookup <- FED_lookup_geo %>% st_drop_geometry()
DA_lookup <- DA_lookup_geo %>% st_drop_geometry()
fed_crs <- st_crs(FED_lookup_geo)
MAX_ATTEMPTS <- 12
N_REPLICATES <- 30
GEO_RESOLUTION <- 500 # higher = more coarse
N_BOOT <- 2000 # bootstrap resamples for the simulated 95% CI

FED_geo_simplified <- st_simplify(FED_lookup_geo, dTolerance = GEO_RESOLUTION)

message("[1/4] Setup complete: data loaded and FED boundaries simplified.")

# ----- Pre-Processing -----

# 1. Augment donations with their distance (`donation_distance`)

# 1.1 Setup efficient lookups
da_to_fed <- PCCF_lookup %>%
  distinct(DAUID, FEDUID) %>%
  distinct(DAUID, .keep_all = TRUE) %>%
  mutate(DAUID = as.character(DAUID))
da_centroids <- DA_lookup %>%
  transmute(
    DAUID = as.character(DAUID),
    donor_lat = as.numeric(centroid_lat),
    donor_lon = as.numeric(centroid_lon)
  )
fed_centroids <- FED_lookup %>%
  transmute(
    recipient_FEDUID = FEDUID,
    name = name,
    fed_lat = as.numeric(centroid_lat),
    fed_lon = as.numeric(centroid_lon)
  )

# 1.2 Calculation.
augmented_localized_data <- localized_donations_data %>%
  mutate(
    donor_DAUID = as.character(donor_dissemination_area)
  ) %>%
  left_join(da_to_fed, by = c("donor_DAUID" = "DAUID")) %>%
  rename(donor_FEDUID = FEDUID) %>%
  left_join(da_centroids, by = c("donor_DAUID" = "DAUID")) %>%
  left_join(fed_centroids, by = c("recipient_district" = "name")) %>%
  mutate(
    in_district = donor_FEDUID == recipient_FEDUID,
    dlat = (fed_lat - donor_lat) * pi / 180, # Vectorised haversine great-circle distance (in kilometres).
    dlon = (fed_lon - donor_lon) * pi / 180,
    hav = sin(dlat / 2)^2 +
      cos(donor_lat * pi / 180) * cos(fed_lat * pi / 180) * sin(dlon / 2)^2,
    donation_distance = if_else(
      in_district,
      0,
      6371 * 2 * atan2(sqrt(hav), sqrt(1 - hav))
    )
  ) %>%
  select(-c(dlat, dlon, hav))

message("[2/4] Distances calculated.")

# ----- Helpers -----

# 1. Geodesic destination point on a sphere (vectorised).
destination_point <- function(lon, lat, bearing_deg, distance_km, R = 6371) {
  phi1 <- lat * pi / 180
  lambda1 <- lon * pi / 180
  theta <- bearing_deg * pi / 180
  delta <- distance_km / R
  phi2 <- asin(sin(phi1) * cos(delta) + cos(phi1) * sin(delta) * cos(theta))
  lambda2 <- lambda1 +
    atan2(
      sin(theta) * sin(delta) * cos(phi1),
      cos(delta) - sin(phi1) * sin(phi2)
    )
  tibble(
    lon = ((lambda2 * 180 / pi + 540) %% 360) - 180,
    lat = phi2 * 180 / pi
  )
}

# 2. Given endpoint lon/lat (degrees), return the containing FED name (NA if none).
landing_fed_for_points <- function(lon, lat) {
  pts <- tibble(lon = lon, lat = lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(fed_crs)
  hits <- st_intersects(pts, FED_geo_simplified)
  idx <- map_int(hits, ~ if (length(.x) > 0L) .x[1] else NA_integer_)
  FED_geo_simplified$name[idx]
}

# 3. Run single simulation.
run_single_simulation <- function(data, decay_distances) {
  sim <- data %>%
    mutate(
      donation_distance_sim = sample(
        decay_distances,
        size = nrow(data),
        replace = TRUE
      ),
      landing_fed = NA_character_,
      resolved = donation_distance_sim < 1 |
        is.na(donation_distance_sim) |
        is.na(donor_lat_obs) |
        is.na(donor_lon_obs)
    )

  near_home <- sim$resolved &
    !is.na(sim$donation_distance_sim) &
    sim$donation_distance_sim < 1
  sim$landing_fed[near_home] <- sim$donor_district_obs[near_home]

  # Resample bearings until every (resolvable) donation lands inside some FED.
  for (attempt in seq_len(MAX_ATTEMPTS)) {
    pending <- which(!sim$resolved)
    if (length(pending) == 0L) {
      break
    }
    bearings <- runif(length(pending), 0, 360)
    ep <- destination_point(
      sim$donor_lon_obs[pending],
      sim$donor_lat_obs[pending],
      bearings,
      sim$donation_distance_sim[pending]
    )
    landed <- landing_fed_for_points(ep$lon, ep$lat)
    ok <- !is.na(landed)
    sim$landing_fed[pending[ok]] <- landed[ok]
    sim$resolved[pending[ok]] <- TRUE
  }

  sim %>%
    mutate(is_out_of_district_sim = landing_fed != donor_district_obs) %>%
    select(-resolved)
}

# 4. Bootstrapped 95% CI of the simulated null (amount-weighted).
bootstrap_ci <- function(x, n_boot = N_BOOT, probs = c(0.025, 0.975)) {
  boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  quantile(boot_means, probs = probs, names = FALSE)
}

# ------ Execution -------

# Run the full hypothesis test for a single localized entity.
run_hyp_test <- function(entity_name) {
  # get entity-specific decay distances
  entity_data <- augmented_localized_data %>%
    filter(political_entity == entity_name)
  decay_distances <- entity_data$donation_distance

  # Fixed simulation set, restricted to the columns the simulation needs.
  simulation_data <- entity_data %>%
    transmute(
      donor_lat_obs = donor_lat,
      donor_lon_obs = donor_lon,
      donor_district_obs = donor_district,
      is_out_of_district_obs = is_out_of_district,
      amount_obs = total_amount
    )

  # Null-model replicates: amount-weighted OOD proportion per FED per replicate.
  replicate_ood <- map_dfr(seq_len(N_REPLICATES), function(replicate_id) {
    run_single_simulation(simulation_data, decay_distances) %>%
      filter(!is.na(is_out_of_district_sim)) %>%
      group_by(donor_district_obs) %>%
      summarise(
        prop_ood_sim = sum(amount_obs * is_out_of_district_sim) /
          sum(amount_obs),
        .groups = "drop"
      ) %>%
      mutate(replicate_id = replicate_id)
  })

  # Observed OOD proportions per FED (fixed; independent of the simulation).
  observed_ood <- simulation_data %>%
    group_by(donor_district_obs) %>%
    summarise(
      n = n(),
      total_amount = sum(amount_obs),
      prop_ood_obs = sum(amount_obs * is_out_of_district_obs) / sum(amount_obs),
      .groups = "drop"
    )

  # Observed proportion alongside the bootstrapped 95% CI of the simulated null.
  observed_ood %>%
    left_join(
      replicate_ood %>%
        group_by(donor_district_obs) %>%
        summarise(
          mean_prop_ood_sim = mean(prop_ood_sim),
          ci = list(bootstrap_ci(prop_ood_sim)),
          .groups = "drop"
        ) %>%
        mutate(
          ci_lower_sim = map_dbl(ci, 1),
          ci_upper_sim = map_dbl(ci, 2)
        ) %>%
        select(-ci),
      by = "donor_district_obs"
    ) %>%
    mutate(political_entity = entity_name)
}

message("[3/4] Running simulations...")

distance_hyp_test_results <- map_dfr(
  c("Registered associations", "Candidates"),
  run_hyp_test
)

message("[4/4] Simulation Completed.")

# ------ Save ------
write_parquet(
  distance_hyp_test_results,
  "data/cached_data/distance_hyp_test_results.parquet"
)
write_parquet(
  augmented_localized_data,
  "data/cached_data/localized_donations_data_distances.parquet"
)

message("Parquets saved successfully --- END OF SCRIPT.")
