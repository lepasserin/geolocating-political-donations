# Purpose: Test hypothesis that the observed district-level out-of-district donation rates can be adequately explained by differences in spatial clustering.
# Author: Benedict Cummins-Mburu
# Last Updated: 2 July 2026
# Status: PILOT
# Contact: b.cumminsmburu@utoronto.ca
# License: MIT

# Simulation Design:
# - Assign a random donation distance to each donation.
# - If the distance is < 1km, immediately declare in-district. Otherwise, continue.
# - Assign a random direction for that donation.
# - Determine if it landed within Canada. If it didn't choose a different direction. Otherwise, continue.
# - Determine which FED it landed in.
# - Declare in-district vs. out-of-district.

# For each FED, replicate simulations are carried out to get an idea of the sample distribution.
# Finally, we can statistically determine if a FED is significantly outside their null-expected OOD range.

# Implementation Decisions:
# - DETERMINING OBSERVED DISTANCES: Distance between donor DA and recipient FED centroids.
# - DISTANCE SAMPLING: Sampling distance by resampling the observed distribution of donation distances.
# - FED RESOLUTION: actually using the shapefile to decide if within a FED.

# ----- Setup -------
set.seed(417)
library(tidyverse)
library(arrow)
library(sf)
donations_data <- read_parquet("data/processed_data/donations_data.parquet")
DA_lookup_geo <- readRDS("data/clean_data/DA_lookup.rds")
FED_lookup_geo <- readRDS("data/clean_data/FED_lookup.rds")
PCCF_lookup <- read_parquet("data/clean_data/PCCF_lookup.parquet")

FED_lookup <- FED_lookup_geo %>% st_drop_geometry()
DA_lookup <- DA_lookup_geo %>% st_drop_geometry()
fed_crs <- st_crs(FED_lookup_geo)
MAX_ATTEMPTS <- 12
N_REPLICATES <- 30
GEO_RESOLUTION <- 500 # higher = more coarse
FED_SAMPLE_SIZE <- 10
N_BOOT <- 2000 # bootstrap resamples for the simulated 95% CI

FED_geo_simplified <- st_simplify(FED_lookup_geo, dTolerance = GEO_RESOLUTION)

message("[1/4] Setup complete: data loaded and FED boundaries simplified.")

# ----- Configuration -----

# 0. Filter for just election periods (FOR PILOT)
elections_donations_data <- donations_data %>%
  filter(political_entity %in% c("Registered associations", "Candidates")) %>%
  filter(is_general_election_period)

# 1. Augment donations with their distance (called `donation_distance`)

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

# 1.2 Calculate
augmented_elections_donations_data <- elections_donations_data %>%
  mutate(
    donor_DAUID = as.character(donor_dissemination_area)
  ) %>%
  # donor DA -> its FEDUID + centroid
  left_join(da_to_fed, by = c("donor_DAUID" = "DAUID")) %>%
  rename(donor_FEDUID = FEDUID) %>%
  left_join(da_centroids, by = c("donor_DAUID" = "DAUID")) %>%
  # recipient FED name -> its FEDUID + centroid
  left_join(fed_centroids, by = c("recipient_district" = "name")) %>%
  mutate(
    in_district = donor_FEDUID == recipient_FEDUID,
    # Vectorised haversine great-circle distance in kilometres.
    dlat = (fed_lat - donor_lat) * pi / 180,
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

# 2. Select Distance-Decay Distribution (ECDF right now)
observed_distances <- augmented_elections_donations_data$donation_distance
sample_decay_distribution <- function(n_samples) {
  sample(observed_distances, size = n_samples, replace = TRUE)
}

# 3. Define simulation subset and restrict to only relevant columns.
sampled_feds <- FED_lookup %>%
  slice_sample(n = FED_SAMPLE_SIZE) %>%
  pull(name)
simulation_subset <- augmented_elections_donations_data %>%
  filter(donor_district %in% sampled_feds) %>%
  rename(
    donor_lat_obs = donor_lat,
    donor_lon_obs = donor_lon,
    amount_obs = total_amount,
    donor_district_obs = donor_district,
    is_out_of_district_obs = is_out_of_district
  ) %>%
  select(
    donor_lat_obs,
    donor_lon_obs,
    donor_district_obs,
    is_out_of_district_obs,
    amount_obs
  )

message(sprintf(
  "[2/4] Simulation subset: %d donations across %d FEDs.",
  nrow(simulation_subset),
  n_distinct(simulation_subset$donor_district_obs)
))

# 4. Create Single Simulation Wrapper

# 4.1 Geodesic destination point on a sphere (vectorised).
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

# 4.2 Given endpoint lon/lat (degrees), return the containing FED name (NA if none).
landing_fed_for_points <- function(lon, lat) {
  pts <- tibble(lon = lon, lat = lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(fed_crs)
  hits <- st_intersects(pts, FED_geo_simplified)
  idx <- map_int(hits, ~ if (length(.x) > 0L) .x[1] else NA_integer_)
  FED_geo_simplified$name[idx]
}

# 4.3 Replicate function
run_single_simulation <- function(subset) {
  sim <- subset %>%
    mutate(
      donation_distance_sim = sample_decay_distribution(nrow(subset)),
      landing_fed = NA_character_,
      # distance < 1 km stays put; NA inputs can never resolve, so mark them done too.
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

# ------ Execution -------

# Run the null model over many replicates on the SAME fixed subset, collecting
# the amount-weighted OOD proportion per FED for each replicate.
n_replicates <- N_REPLICATES

message(sprintf("[3/4] Running %d replicate simulations...", n_replicates))

replicate_ood <- map_dfr(seq_len(n_replicates), function(replicate_id) {
  run_single_simulation(simulation_subset) %>%
    filter(!is.na(is_out_of_district_sim)) %>%
    group_by(donor_district_obs) %>%
    summarise(
      prop_ood_sim = sum(amount_obs * is_out_of_district_sim) / sum(amount_obs),
      .groups = "drop"
    ) %>%
    mutate(replicate_id = replicate_id)
})


# ------ Analysis -------

# Observed OOD proportion per FED (fixed; independent of the simulation).
observed_ood <- simulation_subset %>%
  group_by(donor_district_obs) %>%
  summarise(
    n = n(),
    total_amount = sum(amount_obs),
    prop_ood_obs = sum(amount_obs * is_out_of_district_obs) / sum(amount_obs),
    .groups = "drop"
  )

# Simulated OOD proportions per FED, one column per replicate (raw values).
simulated_ood <- replicate_ood %>%
  pivot_wider(
    names_from = replicate_id,
    values_from = prop_ood_sim,
    names_prefix = "prop_ood_sim_"
  )

ood_comparison <- observed_ood %>%
  left_join(simulated_ood, by = "donor_district_obs")

# Observed OOD proportion alongside a bootstrapped 95% CI of the simulated null.
bootstrap_ci <- function(x, n_boot = N_BOOT, probs = c(0.025, 0.975)) {
  boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  quantile(boot_means, probs = probs, names = FALSE)
}

ood_observed_vs_ci <- observed_ood %>%
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
  )

message("[4/4] Done: `ood_comparison` and `ood_observed_vs_ci` ready.")

# write_csv(ood_observed_vs_ci, "show_rohan.csv")
