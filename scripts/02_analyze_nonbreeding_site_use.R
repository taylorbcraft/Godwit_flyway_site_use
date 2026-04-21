# =============================================================================
# Godwit nonbreeding site use: peak 2-week use windows and proportional site use
#
# Purpose
#   Summarise how tracked godwits use major nonbreeding and stopover sites
#   across the flyway. The script identifies when sites are most heavily used
#   and what proportion of tracked birds use each site during each migratory
#   cycle.
#
#   Two main metrics are produced:
#     1) the peak 14-day period of use for each site in each migratory cycle
#     2) the proportion of tracked birds that used each site in each cycle,
#        and the average proportion across cycles
#
# General workflow
#   1) read and clean tracking data
#      - load location data from the tracking dataset
#      - keep only stationary fixes (birds not actively migrating)
#      - remove individuals that show little latitudinal movement
#        to exclude non-migratory birds
#      - restrict the dataset to the nonbreeding region
#      - limit to a small number of fixes per bird per day to reduce
#        sampling bias and serial correlation
#
#   2) identify sites used by birds
#      - locations are grouped into spatial clusters representing
#        commonly used nonbreeding or stopover areas
#      - each cluster is treated as a “site”
#      - sites are assigned human-readable names by matching the
#        cluster centre to the nearest known site location
#
#   3) define migratory cycles
#      - cycles are anchored to 1 June so that each cycle represents
#        one full nonbreeding season
#      - dates are converted into a day index relative to the start
#        of each cycle to allow seasonal comparisons
#
#   4) determine daily site use
#      - a bird is considered to use a site on a given day if at least
#        one location fix occurs at that site on that date
#      - this produces a dataset of unique bird × site × date records
#
#   5) calculate peak site use
#      - for each site and cycle, count the number of individual birds
#        present on each day
#      - calculate rolling 14-day totals of these daily counts
#      - identify the 14-day window with the highest total use
#        (the peak use period for that site in that cycle)
#
#   6) calculate proportional site use
#      - for each cycle, determine how many tracked birds used each site
#      - divide this by the total number of tracked birds in that cycle
#      - summarise average proportional use across cycles
#
# Outputs
#   The script produces tables and spatial layers describing:
#     - site locations
#     - daily presence of birds at each site
#     - peak 14-day use windows for each site and cycle
#     - proportional use of each site by tracked birds
#
# Interpretation notes
#   Peak windows indicate when sites experience the greatest concentration
#   of tracked birds during the nonbreeding season. Proportional use reflects
#   the share of tracked birds that visited a site in a given cycle and is
#   therefore conditional on the number of tagged birds tracked that year.
# =============================================================================


# packages
library(dplyr)
library(lubridate)
library(dbscan)
library(sf)
library(tidyr)
library(tibble)
library(slider)

# -------------------------------
# 1) read and clean tracking data
# -------------------------------
location_data_raw <- read.csv("data/all_locations.csv", stringsAsFactors = FALSE)

clean_locations <- location_data_raw %>%
  mutate(
    timestamp = {
      parsed_datetime <- ymd_hms(timestamp, tz = "UTC", quiet = TRUE)
      parsed_date <- as.POSIXct(ymd(timestamp, quiet = TRUE), tz = "UTC")
      coalesce(parsed_datetime, parsed_date)
    },
    date = as.Date(timestamp),
    location_lat = Y,
    location_long = X,
    local_identifier = individual_local_identifier
  ) %>%
  filter(study_site != "PL") %>%
  filter(is.na(ground_speed) | ground_speed == 0) %>%
  filter(
    timestamp >= ymd("2013-06-01"),
    timestamp <  ymd("2025-06-01")
  ) %>%
  group_by(local_identifier) %>%
  mutate(lat_range = max(location_lat, na.rm = TRUE) - min(location_lat, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(lat_range >= 5) %>%
  select(-lat_range) %>%
  # subset to nonbreeding region
  filter(location_lat < 42) %>%
  # thin to max 4 fixes per individual per day
  group_by(local_identifier, date) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  slice_head(n = 4) %>%
  ungroup() %>%
  #select(local_identifier, location_long, location_lat, timestamp, date) %>%
  filter(!is.na(location_lat), !is.na(location_long))

# ---------------------------------------
# 2) assign locations to "sites" (dbscan)
# ---------------------------------------
site_location_fixes <- clean_locations %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)

coords <- st_coordinates(site_location_fixes) %>% as.matrix()

grid_m <- 250
snapped_coords <- tibble(
  x = round(coords[, 1] / grid_m) * grid_m,
  y = round(coords[, 2] / grid_m) * grid_m
)

unique_snapped_coords <- snapped_coords %>%
  count(x, y, name = "n_pts")

site_clusters <- dbscan(
  unique_snapped_coords %>% select(x, y) %>% as.matrix(),
  eps = 30000,
  minPts = 1500
)

unique_snapped_coords <- unique_snapped_coords %>%
  mutate(site_id = site_clusters$cluster)

site_location_fixes <- site_location_fixes %>%
  mutate(
    site_id = unique_snapped_coords$site_id[
      match(
        paste(snapped_coords$x, snapped_coords$y),
        paste(unique_snapped_coords$x, unique_snapped_coords$y)
      )
    ]
  ) %>%
  filter(!is.na(site_id), site_id != 0)

# ---------------------------------------
# 2b) assign site names
#     using nearest reference point
# ---------------------------------------
site_reference_points <- tribble(
  ~site_name,                        ~ref_lat, ~ref_long,
  "Ebro Delta",                        40.7,     0.777,
  "Extremadura",                       39.1,    -5.93,
  "Tagus Estuary",                     38.9,    -8.94,
  "Doñana National Park",              37.1,    -6.24,
  "Senegal River Delta",               16.4,   -16.2,
  "Saloum Delta",                      14.1,   -16.6,
  "Gambia River",                      13.5,   -15.8,
  "Casamance/Coastal Guinea-Bissau",   12.6,   -15.9,
  "Lower Senegal River (West)",        16.5,   -14.4,
  "Lower Senegal River (East)",        16.2,   -13.2,
  "Inner Niger Delta (East)",          15.3,    -4.52,
  "Inner Niger Delta (West)",          16.5,    -3.22
) %>%
  distinct(site_name, ref_lat, ref_long) %>%
  st_as_sf(coords = c("ref_long", "ref_lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)

# one point per dbscan site (median fix coordinates)
site_locations <- site_location_fixes %>%
  st_transform(4326) %>%
  st_drop_geometry() %>%
  group_by(site_id) %>%
  summarise(
    longitude = median(location_long, na.rm = TRUE),
    latitude  = median(location_lat, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(3857) %>%
  # nearest reference-name join
  st_join(site_reference_points %>% select(site_name), join = st_nearest_feature) %>%
  mutate(site_name = if_else(is.na(site_name), paste0("site_", site_id), site_name)) %>%
  # optional distance diagnostic (km) in one line
  mutate(
    match_dist_km = as.numeric(st_distance(geometry, st_geometry(site_reference_points)[st_nearest_feature(., site_reference_points)], by_element = TRUE)) / 1000
  ) %>%
  st_transform(4326) %>%
  group_by(site_name) %>%
  mutate(
    site_label = if (n() > 1L) paste0(site_name, " (site ", site_id, ")") else site_name
  ) %>%
  ungroup()

# attach names back to fixes (single join)
site_location_fixes <- site_location_fixes %>%
  left_join(
    site_locations %>% st_drop_geometry() %>% select(site_id, site_name, site_label),
    by = "site_id"
  )

# quick view
print(site_locations, n = 100)

if (interactive() && requireNamespace("mapview", quietly = TRUE)) {
  mapview::mapview(site_locations)
}

# ------------------------------------------------------------
# 3) define migratory cycle label (nonbreeding year)
# ------------------------------------------------------------
# note: keep the trough calculation for reference, but use a fixed anchor: june 1
n_individuals_year <- clean_locations %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(n_tracked = n_distinct(local_identifier), .groups = "drop")

daily_occ <- clean_locations %>%
  mutate(
    year = year(date),
    doy = yday(date)
  ) %>%
  group_by(year, doy) %>%
  summarise(n_present = n_distinct(local_identifier), .groups = "drop") %>%
  left_join(n_individuals_year, by = "year") %>%
  mutate(prop_present = n_present / n_tracked)

occ_by_doy <- daily_occ %>%
  group_by(doy) %>%
  summarise(mean_prop_present = mean(prop_present, na.rm = TRUE), .groups = "drop") %>%
  arrange(doy) %>%
  mutate(roll_14 = slide_dbl(mean_prop_present, sum, .before = 13, .complete = TRUE))

anchor_month <- 6L
anchor_day <- 1L

add_nb_year <- function(x, date_col = date) {
  d <- dplyr::pull(x, {{ date_col }})
  nb_year <- if_else(
    month(d) > anchor_month | (month(d) == anchor_month & day(d) >= anchor_day),
    year(d),
    year(d) - 1L
  )

  x %>%
    mutate(
      nb_year = nb_year,
      nb_year_start = as.Date(sprintf("%d-%02d-%02d", nb_year, anchor_month, anchor_day))
    )
}

# ---------------------------------------------
# 4) reduce to unique presence-days per site/cycle
# ---------------------------------------------
site_presence_days <- site_location_fixes %>%
  add_nb_year(date) %>%
  st_drop_geometry() %>%
  distinct(local_identifier, site_id, site_name, site_label, nb_year, nb_year_start, date, .keep_all = FALSE) %>%
  mutate(day_in_year = as.integer(date - nb_year_start) + 1L) %>%
  filter(day_in_year >= 1L, day_in_year <= 366L)

# -----------------------------
# helpers for 14-day windows
# -----------------------------
wrap_day <- function(x) ((x - 1L) %% 366L) + 1L

fmt_mmdd <- function(day_in_year) {
  ref_start <- as.Date(sprintf("2001-%02d-%02d", anchor_month, anchor_day))
  format(ref_start + day_in_year - 1L, "%b %d")
}

# ------------------------------------------------------------
# 5) peak use window per site per cycle (nb_year)
# ------------------------------------------------------------
# for each site x nb_year x day_in_year:
#   n_users(day) = number of distinct individuals present that day at that site
# then rolling 14-day sum over n_users(day), pick max window
site_peak_windows_by_year <- site_presence_days %>%
  group_by(site_id, site_name, site_label, nb_year, day_in_year) %>%
  summarise(n_users = n_distinct(local_identifier), .groups = "drop") %>%
  group_by(site_id, site_name, site_label, nb_year) %>%
  complete(day_in_year = 1:366, fill = list(n_users = 0)) %>%
  arrange(day_in_year) %>%
  mutate(roll_14 = slide_dbl(n_users, sum, .before = 13, .complete = TRUE)) %>%
  slice_max(roll_14, n = 1, with_ties = FALSE) %>%
  transmute(
    site_id, site_name, site_label, nb_year,
    peak_end_day = day_in_year,
    peak_start_day = wrap_day(day_in_year - 13L),
    peak_n_users_14d = roll_14,
    peak_window = paste0(fmt_mmdd(wrap_day(day_in_year - 13L)), " - ", fmt_mmdd(day_in_year))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 6) proportional use per site per cycle, then average over years
# ------------------------------------------------------------
tracked_birds_by_year <- site_presence_days %>%
  distinct(nb_year, local_identifier) %>%
  count(nb_year, name = "n_total")

site_use_by_year <- site_presence_days %>%
  distinct(site_id, site_name, site_label, nb_year, local_identifier) %>%
  count(site_id, site_name, site_label, nb_year, name = "n_site") %>%
  left_join(tracked_birds_by_year, by = "nb_year") %>%
  mutate(prop_site = n_site / n_total)

site_use_summary <- site_use_by_year %>%
  group_by(site_id, site_name, site_label) %>%
  summarise(
    prop_mean = mean(prop_site, na.rm = TRUE),
    prop_median = median(prop_site, na.rm = TRUE),
    n_years = n_distinct(nb_year),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 7) single per-site peak window summary (no n_cycles)
# ------------------------------------------------------------
site_peak_window_summary <- site_peak_windows_by_year %>%
  group_by(site_id, site_name, site_label, peak_window) %>%
  summarise(
    mean_peak_n_users_14d = mean(peak_n_users_14d, na.rm = TRUE),
    n_occurrences = n(),
    .groups = "drop"
  ) %>%
  group_by(site_id, site_name, site_label) %>%
  arrange(desc(n_occurrences), desc(mean_peak_n_users_14d), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(site_id, site_name, site_label, peak_window, mean_peak_n_users_14d)

# ------------------------------------------------------------
# 8) final per-site table
# ------------------------------------------------------------
site_summary_points <- site_use_summary %>%
  left_join(site_peak_window_summary, by = c("site_id", "site_name", "site_label")) %>%
  select(
    site_id, site_name, site_label,
    prop_mean, prop_median,
    peak_window, mean_peak_n_users_14d
  ) %>%
  arrange(desc(prop_mean))

site_summary_points <- site_summary_points %>%
  left_join(
    site_locations %>% select(-match_dist_km),
    by = c("site_id", "site_name", "site_label")
  ) %>%
  st_as_sf()


# save outputs
saveRDS(site_peak_windows_by_year, "data/site_peak_windows_by_year.rds")
saveRDS(site_use_by_year, "data/site_use_by_year.rds")
saveRDS(site_summary_points, "data/site_summary_points.rds")
saveRDS(site_presence_days, "data/site_presence_days.rds")
saveRDS(site_location_fixes, "data/site_location_fixes.rds")
