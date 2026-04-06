# =============================================================================
# Godwit nonbreeding site use (recent years): peak 2-week windows and proportional use
#
# purpose
#   Summarise how tracked godwits used major nonbreeding and stopover sites
#   during the two most recent nonbreeding cycles in the tracking dataset.
#   Site definitions are taken from the full dataset so that results are
#   directly comparable with the long-term analysis.
#
#   Two main metrics are produced:
#     1) the peak 14-day period of use for each site in each migratory cycle
#     2) the proportion of tracked birds that used each site in each cycle,
#        and the average across the two cycles
#
# general workflow
#   1) read and clean tracking data
#      - load location data and retain stationary fixes
#      - remove individuals with little latitudinal movement
#      - restrict the dataset to the nonbreeding region
#      - limit the number of fixes per bird per day
#
#   2) identify sites used by birds
#      - locations are grouped into spatial clusters representing commonly
#        used nonbreeding or stopover areas
#      - clusters are matched to known site locations to assign site names
#
#   3) define migratory cycles
#      - cycles are anchored to 1 June so each represents one full
#        nonbreeding season
#
#   4) restrict the dataset to the two most recent cycles
#      - site definitions remain unchanged, but metrics are calculated
#        using only the most recent tracking years
#
#   5) determine daily site use
#      - a bird is considered present if at least one location occurs
#        at a site on that day
#
#   6) calculate peak site use
#      - identify the 14-day period with the highest number of birds
#        present at each site in each cycle
#
#   7) calculate proportional site use
#      - calculate the fraction of tracked birds in each cycle that
#        visited each site
#
# outputs
#   Tables and spatial layers describing:
#     - site locations
#     - daily presence of birds
#     - peak 14-day use windows
#     - proportional site use by cycle
#
# interpretation notes
#   Peak windows indicate when sites experience the greatest concentration
#   of tracked birds. Proportional use reflects the share of tracked birds
#   visiting a site in a given cycle and depends on the number of birds
#   tracked in those years.
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
data_raw <- read.csv("data/all_locations.csv", stringsAsFactors = FALSE)

data_clean <- data_raw %>%
  mutate(
    timestamp = ymd_hms(timestamp, tz = "UTC", quiet = TRUE),
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
loc_sf <- data_clean %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)

coords <- st_coordinates(loc_sf) %>% as.matrix()

grid_m <- 250
coords_snap <- tibble(
  x = round(coords[, 1] / grid_m) * grid_m,
  y = round(coords[, 2] / grid_m) * grid_m
)

coords_snap_unique <- coords_snap %>%
  count(x, y, name = "n_pts")

db_u <- dbscan(
  coords_snap_unique %>% select(x, y) %>% as.matrix(),
  eps = 30000,
  minPts = 1500
)

coords_snap_unique <- coords_snap_unique %>%
  mutate(site_id = db_u$cluster)

loc_sf <- loc_sf %>%
  mutate(
    site_id = coords_snap_unique$site_id[
      match(
        paste(coords_snap$x, coords_snap$y),
        paste(coords_snap_unique$x, coords_snap_unique$y)
      )
    ]
  ) %>%
  filter(!is.na(site_id), site_id != 0)

# ---------------------------------------
# 2b) assign human-readable site names
#     using nearest reference point
# ---------------------------------------
site_ref_sf <- tribble(
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
site_sf <- loc_sf %>%
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
  st_join(site_ref_sf %>% select(site_name), join = st_nearest_feature) %>%
  mutate(site_name = if_else(is.na(site_name), paste0("site_", site_id), site_name)) %>%
  # optional distance diagnostic (km) in one line
  mutate(
    match_dist_km = as.numeric(st_distance(geometry, st_geometry(site_ref_sf)[st_nearest_feature(., site_ref_sf)], by_element = TRUE)) / 1000
  ) %>%
  st_transform(4326)

# attach names back to fixes (single join)
loc_sf <- loc_sf %>%
  left_join(site_sf %>% st_drop_geometry() %>% select(site_id, site_name), by = "site_id")

# quick view
print(site_sf, n = 100)
mapview::mapview(site_sf)

# ------------------------------------------------------------
# 3) define migratory cycle label (nonbreeding year)
# ------------------------------------------------------------
# note: we keep the trough calculation for reference, but we use a fixed anchor: june 1
n_individuals_year <- data_clean %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(n_tracked = n_distinct(local_identifier), .groups = "drop")

daily_occ <- data_clean %>%
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

# ------------------------------------------------------------
# restrict analysis to last 2 nonbreeding cycles
# ------------------------------------------------------------
loc_sf <- loc_sf %>%
  add_nb_year(date)

last_two_nb_years <- loc_sf %>%
  st_drop_geometry() %>%
  distinct(nb_year) %>%
  arrange(nb_year) %>%
  slice_tail(n = 2) %>%
  pull(nb_year)

loc_sf <- loc_sf %>%
  filter(nb_year %in% last_two_nb_years)

# ---------------------------------------------
# 4) reduce to unique presence-days per site/cycle
# ---------------------------------------------
presence_days <- loc_sf %>%
  st_drop_geometry() %>%
  distinct(local_identifier, site_id, site_name, nb_year, nb_year_start, date, .keep_all = FALSE) %>%
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
site_peak_windows_by_year <- presence_days %>%
  group_by(site_id, site_name, nb_year, day_in_year) %>%
  summarise(n_users = n_distinct(local_identifier), .groups = "drop") %>%
  group_by(site_id, site_name, nb_year) %>%
  complete(day_in_year = 1:366, fill = list(n_users = 0)) %>%
  arrange(day_in_year) %>%
  mutate(roll_14 = slide_dbl(n_users, sum, .before = 13, .complete = TRUE)) %>%
  slice_max(roll_14, n = 1, with_ties = FALSE) %>%
  transmute(
    site_id, site_name, nb_year,
    peak_end_day = day_in_year,
    peak_start_day = wrap_day(day_in_year - 13L),
    peak_n_users_14d = roll_14,
    peak_window = paste0(fmt_mmdd(wrap_day(day_in_year - 13L)), "–", fmt_mmdd(day_in_year))
  ) %>%
  ungroup()

# ------------------------------------------------------------
# 6) proportional use per site per cycle, then average over years
# ------------------------------------------------------------
n_total_by_year <- presence_days %>%
  distinct(nb_year, local_identifier) %>%
  count(nb_year, name = "n_total")

site_prop_by_year <- presence_days %>%
  distinct(site_id, site_name, nb_year, local_identifier) %>%
  count(site_id, site_name, nb_year, name = "n_site") %>%
  left_join(n_total_by_year, by = "nb_year") %>%
  mutate(prop_site = n_site / n_total)

site_prop_summary <- site_prop_by_year %>%
  group_by(site_id, site_name) %>%
  summarise(
    prop_mean = mean(prop_site, na.rm = TRUE),
    prop_median = median(prop_site, na.rm = TRUE),
    n_years = n_distinct(nb_year),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 7) single per-site peak window summary (no n_cycles)
# ------------------------------------------------------------
site_peak_summary <- site_peak_windows_by_year %>%
  group_by(site_id, site_name, peak_window) %>%
  summarise(
    mean_peak_n_users_14d = mean(peak_n_users_14d, na.rm = TRUE),
    n_occurrences = n(),
    .groups = "drop"
  ) %>%
  group_by(site_id, site_name) %>%
  arrange(desc(n_occurrences), desc(mean_peak_n_users_14d), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(site_id, site_name, peak_window, mean_peak_n_users_14d)

# ------------------------------------------------------------
# 8) final per-site table
# ------------------------------------------------------------
site_summary <- site_prop_summary %>%
  left_join(site_peak_summary, by = c("site_id", "site_name")) %>%
  select(
    site_id, site_name,
    prop_mean, prop_median,
    peak_window, mean_peak_n_users_14d
  ) %>%
  arrange(desc(prop_mean))

site_summary <- site_summary %>%
  left_join(
    site_sf %>% select(-match_dist_km),
    by = c("site_id", "site_name")
  ) %>%
  st_as_sf()


# save outputs
saveRDS(presence_days, "data/last_2_years/presence_days.rds")
saveRDS(site_prop_by_year, "data/last_2_years/site_prop_by_year.rds")
saveRDS(site_summary, "data/last_2_years/site_summary.rds")
saveRDS(loc_sf, "data/last_2_years/loc_sf.rds")
