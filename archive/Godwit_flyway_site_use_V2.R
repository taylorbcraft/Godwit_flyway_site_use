# =============================================================================
# GODWIT NONBREEDING SITE USE WORKFLOW
#
# goal
#   quantify nonbreeding site use and timing for tracked godwits by:
#     1) clustering nonbreeding fixes into discrete "sites"
#     2) defining per-individual seasonal site use ("visits") within a migration cycle
#     3) summarising each site with key metrics:
#        - proportional use
#        - median length of stay
#        - peak 2-week arrival window
#        - peak 2-week occupancy window
#        - peak 2-week departure window
#
# overview of the pipeline
#   1) data cleaning
#      - drop moving fixes (keep stationary/grounded fixes)
#      - restrict to a study period cutoff date
#      - remove non-migrants: exclude individuals with < 5° total latitudinal range
#      - restrict to nonbreeding region (latitude < 42)
#      - thin to at most 4 fixes per individual per day
#
#   2) site definition (spatial clustering)
#      - convert fixes to sf and project to meters
#      - snap projected coordinates to a 250 m grid for computational speed
#      - run dbscan on unique snapped points (eps = 30 km, minPts = 1500)
#      - assign a site_id back to each fix and drop noise (cluster 0)
#      - attach human-readable site names via a manual lookup table
#
#   3) define a "migration cycle" (nonbreeding year)
#      - compute daily proportional occupancy in the nonbreeding region:
#           prop_present(y, d) = n_present(y, d) / n_tracked(y)
#        where:
#           n_present(y, d) = distinct individuals present on day-of-year d in calendar year y
#           n_tracked(y)    = distinct individuals tracked in calendar year y
#      - average across years to obtain mean_prop_present(d)
#      - locate the lowest-occupancy 14-day window (data-driven trough)
#      - choose a clean anchor date within that trough:
#           anchor = 1 june
#      - define nonbreeding year label (nb_year) for any date:
#           if date >= anchor in calendar year Y -> nb_year = Y
#           else                                 -> nb_year = Y - 1
#        and nb_year_start = anchor date of nb_year
#
#   4) define visits (split by migration cycle)
#      - a "visit" is defined as a continuous presence spell within:
#           individual × site × nb_year
#        where new visits are started when gaps between presence-days exceed a threshold
#        (e.g., > 7 days). this prevents counting leave/return as the same visit.
#
#   5) daily presence and key date metrics (per site)
#      - expand each visit into daily presence (all days between visit_start and visit_end)
#      - convert each presence day to day_in_year relative to nb_year_start:
#           day_in_year = (date - nb_year_start) + 1
#
# outputs
#   - visits: seasonal site-use records (individual × site × nb_year × visit_id)
#   - site_key_dates: per-site table of use, timing, and proportional use
# =============================================================================

# packages
library(dplyr)
library(lubridate)
library(dbscan)
library(sf)
library(tidyr)
library(purrr)
library(tibble)
library(slider)

# -------------------------------
# 1) read and clean tracking data
# -------------------------------
data_raw <- read.csv("all_locations.csv", stringsAsFactors = FALSE)

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
  filter(timestamp < ymd("2025-07-01")) %>%
  # remove birds with low latitudinal range across full tracking period
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
  select(local_identifier, location_long, location_lat, timestamp, date) %>%
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
# 2b) attach site names (manual lookup)
# ---------------------------------------
site_name_lookup <- tribble(
  ~site_id, ~site_name,                                ~ref_lat, ~ref_long,
  13L,      "Ebro Delta",                               40.7,      0.777,
  10L,      "Extremadura",                              39.1,     -5.93,
  8L,       "Tagus Estuary",                            38.9,     -8.94,
  9L,       "Doñana National Park",                     37.1,     -6.24,
  3L,       "Senegal River Delta",                      16.4,    -16.2,
  1L,       "Saloum Delta",                             14.1,    -16.6,
  4L,       "Gambia River",                             13.5,    -15.8,
  2L,       "Casamance",                                12.6,    -15.9,
  5L,       "Coastal Guinea-Bissau",                    11.2,    -15.3,
  7L,       "Lower Senegal River (West)",               16.2,    -13.2,
  6L,       "Lower Senegal River (East)",               16.5,    -14.4,
  11L,      "Inner Niger Delta (East)",                 15.3,     -4.52,
  12L,      "Inner Niger Delta (West)",                 16.5,     -3.22
)

loc_sf <- loc_sf %>%
  left_join(site_name_lookup %>% select(site_id, site_name), by = "site_id") %>%
  mutate(site_name = if_else(is.na(site_name), paste0("site_", site_id), site_name))

# ------------------------------------------------------------
# 3) choose anchor for nonbreeding year (data-driven, then round)
# ------------------------------------------------------------
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

anchor_doy <- occ_by_doy %>%
  slice_min(roll_14, n = 1, with_ties = FALSE) %>%
  summarise(anchor_doy = first(doy) - 7L) %>%
  pull(anchor_doy)

anchor_doy <- ((anchor_doy - 1L) %% 366L) + 1L
anchor_date_label <- format(as.Date(anchor_doy - 1L, origin = "2001-01-01"), "%b %d")

# use a clean boundary within the trough
anchor_month <- 6L
anchor_day <- 1L

# -------------------------------------------------------
# 4) label nonbreeding years using the chosen anchor date
# -------------------------------------------------------
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
# 5) define visits within each nonbreeding year
#    split by gaps (leave/return => new visit)
# ---------------------------------------------
gap_days <- 7L

# reduce to unique presence-days per individual-site-cycle
presence_days <- loc_sf %>%
  add_nb_year(date) %>%
  st_drop_geometry() %>%
  distinct(local_identifier, site_id, site_name, nb_year, nb_year_start, date, .keep_all = TRUE) %>%
  group_by(local_identifier, site_id, site_name, nb_year, nb_year_start) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    gap = as.integer(date - lag(date)),
    new_visit = if_else(is.na(gap) | gap > gap_days, 1L, 0L),
    visit_id = cumsum(new_visit)
  ) %>%
  ungroup()

# summarise continuous visits
visits <- presence_days %>%
  group_by(local_identifier, site_id, site_name, nb_year, nb_year_start, visit_id) %>%
  summarise(
    visit_start = min(date, na.rm = TRUE),
    visit_end   = max(date, na.rm = TRUE),
    stay_days   = as.integer(visit_end - visit_start) + 1L,
    n_days_observed = n_distinct(date),
    median_lat  = median(location_lat, na.rm = TRUE),
    median_long = median(location_long, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------------------------------
# 6) create the site key date table (simple modal metrics)
# -------------------------------------------------------
# build daily presence (within nonbreeding year) from continuous visits
presence_daily <- visits %>%
  mutate(date = map2(visit_start, visit_end, ~ seq(.x, .y, by = "day"))) %>%
  select(site_id, site_name, local_identifier, nb_year, nb_year_start, visit_id, date) %>%
  unnest(date) %>%
  mutate(day_in_year = as.integer(date - nb_year_start) + 1L)

# core site metrics
site_core <- visits %>%
  group_by(site_id, site_name) %>%
  summarise(
    n_individuals = n_distinct(local_identifier),
    n_individual_years = n_distinct(paste(local_identifier, nb_year)),
    n_years = n_distinct(nb_year),
    median_stay_days = median(stay_days, na.rm = TRUE),
    .groups = "drop"
  )

# arrivals and departures per visit (counts leave/return as separate events)
arrdep <- visits %>%
  mutate(
    arrival_day = as.integer(visit_start - nb_year_start) + 1L,
    departure_day = as.integer(visit_end - nb_year_start) + 1L
  ) %>%
  select(site_id, site_name, local_identifier, nb_year, visit_id, arrival_day, departure_day)

# helper: wrap day into 1..366 (for window starts)
wrap_day <- function(x) ((x - 1L) %% 366L) + 1L

# peak 2-week arrival window
arrival_modal <- arrdep %>%
  group_by(site_id, site_name) %>%
  count(arrival_day, name = "n_arrivals") %>%
  complete(arrival_day = 1:366, fill = list(n_arrivals = 0)) %>%
  arrange(arrival_day) %>%
  mutate(roll_14 = slide_dbl(n_arrivals, sum, .before = 13, .complete = TRUE)) %>%
  slice_max(roll_14, n = 1, with_ties = FALSE) %>%
  transmute(
    site_id, site_name,
    arrival_window_end_day = arrival_day,
    arrival_window_start_day = wrap_day(arrival_day - 13L),
    arrival_window_n = roll_14
  ) %>%
  ungroup()

# peak 2-week departure window
departure_modal <- arrdep %>%
  group_by(site_id, site_name) %>%
  count(departure_day, name = "n_departures") %>%
  complete(departure_day = 1:366, fill = list(n_departures = 0)) %>%
  arrange(departure_day) %>%
  mutate(roll_14 = slide_dbl(n_departures, sum, .before = 13, .complete = TRUE)) %>%
  slice_max(roll_14, n = 1, with_ties = FALSE) %>%
  transmute(
    site_id, site_name,
    departure_window_end_day = departure_day,
    departure_window_start_day = wrap_day(departure_day - 13L),
    departure_window_n = roll_14
  ) %>%
  ungroup()

# peak 2-week occupancy window (mean across nonbreeding years)
occupancy_modal <- presence_daily %>%
  group_by(site_id, site_name, nb_year, day_in_year) %>%
  summarise(n_individuals = n_distinct(local_identifier), .groups = "drop") %>%
  group_by(site_id, site_name, day_in_year) %>%
  summarise(mean_n = mean(n_individuals, na.rm = TRUE), .groups = "drop") %>%
  complete(day_in_year = 1:366, fill = list(mean_n = 0)) %>%
  arrange(day_in_year) %>%
  group_by(site_id, site_name) %>%
  mutate(roll_14 = slide_dbl(mean_n, sum, .before = 13, .complete = TRUE)) %>%
  slice_max(roll_14, n = 1, with_ties = FALSE) %>%
  transmute(
    site_id, site_name,
    peak_window_end_day = day_in_year,
    peak_window_start_day = wrap_day(day_in_year - 13L),
    peak_window_sum_mean_n = roll_14
  ) %>%
  ungroup()

# format day-in-year as month-day relative to the anchor
fmt_mmdd <- function(day_in_year) {
  ref_start <- as.Date(sprintf("2001-%02d-%02d", anchor_month, anchor_day))
  format(ref_start + day_in_year - 1L, "%b %d")
}

site_key_dates <- site_core %>%
  left_join(arrival_modal, by = c("site_id", "site_name")) %>%
  left_join(occupancy_modal, by = c("site_id", "site_name")) %>%
  left_join(departure_modal, by = c("site_id", "site_name")) %>%
  mutate(
    arrival_window = paste0(fmt_mmdd(arrival_window_start_day), "–", fmt_mmdd(arrival_window_end_day)),
    peak_window = paste0(fmt_mmdd(peak_window_start_day), "–", fmt_mmdd(peak_window_end_day)),
    departure_window = paste0(fmt_mmdd(departure_window_start_day), "–", fmt_mmdd(departure_window_end_day))
  ) %>%
  select(
    site_id, site_name,
    n_individuals, n_individual_years, n_years,
    median_stay_days,
    arrival_window, arrival_window_n,
    peak_window, peak_window_sum_mean_n,
    departure_window, departure_window_n
  ) %>%
  arrange(desc(n_individual_years))

# proportion of individuals using each site per nonbreeding year, then average across years
site_prop <- visits %>%
  group_by(nb_year) %>%
  mutate(n_total = n_distinct(local_identifier)) %>%
  group_by(nb_year, site_id, site_name, n_total) %>%
  summarise(n_site = n_distinct(local_identifier), .groups = "drop") %>%
  mutate(prop_site = n_site / n_total) %>%
  group_by(site_id, site_name) %>%
  summarise(
    prop_mean = mean(prop_site, na.rm = TRUE),
    .groups = "drop"
  )

# add to site_key_dates table
site_key_dates <- site_key_dates %>%
  left_join(site_prop, by = c("site_id", "site_name"))

saveRDS(site_key_dates, "site_key_dates.rds")
