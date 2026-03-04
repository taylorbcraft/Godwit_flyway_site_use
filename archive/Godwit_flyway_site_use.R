# packages
library(dplyr)
library(lubridate)
library(dbscan)
library(sf)
library(tidyr)
library(purrr)
library(ggplot2)
library(forcats)
library(ggridges)
library(mapview)
library(rnaturalearth)
library(scales)
library(tibble)
library(ggrepel)

# read data
data_raw <- read.csv("all_locations.csv", stringsAsFactors = FALSE)

# clean locations
data_clean <- data_raw %>%
  mutate(
    # parse times and create date
    timestamp = ymd_hms(timestamp, tz = "UTC", quiet = TRUE),
    date = as.Date(timestamp),
    location_lat = Y,
    location_long = X,
    local_identifier = individual_local_identifier) %>%
  # filter study and movement states
  filter(study_site != "PL") %>%
  filter(is.na(ground_speed) | ground_speed == 0) %>%
  filter(timestamp < ymd("2025-07-01")) %>%
  # keep only non-breeding fixes (below France)
  filter(location_lat < 42)  %>%
  # keep max 4 locations per individual per day
  group_by(local_identifier, date) %>%
  arrange(timestamp, .by_group = TRUE) %>%
  slice_head(n = 4) %>%
  ungroup() %>%
  select(c(local_identifier,location_long,location_lat,timestamp,date))

# convert to sf and project to meters
loc_sf <- data_clean %>%
  filter(!is.na(location_lat), !is.na(location_long)) %>%
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326, remove = FALSE) %>%
  st_transform(3857)

# dbscan on snapped coordinates for speed
coords <- st_coordinates(loc_sf) %>% as.matrix()

grid_m <- 250
coords_snap <- cbind(
  x = round(coords[, 1] / grid_m) * grid_m,
  y = round(coords[, 2] / grid_m) * grid_m
)

coords_snap_df <- tibble(x = coords_snap[, 1], y = coords_snap[, 2])

coords_snap_unique <- coords_snap_df %>%
  count(x, y, name = "n_pts")

db_u <- dbscan(
  coords_snap_unique %>% dplyr::select(x, y) %>% as.matrix(),
  eps = 30000,
  minPts = 1500
)

coords_snap_unique <- coords_snap_unique %>%
  mutate(site_id = db_u$cluster)

coords_snap_df <- coords_snap_df %>%
  left_join(coords_snap_unique %>% dplyr::select(x, y, site_id), by = c("x", "y"))

loc_sf <- loc_sf %>%
  mutate(site_id = coords_snap_df$site_id) %>%
  filter(site_id != 0)

visits <- loc_sf %>%
  group_by(local_identifier, site_id) %>%
  summarise(
    visit_start = min(date),
    visit_end = max(date),
    stay_days = n_distinct(date),
    median_lat = median(location_lat, na.rm = TRUE),
    median_long = median(location_long, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  st_drop_geometry()

visits <- visits %>%
  mutate(
    arrival_doy = yday(visit_start),
    departure_doy = yday(visit_end)
  )

key_dates_per_site <- visits %>%
  group_by(site_id) %>%
  summarise(
    n_individuals = n_distinct(local_identifier),
    n_visits = n(),
    arrival_doy_median = as.integer(median(arrival_doy, na.rm = TRUE)),
    departure_doy_median = as.integer(median(departure_doy, na.rm = TRUE)),
    arrival_doy_p10 = as.integer(quantile(arrival_doy, 0.10, na.rm = TRUE, names = FALSE)),
    arrival_doy_p90 = as.integer(quantile(arrival_doy, 0.90, na.rm = TRUE, names = FALSE)),
    departure_doy_p10 = as.integer(quantile(departure_doy, 0.10, na.rm = TRUE, names = FALSE)),
    departure_doy_p90 = as.integer(quantile(departure_doy, 0.90, na.rm = TRUE, names = FALSE)),
    .groups = "drop"
  ) %>%
  mutate(
    ref_year = 2001,  # non-leap reference year
    arrival_median = format(as.Date(arrival_doy_median - 1, origin = paste0(ref_year, "-01-01")), "%b %d"),
    departure_median = format(as.Date(departure_doy_median - 1, origin = paste0(ref_year, "-01-01")), "%b %d"),
    arrival_window = paste0(
      format(as.Date(arrival_doy_p10 - 1, origin = paste0(ref_year, "-01-01")), "%b %d"),
      "–",
      format(as.Date(arrival_doy_p90 - 1, origin = paste0(ref_year, "-01-01")), "%b %d")
    ),
    departure_window = paste0(
      format(as.Date(departure_doy_p10 - 1, origin = paste0(ref_year, "-01-01")), "%b %d"),
      "–",
      format(as.Date(departure_doy_p90 - 1, origin = paste0(ref_year, "-01-01")), "%b %d")
    )
  ) %>%
  select(-ref_year)



# summarise key sites
site_table <- visits %>%
  group_by(site_id) %>%
  summarise(
    n_individuals = n_distinct(local_identifier),
    n_visits = n(),
    total_stay_days = sum(stay_days, na.rm = TRUE),
    median_stay_days = median(stay_days, na.rm = TRUE),
    site_lat = median(median_lat, na.rm = TRUE),
    site_long = median(median_long, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_stay_days))

# manual site name lookup (based on current centroids)
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
  12L,      "Inner Niger Delta (West)",                 16.5,     -3.22,
)

# attach site names
site_table <- site_table %>%
  dplyr::select(-any_of(c("site_name", "site_name.x", "site_name.y"))) %>%
  left_join(site_name_lookup %>% dplyr::select(site_id, site_name), by = "site_id") %>%
  mutate(site_name = if_else(is.na(site_name), paste0("site_", site_id), site_name))

visits <- visits %>%
  dplyr::select(-any_of(c("site_name", "site_name.x", "site_name.y"))) %>%
  left_join(site_name_lookup %>% dplyr::select(site_id, site_name), by = "site_id") %>%
  mutate(site_name = if_else(is.na(site_name), paste0("site_", site_id), site_name))

# map sites
site_sf <- site_table %>%
  st_as_sf(coords = c("site_long", "site_lat"), crs = 4326, remove = FALSE)
mapview(site_sf)

# expand visits into daily presence
presence_daily <- visits %>%
  mutate(date_seq = map2(visit_start, visit_end, ~ seq(.x, .y, by = "day"))) %>%
  dplyr::select(site_id, site_name, local_identifier, date_seq) %>%
  unnest(date_seq) %>%
  rename(date = date_seq)

# weekly counts per site, aggregated across years
presence_weekly <- presence_daily %>%
  mutate(
    week = isoweek(date),
    year = year(date)
  ) %>%
  group_by(site_id, site_name, year, week) %>%
  summarise(n_individuals = n_distinct(local_identifier), .groups = "drop") %>%
  group_by(site_id, site_name, week) %>%
  summarise(mean_individuals = mean(n_individuals, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    site_table %>% dplyr::select(site_id, site_lat),
    by = "site_id"
  ) %>%
  mutate(
    # order north to south
    site_name_f = fct_reorder(site_name, site_lat)
  )

saveRDS(site_sf, 'site_sf.rds')
saveRDS(presence_daily, 'presence_daily.rds')
saveRDS(presence_weekly, 'presence_weekly.rds')
