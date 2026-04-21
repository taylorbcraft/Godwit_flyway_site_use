# =============================================================================
# Godwit migration map: example tracks by tagging site
#
# Purpose
#   Create a simple map showing example migration routes of tracked godwits.
#   One representative bird is selected from each tagging site and its route
#   is plotted to illustrate the general structure of the flyway.
#
# Workflow
#   1) read tracking data and restrict to one annual cycle
#   2) reduce the dataset to one location per bird per day
#   3) assign each bird to its tagging site
#   4) select one representative bird per tagging site
#   5) convert locations into migration routes
#   6) plot routes on an orthographic flyway map
#
# Input
#   - data/all_locations.csv: raw tracking data
# Output
#   - figures/example_migration_map.pdf
# =============================================================================

# packages
library(dplyr)
library(lubridate)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(stringr)

# ------------------------------------------------------------
# 1) create 1 fix per bird per day (time window subset)
# ------------------------------------------------------------
location_data_raw <- read.csv("data/all_locations.csv", stringsAsFactors = FALSE)


t_min <- as.POSIXct("2021-06-01", tz = "UTC")
t_max <- as.POSIXct("2022-06-01", tz = "UTC")

data_1day <- location_data_raw %>%
  mutate(
    timestamp = {
      parsed_datetime <- ymd_hms(timestamp, tz = "UTC", quiet = TRUE)
      parsed_date <- as.POSIXct(ymd(timestamp, quiet = TRUE), tz = "UTC")
      coalesce(parsed_datetime, parsed_date)
    }
  ) %>%
  filter(!is.na(X), !is.na(Y), !is.na(timestamp)) %>%
  filter(timestamp > t_min, timestamp < t_max) %>%
  mutate(date = as.Date(timestamp)) %>%
  arrange(individual_local_identifier, date, timestamp) %>%
  group_by(individual_local_identifier, date) %>%
  slice(1) %>%
  ungroup()

# ------------------------------------------------------------
# 2) assign tagging site per individual (for colouring)
# ------------------------------------------------------------
bird_tag_site <- data_1day %>%
  distinct(individual_local_identifier, study_site, study_name) %>%
  mutate(
    tag_site = case_when(
      str_detect(study_name, regex("South Holland", ignore_case = TRUE)) ~ "South Holland",
      str_detect(study_name, regex("Tagus|BtgTagus", ignore_case = TRUE)) ~ "Tagus estuary",
      str_detect(study_name, regex("Extremadura", ignore_case = TRUE)) ~ "Extremadura",
      study_site == "Dümmer" ~ "Dümmer",
      study_site == "Unterelbe" ~ "Unterelbe",
      study_site == "NL" ~ "SW Friesland",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(individual_local_identifier) %>%
  summarise(
    tag_site = tag_site[which(!is.na(tag_site))[1]],
    .groups = "drop"
  )

data_1day2 <- data_1day %>%
  left_join(bird_tag_site, by = "individual_local_identifier") %>%
  filter(!is.na(tag_site))

# ------------------------------------------------------------
# 3) choose 1 representative bird per tagging site
#     - chosen as the bird with the most days in the time window
# ------------------------------------------------------------
one_bird_per_site <- data_1day2 %>%
  count(tag_site, individual_local_identifier, name = "n_days") %>%
  arrange(tag_site, desc(n_days)) %>%
  group_by(tag_site) %>%
  slice(1) %>%
  ungroup()

tracks_1bird <- data_1day2 %>%
  inner_join(
    one_bird_per_site %>% select(tag_site, individual_local_identifier),
    by = c("tag_site", "individual_local_identifier")
  ) %>%
  arrange(tag_site, timestamp)

# ------------------------------------------------------------
# 4) build routes (one line per selected bird)
# ------------------------------------------------------------
routes_sf <- tracks_1bird %>%
  group_by(tag_site, individual_local_identifier) %>%
  summarise(
    geometry = st_sfc(st_linestring(as.matrix(cbind(X, Y)))),
    .groups = "drop"
  ) %>%
  st_as_sf(crs = 4326)

# ------------------------------------------------------------
# 5) basemap, graticules, and projection
# ------------------------------------------------------------

land <- ne_countries(scale = "medium", returnclass = "sf")

grat_ll <- st_graticule(
  lon = seq(-180, 180, by = 10),
  lat = seq(-90, 90, by = 10),
  crs = 4326
)

ortho_crs <- "+proj=ortho +lat_0=35 +lon_0=-10 +datum=WGS84 +units=m +no_defs"

# colourblind-safe Okabe–Ito palette
okabe_ito <- c(
  "SW Friesland"   = "#0072B2",
  "South Holland"  = "#009E73",
  "Dümmer"         = "#D55E00",
  "Unterelbe"      = "#CC79A7",
  "Tagus estuary"  = "#E69F00",
  "Extremadura"    = "#56B4E9"
)

land_o   <- st_transform(land, ortho_crs)
grat_o   <- st_transform(grat_ll, ortho_crs)
routes_o <- st_transform(routes_sf, ortho_crs)

# ------------------------------------------------------------
# 6) crop to routes extent (projected coordinates)
# ------------------------------------------------------------
bb <- st_bbox(routes_o)

pad_x <- 0.4 * (bb["xmax"] - bb["xmin"])
pad_y <- 0.4 * (bb["ymax"] - bb["ymin"])

xlim_use <- c(bb["xmin"] - pad_x, bb["xmax"] + pad_x)
ylim_use <- c(bb["ymin"] - pad_y, bb["ymax"] + pad_y)

# ------------------------------------------------------------
# 7) plot (no legend; illustrative example tracks)
# ------------------------------------------------------------
p_globe_crop <- ggplot() +
  geom_sf(data = land_o, fill = "grey90", colour = NA) +
  geom_sf(
    data = grat_o,
    colour = "grey70",
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  geom_sf(
    data = routes_o,
    aes(colour = tag_site),
    linewidth = 0.8,
    alpha = 0.55
  ) +
  scale_colour_manual(values = okabe_ito) +
  coord_sf(
    crs = st_crs(routes_o),
    xlim = xlim_use,
    ylim = ylim_use,
    expand = FALSE
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = NA)
  )

p_globe_crop

ggsave(
  filename = "figures/example_migration_map.pdf",
  plot = p_globe_crop,
  width = 8,
  height = 5,
  units = "in"
)
