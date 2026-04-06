# =============================================================================
# Godwit migration map: tagging site locations
#
# purpose
#   Create a simple overview map showing the locations of tagging sites used in
#   the project.
#
# output
#   - figures/tag_sites_map.pdf
# =============================================================================

# packages
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

# -------------------------------
# tagging site coordinates
# -------------------------------
tag_sites <- tibble::tribble(
  ~tag_site,              ~lon,   ~lat,
  "Southwest Friesland",   5.60,  53.15,
  "South Holland",         4.40,  52.05,
  "Dümmer",                8.35,  52.52,
  "Unterelbe",             9.20,  53.75,
  "Extremadura",          -6.30,  39.10,
  "Tagus estuary",        -9.10,  38.75
) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# create output directory if needed
if (!dir.exists("figures")) dir.create("figures", recursive = TRUE)

# -------------------------------
# basemap and projection
# -------------------------------
land <- ne_countries(scale = "medium", returnclass = "sf")

grat_ll <- st_graticule(
  lon = seq(-180, 180, by = 10),
  lat = seq(-90, 90, by = 10),
  crs = 4326
)

ortho_crs <- "+proj=ortho +lat_0=35 +lon_0=-10 +datum=WGS84 +units=m +no_defs"

# colourblind-safe Okabe–Ito palette
okabe_ito <- c(
  "Southwest Friesland" = "#0072B2",
  "South Holland"       = "#009E73",
  "Dümmer"              = "#D55E00",
  "Unterelbe"           = "#CC79A7",
  "Tagus estuary"       = "#E69F00",
  "Extremadura"         = "#56B4E9"
)

land_o <- st_transform(land, ortho_crs)
grat_o <- st_transform(grat_ll, ortho_crs)
tags_o <- st_transform(tag_sites, ortho_crs)

# -------------------------------
# crop to tagging sites extent
# -------------------------------
bb <- st_bbox(tags_o)

pad_x <- 0.4 * (bb["xmax"] - bb["xmin"])
pad_y <- 0.4 * (bb["ymax"] - bb["ymin"])

xlim_use <- c(bb["xmin"] - pad_x, bb["xmax"] + pad_x)
ylim_use <- c(bb["ymin"] - pad_y, bb["ymax"] + pad_y)

# -------------------------------
# plot
# -------------------------------
p_globe_tags <- ggplot() +
  geom_sf(data = land_o, fill = "grey92", colour = NA) +
  geom_sf(
    data = grat_o,
    colour = "grey70",
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  geom_sf(
    data = tags_o,
    aes(colour = tag_site),
    size = 3
  ) +
  scale_colour_manual(values = okabe_ito) +
  coord_sf(
    crs = st_crs(tags_o),
    xlim = xlim_use,
    ylim = ylim_use,
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.title = element_blank(),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.position = c(0.5, 0.98),
    legend.justification = c(1, 1)
  )

print(p_globe_tags)

ggsave(
  filename = "figures/tag_sites_map.pdf",
  plot = p_globe_tags,
  width = 8,
  height = 5,
  units = "in"
)
