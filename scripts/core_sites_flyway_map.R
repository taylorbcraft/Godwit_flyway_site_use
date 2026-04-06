# =============================================================================
# Godwit flyway map: core sites and site-use overview
#
# purpose
#   Create a simple, publication-ready map showing the main nonbreeding / stopover
#   sites across the flyway, scaled by average proportional site use across years
#
# inputs
#   - data/site_summary.rds: site points with summary metrics (e.g., prop_mean)
#   - data/loc_sf.rds: site-assigned location fixes (sf)
#
# outputs
#   - figures/core_nonbreeding_sites_map.pdf
# =============================================================================

# packages
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(ggrepel)

# -------------------------------
# read inputs
# -------------------------------
site_summary <- readRDS("data/site_summary.rds")
loc_sf <- readRDS("data/loc_sf.rds")

# natural earth basemap
world <- ne_countries(scale = 50, returnclass = "sf")

# =============================================================================
# 1) flyway overview map (orthographic projection)
# =============================================================================

# graticule in lon/lat, then reproject
grat_ll <- st_graticule(
  lon = seq(-180, 180, by = 10),
  lat = seq(-90, 90, by = 10),
  crs = 4326
)

# orthographic view roughly centred on the flyway
ortho_crs <- "+proj=ortho +lat_0=35 +lon_0=-10 +datum=WGS84 +units=m +no_defs"

world_o <- st_transform(world, ortho_crs)
grat_o <- st_transform(grat_ll, ortho_crs)
sites_o <- st_transform(site_summary, ortho_crs)

# set plot bounds to the sites extent with padding
bb <- st_bbox(sites_o)

pad_x <- 0.6 * (bb["xmax"] - bb["xmin"])
pad_y <- 0.4 * (bb["ymax"] - bb["ymin"])

xlim_use <- c(bb["xmin"] - pad_x, bb["xmax"] + pad_x)
ylim_use <- c(bb["ymin"] - pad_y, bb["ymax"] + pad_y)

flyway <- ggplot() +
  # basemap
  geom_sf(
    data = world_o,
    fill = "grey90",
    colour = "white",
    linewidth = 0.1
  ) +
  # graticule
  geom_sf(
    data = grat_o,
    colour = "grey50",
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  # sites scaled by average proportional use
  geom_sf(
    data = sites_o,
    aes(size = prop_mean),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.8,
    alpha = 0.85
  ) +
  # labels
  geom_label_repel(
    data = sites_o,
    aes(
      geometry = geometry,
      label = sprintf("%s: %.0f%%", site_name, prop_mean * 100)
    ),
    stat = "sf_coordinates",
    min.segment.length = 0,
    segment.colour = "black",
    box.padding = 0.35,
    point.padding = 0.2,
    label.size = 0.15,
    label.r = unit(0.5, "lines"),
    size = 3,
    force = 10,
    force_pull = 0.1,
    max.time = 2,
    seed = 2
  ) +
  coord_sf(
    xlim = xlim_use,
    ylim = ylim_use,
    expand = FALSE
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "none",
    plot.title = element_text(size = 24, colour = "#6F9CB5", face = "plain")
  ) +
  labs(title = "Core nonbreeding sites and\naverage site use (2013–2025)")

print(flyway)

# save with aspect ratio matching the map extent
xr <- as.numeric(diff(xlim_use))
yr <- as.numeric(diff(ylim_use))
asp <- xr / yr

w <- 6
h <- w / asp

ggsave(
  filename = "figures/core_nonbreeding_sites_map.pdf",
  plot = flyway,
  width = w,
  height = h,
  units = "in"
)
