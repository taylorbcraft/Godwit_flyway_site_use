# =============================================================================
# Godwit site use: regional base map infographics for iberia and west africa
#
# purpose
#   Create two regional maps showing:
#     - the iberia basemap with site-assigned tracking fixes overlaid
#     - the west africa basemap with site-assigned tracking fixes overlaid
#
# inputs
#   - data/recent_2_years/site_location_fixes.rds: site-assigned tracking fixes (sf), must include site_name
#
# outputs
#   - figures/recent_2_years/iberia_infographic.pdf
#   - figures/recent_2_years/wafrica_infographic.pdf
# =============================================================================

# packages
library(dplyr)
library(sf)
library(ggplot2)
library(rnaturalearth)

# -------------------------------
# read inputs
# -------------------------------
site_location_fixes <- readRDS("data/recent_2_years/site_location_fixes.rds")

# basemap
world <- ne_countries(scale = 50, returnclass = "sf")

# =============================================================================
# 1) iberia map
# =============================================================================

# define region and polygon for filtering points
iberia <- world %>%
  filter(admin %in% c("Spain", "Portugal"))

iberia_poly <- iberia %>%
  st_transform(st_crs(site_location_fixes)) %>%
  st_union() %>%
  st_make_valid()

points_sf_iberia <- site_location_fixes %>%
  st_filter(iberia_poly, .predicate = st_within)

# plot
p_iberia <- ggplot() +
  geom_sf(
    data = iberia,
    fill = "SteelBlue",
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = points_sf_iberia,
    aes(colour = site_name),
    size = 0.5,
    alpha = 0.6
  ) +
  coord_sf(
    xlim = c(-9.5, 4),
    ylim = c(36, 44.5),
    expand = FALSE
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "none"
  )

print(p_iberia)

ggsave(
  filename = "figures/recent_2_years/iberia_infographic.pdf",
  plot = p_iberia,
  width = 5,
  height = 5,
  units = "in"
)

# =============================================================================
# 2) west africa map
# =============================================================================

wafrica <- world %>%
  filter(admin %in% c(
    "Morocco",
    "Western Sahara",
    "Algeria",
    "Mauritania",
    "Mali",
    "Niger",
    "Senegal",
    "Gambia",
    "Guinea-Bissau",
    "Guinea",
    "Sierra Leone",
    "Liberia",
    "Ivory Coast",
    "Burkina Faso",
    "Ghana"
  ))

wafrica_poly <- wafrica %>%
  st_transform(st_crs(site_location_fixes)) %>%
  st_union() %>%
  st_make_valid()

points_sf_wafrica <- site_location_fixes %>%
  st_filter(wafrica_poly, .predicate = st_within)

p_wafrica <- ggplot() +
  geom_sf(
    data = wafrica,
    fill = "SteelBlue",
    colour = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = points_sf_wafrica,
    aes(colour = site_name),
    size = 0.5,
    alpha = 0.6
  ) +
  coord_sf(
    xlim = c(-20, 0),
    ylim = c(0, 38),
    expand = FALSE
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "none"
  )

print(p_wafrica)

ggsave(
  filename = "figures/recent_2_years/wafrica_infographic.pdf",
  plot = p_wafrica,
  width = 5,
  height = 6,
  units = "in"
)
