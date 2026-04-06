# =============================================================================
# Godwit site use: seasonal use profiles by site (ridgeline plot)
#
# Purpose
#   Plot the average seasonal pattern of site use across the nonbreeding cycle.
#   For each site and day-of-cycle, the plot shows the mean proportion of tracked
#   birds present, averaged across years.
#
# Inputs
#   - data/last_2_years/site_summary.rds: site locations (used for ordering sites north to south)
#   - data/last_2_years/presence_days.rds: daily bird presence at sites (one row per bird × site × day)
#
# Output
#   - figures/last_2_years/ridge_plot.pdf
# =============================================================================

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ggridges)
library(sf)

# -------------------------------
# read inputs
# -------------------------------
site_summary <- readRDS("data/last_2_years/site_summary.rds")
presence_days <- readRDS("data/last_2_years/presence_days.rds")


# ------------------------------------------------------------
# prepare daily proportions
# ------------------------------------------------------------

# number of tracked birds per cycle (denominator)
denom_year <- presence_days %>%
  distinct(nb_year, local_identifier) %>%
  count(nb_year, name = "n_tracked")

# birds present per site x day per cycle (numerator) and daily proportion
site_day <- presence_days %>%
  distinct(nb_year, site_id, site_name, day_in_year, local_identifier) %>%
  count(nb_year, site_id, site_name, day_in_year, name = "n_present") %>%
  left_join(denom_year, by = "nb_year") %>%
  mutate(prop_present = n_present / n_tracked)

# average daily proportions across cycles
site_day_mean <- site_day %>%
  group_by(site_id, site_name, day_in_year) %>%
  summarise(prop = mean(prop_present, na.rm = TRUE), .groups = "drop") %>%
  group_by(site_id, site_name) %>%
  complete(day_in_year = 1:366, fill = list(prop = 0)) %>%
  ungroup()

# order sites by latitude (north to south) for plotting
site_order <- site_summary %>%
  st_drop_geometry() %>%
  arrange(desc(latitude)) %>%
  pull(site_name)

site_day_mean <- site_day_mean %>%
  mutate(
    site_name = factor(site_name, levels = site_order),
    site_name = fct_rev(site_name)
  )

# ------------------------------------------------------------
# plot
# ------------------------------------------------------------

# x-axis labels for a cycle anchored to 1 June
month_breaks <- c(1, 31, 62, 92, 123, 153, 184, 215, 245, 276, 306, 337)
month_labels <- c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov",
                  "Dec", "Jan", "Feb", "Mar", "Apr", "May")

p_ridge <- ggplot(site_day_mean, aes(x = day_in_year, y = site_name, height = prop)) +
  geom_ridgeline(
    aes(fill = site_name),
    stat = "identity",
    scale = 12,
    min_height = 0,
    colour = "grey20",
    linewidth = 0.25,
    alpha = 0.5
  ) +
  scale_x_continuous(
    breaks = month_breaks,
    labels = month_labels,
    expand = c(0, 0)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9)
  )

print(p_ridge)

ggsave(
  filename = "figures/last_2_years/ridge_plot.pdf",
  plot = p_ridge,
  width = 8,
  height = 5,
  units = "in"
)
