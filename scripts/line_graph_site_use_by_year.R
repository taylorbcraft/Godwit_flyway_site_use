# =============================================================================
# Godwit site use: proportional use by site and year
#
# Purpose
#   Plot the proportion of tracked birds that used each site in each migratory
#   cycle. Each site is shown as a small panel, making it easy to compare
#   interannual changes in tagged-bird use.
#
# Input
#   - data/site_prop_by_year.rds
#
# Output
#   - figures/site_prop_by_year.pdf
# =============================================================================

# packages
library(dplyr)
library(ggplot2)
library(scales)

# -------------------------------
# read input
# -------------------------------
site_prop_by_year <- readRDS("data/site_prop_by_year.rds")

# -------------------------------
# order sites by overall mean use
# -------------------------------
site_order <- site_prop_by_year %>%
  group_by(site_name) %>%
  summarise(mean_prop = mean(prop_site, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_prop)) %>%
  pull(site_name)

site_prop_by_year <- site_prop_by_year %>%
  mutate(site_name = factor(site_name, levels = site_order))

# -------------------------------
# plot
# -------------------------------
x_breaks <- seq(
  min(site_prop_by_year$nb_year, na.rm = TRUE),
  max(site_prop_by_year$nb_year, na.rm = TRUE),
  by = 2
)

p <- ggplot(site_prop_by_year, aes(x = nb_year, y = prop_site, group = site_name)) +
  geom_line() +
  geom_point(size = 1.8) +
  facet_wrap(~ site_name, scales = "fixed", ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = x_breaks) +
  labs(
    x = NULL,
    y = "Proportion of tagged birds"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(
  filename = "figures/site_prop_by_year.pdf",
  plot = p,
  width = 6,
  height = 9,
  units = "in"
)
