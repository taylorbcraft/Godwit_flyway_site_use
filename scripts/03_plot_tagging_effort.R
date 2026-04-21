# ------------------------------------------------------------
# Tagging effort over time
#
# Purpose: Summarize annual tagging effort and create a figure
#          showing new deployments and active tags per year
#
# Input:
#   - data/all_locations.csv
#
# Output:
#   - figures/tag_effort.pdf
# ------------------------------------------------------------

# packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(scales)
library(tidyr)

# -------------------------------
# read input
# -------------------------------
location_data_raw <- read.csv("data/all_locations.csv", stringsAsFactors = FALSE)

# ------------------------------------------------------------
# data: deployments (new tags) by year
# ------------------------------------------------------------
deployments_year <- location_data_raw %>%
  mutate(
    deploy_on_date = ymd(deploy_on_timestamp, quiet = TRUE),
    deploy_year = year(deploy_on_date)
  ) %>%
  filter(!is.na(deploy_year)) %>%
  distinct(individual_local_identifier, deploy_year) %>%
  count(deploy_year, name = "n_new_deployments")

# ------------------------------------------------------------
# data: active tags by year
# definition: individual has >=1 location in that calendar year
# ------------------------------------------------------------
active_year <- location_data_raw %>%
  mutate(
    ts = {
      parsed_datetime <- ymd_hms(timestamp, tz = "UTC", quiet = TRUE)
      parsed_date <- as.POSIXct(ymd(timestamp, quiet = TRUE), tz = "UTC")
      coalesce(parsed_datetime, parsed_date)
    }
  ) %>%
  filter(!is.na(ts)) %>%
  mutate(year = year(ts)) %>%
  distinct(individual_local_identifier, year) %>%
  count(year, name = "n_active_tags") %>%
  rename(deploy_year = year)

# ------------------------------------------------------------
# combine and reshape for side-by-side bars
# ------------------------------------------------------------
year_min <- 2010
year_max <- 2025

tag_effort_year <- full_join(deployments_year, active_year, by = "deploy_year") %>%
  filter(deploy_year >= year_min, deploy_year <= year_max) %>%
  mutate(
    n_new_deployments = coalesce(n_new_deployments, 0L),
    n_active_tags = coalesce(n_active_tags, 0L)
  ) %>%
  arrange(deploy_year) %>%
  pivot_longer(
    cols = c(n_new_deployments, n_active_tags),
    names_to = "metric",
    values_to = "n"
  ) %>%
  mutate(
    metric = recode(
      metric,
      n_new_deployments = "New deployments",
      n_active_tags = "Active tags"
    ),
    year_f = fct_rev(factor(deploy_year))
  )

# ------------------------------------------------------------
# plot: infographic aesthetic, side-by-side bars
# ------------------------------------------------------------
fill_cols <- c(
  "New deployments" = "#62C7D6",
  "Active tags" = "#6F9CB5"
)

p_tag_effort <- ggplot(tag_effort_year, aes(x = n, y = year_f, fill = metric)) +
  geom_col(
    width = 0.72,
    position = position_dodge(width = 0.78),
    colour = NA
  ) +
  geom_text(
    aes(label = ifelse(n > 0, comma(n), "")),
    position = position_dodge(width = 0.78),
    hjust = -0.15,
    size = 3.2,
    colour = "grey20"
  ) +
  scale_fill_manual(values = fill_cols) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.22))) +
  labs(
    title = "TAGGING EFFORT OVER TIME",
    subtitle = "New deployments vs. number of active tags per year",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(size = 26, colour = "#6F9CB5", face = "plain", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, colour = "grey35", margin = margin(b = 18)),
    axis.text.y = element_text(size = 11, colour = "grey25"),
    plot.caption = element_text(size = 9, colour = "grey45", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(18, 18, 18, 18),
    legend.position = "top",
    legend.text = element_text(size = 10, colour = "grey25")
  )

p_tag_effort

ggsave(p_tag_effort,
       file = "figures/tag_effort.pdf",
       width = 6,
       height = 6)
