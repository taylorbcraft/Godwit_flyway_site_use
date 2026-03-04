data_raw <- read.csv("data/all_locations.csv", stringsAsFactors = FALSE)

# packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(forcats)
library(scales)

# ------------------------------------------------------------
# data: tagging effort by year
# ------------------------------------------------------------
tag_effort_year <- data_raw %>%
  mutate(
    deploy_on_date = ymd(deploy_on_timestamp, quiet = TRUE),
    deploy_year = year(deploy_on_date)
  ) %>%
  filter(!is.na(deploy_year)) %>%
  filter(deploy_year >= 2010, deploy_year <= year(Sys.Date())) %>%
  distinct(individual_local_identifier, deploy_year) %>%
  count(deploy_year, name = "n_tagged") %>%
  arrange(deploy_year)

# ------------------------------------------------------------
# plot: infographic aesthetic
# ------------------------------------------------------------
p_tag_effort <- ggplot(tag_effort_year, aes(x = n_tagged, y = fct_rev(factor(deploy_year)))) +
  geom_col(width = 0.72, fill = "#62C7D6", colour = NA) +
  geom_text(aes(label = comma(n_tagged)), hjust = -0.15, size = 3.4, colour = "grey20") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "TAGGING EFFORT OVER TIME",
    subtitle = "Number of newly tagged individuals per year",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title = element_text(size = 26, colour = "#6F9CB5", face = "plain", margin = margin(b = 8)),
    plot.subtitle = element_text(size = 12, colour = "grey35", margin = margin(b = 18)),
    axis.text.y = element_text(size = 11, colour = "grey25"),
    plot.caption = element_text(size = 9, colour = "grey45", hjust = 0, margin = margin(t = 10)),
    plot.margin = margin(18, 18, 18, 18),
    legend.position = "none"
  )

p_tag_effort

ggsave(p_tag_effort,
       file = "figures/tag_effort.pdf",
       width = 6,
       height = 3.5)
