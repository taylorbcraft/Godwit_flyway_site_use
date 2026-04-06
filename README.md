# Godwit flyway site use

Code for quantifying nonbreeding site use of tracked Black-tailed Godwits (*Limosa limosa limosa*) across the East Atlantic Flyway.

This repository identifies major nonbreeding and stopover sites from tracking data, quantifies how heavily those sites are used, and produces the figures used in the report.

## Project overview

The analysis uses tracking data from individually tagged godwits to:

- identify core nonbreeding and stopover sites
- estimate the peak 14-day use window for each site
- calculate the proportion of tracked birds using each site
- visualise seasonal and annual patterns of site use across the flyway

The main outputs are summary datasets and figures describing site importance, seasonal timing, and interannual variation in tracked-bird site use.

## Reproducibility

There are two ways to use this repository.

### Option 1: reproduce the analysis from the included data

This is the simplest route. The repository already contains:

- `data/all_locations.csv`
- derived `.rds` files used for plotting and summaries

This means most analyses and figures can be reproduced directly from the files included here.

### Option 2: refresh the raw tracking data from Movebank

The script `scripts/1.import_location_data.R` downloads and combines tracking data from Movebank.

This step requires:

- access to the relevant Movebank studies
- valid Movebank credentials
- the `move2` package and its dependencies

If you do not have Movebank access, skip this step and use the included data files instead.

## Setup

To restore the project library with `renv`:

`install.packages("renv")`  
`renv::restore()`

## Script order

The scripts are intended to be run in the following order.

### Data import

`1.import_location_data.R` downloads tracking data and deployment metadata from Movebank, combines studies, and writes a unified location dataset.

Run this only if you want to refresh `data/all_locations.csv` from Movebank.

### Core analysis

`2.main_godwit_nb_site_use.R` processes tracking data, identifies sites, calculates site-use metrics, and saves the derived `.rds` objects used by the plotting scripts.

Main outputs include:

- `data/loc_sf.rds`
- `data/presence_days.rds`
- `data/site_peak_windows_by_year.rds`
- `data/site_prop_by_year.rds`
- `data/site_summary.rds`

### Figure scripts

`3.figure_tagging_effort.R` plots annual tagging effort, including new deployments and active tags.

`4.figure_tagging_site_map.R` creates a map of tagging locations.

`5.figure_example_migration_map.R` creates a flyway map with example migration routes from representative individuals.

`6.figure_core_sites_flyway_map.R` creates the main flyway map of core nonbreeding and stopover sites.

`7.figure_ridge_plot.R` plots seasonal site-use profiles across the nonbreeding cycle.

`8.figure_line_graph_site_use_by_year.R` plots interannual variation in proportional site use for each site.

`9.figure_iberia_wafrica_infographic.R` creates regional maps for Iberia and West Africa.

## Minimal workflow

If you want to reproduce the main outputs using the included files, run:

1. `2.main_godwit_nb_site_use.R`
2. `3.figure_tagging_effort.R`
3. `4.figure_tagging_site_map.R`
4. `5.figure_example_migration_map.R`
5. `6.figure_core_sites_flyway_map.R`
6. `7.figure_ridge_plot.R`
7. `8.figure_line_graph_site_use_by_year.R`
8. `9.figure_iberia_wafrica_infographic.R`

If the derived `.rds` files are already present and up to date, the plotting scripts can also be run individually.

## Last 2 years workflow

A parallel set of files is available under:

- `data/last_2_years/`
- `scripts/last_2_years/`
- `figures/last_2_years/`

These are used for repeating a similar workflow restricted to the two most recent years of tracking data.

## Outputs

The repository produces two main types of outputs.

### Derived data

Intermediate `.rds` files in `data/` that store processed site-use summaries and spatial objects.

### Figures

Final figures written to `figures/`, including:

- tagging effort
- tagging site map
- example migration map
- flyway core site map
- seasonal ridge plot
- annual site-use panels
- Iberia and West Africa regional maps

## Notes

- Some scripts depend on `data/all_locations.csv`, while others depend on the derived `.rds` outputs from the main analysis script.
- The repository also contains report files in `documents/`.
- Temporary Office files beginning with `~$` can be ignored.
