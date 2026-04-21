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

### Data import
The required tracking data is not included in this repository due to size limitations.

The script `scripts/01_import_location_data.R` downloads and combines tracking data from Movebank.

This step requires:

- access to the relevant Movebank studies
- valid Movebank credentials
- the `move2` package and its dependencies

If you already have the tracking data through another source, you can skip this step.

## Setup

To restore the project library (in case packages update or change) with `renv`:

`install.packages("renv")`  
`renv::restore()`

## Run the Workflow

To reproduce the full workflow from `data/all_locations.csv`, run:

`Rscript run_workflow.R`

This runs the full-period analysis, full-period figures, recent two-year analysis, and recent two-year figures in order.

The runner does not import data from Movebank. It assumes `data/all_locations.csv` already exists and has the required columns.

When it starts, `run_workflow.R` prints the expected input file and the required column names.

To refresh the Movebank data, run the import script separately:

`Rscript scripts/01_import_location_data.R`

This requires Movebank access, valid Movebank credentials, and the `move2` package.

## Script Order

The scripts are intended to be run in the following order.

### Data import

`01_import_location_data.R` downloads tracking data and deployment metadata from Movebank, combines studies, and writes a unified location dataset.

Run this only if you want to refresh `data/all_locations.csv` from Movebank.

### Core analysis

`02_analyze_nonbreeding_site_use.R` processes tracking data, identifies sites, calculates site-use metrics, and saves the derived `.rds` objects used by the plotting scripts.

Main outputs include:

- `data/site_location_fixes.rds`
- `data/site_presence_days.rds`
- `data/site_peak_windows_by_year.rds`
- `data/site_use_by_year.rds`
- `data/site_summary_points.rds`

### Figure scripts

`03_plot_tagging_effort.R` plots annual tagging effort, including new deployments and active tags.

`04_plot_tagging_site_map.R` creates a map of tagging locations.

`05_plot_example_migration_map.R` creates a flyway map with example migration routes from representative individuals.

`06_plot_core_sites_flyway_map.R` creates the main flyway map of core nonbreeding and stopover sites.

`07_plot_ridge_plot.R` plots seasonal site-use profiles across the nonbreeding cycle.

`08_plot_line_graph_site_use_by_year.R` plots interannual variation in proportional site use for each site.

`09_plot_regional_infographic.R` creates regional maps for Iberia and West Africa.

## Manual Main Workflow

If you want to run the main scripts manually instead of using `run_workflow.R`, run:

1. `scripts/02_analyze_nonbreeding_site_use.R`
2. `scripts/03_plot_tagging_effort.R`
3. `scripts/04_plot_tagging_site_map.R`
4. `scripts/05_plot_example_migration_map.R`
5. `scripts/06_plot_core_sites_flyway_map.R`
6. `scripts/07_plot_ridge_plot.R`
7. `scripts/08_plot_line_graph_site_use_by_year.R`
8. `scripts/09_plot_regional_infographic.R`

If the derived `.rds` files are already present and up to date, the plotting scripts can also be run individually.

## Recent Two-Year Workflow

A parallel set of files is available under:

- `data/recent_2_years/`
- `scripts/recent_2_years/`
- `figures/recent_2_years/`

These are used for repeating a similar workflow restricted to the two most recent nonbreeding cycles in the tracking data.

## Outputs
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
- The repository also contains report files in `documents/`.
