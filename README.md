# Godwit flyway site use

Analysis scripts for quantifying nonbreeding site use of tracked Black-tailed Godwits (*Limosa limosa*) across the East Atlantic flyway.

This repository contains the code used to identify major nonbreeding and stopover sites from GPS tracking data and to quantify seasonal patterns of site use across the flyway.

---

## Overview

This project uses tracking data from individually tagged godwits to:

- identify clusters of frequently used nonbreeding and stopover sites
- estimate peak seasonal use periods at each site
- calculate the proportion of tracked birds using each site
- visualise seasonal site-use patterns and flyway connectivity

The analysis summarises how tracked birds use key sites across multiple migratory cycles and highlights periods of concentrated site use.

---

## Repository structure

```
scripts/
  analysis scripts for site identification and seasonal site-use metrics

figures/
  scripts used to generate maps and figures

data/
  derived data objects used in the analysis (RDS files)

```
---

## Important note on data

A separate folder within the scripts, figures, and data folders (last_2_years/) is available for running similar analyses on the 2 most recent years of tracking data.

Raw tracking data are not included in this repository.

The original dataset (`all_locations.csv`) is excluded due to file size and data ownership restrictions.

All analyses in the repository rely on derived datasets stored as `.rds` files, which contain processed summaries used for figure generation and analysis.

---

## Main analysis outputs

The analysis produces several key outputs:

- Peak 14-day use windows for each nonbreeding site
- Proportional site use by tracked birds in each migratory cycle
- Seasonal site-use profiles across the nonbreeding period
- Flyway maps showing core sites and representative migration routes

Figures produced by the repository include:

- flyway overview maps  
- tagging site locations  
- seasonal ridgeline plots of site use  
- interannual trends in proportional site use  

---

## Workflow

The main analytical workflow is:

1. Clean and filter tracking data  
2. Identify site clusters using spatial clustering  
3. Calculate daily presence of birds at each site  
4. Estimate peak seasonal use windows  
5. Calculate proportional site use across years  
6. Generate figures and maps  

Scripts are organised so that figures can be reproduced directly from the processed datasets.


## Setup

To reproduce the analysis environment (in case libraries change), run:

```r
install.packages("renv")
renv::restore()
