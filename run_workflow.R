# Run the godwit flyway site-use workflow.
#
# This runner assumes the input tracking data already exist at:
#   data/all_locations.csv
#
# To refresh that file from Movebank, run the import script separately:
#   Rscript scripts/01_import_location_data.R
#
# To run the analysis and plotting workflow:
#   Rscript run_workflow.R

get_script_path <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) {
    return(NULL)
  }

  normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = FALSE)
}

script_path <- get_script_path()
repo_root <- if (is.null(script_path)) normalizePath(getwd(), mustWork = TRUE) else dirname(script_path)
setwd(repo_root)

renv_activate <- file.path(repo_root, "renv", "activate.R")
if (file.exists(renv_activate) && !nzchar(Sys.getenv("RENV_PROJECT"))) {
  source(renv_activate)
}

dir.create("data", showWarnings = FALSE)
dir.create("data/recent_2_years", recursive = TRUE, showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)
dir.create("figures/recent_2_years", recursive = TRUE, showWarnings = FALSE)

input_path <- "data/all_locations.csv"
required_columns <- c(
  "individual_local_identifier",
  "X",
  "Y",
  "timestamp",
  "study_site",
  "study_name",
  "ground_speed",
  "deploy_on_timestamp"
)

message("Expected input file: ", input_path)
message("Required columns: ", paste(required_columns, collapse = ", "))

if (!file.exists(input_path)) {
  stop("Missing required input file `", input_path, "`.", call. = FALSE)
}

run_script <- function(path) {
  message("\n==> Running ", path)
  source(path, local = new.env(parent = globalenv()))
}

main_scripts <- c(
  "scripts/02_analyze_nonbreeding_site_use.R",
  "scripts/03_plot_tagging_effort.R",
  "scripts/04_plot_tagging_site_map.R",
  "scripts/05_plot_example_migration_map.R",
  "scripts/06_plot_core_sites_flyway_map.R",
  "scripts/07_plot_ridge_plot.R",
  "scripts/08_plot_line_graph_site_use_by_year.R",
  "scripts/09_plot_regional_infographic.R"
)

recent_scripts <- c(
  "scripts/recent_2_years/01_analyze_nonbreeding_site_use_recent_2_years.R",
  "scripts/recent_2_years/02_plot_core_sites_flyway_map_recent_2_years.R",
  "scripts/recent_2_years/03_plot_ridge_plot_recent_2_years.R",
  "scripts/recent_2_years/04_plot_regional_infographic_recent_2_years.R"
)

invisible(lapply(main_scripts, run_script))
invisible(lapply(recent_scripts, run_script))

message("\nWorkflow complete.")
