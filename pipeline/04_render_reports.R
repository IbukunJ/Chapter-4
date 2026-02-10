# 04_render_reports.R
# ------------------------------------------------------------------------------
# Renders the analysis notebooks (Rmd) into HTML reports under outputs/reports/.
# This reproduces the chapter tables/figures (subject to local package versions).
#
# NOTE: Ensure Data/*.csv have been generated first by running:
#   source('pipeline/00_project_options.R')
#   source('pipeline/01_build_historical_context.R')
#   source('pipeline/02_export_family_csvs_from_full_normalised.R')
#   source('pipeline/03_build_master_matrix.R')
#
# Then run:
#   source('pipeline/04_render_reports.R')
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(rmarkdown)
})

rmds <- c(
  "notebooks/data_prep.Rmd",
  "notebooks/c_1.Rmd",
  "notebooks/c_2.Rmd",
  "notebooks/c_3.Rmd",
  "notebooks/final_plots.Rmd"
)

for (rmd in rmds) {
  render(
    input = rmd,
    output_dir = "outputs/reports",
    quiet = FALSE
  )
}

message("Reports rendered to outputs/reports/.")
