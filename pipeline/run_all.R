# run_all.R
# ------------------------------------------------------------------------------
# Convenience wrapper to regenerate the Chapter 4 analysis artefacts.
# ------------------------------------------------------------------------------

source('pipeline/00_project_options.R')
source('pipeline/01_build_historical_context.R')

# Stage A (RAW-only; first normalisation)
source('pipeline/02_build_stageA_raw_only_tables.R')

# Stage B (FULL; CLEAN-integrated normalisation)
source('pipeline/03_build_stageB_full_normalised_tables.R')

# Master analysis matrix + figures + dictionaries
source('pipeline/04_build_master_matrix.R')
source('pipeline/05_make_figures.R')
source('pipeline/06_make_dictionaries_and_codebook.R')

# Optional: render narrative reports/notebooks (kept for convenience)
if (file.exists('pipeline/04_render_reports.R')) {
  source('pipeline/04_render_reports.R')
}
