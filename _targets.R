library(targets)

# Optional {targets} workflow for Chapter 4.
# The canonical entrypoint remains `pipeline/run_all.R`.

tar_option_set(
  packages = c(
    "dplyr", "readr", "readxl", "openxlsx", "tidyr", "purrr", "stringr",
    "ggplot2", "stopwords"
  )
)

list(
  tar_target(
    historical_context,
    {
      source("pipeline/01_build_historical_context.R")
      "outputs/data/historical_context.csv"
    },
    format = "file"
  ),
  tar_target(
    stageA,
    {
      source("pipeline/02_build_stageA_raw_only_tables.R")
      list.files("outputs/tables", pattern = "^stageA_.*\\.xlsx$", full.names = TRUE)
    },
    format = "file"
  ),
  tar_target(
    stageB,
    {
      source("pipeline/03_build_stageB_full_normalised_tables.R")
      list.files("outputs/tables", pattern = "FULL_Normalised", full.names = TRUE)
    },
    format = "file"
  ),
  tar_target(
    master_matrix,
    {
      source("pipeline/04_build_master_matrix.R")
      "outputs/data/master_matrix.csv"
    },
    format = "file"
  ),
  tar_target(
    figures,
    {
      source("pipeline/05_make_figures.R")
      list.files("outputs/figures", pattern = "\\.png$", full.names = TRUE)
    },
    format = "file"
  ),
  tar_target(
    codebook,
    {
      source("pipeline/06_make_dictionaries_and_codebook.R")
      "outputs/tables/codebook.xlsx"
    },
    format = "file"
  )
)
