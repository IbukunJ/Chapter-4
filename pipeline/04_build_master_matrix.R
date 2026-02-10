#!/usr/bin/env Rscript

# Builds an analysis-ready master matrix by merging:
#  - Stage B family matrices written to outputs/data/<family>_matrix_wide.csv
#  - Historical context covariates (outputs/data/historical_context.csv)
#
# Output:
#  - outputs/data/master_matrix.csv

source("pipeline/00_project_options.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

hist <- read_csv("outputs/data/historical_context.csv", show_col_types = FALSE) |>
  rename(Institution = institution) |>
  mutate(Institution = as.character(Institution))

family_files <- list.files("outputs/data", pattern = "_matrix_wide\\.csv$", full.names = TRUE)
family_files <- family_files[!grepl("stageA_", basename(family_files))]

family_dfs <- map(family_files, ~ read_csv(.x, show_col_types = FALSE) |> mutate(Institution = as.character(Institution)))

master <- reduce(c(list(hist), family_dfs), function(x, y) full_join(x, y, by = "Institution"))

write_csv(master, "outputs/data/master_matrix.csv")
message("Master matrix written to outputs/data/master_matrix.csv")
