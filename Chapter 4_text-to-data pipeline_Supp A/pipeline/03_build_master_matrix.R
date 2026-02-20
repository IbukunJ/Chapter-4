# 03_build_master_matrix.R
# ------------------------------------------------------------------------------
# Merges the per-family Data/*.csv files into one analysis matrix.
#
# Output:
#   Data/master_matrix.csv
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
})

year_df <- read_csv("Data/year_data.csv", show_col_types = FALSE)

family_files <- list.files("Data", pattern = "_data\.csv$", full.names = TRUE)
family_files <- family_files[!grepl("year_data\.csv$", family_files)]

family_dfs <- map(family_files, ~ read_csv(.x, show_col_types = FALSE))

master <- reduce(c(list(year_df), family_dfs), function(x, y) full_join(x, y, by = "institution"))

write_csv(master, "Data/master_matrix.csv")
message("Master matrix written to Data/master_matrix.csv")
