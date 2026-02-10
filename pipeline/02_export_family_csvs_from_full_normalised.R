# 02_export_family_csvs_from_full_normalised.R
# ------------------------------------------------------------------------------
# Standardises the FULL_Normalised matrices (Matrix_Wide) into analysis-ready CSVs
# with consistent snake_case names and explicit suffixes:
#   *_within_igo, *_across_igo
#
# Inputs:
#   data/processed/full_normalised/*.xlsx  (Matrix_Wide sheets)
#
# Outputs:
#   Data/<family>_data.csv  (one per attribute family)
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(janitor)
  library(purrr)
  library(readr)
})

# Helper: load and standardise one Matrix_Wide sheet
load_matrix_wide <- function(path, family_slug) {
  df <- read_excel(path, sheet = "Matrix_Wide") %>%
    rename(institution = Institution) %>%
    clean_names()

  # clean_names() yields names like: enclosed_or_semi_enclosed_sea_withinigo
  # Standardise suffixes to *_within_igo / *_across_igo for downstream scripts.
  df <- df %>%
    rename_with(~ str_replace_all(.x, "withinigo$", "within_igo")) %>%
    rename_with(~ str_replace_all(.x, "acrossigo$", "across_igo"))

  # Ordinal score
  if ("ordinal_score" %in% names(df)) {
    df <- df %>% rename(!!paste0("ordinal_score_", family_slug) := ordinal_score)
  }

  df
}

# File map: edit names here if files are renamed
files <- tribble(
  ~family_slug, ~path,
  "spatial_jurisdiction", "data/processed/full_normalised/Spatial_Jurisdiction_FULL_Normalised_REAL.xlsx",
  "subject_matter", "data/processed/full_normalised/Subject_Matter_Jurisdiction_FULL_Normalised_REAL REAL.xlsx",
  "sources", "data/processed/full_normalised/Sources_of_Jurisdiction_FULL_Normalised_REAL.xlsx",
  "defined_objectives", "data/processed/full_normalised/Defined_Objectives_FULL_Normalised_REAL.xlsx",
  "strategies", "data/processed/full_normalised/Strategies_FULL_Normalised_REAL.xlsx",
  "relationships", "data/processed/full_normalised/Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx",
  "vertical", "data/processed/full_normalised/Vertical_Coordination_FULL_Normalised.xlsx",
  "horizontal", "data/processed/full_normalised/Horizontal_Coordination_FULL_Normalised_REAL.xlsx"
)

walk2(files$path, files$family_slug, function(p, slug) {
  df <- load_matrix_wide(p, slug)
  write_csv(df, file.path("Data", paste0(slug, "_data.csv")))
})

message("Family CSVs exported to Data/.")
