# pipeline/06_make_dictionaries_and_codebook.R
# - Inject a Data_Dictionary sheet into each output workbook in outputs/tables/
# - Build a consolidated master codebook.xlsx across all output workbooks

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(openxlsx)
  library(fs)
})

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

out_tables <- file.path(repo_root, "outputs", "tables")
out_dicts  <- file.path(repo_root, "outputs", "dictionaries")
out_codebook <- file.path(repo_root, "outputs", "tables", "codebook.xlsx")
docs_codebook <- file.path(repo_root, "docs", "codebook.xlsx")

schema_field_dict <- file.path(repo_root, "schema", "field_dictionary.csv")
field_dict <- if (file_exists(schema_field_dict)) {
  read_csv(schema_field_dict, show_col_types = FALSE)
} else {
  tibble(field = character(), description = character())
}

dir_create(out_dicts)

# Helper: build per-workbook dictionary data frame with descriptions
build_dictionary_df <- function(wb_path) {
  # read sheet names and take column headers for each sheet
  sheets <- openxlsx::getSheetNames(wb_path)

  # Keep only “data” sheets (skip Data_Dictionary itself if present)
  data_sheets <- sheets[!tolower(sheets) %in% c("data_dictionary")]

  dict_list <- map(data_sheets, function(s) {
    df <- openxlsx::read.xlsx(wb_path, sheet = s, rows = 1, cols = 1:400, colNames = TRUE)
    vars <- names(df)
    tibble(
      sheet = s,
      variable = vars
    )
  })

  dict_df <- bind_rows(dict_list) %>%
    distinct() %>%
    mutate(description = NA_character_) %>%
    left_join(field_dict, by = c("variable" = "field")) %>%
    mutate(description = coalesce(description, description.y)) %>%
    select(sheet, variable, description)

  # Pattern-based fill for category score columns
  dict_df <- dict_df %>%
    mutate(description = case_when(
      is.na(description) & str_detect(variable, " \\(WithinIGO\\)$") ~
        paste0("Within-IGO normalised hybrid score (0–10) for category: ",
               str_remove(variable, " \\(WithinIGO\\)$")),
      is.na(description) & str_detect(variable, " \\(AcrossIGO\\)$") ~
        paste0("Across-IGO normalised hybrid score (0–10) for category: ",
               str_remove(variable, " \\(AcrossIGO\\)$")),
      TRUE ~ description
    ))

  dict_df
}

# Inject Data_Dictionary into each workbook
xlsx_files <- dir_ls(out_tables, recurse = FALSE, glob = "*.xlsx")
xlsx_files <- xlsx_files[!str_detect(basename(xlsx_files), "^codebook\\.xlsx$")]

for (wb_path in xlsx_files) {
  dict_df <- build_dictionary_df(wb_path)

  # Save a standalone dictionary workbook too
  dict_book_path <- file.path(out_dicts, paste0(path_ext_remove(basename(wb_path)), "_dictionary.xlsx"))
  wb_dict <- createWorkbook()
  addWorksheet(wb_dict, "Data_Dictionary")
  writeData(wb_dict, "Data_Dictionary", dict_df)
  saveWorkbook(wb_dict, dict_book_path, overwrite = TRUE)

  # Inject/replace in the data workbook
  wb <- loadWorkbook(wb_path)

  if ("Data_Dictionary" %in% names(wb)) {
    removeWorksheet(wb, "Data_Dictionary")
  }
  addWorksheet(wb, "Data_Dictionary")
  writeData(wb, "Data_Dictionary", dict_df)

  saveWorkbook(wb, wb_path, overwrite = TRUE)
}

# Build consolidated master codebook across ALL output workbooks
codebook_rows <- map_dfr(xlsx_files, function(wb_path) {
  # read the injected Data_Dictionary
  dd <- tryCatch({
    openxlsx::read.xlsx(wb_path, sheet = "Data_Dictionary")
  }, error = function(e) {
    tibble(sheet = character(), variable = character(), description = character())
  })

  dd %>%
    mutate(
      workbook = basename(wb_path)
    ) %>%
    select(workbook, sheet, variable, description)
})

wb_cb <- createWorkbook()
addWorksheet(wb_cb, "README")
writeData(wb_cb, "README", tibble(
  note = c(
    "Master codebook for Chapter 4 outputs.",
    "Compiled from Data_Dictionary sheets injected into each workbook under outputs/tables/."
  )
))

addWorksheet(wb_cb, "Variables")
writeData(wb_cb, "Variables", codebook_rows)

addWorksheet(wb_cb, "Files")
writeData(wb_cb, "Files", tibble(
  workbook = basename(xlsx_files),
  rel_path = file.path("outputs", "tables", basename(xlsx_files))
))

saveWorkbook(wb_cb, out_codebook, overwrite = TRUE)

# Also write a copy to docs/ for narrative referencing
dir_create(dirname(docs_codebook))
file_copy(out_codebook, docs_codebook, overwrite = TRUE)

message("Injected per-workbook data dictionaries and wrote master codebook:")
message("  ", out_codebook)
message("  ", docs_codebook)
