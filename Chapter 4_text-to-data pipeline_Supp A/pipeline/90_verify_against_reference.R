#!/usr/bin/env Rscript

# Optional verification: compares freshly generated outputs/ against the
# bundled reference_outputs/ copies.
#
# This is intended as a regression test to detect accidental changes in
# the pipeline once the thesis outputs are final.

source("pipeline/00_project_options.R")
source("R/utils.R")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

ref_root <- "reference_outputs"
stopifnot(dir.exists(ref_root))

out_map <- family_output_map()

tol <- 1e-6

compare_matrix_wide <- function(file_name) {
  f_new <- file.path("outputs/tables", file_name)
  f_ref <- file.path(ref_root, "tables", file_name)
  if (!file.exists(f_new) || !file.exists(f_ref)) {
    message("Missing file for comparison: ", file_name)
    return(FALSE)
  }
  new <- readxl::read_excel(f_new, sheet = "Matrix_Wide")
  ref <- readxl::read_excel(f_ref, sheet = "Matrix_Wide")
  # align columns
  common_cols <- intersect(names(new), names(ref))
  new <- new[, common_cols]
  ref <- ref[, common_cols]
  # numeric diffs
  num_cols <- common_cols[vapply(new, is.numeric, logical(1))]
  if (length(num_cols) == 0) return(TRUE)
  d <- max(abs(as.matrix(new[, num_cols]) - as.matrix(ref[, num_cols])), na.rm = TRUE)
  if (is.na(d)) d <- 0
  ok <- d <= tol
  if (!ok) message("Matrix_Wide differs for ", file_name, "; max abs diff = ", signif(d, 6))
  ok
}

ok_vec <- vapply(unname(out_map), compare_matrix_wide, logical(1))

if (all(ok_vec)) {
  message("Verification passed (Matrix_Wide within tolerance).")
} else {
  stop("Verification failed for one or more families. See messages above.", call. = FALSE)
}
