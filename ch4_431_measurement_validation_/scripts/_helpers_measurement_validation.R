# Helper functions for §4.3.1 measurement validation
# (Keep deterministic; no random seeds unless explicitly set.)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(openxlsx)
})

minmax_0_10 <- function(x, eps = 1e-9) {
  rng <- range(x, na.rm = TRUE)
  10 * (x - rng[1]) / (rng[2] - rng[1] + eps)
}

z_standardise <- function(x) {
  as.numeric(scale(x))
}

shannon_diversity_0_10 <- function(df, suffix = "_WithinIGO", eps = 1e-9) {
  cols <- names(df)[stringr::str_ends(names(df), stringr::fixed(suffix))]
  stopifnot(length(cols) > 1)

  Z <- as.matrix(df[, cols])
  denom <- rowSums(Z, na.rm = TRUE) + eps
  p <- sweep(Z, 1, denom, "/")
  p[p <= 0] <- eps

  H <- -rowSums(p * log(p), na.rm = TRUE)
  H_norm <- H / log(length(cols))
  10 * H_norm
}

binary_presence_from_scores <- function(df, suffix = "_AcrossIGO", threshold = 0) {
  cols <- names(df)[stringr::str_ends(names(df), stringr::fixed(suffix))]
  X <- as.matrix(df[, cols])
  (X > threshold) * 1L
}

write_xlsx_with_dictionary <- function(path, data_sheets, dictionary_df) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(data_sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, data_sheets[[nm]])
    openxlsx::setColWidths(wb, nm, cols = 1:ncol(data_sheets[[nm]]), widths = "auto")
  }
  openxlsx::addWorksheet(wb, "Data_Dictionary")
  openxlsx::writeData(wb, "Data_Dictionary", dictionary_df)
  openxlsx::setColWidths(wb, "Data_Dictionary", cols = 1:ncol(dictionary_df), widths = "auto")
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}