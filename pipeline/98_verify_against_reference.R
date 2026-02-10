# pipeline/98_verify_against_reference.R
# Compare generated outputs/ against frozen reference_outputs/ (hash-based).
# Writes a report to outputs/logs/verify_against_reference.csv

suppressPackageStartupMessages({
  library(digest)
  library(dplyr)
  library(readr)
  library(fs)
  library(stringr)
})

repo_root <- normalizePath(".", winslash = "/", mustWork = TRUE)

out_dir <- file.path(repo_root, "outputs")
ref_dir <- file.path(repo_root, "reference_outputs")
log_dir <- file.path(out_dir, "logs")
dir_create(log_dir)

# Helper to list files under a directory (relative paths)
list_files_rel <- function(root) {
  files <- dir_ls(root, recurse = TRUE, type = "file")
  # drop any .gitkeep
  files <- files[!str_detect(files, "\\.gitkeep$")]
  rel <- str_replace(files, paste0("^", fixed(root), "/?"), "")
  tibble(rel_path = rel, abs_path = files)
}

hash_file <- function(path) {
  # SHA-256 for stable cross-platform checks
  digest(file = path, algo = "sha256")
}

ref_files <- list_files_rel(ref_dir)
out_files <- list_files_rel(out_dir)

# Only compare files that exist in reference_outputs
cmp <- ref_files %>%
  left_join(out_files, by = "rel_path", suffix = c("_ref", "_out")) %>%
  mutate(
    exists_out = !is.na(abs_path_out),
    hash_ref = purrr::map_chr(abs_path_ref, hash_file),
    hash_out = ifelse(exists_out, purrr::map_chr(abs_path_out, hash_file), NA_character_),
    match = exists_out & (hash_ref == hash_out)
  )

report_path <- file.path(log_dir, "verify_against_reference.csv")
write_csv(cmp, report_path)

message("Verification report written to: ", report_path)
message("Summary:")
message("  Reference files: ", nrow(cmp))
message("  Missing outputs: ", sum(!cmp$exists_out))
message("  Hash matches:    ", sum(cmp$match))
message("  Hash mismatches: ", sum(cmp$exists_out & !cmp$match))
