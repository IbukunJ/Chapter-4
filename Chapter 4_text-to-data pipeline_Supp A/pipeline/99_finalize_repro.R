#!/usr/bin/env Rscript

# Finalise reproducibility artefacts:
#  - writes sessionInfo() to docs/session_info.txt
#  - snapshots package versions to renv.lock (if renv is available)

source("pipeline/00_project_options.R")

dir.create("docs", showWarnings = FALSE)

# sessionInfo
sink("docs/session_info.txt")
print(sessionInfo())
sink()

# renv snapshot (optional but recommended)
if (requireNamespace("renv", quietly = TRUE)) {
  # If the project is not yet initialised, renv::init() will create renv.lock.
  if (!file.exists("renv.lock")) {
    message("renv.lock not found; initialising renv...")
    renv::init(bare = TRUE)
  }
  message("Snapshotting package versions to renv.lock...")
  renv::snapshot(prompt = FALSE)
  message("renv snapshot complete.")
} else {
  message("Package 'renv' not installed; skipping renv snapshot. Install renv and re-run 99_finalize_repro.R")
}

message("Wrote docs/session_info.txt")
