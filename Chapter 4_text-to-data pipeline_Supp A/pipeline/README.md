# Chapter 4 text-to-data reproducible pipeline (R)

This pipeline is designed to **regenerate all chapter 4's text-to-data outputs** (tables, figures, logs, and dictionaries) from the inputs in `data/raw/` and `data/clean/` on a clean machine.

Two processing stages are supported:

1. **Stage A (RAW-only / first normalisation)**: lexicon matching and scoring on RAW texts only. This reproduces the *first-round* attribute tables that were later reviewed to produce the CLEAN layer.
2. **Stage B (FULL / CLEAN-integrated normalisation)**: uses CLEAN category sets (column 2 in each CLEAN workbook) as authoritative presence/activation, while retaining RAW evidence fields for auditability. This produces the analysis-ready matrices used in the thesis.

Both stages write their outputs to `outputs/`.

## Quick start

From the repository root:

```r
source("pipeline/run_all.R")
```

On first run, initialise and snapshot the environment:

```r
source("pipeline/00_project_options.R")
source("pipeline/99_finalize_repro.R")
```

## Script order

0. `00_project_options.R`

   Sets reproducibility options and creates output directories.

1. `01_build_historical_context.R`

   Computes `founding_density_5yr` and `cumulative_stock` (and related variables) from `data/raw/Year_of_Establishment_with_Categories_and_Density.xlsx`.

2. `02_build_stageA_raw_only_tables.R`

   Builds **Stage A (RAW-only)** evidence-linked long tables and wide matrices for each attribute family.

3. `03_build_stageB_full_normalised_tables.R`

   Builds **Stage B (FULL)** evidence-linked long tables and wide matrices by integrating CLEAN category sets and ordinal scores.

4. `04_build_master_matrix.R`

   Merges all family matrices and historical context covariates into `outputs/data/master_matrix.csv`.

5. `05_make_figures.R`

   Regenerates the figures used in Chapter 4 (stored in `outputs/figures/`).

6. `06_make_dictionaries_and_codebook.R`

   Writes a **Data_Dictionary worksheet** into every Excel output workbook in `outputs/tables/` and generates the consolidated `outputs/tables/codebook.xlsx`.

7. `99_finalize_repro.R`

   Writes `docs/session_info.txt` and snapshots the environment to `renv.lock`.

## Outputs

* `outputs/tables/` — Excel workbooks, each with a `Data_Dictionary` sheet.
* `outputs/data/` — analysis-ready CSV files.
* `outputs/figures/` — figures (PNG).
* `outputs/logs/` — pipeline logs.
* `outputs/tables/codebook.xlsx` — consolidated master codebook across all outputs.

## Notes on auditability (AUTO vs CLEAN)

Stage B retains an `Override_Flag` field in long-form traceability tables to document whether category activations are:

* `BOTH` (RAW match and CLEAN)
* `CLEAN_only` (introduced during adjudication)
* `RAW_only` (RAW match not retained after adjudication)
* `NONE` (no activation)

This is the primary transparency device for the CLEAN review step.
