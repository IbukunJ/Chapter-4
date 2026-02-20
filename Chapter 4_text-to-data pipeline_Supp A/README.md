# Chapter 4 reproducibility bundle (IGO efficacy)

This repository-style bundle is organised to reproduce the Chapter 4 tables and figures **end-to-end** on a fresh machine. All computation is performed in **R**, with a documented environment (`renv`) and a deterministic pipeline.

## What “reproducible” means here

The Chapter 4 dataset is constructed in two layers:

1. **RAW layer (Stage A)**: lexicon-based matching over verbatim institutional texts generates first-round evidence tables (keywords, KWIC excerpts, keyword frequencies, candidate category activations) and an automated **ordinal score (0–10)**.
2. **CLEAN layer (Stage B)**: a researcher adjudication step revises category activations and ordinal scores to reflect domain knowledge and implicit mandate content not reliably recoverable by dictionary matching alone. Stage B therefore treats the CLEAN category sets as the authoritative **presence/activation** layer, while retaining the RAW evidence fields for auditability.

Stage B outputs are the modelling inputs used for the conjecture tests and the thesis results.

## Directory map

- `schema/`  
  Canonical category schema (attribute families × categories), tier bonuses, and helper lookup tables used to harmonise category labels.

- `data/raw/`  
  RAW attribute-family workbooks (Institution + verbatim text) and `year_of_establishment.csv`.

- `data/clean/`  
  CLEAN adjudication workbooks (Institution + revised category set + reviewed ordinal score).

- `pipeline/`  
  Deterministic scripts that regenerate outputs into `outputs/` (tables, figures, logs, CSVs).  
  The “last mile” script `pipeline/99_finalize_repro.R` produces `renv.lock` and a `session_info.txt` after a successful run.

- `reference_outputs/`  
  Frozen reference artefacts (tables/figures) used to verify that a clean run reproduces the thesis outputs. This folder is **not** used as an input to the analysis.

- `outputs/`  
  **Generated artefacts** (tables, figures, logs, and the master analysis matrix). On a clean run, this directory is re-created by the pipeline.

- `docs/`  
  Protocol notes and consolidated documentation, including a master `codebook.xlsx`.

## Reproducing the outputs

From the repository root:

```r
source("pipeline/run_all.R")
```

This regenerates:

- `outputs/tables/` (Excel workbooks; each includes a `Data_Dictionary` sheet)
- `outputs/figures/` (PNG figures)
- `outputs/data/master_matrix.csv` (analysis-ready merged dataset)
- `outputs/tables/codebook.xlsx` (one consolidated codebook across all outputs)

### Environment snapshot (“last mile”)

After a successful run, finalise reproducibility artefacts:

```r
source("pipeline/99_finalize_repro.R")
```

This writes `docs/session_info.txt` and produces/updates `renv.lock`.

### Optional: {targets}

An optional `_targets.R` is included for a pipeline-manager workflow:

```r
targets::tar_make()
```

## Verification against reference artefacts

The folder `reference_outputs/` contains the expected tables and figures.  
A clean run should reproduce these outputs exactly (byte-for-byte) once the environment is pinned.

A simple verification helper is provided:

```r
source("pipeline/98_verify_against_reference.R")
```

The script writes a comparison report (hashes and mismatches) to `outputs/logs/`.

## Auditability of the CLEAN review layer

Stage B outputs include an `Override_Flag` field in the long-form traceability tables (`Traceability_Long`) to make the adjudication step explicit:

- `BOTH` — category supported by RAW match and retained in CLEAN
- `CLEAN_only` — introduced during adjudication
- `RAW_only` — matched in RAW but not retained after adjudication
- `NONE` — no activation

This is the main transparency mechanism linking the expert review step back to the computational evidence.
