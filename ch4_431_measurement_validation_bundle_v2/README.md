# Chapter 4 — §4.3.1 Measurement validation (reproducibility bundle)

This bundle produces the measurement-validation outputs for Chapter 4 §4.3.1, including:
- factorability checks (KMO, Bartlett),
- parallel analysis figure,
- exploratory factor analysis (EFA) loadings and interpretation,
- confirmatory factor analysis (CFA) for LEF, with fit, loadings, and reliability diagnostics,
- exported construct scores (EEI composite, Adaptive Capacity composite, LEF factor scores).

## Inputs
`data/inputs/full_normalised/` contains the eight FULL-normalised attribute workbooks (`*_FULL_Normalised_*.xlsx`), each of which includes:
- `Matrix_Wide` (IGO × category scores; ordinal and 0–10 normalised),
- `Traceability_Long` (keyword triggers and KWIC evidence).

`data/inputs/year/` contains the year-of-establishment table used to compute age and ecology covariates.

## How to run
From the repository root (the folder that contains `scripts/`), run:

```r
source("scripts/07_measurement_validation.R")
```

or from a terminal:

```bash
Rscript scripts/07_measurement_validation.R
```

The script writes tables to `outputs/tables/` and figures to `outputs/figures/`.
Each Excel output includes a `Data_Dictionary` sheet.

## Outputs (filenames)
- `outputs/tables/Table_4E1_Factorability.xlsx`
- `outputs/figures/Fig_4E1_ParallelAnalysis.png`
- `outputs/tables/Table_4E2_EFA_Loadings.xlsx`
- `outputs/tables/Table_4E3_EFA_FactorInterpretation.xlsx`
- `outputs/tables/Table_4E4_CFA_Fit_LEF.xlsx`
- `outputs/tables/Table_4E5_CFA_Loadings_LEF.xlsx`
- `outputs/tables/Table_4E6_Reliability_AVE_LEF.xlsx`
- `outputs/tables/measurement_validation_scores.csv`
- `outputs/logs/07_measurement_validation_log.txt` (parameters + session info)

## Notes
- EEI and Adaptive Capacity are treated as constructed (composite) indices in the main text, with EFA used as a diagnostic coherence check.
- LEF is treated as reflective (latent) and estimated via CFA.
