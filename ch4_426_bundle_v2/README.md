# Chapter 4 replication bundle — Section 4.2.6 (Descriptives & correlation structure)

This folder contains a self-contained replication unit for the Chapter 4 thesis section **4.2.6 Descriptives & correlation structure**.

## What this bundle does

- Reads the **FULL-normalised attribute workbooks** (sheet: `Matrix_Wide`) for each attribute family.
- Merges them with historical covariates derived from year of establishment (Age_2025, FoundingDensity_5yr, CumulativeStock).
- Constructs analysis indices used in the descriptive and correlation tables:
  - Scope/portfolio indices (0–10) based on min–max normalisation of reviewed `Ordinal_Score`
  - Embeddedness (0–10) from relationships + coordination components
  - Adaptive capacity (0–10) as a proxy from Strategies + Objectives components
  - Niche specialisation (0–10) as inverse of mean scope across core domains
  - **EEI_z** as a standardised weighted composite (weights recorded in the script)
  - **LEF_z** as a standardised proxy computed from mean `*_AcrossIGO` scores in the relationships workbook (validation reported elsewhere in Chapter 4)
- Writes:
  - `outputs/tables/Table_4A1_Descriptives.xlsx`
  - `outputs/tables/Table_4A2_Correlations.xlsx`
  - `outputs/figures/Fig_4A1_CorrelationHeatmap.png`
  - `outputs/logs/runlog_4_2_6.txt`

## How to run

From the project root (this folder), run in R:

```r
source("scripts/05_descriptives_correlations_v3.R")
```

The script uses only relative paths. Outputs will be written under `outputs/`.

## Inputs

All required inputs are included under `inputs/`:

- `Spatial_Jurisdiction_FULL_Normalised_REAL.xlsx`
- `Subject_Matter_Jurisdiction_FULL_Normalised_REAL REAL.xlsx`
- `Sources_of_Jurisdiction_FULL_Normalised_REAL.xlsx`
- `Strategies_FULL_Normalised_REAL.xlsx`
- `Defined_Objectives_FULL_Normalised_REAL.xlsx`
- `Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx`
- `Vertical_Coordination_FULL_Normalised.xlsx`
- `Horizontal_Coordination_FULL_Normalised_REAL.xlsx`
- `Year_of_Establishment_with_Categories_and_Density.xlsx`

## Outputs included (reference artefacts)

This bundle also includes pre-generated outputs (computed from the included inputs) so you can verify the script reproduces the same artefacts:

- `outputs/tables/Table_4A1_Descriptives.xlsx`
- `outputs/tables/Table_4A2_Correlations.xlsx`
- `outputs/figures/Fig_4A1_CorrelationHeatmap.png`
- `outputs/tables/analysis_dataset_4_2_6.csv`

## Codebook

- `docs/codebook_master_4_2_6.xlsx` provides a consolidated codebook for all variables produced in this section.

Generated: 2026-02-12 04:39 UTC

## Add-on outputs in this bundle (requested)

- **Table 4.A3 — Network diagnostics (IGO–IGO projection from relations/coordination)**
  - `outputs/tables/Table_4A3_Network_Diagnostics.xlsx` (+ CSV) and `outputs/tables/network_edge_list_cosine_q90.csv`
  - Built from the **AcrossIGO** category scores in the FULL-normalised `Matrix_Wide` sheets for:
    - `inputs/Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx`
    - `inputs/Vertical_Coordination_FULL_Normalised.xlsx`
    - `inputs/Horizontal_Coordination_FULL_Normalised_REAL.xlsx`

- **Table 4.R1 — Robustness grid (specification stability across C1–C3)**
  - `outputs/tables/Table_4R1_Robustness_Grid.xlsx` (+ CSV)
  - Computed from `outputs/tables/analysis_dataset_4_2_6.csv` (FULL-normalised modelling table), plus the eigenvector centrality column from Table 4.A3 for the network-embeddedness substitution check.

## Scripts

- `scripts/05_descriptives_correlations_v3.R` reproduces Table 4.A1, Table 4.A2, and Figure 4.2.6.
- `scripts/06_network_diagnostics_robustness.R` reproduces **Table 4.A3** and **Table 4.R1**, and writes a run log to `outputs/logs/runlog_4_2_6_network_robustness.txt`.
