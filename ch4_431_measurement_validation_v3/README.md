# Chapter 4 Measurement Validation (v3)

This bundle contains a reproducible (scripted) workflow for exploratory and confirmatory factor analysis (EFA/CFA) of:
- EEI (Endurance/Institutionalisation),
- Adaptive Capacity,
- LEF (Legitimacy/Efficacy).

## Key inputs
- `data/analysis_dataset_full.csv` (FULL-normalised modelling table)

## Key script
- `scripts/07_measurement_validation.R` (generates all tables/figures in `outputs/`)

## Outputs
Tables and figures are written to:
- `outputs/tables/`
- `outputs/figures/`
- `outputs/logs/`

## Notes
This repository is designed for *fresh-machine reproducibility*. The script writes:
- a parameter and session log,
- thesis-ready Excel tables with embedded data dictionaries,
- factor scores for downstream modelling.
