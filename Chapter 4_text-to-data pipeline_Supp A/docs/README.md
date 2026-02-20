# Documentation

This folder contains thesis-facing documentation, protocols for the CLEAN review layer, and machine-readable codebooks.

- `codebook.xlsx`  
  Consolidated master codebook spanning all outputs (tables, figures, logs). A copy is also written to `outputs/tables/codebook.xlsx` by the pipeline.

- `data_dictionary_master_matrix.csv`  
  Variable-level dictionary for the master analysis matrix (written by the pipeline to `outputs/data/master_matrix.csv`).

- `Appendix XXX_Identifying Categories for IGO Attributes.docx`  
  Narrative definitions and justification for the 10-category schema per attribute family.

- `Appendix XXX_Methodological Pipeline for Text Standardisation_Category Mapping_ and Scoring.docx`  
  Full methodological pipeline description (text standardisation, category mapping, scoring, and normalisation).

- `Chapter 4_Updated_cont.docx` / `4_To complete.docx`  
  Working chapter drafts included for context and cross-referencing.

If you add new output tables/figures, re-run `pipeline/06_make_dictionaries_and_codebook.R` to refresh embedded Data_Dictionary sheets and the consolidated codebook.
