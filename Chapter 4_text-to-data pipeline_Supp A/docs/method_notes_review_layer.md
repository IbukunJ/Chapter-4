# Method notes: automated coding and researcher review

The Chapter 4 attribute matrices were built in two stages:

1. **Automated text-to-category mapping (candidate activations)**  
   Institutional texts for each IGO were standardised and then matched to a pre-specified
   10-category lexicon per attribute family. The automated stage produced candidate activations,
   keyword triggers, and KWIC excerpts, exported as a *traceability long table*.

2. **Researcher review and adjudication (CLEAN authoritative layer)**  
   Automated outputs were then reviewed to correct false positives/negatives arising from
   polysemy, abbreviations, and IGO-specific institutional context. Where needed, category
   activations and resulting ordinal counts were updated to reflect the best-fit interpretation.

In the bundled FULL_Normalised workbooks (`data/processed/full_normalised/`), this adjudication
step is recorded via the `Override_Flag` field in `Traceability_Long`. The reviewed (CLEAN)
state is what is used to produce the final `Matrix_Wide` outputs used in modelling.

The separate `data/clean/*.xlsx` files preserve the reviewed category lists per family. In cases
where these disagree with the `Traceability_Long` layer, the **FULL_Normalised workbooks are
treated as the definitive analysis inputs**, as they are the matrices used for the modelling
scripts and report generation.
