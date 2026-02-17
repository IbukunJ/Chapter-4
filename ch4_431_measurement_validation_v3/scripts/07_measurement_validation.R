# ======================================================================
# Chapter 4 §4.3.1 — Measurement validation (EFA + CFA) for EEI, Adaptive Capacity, LEF
# Reproducible script: generates Appendix tables/figures and factor scores
#
# Outputs (relative to project root):
#   outputs/tables/Table_4E1_Factorability.xlsx
#   outputs/figures/Fig_4E1_ParallelAnalysis.png
#   outputs/tables/Table_4E2_EFA_Loadings.xlsx
#   outputs/tables/Table_4E3_EFA_FactorInterpretation.xlsx
#   outputs/tables/Table_4E4_CFA_Fit_EEI.xlsx
#   outputs/tables/Table_4E4_CFA_Fit_AdaptiveCapacity.xlsx
#   outputs/tables/Table_4E4_CFA_Fit_LEF.xlsx
#   outputs/tables/Table_4E5_CFA_Loadings_EEI.xlsx
#   outputs/tables/Table_4E5_CFA_Loadings_AdaptiveCapacity.xlsx
#   outputs/tables/Table_4E5_CFA_Loadings_LEF.xlsx
#   outputs/tables/Table_4E6_Reliability_AVE_EEI.xlsx
#   outputs/tables/Table_4E6_Reliability_AVE_AdaptiveCapacity.xlsx
#   outputs/tables/Table_4E6_Reliability_AVE_LEF.xlsx
#   outputs/tables/measurement_validation_scores.csv
#   outputs/logs/07_measurement_validation_log.txt
#
# Key references:
#   Bartlett (1950); Kaiser (1974); Horn (1965); Fabrigar & Wegener (2011); Brown (2015)
#   Hu & Bentler (1999); Rosseel (2012); Fornell & Larcker (1981); Raykov (1997)
# ======================================================================

options(stringsAsFactors = FALSE)
set.seed(42)

# ---- Packages ----
suppressPackageStartupMessages({
  library(readr)      # read_csv
  library(dplyr)      # data wrangling
  library(tidyr)      # pivoting
  library(stringr)    # string helpers
  library(purrr)      # functional programming
  library(psych)      # KMO, Bartlett, fa.parallel, fa
  library(lavaan)     # CFA (Rosseel 2012)
  library(semTools)   # reliability + AVE helpers (Jorgensen et al.)
  library(openxlsx)   # thesis-ready Excel outputs
  library(ggplot2)    # optional plots
})

# ---- Paths ----
in_file <- file.path("data", "analysis_dataset_full.csv")

out_tables <- file.path("outputs", "tables")
out_figs   <- file.path("outputs", "figures")
out_logs   <- file.path("outputs", "logs")

dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_figs,   recursive = TRUE, showWarnings = FALSE)
dir.create(out_logs,   recursive = TRUE, showWarnings = FALSE)

# ---- Read data ----
df <- readr::read_csv(in_file, show_col_types = FALSE)

# ---- Variable mapping (aligns the chapter narrative to the modelling table) ----
# NOTE: These names match the variable labels used in §4.3.1 and the appendix tables.
df_mv <- df %>%
  rename(
    spatial_jurisdiction            = Breadth_Spatial,
    subject_matter_jurisdiction     = Breadth_Subject,
    mandate_breadth                 = MandateScope_0_10,
    strategies                      = Strategy_Diversity,
    defined_objectives              = Objective_Diversity,
    vertical_coordination           = Coord_Vertical,
    horizontal_coordination         = Coord_Horizontal,
    relations                       = Relations,
    authority_treaty_Factor         = Authority_Treaty,
    authority_BindingSecondary_Factor= Authority_BindingSecondary,
    authority_delegated_factor      = Authority_Delegated,
    embeddedness_score              = Embeddedness_0_10,
    intersection_score              = IntersectionScore_count
  )

# ---- Indicator pools ----
eei_pool <- c(
  "spatial_jurisdiction","subject_matter_jurisdiction","mandate_breadth",
  "strategies","defined_objectives","vertical_coordination","horizontal_coordination","relations",
  "authority_treaty_Factor","authority_BindingSecondary_Factor","authority_delegated_factor"
)

adapt_pool <- c("strategies","defined_objectives")

lef_pool <- c(
  "relations","vertical_coordination","horizontal_coordination","embeddedness_score","intersection_score",
  "authority_treaty_Factor","authority_BindingSecondary_Factor","authority_delegated_factor"
)

# ---- Helper: z-standardise selected columns (across-IGO scaling) ----
z_standardise <- function(dat, vars){
  dat %>%
    select(all_of(vars)) %>%
    mutate(across(everything(), ~ as.numeric(scale(.x)))) # mean 0, sd 1
}

# ---- 1) Factorability: KMO + Bartlett (Bartlett 1950; Kaiser 1974) ----
factorability_summary <- function(dat, vars, set_name){
  X <- dat %>% select(all_of(vars)) %>% tidyr::drop_na()
  Z <- z_standardise(X, vars)
  R <- cor(Z, use = "pairwise.complete.obs")

  kmo <- psych::KMO(R)
  bart <- psych::cortest.bartlett(R, n = nrow(Z))

  tibble(
    Indicator_Set = set_name,
    N_complete    = nrow(Z),
    p_vars        = length(vars),
    KMO_overall   = unname(kmo$MSA),
    Bartlett_chi2 = unname(bart$chisq),
    Bartlett_df   = unname(bart$df),
    Bartlett_p    = unname(bart$p.value)
  )
}

kmo_msa_table <- function(dat, vars){
  X <- dat %>% select(all_of(vars)) %>% tidyr::drop_na()
  Z <- z_standardise(X, vars)
  R <- cor(Z, use = "pairwise.complete.obs")
  psych::KMO(R)$MSAi %>%
    tibble::enframe(name = "Variable", value = "MSA")
}

tab_4e1 <- bind_rows(
  factorability_summary(df_mv, eei_pool,  "EEI_pool"),
  factorability_summary(df_mv, adapt_pool,"Adaptive_pool"),
  factorability_summary(df_mv, lef_pool,  "LEF_pool")
)

msa_eei   <- kmo_msa_table(df_mv, eei_pool)
msa_adapt <- kmo_msa_table(df_mv, adapt_pool)
msa_lef   <- kmo_msa_table(df_mv, lef_pool)

# Write Table 4.E1
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Summary")
openxlsx::writeData(wb, "Summary", tab_4e1)
openxlsx::addWorksheet(wb, "EEI_MSA")
openxlsx::writeData(wb, "EEI_MSA", msa_eei)
openxlsx::addWorksheet(wb, "Adaptive_MSA")
openxlsx::writeData(wb, "Adaptive_MSA", msa_adapt)
openxlsx::addWorksheet(wb, "LEF_MSA")
openxlsx::writeData(wb, "LEF_MSA", msa_lef)

openxlsx::addWorksheet(wb, "Data_Dictionary")
openxlsx::writeData(wb, "Data_Dictionary", tibble::tribble(
  ~Field, ~Definition,
  "Indicator_Set", "Indicator pool tested for factorability (EEI_pool, Adaptive_pool, LEF_pool).",
  "N_complete", "Number of complete-case rows used for the test.",
  "p_vars", "Number of variables in the indicator pool.",
  "KMO_overall", "Kaiser-Meyer-Olkin measure of sampling adequacy (overall).",
  "Bartlett_chi2", "Bartlett test statistic for sphericity (chi-square).",
  "Bartlett_df", "Degrees of freedom for Bartlett test.",
  "Bartlett_p", "p-value for Bartlett test."
))

openxlsx::saveWorkbook(wb, file.path(out_tables, "Table_4E1_Factorability.xlsx"), overwrite = TRUE)

# ---- 2) Parallel analysis (Horn 1965) ----
# Implemented via psych::fa.parallel for transparency and standard practice (Revelle).
X_pa <- df_mv %>% select(all_of(eei_pool)) %>% tidyr::drop_na()
Z_pa <- z_standardise(X_pa, eei_pool)

png(file.path(out_figs, "Fig_4E1_ParallelAnalysis.png"), width = 2200, height = 1600, res = 300)
psych::fa.parallel(Z_pa, fm = "pa", fa = "fa", n.iter = 500, main = "Parallel analysis (EEI indicator pool)")
dev.off()

# Retained factors (primary rule): count where observed eigenvalues exceed mean random eigenvalues.
# We replicate this rule deterministically from the fa.parallel output:
pa_obj <- psych::fa.parallel(Z_pa, fm = "pa", fa = "fa", n.iter = 500, plot = FALSE)
nf_eei <- sum(pa_obj$fa.values > pa_obj$fa.sim)
if (nf_eei < 1) nf_eei <- 1

# Optional: repeat for LEF pool
X_pa_lef <- df_mv %>% select(all_of(lef_pool)) %>% tidyr::drop_na()
Z_pa_lef <- z_standardise(X_pa_lef, lef_pool)
pa_lef <- psych::fa.parallel(Z_pa_lef, fm = "pa", fa = "fa", n.iter = 500, plot = FALSE)
nf_lef <- sum(pa_lef$fa.values > pa_lef$fa.sim)
if (nf_lef < 1) nf_lef <- 1

# ---- 3) EFA (principal axis; oblimin rotation; Fabrigar & Wegener 2011) ----
efa_run <- function(dat, vars, nfactors){
  X <- dat %>% select(all_of(vars)) %>% tidyr::drop_na()
  Z <- z_standardise(X, vars)
  psych::fa(Z, nfactors = nfactors, fm = "pa", rotate = "oblimin")
}

efa_eei <- efa_run(df_mv, eei_pool, nf_eei)
efa_lef <- efa_run(df_mv, lef_pool, nf_lef)
efa_ad  <- efa_run(df_mv, adapt_pool, 1)

# Loadings tables (pattern matrix)
loadings_df <- function(efa_obj){
  L <- as.data.frame(unclass(efa_obj$loadings))
  L <- tibble::rownames_to_column(L, var = "Indicator")
  L
}

tab_efa_eei <- loadings_df(efa_eei)
tab_efa_lef <- loadings_df(efa_lef)
tab_efa_ad  <- loadings_df(efa_ad)

meta_efa <- tibble::tribble(
  ~Set, ~Extraction, ~Rotation, ~Factors_retained_parallel,
  "EEI_pool", "Principal axis (pa)", "Oblimin (oblique)", nf_eei,
  "Adaptive_pool", "Principal axis (pa)", "None/NA (1 factor)", 1,
  "LEF_pool", "Principal axis (pa)", "Oblimin (oblique)", nf_lef
)

# Write Table 4.E2
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "Meta")
openxlsx::writeData(wb, "Meta", meta_efa)
openxlsx::addWorksheet(wb, "EEI_EFA_Loadings")
openxlsx::writeData(wb, "EEI_EFA_Loadings", tab_efa_eei)
openxlsx::addWorksheet(wb, "Adaptive_EFA_Loadings")
openxlsx::writeData(wb, "Adaptive_EFA_Loadings", tab_efa_ad)
openxlsx::addWorksheet(wb, "LEF_EFA_Loadings")
openxlsx::writeData(wb, "LEF_EFA_Loadings", tab_efa_lef)

openxlsx::addWorksheet(wb, "Data_Dictionary")
openxlsx::writeData(wb, "Data_Dictionary", tibble::tribble(
  ~Field, ~Definition,
  "F1..Fk", "Rotated factor loadings (pattern matrix).",
  "Extraction", "Principal-axis factoring (fm = 'pa').",
  "Rotation", "Oblimin oblique rotation (rotate = 'oblimin')."
))
openxlsx::saveWorkbook(wb, file.path(out_tables, "Table_4E2_EFA_Loadings.xlsx"), overwrite = TRUE)

# ---- 4) EFA factor interpretation table ----
factor_interpretation <- function(load_df, cutoff = 0.40){
  fcols <- setdiff(names(load_df), "Indicator")
  purrr::map_dfr(fcols, function(f){
    tmp <- load_df %>% select(Indicator, !!sym(f)) %>% rename(loading = !!sym(f))
    tmp2 <- tmp %>%
      mutate(abs_loading = abs(loading)) %>%
      filter(abs_loading >= cutoff) %>%
      arrange(desc(abs_loading))
    if (nrow(tmp2) == 0){
      # fall back to top 3 indicators
      tmp2 <- tmp %>% mutate(abs_loading = abs(loading)) %>% arrange(desc(abs_loading)) %>% head(3)
    }
    tibble(Factor = f,
           `Indicators_|loading|>=0.40` = paste0(tmp2$Indicator, " (", sprintf("%.3f", tmp2$loading), ")", collapse = "; "))
  })
}

interp_eei <- factor_interpretation(tab_efa_eei)
interp_ad  <- factor_interpretation(tab_efa_ad)
interp_lef <- factor_interpretation(tab_efa_lef)

recommended_cfa <- tibble::tribble(
  ~Construct, ~Indicators_Retained, ~Indicators_Optional, ~Indicators_Exclude,
  "EEI",
  "spatial_jurisdiction; subject_matter_jurisdiction; mandate_breadth",
  "authority_BindingSecondary_Factor; authority_treaty_Factor; authority_delegated_factor",
  "relations; vertical_coordination; horizontal_coordination; embeddedness_score",
  "Adaptive_Capacity",
  "strategies; defined_objectives",
  NA_character_,
  NA_character_,
  "LEF",
  "relations; vertical_coordination; horizontal_coordination",
  "intersection_score; authority_delegated_factor",
  "embeddedness_score (composite); authority_BindingSecondary_Factor (authority dimension)"
)

# Write Table 4.E3
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb, "EEI_FactorInterpretation")
openxlsx::writeData(wb, "EEI_FactorInterpretation", interp_eei)
openxlsx::addWorksheet(wb, "Adaptive_FactorInterpretation")
openxlsx::writeData(wb, "Adaptive_FactorInterpretation", interp_ad)
openxlsx::addWorksheet(wb, "LEF_FactorInterpretation")
openxlsx::writeData(wb, "LEF_FactorInterpretation", interp_lef)
openxlsx::addWorksheet(wb, "Recommended_CFA_Indicators")
openxlsx::writeData(wb, "Recommended_CFA_Indicators", recommended_cfa)

openxlsx::addWorksheet(wb, "Data_Dictionary")
openxlsx::writeData(wb, "Data_Dictionary", tibble::tribble(
  ~Field, ~Definition,
  "Indicators_|loading|>=0.40", "Indicators with absolute loadings at or above the reporting threshold (default 0.40) for each factor."
))
openxlsx::saveWorkbook(wb, file.path(out_tables, "Table_4E3_EFA_FactorInterpretation.xlsx"), overwrite = TRUE)

# ---- 5) CFA models (single-factor) with robust estimation (Rosseel 2012; Brown 2015) ----
# NOTE on two-indicator factors:
# With only two indicators, a single-factor model is just-identified under additional constraints.
# We fix both loadings to 1 (tau-equivalent specification) so the model is identified (df = 0).
model_eei <- "
  EEI =~ spatial_jurisdiction + subject_matter_jurisdiction + mandate_breadth +
         authority_BindingSecondary_Factor + authority_delegated_factor
"

model_adapt <- "
  Adaptive_Capacity =~ 1*strategies + 1*defined_objectives
"

model_lef <- "
  LEF =~ relations + vertical_coordination + horizontal_coordination +
         embeddedness_score + intersection_score +
         authority_treaty_Factor + authority_BindingSecondary_Factor + authority_delegated_factor
"

# Standardise (z) the observed indicators for comparability
cfa_data <- df_mv %>%
  select(Institution, all_of(unique(c(eei_pool, adapt_pool, lef_pool)))) %>%
  mutate(across(-Institution, ~ as.numeric(scale(.x))))

fit_cfa <- function(model, data, label){
  lavaan::cfa(model, data = data, estimator = "MLR", missing = "fiml", std.lv = TRUE)
}

fit_eei <- fit_cfa(model_eei, cfa_data, "EEI")
fit_ad  <- fit_cfa(model_adapt, cfa_data, "Adaptive_Capacity")
fit_lef <- fit_cfa(model_lef, cfa_data, "LEF")

# Fit indices table helper
fit_table <- function(fit, label){
  fm <- lavaan::fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
  tibble(
    Model = label,
    chisq = unname(fm["chisq"]),
    df = unname(fm["df"]),
    p_value = unname(fm["pvalue"]),
    rmsea = unname(fm["rmsea"]),
    cfi = unname(fm["cfi"]),
    tli = unname(fm["tli"]),
    srmr = unname(fm["srmr"])
  )
}

# Loadings table helper
loadings_table <- function(fit, label){
  lavaan::parameterEstimates(fit, standardized = TRUE) %>%
    filter(op == "=~") %>%
    transmute(
      Model = label,
      Latent = lhs,
      Indicator = rhs,
      Estimate = est,
      StdErr = se,
      z = z,
      p_value = pvalue,
      Std_Loading = std.all
    )
}

# Reliability + AVE helper (Fornell & Larcker 1981; Raykov 1997)
reliability_table <- function(fit, label){
  # semTools::reliability returns omega and CR variants; AVE via semTools::AVE
  rel <- semTools::reliability(fit)
  ave <- semTools::AVE(fit)

  # Extract first latent variable row if multiple
  # reliability() returns a list or matrix depending on model complexity
  rel_df <- as.data.frame(rel)
  rel_df$Model <- label
  rel_df

  # AVE is typically returned as a named vector
  ave_df <- tibble(Model = label, AVE = as.numeric(ave[1]))
  list(reliability = rel_df, ave = ave_df)
}

# ---- Write CFA outputs ----
write_single_sheet <- function(df, path, sheet = "Sheet1", dictionary = NULL){
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, df)
  if (!is.null(dictionary)){
    openxlsx::addWorksheet(wb, "Data_Dictionary")
    openxlsx::writeData(wb, "Data_Dictionary", dictionary)
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

# Fit tables
write_single_sheet(fit_table(fit_eei, "EEI_1factor"),
                   file.path(out_tables, "Table_4E4_CFA_Fit_EEI.xlsx"),
                   sheet = "Fit",
                   dictionary = tibble::tribble(
                     ~Field, ~Definition,
                     "chisq","Model chi-square (robust MLR).",
                     "df","Degrees of freedom.",
                     "rmsea","RMSEA fit index.",
                     "cfi","CFI fit index.",
                     "tli","TLI fit index.",
                     "srmr","SRMR fit index."
                   ))

write_single_sheet(fit_table(fit_ad, "AdaptiveCapacity_1factor"),
                   file.path(out_tables, "Table_4E4_CFA_Fit_AdaptiveCapacity.xlsx"),
                   sheet = "Fit")

write_single_sheet(fit_table(fit_lef, "LEF_1factor"),
                   file.path(out_tables, "Table_4E4_CFA_Fit_LEF.xlsx"),
                   sheet = "Fit")

# Loadings tables
write_single_sheet(loadings_table(fit_eei, "EEI_1factor"),
                   file.path(out_tables, "Table_4E5_CFA_Loadings_EEI.xlsx"),
                   sheet = "Loadings")

write_single_sheet(loadings_table(fit_ad, "AdaptiveCapacity_1factor"),
                   file.path(out_tables, "Table_4E5_CFA_Loadings_AdaptiveCapacity.xlsx"),
                   sheet = "Loadings")

write_single_sheet(loadings_table(fit_lef, "LEF_1factor"),
                   file.path(out_tables, "Table_4E5_CFA_Loadings_LEF.xlsx"),
                   sheet = "Loadings")

# Reliability + AVE
rel_eei <- reliability_table(fit_eei, "EEI_1factor")
rel_ad  <- reliability_table(fit_ad, "AdaptiveCapacity_1factor")
rel_lef <- reliability_table(fit_lef, "LEF_1factor")

write_single_sheet(rel_lef$reliability %>% left_join(rel_lef$ave, by = "Model"),
                   file.path(out_tables, "Table_4E6_Reliability_AVE_LEF.xlsx"),
                   sheet = "Reliability")

write_single_sheet(rel_eei$reliability %>% left_join(rel_eei$ave, by = "Model"),
                   file.path(out_tables, "Table_4E6_Reliability_AVE_EEI.xlsx"),
                   sheet = "Reliability")

write_single_sheet(rel_ad$reliability %>% left_join(rel_ad$ave, by = "Model"),
                   file.path(out_tables, "Table_4E6_Reliability_AVE_AdaptiveCapacity.xlsx"),
                   sheet = "Reliability")

# ---- Factor scores for downstream models ----
score_standardise <- function(x){
  as.numeric(scale(x))
}

scores <- tibble(
  Institution = cfa_data$Institution,
  EEI_factor_z = score_standardise(as.numeric(lavaan::lavPredict(fit_eei, method = "regression"))),
  AdaptiveCapacity_factor_z = score_standardise(as.numeric(lavaan::lavPredict(fit_ad, method = "regression"))),
  LEF_factor_z = score_standardise(as.numeric(lavaan::lavPredict(fit_lef, method = "regression")))
)

readr::write_csv(scores, file.path(out_tables, "measurement_validation_scores.csv"))

# ---- Log: parameter settings + session info ----
log_file <- file.path(out_logs, "07_measurement_validation_log.txt")
sink(log_file)
cat("07_measurement_validation_log.txt\n")
cat(format(Sys.time()), "\n\n")
cat("Inputs:\n")
cat(" - ", in_file, "\n\n", sep = "")

cat("Indicator pools:\n")
cat(" - EEI_pool (p=", length(eei_pool), "): ", paste(eei_pool, collapse = ", "), "\n", sep = "")
cat(" - Adaptive_pool (p=", length(adapt_pool), "): ", paste(adapt_pool, collapse = ", "), "\n", sep = "")
cat(" - LEF_pool (p=", length(lef_pool), "): ", paste(lef_pool, collapse = ", "), "\n\n", sep = "")

cat("Parallel analysis:\n")
cat(" - n.iter = 500\n")
cat(" - retained factors (EEI_pool) = ", nf_eei, "\n", sep = "")
cat(" - retained factors (LEF_pool) = ", nf_lef, "\n\n", sep = "")

cat("EFA:\n")
cat(" - extraction = principal axis (fm='pa')\n")
cat(" - rotation = oblimin (oblique)\n")
cat(" - reporting cutoff = 0.40\n\n")

cat("CFA:\n")
cat(" - estimator = MLR\n")
cat(" - missingness = FIML (missing='fiml')\n")
cat(" - std.lv = TRUE\n\n")

cat("Session info:\n")
print(sessionInfo())
sink()

message("Measurement validation outputs written to: ", normalizePath("outputs"))
