#!/usr/bin/env Rscript

# ------------------------------------------------------------
# Chapter 4 — §4.3.1 Measurement validation
# EFA diagnostic mapping + CFA for LEF; EEI and Adaptive Capacity as composites.
#
# Outputs:
#   Table 4.E1  Factorability (KMO + Bartlett)
#   Fig  4.E1   Parallel analysis
#   Table 4.E2  EFA loadings (oblimin)
#   Table 4.E3  EFA factor interpretation
#   Table 4.E4  CFA fit (LEF)
#   Table 4.E5  CFA standardised loadings (LEF)
#   Table 4.E6  Reliability and AVE (LEF)
#   measurement_validation_scores.csv
#
# References (methods): Bartlett (1950); Horn (1965); Kaiser (1974);
# Fabrigar & Wegener (2011); Brown (2015); Rosseel (2012); Hu & Bentler (1999);
# Fornell & Larcker (1981); Raykov (1997); Bollen & Lennox (1991).
# ------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(openxlsx)

  library(psych)
  library(GPArotation)

  library(lavaan)
  library(semTools)

  library(ggplot2)
})

source("scripts/_helpers_measurement_validation.R")

# -----------------------
# Paths and parameters
# -----------------------
root_dir <- getwd()

in_full_dir  <- file.path(root_dir, "data", "inputs", "full_normalised")
in_year_dir  <- file.path(root_dir, "data", "inputs", "year")

out_tables   <- file.path(root_dir, "outputs", "tables")
out_figures  <- file.path(root_dir, "outputs", "figures")
out_logs     <- file.path(root_dir, "outputs", "logs")

dir.create(out_tables,  recursive = TRUE, showWarnings = FALSE)
dir.create(out_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(out_logs,    recursive = TRUE, showWarnings = FALSE)

params <- list(
  seed = 43101,
  parallel_iter = 1000,
  efa_fm = "pa",        # principal axis
  efa_rotate = "oblimin",
  loading_cut = 0.40,
  crossload_gap = 0.30,
  presence_threshold = 0,
  note = "EEI and Adaptive treated as constructed indices; LEF estimated via CFA."
)

# -----------------------
# Input workbooks
# -----------------------
files <- list(
  Spatial_Jurisdiction              = file.path(in_full_dir, "Spatial_Jurisdiction_FULL_Normalised_REAL.xlsx"),
  Subject_Matter_Jurisdiction       = file.path(in_full_dir, "Subject_Matter_Jurisdiction_FULL_Normalised_REAL REAL.xlsx"),
  Sources_of_Jurisdiction           = file.path(in_full_dir, "Sources_of_Jurisdiction_FULL_Normalised_REAL.xlsx"),
  Defined_Objectives                = file.path(in_full_dir, "Defined_Objectives_FULL_Normalised_REAL.xlsx"),
  Strategies                        = file.path(in_full_dir, "Strategies_FULL_Normalised_REAL.xlsx"),
  InterInstitutional_Relationships  = file.path(in_full_dir, "Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx"),
  Vertical_Coordination             = file.path(in_full_dir, "Vertical_Coordination_FULL_Normalised.xlsx"),
  Horizontal_Coordination           = file.path(in_full_dir, "Horizontal_Coordination_FULL_Normalised_REAL.xlsx")
)

year_file <- file.path(in_year_dir, "Year_of_Establishment_with_Categories_and_Density.xlsx")

missing <- names(files)[!file.exists(unlist(files))]
if (length(missing) > 0) stop("Missing input workbook(s): ", paste(missing, collapse = ", "))
if (!file.exists(year_file)) stop("Missing year-of-establishment file: ", year_file)

read_mw <- function(path) readxl::read_excel(path, sheet = "Matrix_Wide")

mw <- lapply(files, read_mw)

# Ensure consistent IGO ordering using Spatial as reference
igos <- mw$Spatial_Jurisdiction %>% select(Institution)
mw <- lapply(mw, function(df) df %>% right_join(igos, by = "Institution"))

# Year-of-establishment covariates
year_df <- readxl::read_excel(year_file) %>%
  rename(Institution = 1) %>%
  rename(YearFounded = Year_cleaned)

year_df <- year_df %>%
  mutate(
    Age_2025 = 2025 - YearFounded
  )

# -----------------------
# Construct indicator table
# -----------------------
# Note: Ordinal_Score is the CLEAN-adjudicated 0–10 family-level score.
# Diversity indices are computed from the 10-category profiles using Shannon entropy.
ind <- igos %>%
  mutate(
    Breadth_Spatial     = mw$Spatial_Jurisdiction$Ordinal_Score,
    Breadth_Subject     = mw$Subject_Matter_Jurisdiction$Ordinal_Score,

    Relations           = mw$InterInstitutional_Relationships$Ordinal_Score,
    Coord_Vertical      = mw$Vertical_Coordination$Ordinal_Score,
    Coord_Horizontal    = mw$Horizontal_Coordination$Ordinal_Score,

    Strategy_Diversity  = shannon_diversity_0_10(mw$Strategies, suffix = "_WithinIGO"),
    Objective_Diversity = shannon_diversity_0_10(mw$Defined_Objectives, suffix = "_WithinIGO"),

    # Selected authority components (category-level, across-IGO scale)
    Authority_Treaty          = mw$Sources_of_Jurisdiction$`Foundational Treaties & Charters_AcrossIGO`,
    Authority_BindingSecondary = mw$Sources_of_Jurisdiction$`Binding Secondary Law_AcrossIGO`,
    Authority_Delegated       = mw$Sources_of_Jurisdiction$`Delegated or Derived Powers_AcrossIGO`,

    # Governance-function emphasis (objectives), across-IGO scale
    Emphasis_InclusionRights           = mw$Defined_Objectives$`Inclusion & Rights_AcrossIGO`,
    Emphasis_MonitoringAccountability  = mw$Defined_Objectives$`Monitoring & Accountability_AcrossIGO`,
    Emphasis_PolicyRegulation          = mw$Defined_Objectives$`Policy & Regulation_AcrossIGO`
  ) %>%
  left_join(
    year_df %>% select(Institution, YearFounded, Age_2025, FoundingDensity_5yr, CumulativeStock, Founding_Era_Category),
    by = "Institution"
  ) %>%
  mutate(
    # Constructed indices (composites)
    AdaptiveCapacity_0_10 = rowMeans(cbind(Strategy_Diversity, Objective_Diversity), na.rm = TRUE),
    MandateScope_0_10     = rowMeans(cbind(Breadth_Spatial, Breadth_Subject), na.rm = TRUE),
    Embeddedness_0_10     = rowMeans(cbind(Relations, Coord_Vertical, Coord_Horizontal), na.rm = TRUE),
    AuthorityCore_0_10    = rowMeans(cbind(Authority_Treaty, Authority_BindingSecondary, Authority_Delegated), na.rm = TRUE),
    NicheSpecialisation_0_10 = 10 - MandateScope_0_10
  )

# Intersection score (structural niche proxy; binary presence intersection of subject × spatial)
spatial_bin <- binary_presence_from_scores(mw$Spatial_Jurisdiction, suffix = "_AcrossIGO", threshold = params$presence_threshold)
subject_bin <- binary_presence_from_scores(mw$Subject_Matter_Jurisdiction, suffix = "_AcrossIGO", threshold = params$presence_threshold)

# Optional: subset spatial categories to match the niche proxy used in robustness checks (see script header).
# Adjust these labels if the underlying column names differ in future revisions.
spatial_keep <- c(
  "Archipelago_AcrossIGO", "Coastal Zone_AcrossIGO", "Enclosed/Semi-enclosed Sea_AcrossIGO",
  "High Seas_AcrossIGO", "Internal Waters_AcrossIGO", "The Area_AcrossIGO"
)
spatial_cols_all <- names(mw$Spatial_Jurisdiction)[str_ends(names(mw$Spatial_Jurisdiction), "_AcrossIGO")]
spatial_idx <- which(spatial_cols_all %in% spatial_keep)
if (length(spatial_idx) == 0) {
  # fallback: use all spatial categories if the subset labels are not found
  spatial_idx <- seq_len(ncol(spatial_bin))
}

intersection_score <- rowSums(spatial_bin[, spatial_idx, drop = FALSE]) * rowSums(subject_bin)
ind$IntersectionScore_count <- intersection_score

# EEI composite (constructed, not treated as reflective in the main text)
eei_components <- ind %>%
  select(
    MandateScope_0_10,
    Strategy_Diversity,
    Objective_Diversity,
    Embeddedness_0_10,
    AuthorityCore_0_10
  ) %>%
  mutate(across(everything(), z_standardise))

ind$EEI_z <- rowMeans(eei_components, na.rm = TRUE)

# Save the assembled indicator table used for §4.3.1
readr::write_csv(ind, file.path(out_tables, "measurement_validation_inputs.csv"))

# -----------------------
# EFA diagnostic mapping
# -----------------------
efa_pool <- ind %>%
  select(
    Breadth_Spatial, Breadth_Subject,
    Strategy_Diversity, Objective_Diversity,
    Relations, Coord_Vertical, Coord_Horizontal,
    Authority_Treaty, Authority_BindingSecondary, Authority_Delegated
  ) %>%
  mutate(across(everything(), z_standardise))

R <- cor(efa_pool, use = "pairwise.complete.obs")

kmo <- psych::KMO(R)
bart <- psych::cortest.bartlett(R, n = nrow(efa_pool))

factorability_tbl <- tibble::tibble(
  Metric = c("KMO_overall", "Bartlett_chi2", "Bartlett_df", "Bartlett_p"),
  Value  = c(kmo$MSA, bart$chisq, bart$df, bart$p.value)
)

kmo_items_tbl <- tibble::tibble(
  Variable = names(kmo$MSAi),
  KMO_MSA  = as.numeric(kmo$MSAi)
) %>% arrange(desc(KMO_MSA))

dict_factorability <- tibble::tibble(
  Field = c("KMO_overall", "KMO_MSA", "Bartlett_chi2", "Bartlett_df", "Bartlett_p"),
  Description = c(
    "Kaiser-Meyer-Olkin overall measure of sampling adequacy (correlation matrix).",
    "Item-level MSA values for each indicator in the EFA pool.",
    "Bartlett test statistic for sphericity (H0: correlation matrix is identity).",
    "Degrees of freedom for Bartlett test.",
    "P-value for Bartlett test."
  ),
  Scale = c("0–1", "0–1", "Chi-square", "count", "0–1"),
  Source = c("psych::KMO()", "psych::KMO()", "psych::cortest.bartlett()", "psych::cortest.bartlett()", "psych::cortest.bartlett()"),
  Script = "scripts/07_measurement_validation.R"
)

write_xlsx_with_dictionary(
  file.path(out_tables, "Table_4E1_Factorability.xlsx"),
  data_sheets = list(
    "Summary" = factorability_tbl,
    "Item_MSA" = kmo_items_tbl
  ),
  dictionary_df = dict_factorability
)

# Parallel analysis figure
set.seed(params$seed)
png(file.path(out_figures, "Fig_4E1_ParallelAnalysis.png"), width = 1600, height = 1200, res = 200)
pa <- psych::fa.parallel(efa_pool, fa = "fa", fm = params$efa_fm, n.iter = params$parallel_iter, main = "Parallel analysis (principal axis)")
dev.off()

# Choose factor count (primary rule is parallel analysis; allow override by editing params)
nfactors <- if (!is.null(pa$nfact)) pa$nfact else 3
if (is.na(nfactors) || nfactors < 1) nfactors <- 3

efa_fit <- psych::fa(
  efa_pool,
  nfactors = nfactors,
  fm = params$efa_fm,
  rotate = params$efa_rotate
)

loadings_mat <- as.data.frame(unclass(efa_fit$loadings))
loadings_mat <- loadings_mat %>%
  tibble::rownames_to_column("Indicator") %>%
  mutate(Communality = efa_fit$communality) %>%
  relocate(Communality, .after = Indicator)

write_xlsx_with_dictionary(
  file.path(out_tables, "Table_4E2_EFA_Loadings.xlsx"),
  data_sheets = list("EFA_Loadings" = loadings_mat),
  dictionary_df = tibble::tibble(
    Field = c("Indicator", paste0("Factor", seq_len(nfactors)), "Communality"),
    Description = c(
      "Indicator name (z-standardised prior to EFA).",
      rep("Rotated factor loading (oblimin rotation).", nfactors),
      "Estimated communality (variance explained by retained factors)."
    ),
    Scale = c("text", rep("−1 to 1", nfactors), "0–1"),
    Source = c("psych::fa()", rep("psych::fa()", nfactors), "psych::fa()"),
    Script = "scripts/07_measurement_validation.R"
  )
)

# Interpretation table (primary factor assignment + cross-loading flag)
loading_only <- loadings_mat %>%
  select(Indicator, starts_with("MR"), starts_with("PA"), starts_with("ML"), starts_with("Factor"))
# Psych may name factors as MR1.. or PA1.. depending on fm; normalise column names
factor_cols <- setdiff(names(loadings_mat), c("Indicator", "Communality"))

interp_tbl <- loadings_mat %>%
  select(Indicator, all_of(factor_cols)) %>%
  rowwise() %>%
  mutate(
    abs_loads = list(abs(c_across(all_of(factor_cols)))),
    max_idx = which.max(unlist(abs_loads)),
    max_loading = c_across(all_of(factor_cols))[max_idx],
    second_loading = sort(unlist(abs_loads), decreasing = TRUE)[2],
    PrimaryFactor = factor_cols[max_idx],
    CrossLoadingFlag = second_loading >= (abs(max_loading) - params$crossload_gap) & second_loading >= params$loading_cut
  ) %>%
  ungroup() %>%
  select(Indicator, PrimaryFactor, max_loading, CrossLoadingFlag) %>%
  arrange(desc(abs(max_loading)))

write_xlsx_with_dictionary(
  file.path(out_tables, "Table_4E3_EFA_FactorInterpretation.xlsx"),
  data_sheets = list("EFA_Interpretation" = interp_tbl),
  dictionary_df = tibble::tibble(
    Field = c("Indicator", "PrimaryFactor", "max_loading", "CrossLoadingFlag"),
    Description = c(
      "Indicator name.",
      "Factor with the largest absolute loading for the indicator.",
      "Rotated loading on the primary factor.",
      "TRUE if the second-largest absolute loading is within the cross-loading gap and exceeds the loading threshold."
    ),
    Scale = c("text", "text", "−1 to 1", "logical"),
    Source = c("Derived from psych::fa() loadings", "Derived from psych::fa() loadings", "Derived from psych::fa() loadings", "Derived from psych::fa() loadings"),
    Script = "scripts/07_measurement_validation.R"
  )
)

# -----------------------
# CFA for LEF (reflective)
# -----------------------
# Parsimonious single-factor model, consistent with CFA best practice at modest N.
lef_data <- ind %>%
  select(
    Relations, Coord_Vertical, Coord_Horizontal,
    Emphasis_InclusionRights, Emphasis_MonitoringAccountability, Emphasis_PolicyRegulation
  ) %>%
  mutate(across(everything(), z_standardise))

lef_model <- '
  LEF =~ Relations + Coord_Vertical + Coord_Horizontal +
         Emphasis_InclusionRights + Emphasis_MonitoringAccountability + Emphasis_PolicyRegulation
'

lef_fit <- lavaan::cfa(
  lef_model,
  data = lef_data,
  estimator = "MLR",
  missing = "fiml",
  std.lv = TRUE
)

fit_tbl <- tibble::tibble(
  chisq = lavaan::fitMeasures(lef_fit, "chisq"),
  df    = lavaan::fitMeasures(lef_fit, "df"),
  p     = lavaan::fitMeasures(lef_fit, "pvalue"),
  cfi   = lavaan::fitMeasures(lef_fit, "cfi"),
  tli   = lavaan::fitMeasures(lef_fit, "tli"),
  rmsea = lavaan::fitMeasures(lef_fit, "rmsea"),
  srmr  = lavaan::fitMeasures(lef_fit, "srmr")
)

load_tbl <- lavaan::parameterEstimates(lef_fit, standardized = TRUE) %>%
  filter(op == "=~") %>%
  transmute(
    Factor = lhs,
    Indicator = rhs,
    Loading = est,
    SE = se,
    z = z,
    p = pvalue,
    Loading_std = std.all
  )

# Reliability and AVE (Fornell & Larcker, 1981; Raykov, 1997)
lambda <- load_tbl$Loading_std
theta  <- 1 - lambda^2
CR  <- (sum(lambda))^2 / ((sum(lambda))^2 + sum(theta))
AVE <- sum(lambda^2) / (sum(lambda^2) + sum(theta))

rel_tbl <- tibble::tibble(
  Construct = "LEF",
  CompositeReliability_CR = CR,
  AVE = AVE,
  n_indicators = length(lambda)
)

write_xlsx_with_dictionary(
  file.path(out_tables, "Table_4E4_CFA_Fit_LEF.xlsx"),
  data_sheets = list("CFA_Fit" = fit_tbl),
  dictionary_df = tibble::tibble(
    Field = c("chisq","df","p","cfi","tli","rmsea","srmr"),
    Description = c(
      "Model chi-square test statistic.",
      "Degrees of freedom.",
      "P-value for chi-square test.",
      "Comparative Fit Index.",
      "Tucker-Lewis Index.",
      "Root Mean Square Error of Approximation.",
      "Standardized Root Mean Square Residual."
    ),
    Scale = c("Chi-square","count","0–1","0–1","0–1","0–1","0–1"),
    Source = "lavaan::fitMeasures()",
    Script = "scripts/07_measurement_validation.R"
  )
)

write_xlsx_with_dictionary(
  file.path(out_tables, "Table_4E5_CFA_Loadings_LEF.xlsx"),
  data_sheets = list("LEF_Loadings" = load_tbl),
  dictionary_df = tibble::tibble(
    Field = c("Factor","Indicator","Loading","SE","z","p","Loading_std"),
    Description = c(
      "Latent factor label.",
      "Observed indicator.",
      "Unstandardised loading (std.lv = TRUE).",
      "Robust standard error (MLR).",
      "z-statistic.",
      "P-value.",
      "Fully standardised loading."
    ),
    Scale = c("text","text","numeric","numeric","numeric","0–1","numeric"),
    Source = "lavaan::parameterEstimates(standardized = TRUE)",
    Script = "scripts/07_measurement_validation.R"
  )
)

write_xlsx_with_dictionary(
  file.path(out_tables, "Table_4E6_Reliability_AVE_LEF.xlsx"),
  data_sheets = list("Reliability" = rel_tbl),
  dictionary_df = tibble::tibble(
    Field = c("CompositeReliability_CR","AVE"),
    Description = c(
      "Composite reliability computed from standardised loadings and residual variances.",
      "Average Variance Extracted computed from standardised loadings and residual variances."
    ),
    Scale = c("0–1","0–1"),
    Source = c("Fornell & Larcker (1981); Raykov (1997)", "Fornell & Larcker (1981)"),
    Script = "scripts/07_measurement_validation.R"
  )
)

# Factor scores (standardised) for downstream regression models
lef_scores <- lavaan::lavPredict(lef_fit, method = "regression")
ind$LEF_z <- as.numeric(scale(lef_scores[, "LEF"]))

# Export constructed and latent scores table
scores_out <- ind %>%
  select(
    Institution,
    EEI_z, LEF_z, AdaptiveCapacity_0_10,
    MandateScope_0_10, Embeddedness_0_10, AuthorityCore_0_10,
    IntersectionScore_count, NicheSpecialisation_0_10,
    YearFounded, Age_2025, FoundingDensity_5yr, CumulativeStock, Founding_Era_Category
  )

readr::write_csv(scores_out, file.path(out_tables, "measurement_validation_scores.csv"))

# -----------------------
# Logging for auditability
# -----------------------
log_path <- file.path(out_logs, "07_measurement_validation_log.txt")
sink(log_path)
cat("Chapter 4 §4.3.1 Measurement validation log\n")
cat("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
cat("Parameters:\n")
print(params)
cat("\nEFA pool variables:\n")
print(names(efa_pool))
cat("\nSelected nfactors (parallel analysis): ", nfactors, "\n\n")
cat("CFA LEF model:\n")
cat(lef_model, "\n\n")
cat("Session info:\n")
print(sessionInfo())
sink()

message("§4.3.1 outputs written to: ", normalizePath(file.path(root_dir, "outputs"), winslash = "/"))