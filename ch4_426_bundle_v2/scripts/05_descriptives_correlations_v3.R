# 05_descriptives_correlations_v3.R
# Chapter 4 replication: Section 4.2.6 Descriptives & correlation structure
#
# Inputs: FULL-normalised attribute workbooks (Matrix_Wide sheets) and historical covariates table.
# Outputs: Table 4.A1 (descriptives), Table 4.A2 (correlations), correlation heatmap figure, run log.
#
# This script is designed to run deterministically on a clean machine once the Chapter 4 pipeline
# has produced the FULL-normalised workbooks in inputs/.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readxl)
  library(openxlsx)
  library(janitor)
  library(Hmisc)      # rcorr: correlations + p-values
  library(ggplot2)
})

# ----------------------------
# Parameters (recorded in run log)
# ----------------------------
params <- list(
  year_ref = 2025L,
  density_window_half = 2L,     # ±2 years -> 5-year window
  sig_levels = c(`*` = 0.05, `**` = 0.01, `***` = 0.001),
  cor_method = "pearson",
  clustering_distance = "1 - r",
  weights_EEI = c(
    age = 0.1108,
    spatial = 0.1841,
    vertical = 0.1127,
    subject = 0.1019,
    strategies = 0.0896,
    objectives = 0.0874,
    relations = 0.0853,
    legal = 0.0817,
    horizontal = 0.1464
  )
)

# ----------------------------
# Paths
# ----------------------------
here <- function(...) file.path(...)

in_dir  <- here("inputs")
out_tbl <- here("outputs", "tables")
out_fig <- here("outputs", "figures")
out_log <- here("outputs", "logs")

dir.create(out_tbl, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)
dir.create(out_log, recursive = TRUE, showWarnings = FALSE)

# Input workbooks (FULL-normalised)
f_spatial    <- here(in_dir, "Spatial_Jurisdiction_FULL_Normalised_REAL.xlsx")
f_subject    <- here(in_dir, "Subject_Matter_Jurisdiction_FULL_Normalised_REAL REAL.xlsx")
f_legal      <- here(in_dir, "Sources_of_Jurisdiction_FULL_Normalised_REAL.xlsx")
f_objectives <- here(in_dir, "Strategies_FULL_Normalised_REAL.xlsx")                      # objectives set
f_strategies <- here(in_dir, "Defined_Objectives_FULL_Normalised_REAL.xlsx")              # strategy/function set
f_relations  <- here(in_dir, "Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx")
f_vertical   <- here(in_dir, "Vertical_Coordination_FULL_Normalised.xlsx")
f_horizontal <- here(in_dir, "Horizontal_Coordination_FULL_Normalised_REAL.xlsx")
f_year       <- here(in_dir, "Year_of_Establishment_with_Categories_and_Density.xlsx")

# ----------------------------
# Helpers
# ----------------------------
clean_igo_name <- function(x) {
  x %>%
    as.character() %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

normalize_0_1 <- function(x) {
  x <- as.numeric(x)
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  if (!is.finite(mn) || !is.finite(mx) || mx == mn) return(rep(0, length(x)))
  (x - mn) / (mx - mn)
}

read_ordinal <- function(path, name) {
  read_excel(path, sheet = "Matrix_Wide") %>%
    transmute(
      IGO = clean_igo_name(Institution),
      !!name := as.numeric(Ordinal_Score)
    )
}

# Extract LPI proxy from AcrossIGO category scores (relations workbook)
read_lpi_raw <- function(path) {
  dat <- read_excel(path, sheet = "Matrix_Wide") %>%
    mutate(IGO = clean_igo_name(Institution))

  across_cols <- names(dat)[str_detect(names(dat), "_AcrossIGO$")]
  if (length(across_cols) == 0) stop("No *_AcrossIGO columns found in relations workbook.")

  dat %>%
    transmute(
      IGO,
      LPI_raw = rowMeans(across(across_cols), na.rm = TRUE)
    )
}

# ----------------------------
# Load component scores from FULL Matrix_Wide sheets
# ----------------------------
spatial    <- read_ordinal(f_spatial,    "spatial_raw")
subject    <- read_ordinal(f_subject,    "subject_raw")
legal      <- read_ordinal(f_legal,      "legal_raw")
objectives <- read_ordinal(f_objectives, "objectives_raw")
strategies <- read_ordinal(f_strategies, "strategies_raw")
relations  <- read_ordinal(f_relations,  "relations_raw")
vertical   <- read_ordinal(f_vertical,   "vertical_raw")
horizontal <- read_ordinal(f_horizontal, "horizontal_raw")
lpi        <- read_lpi_raw(f_relations)

# Historical covariates (computed upstream and released as a standalone table)
year_tbl <- read_excel(f_year) %>%
  transmute(
    IGO = clean_igo_name(`Unnamed: 0`),
    founding_year = as.integer(Year_cleaned),
    Age_2025_years = as.numeric(params$year_ref - founding_year),
    FoundingDensity_5yr_count = as.numeric(FoundingDensity_5yr),
    CumulativeStock_count = as.numeric(CumulativeStock)
  )

# Merge to analysis dataset
df <- spatial %>%
  full_join(subject,    by = "IGO") %>%
  full_join(legal,      by = "IGO") %>%
  full_join(objectives, by = "IGO") %>%
  full_join(strategies, by = "IGO") %>%
  full_join(relations,  by = "IGO") %>%
  full_join(vertical,   by = "IGO") %>%
  full_join(horizontal, by = "IGO") %>%
  full_join(year_tbl,   by = "IGO") %>%
  left_join(lpi,        by = "IGO")

# ----------------------------
# Normalise to 0–1 then scale to 0–10 for interpretability
# ----------------------------
df <- df %>%
  mutate(
    spatial_norm    = normalize_0_1(spatial_raw),
    subject_norm    = normalize_0_1(subject_raw),
    legal_norm      = normalize_0_1(legal_raw),
    strategies_norm = normalize_0_1(strategies_raw),
    objectives_norm = normalize_0_1(objectives_raw),
    relations_norm  = normalize_0_1(relations_raw),
    vertical_norm   = normalize_0_1(vertical_raw),
    horizontal_norm = normalize_0_1(horizontal_raw),
    age_norm        = normalize_0_1(Age_2025_years),

    # composites (computed on 0–1, then rescaled)
    embeddedness_raw = rowMeans(cbind(relations_norm, vertical_norm, horizontal_norm), na.rm = TRUE),
    embeddedness_norm = normalize_0_1(embeddedness_raw),

    adaptive_capacity_raw = rowMeans(cbind(strategies_norm, objectives_norm), na.rm = TRUE),
    adaptive_capacity_norm = normalize_0_1(adaptive_capacity_raw),

    niche_specialisation_norm = 1 - rowMeans(cbind(spatial_norm, subject_norm, strategies_norm, objectives_norm), na.rm = TRUE),

    # EEI weighted composite on 0–1 scale, then standardise
    EEI_raw = age_norm        * params$weights_EEI["age"] +
              spatial_norm    * params$weights_EEI["spatial"] +
              vertical_norm   * params$weights_EEI["vertical"] +
              subject_norm    * params$weights_EEI["subject"] +
              strategies_norm * params$weights_EEI["strategies"] +
              objectives_norm * params$weights_EEI["objectives"] +
              relations_norm  * params$weights_EEI["relations"] +
              legal_norm      * params$weights_EEI["legal"] +
              horizontal_norm * params$weights_EEI["horizontal"],
    EEI_z = as.numeric(scale(EEI_raw)),

    # LEF proxy: standardised LPI_raw (factor-analytic validation is reported later in Chapter 4)
    LEF_z = as.numeric(scale(LPI_raw))
  ) %>%
  mutate(
    SpatialScope_0_10    = 10 * spatial_norm,
    SubjectScope_0_10    = 10 * subject_norm,
    LegalInstruments_0_10= 10 * legal_norm,
    Strategies_0_10      = 10 * strategies_norm,
    Objectives_0_10      = 10 * objectives_norm,
    InterInstitutionalRelations_0_10 = 10 * relations_norm,
    VerticalCoordination_0_10 = 10 * vertical_norm,
    HorizontalCoordination_0_10 = 10 * horizontal_norm,
    Embeddedness_0_10    = 10 * embeddedness_norm,
    AdaptiveCapacity_0_10= 10 * adaptive_capacity_norm,
    NicheSpecialisation_0_10 = 10 * niche_specialisation_norm
  )

# Analysis dataset for this section (released for transparency)
analysis_df <- df %>%
  select(
    IGO,
    EEI_z, LEF_z,
    SpatialScope_0_10, SubjectScope_0_10, LegalInstruments_0_10,
    Strategies_0_10, Objectives_0_10,
    InterInstitutionalRelations_0_10,
    VerticalCoordination_0_10, HorizontalCoordination_0_10,
    Embeddedness_0_10, AdaptiveCapacity_0_10, NicheSpecialisation_0_10,
    Age_2025_years, FoundingDensity_5yr_count, CumulativeStock_count
  )

write.csv(analysis_df, file = here(out_tbl, "analysis_dataset_4_2_6.csv"), row.names = FALSE)

# ----------------------------
# Table 4.A1: Descriptives
# ----------------------------
desc <- analysis_df %>%
  select(-IGO) %>%
  summarise(across(everything(), list(
    N = ~sum(!is.na(.)),
    Mean = ~mean(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  ))) %>%
  pivot_longer(cols = everything(),
               names_to = c("Variable", ".value"),
               names_sep = "_") %>%
  arrange(match(Variable, names(select(analysis_df, -IGO))))

# write to Excel with a minimal data dictionary sheet
wb1 <- createWorkbook()
addWorksheet(wb1, "Table_4A1_Descriptives")
writeData(wb1, "Table_4A1_Descriptives", desc)

addWorksheet(wb1, "Data_Dictionary")
dict <- tibble::tribble(
  ~Variable, ~Definition, ~Scale,
  "EEI_z", "Endurance/Institutionalisation Index, z-scored weighted composite (see script parameters).", "z-score",
  "LEF_z", "Legitimacy–Efficacy proxy, z-scored LPI_raw computed from mean AcrossIGO scores in the relations workbook.", "z-score",
  "SpatialScope_0_10", "Spatial scope (0–10) derived from min–max normalised Ordinal_Score in Spatial Jurisdiction Matrix_Wide.", "0–10",
  "SubjectScope_0_10", "Subject-matter scope (0–10) derived from min–max normalised Ordinal_Score in Subject Matter Matrix_Wide.", "0–10",
  "LegalInstruments_0_10", "Legal instruments breadth (0–10) from min–max normalised Ordinal_Score in legal sources Matrix_Wide.", "0–10",
  "Strategies_0_10", "Strategy/functional repertoire (0–10) from min–max normalised Ordinal_Score.", "0–10",
  "Objectives_0_10", "Objectives repertoire (0–10) from min–max normalised Ordinal_Score.", "0–10",
  "InterInstitutionalRelations_0_10", "Inter-institutional relationships/engagement (0–10) from min–max normalised Ordinal_Score.", "0–10",
  "VerticalCoordination_0_10", "Vertical coordination (0–10) from min–max normalised Ordinal_Score.", "0–10",
  "HorizontalCoordination_0_10", "Horizontal coordination (0–10) from min–max normalised Ordinal_Score.", "0–10",
  "Embeddedness_0_10", "Embeddedness composite (0–10) computed from relations, vertical and horizontal components.", "0–10",
  "AdaptiveCapacity_0_10", "Adaptive capacity proxy (0–10) computed as mean of Strategies and Objectives components.", "0–10",
  "NicheSpecialisation_0_10", "Niche specialisation (0–10) computed as inverse of mean scope across spatial, subject, strategies, objectives.", "0–10",
  "Age_2025_years", "Organisational age in 2025 (2025 − founding year).", "years",
  "FoundingDensity_5yr_count", "Number of IGOs founded within ±2 years of focal founding year.", "count",
  "CumulativeStock_count", "Number of IGOs founded up to and including focal founding year.", "count"
)
writeData(wb1, "Data_Dictionary", dict)

addWorksheet(wb1, "Notes")
writeData(wb1, "Notes", data.frame(
  Notes = c(
    "Descriptives use variable-specific N (missingness handled per variable).",
    "Indices are on 0–10 scales unless otherwise noted; EEI_z and LEF_z are standardised (mean 0, SD 1)."
  )
))

saveWorkbook(wb1, here(out_tbl, "Table_4A1_Descriptives.xlsx"), overwrite = TRUE)

# ----------------------------
# Table 4.A2: Correlations (Pearson), ordered by hierarchical clustering
# ----------------------------
X <- analysis_df %>% select(-IGO)
rc <- Hmisc::rcorr(as.matrix(X), type = params$cor_method)
r  <- rc$r
p  <- rc$P

# hierarchical ordering for readability
hc <- hclust(as.dist(1 - r), method = "average")
ord <- hc$order
r_ord <- r[ord, ord]
p_ord <- p[ord, ord]

# significance stars
star_fun <- function(pval) {
  if (is.na(pval)) return("")
  if (pval < params$sig_levels["***"]) return("***")
  if (pval < params$sig_levels["**"])  return("**")
  if (pval < params$sig_levels["*"])   return("*")
  ""
}

r_star <- matrix("", nrow = nrow(r_ord), ncol = ncol(r_ord),
                 dimnames = dimnames(r_ord))
for (i in seq_len(nrow(r_ord))) {
  for (j in seq_len(ncol(r_ord))) {
    r_star[i, j] <- sprintf("%.2f%s", r_ord[i, j], ifelse(i == j, "", star_fun(p_ord[i, j])))
  }
}

wb2 <- createWorkbook()
addWorksheet(wb2, "Table_4A2_Correlations")
writeData(wb2, "Table_4A2_Correlations", data.frame(Variable = rownames(r_star), r_star, check.names = FALSE))

addWorksheet(wb2, "P_values")
writeData(wb2, "P_values", data.frame(Variable = rownames(p_ord), p_ord, check.names = FALSE))

addWorksheet(wb2, "Notes")
writeData(wb2, "Notes", data.frame(
  Notes = c(
    "Pearson correlations computed with pairwise complete observations (Hmisc::rcorr).",
    "Stars indicate p-values: * p<0.05, ** p<0.01, *** p<0.001.",
    "Variables are reordered using hierarchical clustering on distance (1 − r) for readability."
  )
))

saveWorkbook(wb2, here(out_tbl, "Table_4A2_Correlations.xlsx"), overwrite = TRUE)

# Heatmap figure (optional visual diagnostic)
cor_long <- as.data.frame(as.table(r_ord)) %>%
  rename(Var1 = Var1, Var2 = Var2, r = Freq)

p_heat <- ggplot(cor_long, aes(x = Var2, y = Var1, fill = r)) +
  geom_tile() +
  scale_fill_gradient2(limits = c(-1, 1)) +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "r", title = "Correlation structure (Pearson r)")

ggsave(filename = here(out_fig, "Fig_4A1_CorrelationHeatmap.png"),
       plot = p_heat, width = 9, height = 7, dpi = 300)

# ----------------------------
# Run log (parameters + sessionInfo)
# ----------------------------
log_file <- here(out_log, "runlog_4_2_6.txt")
cat("Chapter 4 | Section 4.2.6 | Descriptives & correlation structure\n",
    "Timestamp: ", format(Sys.time(), tz = "UTC"), " UTC\n\n",
    "Parameters:\n", sep = "", file = log_file)
capture.output(str(params), file = log_file, append = TRUE)
cat("\n\nsessionInfo():\n", file = log_file, append = TRUE)
capture.output(sessionInfo(), file = log_file, append = TRUE)

message("Done. Outputs written to: ", normalizePath(here("outputs")))
