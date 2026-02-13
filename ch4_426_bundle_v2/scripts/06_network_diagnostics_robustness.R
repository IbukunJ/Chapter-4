# 06_network_diagnostics_robustness.R
#
# Chapter 4 replication bundle — Section 4.2.6 add-on:
#   Table 4.A3 — Network diagnostics (IGO–IGO projection from relations/coordination)
#   Table 4.R1 — Robustness grid (specification stability across C1–C3)
#
# Inputs: FULL-normalised Matrix_Wide workbooks (AcrossIGO scores) + analysis_dataset_4_2_6.csv
# Outputs written to: outputs/tables/ and outputs/logs/
#
# Required packages:
#   readxl, dplyr, tidyr, stringr, purrr, tibble, writexl
#   igraph (network diagnostics)
#   broom, sandwich, lmtest (robustness grid / HC3 robust SE)
#
# NOTE: This script is deterministic given fixed inputs. Set.seed() is used only to stabilise
#       any stochastic internals (if present) of community detection methods.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(writexl)
  library(readr)
  library(igraph)
  library(broom)
  library(sandwich)
  library(lmtest)
})

# ---------------------------
# Paths
# ---------------------------
inputs_dir  <- file.path("inputs")
scripts_dir <- file.path("scripts")
out_tables  <- file.path("outputs", "tables")
out_logs    <- file.path("outputs", "logs")
dir.create(out_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_logs, recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Helpers
# ---------------------------
read_matrix_wide_across <- function(path) {
  df <- readxl::read_excel(path, sheet = "Matrix_Wide")
  names(df) <- str_trim(names(df))
  inst_col <- if ("Institution" %in% names(df)) "Institution" else names(df)[1]
  across_cols <- names(df)[str_ends(names(df), "_AcrossIGO")]
  df %>%
    select(all_of(inst_col), all_of(across_cols)) %>%
    rename(Institution = all_of(inst_col))
}

cosine_similarity <- function(X) {
  # X: numeric matrix (n x p)
  X[is.na(X)] <- 0
  norms <- sqrt(rowSums(X^2))
  norms[norms == 0] <- 1e-9
  sim <- (X %*% t(X)) / (norms %o% norms)
  diag(sim) <- 0
  sim
}

minmax_0_10 <- function(x, eps = 1e-9) {
  rng <- range(x, na.rm = TRUE)
  if (isTRUE(all.equal(rng[1], rng[2]))) return(rep(0, length(x)))
  10 * (x - rng[1]) / (rng[2] - rng[1] + eps)
}

winsorise_vec <- function(x, p = 0.05) {
  qs <- quantile(x, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

extract_term_stats <- function(model, terms) {
  # model: lm object
  co <- coef(summary(model))
  out <- tibble(
    term = terms,
    estimate = NA_real_,
    p.value = NA_real_
  )
  for (t in terms) {
    if (t %in% rownames(co)) {
      out[out$term == t, "estimate"] <- co[t, "Estimate"]
      out[out$term == t, "p.value"]  <- co[t, "Pr(>|t|)"]
    }
  }
  out
}

extract_term_stats_hc3 <- function(model, terms) {
  # HC3 robust p-values
  ct <- lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC3"))
  out <- tibble(
    term = terms,
    estimate = NA_real_,
    p.value = NA_real_
  )
  for (t in terms) {
    if (t %in% rownames(ct)) {
      out[out$term == t, "estimate"] <- ct[t, "Estimate"]
      out[out$term == t, "p.value"]  <- ct[t, "Pr(>|t|)"]
    }
  }
  out
}

classify_stability <- function(base_stats, var_stats, alpha = 0.05) {
  # base_stats, var_stats: tibble(term, estimate, p.value)
  merged <- base_stats %>%
    rename(base_est = estimate, base_p = p.value) %>%
    left_join(var_stats %>% rename(var_est = estimate, var_p = p.value), by = "term")

  sign_same <- all(sign(merged$base_est) == sign(merged$var_est) | is.na(merged$var_est) | is.na(merged$base_est))
  sig_same  <- all((merged$base_p < alpha) == (merged$var_p < alpha) | is.na(merged$var_p) | is.na(merged$base_p))

  if (!sign_same) return("Unstable")
  if (sign_same && sig_same) return("Stable")
  "Partially stable"
}

# ============================================================
# Table 4.A3 — Network diagnostics (IGO–IGO projection)
# ============================================================

# Input files (as used in Section 4.2.6 pipeline)
f_inter <- file.path(inputs_dir, "Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx")
f_vert  <- file.path(inputs_dir, "Vertical_Coordination_FULL_Normalised.xlsx")
f_horiz <- file.path(inputs_dir, "Horizontal_Coordination_FULL_Normalised_REAL.xlsx")

rel_inter <- read_matrix_wide_across(f_inter)
rel_vert  <- read_matrix_wide_across(f_vert)
rel_horiz <- read_matrix_wide_across(f_horiz)

# Merge (inner join to keep consistent IGO set)
rel_all <- rel_inter %>%
  inner_join(rel_vert,  by = "Institution") %>%
  inner_join(rel_horiz, by = "Institution")

X <- rel_all %>%
  select(-Institution) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.matrix()

sim <- cosine_similarity(X)

# Threshold edges at the global 90th percentile of pairwise similarities (upper triangle)
upper_vals <- sim[upper.tri(sim)]
edge_q <- 0.90
edge_thr <- as.numeric(quantile(upper_vals, probs = edge_q, na.rm = TRUE))

# Build edge list
inst <- rel_all$Institution
edge_idx <- which(sim >= edge_thr & upper.tri(sim), arr.ind = TRUE)
edge_list <- tibble(
  source = inst[edge_idx[, 1]],
  target = inst[edge_idx[, 2]],
  weight = as.numeric(sim[edge_idx])
) %>%
  mutate(
    similarity_metric = "cosine",
    edge_threshold_quantile = edge_q,
    edge_threshold_value = edge_thr
  )

# Graph
g <- igraph::graph_from_data_frame(edge_list, directed = FALSE, vertices = tibble(name = inst))
E(g)$distance <- 1 / pmax(E(g)$weight, 1e-9)

# Centralities
deg <- igraph::degree(g, mode = "all")
strg <- igraph::strength(g, weights = E(g)$weight)
eig <- igraph::eigen_centrality(g, weights = E(g)$weight, directed = FALSE)$vector
if (max(eig, na.rm = TRUE) > 0) eig <- eig / max(eig, na.rm = TRUE)

bet <- igraph::betweenness(g, weights = E(g)$distance, normalized = TRUE)

# Communities: greedy modularity (deterministic for an undirected graph)
# (If you prefer Louvain, replace with cluster_louvain(g, weights = E(g)$weight))
comm <- igraph::cluster_fast_greedy(g, weights = E(g)$weight)
comm_id <- igraph::membership(comm)

tab_4A3 <- tibble(
  IGO = names(deg),
  degree = as.integer(deg),
  strength = as.numeric(strg),
  eigenvector = as.numeric(eig),
  betweenness = as.numeric(bet),
  community = as.integer(comm_id[names(deg)])
) %>%
  mutate(
    rank_eigenvector = rank(-eigenvector, ties.method = "min"),
    similarity_metric = "cosine",
    edge_threshold_quantile = edge_q,
    edge_threshold_value = edge_thr
  ) %>%
  arrange(rank_eigenvector, IGO)

# Export edge list for audit
readr::write_csv(edge_list, file.path(out_tables, "network_edge_list_cosine_q90.csv"))
readr::write_csv(tab_4A3,  file.path(out_tables, "Table_4A3_Network_Diagnostics.csv"))

# Workbook with Data_Dictionary + Notes
dd_4A3 <- tribble(
  ~Variable, ~Definition, ~Type, ~`Provenance/Computation`,
  "IGO", "Intergovernmental organisation identifier (name as in matrix-wide inputs).", "character", "Matrix_Wide: Institution",
  "degree", "Number of retained similarity ties in the thresholded IGO–IGO network.", "integer", "Count incident edges after thresholding",
  "strength", "Sum of cosine-similarity weights of retained ties.", "numeric", "Sum of incident edge weights",
  "eigenvector", "Eigenvector centrality on weighted adjacency (scaled so max=1).", "numeric", "Principal eigenvector of weighted adjacency",
  "betweenness", "Betweenness on weighted shortest paths using distance = 1/weight (normalised).", "numeric", "Shortest-path betweenness with inverse-weight distances",
  "community", "Community assignment from greedy modularity clustering on weighted graph.", "integer", "cluster_fast_greedy()",
  "rank_eigenvector", "Rank of eigenvector centrality (1=highest).", "integer", "Descending rank",
  "similarity_metric", "Similarity metric used for projection.", "character", "Fixed: cosine",
  "edge_threshold_quantile", "Quantile used to threshold similarities into edges.", "numeric", "Fixed: 0.90",
  "edge_threshold_value", "Cosine similarity cutoff; edges retained if weight ≥ cutoff.", "numeric", "Computed from upper-triangular similarities"
)

notes_4A3 <- tribble(
  ~Item, ~Description,
  "Projection basis", "Concatenated AcrossIGO (0–10) category profiles from InterInstitutionalRelations, VerticalCoordination, HorizontalCoordination (30 dimensions).",
  "Similarity and threshold", "Cosine similarity computed between all IGO pairs; edges retained if similarity ≥ global 90th percentile (upper triangle).",
  "Edge list", "Retained edges and weights exported to network_edge_list_cosine_q90.csv.",
  "Community detection", "Greedy modularity maximisation on weighted graph (cluster_fast_greedy)."
)

write_xlsx(
  list(
    "Table_4A3_Network_Diagnostics" = tab_4A3,
    "Data_Dictionary" = dd_4A3,
    "Notes" = notes_4A3
  ),
  file.path(out_tables, "Table_4A3_Network_Diagnostics.xlsx")
)

# ============================================================
# Table 4.R1 — Robustness grid (C1–C3)
# ============================================================

analysis_path <- file.path(out_tables, "analysis_dataset_4_2_6.csv")
dat <- readr::read_csv(analysis_path, show_col_types = FALSE)

# Representative baseline models (one per conjecture)
formulas <- list(
  C1 = EEI_z ~ SpatialScope_0_10 + SubjectScope_0_10 + LegalInstruments_0_10 + Embeddedness_0_10 + Age_2025_years,
  C2 = NicheSpecialisation_0_10 ~ FoundingDensity_5yr_count + CumulativeStock_count,
  C3 = LEF_z ~ LegalInstruments_0_10 * Embeddedness_0_10
)

key_terms <- list(
  C1 = c("SpatialScope_0_10", "SubjectScope_0_10", "LegalInstruments_0_10", "Embeddedness_0_10"),
  C2 = c("FoundingDensity_5yr_count"),
  C3 = c("LegalInstruments_0_10", "Embeddedness_0_10", "LegalInstruments_0_10:Embeddedness_0_10")
)

fit_baseline <- list(
  C1 = lm(formulas$C1, data = dat),
  C2 = lm(formulas$C2, data = dat),
  C3 = lm(formulas$C3, data = dat)
)

base_stats <- list(
  C1 = extract_term_stats(fit_baseline$C1, key_terms$C1),
  C2 = extract_term_stats(fit_baseline$C2, key_terms$C2),
  C3 = extract_term_stats(fit_baseline$C3, key_terms$C3)
)

# Robustness levers
robustness_rows <- list()

# Lever 1: HC3 robust SE
hc3_stats <- list(
  C1 = extract_term_stats_hc3(fit_baseline$C1, key_terms$C1),
  C2 = extract_term_stats_hc3(fit_baseline$C2, key_terms$C2),
  C3 = extract_term_stats_hc3(fit_baseline$C3, key_terms$C3)
)

robustness_rows[["HC3 robust SE"]] <- tibble(
  Robustness_lever = "HC3 robust SE",
  C1 = classify_stability(base_stats$C1, hc3_stats$C1),
  C2 = classify_stability(base_stats$C2, hc3_stats$C2),
  C3 = classify_stability(base_stats$C3, hc3_stats$C3),
  Notes = "Heteroskedasticity-consistent (HC3) standard errors for OLS."
)

# Lever 2: Winsorised 5–95%
dat_w <- dat %>%
  mutate(across(where(is.numeric), ~ winsorise_vec(.x, p = 0.05)))

fit_w <- list(
  C1 = lm(formulas$C1, data = dat_w),
  C2 = lm(formulas$C2, data = dat_w),
  C3 = lm(formulas$C3, data = dat_w)
)

w_stats <- list(
  C1 = extract_term_stats(fit_w$C1, key_terms$C1),
  C2 = extract_term_stats(fit_w$C2, key_terms$C2),
  C3 = extract_term_stats(fit_w$C3, key_terms$C3)
)

robustness_rows[["Winsorised 5–95%"]] <- tibble(
  Robustness_lever = "Winsorised 5–95%",
  C1 = classify_stability(base_stats$C1, w_stats$C1),
  C2 = classify_stability(base_stats$C2, w_stats$C2),
  C3 = classify_stability(base_stats$C3, w_stats$C3),
  Notes = "Clips numeric variables at 5th/95th percentiles."
)

# Lever 3: Drop top Cook's D (n=1)
drop_top_cook <- function(model, data, n = 1) {
  cd <- cooks.distance(model)
  drop_idx <- order(cd, decreasing = TRUE)[seq_len(n)]
  data[-drop_idx, , drop = FALSE]
}

dat_c1 <- drop_top_cook(fit_baseline$C1, dat, n = 1)
dat_c2 <- drop_top_cook(fit_baseline$C2, dat, n = 1)
dat_c3 <- drop_top_cook(fit_baseline$C3, dat, n = 1)

fit_cook <- list(
  C1 = lm(formulas$C1, data = dat_c1),
  C2 = lm(formulas$C2, data = dat_c2),
  C3 = lm(formulas$C3, data = dat_c3)
)

cook_stats <- list(
  C1 = extract_term_stats(fit_cook$C1, key_terms$C1),
  C2 = extract_term_stats(fit_cook$C2, key_terms$C2),
  C3 = extract_term_stats(fit_cook$C3, key_terms$C3)
)

robustness_rows[["Drop top Cook's D (n=1)"]] <- tibble(
  Robustness_lever = "Drop top Cook's D (n=1)",
  C1 = classify_stability(base_stats$C1, cook_stats$C1),
  C2 = classify_stability(base_stats$C2, cook_stats$C2),
  C3 = classify_stability(base_stats$C3, cook_stats$C3),
  Notes = "Re-estimates each model after removing the highest-influence observation."
)

# Lever 4: Density window ±3 years (7-year window)
# Founding year reconstructed from Age_2025 (YearFounded = 2025 - Age_2025_years).
dat_dens <- dat %>%
  mutate(YearFounded_est = round(2025 - Age_2025_years)) %>%
  group_by(dummy = 1) %>%
  mutate(
    FoundingDensity_7yr_count = purrr::map_int(YearFounded_est, ~ sum(abs(YearFounded_est - .x) <= 3))
  ) %>%
  ungroup() %>%
  select(-dummy)

fit_dens <- list(
  C1 = lm(formulas$C1, data = dat_dens),
  C2 = lm(NicheSpecialisation_0_10 ~ FoundingDensity_7yr_count + CumulativeStock_count, data = dat_dens),
  C3 = lm(formulas$C3, data = dat_dens)
)

dens_stats <- list(
  C1 = extract_term_stats(fit_dens$C1, key_terms$C1),
  C2 = extract_term_stats(fit_dens$C2, c("FoundingDensity_7yr_count")),
  C3 = extract_term_stats(fit_dens$C3, key_terms$C3)
)

# Compare density term sign/sig to baseline FoundingDensity_5yr
base_C2_alt <- tibble(term = "FoundingDensity_7yr_count",
                      estimate = base_stats$C2$estimate[base_stats$C2$term == "FoundingDensity_5yr_count"],
                      p.value  = base_stats$C2$p.value[base_stats$C2$term == "FoundingDensity_5yr_count"])

robustness_rows[["Density window ±3 years"]] <- tibble(
  Robustness_lever = "Density window ±3 years",
  C1 = classify_stability(base_stats$C1, dens_stats$C1),
  C2 = classify_stability(base_C2_alt, dens_stats$C2),
  C3 = classify_stability(base_stats$C3, dens_stats$C3),
  Notes = "Replaces FoundingDensity_5yr with FoundingDensity_7yr (±3 years)."
)

# Lever 5: Replace composite embeddedness with network eigenvector centrality
dat_net <- dat %>%
  left_join(tab_4A3 %>% select(IGO, eigenvector) %>% rename(NetEigenvector = eigenvector),
            by = c("IGO" = "IGO"))

fit_net <- list(
  C1 = lm(EEI_z ~ SpatialScope_0_10 + SubjectScope_0_10 + LegalInstruments_0_10 + NetEigenvector + Age_2025_years, data = dat_net),
  C2 = lm(formulas$C2, data = dat_net),
  C3 = lm(LEF_z ~ LegalInstruments_0_10 * NetEigenvector, data = dat_net)
)

net_stats <- list(
  C1 = extract_term_stats(fit_net$C1, c("SpatialScope_0_10", "SubjectScope_0_10", "LegalInstruments_0_10", "NetEigenvector")),
  C2 = extract_term_stats(fit_net$C2, key_terms$C2),
  C3 = extract_term_stats(fit_net$C3, c("LegalInstruments_0_10", "NetEigenvector", "LegalInstruments_0_10:NetEigenvector"))
)

# Compare NetEigenvector to baseline Embeddedness_0_10 as the conceptual analogue
base_C1_net <- base_stats$C1 %>%
  mutate(term = ifelse(term == "Embeddedness_0_10", "NetEigenvector", term))
base_C3_net <- base_stats$C3 %>%
  mutate(term = case_when(
    term == "Embeddedness_0_10" ~ "NetEigenvector",
    term == "LegalInstruments_0_10:Embeddedness_0_10" ~ "LegalInstruments_0_10:NetEigenvector",
    TRUE ~ term
  ))

robustness_rows[["Replace composite embeddedness with network eigenvector"]] <- tibble(
  Robustness_lever = "Replace composite embeddedness with network eigenvector",
  C1 = classify_stability(base_C1_net, net_stats$C1),
  C2 = classify_stability(base_stats$C2, net_stats$C2),
  C3 = classify_stability(base_C3_net, net_stats$C3),
  Notes = "Uses Table 4.A3 eigenvector centrality as an alternative embeddedness proxy."
)

tab_4R1 <- bind_rows(robustness_rows)

readr::write_csv(tab_4R1, file.path(out_tables, "Table_4R1_Robustness_Grid.csv"))

dd_4R1 <- tribble(
  ~Variable, ~Definition, ~Type, ~`Provenance/Computation`,
  "Robustness_lever", "Specification lever varied relative to baseline models (C1–C3).", "character", "Defined in this script",
  "C1", "Stability classification for Conjecture 1 representative model under the lever.", "character", "Stable / Partially stable / Unstable",
  "C2", "Stability classification for Conjecture 2 representative model under the lever.", "character", "Stable / Partially stable / Unstable",
  "C3", "Stability classification for Conjecture 3 representative model under the lever.", "character", "Stable / Partially stable / Unstable",
  "Notes", "Short notes on the lever or interpretation.", "character", "Free text"
)

notes_4R1 <- tribble(
  ~Item, ~Description,
  "Baseline models", "C1: EEI_z ~ SpatialScope + SubjectScope + LegalInstruments + Embeddedness + Age_2025. C2: NicheSpecialisation ~ FoundingDensity_5yr + CumulativeStock. C3: LEF_z ~ LegalInstruments * Embeddedness.",
  "Key terms tracked", "C1: SpatialScope, SubjectScope, LegalInstruments, Embeddedness. C2: FoundingDensity. C3: LegalInstruments, Embeddedness, and their interaction.",
  "Stability rule", "Stable = same coefficient sign and same significance class (p<0.05) for all tracked terms. Partially stable = sign consistent but at least one term changes significance class. Unstable = at least one tracked term changes sign."
)

write_xlsx(
  list(
    "Table_4R1_Robustness_Grid" = tab_4R1,
    "Data_Dictionary" = dd_4R1,
    "Notes" = notes_4R1
  ),
  file.path(out_tables, "Table_4R1_Robustness_Grid.xlsx")
)

# ---------------------------
# Run log
# ---------------------------
runlog <- c(
  paste0("Script: 06_network_diagnostics_robustness.R"),
  paste0("Timestamp (UTC): ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("Similarity metric: cosine"),
  paste0("Edge threshold quantile: ", edge_q),
  paste0("Edge threshold value: ", signif(edge_thr, 6)),
  paste0("Community method: cluster_fast_greedy (weighted)"),
  paste0("Robust SE: HC3 (sandwich::vcovHC type='HC3')"),
  paste0("Winsorisation: 5th/95th percentiles"),
  paste0("Cook's distance removal: top 1 observation per model"),
  paste0("Density alternative window: ±3 years (FoundingDensity_7yr_count)")
)

writeLines(runlog, con = file.path(out_logs, "runlog_4_2_6_network_robustness.txt"))

message("Done. Wrote Table 4.A3 and Table 4.R1 outputs to outputs/tables/.")
