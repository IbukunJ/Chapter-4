#!/usr/bin/env Rscript

# Stage B (FULL / CLEAN-integrated) attribute tables
#
# This script integrates:
#  - RAW verbatim texts (for evidence fields: triggers/KWIC/Raw_Freq/TFIDF)
#  - CLEAN category sets (column 2 in each CLEAN workbook) as authoritative
#    presence/activation used for scoring and the family-level ordinal score.
#
# Outputs (per family):
#  - outputs/tables/<family>_FULL_Normalised.xlsx
#  - outputs/data/<family>_matrix_wide.csv

source("pipeline/00_project_options.R")
source("R/utils.R")

assert_pkg(c("dplyr", "tidyr", "purrr", "stringr", "readr"))

schema <- read_schema("schema/category_schema.csv")
fam_map <- family_file_map()
out_map <- family_output_map()

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/data", recursive = TRUE, showWarnings = FALSE)

for (fam in names(fam_map)) {
  message("[Stage B] Processing family: ", fam)
  cfg <- fam_map[[fam]]
  dat <- read_raw_clean(cfg$raw, cfg$clean, cfg$raw_col, cfg$clean_col)
  schema_f <- schema |> dplyr::filter(attribute_family == fam)
  cats <- schema_f$category_label

  dat <- dat |>
    dplyr::mutate(
      Text_Clean = clean_text(Verbatim),
      Tokens = tokenise_no_stopwords(Verbatim),
      Clean_Categories = purrr::map(Clean_Categories_Raw, parse_category_list)
    )

  # AUTO evidence from RAW
  matches <- lapply(dat$Text_Clean, match_categories_from_text, schema_family = schema_f)
  raw_freq_mat <- do.call(rbind, lapply(matches, function(m) as.integer(m$raw_freq_map[cats])))
  colnames(raw_freq_mat) <- cats
  rownames(raw_freq_mat) <- dat$Institution
  tf <- compute_tfidf_by_category(raw_freq_mat)

  # CLEAN authoritative presence
  present_clean <- matrix(0L, nrow = nrow(dat), ncol = length(cats))
  colnames(present_clean) <- cats
  rownames(present_clean) <- dat$Institution
  for (i in seq_len(nrow(dat))) {
    present_clean[i, cats %in% dat$Clean_Categories[[i]]] <- 1L
  }

  # Override flags
  present_raw <- 1L * (raw_freq_mat > 0)
  override <- matrix("NONE", nrow = nrow(dat), ncol = length(cats))
  override[present_raw == 1 & present_clean == 1] <- "BOTH"
  override[present_raw == 0 & present_clean == 1] <- "CLEAN_only"
  override[present_raw == 1 & present_clean == 0] <- "RAW_only"

  # Hybrid scoring (presence + capped intensity + tier bonus)
  tier <- schema_f$tier_bonus
  names(tier) <- cats
  bounded <- pmin(raw_freq_mat, 5)
  tier_bonus <- matrix(rep(tier, each = nrow(raw_freq_mat)), nrow = nrow(raw_freq_mat)) * present_clean
  hybrid <- present_clean + bounded + tier_bonus
  norm <- dual_normalise(hybrid)

  # Family-level ordinal score: use CLEAN where provided; else fall back to clean presence count
  ordinal_score <- dat$Ordinal_Score
  ordinal_score[is.na(ordinal_score)] <- rowSums(present_clean)[is.na(ordinal_score)]

  # Long traceability table
  long <- tidyr::crossing(
    Institution = dat$Institution,
    Category = cats
  ) |>
    dplyr::left_join(
      dat |> dplyr::select(Institution, Verbatim, Tokens),
      by = "Institution"
    ) |>
    dplyr::mutate(
      Attribute = gsub("_", " ", fam),
      `Keywords Extracted (Cleaned & No Stopwords)` = vapply(Tokens, function(t) paste(t, collapse = ", "), character(1)),
      `Keyword Frequency` = vapply(Tokens, function(t) freq_map_to_py_dict(freq_map(t)), character(1)),
      Trigger_Keywords = NA_character_,
      `KWIC Evidence (keywords→snippet)` = NA_character_,
      Raw_Freq = as.integer(raw_freq_mat[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      TFIDF_Weight = as.numeric(tf$tfidf[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Presence_Bonus = as.integer(present_clean[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Tier_Bonus = as.integer(tier_bonus[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Hybrid_Score_Raw = as.numeric(hybrid[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Ordinal_Score = as.integer(ordinal_score[match(Institution, dat$Institution)]),
      Override_Flag = as.character(override[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Score_WithinIGO_0_10 = as.numeric(norm$within[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Score_AcrossIGO_0_10 = as.numeric(norm$across[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))])
    )

  # Fill trigger/kwic fields from match objects
  trig_lookup <- setNames(matches, dat$Institution)
  long <- long |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Trigger_Keywords = paste(trig_lookup[[Institution]]$trigger_map[[Category]], collapse = "; "),
      `KWIC Evidence (keywords→snippet)` = paste(trig_lookup[[Institution]]$kwic_map[[Category]], collapse = " | ")
    ) |>
    dplyr::ungroup()

  # Wide matrix
  within_df <- as.data.frame(norm$within)
  across_df <- as.data.frame(norm$across)
  names(within_df) <- paste0(names(within_df), "_WithinIGO")
  names(across_df) <- paste0(names(across_df), "_AcrossIGO")

  wide <- dplyr::tibble(
    Institution = dat$Institution,
    Ordinal_Score = as.integer(ordinal_score)
  ) |>
    dplyr::bind_cols(within_df) |>
    dplyr::bind_cols(across_df)

  dict <- build_dictionary_from_sheets(
    workbook_name = paste0(fam, "_FULL_Normalised.xlsx"),
    sheets_named_list = list(Traceability_Long = long, Matrix_Wide = wide)
  )

  out_xlsx <- file.path("outputs/tables", out_map[[fam]])
  write_workbook_with_dictionary(out_xlsx, list(Traceability_Long = long, Matrix_Wide = wide), dict)
  readr::write_csv(wide, file.path("outputs/data", paste0(fam, "_matrix_wide.csv")))
}
