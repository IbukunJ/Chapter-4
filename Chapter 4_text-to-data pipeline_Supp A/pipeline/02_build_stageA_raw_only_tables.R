#!/usr/bin/env Rscript

# Stage A (RAW-only) attribute tables
#
# This script constructs first-round evidence-linked tables from the RAW
# verbatim texts only, using deterministic lexicon matching defined in
# `schema/category_schema.csv`.
#
# Outputs (per family):
#  - outputs/tables/stageA_<family>.xlsx
#  - outputs/data/stageA_<family>_matrix_wide.csv

source("pipeline/00_project_options.R")
source("R/utils.R")

assert_pkg(c("dplyr", "tidyr", "purrr", "stringr", "readr"))

schema <- read_schema("schema/category_schema.csv")
fam_map <- family_file_map()

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/data", recursive = TRUE, showWarnings = FALSE)

for (fam in names(fam_map)) {
  message("[Stage A] Processing family: ", fam)
  cfg <- fam_map[[fam]]
  dat <- read_raw_clean(cfg$raw, cfg$clean, cfg$raw_col, cfg$clean_col)
  schema_f <- schema |> dplyr::filter(attribute_family == fam)

  # Clean text and tokenise for keyword frequency evidence
  dat <- dat |>
    dplyr::mutate(
      Text_Clean = clean_text(Verbatim),
      Tokens = tokenise_no_stopwords(Verbatim)
    )

  # Match categories deterministically from RAW text
  matches <- lapply(dat$Text_Clean, match_categories_from_text, schema_family = schema_f)
  cats <- schema_f$category_label

  # Build raw frequency matrix
  raw_freq_mat <- do.call(rbind, lapply(matches, function(m) as.integer(m$raw_freq_map[cats])))
  colnames(raw_freq_mat) <- cats
  rownames(raw_freq_mat) <- dat$Institution
  tf <- compute_tfidf_by_category(raw_freq_mat)

  # Presence and ordinal score are derived from RAW-only matches
  present_mat <- raw_freq_mat > 0
  ordinal_score <- rowSums(present_mat)

  # Hybrid scores (presence + capped intensity + tier bonus)
  tier <- schema_f$tier_bonus
  names(tier) <- cats
  bounded <- pmin(raw_freq_mat, 5)
  presence_bonus <- 1L * present_mat
  tier_bonus <- matrix(rep(tier, each = nrow(raw_freq_mat)), nrow = nrow(raw_freq_mat)) * presence_bonus
  hybrid <- presence_bonus + bounded + tier_bonus

  norm <- dual_normalise(hybrid)

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
      Presence_Bonus = as.integer(presence_bonus[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Tier_Bonus = as.integer(tier_bonus[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Hybrid_Score_Raw = as.numeric(hybrid[cbind(match(Institution, rownames(raw_freq_mat)), match(Category, cats))]),
      Ordinal_Score = as.integer(ordinal_score[match(Institution, dat$Institution)]),
      Override_Flag = ifelse(Presence_Bonus == 1, "RAW_only", "NONE"),
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

  # Wide matrix for modelling
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

  # Data dictionary (workbook-level; embedded as first sheet)
  dict <- build_dictionary_from_sheets(
    workbook_name = paste0("stageA_", fam, ".xlsx"),
    sheets_named_list = list(Traceability_Long = long, Matrix_Wide = wide)
  )

  out_xlsx <- file.path("outputs/tables", paste0("stageA_", fam, ".xlsx"))
  write_workbook_with_dictionary(out_xlsx, list(Traceability_Long = long, Matrix_Wide = wide), dict)

  readr::write_csv(wide, file.path("outputs/data", paste0("stageA_", fam, "_matrix_wide.csv")))
}

message("Stage A completed.")
