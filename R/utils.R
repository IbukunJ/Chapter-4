# Utility functions for Chapter 4 reproducible pipeline

# NOTE: These utilities are intentionally lightweight and dependency-minimal.
# They implement the core operations described in Chapter 4:
#  - text standardisation
#  - evidence-linked lexicon matching with KWIC
#  - TF–IDF computation
#  - hybrid scoring + dual normalisation
#  - writing Excel workbooks with embedded Data_Dictionary sheets

assert_pkg <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "), "\n",
      "Install them (recommended via renv) then re-run.",
      call. = FALSE
    )
  }
}

clean_text <- function(x) {
  assert_pkg(c("stringr"))
  x <- as.character(x)
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "https?://\\S+", " ")
  x <- stringr::str_replace_all(x, "www\\.\\S+", " ")
  x <- stringr::str_replace_all(x, "[^a-z0-9\\s]", " ")
  x <- stringr::str_squish(x)
  x
}

tokenise_no_stopwords <- function(x, language = "en") {
  assert_pkg(c("stringr", "stopwords"))
  x <- clean_text(x)
  toks <- stringr::str_split(x, "\\s+", simplify = FALSE)
  sw <- stopwords::stopwords(language)
  toks <- lapply(toks, function(v) v[nchar(v) > 1 & !(v %in% sw)])
  toks
}

freq_map <- function(tokens, top_n = 200) {
  # tokens: character vector
  if (length(tokens) == 0) return(setNames(integer(0), character(0)))
  tab <- sort(table(tokens), decreasing = TRUE)
  if (length(tab) > top_n) tab <- tab[seq_len(top_n)]
  as.integer(tab) |> setNames(names(tab))
}

freq_map_to_py_dict <- function(freq_named_int) {
  # Write a stable, Excel-friendly representation similar to the existing outputs.
  if (length(freq_named_int) == 0) return("{}")
  items <- paste0("'", names(freq_named_int), "': ", as.integer(freq_named_int))
  paste0("{", paste(items, collapse = ", "), "}")
}

kwic_first <- function(text, pattern, window = 40) {
  # Returns a single KWIC snippet for first match; empty string if none.
  assert_pkg(c("stringr"))
  m <- stringr::str_locate(text, pattern)
  if (is.na(m[1,1])) return("")
  start <- max(1, m[1,1] - window)
  end <- min(nchar(text), m[1,2] + window)
  stringr::str_sub(text, start, end)
}

read_schema <- function(path = "schema/category_schema.csv") {
  assert_pkg(c("readr", "dplyr"))
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::mutate(
      attribute_family = as.character(attribute_family),
      category_label = as.character(category_label),
      category_slug = as.character(category_slug),
      example_triggers = dplyr::coalesce(as.character(example_triggers), ""),
      tier_bonus = dplyr::coalesce(as.integer(tier_bonus), 0L)
    )
}

family_file_map <- function() {
  # Central map used by the pipeline; file names match the bundle.
  list(
    spatial_jurisdiction = list(raw = "data/raw/Spatial Jurisdiction_RAW.xlsx", clean = "data/clean/Spatial Juriscation_Clean.xlsx", raw_col = "Spatial Jurisdiction", clean_col = "Spatial Jurisdiction"),
    subject_matter_jurisdiction = list(raw = "data/raw/Subject Matter Juris_RAW.xlsx", clean = "data/clean/Subject Matter Juris_Clean.xlsx", raw_col = "Subject Matter Jurisdiction", clean_col = "Subject Matter Jurisdiction"),
    sources_of_jurisdiction = list(raw = "data/raw/Sources of Juridiction_RAW.xlsx", clean = "data/clean/Sources of Jurisdiction_Clean.xlsx", raw_col = "Sources of Jurisdiction", clean_col = "Sources of Jurisdiction"),
    defined_objectives = list(raw = "data/raw/Defined Objective_RAW.xlsx", clean = "data/clean/Defined Objectives_Clean.xlsx", raw_col = "Defined Objectives", clean_col = "Defined Objectives"),
    strategies = list(raw = "data/raw/Strategies_RAW.xlsx", clean = "data/clean/Strategies_Clean.xlsx", raw_col = "Strategies", clean_col = "Strategies"),
    defined_interinstitutional_relationships = list(raw = "data/raw/Define Int_Inst Rel_RAW.xlsx", clean = "data/clean/Define Int_Inst Rel_Clean.xlsx", raw_col = "Defined Inter-institutional Relationships", clean_col = "Defined Inter-institutional Relationships"),
    vertical_coordination = list(raw = "data/raw/Vertical Coordination_RAW.xlsx", clean = "data/clean/Vertical Coordination_Clean.xlsx", raw_col = "Vertical Coordination", clean_col = "Vertical Coordination"),
    horizontal_coordination = list(raw = "data/raw/Horizontal Coord_RAW.xlsx", clean = "data/clean/Horizontal Coord_Clean.xlsx", raw_col = "Horizontal Coordination", clean_col = "Horizontal Coordination")
  )
}

family_output_map <- function() {
  # Canonical output filenames (match existing bundle naming as closely as possible).
  list(
    spatial_jurisdiction = "Spatial_Jurisdiction_FULL_Normalised_REAL.xlsx",
    subject_matter_jurisdiction = "Subject_Matter_Jurisdiction_FULL_Normalised_REAL.xlsx",
    sources_of_jurisdiction = "Sources_of_Jurisdiction_FULL_Normalised_REAL.xlsx",
    defined_objectives = "Defined_Objectives_FULL_Normalised_REAL.xlsx",
    strategies = "Strategies_FULL_Normalised_REAL.xlsx",
    defined_interinstitutional_relationships = "Defined_InterInstitutional_Relationships_FULL_Normalised_REAL.xlsx",
    vertical_coordination = "Vertical_Coordination_FULL_Normalised.xlsx",
    horizontal_coordination = "Horizontal_Coordination_FULL_Normalised_REAL.xlsx"
  )
}

read_raw_clean <- function(raw_path, clean_path, raw_text_col, clean_cat_col, clean_ordinal_col = "Ordinal Score (0–10)") {
  assert_pkg(c("readxl", "dplyr", "stringr"))
  raw <- readxl::read_excel(raw_path) |>
    dplyr::rename(Institution = 1) |>
    dplyr::mutate(Institution = as.character(Institution))
  if (!raw_text_col %in% names(raw)) {
    # fall back: second column
    raw_text_col <- names(raw)[2]
  }
  raw <- raw |>
    dplyr::transmute(
      Institution,
      Verbatim = as.character(.data[[raw_text_col]])
    )
  clean <- readxl::read_excel(clean_path) |>
    dplyr::rename(Institution = 1) |>
    dplyr::mutate(Institution = as.character(Institution))
  if (!clean_cat_col %in% names(clean)) {
    clean_cat_col <- names(clean)[2]
  }
  if (!clean_ordinal_col %in% names(clean)) {
    # fall back: third column
    clean_ordinal_col <- names(clean)[3]
  }
  clean <- clean |>
    dplyr::transmute(
      Institution,
      Clean_Categories_Raw = as.character(.data[[clean_cat_col]]),
      Ordinal_Score = as.integer(.data[[clean_ordinal_col]])
    )
  raw |>
    dplyr::left_join(clean, by = "Institution")
}

parse_category_list <- function(x) {
  # Parses CLEAN category list into a set of normalised labels.
  assert_pkg(c("stringr"))
  x <- stringr::str_replace_all(as.character(x), "[\\n\\r]", ";")
  parts <- unlist(stringr::str_split(x, "[,;|]"))
  parts <- stringr::str_squish(parts)
  parts <- parts[parts != "" & !is.na(parts)]
  unique(parts)
}

match_categories_from_text <- function(text_clean, schema_family) {
  # Deterministic dictionary matching using example_triggers.
  # Returns a list with: categories_present, trigger_map, kwic_map, raw_freq_map
  assert_pkg(c("stringr"))
  cats <- schema_family$category_label
  trig_str <- schema_family$example_triggers
  res_present <- character(0)
  trig_map <- list()
  kwic_map <- list()
  freq_map_cat <- integer(0)
  names(freq_map_cat) <- character(0)
  for (i in seq_along(cats)) {
    cat <- cats[i]
    triggers <- trig_str[i]
    triggers <- stringr::str_split(triggers, "\\s*\\|\\s*", simplify = FALSE)[[1]]
    triggers <- stringr::str_squish(triggers)
    triggers <- triggers[triggers != "" & !is.na(triggers)]
    # fallback trigger derived from label
    if (length(triggers) == 0) {
      fallback <- stringr::str_to_lower(cat)
      fallback <- stringr::str_replace_all(fallback, "[^a-z0-9\\s]", " ")
      fallback <- stringr::str_squish(fallback)
      triggers <- unique(unlist(stringr::str_split(fallback, "\\s+")))
      triggers <- triggers[nchar(triggers) > 2]
    }
    hits <- character(0)
    kwics <- character(0)
    freq <- 0L
    for (t in triggers) {
      pat <- paste0("\\b", stringr::str_replace_all(t, "\\s+", "\\\\s+"), "\\b")
      n <- stringr::str_count(text_clean, pat)
      if (n > 0) {
        hits <- c(hits, t)
        freq <- freq + as.integer(n)
        if (length(kwics) < 3) {
          kw <- kwic_first(text_clean, pat, window = 40)
          if (kw != "") kwics <- c(kwics, paste0(t, "→", kw))
        }
      }
    }
    if (freq > 0) {
      res_present <- c(res_present, cat)
      trig_map[[cat]] <- unique(hits)
      kwic_map[[cat]] <- unique(kwics)
      freq_map_cat[cat] <- freq
    } else {
      trig_map[[cat]] <- character(0)
      kwic_map[[cat]] <- character(0)
      freq_map_cat[cat] <- 0L
    }
  }
  list(
    categories_present = unique(res_present),
    trigger_map = trig_map,
    kwic_map = kwic_map,
    raw_freq_map = freq_map_cat
  )
}

compute_tfidf_by_category <- function(raw_freq_mat) {
  # raw_freq_mat: matrix [n_igo x n_cat] with integer counts
  N <- nrow(raw_freq_mat)
  df <- colSums(raw_freq_mat > 0)
  idf <- log((N + 1) / (df + 1)) + 1
  tfidf_raw <- sweep(raw_freq_mat, 2, idf, `*`)
  maxv <- max(tfidf_raw, na.rm = TRUE)
  if (!is.finite(maxv) || maxv == 0) {
    tfidf <- tfidf_raw
  } else {
    tfidf <- tfidf_raw / maxv
  }
  list(idf = idf, tfidf = tfidf)
}

dual_normalise <- function(hybrid_mat) {
  # hybrid_mat: matrix [n_igo x n_cat]
  # within-IGO: row-wise minmax to 0–10
  rng_row <- function(x) {
    mn <- min(x, na.rm = TRUE)
    mx <- max(x, na.rm = TRUE)
    if (!is.finite(mn) || !is.finite(mx) || mx == mn) {
      return(rep(0, length(x)))
    }
    (x - mn) / (mx - mn) * 10
  }
  within <- t(apply(hybrid_mat, 1, rng_row))
  # across-IGO: column-wise minmax to 0–10
  rng_col <- function(x) {
    mn <- min(x, na.rm = TRUE)
    mx <- max(x, na.rm = TRUE)
    if (!is.finite(mn) || !is.finite(mx) || mx == mn) {
      return(rep(0, length(x)))
    }
    (x - mn) / (mx - mn) * 10
  }
  across <- apply(hybrid_mat, 2, rng_col)
  list(within = within, across = across)
}

write_workbook_with_dictionary <- function(path, sheets_named_list, dict_df) {
  assert_pkg(c("openxlsx"))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Data_Dictionary")
  openxlsx::writeData(wb, "Data_Dictionary", dict_df)
  for (nm in names(sheets_named_list)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets_named_list[[nm]])
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

build_dictionary_from_sheets <- function(workbook_name, sheets_named_list) {
  assert_pkg(c("dplyr"))
  out <- list()
  for (nm in names(sheets_named_list)) {
    df <- sheets_named_list[[nm]]
    cols <- names(df)
    for (c in cols) {
      v <- df[[c]]
      type <- if (is.numeric(v)) "numeric" else if (is.integer(v)) "integer" else if (inherits(v, "Date")) "date" else "string"
      out[[length(out) + 1]] <- data.frame(
        workbook = workbook_name,
        sheet = nm,
        variable = c,
        type = type,
        description = NA_character_,
        stringsAsFactors = FALSE
      )
    }
  }
  dplyr::bind_rows(out)
}
