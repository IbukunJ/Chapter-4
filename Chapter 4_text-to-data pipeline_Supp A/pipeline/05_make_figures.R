#!/usr/bin/env Rscript

# Regenerates core Chapter 4 descriptive figures used in the thesis methods/results.
#
# Outputs:
#  - outputs/figures/Figure_4_1_Data_pipeline_schematic.png
#  - outputs/figures/Figure_4_2a_IGO_founding_by_era.png
#  - outputs/figures/Figure_4_2b_Founding_density_and_cumulative_stock.png

source("pipeline/00_project_options.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(stringr)
})

hist <- readr::read_csv("outputs/data/historical_context.csv", show_col_types = FALSE)

# ---- Figure 4.2a: founding by era ----
era_counts <- hist |>
  count(founding_era_category, name = "n") |>
  mutate(founding_era_category = factor(founding_era_category, levels = founding_era_category))

p1 <- ggplot(era_counts, aes(x = founding_era_category, y = n)) +
  geom_col() +
  labs(x = NULL, y = "Number of IGOs", title = "Number of IGOs founded by era") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

ggsave("outputs/figures/Figure_4_2a_IGO_founding_by_era.png", p1, width = 10, height = 4.5, dpi = 300)

# ---- Figure 4.2b: founding density and cumulative growth (5-year bins) ----
hist_bins <- hist |>
  mutate(bin5 = floor(year_founded / 5) * 5) |>
  count(bin5, name = "founded") |>
  arrange(bin5) |>
  mutate(cumulative = cumsum(founded))

p2 <- ggplot(hist_bins, aes(x = bin5)) +
  geom_col(aes(y = founded)) +
  geom_line(aes(y = cumulative), linewidth = 1) +
  labs(x = "Founding year (5-year bins)", y = NULL,
       title = "Founding density (bars) and cumulative stock (line)") +
  theme_minimal(base_size = 11)

ggsave("outputs/figures/Figure_4_2b_Founding_density_and_cumulative_stock.png", p2, width = 10, height = 4.5, dpi = 300)

# ---- Figure 4.1: pipeline schematic (lightweight, rendered as a plot) ----
# (A diagram tool can be substituted later; this plot keeps the workflow explicit and reproducible.)
steps <- data.frame(
  step = c(
    "0. Specify schema (8 families × 10 categories) and lexicons",
    "1. Ingest RAW texts (attribute-family workbooks)",
    "2. Standardise texts (clean, tokenise, stopwords)",
    "3. Lexicon matching + KWIC + Raw_Freq + TF–IDF (Stage A)",
    "4. Expert adjudication → CLEAN category sets + ordinal scores",
    "5. Hybrid scoring + dual normalisation (Stage B)",
    "6. Wide matrices + derived indices → models/figures"
  ),
  y = rev(seq_len(7))
)

p3 <- ggplot(steps, aes(x = 0, y = y, label = step)) +
  geom_label(size = 3.2, label.size = 0.2, hjust = 0) +
  coord_cartesian(xlim = c(-0.2, 1.5), ylim = c(0.5, 7.5)) +
  theme_void() +
  labs(title = "Figure 4.1. Data pipeline for IGO attribute coding and scoring") +
  theme(plot.title = element_text(size = 12, face = "bold"))

ggsave("outputs/figures/Figure_4_1_Data_pipeline_schematic.png", p3, width = 11, height = 4.8, dpi = 300)

message("Figures written to outputs/figures")
