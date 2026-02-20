# 01_build_historical_context.R
# ------------------------------------------------------------------------------
# Constructs the 'Year of Establishment' historical-context variables used in
# Conjecture 1 and Conjecture 2, including:
#   - igo_age (Age_2025)
#   - founding_era_category
#   - founding_density_5yr  (±2 years around founding year)
#   - cumulative_stock      (running total up to founding year)
#
# Input:
#   data/raw/year_of_establishment.csv  (institution, year_founded)
#
# Outputs:
#   outputs/data/historical_context.csv
#   (Legacy copies for backward compatibility: data/processed/... and Data/...)
# ------------------------------------------------------------------------------

source("pipeline/00_project_options.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

df <- read_csv("data/raw/year_of_establishment.csv", show_col_types = FALSE) %>%
  mutate(
    year_founded = as.integer(year_founded),
    igo_age = 2025L - year_founded
  )

# Founding era bins (edit cut points if thesis definitions change)
df <- df %>%
  mutate(
    founding_era_category = case_when(
      year_founded < 1900 ~ "Early Founding Years (Pre-1900)",
      year_founded >= 1900 & year_founded <= 1945 ~ "Early 20th Century (1900-1945)",
      year_founded >= 1946 & year_founded <= 1960 ~ "Post-WWII Boom (1946-1960)",
      year_founded >= 1961 & year_founded <= 1970 ~ "Cold War Era I (1961-1970)",
      year_founded >= 1971 & year_founded <= 1980 ~ "Cold War Era II (1971-1980)",
      year_founded >= 1981 & year_founded <= 1990 ~ "Late Cold War (1981-1990)",
      year_founded >= 1991 & year_founded <= 2000 ~ "Post-Cold War (1991-2000)",
      year_founded >= 2001 & year_founded <= 2010 ~ "Globalisation Era (2001-2010)",
      year_founded >= 2011 & year_founded <= 2020 ~ "SDG & Climate Action Era (2011-2020)",
      TRUE ~ "Other/Unknown"
    )
  )

# Founding density within a ±2-year window around each IGO's founding year
# (i.e., a 5-year window length).
df <- df %>%
  rowwise() %>%
  mutate(
    founding_density_5yr = sum(abs(year_founded - df$year_founded) <= 2, na.rm = TRUE)
  ) %>%
  ungroup()

# Cumulative stock: number of IGOs founded up to (and including) the focal year
df <- df %>%
  mutate(
    cumulative_stock = sapply(year_founded, function(y) sum(df$year_founded <= y, na.rm = TRUE))
  )

# Export (canonical)
write_csv(df, "outputs/data/historical_context.csv")

# Legacy copies (optional)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
write_csv(df, "data/processed/Year_of_Establishment_with_Categories_and_Density.csv")
dir.create("Data", showWarnings = FALSE)
write_csv(df, "Data/year_data.csv")

message("Historical-context variables exported to outputs/data (and legacy locations).")
