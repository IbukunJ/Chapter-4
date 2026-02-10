# ==== Warnings ====
knitr::opts_chunk$set(
  warning = FALSE,   
  message = FALSE 
)
options(warn = -1)


# ===== Packages =====
library(readr)
library(dplyr)
library(purrr)

# Load datasets
df_year <- read_csv("Data/year_data.csv")
df_spatial <- read_csv("Data/spatial_jurisdiction_data.csv")
df_vertical <- read_csv("Data/vertical_coordinations_data.csv")
df_subject <- read_csv("Data/subject_matter_jurisdiction_data.csv")
df_strategies <- read_csv("Data/strategies_data.csv")
df_objectives <- read_csv("Data/defined_objectives_data.csv")
df_relationships <- read_csv("Data/defined_inter_institutional_relationships_data.csv")
df_sources <- read_csv("Data/sources_of_jurisdiction_data.csv")
df_horizontal <- read_csv("Data/horizontal_coordination.csv")

# Put all dataframes in a list
all_dfs <- list(
  df_year, df_spatial, df_vertical, df_subject, 
  df_strategies, df_objectives, df_relationships, 
  df_sources, df_horizontal
)

# Standardize column names: make sure all have "Institution" as key
all_dfs <- lapply(all_dfs, function(df) {
  colnames(df) <- gsub("^[Ii]nstitution$", "Institution", colnames(df))
  df
})

# Merge using full_join on "Institution"
merged_df <- reduce(all_dfs, function(x, y) full_join(x, y, by = "Institution"))


library(stringr)

clean_to_snake_case <- function(names_vector) {
  names_vector %>%
    # Convert to lowercase
    str_to_lower() %>%
    # Replace spaces, slashes, question marks, commas, ampersands, parentheses, and other special chars with underscore
    str_replace_all("[ /?&(),.-]+", "_") %>%
    # Replace multiple underscores with a single underscore
    str_replace_all("_+", "_") %>%
    # Remove trailing or leading underscores
    str_replace_all("^_|_$", "")
}

# Suppose your data frame is merged_df
colnames(merged_df) <- clean_to_snake_case(colnames(merged_df))

# Define relevant column families
id_cols <- c("institution", "year_cleaned", "founding_era_category")
density_cols <- c("foundingdensity_5yr", "cumulativestock")
score_cols <- c("ordinal_score_spatial", "ordinal_score_subject_matter",
                "ordinal_score_strategies", "ordinal_score_defined_objectives",
                "ordinal_score_vertical_coordination", "ordinal_score_horizontal",
                "ordinal_score_sources")

# Helper function to find all cols matching a family prefix
fam_cols <- function(prefixes) {
  pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")_(withinigo|acrossigo)$")
  grep(pattern, colnames(merged_df), value = TRUE)
}

# Define families by prefixes
spatial_prefixes <- c("archipelago", "coastal_zone", "contiguous_zone_cz", "enclosed_or_semi_enclosed_sea",
                      "exclusive_economic_zone_eez", "extended_continental_shelf_cs", "high_seas",
                      "internal_waters", "territorial_sea_ts", "the_area")

subject_prefixes <- c("biodiversity_ecosystem_conservation", "cultural_heritage_traditional_knowledge_data_governance",
                      "disaster_risk_reduction_resilience", "environmental_protection_climate_change",
                      "human_rights_social_justice_advocacy", "international_cooperation_governance",
                      "research_science_innovation", "security_safety",
                      "sustainable_development_capacity_building", "trade_investment_economic_cooperation")

strategy_prefixes <- c("capacity_development_operational_delivery", "collaboration_partnerships_networks",
                       "environmental_climate_biodiversity_action", "financial_budgetary_management",
                       "inclusion_rights_social_justice", "innovation_technology_development",
                       "knowledge_research_data_systems", "monitoring_evaluation_accountability",
                       "policy_regulation_legal_frameworks", "strategic_institutional_planning")

objective_prefixes <- c("environmental_action", "financial_stewardship", "governance_planning", "inclusion_rights",
                        "innovation_technology", "knowledge_data", "monitoring_accountability",
                        "operational_delivery", "partnerships_networks", "policy_regulation")

vertical_prefixes <- c("data_integration_systems", "global_regional_national_coordination",
                       "intergovernmental_to_national_institutions", "multi_level_planning_structures",
                       "policy_alignment_with_national_plans", "regional_implementing_partners",
                       "reporting_compliance_mechanisms", "sectoral_to_national_coordination",
                       "technical_assistance_to_states", "un_to_member_states")

horizontal_prefixes <- c("advocacy_and_communication", "cross_border_initiatives", "cross_sectoral_collaboration",
                         "inter_agency_technical_cooperation", "joint_research_and_projects",
                         "multi_stakeholder_platforms", "peer_to_peer_learning_mechanisms",
                         "regional_economic_community_coordination", "shared_monitoring_frameworks",
                         "thematic_working_groups")

source_prefixes <- c("bilateral_multilateral_arrangements", "binding_secondary_law", "compliance_oversight",
                     "customary_soft_law", "delegated_or_derived_powers", "foundational_treaties_charters",
                     "non_binding_secondary_law", "other_governance_instruments", "strategic_frameworks",
                     "technical_norms_standards")

# Apply helper function
spatial_cols    <- fam_cols(spatial_prefixes)
subject_cols    <- fam_cols(subject_prefixes)
strategy_cols   <- fam_cols(strategy_prefixes)
objective_cols  <- fam_cols(objective_prefixes)
vertical_cols   <- fam_cols(vertical_prefixes)
horizontal_cols <- fam_cols(horizontal_prefixes)
source_cols     <- fam_cols(source_prefixes)

# Combine all columns to extract
final_cols <- c(id_cols, density_cols, score_cols,
                spatial_cols, subject_cols,
                strategy_cols, objective_cols,
                vertical_cols, horizontal_cols,
                source_cols)

# Create Conjecture 2 dataframe
conj2_df <- merged_df[, final_cols]

# Quick check
cat("✅ conj2_df created with", nrow(conj2_df), "rows and", ncol(conj2_df), "columns.\n")

# --------------------------
# Step 1: Density Component Setup
# --------------------------

# Dimension columns
subject_cols <- grep("subject_matter", names(conj2_df), value = TRUE)
spatial_cols <- grep("_(eez|high_seas|the_area|territorial_sea|enclosed_sea)", names(conj2_df), value = TRUE)
coordination_cols <- grep("coordination|collaboration|partnership|platform", names(conj2_df), value = TRUE)
legal_cols <- grep("sources|binding|treaty|law|frameworks", names(conj2_df), value = TRUE)

# Calculate raw counts per dimension
conj2_df$density_subject <- rowSums(conj2_df[subject_cols], na.rm = TRUE)
conj2_df$density_spatial <- rowSums(conj2_df[spatial_cols], na.rm = TRUE)
conj2_df$density_coordination <- rowSums(conj2_df[coordination_cols], na.rm = TRUE)
conj2_df$density_legal <- rowSums(conj2_df[legal_cols], na.rm = TRUE)

# --------------------------
# Step 2: Z-Score Standardization and Composite Score
# --------------------------
standardize <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

conj2_df$density_subject_z <- standardize(conj2_df$density_subject)
conj2_df$density_spatial_z <- standardize(conj2_df$density_spatial)
conj2_df$density_coordination_z <- standardize(conj2_df$density_coordination)
conj2_df$density_legal_z <- standardize(conj2_df$density_legal)

# Composite z-score
conj2_df$institutional_density_score_z <- rowMeans(conj2_df[, c(
  "density_subject_z", "density_spatial_z",
  "density_coordination_z", "density_legal_z"
)], na.rm = TRUE)

# --------------------------
# Step 3: Rescale to 0–10 and Categorize
# --------------------------
min_z <- min(conj2_df$institutional_density_score_z, na.rm = TRUE)
max_z <- max(conj2_df$institutional_density_score_z, na.rm = TRUE)

conj2_df$institutional_density_score_scaled <- (
  (conj2_df$institutional_density_score_z - min_z) /
  (max_z - min_z)
) * 10

# High / Low split based on 75th percentile
percentile_75 <- quantile(conj2_df$institutional_density_score_scaled, 0.75, na.rm = TRUE)

conj2_df$density_group_composite <- ifelse(
  conj2_df$institutional_density_score_scaled >= percentile_75,
  "High", "Low"
)

# --------------------------
# Step 4: Visualization
# --------------------------

library(ggplot2)

# Calculate 75th percentile
percentile_75 <- quantile(conj2_df$institutional_density_score_scaled, 0.75, na.rm = TRUE)

# Plot histogram with 75th percentile line
ggplot(conj2_df, aes(x = institutional_density_score_scaled)) +
  geom_histogram(fill = "#0073C2FF", color = "white", bins = 20) +
  geom_vline(xintercept = percentile_75, color = "red", linetype = "dashed", linewidth = 1) +
  annotate("text", x = percentile_75 + 0.3, y = Inf, label = "75th percentile", 
           vjust = 2, hjust = 0, color = "red", size = 4) +
  labs(
    title = "Figure 1A Distribution of Institutional Density Scores (Scaled 0–10)",
    x = "Institutional Density Score",
    y = "Count"
  ) +
  theme_minimal()

library(dplyr)

# Choose cutoff - median or 75th percentile
cutoff <- quantile(conj2_df$foundingdensity_5yr, 0.75, na.rm = TRUE)

# Create density group based on cutoff
conj2_df <- conj2_df %>%
  mutate(founding_density_group = ifelse(foundingdensity_5yr > cutoff, "High", "Low/Moderate"))

# Summary stats by new group
summary_stats <- conj2_df %>%
  group_by(founding_density_group) %>%
  summarise(
    Count = n(),
    Mean_Subjects = mean(ordinal_score_subject_matter, na.rm = TRUE),
    SD_Subjects = sd(ordinal_score_subject_matter, na.rm = TRUE),
    Median_Subjects = median(ordinal_score_subject_matter, na.rm = TRUE),

    Mean_Spatial = mean(ordinal_score_spatial, na.rm = TRUE),
    SD_Spatial = sd(ordinal_score_spatial, na.rm = TRUE),
    Median_Spatial = median(ordinal_score_spatial, na.rm = TRUE)
  )

print(summary_stats)

t.test(ordinal_score_subject_matter ~ founding_density_group, data = conj2_df)

# Spatial mandates t-test
t.test(ordinal_score_spatial ~ founding_density_group, data = conj2_df)


# Non-parametric (Wilcoxon) tests
wilcox.test(ordinal_score_subject_matter ~ founding_density_group, data = conj2_df)

wilcox.test(ordinal_score_spatial ~ founding_density_group, data = conj2_df)

library(ggplot2)
library(ggrepel)

ggplot(conj2_df, aes(x = founding_density_group, y = ordinal_score_subject_matter)) +
  geom_boxplot(fill = "#0073C2FF", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(label = institution), 
              width = 0.2, height = 0.1, size = 3, alpha = 0.7, color = "black") +
  geom_text_repel(aes(label = institution), size = 3, max.overlaps = 15) +
  labs(
    title = "Figure 2.1A. Subject-Matter Mandate Scores by Founding Density Group",
    x = "Founding Density Group",
    y = "Subject-Matter Ordinal Score"
  ) +
  theme_minimal()


ggplot(conj2_df, aes(x = founding_density_group, y = ordinal_score_spatial)) +
  geom_boxplot(fill = "#EFC000FF", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(aes(label = institution), 
              width = 0.2, height = 0.1, size = 3, alpha = 0.7, color = "black") +
  geom_text_repel(aes(label = institution), size = 3, max.overlaps = 15) +
  labs(
    title = "Figure 2.1B. Spatial Mandate Scores by Founding Density Group",
    x = "Founding Density Group",
    y = "Spatial Ordinal Score"
  ) +
  theme_minimal()


ggplot(conj2_df, aes(x = ordinal_score_subject_matter, y = ordinal_score_spatial, color = founding_density_group)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text_repel(aes(label = institution), size = 3, max.overlaps = 20) +
  labs(
    title = "Figure 2.1C. IGOs by Subject-Matter and Spatial Mandates",
    x = "Subject-Matter Score",
    y = "Spatial Score",
    color = "Founding Density Group"
  ) +
  theme_minimal()


library(dplyr)
library(tidyr)
library(ggplot2)

# Define spatial and subject columns explicitly
spatial_within_cols <- c(
  "archipelago_withinigo", "coastal_zone_withinigo", "contiguous_zone_cz_withinigo",
  "enclosed_or_semi_enclosed_sea_withinigo", "exclusive_economic_zone_eez_withinigo",
  "extended_continental_shelf_cs_withinigo", "high_seas_withinigo",
  "internal_waters_withinigo", "territorial_sea_ts_withinigo", "the_area_withinigo"
)

subject_within_cols <- c(
  "biodiversity_ecosystem_conservation_withinigo",
  "cultural_heritage_traditional_knowledge_data_governance_withinigo",
  "disaster_risk_reduction_resilience_withinigo",
  "environmental_protection_climate_change_withinigo",
  "human_rights_social_justice_advocacy_withinigo",
  "international_cooperation_governance_withinigo",
  "research_science_innovation_withinigo",
  "security_safety_withinigo",
  "sustainable_development_capacity_building_withinigo",
  "trade_investment_economic_cooperation_withinigo"
)

# Filter IGOs with high density and high scores in subject and spatial scope
high_high_df <- conj2_df %>%
  filter(
    density_group_composite == "High",
    ordinal_score_subject_matter >= 7,
    ordinal_score_spatial >= 7
  )

# Pivot and categorize subject scores
subject_long <- high_high_df %>%
  select(institution, all_of(subject_within_cols)) %>%
  pivot_longer(-institution, names_to = "subject_area", values_to = "score") %>%
  filter(score > 0) %>%
  mutate(score_category = case_when(
    score >= 7 ~ "Very High",
    score >= 5 ~ "High",
    score >= 3 ~ "Moderate",
    TRUE ~ "Low"
  ))

library(stringr)  

# After pivoting, clean the names by removing "_withinigo"
subject_long <- high_high_df %>%
  select(institution, all_of(subject_within_cols)) %>%
  pivot_longer(-institution, names_to = "subject_area", values_to = "score") %>%
  filter(score > 0) %>%
  mutate(
    subject_area = str_replace(subject_area, "_withinigo$", ""),
    score_category = case_when(
      score >= 7 ~ "Very High",
      score >= 5 ~ "High",
      score >= 3 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

spatial_long <- high_high_df %>%
  select(institution, all_of(spatial_within_cols)) %>%
  pivot_longer(-institution, names_to = "spatial_area", values_to = "score") %>%
  filter(score > 0) %>%
  mutate(
    spatial_area = str_replace(spatial_area, "_withinigo$", ""),
    score_category = case_when(
      score >= 7 ~ "Very High",
      score >= 5 ~ "High",
      score >= 3 ~ "Moderate",
      TRUE ~ "Low"
    )
  )

# Now plot with cleaned labels (same as before)
ggplot(subject_long, aes(x = reorder(subject_area, score, FUN = median), fill = score_category)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Figure 2.1D. Subject Areas by Score Level (High-High IGOs)",
    x = "Subject Area",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Low" = "#d9f0a3", "Moderate" = "#addd8e", "High" = "#78c679", "Very High" = "#31a354")) +
  theme_minimal()


ggplot(spatial_long, aes(x = reorder(spatial_area, score, FUN = median), fill = score_category)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Figure 2.1E. Spatial Areas by Score Level (High-High IGOs)",
    x = "Spatial Area",
    y = "Count"
  ) +
  scale_fill_manual(values = c("Low" = "#fee0d2", "Moderate" = "#fcbba1", "High" = "#fc9272", "Very High" = "#de2d26")) +
  theme_minimal()

# Composite Coordination Score (averaging vertical + horizontal)
conj2_df$coordination_score <- rowMeans(
  conj2_df[, c("ordinal_score_vertical_coordination", "ordinal_score_horizontal")],
  na.rm = TRUE
)

# Legal Authority Score (already scaled 0–10, use directly)
conj2_df$legal_authority_score <- conj2_df$foundational_treaties_charters_withinigo

# Mandate Score (averaging subject and spatial breadth)
conj2_df$mandate_score <- rowMeans(
  conj2_df[, c("ordinal_score_subject_matter", "ordinal_score_spatial")],
  na.rm = TRUE
)

# OPTIONAL: Check structure
summary(conj2_df[, c("coordination_score", "legal_authority_score", "mandate_score", "institutional_density_score_scaled")])

# Fit moderated regression model
model_h2_2 <- lm(
  mandate_score ~ institutional_density_score_scaled * coordination_score +
                  institutional_density_score_scaled * legal_authority_score,
  data = conj2_df
)

# Summarise the model
summary(model_h2_2)


library(ggplot2)
library(dplyr)

# Optional: Filter to include only IGOs with scores > 6 (if needed)
filtered_df <- conj2_df %>%
  filter(coordination_score > 0,
         legal_authority_score > 0,
         mandate_score > 0)

# Plot
ggplot(filtered_df, aes(x = coordination_score,
                        y = institutional_density_score_scaled,
                        color = legal_authority_score)) +
  geom_point(size = 4) +
  geom_text(aes(label = institution), vjust = -1, size = 3) +
  scale_color_viridis_c(option = "D", name = "Legal Authority Score") +
  labs(
    title = "Figure 2.2A Coordination vs. Institutional Density\nColored by Legal Authority Score",
    x = "Coordination Score",
    y = "Institutional Density Score (Scaled)"
  ) +
  theme_minimal()

library(dplyr)

# Subject-matter domain columns (presence/absence)
subject_cols <- c(
  "biodiversity_ecosystem_conservation_withinigo",
  "cultural_heritage_traditional_knowledge_data_governance_withinigo",
  "disaster_risk_reduction_resilience_withinigo",
  "environmental_protection_climate_change_withinigo",
  "human_rights_social_justice_advocacy_withinigo",
  "international_cooperation_governance_withinigo",
  "research_science_innovation_withinigo",
  "security_safety_withinigo",
  "sustainable_development_capacity_building_withinigo",
  "trade_investment_economic_cooperation_withinigo"
)

# Spatial domain columns (presence/absence)
spatial_cols <- c(
  "archipelago_withinigo",
  "coastal_zone_withinigo",
  "contiguous_zone_cz_withinigo",
  "enclosed_or_semi_enclosed_sea_withinigo",
  "exclusive_economic_zone_eez_withinigo",
  "extended_continental_shelf_cs_withinigo",
  "high_seas_withinigo",
  "internal_waters_withinigo",
  "territorial_sea_ts_withinigo",
  "the_area_withinigo"
)

# Base R implementation of Shannon entropy
compute_entropy <- function(x) {
  x <- as.numeric(x)
  if (sum(x, na.rm = TRUE) == 0) return(0)
  p <- x / sum(x, na.rm = TRUE)
  p <- p[p > 0]
  -sum(p * log2(p))
}

# Compute entropy scores
conj2_df <- conj2_df %>%
  rowwise() %>%
  mutate(
    subject_entropy = compute_entropy(c_across(all_of(subject_cols))),
    spatial_entropy = compute_entropy(c_across(all_of(spatial_cols))),
    combined_entropy = mean(c(subject_entropy, spatial_entropy), na.rm = TRUE)
  ) %>%
  ungroup()


# Now test relationship with institutional density
library(ggplot2)

# Scatter plot: institutional_density_score_scaled vs combined_entropy
hy3 <- ggplot(conj2_df, aes(x = institutional_density_score_scaled, y = combined_entropy, label = institution)) +
  geom_point(color = "steelblue", size = 3) +
  geom_text(vjust = -1, size = 3) +
  labs(
    title = "Figure 2.3A Niche Differentiation vs Institutional Density",
    x = "Institutional Density (scaled)",
    y = "Combined Entropy (Subject & Spatial Mandates)"
  ) +
  theme_minimal()

# Linear regression to check significance
model <- lm(combined_entropy ~ institutional_density_score_scaled, data = conj2_df)
summary(model)


library(dplyr)
library(tidyr)
library(ggplot2)

# Threshold for high institutional density (e.g., 75th percentile)
threshold <- quantile(conj2_df$institutional_density_score_scaled, 0.75, na.rm = TRUE)

# Filter IGOs with high institutional density
high_density_igos <- conj2_df %>%
  filter(institutional_density_score_scaled > threshold)

# Subject columns
subject_cols <- c(
  "biodiversity_ecosystem_conservation_withinigo",
  "cultural_heritage_traditional_knowledge_data_governance_withinigo",
  "disaster_risk_reduction_resilience_withinigo",
  "environmental_protection_climate_change_withinigo",
  "human_rights_social_justice_advocacy_withinigo",
  "international_cooperation_governance_withinigo",
  "research_science_innovation_withinigo",
  "security_safety_withinigo",
  "sustainable_development_capacity_building_withinigo",
  "trade_investment_economic_cooperation_withinigo"
)

# Summarize presence of subject matters
subject_summary <- high_density_igos %>%
  select(institution, all_of(subject_cols)) %>%
  pivot_longer(cols = -institution, names_to = "subject", values_to = "presence") %>%
  mutate(subject = gsub("_withinigo$", "", subject)) %>%
  filter(presence > 0) %>%
  group_by(subject) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot
ggplot(subject_summary, aes(x = reorder(subject, count), y = count)) +
  geom_col(fill = "#2c7fb8") +
  coord_flip() +
  labs(
    title = "2.3A High Institutional Density IGOs",
    x = "Subject Matter",
    y = "Number of IGOs"
  ) +
  theme_minimal()


# Spatial columns
spatial_cols <- c(
  "archipelago_withinigo",
  "coastal_zone_withinigo",
  "contiguous_zone_cz_withinigo",
  "enclosed_or_semi_enclosed_sea_withinigo",
  "exclusive_economic_zone_eez_withinigo",
  "extended_continental_shelf_cs_withinigo",
  "high_seas_withinigo",
  "internal_waters_withinigo",
  "territorial_sea_ts_withinigo",
  "the_area_withinigo"
)

# Summarize presence of spatial terms
spatial_summary <- high_density_igos %>%
  select(institution, all_of(spatial_cols)) %>%
  pivot_longer(cols = -institution, names_to = "spatial_term", values_to = "presence") %>%
  mutate(spatial_term = gsub("_withinigo$", "", spatial_term)) %>%
  filter(presence > 0) %>%
  group_by(spatial_term) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Plot
ggplot(spatial_summary, aes(x = reorder(spatial_term, count), y = count)) +
  geom_col(fill = "#7fcdbb") +
  coord_flip() +
  labs(
    title = "2.3B High Institutional Density IGOs",
    x = "Spatial Term",
    y = "Number of IGOs"
  ) +
  theme_minimal()


# Subject matter columns - within IGO scope (adjust columns as needed)
subject_cols <- c(
  "biodiversity_ecosystem_conservation_withinigo",
  "cultural_heritage_traditional_knowledge_data_governance_withinigo",
  "disaster_risk_reduction_resilience_withinigo",
  "environmental_protection_climate_change_withinigo",
  "human_rights_social_justice_advocacy_withinigo",
  "international_cooperation_governance_withinigo",
  "research_science_innovation_withinigo",
  "security_safety_withinigo",
  "sustainable_development_capacity_building_withinigo",
  "trade_investment_economic_cooperation_withinigo"
)

# Create a portfolio diversity score: count how many subject matters have presence > 0
conj2_df$portfolio_diversity <- rowSums(conj2_df[, subject_cols] > 0, na.rm = TRUE)

conj2_df$institutional_density_sq <- conj2_df$institutional_density_score_scaled^2

model <- lm(
  mandate_score ~ institutional_density_score_scaled + institutional_density_sq + 
    portfolio_diversity + 
    institutional_density_score_scaled:portfolio_diversity + 
    institutional_density_sq:portfolio_diversity, 
  data = conj2_df
)

summary(model)

library(ggplot2)
library(dplyr)

# Step 1: Create a prediction dataset
density_seq <- seq(from = min(conj2_df$institutional_density_score_scaled, na.rm = TRUE),
                   to = max(conj2_df$institutional_density_score_scaled, na.rm = TRUE),
                   length.out = 100)

# Choose representative levels of portfolio diversity
portfolio_levels <- c(2, 5, 8)  # low, medium, high diversity

# Create a new data frame for predictions
pred_data <- expand.grid(
  institutional_density_score_scaled = density_seq,
  portfolio_diversity = portfolio_levels
) %>%
  mutate(
    institutional_density_sq = institutional_density_score_scaled^2
  )

# Step 2: Use the model to predict mandate_score
model <- lm(
  mandate_score ~ institutional_density_score_scaled +
    institutional_density_sq +
    portfolio_diversity +
    institutional_density_score_scaled:portfolio_diversity +
    institutional_density_sq:portfolio_diversity,
  data = conj2_df
)

# Add predictions to the prediction data
pred_data$predicted_mandate <- predict(model, newdata = pred_data)

# Step 3: Plot the interaction
ggplot(pred_data, aes(x = institutional_density_score_scaled, y = predicted_mandate,
                      color = factor(portfolio_diversity))) +
  geom_line(size = 1.2) +
  labs(
    title = "Figure 2.4A Interaction of Institutional Density and Portfolio Diversity on Mandate Score",
    x = "Institutional Density (Scaled)",
    y = "Predicted Mandate Score",
    color = "Portfolio Diversity"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
