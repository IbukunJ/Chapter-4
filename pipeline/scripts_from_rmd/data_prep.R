# Load necessary libraries
library(tidyverse)
library(lubridate)

# Load the data
df <- read.csv("full_data.csv")

# Inspect the first few rows
head(df)


# Define column index ranges for each category
column_ranges <- list(
  Density_Metrics = 4:5,
  Spatial_Governance = 6:26,
  Vertical_Coordination = 27:47,
  Subject_Matter_Coverage = 48:68,
  Strategies = 69:89,
  Defined_Objectives = 90:110,
  Defined_Interactions = 111:131,
  Sources_Legal_Instruments = 132:152,
  Horizontal_Coordination = 153:173
)

# Calculate the standard deviation of each category's summed score
category_weights <- sapply(names(column_ranges), function(cat) {
  cols <- column_ranges[[cat]]
  scores <- rowSums(df[, cols], na.rm = TRUE)
  sd(scores)
})

# Normalize the weights so they sum to 1
normalized_weights <- category_weights / sum(category_weights)


# Display the weights
#print(round(normalized_weights, 4))


# Select and clean relevant ordinal score columns
ordinal_cols <- df %>%
  select(institution,
         ordinal_score_spatial,
         ordinal_score_vertical_coordination,
         ordinal_score_subject_matter,
         ordinal_score_strategies,
         ordinal_score_defined_objectives,
         ordinal_score_defined_inter,
         ordinal_score_sources,
         ordinal_score_horizontal)

# Reshape data to long format and clean feature names
ordinal_long <- ordinal_cols %>%
  pivot_longer(cols = -institution,
               names_to = "feature",
               values_to = "score") %>%
  mutate(feature = str_replace_all(feature, "ordinal_score_", ""),
         feature = str_replace_all(feature, "_", " "),
         feature = str_to_title(feature))

# Calculate average score per feature per institution
avg_scores <- ordinal_long %>%
  group_by(institution, feature) %>%
  summarise(avg_score = mean(score, na.rm = TRUE), .groups = "drop")

overall_avg <- avg_scores %>%
  group_by(feature) %>%
  summarise(overall_avg = mean(avg_score), .groups = "drop")

# Add to plot
ggplot(avg_scores, aes(x = feature, y = avg_score, group = institution, color = institution)) +
  geom_line(alpha = 0.3) +
  geom_point(size = 1, alpha = 0.5) +
  geom_line(data = overall_avg, aes(x = feature, y = overall_avg, group = 1),
            color = "black", size = 1.2, linetype = "dashed") +
  labs(title = "Figure 1A Average Ordinal Scores by Institutional Design Feature",
       subtitle = "Black dashed line shows overall mean",
       x = "Design Feature", y = "Average Score") +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Boxplot of scores by feature
Boxplot <- ggplot(ordinal_long, aes(x = feature, y = score)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
  labs(
    title = "Figure 1B Distribution of Ordinal Scores",
    subtitle = "Boxplots show spread of scores across institutions",
    x = "Design Feature", y = "Ordinal Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(),
    plot.subtitle = element_text(size = 10)
  )


# Load libraries
library(dplyr)
library(tidyr)
library(scales)

# 1. Derive Age (IGO Age)
df <- df %>%
  mutate(igo_age = 2025 - year_founded)  # Assuming current year is 2025

# 2. Jurisdictional Breadth
df <- df %>%
  mutate(
    spatial_breadth = rowSums(select(., 6:26), na.rm = TRUE),
    subject_breadth = rowSums(select(., 48:68), na.rm = TRUE)
  )

# 3. Institutional Design
df <- df %>%
  mutate(
    legal_breadth = rowSums(select(., 132:152), na.rm = TRUE),
    vertical_coordination_strength = rowSums(select(., 27:47), na.rm = TRUE),
    horizontal_coordination_strength = rowSums(select(., 153:173), na.rm = TRUE),
    defined_interactions = rowSums(select(., 111:131), na.rm = TRUE)
  )

# 4. Strategic Reach
df <- df %>%
  mutate(
    strategy_diversification = rowSums(select(., 69:89), na.rm = TRUE),
    objective_diversification = rowSums(select(., 90:110), na.rm = TRUE)
  )

# Normalize each component 0–1 (min-max scaling)
normalize <- function(x) rescale(x, to = c(0, 1))

df <- df %>%
  mutate(
    score_age = normalize(igo_age),
    score_spatial = normalize(spatial_breadth),
    score_subject = normalize(subject_breadth),
    score_vertical = normalize(vertical_coordination_strength),
    score_horizontal = normalize(horizontal_coordination_strength),
    score_strategies = normalize(strategy_diversification),
    score_objectives = normalize(objective_diversification),
    score_legal = normalize(legal_breadth),
    score_defined_inter = normalize(defined_interactions)
  )

# Final weighted Endurance Score
df <- df %>%
  mutate(
    endurance_score = (
      0.1108 * score_age +
      0.1841 * score_spatial +
      0.0743 * score_vertical +
      0.1170 * score_subject +
      0.1280 * score_strategies +
      0.1288 * score_objectives +
      0.0950 * score_defined_inter +
      0.0943 * score_legal +
      0.0678 * score_horizontal
    )
  )

# Load necessary libraries
library(dplyr)
library(scales)

# Derive Niche Specialisation
# Combine spatial + subject breadth, normalize, then invert to show specialisation
df <- df %>%
  mutate(
    total_breadth = spatial_breadth + subject_breadth,
    niche_specialisation = 1 - rescale(total_breadth, to = c(0, 1))  # Higher = more specialised
  )

# Adaptive Capacity Index
# Based on strategic diversification and objectives (proxy for functional flexibility)
df <- df %>%
  mutate(
    adaptive_capacity_index = rescale(strategy_diversification, to = c(0, 1)) +
                              rescale(objective_diversification, to = c(0, 1)),
    adaptive_capacity_index = adaptive_capacity_index / 2  # Average to keep scale 0–1
  )

# Density Squared (for non-linear density effects)
df <- df %>%
  mutate(
    foundingdensity_5yr_norm = rescale(foundingdensity_5yr, to = c(0, 1)),
    density_squared = foundingdensity_5yr_norm^2
  )

library(dplyr)

#  Mandate Breadth
df <- df %>%
  mutate(
    spatial_breadth = rowSums(select(., 6:26), na.rm = TRUE),  # Already derived
    subject_breadth = rowSums(select(., 49:68), na.rm = TRUE), # Already derived
    mandate_breadth = spatial_breadth + subject_breadth        # Composite measure
  )

# Relational Embeddedness
df <- df %>%
  mutate(
    embeddedness_score = rowSums(select(., 27:47, 153:173), na.rm = TRUE) # Vertical + Horizontal coordination
  )

# Strategy Diversification (already derived)
# Sum of strategic categories (both within/across): strategy_diversification

# 5. Composite Legitimacy & Efficacy Score
df <- df %>%
  mutate(
    legitimacy_efficacy_score = 
      scales::rescale(ordinal_score_sources, to = c(0, 1)) +
      scales::rescale(mandate_breadth, to = c(0, 1)) +
      scales::rescale(embeddedness_score, to = c(0, 1)) +
      scales::rescale(strategy_diversification, to = c(0, 1))
  )

# Preview results
df %>% select(institution, ordinal_score_sources, mandate_breadth, embeddedness_score, strategy_diversification, legitimacy_efficacy_score)

library(dplyr)

#  Mandate Breadth
df <- df %>%
  mutate(
    spatial_breadth = rowSums(select(., 6:26), na.rm = TRUE),  # Already derived
    subject_breadth = rowSums(select(., 49:68), na.rm = TRUE), # Already derived
    mandate_breadth = spatial_breadth + subject_breadth        # Composite measure
  )

# Relational Embeddedness
df <- df %>%
  mutate(
    embeddedness_score = rowSums(select(., 27:47, 153:173), na.rm = TRUE) # Vertical + Horizontal coordination
  )

# Strategy Diversification (already derived)
# Sum of strategic categories (both within/across): strategy_diversification

# 5. Composite Legitimacy & Efficacy Score
df <- df %>%
  mutate(
    legitimacy_efficacy_score = 
      scales::rescale(ordinal_score_sources, to = c(0, 1)) +
      scales::rescale(mandate_breadth, to = c(0, 1)) +
      scales::rescale(embeddedness_score, to = c(0, 1)) +
      scales::rescale(strategy_diversification, to = c(0, 1))
  )

# Preview results
df %>% select(institution, ordinal_score_sources, mandate_breadth, embeddedness_score, strategy_diversification, legitimacy_efficacy_score)


view(colnames(df))

# Custom normalization function: only apply if max value > 10
normalize_if_needed <- function(x) {
  if (is.numeric(x) && max(x, na.rm = TRUE) > 10) {
    rng <- range(x, na.rm = TRUE)
    if (rng[1] == rng[2]) return(rep(0, length(x)))  # avoid divide-by-zero
    return(10 * (x - rng[1]) / (rng[2] - rng[1]))
  } else {
    return(x)
  }
}

# Apply to columns 6 to 200 in-place
df[ , 6:200] <- lapply(df[ , 6:200], normalize_if_needed)

# Save the resulting data frame to CSV
write.csv(df, "IGO_full_data.csv", row.names = FALSE)
