# ==== Warnings ====
knitr::opts_chunk$set(
  warning = FALSE,   
  message = FALSE 
)
options(warn = -1)


# ===== Packages =====
library(dplyr)
library(janitor)
library(dplyr)
library(tidyverse)
library(readr)

# Load all datasets
df_year <- read_csv("Data/year_data.csv") %>%
  clean_names()

df_spatial <- read_csv("Data/spatial_jurisdiction_data.csv") %>%
  clean_names()

df_vertical <- read_csv("Data/vertical_coordinations_data.csv") %>%
  clean_names()

df_subject <- read_csv("Data/subject_matter_jurisdiction_data.csv") %>%
  clean_names()

df_strategies <- read_csv("Data/strategies_data.csv") %>%
  clean_names()

df_objectives <- read_csv("Data/defined_objectives_data.csv") %>%
  clean_names()

df_relationships <- read_csv("Data/defined_inter_institutional_relationships_data.csv") %>%
  clean_names()

df_sources <- read_csv("Data/sources_of_jurisdiction_data.csv") %>%
  clean_names()

df_horizontal <- read_csv("Data/horizontal_coordination.csv") %>%
  clean_names()

# ==== Merge All Data ====
df <- df_year %>%
  left_join(df_spatial, by = "institution") %>%
  left_join(df_vertical, by = "institution") %>%
  left_join(df_subject, by = "institution") %>%
  left_join(df_strategies, by = "institution") %>%
  left_join(df_objectives, by = "institution") %>%
  left_join(df_relationships, by = "institution") %>%
  left_join(df_sources, by = "institution") %>%
  left_join(df_horizontal, by = "institution")

view(df_horizontal)

full <- df
nms <- names(full)

# helper: build family columns (within/across variants)
fam_cols <- function(terms, nms) {
  pat <- paste0("^(", paste(terms, collapse="|"), ")_(within|across)_igo$")
  out <- grep(pat, nms, value = TRUE)
  return(out)
}

# Families (prefixes as in your dataset)
spatial_terms <- c(
  "archipelago","coastal_zone","contiguous_zone_cz","enclosed_or_semi_enclosed_sea",
  "exclusive_economic_zone_eez","extended_continental_shelf_cs","high_seas",
  "internal_waters","territorial_sea_ts","the_area"
)

subject_terms <- c(
  "biodiversity_ecosystem_conservation","cultural_heritage_traditional_knowledge_data_governance",
  "disaster_risk_reduction_resilience","environmental_protection_climate_change",
  "human_rights_social_justice_advocacy","international_cooperation_governance",
  "research_science_innovation","security_safety",
  "sustainable_development_capacity_building","trade_investment_economic_cooperation"
)

vertical_terms <- c(
  "data_integration_systems","global_regional_national_coordination",
  "intergovernmental_to_national_institutions","multi_level_planning_structures",
  "policy_alignment_with_national_plans","regional_implementing_partners",
  "reporting_compliance_mechanisms","sectoral_to_national_coordination",
  "technical_assistance_to_states","un_to_member_states"
)

horizontal_terms <- c(
  "advocacy_and_communication","cross_border_initiatives",
  "cross_sectoral_collaboration","inter_agency_technical_cooperation",
  "joint_research_and_projects","multi_stakeholder_platforms",
  "peer_to_peer_learning_mechanisms","regional_economic_community_coordination",
  "shared_monitoring_frameworks","thematic_working_groups" 
)

strategy_terms <- c(
  "capacity_development_operational_delivery","collaboration_partnerships_networks",
  "environmental_climate_biodiversity_action","financial_budgetary_management",
  "inclusion_rights_social_justice","innovation_technology_development",
  "knowledge_research_data_systems","monitoring_evaluation_accountability",
  "policy_regulation_legal_frameworks","strategic_institutional_planning"
)

objective_terms <- c(
  "environmental_action","financial_stewardship","governance_planning","inclusion_rights",
  "innovation_technology","knowledge_data","monitoring_accountability","operational_delivery",
  "partnerships_networks","policy_regulation"
)

inter_terms <- c(
  "civil_society_engagement","donor_partnerships","intergovernmental_consultations",
  "ngo_engagement","private_sector_partnerships","regional_body_coordination",
  "scientific_community_linkages","technical_or_expert_groups",
  "treaty_body_coordination","un_system_collaboration"
)

source_terms <- c(
  "bilateral_multilateral_arrangements","binding_secondary_law","compliance_oversight",
  "customary_soft_law","delegated_or_derived_powers","foundational_treaties_charters",
  "non_binding_secondary_law","other_governance_instruments",
  "strategic_frameworks","technical_norms_standards"
)

# --- collect actual column names from families -------------------------------
spatial_cols   <- fam_cols(spatial_terms, nms)
subject_cols   <- fam_cols(subject_terms, nms)
vertical_cols  <- fam_cols(vertical_terms, nms)
strategy_cols  <- fam_cols(strategy_terms, nms)
objective_cols <- fam_cols(objective_terms, nms)
inter_cols     <- fam_cols(inter_terms, nms)
source_cols    <- fam_cols(source_terms, nms)
horizontal_cols <-fam_cols(horizontal_terms, nms)

# Scores and identifiers
score_cols <- intersect(c(
  "ordinal_score_spatial","ordinal_score_vertical_coordination",
  "ordinal_score_subject_matter","ordinal_score_strategies",
  "ordinal_score_defined_objectives","ordinal_score_defined_inter",
  "ordinal_score_sources", "ordinal_score_horizontal"
), nms)

id_cols <- intersect(c("institution","year_cleaned","founding_era_category"), nms)
density_cols <- intersect(c("founding_density_5yr","cumulative_stock"), nms)

# --- Conjecture 1: design coherence -----------------------------------------
c1_cols <- unique(c(
  id_cols, density_cols,
  intersect(c("ordinal_score_spatial","ordinal_score_subject_matter",
              "ordinal_score_vertical_coordination",
              "ordinal_score_strategies","ordinal_score_defined_objectives",
              "ordinal_score_sources",
              "ordinal_score_horizontal"), score_cols),
  spatial_cols, subject_cols, vertical_cols, strategy_cols, objective_cols, source_cols, horizontal_cols
))
conj1_df <- dplyr::select(full, all_of(c1_cols))

# --- Conjecture 2: density → niche & adaptation -----------------------------
strategy_adapt <- grep("^(innovation_technology_development|knowledge_research_data_systems|capacity_development_operational_delivery|monitoring_evaluation_accountability|collaboration_partnerships_networks)_(within|across)_igo$", nms, value = TRUE)
objective_adapt <- grep("^(innovation_technology|knowledge_data|monitoring_accountability|operational_delivery|partnerships_networks)_(within|across)_igo$", nms, value = TRUE)

c2_cols <- unique(c(
  id_cols, density_cols,
  intersect(c("ordinal_score_subject_matter","ordinal_score_spatial",
              "ordinal_score_strategies","ordinal_score_defined_objectives"), score_cols),
  subject_cols, spatial_cols, strategy_adapt, objective_adapt
))
conj2_df <- dplyr::select(full, all_of(c2_cols))

# --- Conjecture 3: embeddedness & legitimacy --------------------------------
strategy_legit <- grep("^(inclusion_rights_social_justice|financial_budgetary_management|monitoring_evaluation_accountability|policy_regulation_legal_frameworks|strategic_institutional_planning)_(within|across)_igo$", nms, value = TRUE)
objective_legit <- grep("^(inclusion_rights|financial_stewardship|monitoring_accountability|governance_planning|policy_regulation|partnerships_networks)_(within|across)_igo$", nms, value = TRUE)
vertical_compliance <- grep("^reporting_compliance_mechanisms_(within|across)_igo$", nms, value = TRUE)

c3_cols <- unique(c(
  id_cols,
  intersect(c("ordinal_score_defined_inter","ordinal_score_sources",
              "ordinal_score_defined_objectives","ordinal_score_strategies"), score_cols),
  inter_cols, strategy_legit, objective_legit, vertical_compliance, source_cols
))
conj3_df <- dplyr::select(full, all_of(c3_cols))

# --- sanity checks ----------------------------------------------------------
cat("Conj1 cols:", length(c1_cols), "\n")
cat("Conj2 cols:", length(c2_cols), "\n")
cat("Conj3 cols:", length(c3_cols), "\n")


# ---- C1-H1: Earlier-established IGOs endure longer --------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(forcats); library(ggplot2); library(knitr)
  library(broom); library(lmtest); library(sandwich); library(ggrepel)
})

# 0) Data used
stopifnot(exists("conj1_df"))
c1 <- conj1_df

# 1) Construct endurance proxy (EII)
num_na0 <- function(x) replace_na(as.numeric(x), 0)
z <- function(x) as.numeric(scale(x))

bind_cols <- intersect(c(
  "binding_secondary_law_within_igo","delegated_or_derived_powers_within_igo",
  "foundational_treaties_charters_within_igo","binding_secondary_law_across_igo",
  "delegated_or_derived_powers_across_igo","foundational_treaties_charters_across_igo"
), names(c1))

c1 <- c1 %>%
  mutate(
    endurance_age = 2025 - year_cleaned,  
    legal_binding_ct = if (length(bind_cols) > 0)
      rowSums(across(all_of(bind_cols), num_na0), na.rm = TRUE) else NA_real_,
    EII = rowMeans(cbind(
      z(ordinal_score_vertical_coordination),
      z(ordinal_score_strategies),
      z(ordinal_score_defined_objectives),
      z(ordinal_score_subject_matter),
      z(ordinal_score_horizontal),
      z(legal_binding_ct)
    ), na.rm = TRUE),
    founding_era_category = fct_reorder(founding_era_category, year_cleaned, .fun = min, .na_rm = TRUE)
  )

# 2) Define governance landmarks
c1 <- c1 %>%
  mutate(landmark_era = case_when(
    year_cleaned <= 1918 ~ "Pre–WWI Foundations",
    year_cleaned %in% 1919:1944 ~ "League of Nations Era",
    year_cleaned %in% 1945:1989 ~ "UN Charter",
    year_cleaned %in% 1990:2001 ~ "Post–Cold War Expansion",
    year_cleaned >= 2002 ~ "21st Century Governance",
    TRUE ~ "Unclassified"
  ))

# 3) Summary Table by Founding Era
era_summary <- c1 %>%
  group_by(founding_era_category) %>%
  summarise(
    N        = n(),
    year_med = median(year_cleaned, na.rm = TRUE),
    age_mean = mean(2025 - year_cleaned, na.rm = TRUE),
    EII_mean = mean(EII, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Identify extremes per era
extremes <- c1 %>%
  group_by(founding_era_category) %>%
  summarise(
    earliest_year = min(year_cleaned, na.rm = TRUE),
    latest_year   = max(year_cleaned, na.rm = TRUE),
    top_EII_val   = max(EII, na.rm = TRUE),
    low_EII_val   = min(EII, na.rm = TRUE),
    .groups = "drop"
  )

label_df <- c1 %>%
  inner_join(extremes, by = "founding_era_category") %>%
  filter(year_cleaned == earliest_year |
           year_cleaned == latest_year |
           EII == top_EII_val |
           EII == low_EII_val) %>%
  distinct(institution, .keep_all = TRUE)

# 5) Plot 
p_landmarks_named <- ggplot(c1, aes(x = year_cleaned, y = EII, color = landmark_era)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(aes(group = landmark_era), method = "lm", formula = y ~ x,
              se = FALSE, linewidth = 1) +
  geom_vline(xintercept = c(1919, 1945, 1990, 2002),
             linetype = "dashed", color = "black", linewidth = 0.7) +
  geom_text_repel(
    data = label_df,
    aes(label = institution),
    size = 3, max.overlaps = 15, show.legend = FALSE
  ) +
  labs(
    title = "Figure 1.1A — Endurance of IGOs by Governance Landmark",
    subtitle = "Unique extremes labelled: earliest/latest founding, highest/lowest EII per era",
    x = "Founding Year", 
    y = "Endurance (EII)",
    color = "Landmark Era"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.margin = margin(t = 14, b = 14)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))   # 👈 makes legend wrap into rows

ggsave("analysis_images/C_1A_h_1.png", plot = p_landmarks_named,
       width = 9, height = 6, dpi = 300, bg = "white")

knitr::kable(era_summary,
             caption = "Table 1.1 — IGOs by Founding Era Statistics")

# ---- C1-H1: Earlier-established IGOs endure longer --------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(forcats); library(ggplot2)
  library(knitr); library(broom); library(lmtest); library(sandwich); library(ggrepel)
})

# 0) Data used
stopifnot(exists("conj1_df"))
c1 <- conj1_df

# 1) Construct endurance proxy (EII)
num_na0 <- function(x) replace_na(as.numeric(x), 0)
z <- function(x) as.numeric(scale(x))

bind_cols <- intersect(c(
  "binding_secondary_law_within_igo", "delegated_or_derived_powers_within_igo",
  "foundational_treaties_charters_within_igo", "binding_secondary_law_across_igo",
  "delegated_or_derived_powers_across_igo", "foundational_treaties_charters_across_igo"
), names(c1))

c1 <- c1 %>%
  mutate(
    endurance_age = 2025 - year_cleaned,  
    legal_binding_ct = if (length(bind_cols) > 0)
      rowSums(across(all_of(bind_cols), num_na0), na.rm = TRUE) else NA_real_,
    EII = rowMeans(cbind(
      z(ordinal_score_vertical_coordination),
      z(ordinal_score_strategies),
      z(ordinal_score_defined_objectives),
      z(ordinal_score_subject_matter),
      z(ordinal_score_horizontal),
      z(legal_binding_ct)
    ), na.rm = TRUE),
    founding_era_category = fct_reorder(founding_era_category, year_cleaned, .fun = min, .na_rm = TRUE)
  )

# 2) Define governance landmarks
c1 <- c1 %>%
  mutate(landmark_era = case_when(
    year_cleaned <= 1918 ~ "Pre–WWI Foundations",
    year_cleaned %in% 1919:1944 ~ "League of Nations Era",
    year_cleaned %in% 1945:1989 ~ "UN Charter",
    year_cleaned %in% 1990:2001 ~ "Post–Cold War Expansion",
    year_cleaned >= 2002 ~ "21st Century Governance",
    TRUE ~ "Unclassified"
  ))

# 3) Summary Table by Founding Era
era_summary <- c1 %>%
  group_by(founding_era_category) %>%
  summarise(
    N        = n(),
    year_med = median(year_cleaned, na.rm = TRUE),
    age_mean = mean(2025 - year_cleaned, na.rm = TRUE),
    EII_mean = mean(EII, na.rm = TRUE),
    .groups = "drop"
  )

# 4) Identify extremes with EII > 0.5 only
extremes <- c1 %>%
  group_by(founding_era_category) %>%
  summarise(
    earliest_year = min(year_cleaned, na.rm = TRUE),
    latest_year   = max(year_cleaned, na.rm = TRUE),
    top_EII_val   = max(EII, na.rm = TRUE),
    .groups = "drop"
  )

label_df <- c1 %>%
  inner_join(extremes, by = "founding_era_category") %>%
  filter(EII == top_EII_val & EII > 0.5) %>%
  distinct(institution, .keep_all = TRUE)

# Optional: Custom color palette
landmark_colors <- c(
  "Pre–WWI Foundations" = "#1b9e77",
  "League of Nations Era" = "#d95f02",
  "UN Charter" = "#7570b3",
  "Post–Cold War Expansion" = "#e7298a",
  "21st Century Governance" = "#66a61e"
)

# 5) Plot
p_landmarks_named <- ggplot(c1, aes(x = year_cleaned, y = EII, color = landmark_era)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(aes(group = landmark_era), method = "lm", formula = y ~ x,
              se = FALSE, linewidth = 1, alpha = 0.6) +
  geom_vline(xintercept = c(1919, 1945, 1990, 2002),
             linetype = "dashed", color = "black", linewidth = 0.6) +
  geom_hline(yintercept = mean(c1$EII, na.rm = TRUE),
             linetype = "dotted", color = "gray40", linewidth = 0.6) +
  geom_text_repel(
    data = label_df,
    aes(label = institution),
    size = 3.2,
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = 'grey70',
    show.legend = FALSE
  ) +
  scale_color_manual(values = landmark_colors) +
  labs(
    title = "Older IGOs Tend to Be More Enduring",
    subtitle = "Endurance Index plotted against founding year by governance era.\nOnly IGOs with high Endurance in each era are labeled.",
    x = "Founding Year of IGO", 
    y = "Endurance Index (EII, standardized)",
    color = "Landmark Era"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.margin = margin(t = 14, b = 14)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

p_landmarks_named

print(p_landmarks_named)

# 1) Categorise IGOs into High vs Low EEI
threshold <- median(c1$EII, na.rm = TRUE)

c1 <- c1 %>%
  mutate(
    EEI_category = ifelse(EII >= threshold, "High EEI", "Low EEI")
  )

# 2) Select top/bottom 7 IGOs
high7 <- c1 %>% arrange(desc(EII)) %>% slice_head(n = 7)
low7  <- c1 %>% arrange(EII) %>% slice_head(n = 7)

plot_df <- bind_rows(high7, low7)

# 3) Plot with shaded background
p_cats <- ggplot(plot_df, aes(x = year_cleaned, y = EII, color = EEI_category)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = threshold),
            fill = "grey", alpha = 0.1, inherit.aes = FALSE) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = threshold, ymax = Inf),
            fill = "darkgrey", alpha = 0.1, inherit.aes = FALSE) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = institution),
                  size = 3, max.overlaps = 20, show.legend = FALSE) +
  labs(title = "Figure 1.1b — IGOs by Endurance",
       subtitle = "High vs Low EEI categories (7 IGOs per group shown)",
       x = "Founding Year", y = "Endurance", color = "Category") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.box.margin = margin(t = 8, b = 6))

ggsave("analysis_images/C_1B_H1_EEI_categories.png", plot = p_cats,
       width = 9, height = 6, dpi = 300, bg = "white")

print(p_cats)


# ---- Contribution of Mandate Dimensions to EEI ----
c1_std <- c1 %>%
    mutate(
        spatial_z = scale(ordinal_score_spatial),
        subject_z = scale(ordinal_score_subject_matter),
        sources_z = scale(ordinal_score_sources),
        EEI_z = scale(EII)
    )
# Fit Linear regression
fit <- lm(EEI_z ~ spatial_z + subject_z + sources_z, data = c1_std)
# Extract tidy coefficients
coef_df <- broom::tidy(fit) %>%
    filter(term != "(Intercept)") %>%
    mutate(term = recode(term,
                                    "spatial_z" = "Spatial Jurisdiction",
                                    "subject_z" = "Subject-Matter Coverage",
                                    "sources_z" = "Sources of Jurisdiction"))
# Plot coefficients
p_contrib <- ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate, fill = term)) +
    geom_col(alpha = 0.8, width = 0.6) +
    geom_text(aes(label = round(estimate, 2)), vjust = -0.5, size = 3) +
    scale_fill_brewer(palette = "Set2") +
    labs(
        title = "Figure 1.2a Relative Contribution of Mandate Dimensions to Endurance",
        subtitle = "Standardised coefficients from linear regression",
        x = "Mandate Dimension",
        y = "Effect on Endurance"
    ) +
    theme_minimal() +
    theme(legend.position = "none")


# show R² and summary
summary(fit)

print(p_contrib)

# ==== Create mandate breadth score ====
c1 <- c1 %>%
  mutate(
    Mandate_Breadth = (0.12 * ordinal_score_spatial + 
                       0.3 * ordinal_score_subject_matter + 
                       0.47 * ordinal_score_sources),
    Mandate_Breadth_Cat = case_when(
      Mandate_Breadth >= 1.534 & Mandate_Breadth <= 2.896 ~ "Low",
      Mandate_Breadth >  2.896 & Mandate_Breadth <= 4.258 ~ "Medium",
      Mandate_Breadth >  4.258 & Mandate_Breadth <= 5.62  ~ "High",
      TRUE ~ NA_character_
    )
  )

# === Subsets ===
highlight_high   <- c1 %>% filter(Mandate_Breadth_Cat == "High")
highlight_medium <- c1 %>% filter(Mandate_Breadth_Cat == "Medium")
highlight_low    <- c1 %>% filter(Mandate_Breadth_Cat == "Low")

# === High Mandate Breadth Plot ===
p_high <- ggplot(c1, aes(x = Mandate_Breadth, y = EII,
                         color = Mandate_Breadth_Cat, label = institution)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(
    data = highlight_high,
    aes(label = institution),
    size = 3.5, max.overlaps = 12, segment.color = "grey60"
  ) +
  geom_vline(xintercept = 4.259, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Low" = "firebrick",
                                "Medium" = "steelblue",
                                "High" = "forestgreen")) +
  labs(
    title = "Figure 1.2B1 — High Mandate Breadth vs Endurance",
    subtitle = "Dashed line = threshold for Broadest Mandates",
    x = "Mandate Breadth Score", y = "Endurance (EEI)", color = "Mandate Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# === Medium Mandate Breadth Plot ===
p_medium <- ggplot(c1, aes(x = Mandate_Breadth, y = EII,
                           color = Mandate_Breadth_Cat, label = institution)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(
    data = highlight_medium,
    aes(label = institution),
    size = 3.5, max.overlaps = 12, segment.color = "grey60"
  ) +
  geom_vline(xintercept = 2.897, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 4.258, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Low" = "firebrick",
                                "Medium" = "steelblue",
                                "High" = "forestgreen")) +
  labs(
    title = "Figure 1.2B2 — Medium Mandate Breadth vs Endurance",
    subtitle = "Dashed lines = Medium category thresholds",
    x = "Mandate Breadth Score", y = "Endurance (EEI)", color = "Mandate Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# === Low Mandate Breadth Plot ===
p_low <- ggplot(c1, aes(x = Mandate_Breadth, y = EII,
                        color = Mandate_Breadth_Cat, label = institution)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(
    data = highlight_low,
    aes(label = institution),
    size = 3.5, max.overlaps = 12, segment.color = "grey60"
  ) +
  geom_vline(xintercept = 2.896, linetype = "dashed", color = "red") +
  scale_color_manual(values = c("Low" = "firebrick",
                                "Medium" = "steelblue",
                                "High" = "forestgreen")) +
  labs(
    title = "Figure 1.2B3 — Low Mandate Breadth vs Endurance",
    subtitle = "Dashed line = Narrow mandate threshold",
    x = "Mandate Breadth Score", y = "Endurance (EEI)", color = "Mandate Category"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# === Print all three ===
print(p_high)
print(p_medium)
print(p_low)


# Filter High Mandate Breadth IGOs
high_mandate <- c1 %>% filter(Mandate_Breadth_Cat == "High")

# Quick view
knitr::kable(high_mandate %>% 
               select(institution, year_cleaned, founding_era_category, 
                      Mandate_Breadth, EII),
             caption = "Table 1.2A — High Mandate Breadth IGOs")

# ---- Filter for Positive EEI High Mandate Breadth IGOs ----
pos_high <- c1 %>%
  filter(Mandate_Breadth_Cat %in% c("Medium", "High"), EII > 0)

# Median split of EEI within this group
eei_median <- median(pos_high$EII, na.rm = TRUE)

pos_high <- pos_high %>%
  mutate(
    EEI_cat = case_when(
      EII >= eei_median ~ "High-High (HH)",
      EII < eei_median ~ "Low-High (LH)",
      TRUE ~ NA_character_
    )
  )

# ==== Plot ====
p_hh <- ggplot(pos_high, aes(x = Mandate_Breadth, y = EII,
                             color = EEI_cat, label = institution)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(size = 3, max.overlaps = 15) +
  scale_color_manual(values = c("High-High (HH)" = "darkgreen", "Low-High (LH)" = "orange")) +
  labs(title = "Figure 1.2C — High Mandate Breadth IGOs by EEI Category",
       subtitle = "Distinguishing High–High vs Low–High performers",
       x = "Mandate Breadth (Weighted Score)",
       y = "Endurance (EEI)",
       color = "Category") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p_hh)

# ==== Filter to High-High (HH) IGOs ====
hh_igos <- pos_high %>%
  filter(EEI_cat == "High-High (HH)")

# Long format for plotting 3 jurisdictional dimensions
hh_long <- hh_igos %>%
  select(institution, founding_era_category,
         sources = ordinal_score_sources,
         spatial = ordinal_score_spatial,
         subject = ordinal_score_subject_matter) %>%
  tidyr::pivot_longer(cols = c(sources, spatial, subject),
                      names_to = "Dimension",
                      values_to = "Score")

# ==== Plot grouped bar chart ====
library(ggplot2)

p_hh_dims <- ggplot(hh_long, aes(x = reorder(institution, Score),
                                 y = Score, fill = Dimension)) +
  geom_col(position = position_dodge(width = 0.8)) +
  coord_flip() +
  scale_fill_manual(values = c("sources" = "#1b9e77",
                               "spatial" = "#d95f02",
                               "subject" = "#7570b3")) +
  labs(title = "Figure 1.2D — Jurisdictional Dimensions of High–High IGOs",
       subtitle = "Scores across sources, spatial reach, and subject-matter breadth",
       x = "IGO",
       y = "Jurisdictional Score (0–10)",
       fill = "Dimension") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

print(p_hh_dims)

# ============================================================
# High–High (HH) IGOs — Three-Lens Analysis for Hypothesis 1.2
# ============================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(forcats); library(stringr)
})

# --------- 0) Filter to HH IGOs ----------
stopifnot(exists("pos_high"))
hh_igos <- pos_high %>% filter(EEI_cat == "High-High (HH)")

# Make sure expected columns exist
needed_cols <- c("institution","founding_era_category",
                 "ordinal_score_sources","ordinal_score_spatial","ordinal_score_subject_matter")
stopifnot(all(needed_cols %in% names(hh_igos)))

# Nice era palette (covers most labels you’ve used)
era_colors <- c(
  "Early Founding Years (Pre-1900)"       = "#8dd3c7",
  "Early 20th Century (1900-1945)"        = "#ffffb3",
  "Post-WWII Boom (1946-1960)"            = "#bebada",
  "Cold War Era I (1961-1970)"            = "#fb8072",
  "Cold War Era II (1971-1980)"           = "#80b1d3",
  "Late Cold War (1981-1990)"             = "#fdb462",
  "Post-Cold War (1991-2000)"             = "#b3de69",
  "Globalisation Era (2001-2010)"         = "#fccde5",
  "SDG & Climate Action Era (2011-2020)"  = "#d9d9d9"
)

# ============================================================
# STEP 1 — Distribution across the three dimensions (per IGO)
# ============================================================
hh_long <- hh_igos %>%
  select(institution, founding_era_category,
         sources = ordinal_score_sources,
         spatial = ordinal_score_spatial,
         subject = ordinal_score_subject_matter) %>%
  pivot_longer(cols = c(sources, spatial, subject),
               names_to = "Dimension", values_to = "Score") %>%
  mutate(Dimension = factor(Dimension, levels = c("sources","spatial","subject"),
                            labels = c("Sources","Spatial","Subject")))

# Order IGOs by their average across the three dimensions (helps readability)
igo_order <- hh_long %>%
  group_by(institution) %>%
  summarise(avg_score = mean(Score, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_score)) %>% pull(institution)

hh_long$institution <- factor(hh_long$institution, levels = igo_order)

p1_distribution <- ggplot(hh_long,
                          aes(x = institution, y = Score, fill = Dimension)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(aes(label = round(Score,1)),
            position = position_dodge(width = 0.8),
            vjust = -0.25, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("Sources"="#4DB6AC","Spatial"="#5C6BC0","Subject"="#FF8A65")) +
  labs(title = "Figure 1.2E Distribution of Jurisdictional Dimensions",
       subtitle = "Scores for Sources, Spatial, and Subject per institution",
       x = "Institution", y = "Score (0–10)", fill = "Dimension") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
print(p1_distribution)

# =================================================================
# STEP 2 — What’s common? Count the specific elements (within_igo)
# =================================================================

# Safely intersect columns present in your data (robust to schema changes)
sources_cols <- intersect(c(
  "bilateral_multilateral_arrangements_within_igo",
  "binding_secondary_law_within_igo",
  "compliance_oversight_within_igo",
  "customary_soft_law_within_igo",
  "delegated_or_derived_powers_within_igo",
  "foundational_treaties_charters_within_igo",
  "non_binding_secondary_law_within_igo",
  "strategic_frameworks_within_igo",
  "technical_norms_standards_within_igo"
), names(hh_igos))

spatial_cols <- intersect(c(
  "archipelago_within_igo",
  "coastal_zone_within_igo",
  "contiguous_zone_cz_within_igo",
  "enclosed_or_semi_enclosed_sea_within_igo",
  "exclusive_economic_zone_eez_within_igo",
  "extended_continental_shelf_cs_within_igo",
  "high_seas_within_igo",
  "internal_waters_within_igo",
  "territorial_sea_ts_within_igo",
  "the_area_within_igo"
), names(hh_igos))

subject_cols <- intersect(c(
  "biodiversity_ecosystem_conservation_within_igo",
  "cultural_heritage_traditional_knowledge_data_governance_within_igo",
  "disaster_risk_reduction_resilience_within_igo",
  "environmental_protection_climate_change_within_igo",
  "human_rights_social_justice_advocacy_within_igo",
  "international_cooperation_governance_within_igo",
  "research_science_innovation_within_igo",
  "security_safety_within_igo",
  "sustainable_development_capacity_building_within_igo",
  "trade_investment_economic_cooperation_within_igo"
), names(hh_igos))

pretty_element <- function(x) {
  x %>%
    str_remove("_within_igo$") %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

count_elements <- function(df, cols) {
  if (length(cols) == 0) return(tibble(Element = character(), Count = integer()))
  df %>%
    select(institution, all_of(cols)) %>%
    pivot_longer(cols = -institution, names_to = "Element", values_to = "Value") %>%
    filter(!is.na(Value), Value > 0) %>%
    group_by(Element) %>%
    summarise(Count = n_distinct(institution), .groups = "drop") %>%
    mutate(Element = pretty_element(Element)) %>%
    arrange(desc(Count))
}

sources_count <- count_elements(hh_igos, sources_cols)
spatial_count <- count_elements(hh_igos, spatial_cols)
subject_count <- count_elements(hh_igos, subject_cols)

plot_counts <- function(df, title, subtitle) {
  ggplot(df, aes(x = reorder(Element, Count), y = Count)) +
    geom_col(fill = "#607D8B", width = 0.7) +
    geom_text(aes(label = Count), hjust = -0.1, size = 3.2) +
    coord_flip() +
    expand_limits(y = max(df$Count, 1) * 1.12) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Number of HH IGOs") +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major.y = element_blank())
}

p2_sources <- plot_counts(sources_count,
                          "Figure 1.2F Sources of Jurisdiction IGOs",
                          "Count of IGOs using each legal/authority source")
p2_spatial  <- plot_counts(spatial_count,
                           "Figure 1.3G Spatial Domains among HH IGOs",
                           "Count of HH IGOs active in each oceanic domain")
p2_subject  <- plot_counts(subject_count,
                           "Figure 1.4H Subject-Matter Areas among HH IGOs",
                           "Count of HH IGOs working in each area")

print(p2_sources)

print(p2_spatial)

print(p2_subject)

# ============================================================
# STEP 3 — Founding-era overlays (who uses what, by era?)
# ============================================================

count_elements_by_era <- function(df, cols) {
  if (length(cols) == 0) return(tibble())
  df %>%
    select(institution, founding_era_category, all_of(cols)) %>%
    pivot_longer(cols = -(institution:founding_era_category),
                 names_to = "Element", values_to = "Value") %>%
    filter(!is.na(Value), Value > 0) %>%
    group_by(Element, founding_era_category) %>%
    summarise(Count = n_distinct(institution), .groups = "drop") %>%
    mutate(Element = pretty_element(Element))
}

sources_by_era <- count_elements_by_era(hh_igos, sources_cols)
spatial_by_era <- count_elements_by_era(hh_igos, spatial_cols)
subject_by_era <- count_elements_by_era(hh_igos, subject_cols)

plot_by_era <- function(df, title, subtitle) {
  if (nrow(df) == 0) return(ggplot() + theme_void())
  ggplot(df, aes(x = reorder(Element, dplyr::desc(Count)), y = Count,
                 fill = founding_era_category)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = era_colors) +
    labs(title = title, subtitle = subtitle,
         x = NULL, y = "Number of HH IGOs", fill = "Founding Era") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank())
}

p3_sources_era <- plot_by_era(sources_by_era,
                              "Sources of Jurisdiction — IGOs by Founding Era",
                              "Stacked counts show which eras adopted each source")
p3_spatial_era <- plot_by_era(spatial_by_era,
                              "Spatial Domains Jurisdiction",
                              "Stacked counts show which eras are active in each domain")
p3_subject_era <- plot_by_era(subject_by_era,
                              "Subject Areas — by Founding Era",
                              "Stacked counts show which eras emphasize each area")

print(p3_sources_era)

print(p3_spatial_era)

print(p3_subject_era)

# === Create summary table ===
coord_table <- c1 %>%
  select(institution, EII,
         vertical_coordination = ordinal_score_vertical_coordination,
         horizontal_coordination = ordinal_score_horizontal) %>%
  arrange(desc(EII))

# Print nicely for markdown
knitr::kable(
  coord_table,
  caption = "Table 1.3A — EEI and Coordination Scores (Vertical & Horizontal) by IGO",
  digits = 2
)


# Standardise predictors
c1_std <- c1 %>%
  mutate(
    vert_z = scale(ordinal_score_vertical_coordination),
    horiz_z = scale(ordinal_score_horizontal),
    EEI_z = scale(EII)
  )

# Fit regression with standardised values
coord_model_std <- lm(EEI_z ~ vert_z + horiz_z, data = c1_std)

summary(coord_model_std)

# ==== Weighted Coordination Score based on regression coefficients ====
# Use standardized regression coefficients as weights
w_vert  <- 0.286   # from regression
w_horiz <- 0.232   # from regression

# 1) Create weighted coordination score
c1 <- c1 %>%
  mutate(
    Coordination_Score = (w_vert * scale(ordinal_score_vertical_coordination)[,1] +
                          w_horiz * scale(ordinal_score_strategies)[,1])
  )

# 2) Categorize into Low / Medium / High
c1 <- c1 %>%
  mutate(
    Coord_Cat = case_when(
      Coordination_Score <= quantile(Coordination_Score, 1/3, na.rm = TRUE) ~ "Low",
      Coordination_Score <= quantile(Coordination_Score, 2/3, na.rm = TRUE) ~ "Medium",
      TRUE ~ "High"
    )
  )

# 3) Highlight High EEI IGOs
highlight_high <- c1 %>% filter(Coord_Cat == "High")

# 4) Plot EEI vs Coordination Score
p_coord <- ggplot(c1, aes(x = Coordination_Score, y = EII,
                          color = Coord_Cat, label = institution)) +
  geom_point(size = 4, alpha = 0.8) +
  geom_text_repel(
    data = highlight_high,
    aes(label = institution),
    size = 3.5, max.overlaps = 15, segment.color = "grey60"
  ) +
  scale_color_manual(values = c("Low" = "firebrick",
                                "Medium" = "steelblue",
                                "High" = "forestgreen")) +
  labs(
    title = "Figure 1.3A — Coordination Score vs Endurance (EEI)",
    subtitle = "Coordination Score = 0.29*Vertical + 0.23*Horizontal",
    x = "Weighted Coordination Score",
    y = "Endurance (EEI)",
    color = "Coordination Category"
  ) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")

print(p_coord)


# ==== Zoom into Coordination Categories ====

# Define colors for founding era
era_colors <- c(
  "Early 20th Century (1900-1945)" = "#e69f00",
  "Post-WWII Boom (1946-1960)"     = "#56b4e9",
  "Cold War I (1961-1970)"         = "#009e73",
  "Cold War II (1971-1980)"        = "#f0e442",
  "Late Cold War (1981-1990)"      = "#d55e00",
  "Post-Cold War (1991-2000)"      = "#cc79a7",
  "Globalisation Era (2001-2020)"  = "#8da0cb"
)

# Function to plot each category separately
plot_category <- function(df, category, fig_label){
  ggplot(df %>% filter(Coord_Cat == category),
         aes(x = Coordination_Score, y = EII,
             color = founding_era_category,
             label = institution)) +
    geom_point(size = 4, alpha = 0.8) +
    geom_text_repel(size = 3.5, max.overlaps = 12, segment.color = "grey60") +
    scale_color_manual(values = era_colors) +
    labs(
      title = paste0("Figure ", fig_label, " — EEI vs Weighted Coordination Score (", category, " Coordination)"),
      subtitle = "Bubble colors = Founding Era",
      x = "Coordination Score Index",
      y = "Endurance (EEI)",
      color = "Founding Era"
    ) +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom")
}

# Create plots
p_high   <- plot_category(c1, "High", "1.3B")
p_medium <- plot_category(c1, "Medium", "1.3C")
p_low    <- plot_category(c1, "Low", "1.3D")


print(p_high)

print(p_medium)

print(p_low)

# ==== Hypothesis 1.4: Strategic & Objective Diversification and Endurance ====

# Select relevant variables
h14_df <- c1 %>%
  select(institution, EII,
         objectives = ordinal_score_defined_objectives,
         strategies = ordinal_score_strategies)

# Print table for report
knitr::kable(h14_df,
             caption = "Table 1.4A — Top 5 IGOs by EEI with Objectives & Strategies Scores",
             col.names = c("Institution", "EEI", "Objectives Score", "Strategies Score"),
             digits = 2)
# ==== Descriptive Statistics for H1.4 ====

# Overall descriptive statistics
desc_stats <- h14_df %>%
  summarise(
    mean_EEI   = mean(EII, na.rm = TRUE),
    sd_EEI     = sd(EII, na.rm = TRUE),
    min_EEI    = min(EII, na.rm = TRUE),
    max_EEI    = max(EII, na.rm = TRUE),
    
    mean_obj   = mean(objectives, na.rm = TRUE),
    sd_obj     = sd(objectives, na.rm = TRUE),
    min_obj    = min(objectives, na.rm = TRUE),
    max_obj    = max(objectives, na.rm = TRUE),
    
    mean_strat = mean(strategies, na.rm = TRUE),
    sd_strat   = sd(strategies, na.rm = TRUE),
    min_strat  = min(strategies, na.rm = TRUE),
    max_strat  = max(strategies, na.rm = TRUE)
  )

print(desc_stats)

# 2) Optionally, quick psych::describe() for detailed summary
psych::describe(h14_df[, c("EII", "objectives", "strategies")])


# Categorise EEI into High/Low based on median
median_EEI <- median(c1$EII, na.rm = TRUE)

c1 <- c1 %>%
  mutate(EEI_cat = ifelse(EII >= median_EEI, "High EEI", "Low EEI"))

# 1) EEI vs Objectives
p_obj <- ggplot(c1, aes(x = ordinal_score_defined_objectives, y = EII,
                        color = EEI_cat, label = institution)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(size = 3, max.overlaps = 12) +
  geom_hline(yintercept = median_EEI, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("High EEI" = "forestgreen", "Low EEI" = "firebrick")) +
  labs(title = "Figure 1.4A — Endurance vs Objectives Score",
       subtitle = paste0("Dashed line = EEI median cutoff (", round(median_EEI, 2), ")"),
       x = "Objectives Score (Ordinal)",
       y = "Endurance Index (EEI)",
       color = "EEI Category") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# 2) EEI vs Strategies
p_strat <- ggplot(c1, aes(x = ordinal_score_strategies, y = EII,
                          color = EEI_cat, label = institution)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(size = 3, max.overlaps = 12) +
  geom_hline(yintercept = median_EEI, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("High EEI" = "forestgreen", "Low EEI" = "firebrick")) +
  labs(title = "Figure 1.4B — Endurance vs Strategies Score",
       subtitle = paste0("Dashed line = EEI median cutoff (", round(median_EEI, 2), ")"),
       x = "Strategies Score (Ordinal)",
       y = "Endurance Index (EEI)",
       color = "EEI Category") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")


# Print both
print(p_obj)

print(p_strat)

# Standardise
c1_std <- c1 %>%
  mutate(
    EEI_z = scale(EII),
    obj_z = scale(ordinal_score_defined_objectives),
    strat_z = scale(ordinal_score_strategies)
  )

# Run regression
model_h14 <- lm(EEI_z ~ obj_z + strat_z, data = c1_std)
summary(model_h14)

# Extract tidy results
coef_df <- tidy(model_h14, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term,
                       "obj_z" = "Objectives Diversification",
                       "strat_z" = "Strategies Diversification"))


# ---- Coefficient Plot ----
ggplot(coef_df, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(color = "steelblue", size = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_text(aes(label = paste0("β = ", round(estimate, 2))),
            vjust = -1.2, color = "black", size = 4.5) +
  labs(
    title = "Figure 1.4C — Contribution of Objectives vs Strategies to EEI",
    subtitle = "Standardised regression coefficients (β) with 95% CI",
    x = "", y = "Contribution on Endurance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(size = 12, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# Calculate 75th percentiles
p75_EEI <- quantile(c1$EII, 0.75, na.rm = TRUE)
p75_obj <- quantile(c1$ordinal_score_defined_objectives, 0.75, na.rm = TRUE)
p75_strat <- quantile(c1$ordinal_score_strategies, 0.75, na.rm = TRUE)

# Categorize into High/Low based on 75th percentiles
c1 <- c1 %>%
  mutate(
    EEI_cat = ifelse(EII >= p75_EEI, "High EEI", "Low EEI"),
    Obj_cat = ifelse(ordinal_score_defined_objectives >= p75_obj, "High Objectives", "Low Objectives"),
    Strat_cat = ifelse(ordinal_score_strategies >= p75_strat, "High Strategies", "Low Strategies")
  )

# Unified plot: High EEI, High Objectives, and High Strategies
ggplot(c1, aes(x = ordinal_score_defined_objectives,
               y = ordinal_score_strategies,
               color = EEI_cat,
               shape = interaction(Obj_cat, Strat_cat),
               label = institution)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text_repel(size = 3, max.overlaps = 12) +
  scale_color_manual(values = c("High EEI" = "forestgreen", "Low EEI" = "firebrick")) +
  scale_shape_manual(values = c("High Objectives,High Strategies" = 17,
                                 "High Objectives,Low Strategies" = 15,
                                 "Low Objectives,High Strategies" = 16,
                                 "Low Objectives,Low Strategies" = 1)) +
  labs(
    title = "Figure 1.4D The Highs (EEI, Objectives, and Strategies) IGOs",
    subtitle = paste0("Cutoffs: EEI (", round(p75_EEI, 2), "), Objectives (", round(p75_obj, 2),
                      "), Strategies (", round(p75_strat, 2), ")"),
    x = "Objectives Score (Ordinal)",
    y = "Strategies Score (Ordinal)",
    color = "Endurance",
    shape = "Objectives/Strategies Category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )


# Calculate 75th percentiles
p75_EEI <- quantile(c1$EII, 0.75, na.rm = TRUE)
p75_obj <- quantile(c1$ordinal_score_defined_objectives, 0.75, na.rm = TRUE)
p75_strat <- quantile(c1$ordinal_score_strategies, 0.75, na.rm = TRUE)

# Categorize
c1 <- c1 %>%
  mutate(
    EEI_cat = ifelse(EII >= p75_EEI, "High EEI", "Low EEI"),
    Obj_cat = ifelse(ordinal_score_defined_objectives >= p75_obj, "High Objectives", "Low Objectives"),
    Strat_cat = ifelse(ordinal_score_strategies >= p75_strat, "High Strategies", "Low Strategies")
  )

high_c1 <- c1 %>%
  filter(
    EII >= p75_EEI,
    ordinal_score_defined_objectives >= p75_obj,
    ordinal_score_strategies >= p75_strat
  )

# Match strategy and objective columns
strategy_cols <- grep("_(within)_igo$", names(high_c1), value = TRUE) %>%
  grep("capacity_development_operational_delivery|collaboration_partnerships_networks|environmental_climate_biodiversity_action|financial_budgetary_management|inclusion_rights_social_justice|innovation_technology_development|knowledge_research_data_systems|monitoring_evaluation_accountability|policy_regulation_legal_frameworks|strategic_institutional_planning", ., value = TRUE)

objective_cols <- grep("_(within)_igo$", names(high_c1), value = TRUE) %>%
  grep("environmental_action|financial_stewardship|governance_planning|inclusion_rights|innovation_technology|knowledge_data|monitoring_accountability|operational_delivery|partnerships_networks|policy_regulation", ., value = TRUE)

# Sum across IGOs for each strategy and objective
strategy_summary <- colSums(high_c1[, strategy_cols], na.rm = TRUE)
objective_summary <- colSums(high_c1[, objective_cols], na.rm = TRUE)

# Turn into data frames
strategy_df <- data.frame(
  Strategy = names(strategy_summary),
  Count = as.integer(strategy_summary)
)

objective_df <- data.frame(
  Objective = names(objective_summary),
  Count = as.integer(objective_summary)
)

# Optional: Clean names
clean_names <- function(x) {
  gsub("_(within|across)_igo$", "", x)
}
strategy_df$Strategy <- clean_names(strategy_df$Strategy)
objective_df$Objective <- clean_names(objective_df$Objective)


# For strategies
strategy_df <- strategy_df %>%
  mutate(Pct = round(Count / sum(Count) * 100))

ggplot(strategy_df, aes(x = reorder(Strategy, Count), y = Count)) +
  geom_col(fill = "#D9F0A3") +
  geom_text(aes(label = paste0(Pct, "%")), hjust = -0.1, size = 4) +
  coord_flip() +
  labs(
    title = "Figure 1.4b Strategies Among High Endurance IGOs",
    x = "Strategy",
    y = "Number of IGOs"
  ) +
  theme_minimal()

strategy_df

# Same for objectives
objective_df <- objective_df %>%
  mutate(Pct = round(Count / sum(Count) * 100))

ggplot(objective_df, aes(x = reorder(Objective, Count), y = Count)) +
  geom_col(fill = "darkorange") +
  geom_text(aes(label = paste0(Pct, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(
    title = "Figure 1.4c Objectives Among High-EEI IGOs",
    x = "Objective",
    y = "Number of IGOs"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)
library(ggrepel)

# Improved reusable plot function
plot_igo <- function(data, title, subtitle = NULL) {
  
  # Optional: extract unique levels for shape and size mappings
  shape_levels <- unique(data$Coord_Cat)
  size_levels <- unique(data$Mandate_Breadth_Cat)
  
  # Custom shape and size values — adapt if levels vary
  shape_vals <- c(16, 17, 15)[seq_along(shape_levels)]
  size_vals <- c(3, 5, 7)[seq_along(size_levels)]
  
  ggplot(data, aes(
    x = ordinal_score_defined_objectives,
    y = ordinal_score_strategies,
    shape = Coord_Cat,
    size = Mandate_Breadth_Cat,
    color = EEI_category,
    label = institution
  )) +
    geom_point(alpha = 0.8, stroke = 0.2) +
    geom_text_repel(size = 3, max.overlaps = 15) +
    facet_wrap(~ founding_era_category) +
    
    # Manual scales
    scale_color_manual(
      values = c("High EEI" = "forestgreen", "Low EEI" = "firebrick"),
      drop = FALSE
    ) +
    scale_shape_manual(
      values = setNames(shape_vals, shape_levels),
      drop = FALSE
    ) +
    scale_size_manual(
      values = setNames(size_vals, size_levels),
      drop = FALSE
    ) +
    
    # Labels
    labs(
      title = title,
      subtitle = subtitle,
      x = "Objectives Score",
      y = "Strategies Score",
      color = "Endurance Intensity (EEI)",
      shape = "Coordination Category",
      size = "Mandate Breadth"
    ) +
    
    # Theme customization
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.6, "cm"),
      legend.spacing.y = unit(0.2, "cm"),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    
    # Arrange legends in rows
    guides(
      color = guide_legend(nrow = 1, byrow = TRUE, override.aes = list(size = 4)),
      shape = guide_legend(nrow = 1, byrow = TRUE),
      size = guide_legend(nrow = 1, byrow = TRUE)
    )
}

# Create filtered data
high_eei_data <- c1 %>% filter(EEI_category == "High EEI")
low_eei_data  <- c1 %>% filter(EEI_category == "Low EEI")

# Generate plots
plot_high <- plot_igo(high_eei_data, 
                      title = "High EEI IGOs: Institutional Design and Endurance")

plot_low <- plot_igo(low_eei_data, 
                     title = "Low EEI IGOs: Institutional Design and Endurance")


plot_high 

plot_low
