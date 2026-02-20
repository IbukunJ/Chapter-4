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

# Suppose your dataframe is merged_df
colnames(merged_df) <- clean_to_snake_case(colnames(merged_df))


full <- merged_df
nms <- names(full)

# helper: build family columns (within/across variants)
fam_cols <- function(terms, nms) {
  pat <- paste0("^(", paste(terms, collapse="|"), ")_(withinigo|acrossigo)$")
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
horizontal_cols <- fam_cols(horizontal_terms, nms)

# Scores and identifiers
score_cols <- intersect(c(
  "ordinal_score_spatial","ordinal_score_vertical_coordination",
  "ordinal_score_subject_matter","ordinal_score_strategies",
  "ordinal_score_defined_objectives","ordinal_score_defined_inter",
  "ordinal_score_sources", "ordinal_score_horizontal"
), nms)

id_cols <- intersect(c("institution","year_cleaned","founding_era_category"), nms)
density_cols <- intersect(c("foundingdensity_5yr","cumulativestock"), nms)

# --- Conjecture 3: embeddedness & legitimacy --------------------------------
strategy_legit <- grep("^(inclusion_rights_social_justice|financial_budgetary_management|monitoring_evaluation_accountability|policy_regulation_legal_frameworks|strategic_institutional_planning)_(withinigo|acrossigo)$", nms, value = TRUE)
objective_legit <- grep("^(inclusion_rights|financial_stewardship|monitoring_accountability|governance_planning|policy_regulation|partnerships_networks)_(withinigo|acrossigo)$", nms, value = TRUE)
vertical_compliance <- grep("^reporting_compliance_mechanisms_(withinigo|acrossigo)$", nms, value = TRUE)

c3_cols <- unique(c(
  id_cols,
  intersect(c("ordinal_score_defined_inter","ordinal_score_sources",
              "ordinal_score_defined_objectives","ordinal_score_strategies"), score_cols),
  inter_cols, strategy_legit, objective_legit, vertical_compliance, source_cols
))
conj3_df <- dplyr::select(full, all_of(c3_cols))

# --- sanity checks ----------------------------------------------------------
cat("Conj3 cols:", length(c3_cols), "\n")


colnames(merged_df)
# Assume you have a data frame called df
write.csv(merged_df, "Data/full_data.csv", row.names = FALSE)


# Columns for Legal Authority
lai_cols <- c(
  "binding_secondary_law_withinigo", "binding_secondary_law_acrossigo",
  "compliance_oversight_withinigo", "compliance_oversight_acrossigo",
  "delegated_or_derived_powers_withinigo", "delegated_or_derived_powers_acrossigo",
  "foundational_treaties_charters_withinigo", "foundational_treaties_charters_acrossigo",
  "non_binding_secondary_law_withinigo", "non_binding_secondary_law_acrossigo",
  "strategic_frameworks_withinigo", "strategic_frameworks_acrossigo",
  "technical_norms_standards_withinigo", "technical_norms_standards_acrossigo",
  "bilateral_multilateral_arrangements_withinigo", "bilateral_multilateral_arrangements_acrossigo",
  "customary_soft_law_withinigo", "customary_soft_law_acrossigo",
  "other_governance_instruments_withinigo", "other_governance_instruments_acrossigo"
)

# Compute LAI as row mean (index)
conj3_df <- conj3_df %>%
  mutate(LAI = rowMeans(across(all_of(lai_cols)), na.rm = TRUE))

# Columns that contribute to legitimacy participation
lpi_cols <- c(
  "civil_society_engagement_withinigo", "civil_society_engagement_acrossigo",
  "ngo_engagement_withinigo", "ngo_engagement_acrossigo",
  "private_sector_partnerships_withinigo", "private_sector_partnerships_acrossigo",
  "scientific_community_linkages_withinigo", "scientific_community_linkages_acrossigo",
  "treaty_body_coordination_withinigo", "treaty_body_coordination_acrossigo",
  "un_system_collaboration_withinigo", "un_system_collaboration_acrossigo",
  "reporting_compliance_mechanisms_withinigo", "reporting_compliance_mechanisms_acrossigo"
)

# Construct Legitimacy Participation Index (LPI)
conj3_df <- conj3_df %>%
  mutate(LPI = rowMeans(across(all_of(lpi_cols)), na.rm = TRUE))


# Model: Legal Authority → Legitimacy
m_h31 <- lm(LPI ~ LAI, data = conj3_df)
summary(m_h31)

library(ggeffects)
library(ggrepel)

pred_h31 <- ggpredict(m_h31, terms = "LAI [all]")

p_h31 <- ggplot(pred_h31, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .2, fill = "skyblue") +
  geom_line(size = 1.2, color = "blue") +
  # add actual data points with IGOs
  geom_point(data = conj3_df, aes(x = LAI, y = LPI, color = founding_era_category), size = 3, alpha = 0.7) +
  geom_text_repel(data = conj3_df, aes(x = LAI, y = LPI, label = institution, color = founding_era_category),
                  size = 3, max.overlaps = 12) +
  scale_color_brewer(palette = "Dark2") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Figure 3.1 Legal Authority and Legitimacy",
    x = "Legal Authority Index (LAI)",
    y = "Legitimacy Participation Index (LPI)",
    color = "Founding Era",
    caption = "IGOs with stronger legal authority."
  )

print(p_h31)

# Construct Mandate Breadth Score
conj3_df$Mandate_Breadth <- rowMeans(conj3_df[, c(
  "ordinal_score_defined_objectives",
  "ordinal_score_strategies"
)], na.rm = TRUE)

# Standardize for comparability
conj3_df$Mandate_Breadth_z <- scale(conj3_df$Mandate_Breadth)

# Regression: Mandate breadth → Legitimacy
model_h3_2 <- lm(LPI ~ Mandate_Breadth_z, data = conj3_df)
summary(model_h3_2)


library(ggplot2)
library(ggrepel)
library(dplyr)

# --- data ---
conj3_df <- conj3_df %>%
  mutate(
    Mandate_Breadth = rowSums(select(., 
                                     financial_budgetary_management_withinigo,
                                     inclusion_rights_social_justice_withinigo,
                                     monitoring_evaluation_accountability_withinigo,
                                     policy_regulation_legal_frameworks_withinigo,
                                     strategic_institutional_planning_withinigo,
                                     financial_budgetary_management_acrossigo,
                                     inclusion_rights_social_justice_acrossigo,
                                     monitoring_evaluation_accountability_acrossigo,
                                     policy_regulation_legal_frameworks_acrossigo,
                                     strategic_institutional_planning_acrossigo
    ), na.rm = TRUE),
    Mandate_Breadth_z = scale(Mandate_Breadth)[,1]
  )

# --- Identify IGOs with both high mandate breadth & high legitimacy ---
threshold_mb <- quantile(conj3_df$Mandate_Breadth, 0.75, na.rm = TRUE) # top 25%
threshold_lpi <- quantile(conj3_df$LPI, 0.75, na.rm = TRUE)

high_igos <- conj3_df %>%
  filter(Mandate_Breadth >= threshold_mb & LPI >= threshold_lpi)

# --- Plot ---
p_h32 <- ggplot(conj3_df, aes(x = Mandate_Breadth, y = LPI, 
                              color = founding_era_category)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  geom_point(data = high_igos, aes(x = Mandate_Breadth, y = LPI), 
             size = 4, color = "red", shape = 17) +  # highlight in red triangles
  geom_text_repel(data = high_igos, aes(label = institution),
                  size = 3.5, color = "red", fontface = "bold") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Figure 3.2 Hypothesis 3.2 — Mandate Breadth and Legitimacy",
    subtitle = "Highlighted: IGOs with broad mandates AND high legitimacy",
    x = "Mandate Breadth (count of domains)",
    y = "Legitimacy Index (LPI)",
    caption = "Red triangles = IGOs in the top quartile for both mandate breadth and legitimacy.\nDashed line = overall linear trend."
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, face = "italic", size = 10)
  )

print(p_h32)

library(dplyr)

# --- Derive IEI ---
conj3_df <- conj3_df %>%
  mutate(
    IEI_raw = rowSums(select(., 
      civil_society_engagement_withinigo, donor_partnerships_withinigo,
      intergovernmental_consultations_withinigo, ngo_engagement_withinigo,
      private_sector_partnerships_withinigo, regional_body_coordination_withinigo,
      scientific_community_linkages_withinigo, technical_or_expert_groups_withinigo,
      treaty_body_coordination_withinigo, un_system_collaboration_withinigo,
      civil_society_engagement_acrossigo, donor_partnerships_acrossigo,
      intergovernmental_consultations_acrossigo, ngo_engagement_acrossigo,
      private_sector_partnerships_acrossigo, regional_body_coordination_acrossigo,
      scientific_community_linkages_acrossigo, technical_or_expert_groups_acrossigo,
      treaty_body_coordination_acrossigo, un_system_collaboration_acrossigo
    ), na.rm = TRUE),
    
    IEI = scale(IEI_raw)[,1]
  )


library(dplyr)
library(ggplot2)

# --- Model: Embeddedness and Legitimacy ---
m_h33 <- lm(LPI ~ IEI, data = conj3_df)
summary(m_h33)

# --- Visualization ---
p_h33 <- ggplot(conj3_df, aes(x = IEI, y = LPI, color = founding_era_category, label = institution)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue") +
  ggrepel::geom_text_repel(
    data = conj3_df %>% filter(LPI == max(LPI) | LPI == min(LPI) | IEI == max(IEI) | IEI == min(IEI)),
    aes(label = institution),
    size = 3.5,
    box.padding = 0.3,
    point.padding = 0.3,
    max.overlaps = 10,
    color = "black"
  ) +
  labs(
    title = "Figure 3.3 Hypothesis 3.3 — Embeddedness and Legitimacy",
    x = "Inter-Institutional Embeddedness Index (IEI)",
    y = "Legitimacy Participation Index (LPI)",
    caption = "IGOs embedded in broader governance networks (coordination, partnerships, UN ties) are hypothesized to have higher legitimacy. 
The fitted line shows predicted legitimacy with 95% CI. IGOs with extreme values of embeddedness or legitimacy are labeled for interpretation."
  ) +
  scale_color_brewer(palette = "Set2", name = "Founding Era") +
  theme_minimal(base_size = 13) +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", size = 10, color = "gray40"),
    plot.title = element_text(size = 14, face = "bold")
  )
print(p_h33)

# Define strategy-related columns
strategy_cols <- c(
  "financial_budgetary_management_withinigo",
  "policy_regulation_legal_frameworks_withinigo",
  "strategic_institutional_planning_withinigo",
  "monitoring_evaluation_accountability_withinigo",
  "inclusion_rights_social_justice_withinigo",
  "technical_or_expert_groups_withinigo",
  "civil_society_engagement_withinigo",
  "private_sector_partnerships_withinigo",
  "scientific_community_linkages_withinigo",
  "un_system_collaboration_withinigo",
  "ngo_engagement_withinigo"
)

# Create Strategy Breadth Index (SBI)
conj3_df$SBI <- rowSums(conj3_df[, strategy_cols], na.rm = TRUE)


# Basic model
model <- lm(LPI ~ SBI, data = conj3_df)
summary(model)


model_control <- lm(LPI ~ SBI + IEI + founding_era_category, data = conj3_df)
summary(model_control)


library(ggrepel)

high_LPI <- 4   
high_SBI <- 55  

# Filter for high-scoring IGOs
high_IGOs <- conj3_df[conj3_df$LPI >= high_LPI | conj3_df$SBI >= high_SBI, ]

# Plot
ggplot(conj3_df, aes(x = SBI, y = LPI)) +
  geom_point(aes(color = founding_era_category), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  geom_text_repel(
    data = high_IGOs,
    aes(label = institution),
    size = 3.5,
    max.overlaps = 15
  ) +
  labs(
    title = "H3.4 — Strategy Breadth and Legitimacy",
    x = "Strategy Breadth Index (SBI)",
    y = "Legitimacy Participation Index (LPI)"
  ) +
  theme_minimal()
