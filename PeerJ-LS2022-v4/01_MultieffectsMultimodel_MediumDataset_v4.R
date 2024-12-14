# 01_MultieffectsMultimodel_MediumDataset_v4.R

# Updated R Script: Two Baselines (Pilot and Solo)
# Install required packages (only if not installed)
required_packages <- c("openxlsx", "nlme", "dplyr", "tidyr", "ggplot2", "emmeans")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")
lapply(required_packages, require, character.only = TRUE)

# Define the dataset path (medium dataset only)
dataset <- "Stats_WS2021+SS2022_Ready.xlsx"

# Prepare output files
results_file_interaction_pilot <- "Model_Summaries_InteractionBigFive_Pilot_Medium.txt"
results_file_interaction_solo <- "Model_Summaries_InteractionBigFive_Solo_Medium.txt"
results_file_bigfive_pilot <- "Model_Summaries_BigFiveFixedEffects_Pilot_Medium.txt"
results_file_bigfive_solo <- "Model_Summaries_BigFiveFixedEffects_Solo_Medium.txt"
results_file_cluster_pilot <- "Model_Summaries_PersonalityCluster_Pilot_Medium.txt"
results_file_cluster_solo <- "Model_Summaries_PersonalityCluster_Solo_Medium.txt"
graphical_file <- "Graphical_Diagnostics_Medium.pdf"

# Clear previous outputs
cat("", file = results_file_interaction_pilot, append = FALSE)
cat("", file = results_file_interaction_solo, append = FALSE)
cat("", file = results_file_bigfive_pilot, append = FALSE)
cat("", file = results_file_bigfive_solo, append = FALSE)
cat("", file = results_file_cluster_pilot, append = FALSE)
cat("", file = results_file_cluster_solo, append = FALSE)

# Open graphical output
pdf(graphical_file)

# Load and prepare the dataset
Stats <- openxlsx::read.xlsx(dataset)
cat("Dataset:", dataset, "has", nrow(Stats), "rows.\n")

##### Step 0 - Data Preparation #####
# Convert role columns to factors
role_columns <- c("Role_01", "Role_02", "Role_03", "Role_04", "Role_05", "Role_06")
Stats[role_columns] <- lapply(Stats[role_columns], as.factor)

# Create PersonalityCluster column
Stats <- Stats %>%
  rowwise() %>%
  mutate(
    PersonalityCluster = case_when(
      B5_O == max(c(B5_O, B5_C, B5_E, B5_A, B5_N)) ~ "Openness",
      B5_C == max(c(B5_O, B5_C, B5_E, B5_A, B5_N)) ~ "Conscientiousness",
      B5_E == max(c(B5_O, B5_C, B5_E, B5_A, B5_N)) ~ "Extraversion",
      B5_A == max(c(B5_O, B5_C, B5_E, B5_A, B5_N)) ~ "Agreeableness",
      B5_N == max(c(B5_O, B5_C, B5_E, B5_A, B5_N)) ~ "Neuroticism"
    )
  ) %>%
  ungroup()

Stats$PersonalityCluster <- factor(Stats$PersonalityCluster, 
                                   levels = c("Openness", "Conscientiousness", "Extraversion", "Agreeableness", "Neuroticism"))

# Pivot data to long format for mixed-effects modeling
long_data <- Stats %>%
  pivot_longer(
    cols = starts_with("INNER_R"), 
    names_to = "Round", 
    values_to = "IntrinsicMotivation"
  ) %>%
  mutate(
    Role = case_when(
      Round == "INNER_R1" ~ as.character(Role_01),
      Round == "INNER_R2" ~ as.character(Role_02),
      Round == "INNER_R3" ~ as.character(Role_03),
      Round == "INNER_R4" ~ as.character(Role_04),
      Round == "INNER_R5" ~ as.character(Role_05),
      Round == "INNER_R6" ~ as.character(Role_06)
    ),
    Role = factor(Role, levels = c("1", "2", "3"), labels = c("Pilot", "Solo", "Navigator"))
  )

##### Function to Relevel Baseline and Fit Models #####
fit_models <- function(baseline, results_file_interaction, results_file_bigfive, results_file_cluster) {
  # Relevel Role
  long_data$Role <- relevel(long_data$Role, ref = baseline)
  
  # Model 1: Big Five interaction effects
  model_interaction <- lme(
    fixed = IntrinsicMotivation ~ Role * (B5_O + B5_C + B5_E + B5_A + B5_N),
    random = ~ 1 | Student_ID,
    data = long_data,
    method = "REML"
  )
  capture.output(summary(model_interaction), file = results_file_interaction)
  
  # Model 2: Big Five fixed effects
  model_bigfive <- lme(
    fixed = IntrinsicMotivation ~ Role + B5_O + B5_C + B5_E + B5_A + B5_N,
    random = ~ 1 | Student_ID,
    data = long_data,
    method = "REML"
  )
  capture.output(summary(model_bigfive), file = results_file_bigfive)
  
  # Model 3: PersonalityCluster effects
  model_cluster <- lme(
    fixed = IntrinsicMotivation ~ Role * PersonalityCluster,
    random = ~ 1 | Student_ID,
    data = long_data,
    method = "REML"
  )
  capture.output(summary(model_cluster), file = results_file_cluster)
}

##### Run Models with Different Baselines #####
# Baseline: Pilot
fit_models("Pilot", results_file_interaction_pilot, results_file_bigfive_pilot, results_file_cluster_pilot)

# Baseline: Solo
fit_models("Solo", results_file_interaction_solo, results_file_bigfive_solo, results_file_cluster_solo)

##### Step 3 - Visualization #####
# Boxplot for intrinsic motivation by role
p <- ggplot(long_data, aes(x = Role, y = IntrinsicMotivation, fill = Role)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = paste("Intrinsic Motivation by Programming Role -", dataset),
    x = "Programming Role",
    y = "Intrinsic Motivation (1-10)"
  )
print(p)

# Close graphical output
dev.off()

cat("Analysis complete. Results saved in:\n",
    results_file_interaction_pilot, "\n",
    results_file_interaction_solo, "\n",
    results_file_bigfive_pilot, "\n",
    results_file_bigfive_solo, "\n",
    results_file_cluster_pilot, "\n",
    results_file_cluster_solo, "\n",
    graphical_file, "\n")

