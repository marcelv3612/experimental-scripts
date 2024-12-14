# 01_MultieffectsMultimodel_AllDatasets_v4.R

# Install required packages (only if not installed)
required_packages <- c("openxlsx", "nlme", "dplyr", "tidyr", "ggplot2", "emmeans")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")
lapply(required_packages, require, character.only = TRUE)

# File paths
datasets <- list(
  "Stats_SS2022_Ready.xlsx",
  "Stats_WS2021+SS2022_Ready.xlsx",
  "Stats_WS2021+SS2022+WS2022_Ready.xlsx"
)

# Prepare output files
results_file <- "Model_Summaries.txt"
graphical_file <- "Graphical_Diagnostics.pdf"

# Define role labels
role_labels <- c("Pilot", "Solo", "Navigator")

# Clear the results
cat("", file = results_file, append = FALSE)

# Open graphical output
pdf(graphical_file)

# Loop through datasets
for (dataset in datasets) {
  # Print dataset info for debugging
  cat("\n### Analyzing Dataset:", dataset, "###\n\n")
    
  # Read dataset
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
  
  # Optional: Relevel Role to make Solo the baseline
  # long_data$Role <- relevel(long_data$Role, ref = "Solo")
  
  ##### Missing Data Check #####
  # Summarize missing values
  missing_summary <- long_data %>%
    summarise(across(everything(), ~ sum(is.na(.))))
  
  cat("\n### Missing Data Summary for", dataset, "###\n\n", file = results_file, append = TRUE)
  capture.output(missing_summary, file = results_file, append = TRUE)
  
  # Filter rows with missing data
  missing_rows <- long_data %>%
    filter(is.na(IntrinsicMotivation) | is.na(Role) | is.na(PersonalityCluster))
  
  if (nrow(missing_rows) > 0) {
    cat("\n### Rows with Missing Data for", dataset, "###\n\n", file = results_file, append = TRUE)
    capture.output(missing_rows, file = results_file, append = TRUE)
    cat("\nSkipping analysis for", dataset, "due to missing data.\n", file = results_file, append = TRUE)
    next
  }
  
  # Debugging: Check transformed data
  cat("Transformed dataset has", nrow(long_data), "rows.\n")
  
  ##### Step 1 - Fitting Interaction Mixed-Effects Models #####
  # Model 1: With PersonalityCluster only
  model_cluster <- lme(
    fixed = IntrinsicMotivation ~ Role * PersonalityCluster,
    random = ~ 1 | Student_ID,
    data = long_data,
    method = "REML"
  )
  cat("Number of observations in PersonalityCluster model:", model_cluster$dims$N, "\n")
  
  # Save output
  dataset_results_file_cluster <- paste0("Model_Summaries_Cluster_", gsub(".xlsx", "", dataset), ".txt")
  capture.output(summary(model_cluster), file = dataset_results_file_cluster)
  
  # Model 2: With Big Five traits only
  model_bigfive <- lme(
    fixed = IntrinsicMotivation ~ Role + B5_O + B5_C + B5_E + B5_A + B5_N,
    random = ~ 1 | Student_ID,
    data = long_data,
    method = "REML"
  )
  cat("Number of observations in Big Five model:", model_bigfive$dims$N, "\n")
  
  # Save output
  dataset_results_file_bigfive <- paste0("Model_Summaries_BigFive_", gsub(".xlsx", "", dataset), ".txt")
  capture.output(summary(model_bigfive), file = dataset_results_file_bigfive)
  
  # Model 3: With Role * Big Five interaction terms
  model <- lme(fixed = IntrinsicMotivation ~ Role * (B5_O + B5_C + B5_E + B5_A + B5_N),
               random = ~ 1 | Student_ID,
               data = long_data,
               method = "REML"
  )
  
  # Save the model summary
  dataset_results_file <- paste0("Model_Summaries_InteractionBigFive_", gsub(".xlsx", "", dataset), ".txt")
  capture.output(summary(model), file = dataset_results_file)
  
  ##### Step 2 - Post-Hoc Tests #####
  # Perform pairwise comparisons for roles
  role_comparisons <- emmeans(model, pairwise ~ Role)
  
  # Convert role contrasts to a human-readable format
  contrast_table <- summary(role_comparisons$contrasts)
  contrast_table$contrast <- gsub("Role1", "Pilot", contrast_table$contrast)
  contrast_table$contrast <- gsub("Role2", "Solo", contrast_table$contrast)
  contrast_table$contrast <- gsub("Role3", "Navigator", contrast_table$contrast)
  
  cat("\n### Pairwise Comparisons for", dataset, "###\n\n", file = results_file, append = TRUE)
  capture.output(contrast_table, file = results_file, append = TRUE)
  
  # Explore the interaction effects of Big Five and Role on Intrinsic Motivation
  # Post-hoc tests for interactions
  cat("\n### Big Five x Role Interaction Effects for", dataset, "###\n\n", file = results_file, append = TRUE)
  # bigfive_interactions <- emmeans(model, pairwise ~ Role | PersonalityCluster)
  bigfive_role_interactions <- emmeans(model, ~ Role | (B5_O + B5_C + B5_E + B5_A + B5_N))
  capture.output(summary(bigfive_interactions), file = results_file, append = TRUE)
  capture.output(summary(bigfive_role_interactions), file = results_file, append = TRUE)
  
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
  
  # Print the plot to the PDF device
  print(p)
  
  # Visualization for Big Five traits
  interaction_plot <- ggplot(long_data, aes(x = Role, y = IntrinsicMotivation, fill = Role)) +
    geom_boxplot() +
    theme_minimal() +
    facet_wrap(~ B5_O + B5_C + B5_E + B5_A + B5_N) +
    labs(
      title = paste("Intrinsic Motivation by Role and Big Five Traits -", dataset),
      x = "Programming Role",
      y = "Intrinsic Motivation"
    )
  print(interaction_plot)
}

# Close graphical output
dev.off()

cat("Analysis complete. Results saved in:", results_file, "and", graphical_file, "\n")

# Resolving Warnings:
# 1.	“NOTE: Results may be misleading due to involvement in interactions”:
#   This warning occurs when interpreting main effects in the presence of interaction terms.
#   It is expected behavior when analyzing interaction models.