# 01_Multieffects_AllDatasets_v5.R

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
results_file <- "Model_Summaries_InteractionBigFive.txt"
graphical_file <- "Graphical_Diagnostics_InteractionBigFive.pdf"

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
  
  ##### Missing Data Check #####
  # Summarize missing values
  missing_summary <- long_data %>%
    summarise(across(everything(), ~ sum(is.na(.))))
  
  cat("\n### Missing Data Summary for", dataset, "###\n\n", file = results_file, append = TRUE)
  capture.output(missing_summary, file = results_file, append = TRUE)
  
  # Filter rows with missing data
  missing_rows <- long_data %>%
    filter(is.na(IntrinsicMotivation) | is.na(Role))
  
  if (nrow(missing_rows) > 0) {
    cat("\n### Rows with Missing Data for", dataset, "###\n\n", file = results_file, append = TRUE)
    capture.output(missing_rows, file = results_file, append = TRUE)
    cat("\nSkipping analysis for", dataset, "due to missing data.\n", file = results_file, append = TRUE)
    next
  }
  
  # Debugging: Check transformed data
  cat("Transformed dataset has", nrow(long_data), "rows.\n")
  
  ##### Step 1 - Mixed-Effects Model #####
  # Model with Role * Big Five interactions
  model <- lme(
    fixed = IntrinsicMotivation ~ Role * (B5_O + B5_C + B5_E + B5_A + B5_N),
    random = ~ 1 | Student_ID,
    data = long_data,
    method = "REML"
  )
  
  # Save model summary
  dataset_results_file <- paste0("Model_Summaries_InteractionBigFive_", gsub(".xlsx", "", dataset), ".txt")
  capture.output(summary(model), file = dataset_results_file)
  
  ##### Step 2 - Post-Hoc Tests #####
  # Examine interaction effects
  interaction_effects <- emmeans(model, ~ Role | (B5_O + B5_C + B5_E + B5_A + B5_N))
  cat("\n### Big Five x Role Interaction Effects for", dataset, "###\n\n", file = results_file, append = TRUE)
  capture.output(summary(interaction_effects), file = results_file, append = TRUE)
  
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
}

# Close graphical output
dev.off()

cat("Analysis complete. Results saved in:", results_file, "and", graphical_file, "\n")
