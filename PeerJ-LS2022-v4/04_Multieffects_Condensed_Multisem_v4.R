## 04_Multieffects_Condensed_Multisem_v4.R
# PeerJ - Mixed-Effects Models for SS2022 + WS2021
##### Step 0 - Install and Load Required Packages #####
required_packages <- c("openxlsx", "nlme", "dplyr", "tidyr", "ggplot2", "emmeans")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

# Load libraries
lapply(required_packages, require, character.only = TRUE)

##### Step 1 - Data Preprocessing #####
# Load cleaned data
# Stats <- openxlsx::read.xlsx("Stats_Multisem_Cleaned.xlsx")
Stats <- openxlsx::read.xlsx("Stats.xlsx")

# Convert role columns to factors (note: "as.factor")
role_columns <- c("Role_01", "Role_02", "Role_03", "Role_04", "Role_05", "Role_06")
Stats[role_columns] <- lapply(Stats[role_columns], as.factor)

# Create PersonalityCluster column based on dominant Big Five trait
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

# Convert PersonalityCluster to a factor
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
      Round == "INNER_R1" ~ Role_01,
      Round == "INNER_R2" ~ Role_02,
      Round == "INNER_R3" ~ Role_03,
      Round == "INNER_R4" ~ Role_04,
      Round == "INNER_R5" ~ Role_05,
      Round == "INNER_R6" ~ Role_06
    ),
    Round = factor(Round)
  )

##### Step 2 - Mixed-Effects Model #####
# Fit the mixed-effects model using nlme
model <- lme(
  fixed = IntrinsicMotivation ~ Role * PersonalityCluster,
  random = ~ 1 | Student_ID,
  data = long_data,
  method = "REML"
)

# Display model summary
summary(model)

##### Step 4 - Model Diagnostics #####
# Check residuals for normality
qqnorm(residuals(model))
qqline(residuals(model))

# Check for homoscedasticity
plot(fitted(model), residuals(model))
abline(h = 0, col = "red")

##### Step 5 - Post-Hoc Tests #####
# Perform pairwise comparisons for roles
role_comparisons <- emmeans(model, pairwise ~ Role)
summary(role_comparisons)

##### Step 6 - Visualization #####
# Boxplot for intrinsic motivation by role
ggplot(long_data, aes(x = Role, y = IntrinsicMotivation)) +
  geom_boxplot(aes(fill = Role)) +
  theme_minimal() +
  labs(title = "Intrinsic Motivation by Programming Role",
       x = "Programming Role",
       y = "Intrinsic Motivation (1-10)")

##### Step 7 - Save Model Results #####
# Save model output to a text file
capture.output(summary(model), file = "Mixed_Effects_Model_Summary.txt")

# Save pairwise comparison results
capture.output(summary(role_comparisons), file = "Pairwise_Comparisons_Summary.txt")

