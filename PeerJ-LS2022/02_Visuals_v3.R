## Sample's Big Five Inventory scores
# Calculate means of Big Five traits
mean_openness <- mean(outhc$B5_O, na.rm = TRUE)
mean_conscientiousness <- mean(outhc$B5_C, na.rm = TRUE)
mean_extraversion <- mean(outhc$B5_E, na.rm = TRUE)
mean_agreeableness <- mean(outhc$B5_A, na.rm = TRUE)
mean_neuroticism <- mean(outhc$B5_N, na.rm = TRUE)

# Calculate standard deviations of Big Five traits
sd_openness <- sd(outhc$B5_O, na.rm = TRUE)
sd_conscientiousness <- sd(outhc$B5_C, na.rm = TRUE)
sd_extraversion <- sd(outhc$B5_E, na.rm = TRUE)
sd_agreeableness <- sd(outhc$B5_A, na.rm = TRUE)
sd_neuroticism <- sd(outhc$B5_N, na.rm = TRUE)

# Print results
mean_openness
sd_openness
mean_conscientiousness
sd_conscientiousness
mean_extraversion
sd_extraversion
mean_agreeableness
sd_agreeableness
mean_neuroticism
sd_neuroticism

# Calculate means and standard deviations of motivation per role
motivation_stats <- motivation_by_role %>%
  group_by(Role) %>%
  summarize(
    Mean = mean(mean_motivation, na.rm = TRUE),
    SD = sd(mean_motivation, na.rm = TRUE)
  ) %>%
  as.data.frame()

# Print the result
print(motivation_stats)

# ### Visualizations ###
# Create a summary table for Big Five personality dimensions
cluster_summary <- outhc %>% 
  group_by(OC) %>% 
  summarize(
    Mean_O = mean(B5_O, na.rm = TRUE), SD_O = sd(B5_O, na.rm = TRUE),
    Mean_C = mean(B5_C, na.rm = TRUE), SD_C = sd(B5_C, na.rm = TRUE),
    Mean_E = mean(B5_E, na.rm = TRUE), SD_E = sd(B5_E, na.rm = TRUE),
    Mean_A = mean(B5_A, na.rm = TRUE), SD_A = sd(B5_A, na.rm = TRUE),
    Mean_N = mean(B5_N, na.rm = TRUE), SD_N = sd(B5_N, na.rm = TRUE)
  )

# Reshape the data to have Means and SDs stacked into two sub-rows
cluster_summary_long <- outhc %>%
  group_by(OC) %>%
  summarize(
    Mean_O = mean(B5_O, na.rm = TRUE), SD_O = sd(B5_O, na.rm = TRUE),
    Mean_C = mean(B5_C, na.rm = TRUE), SD_C = sd(B5_C, na.rm = TRUE),
    Mean_E = mean(B5_E, na.rm = TRUE), SD_E = sd(B5_E, na.rm = TRUE),
    Mean_A = mean(B5_A, na.rm = TRUE), SD_A = sd(B5_A, na.rm = TRUE),
    Mean_N = mean(B5_N, na.rm = TRUE), SD_N = sd(B5_N, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("Mean") | starts_with("SD"), names_to = c(".value", "Trait"), names_sep = "_")

# PeerJ 2024 BFI Data Visualization
# Data from OC for the five clusters
oc_data <- data.frame(
  Cluster = c("Cluster 1", "Cluster 1", "Cluster 2", "Cluster 2", "Cluster 3", "Cluster 3", "Cluster 4", "Cluster 4", "Cluster 5", "Cluster 5"),
  Statistic = c("η", "σ", "η", "σ", "η", "σ", "η", "σ", "η", "σ"),
  Openness = c(8.38, 1.14, 3.81, 0.85, 6.63, 0.00, 9.44, 0.65, 6.06, 1.95),
  Conscientiousness = c(4.63, 1.35, 4.94, 1.20, 5.50, 3.18, 7.75, 2.05, 5.78, 2.13),
  Extraversion = c(4.63, 1.57, 5.22, 1.44, 7.19, 0.80, 6.34, 1.08, 2.41, 0.56),
  Agreeableness = c(5.13, 1.87, 5.78, 1.56, 8.31, 0.80, 7.19, 1.13, 6.34, 1.69),
  Neuroticism = c(4.75, 1.69, 3.95, 1.03, 9.44, 0.80, 1.56, 0.65, 7.75, 1.59)
)

# Create a kable table for the five clusters
oc_data %>%
  kbl(align = 'c', caption = "Table: Big Five Personality Dimensions by Cluster (OC Data)") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" " = 2, "Openness" = 1, "Conscientiousness" = 1, "Extraversion" = 1, "Agreeableness" = 1, "Neuroticism" = 1)) %>%
  row_spec(seq(2, 10, by = 2), italic = TRUE)



##### WS 2021 Visualizations ######
# WS2021 BFI Data Visualization
cluster_data <- data.frame(
  Cluster = c("WS21-Cluster 1", "", "WS21-Cluster 2", "", "WS21-Cluster 3", ""),
  Statistic = c("η", "σ", "η", "σ", "η", "σ"),
  Openness = c(8.29, 1.21, 6.09, 1.76, 6.73, 1.68),
  Conscientiousness = c(6.53, 1.66, 6.00, 1.48, 5.82, 1.72),
  Extraversion = c(6.35, 1.58, 7.36, 1.36, 3.55, 1.04),
  Agreeableness = c(6.12, 1.54, 7.91, 0.83, 5.91, 1.14),
  Neuroticism = c(5.94, 1.43, 5.00, 1.48, 7.82, 1.40)
)

# Create a kable table
cluster_data %>%
  kbl(align = 'c', caption = "Table: Big Five Personality Dimensions by Cluster") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" " = 2, "Openness" = 1, "Conscientiousness" = 1, "Extraversion" = 1, "Agreeableness" = 1, "Neuroticism" = 1)) %>%
  row_spec(seq(2, 6, by = 2), italic = TRUE)



# ###### CONTINGENCY TABLE #####
outhc %$% table(HD,max_mean)
# Data for the contingency table
contingency_data <- data.frame(
  Cluster = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
  Preferred_Role = c("Pilot", "Navigator, Pilot", "Navigator, Solo", "Pilot", "Navigator, Pilot, Solo"),
  Frequency = c("55% (5/9)", "50% each (4/8)", "50% each (1/2)", "75% (3/4)", "50%, 25%, 25% (of 4)")
)

# Create the table with formatting
contingency_data %>%
  kbl(align = 'c', caption = "Table 2: Cluster Role Preferences and Frequency") %>%
  kable_styling(full_width = F, bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE) %>% # Bold header row
  row_spec(seq(1, nrow(contingency_data), by = 2), background = "#f2f2f2") # Add shading to alternating rows

