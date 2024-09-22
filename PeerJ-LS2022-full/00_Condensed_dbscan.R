# PeerJ - LS2022 + ZS2021

# Install required packages (only if not installed)
required_packages <- c("openxlsx", "openssl", "magrittr", "dplyr", "ggplot2", "tidyr", "dbscan")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(required_packages, require, character.only = TRUE)

##### File 1 - Data Preprocessing #####
# Read data
data <- read.xlsx("Experiment-PeerJ+CIMPS.xlsx", sheet = "Sheet1")

# Rename columns for readability
colnames(data) <- c('ID', 'Start_time', 'Completion_time', 'Email', 'Name',
                    'Consent_read', 'Experience_yrs', 'Gender',
                    '01_01', '01_02', '01_03', '01_04', '01_05',
                    '01_06', '01_07', '01_08', '01_09', '01_10',
                    '03_01', '03_02', '03_03', '03_04', '03_05', 
                    '03_06', '03_07', '03_Role',
                    '04_01', '04_02', '04_03', '04_04', '04_05',
                    '04_06', '04_07', '04_Role',
                    '05_01', '05_02', '05_03', '05_04', '05_05',
                    '05_06', '05_07', '05_Role',
                    '06_01', '06_02', '06_03', '06_04', '06_05',
                    '06_06', '06_07', '06_Role',
                    '07_01', '07_02', '07_03', '07_04', '07_05',
                    '07_06', '07_07', '07_Role',
                    '08_01', '08_02', '08_03', '08_04', '08_05',
                    '08_06', '08_07', '08_Role',
                    'Ps_results_flag', 'Initials')

# Factorize values (Likert scale to numbers)
likert_levels <- c("Disagree strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree strongly")
data[9:66] <- lapply(colnames(data)[9:66], function(col_name) {
  if (grepl("Role", col_name)) {
    return(data[[col_name]])  # Skip factorization for Role columns
  } else {
    return(factor(data[[col_name]], levels = likert_levels, labels = 1:5))
  }
})

# ROLES
# Update the role mapping function to handle variations in role responses
role_mapping <- function(x) {
  # Standardize case and remove leading/trailing whitespaces
  x <- trimws(tolower(x))
  
  ifelse(x == "disagree strongly", "Pilot",
         ifelse(x == "neither agree nor disagree", "Solo", 
                ifelse(x == "agree strongly", "Navigator", NA)))
}

# Apply the role mapping to role columns
role_columns <- c('03_Role', '04_Role', '05_Role', '06_Role', '07_Role', '08_Role')
data[role_columns] <- lapply(data[role_columns], function(x) {
  factor(role_mapping(x), levels = c("Pilot", "Solo", "Navigator"))
})

# Convert to numeric
data[9:66] <- lapply(data[9:66], as.numeric)

# Clean initials (removing prefixes)
data$Initials <- tolower(trimws(data$Initials))

# Create a new cleaned dataset with Big Five personality traits and intrinsic motivation per round
Stats <- data.frame(
  Start_time = data$Start_time,
  Completion_time = data$Completion_time,
  Experience_yrs = data$Experience_yrs,
  Gender = data$Gender,
  Initials = data$Initials,
  B5_O = ((6 - data$`01_05`) + data$`01_10`) / 2,
  B5_C = ((6 - data$`01_03`) + data$`01_08`) / 2,
  B5_E = ((6 - data$`01_01`) + data$`01_06`) / 2,
  B5_A = (data$`01_02` + (6 - data$`01_07`)) / 2,
  B5_N = ((6 - data$`01_04`) + data$`01_09`) / 2,
  
  # Add the intrinsic motivation for each round
  INNER_R1 = rowSums(data[ ,c('03_01', '03_02', '03_03', '03_04', '03_05', '03_06', '03_07')], na.rm = TRUE),
  INNER_R2 = rowSums(data[ ,c('04_01', '04_02', '04_03', '04_04', '04_05', '04_06', '04_07')], na.rm = TRUE),
  INNER_R3 = rowSums(data[ ,c('05_01', '05_02', '05_03', '05_04', '05_05', '05_06', '05_07')], na.rm = TRUE),
  INNER_R4 = rowSums(data[ ,c('06_01', '06_02', '06_03', '06_04', '06_05', '06_06', '06_07')], na.rm = TRUE),
  INNER_R5 = rowSums(data[ ,c('07_01', '07_02', '07_03', '07_04', '07_05', '07_06', '07_07')], na.rm = TRUE),
  INNER_R6 = rowSums(data[ ,c('08_01', '08_02', '08_03', '08_04', '08_05', '08_06', '08_07')], na.rm = TRUE),
  
  Role_01 = data$`03_Role`,
  Role_02 = data$`04_Role`,
  Role_03 = data$`05_Role`,
  Role_04 = data$`06_Role`,
  Role_05 = data$`07_Role`,
  Role_06 = data$`08_Role`
)

##### File 2 - DBSCAN Clustering #####
# Perform DBSCAN clustering
clust_data <- Stats %>%
  group_by(Initials) %>%
  summarize(
    B5_O = mean(B5_O, na.rm = TRUE),
    B5_C = mean(B5_C, na.rm = TRUE),
    B5_E = mean(B5_E, na.rm = TRUE),
    B5_A = mean(B5_A, na.rm = TRUE),
    B5_N = mean(B5_N, na.rm = TRUE)
  ) %>%
  distinct()

# DBSCAN algorithm - adjust the eps (epsilon) and minPts parameters as needed
dbscan_result <- dbscan::dbscan(select(clust_data, -Initials), eps = 0.8, minPts = 5)

# Add DBSCAN cluster results to clust_data
clust_data$Cluster <- dbscan_result$cluster

# Save DBSCAN results to file
write.xlsx(clust_data, file = "DBSCAN_Results.xlsx", colNames = TRUE, overwrite = TRUE)

##### File 3 - Reporting on DBSCAN Clusters #####
# Add Cluster column to Stats by joining with clust_data (containing DBSCAN cluster assignments)
Stats_clustered <- Stats %>%
  left_join(clust_data %>% select(Initials, Cluster), by = "Initials")

# Group by the Cluster and calculate statistics for each Big Five trait
cluster_stats <- Stats_clustered %>%
  group_by(Cluster) %>%
  summarize(
    mean_B5_O = mean(B5_O, na.rm = TRUE),
    sd_B5_O = sd(B5_O, na.rm = TRUE),
    mean_B5_C = mean(B5_C, na.rm = TRUE),
    sd_B5_C = sd(B5_C, na.rm = TRUE),
    mean_B5_E = mean(B5_E, na.rm = TRUE),
    sd_B5_E = sd(B5_E, na.rm = TRUE),
    mean_B5_A = mean(B5_A, na.rm = TRUE),
    sd_B5_A = sd(B5_A, na.rm = TRUE),
    mean_B5_N = mean(B5_N, na.rm = TRUE),
    sd_B5_N = sd(B5_N, na.rm = TRUE)
  )

# Display the summarized statistics for each DBSCAN cluster
print(cluster_stats)

##### File 4 - Contingency Table and Chi-Squared Test #####
# Assign preferred role based on maximum motivation across rounds
# Step 1: Calculate the average motivation per role
preferred_role <- Stats %>%
  pivot_longer(cols = starts_with("INNER_R"), names_to = "Round", values_to = "Motivation") %>%
  mutate(Role = case_when(
    Round == "INNER_R1" ~ Role_01,
    Round == "INNER_R2" ~ Role_02,
    Round == "INNER_R3" ~ Role_03,
    Round == "INNER_R4" ~ Role_04,
    Round == "INNER_R5" ~ Role_05,
    Round == "INNER_R6" ~ Role_06
  )) %>%
  group_by(Initials, Role) %>%
  summarize(mean_motivation = mean(Motivation, na.rm = TRUE), .groups = "drop")

preferred_role <- preferred_role %>%
  group_by(Initials) %>%
  slice_max(order_by = mean_motivation, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Initials, Preferred_Role = Role)

# Merge preferred role with cluster data
Stats_clustered <- clust_data %>%
  left_join(preferred_role, by = "Initials")

# Create a contingency table
contingency_table <- table(Stats_clustered$Cluster, Stats_clustered$Preferred_Role)
role_labels <- c("Pilot", "Solo", "Navigator")
colnames(contingency_table) <- role_labels
print(contingency_table)

# Perform the chi-squared test
chi_squared_test <- chisq.test(contingency_table)
print(chi_squared_test)


# Perform Fisher's Exact Test instead of Chi-Squared
fisher_test <- fisher.test(contingency_table)
print(fisher_test)
