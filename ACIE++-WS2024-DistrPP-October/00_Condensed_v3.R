# ACIE 2025 - Empirical Blockchain Software Engineering Applied in Distributed Personality-Based Pair Programming

# The following is the condensed R script that combines our separate files for
# quantitatively analyzing the Personality-Based Pair Programming experiments in
# distributed application.

# Dependencies
required_packages <- c("openxlsx", "openssl", "magrittr", "dplyr", "ggplot2", "tidyr", "kableExtra")

# Function to install and load packages if needed
install_and_load <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if(length(new_packages)) install.packages(new_packages)
  invisible(lapply(packages, require, character.only = TRUE))
}

# Install and load the required packages
install_and_load(required_packages)

##### Step 1 - Pre-processing #####
# Read data
data <- read.xlsx("Experiment-WS2024+SS2022+WS2021.xlsx", sheet = "Sheet1")

## basic data info
# summary(data)
# str(data)
# dim(data)

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

# Factorize values for Likert scale and handle Role columns separately
for (i in 9:66){
  data[,i] = factor(data[,i], levels = c("Disagree strongly","Disagree a little",
                                         "Neither agree nor disagree",
                                         "Agree a little","Agree strongly"))
  if (length(grep("Role",colnames(data)[i])) == 0) {
    data[,i] = as.numeric(data[,i])
  }else{
    # Role should stay string
    levels(data[,i]) = c('Pilot','Y','Solo','X','Navigator')
  }
}

# Remove prefixes from initials
for (i in 1:length(data$Initials)) {
  len = nchar(data$Initials[i])
  data$Initials[i] = substr(data$Initials[i],len-5,len)
}

# To lower case
data$Initials = tolower(data$Initials)

# Replace first two letters of initials with 'x' - semi-anonymization
# data$Initials <- sub("^.{1}", "x", data$Initials)

# Save the semi-anonymized dataset to a new Excel file
#write.xlsx(data, file = "Anonymized_Experiment.xlsx", colNames = TRUE, overwrite = TRUE)

# Hash Initials - full anonymization
data$Student_ID <- md5(data$Initials)

# New variable of round of experiment based on start time
data$Exc_round = ceiling((data$Start_time -  44652)/7)
data$Exc_round = ifelse(data$Exc_round == 7, 3, data$Exc_round)

attach(data)
Stats = data.frame(Start_time = Start_time,
                   Completion_time = Completion_time,
                   Experience_yrs = Experience_yrs,
                   Gender = Gender,
                   Ps_results_flag = Ps_results_flag, 
                   Student_ID = as.character(Student_ID),
                   Initials = Initials,
                   Exc_round = Exc_round,
                   
                   B5_O = (6 - `01_05`) + `01_10`,
                   B5_C = (6 - `01_03`) + `01_08`,
                   B5_E = (6 - `01_01`) + `01_06`,
                   B5_A = `01_02` + (6 - `01_07`),
                   B5_N = (6 - `01_04`) + `01_09`,
                   
                   INNER_R1 =`03_01`+`03_02`+(6-`03_03`)+(6-`03_04`)+`03_05`+`03_06`+`03_07`,
                   INNER_R2 =`04_01`+`04_02`+(6-`04_03`)+(6-`04_04`)+`04_05`+`04_06`+`04_07`,
                   INNER_R3 =`05_01`+`05_02`+(6-`05_03`)+(6-`05_04`)+`05_05`+`05_06`+`05_07`,
                   INNER_R4 =`06_01`+`06_02`+(6-`06_03`)+(6-`06_04`)+`06_05`+`06_06`+`06_07`,
                   INNER_R5 =`07_01`+`07_02`+(6-`07_03`)+(6-`07_04`)+`07_05`+`07_06`+`07_07`,
                   INNER_R6 =`08_01`+`08_02`+(6-`08_03`)+(6-`08_04`)+`08_05`+`08_06`+`08_07`,
                   
                   Role_01 = `03_Role`, Role_02 = `04_Role`, Role_03 = `05_Role`,
                   Role_04 = `06_Role`, Role_05 = `07_Role`, Role_06 = `08_Role`)

# Calculate total pilot and navigator motivation based on Role_01
Stats$Pilot_Motivation <- ifelse(Stats$Role_01 == "Pilot",
                                 Stats$INNER_R1 + Stats$INNER_R3 + Stats$INNER_R5,  # Pilot starts as R1, R3, R5
                                 Stats$INNER_R2 + Stats$INNER_R4 + Stats$INNER_R6)  # Navigator starts as R1, R3, R5

Stats$Navigator_Motivation <- ifelse(Stats$Role_01 == "Pilot",
                                     Stats$INNER_R2 + Stats$INNER_R4 + Stats$INNER_R6,  # Navigator starts as R2, R4, R6
                                     Stats$INNER_R1 + Stats$INNER_R3 + Stats$INNER_R5)  # Pilot starts as R2, R4, R6

# Determine preferred role based on the highest motivation score
Stats$Preferred_Role <- ifelse(Stats$Pilot_Motivation > Stats$Navigator_Motivation, "Pilot", "Navigator")

detach(data)

rm(list = c("data","i","len"))

# Write the updated Stats to Stats.xlsx with the Preferred Role column
write.xlsx(subset(Stats,select = -c(Start_time, Completion_time, Gender, 
                                    Experience_yrs, Ps_results_flag)), 
           file = "Stats.xlsx", colNames = TRUE, overwrite = TRUE)


##### File 2

# read xlsx data if it does not exist yet
#if (!exists("Stats")) 
Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

# We do not want non-integer values
last_PS <- Stats %>% group_by(Initials) %>% summarize(MER = max(Exc_round))

# for both clustering options (w/wo SS) Ward's method gives more clear cluster split

# clustering based on B5
clust_data <- Stats %>% 
  inner_join(last_PS, by = c("Initials","Exc_round"="MER")) %>% 
  select(Initials, B5_O:B5_N)

#clust_data <- clust_data[-40,] # old update not...
rownames(clust_data) <- clust_data$Initials
clust_data %<>% select(-Initials)

# clustering
clusters = hclust(dist(clust_data), method = "ward.D2")
# chart
plot(clusters, xlab = "")
# cut
abline(h=8, col = "blue")

# split(cut) the tree into four groups
HC_BS_5<- cutree(clusters,5)


##### File 3 - Filtering, Ordering

# read xlsx data if it does not exist yet
#if (!exists("Stats"))
Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

# students with average(for exercises) motivation by role
motivation_by_role <- Stats %>% as_tibble %>%
  select(Initials, Exc_round, INNER_R1:Role_06) %>%
  gather(Round,Result,INNER_R1:INNER_R6, factor_key=TRUE) %>%
  mutate(Role = case_when(Round == "INNER_R1" ~ Role_01,
                          Round == "INNER_R2" ~ Role_02,
                          Round == "INNER_R3" ~ Role_03,
                          Round == "INNER_R4" ~ Role_04,
                          Round == "INNER_R5" ~ Role_05,
                          Round == "INNER_R6" ~ Role_06)) %>%
  select(Initials, Role, Exc_round, Round, Result) %>% 
  group_by(Initials, Role) %>% 
  summarize(mean_motivation = mean(Result), .groups = "keep") %>% 
  mutate(Role = factor(Role))

# means by each group and ANOVA
motivation_by_role %>% group_by(Role) %>% 
  summarize(Mean=mean(mean_motivation)) %>% 
  as.data.frame
motivation_by_role %$% aov(mean_motivation ~ Role) %>% summary

motivation_by_role %$% kruskal.test(mean_motivation ~ Role)



ggplot(motivation_by_role, aes(x = mean_motivation)) + geom_histogram()

ks.test(motivation_by_role$mean_motivation, "pnorm")

shapiro.test(motivation_by_role$mean_motivation)

#
ordered_results <- Stats %>% 
  as_tibble %>%
  select(Initials, Exc_round, INNER_R1:Role_06) %>%
  gather(Round,Result,INNER_R1:INNER_R6, factor_key=TRUE) %>%
  mutate(Role = case_when(Round == "INNER_R1" ~ Role_01,
                          Round == "INNER_R2" ~ Role_02,
                          Round == "INNER_R3" ~ Role_03,
                          Round == "INNER_R4" ~ Role_04,
                          Round == "INNER_R5" ~ Role_05,
                          Round == "INNER_R6" ~ Role_06))%>%
  select(Initials, Role, Exc_round, Round, Result) %>% 
  arrange(Initials,Role) 


#### H1 - Testing the individual differences in motivation by role
# Step 1: Calculate the average intrinsic motivation per role for each individual
# - already performed above
# Step 2: Calculate the maximum and minimum intrinsic motivation for each individual
max_min_diff <- motivation_by_role %>%
  group_by(Initials) %>%
  summarize(
    max_motivation = max(mean_motivation, na.rm = TRUE),
    min_motivation = min(mean_motivation, na.rm = TRUE),
    max_diff = max_motivation - min_motivation
  )

# Step 3: Paired t-test or Wilcoxon signed-rank test
# This tests whether the difference between max and min motivation is significant.
# We use paired t-test if data is normally distributed, otherwise Wilcoxon.

# Paired t-test
t_test_results <- t.test(max_min_diff$max_motivation, max_min_diff$min_motivation, paired = TRUE)

# Wilcoxon signed-rank test (non-parametric alternative)
wilcox_results <- wilcox.test(max_min_diff$max_motivation, max_min_diff$min_motivation, paired = TRUE)

# Step 4: Summary statistics
average_increase <- mean(max_min_diff$max_diff, na.rm = TRUE)
sd_increase <- sd(max_min_diff$max_diff, na.rm = TRUE)

# Display results
list(
  max_min_diff = max_min_diff,  # Differences for each individual
  t_test_results = t_test_results,  # Paired t-test results
  wilcox_results = wilcox_results,  # Wilcoxon signed-rank test results
  average_increase = average_increase,  # Average increase in motivation
  sd_increase = sd_increase  # Standard deviation of increase
)

### H1-Corr (Motivation across roles), posited that the difference in individual average intrinsic motivation across roles is significant and consistent, indicating that participants exhibit predictable variations in motivation when assigned to different roles based on their personality traits
friedman_test <- friedman.test(as.matrix(motivation_by_role))
print(friedman_test)


# Step to rescale the mean motivation values
# Corrected step to rescale the mean motivation values
motivation_by_role_rescaled <- motivation_by_role %>%
  mutate(rescaled_motivation = ((mean_motivation - 7) / (35 - 7)) * (10 - 1) + 1)

# Display rescaled averages for each role
rescaled_averages <- motivation_by_role_rescaled %>%
  group_by(Role) %>%
  summarize(
    Mean_Rescaled = mean(rescaled_motivation, na.rm = TRUE),
    SD_Rescaled = sd(rescaled_motivation, na.rm = TRUE)
  )

print(rescaled_averages)

# Increases in intrinsic motivation
# Step 1: Calculate max and min motivation per individual (this should have been done already)
max_min_diff_rescaled <- motivation_by_role_rescaled %>%
  group_by(Initials) %>%
  summarize(
    max_motivation_rescaled = max(rescaled_motivation, na.rm = TRUE),
    min_motivation_rescaled = min(rescaled_motivation, na.rm = TRUE)
  )

# Step 2: Perform the paired t-test on rescaled values
t_test_rescaled <- t.test(max_min_diff_rescaled$max_motivation_rescaled, 
                          max_min_diff_rescaled$min_motivation_rescaled, 
                          paired = TRUE)

# Step 3: Perform the Wilcoxon signed-rank test on rescaled values
wilcox_rescaled <- wilcox.test(max_min_diff_rescaled$max_motivation_rescaled, 
                               max_min_diff_rescaled$min_motivation_rescaled, 
                               paired = TRUE)

# Output the test results
t_test_rescaled
wilcox_rescaled




# Filter data for only Pilot role
pilot_data <- motivation_by_role_rescaled %>% 
  filter(Role == "Pilot") %>%
  inner_join(Stats %>% select(Initials, B5_O), by = "Initials")

# Perform correlation between openness (B5_O) and motivation in Pilot role
cor_test_pilot <- cor.test(pilot_data$B5_O, pilot_data$rescaled_motivation)

# Print correlation results
print(cor_test_pilot)


### Rescale Big Five from 2-10 to 1-10
# Define the rescaling function
rescale_to_1_10 <- function(x) {
  return(1 + ((x - 2) / (10 - 2)) * (10 - 1))
}

# Apply the rescaling to the Big Five columns from the dataset
Stats$B5_O <- rescale_to_1_10(Stats$B5_O)
Stats$B5_C <- rescale_to_1_10(Stats$B5_C)
Stats$B5_E <- rescale_to_1_10(Stats$B5_E)
Stats$B5_A <- rescale_to_1_10(Stats$B5_A)
Stats$B5_N <- rescale_to_1_10(Stats$B5_N)

# Calculate the mean and standard deviation for the rescaled Big Five dimensions
big_five_stats <- Stats %>%
  summarize(
    B5_O_Mean = mean(B5_O, na.rm = TRUE),
    B5_O_SD = sd(B5_O, na.rm = TRUE),
    B5_C_Mean = mean(B5_C, na.rm = TRUE),
    B5_C_SD = sd(B5_C, na.rm = TRUE),
    B5_E_Mean = mean(B5_E, na.rm = TRUE),
    B5_E_SD = sd(B5_E, na.rm = TRUE),
    B5_A_Mean = mean(B5_A, na.rm = TRUE),
    B5_A_SD = sd(B5_A, na.rm = TRUE),
    B5_N_Mean = mean(B5_N, na.rm = TRUE),
    B5_N_SD = sd(B5_N, na.rm = TRUE)
  )

# Display the results for the rescaled Big Five dimensions
print(big_five_stats)

