# PeerJ-LS2022

# Install required packages (only if not installed)
required_packages <- c("lavaan", "dplyr", "ggplot2", "openxlsx", "tidyr", "semPlot")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(required_packages, require, character.only = TRUE)

# Load the data
data <- read.xlsx("Experiment.xlsx", sheet = "Sheet1")

# Rename columns
colnames(data) = c('ID', 'Start_time', 'Completion_time', 'Email','Name',
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
                   'Ps_results_flag', 'Initials'
)

# Convert the 'Start_time' to date to identify different sessions
data$Start_time <- as.Date(data$Start_time, origin = "1899-12-30")

# Map Likert scale responses to numeric values
likert_scale <- function(x) {
  map <- c("Disagree strongly" = 1, 
           "Disagree a little" = 2, 
           "Neither agree nor disagree" = 3, 
           "Agree a little" = 4, 
           "Agree strongly" = 5)
  as.numeric(map[x])
}

# Apply Likert scale conversion to relevant columns
likert_columns <- c('01_01', '01_02', '01_03', '01_04', '01_05',
                    '01_06', '01_07', '01_08', '01_09', '01_10',
                    '03_01', '03_02', '03_03', '03_04', '03_05', 
                    '03_06', '03_07',
                    '04_01', '04_02', '04_03', '04_04', '04_05', 
                    '04_06', '04_07',
                    '05_01', '05_02', '05_03', '05_04', '05_05', 
                    '05_06', '05_07',
                    '06_01', '06_02', '06_03', '06_04', '06_05', 
                    '06_06', '06_07',
                    '07_01', '07_02', '07_03', '07_04', '07_05', 
                    '07_06', '07_07',
                    '08_01', '08_02', '08_03', '08_04', '08_05', 
                    '08_06', '08_07')

# Convert Likert scale columns
data[likert_columns] <- lapply(data[likert_columns], likert_scale)

# Step 1: Calculate the Big Five personality traits for each participant
data <- data %>%
  mutate(B5_O = (`01_05` + `01_10`),
         B5_C = (`01_03` + `01_08`),
         B5_E = (`01_01` + `01_06`),
         B5_A = (`01_02` + `01_07`),
         B5_N = (`01_04` + `01_09`))

# Step 2: Map Role responses to "Pilot", "Solo", "Navigator"
role_mapping <- function(x) {
  ifelse(x == "Disagree strongly", "Pilot",
         ifelse(x == "Neither agree nor disagree", "Solo", 
                ifelse(x == "Agree strongly", "Navigator", NA)))
}

# Apply role mapping to role columns
role_columns <- c('03_Role', '04_Role', '05_Role', '06_Role', '07_Role', '08_Role')

data[role_columns] <- lapply(data[role_columns], role_mapping)

# Check the structure of the data to verify the conversions
str(data)

# Step 3: Reshape the data for motivational inventories across all rounds
long_data <- data %>%
  # Gather intrinsic motivation scores from all rounds into a long format
  gather(key = "Round", value = "Motivation",
         c('03_01', '03_02', '03_03', '03_04', '03_05', '03_06', '03_07',
           '04_01', '04_02', '04_03', '04_04', '04_05', '04_06', '04_07',
           '05_01', '05_02', '05_03', '05_04', '05_05', '05_06', '05_07',
           '06_01', '06_02', '06_03', '06_04', '06_05', '06_06', '06_07',
           '07_01', '07_02', '07_03', '07_04', '07_05', '07_06', '07_07',
           '08_01', '08_02', '08_03', '08_04', '08_05', '08_06', '08_07'),
         factor_key = TRUE) %>%
  
  # Gather the role information across rounds
  gather(key = "Role_Round", value = "Role",
         c('03_Role', '04_Role', '05_Role', '06_Role', '07_Role', '08_Role'),
         factor_key = TRUE)

# Step 4: Filter out unnecessary rows and match each motivation score with its corresponding round and role
long_data <- long_data %>%
  mutate(Experiment_Session = case_when(
    Start_time == "2022-04-01" ~ "Session 1",
    Start_time == "2022-04-08" ~ "Session 2",
    Start_time == "2022-05-13" ~ "Session 3"
  )) %>%
  
  filter(!is.na(Motivation)) %>%
  select(Initials, Experiment_Session, Round, Role_Round, Role, Motivation, B5_O, B5_C, B5_E, B5_A, B5_N)

# Step 5: Run SEM analysis on the reshaped data
sem_model <- '
  # Direct effect
  Motivation ~ c*Role
  
  # Mediation paths (personality traits as mediators)
  B5_O ~ a1*Role
  B5_C ~ a2*Role
  B5_E ~ a3*Role
  B5_A ~ a4*Role
  B5_N ~ a5*Role
  
  Motivation ~ b1*B5_O + b2*B5_C + b3*B5_E + b4*B5_A + b5*B5_N
  
  # Indirect effects (mediation effects)
  indirect_O := a1*b1
  indirect_C := a2*b2
  indirect_E := a3*b3
  indirect_A := a4*b4
  indirect_N := a5*b5
  
  # Total effect
  total := c + indirect_O + indirect_C + indirect_E + indirect_A + indirect_N
'

# Fit the SEM model using lavaan
fit <- sem(sem_model, data = long_data)

# Display model summary
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Visualize the mediation effects
semPlot::semPaths(fit, "standardized", layout = "tree", residuals = FALSE)
