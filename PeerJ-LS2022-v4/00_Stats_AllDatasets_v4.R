# 00_Stats_Computation_v4.R
# PeerJ-CS, "Personality-Based Pair Programming"

# Install required packages (only if not installed)
required_packages <- c("openxlsx", "dplyr", "digest")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

# Load libraries
lapply(required_packages, require, character.only = TRUE)

# Define dataset filenames
datasets <- c(
  "Experiment_SS2022.xlsx",
  "Experiment_WS2021+SS2022.xlsx",
  "Experiment_WS2021+SS2022+WS2022.xlsx"
)

# Process each dataset
for (file in datasets) {
  
  ##### Step 1: Preprocessing #####
  # Read data
  data <- read.xlsx(file, sheet = "Sheet1")
  
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
  
  # Convert numeric columns
  data[9:66] <- lapply(data[9:66], as.numeric)
  
  # Clean initials and generate Student_ID
  data$Initials <- tolower(substring(data$Initials, nchar(data$Initials)-5))
  # Generate unique Student_ID by hashing initials
  data$Student_ID <- sapply(data$Initials, function(x) digest::digest(x, algo = "md5"))
  
  # Create a variable for the experiment round
  data$Start_time <- as.Date(data$Start_time, origin = "1899-12-30")
  data$Exc_round <- ceiling((as.numeric(data$Start_time) -  44652) / 7)
  data$Exc_round <- ifelse(data$Exc_round == 7, 3, data$Exc_round)
  
  ##### Step 2: Create Stats_Session_Data.xlsx #####
  Stats_Session_Data <- data.frame(
    Start_time = data$Start_time,
    Completion_time = data$Completion_time,
    Experience_yrs = data$Experience_yrs,
    Gender = data$Gender,
    Student_ID = data$Student_ID,
    Initials = data$Initials,
    Exc_round = data$Exc_round,
    
    B5_O = ((6 - data$`01_05`) + data$`01_10`) / 2,
    B5_C = ((6 - data$`01_03`) + data$`01_08`) / 2,
    B5_E = ((6 - data$`01_01`) + data$`01_06`) / 2,
    B5_A = (data$`01_02` + (6 - data$`01_07`)) / 2,
    B5_N = ((6 - data$`01_04`) + data$`01_09`) / 2,
    
    INNER_R1 = rowSums(data[, c('03_01', '03_02', '03_03', '03_04', '03_05', '03_06', '03_07')]),
    INNER_R2 = rowSums(data[, c('04_01', '04_02', '04_03', '04_04', '04_05', '04_06', '04_07')]),
    INNER_R3 = rowSums(data[, c('05_01', '05_02', '05_03', '05_04', '05_05', '05_06', '05_07')]),
    INNER_R4 = rowSums(data[, c('06_01', '06_02', '06_03', '06_04', '06_05', '06_06', '06_07')]),
    INNER_R5 = rowSums(data[, c('07_01', '07_02', '07_03', '07_04', '07_05', '07_06', '07_07')]),
    INNER_R6 = rowSums(data[, c('08_01', '08_02', '08_03', '08_04', '08_05', '08_06', '08_07')]),
    
    Role_01 = data$`03_Role`, Role_02 = data$`04_Role`, Role_03 = data$`05_Role`,
    Role_04 = data$`06_Role`, Role_05 = data$`07_Role`, Role_06 = data$`08_Role`
  )
  output_prefix <- gsub("Experiment_", "Stats_", sub("\\.xlsx", "", file))
  write.xlsx(Stats_Session_Data, file = paste0(output_prefix, "_Session_Data.xlsx"), colNames = TRUE, overwrite = TRUE)
  
  ##### Step 3: Create Stats_B5Avg.xlsx #####
  Stats_B5Avg <- Stats_Session_Data %>%
    group_by(Student_ID, Initials) %>%
    summarize(
      B5_O = round(mean(B5_O, na.rm = TRUE), 4),
      B5_C = round(mean(B5_C, na.rm = TRUE), 4),
      B5_E = round(mean(B5_E, na.rm = TRUE), 4),
      B5_A = round(mean(B5_A, na.rm = TRUE), 4),
      B5_N = round(mean(B5_N, na.rm = TRUE), 4),
      .groups = "drop"
    )
  write.xlsx(Stats_B5Avg, file = paste0(output_prefix, "_B5Avg.xlsx"), colNames = TRUE, overwrite = TRUE)
  
  ##### Step 4: Create Stats_Ready.xlsx #####
  Stats_Ready <- Stats_Session_Data
  for (i in 1:nrow(Stats_B5Avg)) {
    current_initials <- Stats_B5Avg$Initials[i]
    avg_B5 <- Stats_B5Avg[i, c("B5_O", "B5_C", "B5_E", "B5_A", "B5_N")]
    Stats_Ready[Stats_Ready$Initials == current_initials, c("B5_O", "B5_C", "B5_E", "B5_A", "B5_N")] <- avg_B5
  }
  write.xlsx(Stats_Ready, file = paste0(output_prefix, "_Ready.xlsx"), colNames = TRUE, overwrite = TRUE)
}

