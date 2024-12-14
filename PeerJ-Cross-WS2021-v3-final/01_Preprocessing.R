### Note for PeerJ-CS article readers.
# The presented unaltered scripts reflect the original analysis from WS of 2021.
# In order to run the Pearson Chi-squared and Fisher Exact tests verifying the
# statistical significance of the relationship between clusters and preferred roles,
# run the scripts in the following order: 01, 02, 03, 04, 06, 07, 08.
# File 05 can be skipped.



#setwd("~/R/Experiment/")

# Dependencies
list.of.packages <- c("openxlsx","openssl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))




## read data
data = read.xlsx("Experiment.xlsx", sheet = "Sheet1")


## basic data info
# summary(data)
# str(data)
# dim(data)

## rename columns
colnames(data) = c('ID', 'Start_time', 'Completion_time', 'Email','Name',
                   'Consent_read', 'Experience_yrs', 'Gender',
                   '01_01', '01_02', '01_03', '01_04', '01_05',
                   '01_06', '01_07', '01_08', '01_09', '01_10',
                   '02_01', '02_02', '02_03', '02_04', '02_05',
                   '02_06', '02_07', '02_08',
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

# úprava - bolo zle označená rola
data$"08_Role"[data$"08_Role"=="Disagree a little"] = "Neither agree nor disagree"


# factorize values by changing it to numbers
for (i in 9:74){
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

# replace wrong initials values
data$Initials[7] = "vasv08"
data$Initials[38] = "krod04"
data$Initials[25] = data$Initials[64] = "hasp00"
data$Initials[27] = data$Initials[66] = "wald03"
data$Initials[30] = "prod13"

# remove prefixes from initials
for (i in 1:length(data$Initials)) {
  len = nchar(data$Initials[i])
  data$Initials[i] = substr(data$Initials[i],len-5,len)
}

# to lower case
data$Initials = tolower(data$Initials)


# hash Initials
data$Student_ID <- md5(data$Initials)

# new variable of round of experiment based on start time
data$Exc_round = ceiling((data$Start_time -  44505)/7)
data$Exc_round = ifelse(data$Exc_round == 6, 3, data$Exc_round)

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
                    
                    SS_Exp_seek = `02_01` + `02_05`,
                    SS_Boredom_susc = `02_02` + `02_06`,
                    SS_Thrill_adv = `02_03` + `02_07`,
                    SS_Disinhib = `02_04` + `02_08`,
                    
                    INNER_R1 =`03_01`+`03_02`+`03_03`+`03_04`+`03_05`+`03_06`+`03_07`,
                    INNER_R2 =`04_01`+`04_02`+`04_03`+`04_04`+`04_05`+`04_06`+`04_07`,
                    INNER_R3 =`05_01`+`05_02`+`05_03`+`05_04`+`05_05`+`05_06`+`05_07`,
                    INNER_R4 =`06_01`+`06_02`+`06_03`+`06_04`+`06_05`+`06_06`+`06_07`,
                    INNER_R5 =`07_01`+`07_02`+`07_03`+`07_04`+`07_05`+`07_06`+`07_07`,
                    INNER_R6 =`08_01`+`08_02`+`08_03`+`08_04`+`08_05`+`08_06`+`08_07`,
                    
                    Role_01 = `03_Role`, Role_02 = `04_Role`, Role_03 = `05_Role`,
                    Role_04 = `06_Role`, Role_05 = `07_Role`, Role_06 = `08_Role`)
detach(data)

Stats[Stats$Initials == "korn02" & Stats$Exc_round == 3, "Role_05"] <- "Pilot"

rm(list = c("data","i","len"))

write.xlsx(subset(Stats,select = -c(Start_time, Completion_time, Gender, 
                                     Experience_yrs, Ps_results_flag)), 
           file = "Stats.xlsx", colNames = TRUE, overwrite = TRUE)


