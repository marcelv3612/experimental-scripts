# PeerJ - LS2022

# The following is the condensed R script that combines our separate files for
# quantitatively analyzing the Personality-Based Pair Programming experiments in
# undergraduate classrooms.

# Dependencies
required_packages <- c("openxlsx", "openssl", "magrittr", "dplyr", "ggplot2", "tidyr")

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
data <- read.xlsx("Experiment-PeerJ-LS2022.xlsx", sheet = "Sheet1")

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

# remove prefixes from initials
for (i in 1:length(data$Initials)) {
  len = nchar(data$Initials[i])
  data$Initials[i] = substr(data$Initials[i],len-5,len)
}

# to lower case
data$Initials = tolower(data$Initials)

# Replace first two letters of initials with 'x'
data$Initials <- sub("^.{1}", "x", data$Initials)
# Save the updated dataset to a new Excel file
#write.xlsx(data, file = "Anonymized_Experiment.xlsx", colNames = TRUE, overwrite = TRUE)

# hash Initials
data$Student_ID <- md5(data$Initials)

# new variable of round of experiment based on start time
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
                   
                   INNER_R1 =`03_01`+`03_02`+`03_03`+`03_04`+`03_05`+`03_06`+`03_07`,
                   INNER_R2 =`04_01`+`04_02`+`04_03`+`04_04`+`04_05`+`04_06`+`04_07`,
                   INNER_R3 =`05_01`+`05_02`+`05_03`+`05_04`+`05_05`+`05_06`+`05_07`,
                   INNER_R4 =`06_01`+`06_02`+`06_03`+`06_04`+`06_05`+`06_06`+`06_07`,
                   INNER_R5 =`07_01`+`07_02`+`07_03`+`07_04`+`07_05`+`07_06`+`07_07`,
                   INNER_R6 =`08_01`+`08_02`+`08_03`+`08_04`+`08_05`+`08_06`+`08_07`,
                   
                   Role_01 = `03_Role`, Role_02 = `04_Role`, Role_03 = `05_Role`,
                   Role_04 = `06_Role`, Role_05 = `07_Role`, Role_06 = `08_Role`)
detach(data)

rm(list = c("data","i","len"))

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

# means by each group and ANOVA (not significant)
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


#### File 7 - Hierarchical Clustering

# It is required to first execute files 01, 02, 03.

dyn_solo <- ordered_results %>%
  filter(Role == "Solo") %>% 
  mutate(Result1 = Result,
         Result2 = lead(Result,1),
         Result3 = lead(Result,2),
         Result4 = lead(Result,3),
         Result5 = lead(Result,4),
         Result6 = lead(Result,5)) %>% 
  filter(Round == "INNER_R1") %>% 
  select(Initials,Role,Result1,Result2,Result3,Result4,Result5,Result6) %>% 
  mutate(c1_2 = sign(Result2 - Result1),
         c2_3 = sign(Result3 - Result2),
         c3_4 = sign(Result4 - Result3),
         c4_5 = sign(Result5 - Result4),
         c5_6 = sign(Result6 - Result5)) %>% 
  mutate(mean_dynamics = (c1_2+c2_3+c3_4+c4_5+c5_6)/5) %>% 
  select(-(Result1:Result6)) %>% 
  group_by(Motivation=factor(sign(mean_dynamics))) %>%  
  summarize(cnt = n()) %>% 
  mutate(Role = factor("Solo"))

dyn_team <- ordered_results %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  mutate(Result2 = lead(Result,1),Result3 = lead(Result,2)) %>% 
  filter(Round %in% c("INNER_R1","INNER_R2")) %>% 
  select(Initials,Role,Result1=Result,Result2,Result3) %>% 
  mutate(c1_2 = sign(Result2 - Result1),
         c2_3 = sign(Result3 - Result2)) %>% 
  select(-(Result1:Result3)) 

dyn_nav = dyn_team %>% 
  filter(Role == "Navigator") %>% 
  mutate(mean_dynamics = (c1_2+c2_3)/2) %>% 
  group_by(Motivation = factor(sign(mean_dynamics))) %>% 
  summarize(cnt = n()) %>% 
  mutate(Role = factor("Navigator"))

dyn_pil = dyn_team %>% 
  filter(Role == "Pilot") %>% 
  mutate(mean_dynamics = (c1_2+c2_3)/2) %>% 
  group_by(Motivation = factor(sign(mean_dynamics))) %>%
  summarize(cnt = n()) %>% 
  mutate(Role = factor("Pilot"))


dyn_solo
dyn_nav
dyn_pil

dyn = rbind(dyn_solo, dyn_nav, dyn_pil)

dyn

levels(dyn$Motivation) = c("Decreasing","Consistent","Increasing")

ggplot(dyn, aes(x = Role, y = cnt, fill = Motivation)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  ylab("")



ordered_results %<>% arrange(Initials, Exc_round, Round)

# Pil vs Nav results
vs_dyn <- ordered_results %>% 
  arrange(Initials, Exc_round, Round) %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  mutate(N_1 = ifelse(Role == "Navigator",Result,lead(Result,1)),
         N_2 = ifelse(Role == "Navigator",lead(Result,2),lead(Result,3)),
         N_3 = ifelse(Role == "Navigator",lead(Result,4),lead(Result,5)),
         P_1 = ifelse(Role == "Pilot",Result,lead(Result,1)),
         P_2 = ifelse(Role == "Pilot",lead(Result,2),lead(Result,3)),
         P_3 = ifelse(Role == "Pilot",lead(Result,4),lead(Result,5))) %>% 
  filter(Round %in% c("INNER_R1")) %>% 
  select(Initials,Role,N_1,N_2,N_3,P_1,P_2,P_3)  %>% 
  mutate(PtoN_1 = ifelse(Role == "Pilot",sign(N_1-P_1),sign(N_2-P_1)),
         PtoN_2 = ifelse(Role == "Pilot",sign(N_2-P_2),sign(N_3-P_2)),
         PtoN_3 = ifelse(Role == "Pilot",sign(N_3-P_3),NaN),
         NtoP_1 = ifelse(Role == "Pilot",sign(P_1-N_1),sign(P_2-N_1)),
         NtoP_2 = ifelse(Role == "Pilot",sign(P_2-N_2),sign(P_3-N_2)),
         NtoP_3 = ifelse(Role == "Pilot",sign(P_3-N_3),NaN)) %>% 
  mutate(PtoN = factor(sign(PtoN_1+PtoN_2+ifelse(is.na(PtoN_3),0,PtoN_3))),
         NtoP = factor(sign(NtoP_1+NtoP_2+ifelse(is.na(NtoP_3),0,NtoP_3)))) %>% 
  select(Initials, PtoN, NtoP)


vsd = rbind(data.frame(Initials = vs_dyn$Initials,
                       Stat = vs_dyn$PtoN,
                       RoleChange = "Pilot to Navigator"),
            data.frame(Initials = vs_dyn$Initials,
                       Stat = vs_dyn$NtoP,
                       RoleChange = "Navigator to Pilot"))

levels(vsd$Stat) = c("Decreasing","Consistent","Increasing")

ggplot(vsd, aes(x = RoleChange, fill = Stat)) +
  geom_bar(stat = "count", position=position_dodge()) +
  xlab("") + ylab("") + ggtitle("Přechod mezi rolemi v páru") 



# Same as 06_... line 34...
personality <- Stats %>% 
  group_by(Initials) %>% 
  summarize(B5_O = round(mean(B5_O)),
            B5_C = round(mean(B5_C)),
            B5_E = round(mean(B5_E)),
            B5_A = round(mean(B5_A)),
            B5_N = round(mean(B5_N)),
  ) %>% 
  select(Initials, B5_O:B5_N)

dt=personality[,c("Initials","B5_O","B5_C","B5_E","B5_A","B5_N")] %>% data.frame

rownames(dt) <- dt$Initials
# clustering
clusters = hclust(dist(dt %>% select(-Initials)))
# chart
plot(clusters, xlab = "")

HD <- cutree(clusters,5)

outhc = data.frame(HD) 

outhc$Initials = rownames(outhc)

# Same as 06_... lines 81+++
pref=personality[,c("Initials","B5_O","B5_C","B5_E","B5_A","B5_N")]

retRole = function(data,cols,max_min = "max"){
  if(max_min == "max"){
    val = apply(X=data[,cols], MARGIN=1, FUN=max, na.rm = T)
  } else {
    val = apply(X=data[,cols], MARGIN=1, FUN=min, na.rm = T)
  }
  role = ifelse(val==data[,cols[1]],"Nav",NA)
  role = ifelse(!is.na(role),role,ifelse(val==data[,cols[2]],"Pil","Solo"))
  role = ifelse(!is.na(role),role,"Solo")
  return(as.character(role))
}

# students with average(for exercises) motivation by role
mot_by_role <- Stats %>% as_tibble %>%
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
  summarize(mean_mot = mean(Result, na.rm = T),
            min_mot = min(Result, na.rm = T),
            max_mot = max(Result, na.rm = T),
            rng_mot = max(Result, na.rm = T) - min(Result, na.rm = T),
            .groups = "keep") %>% 
  mutate(Role = factor(Role))

# personal results, remove personality 
person_res <- mot_by_role %>% 
  pivot_wider(names_from = "Role",
              values_from = c("mean_mot","min_mot","max_mot","rng_mot")) %>% 
  inner_join(personality, by  = "Initials")

cols = c("mean_mot_Navigator","mean_mot_Pilot","mean_mot_Solo")
pref$max_mean = retRole(person_res,cols,"max")
pref$min_mean = retRole(person_res,cols,"min")

cols = c("max_mot_Navigator","max_mot_Pilot","max_mot_Solo")
pref$max = retRole(person_res,cols,"max")
cols = c("min_mot_Navigator","min_mot_Pilot","min_mot_Solo")
pref$min = retRole(person_res,cols,"min")
pref


outhc %<>% inner_join(pref)

outhc 

outhc %>% group_by(HD) %>% summarize(mean(B5_O), 
                                     sd(B5_O), 
                                     mean(B5_C), 
                                     sd(B5_C), 
                                     mean(B5_E), 
                                     sd(B5_E), 
                                     mean(B5_A), 
                                     sd(B5_A), 
                                     mean(B5_N), 
                                     sd(B5_N)) %>% 
  data.frame

outhc %$% table(HD,max_mean)

chisq.test(HD, pref$max_mean, correct=FALSE)
chisq.test(HD, retRole(person_res,cols,"max"), correct=FALSE)

##### STATISTICAL TESTS - Scaling Big Five from 2-10 to 1-10 #####
## Adjust Big Five scores from scale 2-10 to 1-10
# Linear rescaling function from 2-10 to 1-10
rescale_to_1_10 <- function(x) {
  return(1 + ((x - 2) / (10 - 2)) * (10 - 1))
}

# Apply the rescaling to the Big Five columns
outhc$B5_O <- rescale_to_1_10(outhc$B5_O)
outhc$B5_C <- rescale_to_1_10(outhc$B5_C)
outhc$B5_E <- rescale_to_1_10(outhc$B5_E)
outhc$B5_A <- rescale_to_1_10(outhc$B5_A)
outhc$B5_N <- rescale_to_1_10(outhc$B5_N)

outhc %$% table(HD,max_mean)
outhc %>% group_by(HD) %>% summarize(mean(B5_O), 
                                     sd(B5_O), 
                                     mean(B5_C), 
                                     sd(B5_C), 
                                     mean(B5_E), 
                                     sd(B5_E), 
                                     mean(B5_A), 
                                     sd(B5_A), 
                                     mean(B5_N), 
                                     sd(B5_N)) %>% 
  data.frame

# Performing statistical tests after scaling

# Student' t-tests and Shapiro-Wilk normality tests

# Note for reproducibility: If running with the initials further censored, 
# the original order of the clusters becomes changed. The members of clusters and
# results are same, but the HD numbers must be swapped.
# Update: we have swapped the HD numbers to reflect the ordering of clusters
# that comes out with initials first letter being replaced by the 'x' char.
# Tables of BFI before and after transformations are as follows:
# Original:
#  HD mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A.  sd.B5_A.
# 1  1   8.555556 1.0137938   5.222222 1.201850   5.222222 1.3944334   5.666667 1.6583124
# 2  2   4.500000 0.7559289   5.500000 1.069045   5.750000 1.2817399   6.250000 1.3887301
# 3  3   7.000000 0.0000000   6.000000 2.828427   7.500000 0.7071068   8.500000 0.7071068
# 4  4   9.500000 0.5773503   8.000000 1.825742   6.750000 0.9574271   7.500000 1.0000000
# 5  5   6.500000 1.7320508   6.250000 1.892969   3.250000 0.5000000   6.750000 1.5000000
# mean.B5_N.  sd.B5_N.
# 1   5.333333 1.5000000
# 2   4.625000 0.9161254
# 3   9.500000 0.7071068
# 4   2.500000 0.5773503
# 5   8.000000 1.4142136
#
# Cluster ordering after censoring first char with ‘x’.
# HD mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A.  sd.B5_A.
# 1  1   6.500000 1.7320508   6.250000 1.892969   3.250000 0.5000000   6.750000 1.5000000
# 2  2   8.555556 1.0137938   5.222222 1.201850   5.222222 1.3944334   5.666667 1.6583124
# 3  3   4.500000 0.7559289   5.500000 1.069045   5.750000 1.2817399   6.250000 1.3887301
# 4  4   9.500000 0.5773503   8.000000 1.825742   6.750000 0.9574271   7.500000 1.0000000
# 5  5   7.000000 0.0000000   6.000000 2.828427   7.500000 0.7071068   8.500000 0.7071068
# mean.B5_N.  sd.B5_N.
# 1   8.000000 1.4142136
# 2   5.333333 1.5000000
# 3   4.625000 0.9161254
# 4   2.500000 0.5773503
# 5   9.500000 0.7071068
#
# After transformation to the 1-10 scale
# HD mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A.  sd.B5_A.
# 1  1     6.0625 1.9485572    5.78125 2.129591    2.40625 0.5625000    6.34375 1.6875000
# 2  2     8.3750 1.1405180    4.62500 1.352082    4.62500 1.5687375    5.12500 1.8656014
# 3  3     3.8125 0.8504201    4.93750 1.202676    5.21875 1.4419574    5.78125 1.5623214
# 4  4     9.4375 0.6495191    7.75000 2.053960    6.34375 1.0771055    7.18750 1.1250000
# 5  5     6.6250 0.0000000    5.50000 3.181981    7.18750 0.7954951    8.31250 0.7954951
# mean.B5_N.  sd.B5_N.
# 1   7.750000 1.5909903
# 2   4.750000 1.6875000
# 3   3.953125 1.0306411
# 4   1.562500 0.6495191
# 5   9.437500 0.7954951

# After transformation to the 1-10 scale and reordering back to the original cluster ordering
# OC mean.B5_O.  sd.B5_O. mean.B5_C. sd.B5_C. mean.B5_E.  sd.B5_E. mean.B5_A. sd.B5_A.
# 1  1   8.171875 1.2830827   3.953125 1.521092   3.953125 1.7648297   4.515625 2.098802
# 2  2   3.039062 0.9567226   4.304688 1.353010   4.621094 1.6222020   5.253906 1.757612
# 3  3   6.203125 0.0000000   4.937500 3.579728   6.835938 0.8949320   8.101562 0.894932
# 4  4   9.367188 0.7307089   7.468750 2.310705   5.886719 1.2117437   6.835938 1.265625
# 5  5   5.570312 2.1921268   5.253906 2.395789   1.457031 0.6328125   5.886719 1.898438
# mean.B5_N.  sd.B5_N.
# 1  4.0937500 1.8984375
# 2  3.1972656 1.1594712
# 3  9.3671875 0.8949320
# 4  0.5078125 0.7307089
# 5  7.4687500 1.7898640



outhc %>% filter(max_mean == "Pil") %>% select(B5_O)

# Create a transformation table for HD to OC mapping
outhc$OC <- NA  # Initialize a new column for Original Cluster

# Map HD values to OC based on the transformation table
outhc$OC[outhc$HD == 1] <- 5  # OC5 = HD1
outhc$OC[outhc$HD == 2] <- 1  # OC1 = HD2
outhc$OC[outhc$HD == 3] <- 2  # OC2 = HD3
outhc$OC[outhc$HD == 4] <- 4  # OC4 = HD4
outhc$OC[outhc$HD == 5] <- 3  # OC3 = HD5

outhc %$% table(OC,max_mean)
outhc %>% group_by(OC) %>% summarize(mean(B5_O), 
                                     sd(B5_O), 
                                     mean(B5_C), 
                                     sd(B5_C), 
                                     mean(B5_E), 
                                     sd(B5_E), 
                                     mean(B5_A), 
                                     sd(B5_A), 
                                     mean(B5_N), 
                                     sd(B5_N)) %>% 
  data.frame

# Calculate the sample mean of Openness (B5_O)
mean_openness <- mean(outhc$B5_O)
print(mean_openness)

# Original cluster 1
t.test(outhc %>% filter(OC == 1) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

# Original cluster 2
t.test(outhc %>% filter(OC == 2) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "less")

t.test(outhc %>% filter(OC == 2) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "two.sided") 

# Original cluster 3
t.test(outhc %>% filter(OC == 3) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "greater")

t.test(outhc %>% filter(OC == 3) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater")

t.test(outhc %>% filter(OC == 3) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

# Original cluster 4
t.test(outhc %>% filter(OC == 4) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

t.test(outhc %>% filter(OC == 4) %>% select(B5_C),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_C))),
       alternative = "greater") 

# Original cluster 5
t.test(outhc %>% filter(OC == 5) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "two.sided") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "two.sided") 

shapiro.test(as.numeric(outhc[,"B5_O"]))
shapiro.test(as.numeric(outhc[,"B5_C"]))
shapiro.test(as.numeric(outhc[,"B5_E"]))
shapiro.test(as.numeric(outhc[,"B5_A"]))
shapiro.test(as.numeric(outhc[,"B5_N"]))


TukeyHSD(aov(mean_motivation ~ Role, data = motivation_by_role))

pairwise.wilcox.test(motivation_by_role$mean_motivation, motivation_by_role$Role)

## Sample Big Five Inventory scores
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
