# PeerJ - LS2022
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
data <- read.xlsx("Experiment.xlsx", sheet = "Sheet1")

# Display basic data information
#summary(data)
#str(data)
#dim(data)

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
likert_levels <- c("Disagree strongly", "Disagree a little", 
                   "Neither agree nor disagree", "Agree a little", "Agree strongly")

# Apply factorization for non-role columns and handle role columns as strings
for (i in 9:66) {
  if (grepl("Role", colnames(data)[i])) {
    levels(data[, i]) <- c('Pilot', 'Y', 'Solo', 'X', 'Navigator')  # Role as string
  } else {
    data[, i] <- as.numeric(factor(data[, i], levels = likert_levels))  # Likert scale to numbers
  }
}

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

##### File 8 - Student' t-tests and Shapiro-Wilk normality tests

# First the file 07 must be ran
outhc %>% filter(max_mean == "Pil") %>% select(B5_O)

t.test(outhc %>% filter(HD == 1) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 2) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 2) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "less") 

t.test(outhc %>% filter(HD == 3) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 4) %>% select(B5_O),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_O))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 4) %>% select(B5_C),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_C))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_N),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_N))),
       alternative = "greater") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_E),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_E))),
       alternative = "less") 

t.test(outhc %>% filter(HD == 5) %>% select(B5_A),
       mu = as.numeric(outhc  %>% summarize(mean = mean(B5_A))),
       alternative = "less") 

shapiro.test(as.numeric(outhc[,"B5_O"]))
shapiro.test(as.numeric(outhc[,"B5_C"]))
shapiro.test(as.numeric(outhc[,"B5_E"]))
shapiro.test(as.numeric(outhc[,"B5_A"]))
shapiro.test(as.numeric(outhc[,"B5_N"]))

