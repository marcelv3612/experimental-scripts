
# Dependencies
list.of.packages <- c("openxlsx","magrittr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# read xlsx data if it does not exist yet
if (!exists("Stats")) Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

# filtrovanie chybných
Stats <- subset(Stats,!(Student_ID %in% c("1ac81dbe09bf3f0f3eac3d67ebc7c53e") & Exc_round  == 1)) #
Stats <- subset(Stats,!(Student_ID %in% c("6f1c1fea47a3b121012af306c5824c02") & Exc_round  == 2)) #
Stats <- subset(Stats,!(Student_ID %in% c("9547de7153ef46ae8a67d6548b3c2e25"))) # hasp00


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


#############################################
# 1. Najít protikladné dynamiky u jedince v režimu Solo vs Team, tzn. tvar písmene M vs W (Excel)

# All
mot_range_all = motivation_by_role %>% 
  group_by(Initials) %>% 
  summarize(rng = max(mean_motivation)-min(mean_motivation),pct = n()) %>% 
  filter(pct > 1) %>% 
  select(-pct) %>% 
  mutate(rng_f=cut(rng, breaks=c(-1,0,1,2,3,4,5,7,10), 
                   labels=c("0","(0,1]","(1,2]","(2,3]","(3,4]","(4,5]","(5,7]",">7")))

mot_range_all %>% arrange(desc(rng))
mot_range_all %>% arrange(rng)

ggplot(mot_range_all, aes(x = rng_f)) + 
  geom_bar(stat="count", col = "black", fill = "blue", width = 1) 
  

# Team
mot_range_team = motivation_by_role %>% 
  group_by(Initials) %>% 
  filter(Role %in% c("Navigator","Pilot")) %>% 
  summarize(rng = max(mean_motivation)-min(mean_motivation),pct = n()) %>% 
  filter(pct > 1) %>% 
  select(-pct) %>% 
  mutate(rng_f=cut(rng, breaks=c(-1,0,1,2,3,4,5,7,10), 
                   labels=c("0","(0,1]","(1,2]","(2,3]","(3,4]","(4,5]","(5,7]",">7")))

mot_range_team %>% arrange(desc(rng))
mot_range_team %>% arrange(rng)

ggplot(mot_range_team, aes(x = rng_f)) + 
  geom_bar(stat="count", col = "black", fill = "blue", width = 1) 


#############################################
# 2. Prověřit dynamiku, tzn. identifikovat zda některé typy os. měli stále 
  # rostoucí trend v motivaci (příp. stále klesající)
# - katj00


# Team results
team_res <- ordered_results %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  mutate(Result2 = lead(Result,1),Result3 = lead(Result,2)) %>% 
  filter(Round %in% c("INNER_R1","INNER_R2")) %>% 
  select(Initials,Role,Result1=Result,Result2,Result3)
team_res

# dynamics
team_pil_dyn <- team_res %>% 
  mutate(c1_2 = sign(Result2 - Result1),
         c2_3 = sign(Result3 - Result2)) %>% 
  select(-(Result1:Result3)) %>% 
  filter(Role == "Pilot") %>% 
  mutate(mean_dynamics = (c1_2+c2_3)/2)
team_pil_dyn

team_nav_dyn <- team_res %>% 
  mutate(c1_2 = sign(Result2 - Result1),
         c2_3 = sign(Result3 - Result2)) %>% 
  select(-(Result1:Result3)) %>% 
  filter(Role == "Navigator") %>% 
  mutate(mean_dynamics = (c1_2+c2_3)/2)
team_nav_dyn


# Solo results
solo_res <- ordered_results %>%
  filter(Role == "Solo") %>% 
  mutate(Result1 = Result,
         Result2 = lead(Result,1),
         Result3 = lead(Result,2),
         Result4 = lead(Result,3),
         Result5 = lead(Result,4),
         Result6 = lead(Result,5)) %>% 
  filter(Round == "INNER_R1") %>% 
  select(Initials,Role,Result1,Result2,Result3,Result4,Result5,Result6)
solo_res

solo_dyn <- solo_res %>% 
  mutate(c1_2 = sign(Result2 - Result1),
         c2_3 = sign(Result3 - Result2),
         c3_4 = sign(Result4 - Result3),
         c4_5 = sign(Result5 - Result4),
         c5_6 = sign(Result6 - Result5)) %>% 
  mutate(mean_dynamics = (c1_2+c2_3+c3_4+c4_5+c5_6)/5) %>% 
  select(-(Result1:Result6)) 
solo_dyn

# TODO prechod na navigátora/ prechod na pilota

team_pil_dyn %>% summarize(mean = mean(mean_dynamics))
team_nav_dyn %>% summarize(mean = mean(mean_dynamics))
solo_dyn %>% summarize(mean = mean(mean_dynamics))

team_pil_dyn %>% group_by(sign(mean_dynamics)) %>%  summarize(cnt = n())
team_nav_dyn %>% group_by(sign(mean_dynamics)) %>%  summarize(cnt = n())
solo_dyn %>% group_by(sign(mean_dynamics)) %>%  summarize(cnt = n())


#############################################
# 3. Vytvořit clustery z B5 - a regresi dělat vůči nim a ne vůči izolovaným dimenzím B5
print("viz src Clustering.R")


#############################################
# 4. Vyzkoušet Top-Down = hledání obecných vztahů

print("nenašiel som absolútne žiadny vzťah...")

#############################################
# 5. Bottom-Up = zbavíme se noise.

print("???")
# TODO nugget search -- kvalitatívne
# I.P.A. Interpretative phenomenological analysis 

#############################################
# 6. Identifikovat skokany mezi outer-rounds (vnějšími koly).

# differences between each groups
# might be helpful to indentify which personal characteristics are the most crucial - does not
motivation_by_role %>% group_by(Initials) %>% # týmito začať v 5ke
  summarize(rng = max(mean_motivation)-min(mean_motivation)) %>% 
  arrange(desc(rng))

motivation_by_role %>% group_by(Initials) %>% 
  summarize(rng = max(mean_motivation)-min(mean_motivation)) %>% 
  arrange(rng)

motivation_by_role %>% group_by(Initials) %>% 
  summarize(rng = max(mean_motivation)-min(mean_motivation)) %>% 
  arrange(desc(rng)) %>% inner_join(DTree_data)

#############################################
# 7. Zohlednit Pilot vs Navigator podle osobnosti
# - kovj19

motivation_by_role %>% group_by(Initials) %>% 
  filter(Role %in% c("Navigator","Pilot")) %>% 
  summarize(rng = max(mean_motivation)-min(mean_motivation),pct = n()) %>% 
  filter(pct > 1) %>% 
  select(-pct) %>% 
  arrange(desc(rng))

#############################################
# 8. Udělat šest chlívečků: která role koho baví nejvíc 
# (Max-Pilot, Max-Solo, Max-Nav), (Min-Pilot, Min-Solo, Min-Nav)
# tak jeho B5 dát do toho chlívečku. A pak zanalyzovat shluky těchto B5.
# Tzn. že těch cca 50 lidí x2 (jejich nejoblíbenější a nejmíň oblíbená) dáme do 6 chlívečků

# rozdielne počty lebo viac hodnot baví najviac, menej najmenej 
# min
min_motivation <- motivation_by_role %>% 
  group_by(Initials) %>% 
  filter(mean_motivation == min(mean_motivation))

# summary
min_motivation %>% group_by(Role) %>% summarize(cnt = n())

# max
max_motivation <- motivation_by_role %>% 
  group_by(Initials) %>% 
  filter(mean_motivation == max(mean_motivation))

# summary
max_motivation %>% group_by(Role) %>% summarize(cnt = n())


#############################################
# 9. Udělat rozhodovací strom a natrénovat ho.

# min zmena na max? ani jedno nie je dobré
DTree_data_upr <- motivation_by_role %>% 
  group_by(Initials) %>% 
  filter(mean_motivation == min(mean_motivation)) %>% 
  select(-mean_motivation) %>% 
  inner_join(DTree_data, by = "Initials") %>% 
  as.data.frame

#rownames(DTree_data_upr) <- DTree_data_upr$Initials

DTree_data_upr <- DTree_data_upr %>% select(-Initials)

set.seed(0)
shuffle_index <- sample(1:nrow(DTree_data_upr))

DTree_data_upr <- DTree_data_upr[shuffle_index, ]


create_train_test <- function(data, size = 0.8, train = TRUE) {
  my_sample <- 1:as.integer(size * nrow(data))
  if (train == TRUE) {
    return (data[my_sample, ])
  } else {
    return (data[-my_sample, ])
  }
}

data_train <- create_train_test(DTree_data_upr, 0.7, train = TRUE)
data_test <- create_train_test(DTree_data_upr, 0.7, train = FALSE)
dim(data_train)

prop.table(table(data_train$Role))
prop.table(table(data_test$Role))

library(rpart)
library(rpart.plot)
fit <- rpart(Role~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$Role, predict_unseen)
table_mat

accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test     

DTree_data_upr %$% table(Role,HC_BS_3)
DTree_data_upr %$% table(Role,HC_BS_SS_2)
DTree_data_upr %$% table(Role,HC_BS_SS_4) 

#############################################
# poznámka: první a poslední vnitřní kola mohou být specifická - zvážit jejich odstranění, příp. soustředění se pouze na ně. 


# task 10 TODO top priority
# proc Zastoupení rolí v extremních hodnotách škály motivace
# ked je navigátor môže sa dostať na nízku uroveň motivácie

# task 11 doplniť statistické významnosti - neni moc kam






