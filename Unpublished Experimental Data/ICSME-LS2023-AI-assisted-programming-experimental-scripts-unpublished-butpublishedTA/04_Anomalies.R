#### Do not execute - following is further, unpublished analysis.

# Dependencies
list.of.packages <- c("openxlsx","magrittr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# read xlsx data if it is not loaded yet
if (!exists("Stats")) Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

# personality
personality <- Stats %>% 
  group_by(Initials) %>% 
  summarize(B5_O = round(mean(B5_O)),
             B5_C = round(mean(B5_C)),
             B5_E = round(mean(B5_E)),
             B5_A = round(mean(B5_A)),
             B5_N = round(mean(B5_N)),
             SS_Exp_seek = round(mean(SS_Exp_seek)),
             SS_Boredom_susc = round(mean(SS_Boredom_susc)),
             SS_Thrill_adv = round(mean(SS_Thrill_adv)),
             SS_Disinhib= round(mean(SS_Disinhib))) %>% 
  select(Initials, B5_O:SS_Disinhib)

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

rm(personality)

# exercise results
ex_res_long <- Stats %>% 
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
  arrange(Initials,Role,Exc_round,Round) 





#############################################
# 1. Protikladné dynamiky u jedince v režimu Solo vs Team

dt <- mot_by_role %>% group_by(Initials) %>% 
  summarize(max_rng = max(mean_mot)-min(mean_mot),
            pct = n()) %>% 
  filter(pct > 1) %>% 
  select(-pct) %>% left_join(mot_by_role %>% 
                                filter(Role %in% c("Navigator","Pilot")) %>% 
                                group_by(Initials) %>% 
                                summarize(max_rng_team = max(mean_mot)-min(mean_mot),
                                          pct = n()) %>%
                                select(-pct),
                              by = "Initials")

ggplot(dt, aes(x = max_rng_team, y = max_rng, col = Initials)) +
  geom_point() + xlab("Rozdiel v motivácii medzi výsledkami v týme") + ylab("Max. rozdiel v motivácii medzi výsledkami") + 
  ggtitle("Rozdiely medzi týmovými/netýmovými výsledkami")



zaujimavy_vysledok1 = data.frame(Inits = dt[dt$max_rng > 4,"Initials"],
                                Reason = "Huge difference in motivation between roles")

# Team results
team_res <- ex_res_long %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  mutate(Result2 = lead(Result,1),Result3 = lead(Result,2)) %>% 
  filter(Round %in% c("INNER_R1","INNER_R2")) %>% 
  select(Initials,Role,Result1=Result,Result2,Result3)

# Solo results
solo_res <- ex_res_long %>%
  filter(Role == "Solo") %>% 
  mutate(Result1 = Result,
         Result2 = lead(Result,1),
         Result3 = lead(Result,2),
         Result4 = lead(Result,3),
         Result5 = lead(Result,4),
         Result6 = lead(Result,5)) %>% 
  filter(Round == "INNER_R1") %>% 
  select(Initials,Role,Result1,Result2,Result3,Result4,Result5,Result6)


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

solo_dyn <- solo_res %>% 
  mutate(c1_2 = sign(Result2 - Result1),
         c2_3 = sign(Result3 - Result2),
         c3_4 = sign(Result4 - Result3),
         c4_5 = sign(Result5 - Result4),
         c5_6 = sign(Result6 - Result5)) %>% 
  mutate(mean_dynamics = (c1_2+c2_3+c3_4+c4_5+c5_6)/5) %>% 
  select(-(Result1:Result6)) 
solo_dyn


team_pil_dyn %>% summarize(mean = mean(mean_dynamics))
team_nav_dyn %>% summarize(mean = mean(mean_dynamics))
solo_dyn %>% summarize(mean = mean(mean_dynamics))

team_pil_dyn %>% group_by(sign(mean_dynamics)) %>%  summarize(cnt = n())
team_nav_dyn %>% group_by(sign(mean_dynamics)) %>%  summarize(cnt = n())
solo_dyn %>% group_by(sign(mean_dynamics)) %>%  summarize(cnt = n())




# prechod na navigátora/ prechod na pilota
# Navigator
nav_to_pil <- ex_res_long %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  arrange(Initials,Exc_round,Round) %>% 
  mutate(Change = sign(lead(Result,1) - Result)) %>% 
  filter(Role %in% c("Navigator")) %>% 
  select(Initials,Role,Exc_round,Round,Change) %>% 
  pivot_wider(names_from = "Round",
              values_from = "Change") %>% 
  mutate(Change1 = ifelse(is.na(INNER_R1),INNER_R2,INNER_R1),
         Change2 = ifelse(is.na(INNER_R3),INNER_R4,INNER_R3),
         Change3 = INNER_R1) %>% 
  group_by(Initials,Role) %>% 
  summarise(Change1 = round(mean(Change1)),
            Change2 = round(mean(Change2)),
            Change3 = round(mean(Change3))) %>% 
  mutate(Change = sum(Change1,Change2,Change3, na.rm = T),
         cnt = sum(!is.na(c(Change1,Change2,Change3)))) %>% 
  mutate(nav_to_pil = Change/cnt) %>% 
  select(Initials,nav_to_pil)
nav_to_pil


ggplot(nav_to_pil, aes(x = nav_to_pil)) +
  geom_histogram(binwidth = .5, fill = "blue")


zaujimavy_vysledok2 = data.frame(Inits = nav_to_pil[nav_to_pil$nav_to_pil == 1,"Initials"],
                                 Reason = "Nav to pilot always ascending")
zaujimavy_vysledok3 = data.frame(Inits = nav_to_pil[nav_to_pil$nav_to_pil == -1,"Initials"],
                                 Reason = "Nav to pilot always descending")



#  Pilot to navigator changes
pil_to_nav <- ex_res_long %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  arrange(Initials,Exc_round,Round) %>% 
  mutate(Change = sign(lead(Result,1) - Result)) %>% 
  filter(Role %in% c("Pilot")) %>% 
  select(Initials,Role,Exc_round,Round,Change) %>% 
  pivot_wider(names_from = "Round",
              values_from = "Change") %>% 
  mutate(Change1 = ifelse(is.na(INNER_R1),INNER_R2,INNER_R1),
         Change2 = ifelse(is.na(INNER_R3),INNER_R4,INNER_R3),
         Change3 = INNER_R1) %>% 
  group_by(Initials,Role) %>% 
  summarise(Change1 = round(mean(Change1)),
            Change2 = round(mean(Change2)),
            Change3 = round(mean(Change3))) %>% 
  mutate(Change = sum(Change1,Change2,Change3, na.rm = T),
         cnt = sum(!is.na(c(Change1,Change2,Change3)))) %>% 
  mutate(pil_to_nav = Change/cnt) %>% 
  select(Initials,pil_to_nav)
pil_to_nav

ggplot(pil_to_nav, aes(x = pil_to_nav)) +
  geom_histogram(binwidth = .5, fill = "blue")


zaujimavy_vysledok4 = data.frame(Inits = pil_to_nav[pil_to_nav$pil_to_nav == 1,"Initials"],
                                 Reason = "Pilot to nav always ascending")
zaujimavy_vysledok5 = data.frame(Inits = pil_to_nav[pil_to_nav$pil_to_nav == -1,"Initials"],
                                 Reason = "Pilot to nav always descending")

team <- ex_res_long %>%
  filter(Role %in% c("Navigator","Pilot")) %>% 
  arrange(Initials,Exc_round,Round) %>% 
  filter(Role %in% c("Pilot", "Navigator")) %>% 
  mutate(Round = case_when(Round %in% c("INNER_R1","INNER_R2") ~ "R1",
                           Round %in% c("INNER_R3","INNER_R4") ~ "R2",
                           Round %in% c("INNER_R5","INNER_R6") ~ "R3")) %>% 
  mutate(Round = ifelse(Role =="Pilot",paste0("P_",Round),paste0("N_",Round))) %>% 
  group_by(Initials,Round) %>% 
  summarise(Result = round(mean(Result)), .groups = "keep") %>% 
  pivot_wider(names_from = "Round",
              values_from = "Result") %>% 
  as.data.frame

rownames(team) = team$Initials
team %<>% select(N_R1:P_R3)

D2 <- mahalanobis(team,colMeans(team), cov(team))
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances") ; rug(D2)
names(D2[D2 > 8])

zaujimavy_vysledok6 = data.frame(Initials = names(D2[D2 > 8]),
                                 Reason = "Different behavior from others in teams")


solo <- ex_res_long %>%
  arrange(Initials,Exc_round,Round) %>% 
  filter(Role %in% c("Solo")) %>% 
  pivot_wider(names_from = "Round",
              values_from = "Result") %>% 
  mutate(ID = paste(Initials,Exc_round, sep = "_")) %>% 
  select(ID,INNER_R1:INNER_R6) %>% as.data.frame

rownames(solo) = solo$ID
solo %<>% select(INNER_R1:INNER_R6)


D2s <- mahalanobis(solo, colMeans(solo), cov(solo))
plot(density(D2s, bw = 0.5),
     main="Squared Mahalanobis distances") ; rug(D2s)
names(D2s[D2s > 10])


zaujimavy_vysledok7 = data.frame(Initials = names(D2s[D2s > 10]),
                                 Reason = "Different behavior from others while solo (includes outer exercise)")

outv <- rbind(zaujimavy_vysledok1,
              zaujimavy_vysledok2,
              zaujimavy_vysledok3,
              zaujimavy_vysledok4,
              zaujimavy_vysledok5,
              zaujimavy_vysledok6,
              zaujimavy_vysledok7)

 
write.xlsx(outv, file = "Different.xlsx", colNames = TRUE, overwrite = TRUE)
