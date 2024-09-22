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


#### Do not execute - following is further, unpublished analysis.
aa = person_res %>% select(max_mot_Pilot,max_mot_Solo,max_mot_Navigator) %>% data.frame


fa2 = factanal(outhc %>% select(B5_O,B5_C,B5_E,B5_A,B5_N),factors = 2, scores = "regression")

colors <- c("blue", "red", "black")
colors <- colors[as.numeric(outhc[,"HD"])]
plot(fa2$scores, col = colors, pch = 2)

write.csv(outhc %>% select(Initials,B5_O,B5_C,B5_E,B5_A,B5_N),"fa.csv")

fao = read.csv("fa.csv", sep = "\t")


library(scatterplot3d)

  shapes = c(16, 17, 18) 
  shapes <- shapes[as.numeric(outhc[,"HD"])]
  scatterplot3d(fao[,1:3], pch = shapes)
  
  colors <- c("#999999", "#E69F00", "#56B4E9")
  colors <- colors[as.numeric(outhc[,"HD"])]
  scatterplot3d(fao[,1:3], pch = 16, color=colors, angle = 70, type="h")
  
  
  
  outhc[outhc$Initials == "haum04","max_mean"] = "Pil" # multiple nastavené na správne
  outhc[outhc$Initials == "matj27","max_mean"] = "Solo" # multiple nastavené na správne
  