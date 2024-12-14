#### Do not execute - following is further, unpublished analysis.

# Dependencies
list.of.packages <- c("openxlsx","magrittr","tidyverse")
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages"))


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


# read xlsx data if it is not loaded yet
if (!exists("Stats")) Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

# filtrovanie a oprava chybných
Stats <- subset(Stats,!(Student_ID %in% c("1ac81dbe09bf3f0f3eac3d67ebc7c53e") 
                        & Exc_round  == 1)) #
Stats <- subset(Stats,!(Student_ID %in% c("6f1c1fea47a3b121012af306c5824c02") 
                        & Exc_round  == 2)) #
Stats <- subset(Stats,!(Student_ID %in% c("9547de7153ef46ae8a67d6548b3c2e25")))

 
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



mydata = data.frame(personality[,c("B5_O","B5_C","B5_E","B5_A","B5_N")])
rownames(mydata) = personality$Initials



pref=personality[,c("Initials","B5_O","B5_C","B5_E","B5_A","B5_N","SS_Exp_seek",
                    "SS_Boredom_susc","SS_Thrill_adv","SS_Disinhib")]

cols = c("mean_mot_Navigator","mean_mot_Pilot","mean_mot_Solo")
pref$max_mean = retRole(person_res,cols,"max")
pref$min_mean = retRole(person_res,cols,"min")

cols = c("max_mot_Navigator","max_mot_Pilot","max_mot_Solo")
pref$max = retRole(person_res,cols,"max")
cols = c("min_mot_Navigator","min_mot_Pilot","min_mot_Solo")
pref$min = retRole(person_res,cols,"min")
pref



###################################################################
# zaujímavé
ggplot(pref, aes(B5_O,B5_E, col = max_mean)) + 
  geom_point()  + geom_abline(intercept=12.5, slope=-1) # modré
ggplot(pref, aes(B5_O,B5_N, col = max_mean)) + 
  geom_point()  + geom_abline(intercept=12.5, slope=-1) +
  geom_abline(intercept=14.5, slope=-1) # oranžové
ggplot(pref, aes(B5_C,B5_N, col = max_mean)) + 
  geom_point()  + geom_abline(intercept=13.5, slope=-.8) # B5_C celkovo zaujímavé
ggplot(pref, aes(B5_E,B5_N, col = max_mean)) + 
  geom_point()  + geom_abline(intercept=6.7, slope=.2) # rohy

ggplot(pref, aes(SS_Boredom_susc,SS_Thrill_adv, col = max_mean)) + 
  geom_point()  + geom_abline(intercept=5.6, slope=.25)
ggplot(pref, aes(SS_Thrill_adv,SS_Disinhib, col = max_mean)) + 
  geom_point()  + geom_abline(intercept=-12, slope=2.6)

ggplot(pref, aes(B5_O,B5_C, col = min_mean)) + geom_point() + 
  geom_abline(intercept=.7, slope=.6)
ggplot(pref, aes(B5_E,B5_N, col = min_mean)) + geom_point() 

ggplot(pref, aes(B5_O,B5_E, col = max)) + geom_point() 
ggplot(pref, aes(B5_C,B5_E, col = max)) + geom_point() + 
  geom_abline(intercept=-2.9, slope=1.2) # B5_C celkovo zaujímavé
ggplot(pref, aes(B5_C,B5_N, col = max)) + geom_point() + 
  geom_abline(intercept=-3, slope=1.3)

ggplot(pref, aes(SS_Exp_seek,SS_Thrill_adv, col = max)) + 
  geom_point() + geom_abline(intercept=-11, slope=1.83) +
  geom_abline(intercept=-1.4, slope=1.12)
ggplot(pref, aes(SS_Boredom_susc,SS_Disinhib, col = max)) + 
  geom_point() + geom_abline(intercept=10, slope=-.4)
ggplot(pref, aes(SS_Boredom_susc,SS_Thrill_adv, col = max)) + 
  geom_point() + geom_abline(intercept=-3, slope=1.7)
ggplot(pref, aes(SS_Boredom_susc,SS_Disinhib, col = max)) + 
  geom_point() + geom_abline(intercept=10, slope=-.4)
ggplot(pref, aes(SS_Thrill_adv,SS_Disinhib, col = max)) + 
  geom_point() 

ggplot(pref, aes(B5_E,B5_N, col = min)) + geom_point() 
ggplot(pref, aes(B5_A,B5_N, col = min)) + geom_point() 




###########################################################
# nezaujímavé

# max_mean
ggplot(pref, aes(B5_O,B5_C, col = max_mean)) + geom_point()
ggplot(pref, aes(B5_O,B5_E, col = max_mean)) + geom_point()
ggplot(pref, aes(B5_O,B5_A, col = max_mean)) + geom_point()
ggplot(pref, aes(B5_O,B5_N, col = max_mean)) + geom_point()

ggplot(pref, aes(B5_C,B5_E, col = max_mean)) + geom_point()
ggplot(pref, aes(B5_C,B5_A, col = max_mean)) + geom_point()
ggplot(pref, aes(B5_C,B5_N, col = max_mean)) + geom_point()

ggplot(pref, aes(B5_E,B5_A, col = max_mean)) + geom_point()
ggplot(pref, aes(B5_E,B5_N, col = max_mean)) + geom_point()

ggplot(pref, aes(B5_A,B5_N, col = max_mean)) + geom_point()


ggplot(pref, aes(SS_Exp_seek,SS_Boredom_susc, col = max_mean)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Thrill_adv, col = max_mean)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Disinhib, col = max_mean)) + geom_point()

ggplot(pref, aes(SS_Boredom_susc,SS_Thrill_adv, col = max_mean)) + geom_point()
ggplot(pref, aes(SS_Boredom_susc,SS_Disinhib, col = max_mean)) + geom_point()

ggplot(pref, aes(SS_Thrill_adv,SS_Disinhib, col = max_mean)) + geom_point()



# min_mean
ggplot(pref, aes(B5_O,B5_C, col = min_mean)) + geom_point()
ggplot(pref, aes(B5_O,B5_E, col = min_mean)) + geom_point()
ggplot(pref, aes(B5_O,B5_A, col = min_mean)) + geom_point()
ggplot(pref, aes(B5_O,B5_N, col = min_mean)) + geom_point()

ggplot(pref, aes(B5_C,B5_E, col = min_mean)) + geom_point()
ggplot(pref, aes(B5_C,B5_A, col = min_mean)) + geom_point()
ggplot(pref, aes(B5_C,B5_N, col = min_mean)) + geom_point()

ggplot(pref, aes(B5_E,B5_A, col = min_mean)) + geom_point()
ggplot(pref, aes(B5_E,B5_N, col = min_mean)) + geom_point()

ggplot(pref, aes(B5_A,B5_N, col = min_mean)) + geom_point()


ggplot(pref, aes(SS_Exp_seek,SS_Boredom_susc, col = min_mean)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Thrill_adv, col = min_mean)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Disinhib, col = min_mean)) + geom_point()

ggplot(pref, aes(SS_Boredom_susc,SS_Thrill_adv, col = min_mean)) + geom_point()
ggplot(pref, aes(SS_Boredom_susc,SS_Disinhib, col = min_mean)) + geom_point()

ggplot(pref, aes(SS_Thrill_adv,SS_Disinhib, col = min_mean)) + geom_point()



# max
ggplot(pref, aes(B5_O,B5_C, col = max)) + geom_point()
ggplot(pref, aes(B5_O,B5_E, col = max)) + geom_point()
ggplot(pref, aes(B5_O,B5_A, col = max)) + geom_point()
ggplot(pref, aes(B5_O,B5_N, col = max)) + geom_point()

ggplot(pref, aes(B5_C,B5_E, col = max)) + geom_point()
ggplot(pref, aes(B5_C,B5_A, col = max)) + geom_point()
ggplot(pref, aes(B5_C,B5_N, col = max)) + geom_point()

ggplot(pref, aes(B5_E,B5_A, col = max)) + geom_point()
ggplot(pref, aes(B5_E,B5_N, col = max)) + geom_point()

ggplot(pref, aes(B5_A,B5_N, col = max)) + geom_point()


ggplot(pref, aes(SS_Exp_seek,SS_Boredom_susc, col = max)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Thrill_adv, col = max)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Disinhib, col = max)) + geom_point()

ggplot(pref, aes(SS_Boredom_susc,SS_Thrill_adv, col = max)) + geom_point()
ggplot(pref, aes(SS_Boredom_susc,SS_Disinhib, col = max)) + geom_point()

ggplot(pref, aes(SS_Thrill_adv,SS_Disinhib, col = max)) + geom_point()



# min
ggplot(pref, aes(B5_O,B5_C, col = min)) + geom_point()
ggplot(pref, aes(B5_O,B5_E, col = min)) + geom_point()
ggplot(pref, aes(B5_O,B5_A, col = min)) + geom_point()
ggplot(pref, aes(B5_O,B5_N, col = min)) + geom_point()

ggplot(pref, aes(B5_C,B5_E, col = min)) + geom_point()
ggplot(pref, aes(B5_C,B5_A, col = min)) + geom_point()
ggplot(pref, aes(B5_C,B5_N, col = min)) + geom_point()

ggplot(pref, aes(B5_E,B5_A, col = min)) + geom_point()
ggplot(pref, aes(B5_E,B5_N, col = min)) + geom_point()

ggplot(pref, aes(B5_A,B5_N, col = min)) + geom_point()


ggplot(pref, aes(SS_Exp_seek,SS_Boredom_susc, col = min)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Thrill_adv, col = min)) + geom_point()
ggplot(pref, aes(SS_Exp_seek,SS_Disinhib, col = min)) + geom_point()

ggplot(pref, aes(SS_Boredom_susc,SS_Thrill_adv, col = min)) + geom_point()
ggplot(pref, aes(SS_Boredom_susc,SS_Disinhib, col = min)) + geom_point()

ggplot(pref, aes(SS_Thrill_adv,SS_Disinhib, col = min)) + geom_point()
