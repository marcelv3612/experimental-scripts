# MDS nerieši nič, grafy nižšie ale vyzerajú niektoré celkom zaujímavo
# problém je, že ja nevidím celú podstatu za tým

# Dependencies
list.of.packages <- c("openxlsx","magrittr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

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



# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- dist(mydata) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     main="Metric MDS", type="n")
text(x, y, labels = row.names(mydata), cex=.7)

retRole = function(data,cols,max_min = "max"){
  if(max_min == "max"){
  val = apply(X=data[,cols], MARGIN=1, FUN=max, na.rm = T)
  } else {
    val = apply(X=data[,cols], MARGIN=1, FUN=min, na.rm = T)
  }
  role = ifelse(val==data[,cols[1]],"N",NA)
  role = ifelse(!is.na(role),role,ifelse(val==data[,cols[2]],"P","S"))
  role = ifelse(!is.na(role),role,"S")
  return(as.character(role))
}

pref=personality[,c("Initials","B5_O","B5_C","B5_E","B5_A","B5_N","SS_Exp_seek",
                    "SS_Boredom_susc","SS_Thrill_adv","SS_Disinhib")]
 
pref$X1 = fit$points[,1]
pref$X2 = fit$points[,2]

cols = c("mean_mot_Navigator","mean_mot_Pilot","mean_mot_Solo")
pref$max_mean = retRole(person_res,cols,"max")
pref$min_mean = retRole(person_res,cols,"min")

cols = c("max_mot_Navigator","max_mot_Pilot","max_mot_Solo")
pref$max = retRole(person_res,cols,"max")
cols = c("min_mot_Navigator","min_mot_Pilot","min_mot_Solo")
pref$min = retRole(person_res,cols,"min")
pref


ggplot(pref, aes(X1,X2, col = max_mean)) + geom_point()

