library(corrgram)
library(ggplot2)

# Dependencies
list.of.packages <- c("ggplot2","openxlsx","tidyr","RColorBrewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# read xlsx data
Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

Stats <- subset(Stats,!(Student_ID %in% c("1ac81dbe09bf3f0f3eac3d67ebc7c53e") & Exc_round  == 1)) #
Stats <- subset(Stats,!(Student_ID %in% c("6f1c1fea47a3b121012af306c5824c02") & Exc_round  == 2)) #
Stats <- subset(Stats,!(Student_ID %in% c("9547de7153ef46ae8a67d6548b3c2e25"))) # hasp00

### Deskriptivní část ###
# statistické charakteristiky pro všech 6 vnitřních kol dohromady
cols <- c("INNER_R1", "INNER_R2", "INNER_R3", "INNER_R4", "INNER_R5", "INNER_R6")
Stats$SUM_EX <- apply(Stats[,cols],1,sum)
Stats$E_mean <- apply(Stats[,cols],1,mean)
Stats$E_med <- apply(Stats[,cols],1,median)
Stats$E_min <- apply(Stats[,cols],1,min)
Stats$E_max <- apply(Stats[,cols],1,max)
Stats$E_sd  <- apply(Stats[,cols],1,sd)
Stats$E_rng <- Stats$E_max - Stats$E_min
Stats$Role <-  ifelse(Stats$Role_01 == "Solo", "Solo", "Team")

# EA + EAO from OCEAN
Stats$BFI_EA = Stats$B5_E + Stats$B5_A
Stats$BFI_EAO = Stats$B5_E + Stats$B5_A + Stats$B5_O

# SS TOTAL
Stats$SS_Total = Stats$SS_Exp_seek +
              Stats$SS_Boredom_susc +
              Stats$SS_Thrill_adv +
              Stats$SS_Disinhib


# Sloupce, které budeme vizualizovat
# cols = selected columns
cols = c("B5_O","B5_C","B5_E","B5_A","B5_N",
          "SS_Exp_seek","SS_Boredom_susc","SS_Thrill_adv","SS_Disinhib")

# Vizualize means and sd, data_graph1
data_graph_1=data.frame(cols,
                   Avg=sapply(Stats[,cols],mean),
                   Sd =sapply(Stats[,cols],sd))

# Aesthetics, vložíme data 
ggplot(data_graph_1, aes(x=cols, y=Avg, fill=cols)) + 
  geom_bar(stat="identity") +                       
  geom_errorbar(aes(ymin=Avg-Sd, ymax=Avg+Sd), width=.3) + 
  xlab(NULL) + ylab(NULL) + ggtitle("Mean scores for personality indicators of students w/ standard deviaton") +
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = "none")

# correlogram
dev.new()
corrgram(Stats[Stats$Role == "Team",], order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt, main="Teams")
dev.new()
corrgram(Stats[Stats$Role == "Solo",], order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt, main="Solo")


dev.off()
dev.off()


# wide to long data
data_long <- gather(Stats, stat, value, B5_O:INNER_R6, factor_key=TRUE)

# take only exercices
data_long$exercise <- as.numeric(substr(data_long$stat,8,8))*1
ex_results_long <- subset(data_long,!is.na(data_long$exercise))

# test - print
data_long$exercise
unique(data_long$exercise)

# Stats by round
Stats1 = subset(Stats, Stats$Exc_round == 1)
Stats2 = subset(Stats, Stats$Exc_round == 2)
Stats3 = subset(Stats, Stats$Exc_round == 3)

# Nechať - pre nízke hodnoty EA máme viac Solo Threshold = 10 
# BFFF

ggplot(data=Stats1, aes(y=B5_O, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats1, aes(y=B5_C, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats1, aes(y=B5_A, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats1, aes(y=B5_E, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats1, aes(y=B5_N, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 

ggplot(data=Stats3, aes(y=B5_O, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats3, aes(y=B5_C, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats3, aes(y=B5_A, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats3, aes(y=B5_E, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats3, aes(y=B5_N, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 


# BSSS
ggplot(data=Stats1, aes(y=SS_Exp_seek, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 

ggplot(data=Stats1, aes(y=SS_Boredom_susc, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 

ggplot(data=Stats1, aes(y=SS_Thrill_adv, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 

ggplot(data=Stats1, aes(y=SS_Disinhib, x=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 
ggplot(data=Stats1, aes(x=SUM_EX, y=SS_Disinhib, col=Role)) +
  geom_point(size=5) + theme_bw() + geom_smooth(method = "lm", se = FALSE)

ggplot(data=Stats1, aes(x=SS_Exp_seek, y=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 

ggplot(data=Stats1, aes(x=SS_Thrill_adv, y=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 

ggplot(data=Stats1, aes(x=SS_Total, y=SUM_EX, col=Role)) +
  geom_point(size=5) + theme_bw() 




# 1. Najít protikladné dynamiky u jedince v režimu Solo vs Team, tzn. tvar písmene M vs W (Excel)

# 2. Prověřit dynamiku, tzn. identifikovat zda některé typy os. měli stále rostoucí trend v motivaci (příp. stále klesající)
# - katj00


# 3. Vytvořit clustery z B5 - a regresi dělat vůči nim a ne vůči izolovaným dimenzím B5

# 4. Vyzkoušet Top-Down = hledání obecných vztahů

# 5. Bottom-Up = zbavíme se noise.

# 6. Identifikovat skokany mezi outer-rounds (vnějšími koly).

# 7. Zohlednit Pilot vs Navigator podle osobnosti
# - kovj19


# poznámka: první a poslední vnitřní kola mohou být specifická - zvážit jejich odstranění, příp. soustředění se pouze na ně. 

# GENERUJEME NOVÝ EXCEL

openxlsx::write.xlsx(x = Stats, file = "Stats-Enhanced.xlsx")


library(sqldf)
data_connected = sqldf("SELECT x1.SUM_EX,
                                x2.SUM_EX,
                                x1.Student_ID
                          FROM Stats x1
                            JOIN Stats x2 
                                ON x1.Student_ID = x2.Student_ID")

ggplot(data=Stats1, aes(x=SUM_EX, col=data_connected)) +
  geom_point(size=5) + theme_bw() 





ggplot(data=ex_results_long1, aes(x=exercise, y=value, col=Student_ID)) +
  geom_line()

ggplot(data=ex_results_long2, aes(x=exercise, y=value, col=Student_ID, fill = factor(Role_06))) +
  geom_line()




ggplot(df, aes(x = factor(age), y = score), fill = variable) + 
  geom_bar(stat = "summary", fun = "mean")





ex_res_solo_long1 <- subset(ex_results_long,Exc_round == 1 & Role == "Solo")
ex_res_team_long1 <- subset(ex_results_long,Exc_round == 1 & Role == "Team")

ex_res_solo_long2 <- subset(ex_results_long,Exc_round == 2 & Role == "Solo")
ex_res_team_long2 <- subset(ex_results_long,Exc_round == 2 & Role == "Team")



# identifikovať osobnosť najlepších výsledkov




