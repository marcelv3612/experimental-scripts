
# Dependencies
list.of.packages <- c("openxlsx","tidyverse","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

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
abline(h=11, col = "blue")

# split(cut) the tree into four groups
HC_BS_5<- cutree(clusters,5)















# do not execute
rm(list = c("clusters","clust_data","last_PS", "HC_BS_3","HC_BS_SS_2","HC_BS_SS_4"))

# Filtering out wrong motivational data
Stats <- subset(Stats,!(Student_ID %in% c("1ac81dbe09bf3f0f3eac3d67ebc7c53e") & Exc_round  == 1)) #
Stats <- subset(Stats,!(Student_ID %in% c("6f1c1fea47a3b121012af306c5824c02") & Exc_round  == 2)) #
Stats <- subset(Stats,!(Student_ID %in% c("9547de7153ef46ae8a67d6548b3c2e25"))) # hasp00

