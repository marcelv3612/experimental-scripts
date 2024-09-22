

# Dependencies
list.of.packages <- c("openxlsx","tidyverse","magrittr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
rm(list = c("list.of.packages","new.packages"))

# read xlsx data if it does not exist yet
if (!exists("Stats")) Stats <- openxlsx::read.xlsx("Stats.xlsx", sheet = "Sheet 1")

# TODO prumer radšej ako posledné TODO -- nechceme mať neceločíslené hodnoty
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
abline(h=13, col = "blue")

# split(cut) the tree into three groups
HC_BS_3 <- cutree(clusters,3)


# clustering based on both BS and SS
clust_data <- Stats %>% 
  inner_join(last_PS, by = c("Initials","Exc_round"="MER")) %>% 
  select(Initials, B5_O:B5_N, SS_Exp_seek:SS_Disinhib)

clust_data <- clust_data[-40,] # kala filled the form twice, remove the first try
rownames(clust_data) <- clust_data$Initials
clust_data %<>% select(-Initials)

# clustering
clusters = hclust(dist(clust_data),method = "ward.D2")
# chart
plot(clusters, xlab = "")
# cuts
abline(h=20, col = "red")
abline(h=13, col = "blue")

HC_BS_SS_2 <- cutree(clusters,2)
HC_BS_SS_4 <- cutree(clusters,4)


clusters = as.data.frame(cbind(Initials = names(HC_BS_3),
                               HC_BS_3,HC_BS_SS_2,HC_BS_SS_4))

clust_data %<>% mutate(Initials = rownames(.))

DTree_data <- clust_data %>% 
  inner_join(clusters, by = "Initials") %>% 
  mutate(HC_BS_3 = factor(HC_BS_3), 
         HC_BS_SS_2 = factor(HC_BS_SS_2),
         HC_BS_SS_4 = factor(HC_BS_SS_4))


rm(list = c("clusters","clust_data","last_PS", "HC_BS_3","HC_BS_SS_2","HC_BS_SS_4"))

